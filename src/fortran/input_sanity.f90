!  This file is part of XOPTFOIL.

!  XOPTFOIL is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.

!  XOPTFOIL is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.

!  You should have received a copy of the GNU General Public License
!  along with XOPTFOIL.  If not, see <http://www.gnu.org/licenses/>.

!  Copyright (C) 2017-2019 Daniel Prosser

module input_sanity

  use os_util

  implicit none

  contains

!=============================================================================80
!
! Checks that the seed airfoil passes all constraints, sets scale factors for
! objective functions at each operating point.
!
!=============================================================================80
subroutine check_seed()

  use vardef
  use airfoil_evaluation 
  use math_deps,          only : interp_vector, curvature, derv1f1, derv1b1, norm_2
  use math_deps,          only : interp_point, derivation_at_point
  use xfoil_driver,       only : run_op_points, op_point_result_type
  use xfoil_driver,       only : xfoil_geometry_amax, xfoil_set_airfoil, &
                                 xfoil_get_geometry_info
  use airfoil_evaluation, only : xfoil_options, xfoil_geom_options
  use airfoil_operations, only : assess_surface, smooth_it, my_stop, rebuild_airfoil
  use airfoil_operations, only : get_curv_violations, show_reversals_highlows
  use airfoil_operations, only : get_max_te_curvature

  type(op_point_specification_type) :: op_spec
  type(op_point_result_type)        :: op
  type(op_point_result_type), dimension(:), allocatable :: op_points_result

  double precision, dimension(:), allocatable :: x_interp, thickness
  double precision, dimension(:), allocatable :: zt_interp, zb_interp
  double precision, dimension(:), allocatable :: xt,xb,zt,zb
  double precision, dimension(naddthickconst) :: add_thickvec 
  double precision :: penaltyval, tegap, gapallow, maxthick, heightfactor
  double precision :: maxt, xmaxt, maxc, xmaxc
  double precision :: panang1, panang2, maxpanang, slope
  double precision :: checkval, len1, len2, growth1, growth2, xtrans
  double precision :: pi
  integer :: i, nptt, nptb, nptint
  character(100) :: text, text2
  character(15) :: opt_type
  logical :: addthick_violation, xfoil_reinitialize
  double precision :: ref_value, seed_value, tar_value, match_delta, cur_te_curvature
  double precision :: dist = 0d0

  penaltyval = 0.d0
  pi = acos(-1.d0)

  write (*,*)
  write(*,'(" - ",A)') 'Checking to make sure seed airfoil passes all constraints ...'

! Smooth surfaces of airfoil *before* other checks are made
!     save original seed surface before smoothing
!     to show original data later in visualizer 

  seed_foil_not_smoothed = seed_foil

  call check_and_smooth_surface (show_details, do_smoothing, seed_foil)

  xt = seed_foil%xt
  xb = seed_foil%xb
  zt = seed_foil%zt
  zb = seed_foil%zb
  nptt = size(xt,1)
  nptb = size(xb,1)

! Get best values fur surface constraints 

  if (auto_curvature) &
    call auto_curvature_constraints (show_details, seed_foil, &
                                    curv_threshold, highlow_threshold, max_te_curvature, &
                                    max_curv_highlow_top, max_curv_highlow_bot, &
                                    max_curv_reverse_top, max_curv_reverse_bot)

! Get allowable panel growth rate

  growth_allowed = 0.d0

! Top surface growth rates

  len1 = sqrt((xt(2)-xt(1))**2.d0 + (zt(2)-zt(1))**2.d0)
  do i = 2, nptt - 1
    len2 = sqrt((xt(i+1)-xt(i))**2.d0 + (zt(i+1)-zt(i))**2.d0)
    growth1 = len2/len1
    growth2 = len1/len2
    if (max(growth1,growth2) > growth_allowed)                                 &
        growth_allowed = 1.5d0*max(growth1,growth2)
    len1 = len2
  end do

! Bottom surface growth rates

  len1 = sqrt((xb(2)-xb(1))**2.d0 + (zb(2)-zb(1))**2.d0)
  do i = 2, nptb - 1
    len2 = sqrt((xb(i+1)-xb(i))**2.d0 + (zb(i+1)-zb(i))**2.d0)
    growth1 = len2/len1
    growth2 = len1/len2
    if (max(growth1,growth2) > growth_allowed)                               &
        growth_allowed = 1.5d0*max(growth1,growth2)
    len1 = len2
  end do

  
! Too blunt or sharp leading edge

  panang1 = atan((zt(2)-zt(1))/(xt(2)-xt(1))) *                &
            180.d0/acos(-1.d0)
  panang2 = atan((zb(1)-zb(2))/(xb(2)-xb(1))) *                &
            180.d0/acos(-1.d0)
  maxpanang = max(panang2,panang1)

  if (maxpanang > 89.99d0) then
    write(text,'(F8.4)') maxpanang
    text = adjustl(text)
    write(*,*) "LE panel angle: "//trim(text)//" degrees"
    call ask_stop("Seed airfoil's leading edge is too blunt.")
  end if
  if (abs(panang1 - panang2) > 20.d0) then
    write(text,'(F8.4)') abs(panang1 - panang2)
    text = adjustl(text)
    write(*,*) "LE panel angle: "//trim(text)//" degrees"
    call ask_stop("Seed airfoil's leading edge is too sharp.")
  end if

! Too high curvature at TE - TE panel problem 
!    In the current Hicks Henne shape functions implementation, the last panel is
!    forced to become TE which can lead to a thick TE area with steep last panel(s)
!       (see create_shape ... do j = 2, npt-1 ...)
!    so the curvature (2nd derivative) at the last 10 panels is checked

  if (check_curvature) then
    call get_max_te_curvature (nptt, xt, zt, cur_te_curvature)
    if (cur_te_curvature  > max_te_curvature) then 
      write(text,'(F8.4)') cur_te_curvature
      text = adjustl(text)
      call ask_stop("Curvature of "//trim(text)// &
                    " on top surface at trailing edge violates max_te_curvature constraint.")
    end if 

    call get_max_te_curvature (nptb, xb, zb, cur_te_curvature)
    if (cur_te_curvature  > max_te_curvature) then 
      write(text,'(F8.4)') cur_te_curvature
      text = adjustl(text)
      call ask_stop("Curvature of "//trim(text)// &
                    " on bottom surface at trailing edge violates max_te_curvature constraint.")
    end if 
  end if 

! Interpolate either bottom surface to top surface x locations or vice versa
! to determine thickness

  if (xt(nptt) <= xb(nptb)) then
    allocate(x_interp(nptt))
    allocate(zt_interp(nptt))
    allocate(zb_interp(nptt))
    allocate(thickness(nptt))
    nptint = nptt
    call interp_vector(xb, zb, xt, zb_interp)
    x_interp = xt
    zt_interp = zt
  else
    allocate(x_interp(nptb))
    allocate(zt_interp(nptb))
    allocate(zb_interp(nptb))
    allocate(thickness(nptb))
    nptint = nptb
    call interp_vector(xt, zt, xb, zt_interp)
    x_interp = xb
    zb_interp = zb
  end if

! Compute thickness parameters

  tegap = zt(nptt) - zb(nptb)
  maxthick = 0.d0
  heightfactor = tan(min_te_angle*acos(-1.d0)/180.d0/2.d0)

  do i = 2, nptint - 1

!   Thickness array and max thickness
    
    thickness(i) = zt_interp(i) - zb_interp(i)
    if (thickness(i) > maxthick) maxthick = thickness(i)

!   Check if thinner than specified wedge angle on back half of airfoil
    
    if (x_interp(i) > 0.5d0) then
      gapallow = tegap + 2.d0 * heightfactor * (x_interp(nptint) -             &
                                                x_interp(i))
      if (thickness(i) < gapallow) then
        ! jx-mod removed scale and xoffset
        ! xtrans = x_interp(i)/foilscale - xoffset
        xtrans = x_interp(i)
        write(text,'(F8.4)') xtrans
        text = adjustl(text)
        write(*,*) "Detected too thin at x = "//trim(text)
        penaltyval = penaltyval + (gapallow - thickness(i))/0.1d0
      end if
    end if

  end do

! Too thin on back half

  if (penaltyval > 0.d0)                                                       &
     call ask_stop("Seed airfoil is thinner than min_te_angle near the "//&
                   "trailing edge.")
  penaltyval = 0.d0

! Check additional thickness constraints

  if (naddthickconst > 0) then
    call interp_vector(x_interp, thickness,                                    &
                       addthick_x(1:naddthickconst), add_thickvec)

    addthick_violation = .false.
    do i = 1, naddthickconst
      if ( (add_thickvec(i) < addthick_min(i)) .or.                            &
           (add_thickvec(i) > addthick_max(i)) ) then
        addthick_violation = .true.
        write(text,'(F8.4)') addthick_x(i)
        text = adjustl(text)
        write(text2,'(F8.4)') add_thickvec(i)
        text2 = adjustl(text2)
        write(*,*) "Thickness at x = "//trim(text)//": "//trim(text2)
      end if
    end do

    if (addthick_violation)                                                    &
      call ask_stop("Seed airfoil violates one or more thickness constraints.")

  end if

! Max thickness too low

  if (maxthick < min_thickness) then
    write(text,'(F8.4)') maxthick
    text = adjustl(text)
    write(*,*) "Thickness: "//trim(text)
    call ask_stop("Seed airfoil violates min_thickness constraint.")
  end if

! Max thickness too high

  if (maxthick > max_thickness) then
    write(text,'(F8.4)') maxthick
    text = adjustl(text)
    write(*,*) "Thickness: "//trim(text)
    call ask_stop("Seed airfoil violates max_thickness constraint.")
  end if


! Check for curvature reversals

  if (check_curvature) then

    call check_handle_curve_violations ('Top surface', xt, zt, &
                                        max_curv_reverse_top, max_curv_highlow_top)
    call check_handle_curve_violations ('Bot surface', xb, zb, &
                                        max_curv_reverse_bot, max_curv_highlow_bot)
  end if 


! If mode match_foils end here with checks as it becomes aero specific, calc scale

  if (match_foils) then
    match_delta = norm_2(zt(2:nptt-1) - foil_to_match%zt(2:nptt-1)) + &
                  norm_2(zb(2:nptb-1) - foil_to_match%zb(2:nptb-1))
    ! Playground: Match foil equals seed foil. Take a dummy objective value to start
    if (match_delta < 1d-10) then 
      call ask_stop('Match foil and seed foil are equal. A dummy initial value '// &
                     'for the objective function will be taken for demo')
      match_delta = 1d-1 
    end if
    match_foils_scale_factor = 1.d0 / match_delta
    return
  end if 


! Check for bad combinations of operating conditions and optimization types

  do i = 1, noppoint

    write(text,*) i
    text = adjustl(text)

    op_spec  = op_points_spec(i) 
    opt_type = op_spec%optimization_type
    if ((op_spec%value <= 0.d0) .and. (op_spec%spec_cl)) then
      if ( (trim(opt_type) /= 'min-drag') .and.                                &
           (trim(opt_type) /= 'max-xtr') .and.                                 &
            ! jx-mod - allow geo target and min-lift-slope, min-glide-slope
           (trim(opt_type) /= 'target-drag') .and.                             &
           (trim(opt_type) /= 'min-lift-slope') .and.                          &
           (trim(opt_type) /= 'min-glide-slope') .and.                         &
           (trim(opt_type) /= 'max-lift-slope') ) then
        write(*,*) "Error: operating point "//trim(text)//" is at Cl = 0. "//  &
                 "Cannot use '"//trim(opt_type)//"' optimization in this case."
        write(*,*) 
        stop
      end if
    elseif (op_spec%spec_cl .and. (trim(opt_type) == 'max-lift')) then
      write(*,*) "Error: Cl is specified for operating point "//trim(text)//   &
                 ". Cannot use 'max-lift' optimization type in this case."
      write(*,*) 
      stop
    elseif (op_spec%spec_cl .and. (trim(opt_type) == 'target-lift')) then              
      write (*,*) ("op_mode = 'spec_cl' doesn't make sense "//                &
                   "for optimization_type 'target-lift'")
      write(*,*) 
      stop
    end if

  end do

! Check for a good value of xfoil vaccel to ensure convergence at higher cl

  if (xfoil_options%vaccel > 0.01d0) then
    write(text,'(F8.4)') xfoil_options%vaccel
    text = adjustl(text)
    call print_note ("The xfoil convergence paramter vaccel: "//trim(text)// &
                     " should be less then 0.01 to avoid convergence problems.")
  end if

! Re-Init boundary layer at each op point to ensure convergence (slower)

  xfoil_reinitialize = xfoil_options%reinitialize
  xfoil_options%reinitialize = .true. 


! Analyze airfoil at requested operating conditions with Xfoil

  write (*,'("   ",A)') 'Analyze seed airfoil at requested operating points'

  call run_op_points (seed_foil, xfoil_geom_options, xfoil_options,        &
                      use_flap, flap_spec, flap_degrees, &
                      op_points_spec, op_points_result)

  ! xfoil_options%show_details = show_details
  xfoil_options%reinitialize = xfoil_reinitialize 

! get airfoil geometry info from xfoil    

  call xfoil_set_airfoil (seed_foil)        
  call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)
  maxpanang = xfoil_geometry_amax() 

! Too large panel angles
  if (maxpanang > 30.d0) then
    write(text,'(F8.4)') maxpanang
    text = adjustl(text)
    write(*,*) "Max panel angle: "//trim(text)
    call ask_stop("Seed airfoil panel angles are too large. Try adjusting "//&
                  "xfoil_paneling_options.")
  end if

! Camber too high

  if (maxc > max_camber) then
    write(text,'(F8.4)') maxc
    text = adjustl(text)
    write(*,*) "Camber: "//trim(text)
    call ask_stop("Seed airfoil violates max_camber constraint.")
  end if

! Camber too low

  if (maxc < min_camber) then
    write(text,'(F8.4)') maxc
    text = adjustl(text)
    write(*,*) "Camber: "//trim(text)
    call ask_stop("Seed airfoil violates min_camber constraint.")
  end if

! jx-mod Geo targets start -------------------------------------------------

! Evaluate seed value of geomtry targets and scale factor 
  
  do i = 1, ngeo_targets

    select case (trim(geo_targets(i)%type))

      case ('zTop')                       ! get z_value top side 
        seed_value = interp_point(x_interp, zt_interp, geo_targets(i)%x)
        ref_value  = interp_point(x_interp, thickness, geo_targets(i)%x)
      case ('zBot')                       ! get z_value bot side
        seed_value = interp_point(x_interp, zb_interp, geo_targets(i)%x)
        ref_value  = interp_point(x_interp, thickness, geo_targets(i)%x)
      case ('Thickness')                  ! take foil thickness calculated above
        seed_value = maxt
        ref_value  = maxt
      case ('Camber')                     ! take xfoil camber from  above
        seed_value = maxc
        ref_value  = maxc
      case default
        call my_stop("Unknown target_type '"//trim(geo_targets(i)%type))
    end select

    geo_targets(i)%seed_value      = seed_value
    geo_targets(i)%reference_value = ref_value

    ! target value negative?  --> take current seed value * |target_value| 
    if ((geo_targets(i)%target_value <= 0.d0) .and. (trim(geo_targets(i)%type) /= 'zBot'))                                 &
        geo_targets(i)%target_value = seed_value * abs(geo_targets(i)%target_value)
    
    tar_value = geo_targets(i)%target_value

    ! will scale objective to 1 ( = no improvement) 
    geo_targets(i)%scale_factor = 1 / ( ref_value + abs(tar_value - seed_value))

  end do 

! Free memory

  deallocate(x_interp)
  deallocate(zt_interp)
  deallocate(zb_interp)
  deallocate(thickness)

! Geo targets - end --------------------------------------------


! Evaluate objectives to establish scale factors for each point

  do i = 1, noppoint

    write(text,*) i
    text = adjustl(text)

    op_spec  = op_points_spec(i)
    op       = op_points_result(i) 
    opt_type = op_spec%optimization_type

    ! Check for unconverged points

    if (.not. op%converged) then
      write(text,*) i
      text = adjustl(text)
      call ask_stop("Xfoil calculations did not converge for operating "//&
                    "point "//trim(text)//".")
    end if

    if (op%cl <= 0.d0 .and. (trim(opt_type) == 'min-sink' .or.   &
        trim(opt_type) == 'max-glide') ) then
      write(*,*) "Error: operating point "//trim(text)//" has Cl <= 0. "//     &
                 "Cannot use "//trim(opt_type)//" optimization "// &
                 "in this case."
      write(*,*)
      stop
    end if

    if (trim(opt_type) == 'min-sink') then
      checkval   = op%cd / op%cl**1.5d0
      seed_value = op%cl ** 1.5d0 / op%cd

    elseif (trim(opt_type) == 'max-glide') then
      checkval   = op%cd / op%cl
      seed_value = op%cl / op%cd

    elseif (trim(opt_type) == 'min-drag') then
      checkval   = op%cd
      seed_value = op%cd

    ! Op point type 'target-....'
    !      - minimize the difference between current value and target value
    !      - target_value negative?  --> take current seed value * |target_value| 

    elseif (trim(opt_type) == 'target-drag') then
      if (op_spec%target_value < 0.d0) op_spec%target_value = op%cd * abs(op_spec%target_value)

      dist = ABS (op_spec%target_value - op%cd)
      if (dist < 0.000004d0) dist = 0d0  ! little threshold to achieve target

      checkval   = op_spec%target_value + dist
      seed_value = op%cd

    elseif (trim(opt_type) == 'target-lift') then
      if (op_spec%target_value < 0.d0) op_spec%target_value = op%cl * abs(op_spec%target_value)
      ! add a constant base value to the lift difference so the relative change won't be to high
      checkval   = 1.d0 + ABS (op_spec%target_value - op%cl)
      seed_value = op%cl

    elseif (trim(opt_type) == 'target-moment') then
      if (op_spec%target_value < 0.d0) op_spec%target_value = op%cm * abs(op_spec%target_value) 
      ! add a base value (Clark y or so ;-) to the moment difference so the relative change won't be to high
      checkval   = ABS (op_spec%target_value - op%cm) + 0.05d0
      seed_value = op%cm

    elseif (trim(opt_type) == 'max-lift') then
      checkval   = 1.d0/op%cl
      seed_value = op%cl

    elseif (trim(opt_type) == 'max-xtr') then
      checkval   = 1.d0/(0.5d0*(op%xtrt + op%xtrb) + 0.1d0)  ! Ensure no division by 0
      seed_value = 0.5d0*(op%xtrt + op%xtrb)

! jx-mod Following optimization based on slope of the curve of op_point
!         convert alpha in rad to get more realistic slope values
!         convert slope in rad to get a linear target 
!         factor 4.d0*pi to adjust range of objective function (not negative)

    elseif (trim(opt_type) == 'max-lift-slope') then
    ! Maximize dCl/dalpha (0.1 factor to ensure no division by 0)
      slope = derivation_at_point (i, (op_points_result%alpha * pi/180.d0) , &
                                      (op_points_result%cl))
      checkval   = 1.d0 / (atan(abs(slope))  + 2.d0*pi)
      seed_value = atan(abs(slope))

    elseif (trim(opt_type) == 'min-lift-slope') then
    ! New: Minimize dCl/dalpha e.g. to reach clmax at alpha(i) 
      slope = derivation_at_point (i, (op_points_result%alpha * pi/180.d0) , &
                                      (op_points_result%cl))
      checkval   = atan(abs(slope)) + 2.d0*pi
      seed_value = atan(abs(slope))

    elseif (trim(opt_type) == 'min-glide-slope') then
    ! New: Minimize d(cl/cd)/dcl e.g. to reach best glide at alpha(i) 
      slope = derivation_at_point (i, (op_points_result%cl * 20d0), &
                                      (op_points_result%cl/op_points_result%cd))
      checkval   = atan(abs(slope)) + 2.d0*pi
      seed_value = atan(abs(slope)) 
     
    else
      write(*,*)
      write(*,*) "Error: requested optimization_type for operating point "//   &
                 trim(text)//" not recognized."
      stop
    end if

    op_spec%scale_factor = 1.d0/checkval

    op_points_spec(i) = op_spec             ! write back target, scale, ...

  end do

end subroutine check_seed


!-------------------------------------------------------------------------
! Checks curvature quality of foil
!   when smooting is active and the quality is not good, smooting will be done
!   prints summary of the quality 
!-------------------------------------------------------------------------

subroutine check_and_smooth_surface (show_details, do_smoothing, foil)

  use vardef,             only: airfoil_type
  use airfoil_evaluation, only: curv_threshold, spike_threshold, highlow_threshold, &
                                max_te_curvature
  use airfoil_operations, only: assess_surface, smooth_it
  use airfoil_operations, only: get_max_te_curvature, rebuild_airfoil
  use math_deps,          only: nreversals_using_threshold

  logical, intent(in)       :: show_details, do_smoothing
  type (airfoil_type), intent (inout)  :: foil

  integer             :: overall_quality, top_quality, bot_quality
  character (80)      :: text1, text2
  logical             :: done_smoothing

  done_smoothing        = .false.


!  ------------ analyze & smooth  top -----

  top_quality = 0
  bot_quality = 0
 
  call assess_surface (show_details, 'Analyzing top side', &
                       curv_threshold, spike_threshold, highlow_threshold, max_te_curvature, &
                       foil%xt, foil%zt, top_quality)

  if (top_quality >= Q_BAD .or. do_smoothing) then 

    call smooth_it (.false., spike_threshold, foil%xt, foil%zt)
    top_quality     = 0 
    done_smoothing  = .true.
    call assess_surface (show_details, 'smoothed', &
                         curv_threshold, spike_threshold, highlow_threshold, max_te_curvature, &
                         foil%xt, foil%zt, top_quality)
  end if
 
!  ------------ analyze & smooth  bot -----

  call assess_surface (show_details, 'Analyzing bot side', &
                       curv_threshold, spike_threshold, highlow_threshold, max_te_curvature, &
                       foil%xb, foil%zb, bot_quality)
  
  if (bot_quality >= Q_BAD .or. do_smoothing) then 

    call smooth_it (.false., spike_threshold, foil%xb, foil%zb)
    bot_quality     = 0 
    done_smoothing  = .true.
    call assess_surface (show_details, 'smoothed', &
                        curv_threshold, spike_threshold, highlow_threshold, max_te_curvature, &
                        foil%xb, foil%zb, bot_quality)
  end if

  overall_quality = int((top_quality + bot_quality)/2)

! When smoothed - Rebuild foil out of smoothed polyline 
  if (done_smoothing) then
    call rebuild_airfoil ((foil%xt), (foil%xb), (foil%zt), (foil%zb), foil)
  end if 

! ... printing stuff 


  if (show_details) then
    call print_colored (COLOR_NOTE, '   Summary: ')
  else
    if (done_smoothing) then
      call print_colored (COLOR_NORMAL, '   Smoothing airfoil due to bad surface quality')
      write (*,*)
    end if
    call print_colored (COLOR_NOTE, '   Airfoil surface assessment: ')
  end if

  if (done_smoothing) then
    text1 = ' smoothed'
    text2 = ' also'
  else
    text1 = ''
    text2 = ''
  end if

  if (overall_quality < Q_OK) then
    call print_colored (COLOR_GOOD,'The'//trim(text1)//' airfoil has a perfect surface quality')
  elseif (overall_quality < Q_BAD) then
    call print_colored (COLOR_NORMAL,'The surface quality of the'//trim(text1)//' airfoil is ok')
  elseif (overall_quality < Q_PROBLEM) then
    call print_colored (COLOR_WARNING,'The surface quality of the'//trim(text1)//' airfoil'//trim(text2)//' is not good. '// &
                         'Better choose another one ...' ) 
  else
    call print_colored (COLOR_ERROR,' The'//trim(text1)//' surface is'//trim(text2)//' not really suitable for optimization')
  end if  
  write (*,*) 

end subroutine

!-------------------------------------------------------------------------
! Evaluates and sets the best values for surface thresholds and constraints
!-------------------------------------------------------------------------

subroutine auto_curvature_constraints (show_details, foil, &
                    curv_threshold, highlow_threshold, max_te_curvature, &
                    max_curv_highlow_top, max_curv_highlow_bot, &
                    max_curv_reverse_top, max_curv_reverse_bot)

  use vardef,             only: airfoil_type
  use airfoil_operations, only: le_find, assess_surface, smooth_it
  use airfoil_operations, only: get_max_te_curvature, get_best_reversal_threshold
  use airfoil_operations, only: get_best_highlow_threshold
  use math_deps,          only: nreversals_using_threshold
  use math_deps,          only: nhighlows_using_threshold
  

  type (airfoil_type), intent (inout)  :: foil
  logical, intent (in)            :: show_details
  double precision, intent(inout) :: curv_threshold, highlow_threshold, max_te_curvature
  integer, intent(inout)          :: max_curv_reverse_top, max_curv_reverse_bot
  integer, intent(inout)          :: max_curv_highlow_top, max_curv_highlow_bot
  
  double precision, dimension(:), allocatable :: xt, xb, yt, yb

  double precision    :: threshold_top, threshold_bot, min_curv_thresh, min_highlow_thresh
  double precision    :: max_te_curvature_top, max_te_curvature_bot
  double precision    :: old_max_te_curvature, old_highlow_threshold, old_curv_threshold
  character (80)      :: str

  write (*,'(" - ", A)') 'Evaluating and auto setting of geometric thresholds and constraints '
  write (*,*)

  xt = foil%xt
  xb = foil%xb
  yt = foil%zt
  yb = foil%zb

!  ------------ curve reversal constraints -----

  min_curv_thresh = 0.01d0

  call get_best_reversal_threshold (size(xt), xt,yt, min_curv_thresh, max_curv_reverse_top,&
                                    threshold_top)
  call get_best_reversal_threshold (size(xb), xb,yb, min_curv_thresh, max_curv_reverse_bot, &
                                    threshold_bot)

  old_curv_threshold = curv_threshold
  curv_threshold = max(threshold_top, threshold_bot)
  ! no retest - how many reversals will we get with the new threshold
  max_curv_reverse_top = nreversals_using_threshold (xt, yt, curv_threshold)
  max_curv_reverse_bot = nreversals_using_threshold (xb, yb, curv_threshold)
 
!  ------------ te curvature -----

  if (max_te_curvature == 10.d0) then        ! te_curvature was not set in inputs --> auto

    old_max_te_curvature = 0.2d0            ! dummy old value
    
    call get_max_te_curvature (size(xt), xt,yt, max_te_curvature_top )
    call get_max_te_curvature (size(xb), xb,yb, max_te_curvature_bot )

    max_te_curvature = max (max_te_curvature_top, max_te_curvature_bot) * 1.1d0 ! little more to breath...
  else 
    old_max_te_curvature = max_te_curvature
  end if 

!  ------------ highlow curvature amplitude -----

  old_highlow_threshold = highlow_threshold
  min_highlow_thresh    = highlow_threshold

  call get_best_highlow_threshold (size(xt), xt,yt, min_highlow_thresh, &
                                   max_curv_highlow_top, threshold_top)
  call get_best_highlow_threshold (size(xb), xb,yb, min_highlow_thresh, &
                                   max_curv_highlow_bot, threshold_bot)

  highlow_threshold = max(threshold_top, threshold_bot) * 1.1d0 ! little more to breath...

  max_curv_highlow_top = nhighlows_using_threshold (xt, yt, highlow_threshold)
  max_curv_highlow_bot = nhighlows_using_threshold (xb, yb, highlow_threshold)


! ... pleasure to print that all ...

  if (show_details) then 

    !  ------------ reversals -----

    str = 'curv_threshold'
    write (*,'(10x,A18,A1)', advance = 'no') str,'='
    write (str,'(F6.3)') old_curv_threshold
    call print_colored (COLOR_NOTE,trim(str))
    write (*,'(A3,F6.3,4x)', advance = 'no') ' ->', curv_threshold 

    write (str,'(A)') 'to get'
    call print_colored (COLOR_NOTE,trim(str))
    write (*,'(A,I2)', advance = 'no') ' max_curv_reverse_top =', max_curv_reverse_top
    write(*,*)

    write (*,'(10x,38x)', advance = 'no')
    write (str,'(A)') 'to get'
    call print_colored (COLOR_NOTE, trim(str))
    write (*,'(A,I2)', advance = 'no') ' max_curv_reverse_bot =', max_curv_reverse_bot
    write(*,*)
  
    !  ------------ highlow curvature amplitude -----

    str = 'highlow_threshold'
    write (*,'(10x,A18,A1)', advance = 'no') str,'='
    write (str,'(F6.3)') old_highlow_threshold
    call print_colored (COLOR_NOTE,trim(str))
    write (*,'(A3,F6.3,4x)', advance = 'no') ' ->', highlow_threshold 

    write (str,'(A)') 'to get'
    call print_colored (COLOR_NOTE,trim(str))
    write (*,'(A,I2)', advance = 'no') ' max_curv_highlow_top =', max_curv_highlow_top
    write(*,*)

    write (*,'(10x,38x)', advance = 'no')
    write (str,'(A)') 'to get'
    call print_colored (COLOR_NOTE, trim(str))
    write (*,'(A,I2)', advance = 'no') ' max_curv_highlow_bot =', max_curv_highlow_bot
    write(*,*)

    !  ------------ te curvature -----

    str = 'max_te_curvature'
    write (*,'(10x,A18,A1)', advance = 'no') str,'='
    write (str,'(F6.3)') old_max_te_curvature
    call print_colored (COLOR_NOTE,trim(str))
    write (*,'(A3,F6.2,4x)', advance = 'no') ' ->', max_te_curvature 
    if ( max_te_curvature > 100d0) then
      call print_colored (COLOR_NOTE, 'due to much too high value of seed airfoil')
      max_te_curvature = 1d6
    end if
    write(*,*)
  end if

end subroutine auto_curvature_constraints

!=============================================================================80
!
! Asks user to stop or continue
!
!=============================================================================80
subroutine ask_stop(message)

  character(*), intent(in) :: message

  character :: choice
  logical :: valid_choice

! Get user input

  valid_choice = .false.
  do while (.not. valid_choice)
  
    if (len(trim(message)) > 0) call print_warning (message)

    write(*,'(/,1x,A)', advance='no') 'Continue anyway? (y/n): '
    read(*,'(A)') choice

    if ( (choice == 'y') .or. (choice == 'Y') ) then
      valid_choice = .true.
      choice = 'y'
    else if ( ( choice == 'n') .or. (choice == 'N') ) then
      valid_choice = .true.
      choice = 'n'
    else
      write(*,'(A)') 'Please enter y or n.'
      valid_choice = .false.
    end if

  end do

! Stop or continue

  write(*,*)
  if (choice == 'n') stop

end subroutine ask_stop

!-----------------------------------------------------------------------------
! Checks surface x,y for violations of curvature contraints 
!     reversals > max_curv_reverse
!     highlows  > max_curv_highlow
! 
! and handles user response  
!-----------------------------------------------------------------------------

subroutine  check_handle_curve_violations (info, x, y, max_curv_reverse, max_curv_highlow)

  use airfoil_evaluation, only : curv_threshold, highlow_threshold
  use airfoil_operations, only : show_reversals_highlows, get_curv_violations


  character(*),                   intent(in) :: info
  double precision, dimension(:), intent(in) :: x, y
  integer,                        intent(in) :: max_curv_reverse, max_curv_highlow

  integer :: n, max, nreverse_violations, nhighlow_violations

  call get_curv_violations (x, y, & 
                            curv_threshold, highlow_threshold, & 
                            max_curv_reverse, max_curv_highlow,   &
                            nreverse_violations, nhighlow_violations)

  ! Exit if everything is ok 
  if ((nreverse_violations + nhighlow_violations) == 0) return 

  call print_warning ("Curvature violations on " // trim(info))
  write (*,*)

  if (nreverse_violations > 0) then 
    n   = nreverse_violations + max_curv_reverse
    max = max_curv_reverse
    write (*,'(10x,A,I2,A,I2)')"Found ",n, " Reversal(s) where max_curv_reverse is set to ", max
  end if 

  if (nhighlow_violations > 0) then 
    n   = nhighlow_violations + max_curv_highlow
    max = max_curv_highlow
    write (*,'(10x,A,I2,A,I2)')"Found ",n, " HighLow(s) where max_curv_highlow is set to ", max
  end if 

  write (*,*)
  call show_reversals_highlows ('', x, y, curv_threshold, highlow_threshold )
  write (*,*)
  write (*,'(10x,A)') 'The Optimizer may not found a solution with this inital violation.'
  write (*,'(10x,A)') 'Either increase max_curv_reverse or curv_threshold (not recommended) or'
  write (*,'(10x,A)') 'choose another seed airfoil. Find details in geometry plot of the viszualizer.'
  call ask_stop('')

end subroutine check_handle_curve_violations



end module input_sanity

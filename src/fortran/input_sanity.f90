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

!=============================================================================
!
! Checks various inputs to be consistent and valid 
!
!=============================================================================

subroutine check_inputs(global_search, pso_options)

  use vardef
  use airfoil_evaluation 
  use particle_swarm
  use xfoil_driver,       only : op_point_specification_type
  use xfoil_driver,       only : xfoil_options_type
  use particle_swarm,     only : pso_options_type

  type (pso_options_type), intent(inout) :: pso_options
  character(80), intent(in)              :: global_search

  integer             :: i, nxtr_opt, ndyn, nscaled
  character           :: choice

! Shape functions and geomtry / curvature checks

  if ((trim(shape_functions) == 'camb-thick') .or. &
  (trim(shape_functions) == 'camb-thick-plus')) then
    ! in case of camb_thick checking of curvature makes no sense
    if (curv_spec%check_curvature) then 
      call print_note ("Because of shape function 'camb-thick' curvature ckecking "// &
                       "will be switched off during optimization")
      curv_spec%check_curvature = .false. 
      curv_spec%auto_curvature  = .false. 
    end if 
    if ((.not. curv_spec%do_smoothing) .and. (.not. match_foils)) then 
      call print_note ("Smoothing switched on for shape function 'camb-thick' "// &
                       "to ensure good results.")
      curv_spec%do_smoothing = .true. 
    end if 
  elseif (trim(shape_functions) == 'hicks-henne' ) then
    if (.not. curv_spec%check_curvature .and. (.not. match_foils)) then 
      call print_warning ("When using shape function 'hicks-henne', curvature ckecking "// &
                       "should be switched to avoid bumps.")
    end if 
  end if 


! Ask about removing turbulent trips for max-xtr optimization

  nxtr_opt = 0
  if ( (xfoil_options%xtript < 1.d0) .or. (xfoil_options%xtripb < 1.d0) ) then
    do i = 1, noppoint
      if (trim(op_points_spec(i)%optimization_type) == "max-xtr") nxtr_opt = nxtr_opt + 1
    end do
  
    if (nxtr_opt > 0) choice = ask_forced_transition()
    if (choice == 'y') then
      xfoil_options%xtript = 1.d0
      xfoil_options%xtripb = 1.d0
    end if
  end if
  
! May the king of xfoil polars be lenient ...
!        ... when patching to support negative cl for Type 2 based op_points
  do i = 1, noppoint
    if ((op_points_spec(i)%re%type == 2) .and. (op_points_spec(i)%spec_cl) & 
                                         .and. (op_points_spec(i)%value < 0d0)) then
      op_points_spec(i)%re%type    = 1
      op_points_spec(i)%re%number  = op_points_spec(i)%re%number / & 
                                    (abs(op_points_spec(i)%value) ** 0.5d0)
    end if
  end do 


! in case of camb_thick a re-paneling is not needed and
! not good for high cl
  if ((trim(shape_functions) == 'camb-thick') .or. &
      (trim(shape_functions) == 'camb-thick-plus')) then
        xfoil_geom_options%repanel = .false. 
  end if 


! Dynamic Weighting --------------------------------------------------

  if (dynamic_weighting_spec%active) then 

  ! - We need enough targets to be dynamic    
  ! - Not too much dynamic weightings should be scaled by user     

    ndyn = 0
    nscaled = 0 

    do i= 1, size(geo_targets)
      if (geo_targets(i)%dynamic_weighting)     ndyn = ndyn + 1
      if (geo_targets(i)%weighting_user /= 1d0) nscaled = nscaled + 1
    end do
    do i = 1, size(op_points_spec)
      if (op_points_spec(i)%dynamic_weighting)     ndyn = ndyn + 1
      if (op_points_spec(i)%weighting_user /= 1d0) nscaled = nscaled + 1
    end do

    if (ndyn < 3) &
      call my_stop("Dynamic weighting needs at least 3 op points with a target based"//  &
                   " optimization_type")
    if (nscaled > (ndyn/4)) &
      call my_stop("For Dynamic weighting only a few targets should have a scaled weighting <> 1.0."//&
                   " Set weighting to 1.0 (or just remove it)")
    call print_note ("Dynamic weighting starting with design #"// & 
                      stri(dynamic_weighting_spec%start_with_design) //" repeating every " //&
                      stri(dynamic_weighting_spec%frequency) //" designs.")
  end if


! PSO auto_retry  --------------------------------------------------

  if ((trim(shape_functions) /= 'hicks-henne') .and. & 
      (trim(global_search) == 'particle_swarm')) then 
    if (pso_options%max_retries >= 0) then 
      call print_note ('Particle retry swiched off (meaningful only for Hicks-Henne shape_type)')
      pso_options%max_retries = 0
      pso_options%auto_retry = .false.
    end if 
  end if 

! Match foil  --------------------------------------------------

  ! Switch off geometric checks 
  if (match_foils) then 
    check_geometry = .false.
    curv_spec%check_curvature = .false. 
    curv_spec%auto_curvature  = .false. 
    curv_spec%do_smoothing = .false. 
    call print_note ("Smoothing, geometry and curvature checks switched off for match foil mode.")
  endif 

  end subroutine check_inputs

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
  use airfoil_operations, only : assess_surface, smooth_it, rebuild_airfoil
  use airfoil_operations, only : show_reversals_highlows
  use airfoil_operations, only : get_max_te_curvature

  type(op_point_specification_type) :: op_spec
  type(op_point_result_type)        :: op
  type(op_point_result_type), dimension(:), allocatable :: op_points_result
  type(xfoil_options_type)          :: local_xfoil_options

  double precision, dimension(:), allocatable :: x_interp, thickness
  double precision, dimension(:), allocatable :: zt_interp, zb_interp
  double precision, dimension(:), allocatable :: xt,xb,zt,zb
  double precision, dimension(naddthickconst) :: add_thickvec 
  double precision :: penaltyval, tegap, gapallow, maxthick, heightfactor
  double precision :: maxt, xmaxt, maxc, xmaxc, correction
  double precision :: panang1, panang2, maxpanang, slope
  double precision :: checkval, len1, len2, growth1, growth2, xtrans
  double precision :: pi
  integer :: i, nptt, nptb, nptint, overall_quality
  character(100) :: text, text2
  character(15) :: opt_type
  logical :: addthick_violation
  double precision :: ref_value, seed_value, tar_value, match_delta
  double precision :: dist = 0d0

  penaltyval = 0.d0
  pi = acos(-1.d0)


! Preset seed airfoil geometry to geo targets and/or thickness-camber constraints ----------

  call preset_airfoil_to_targets (.true., seed_foil, geo_targets, &
                                  min_thickness, max_thickness, min_camber, max_camber) 

! Afterwards sanity check of thickness and camber 

  call check_thickness_camber (seed_foil, min_thickness, max_thickness, min_camber, max_camber)

! Smooth surfaces of airfoil *before* other checks are made
!     save original seed surface before smoothing
!     to show original data later in visualizer 

  seed_foil_not_smoothed = seed_foil

! Check curvature constraints like reversals -------------------------

  if(curv_spec%check_curvature) then

    write(*,'(/," - ",A)') "Check_curvature if it's suitable for Hicks-Henne shape type"

    call check_and_smooth_surface (show_details, curv_spec%do_smoothing, seed_foil, overall_quality)

  ! Get best values fur surface constraints 

    if (curv_spec%auto_curvature) &
      call auto_curvature_constraints (show_details, seed_foil)

  ! Final check for curvature reversals

    call check_handle_curve_violations ('Top surface', seed_foil%xt, seed_foil%zt, curv_top_spec)
    call check_handle_curve_violations ('Bot surface', seed_foil%xb, seed_foil%zb, curv_bot_spec)
    
  ! Final check trailing edge 

    call check_te_curvature_violations ('Top surface', seed_foil%xt, seed_foil%zt, curv_top_spec)
    call check_te_curvature_violations ('Bot surface', seed_foil%xb, seed_foil%zb, curv_bot_spec)

  elseif (curv_spec%do_smoothing) then

  ! In case of 'camb-thick' smoothing was activated 

      call check_and_smooth_surface (show_details, curv_spec%do_smoothing, seed_foil, overall_quality)

  end if

  


! Check geometry ------------------------------------------------------------------

  xt = seed_foil%xt                
  xb = seed_foil%xb
  zt = seed_foil%zt
  zb = seed_foil%zb
  nptt = size(xt,1)
  nptb = size(xb,1)


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
    return        ! end here with checks as it becomes aero specific, calc scale
  end if 


  if(check_geometry) then

    write(*,'(/," - ",A)') 'Checking to make sure seed airfoil passes all constraints ...'
                                  
  ! Top surface growth rates

    growth_allowed = 0.d0

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

  end if        ! check_geometry


! Check for bad combinations of operating conditions and optimization types

  do i = 1, noppoint

    write(text,*) i
    text = adjustl(text)

    op_spec  = op_points_spec(i) 
    opt_type = op_spec%optimization_type
    if ((op_spec%value <= 0.d0) .and. (op_spec%spec_cl)) then
      if ( (trim(opt_type) /= 'min-drag') .and.                                &
           (trim(opt_type) /= 'max-xtr') .and.                                 &
            ! jx-mod - allow target and min-lift-slope, min-glide-slope
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


  ! Analyze airfoil at requested operating conditions with Xfoil

  write (*,'(" - ",A)') 'Analyze seed airfoil at requested operating points'

! Re-Init boundary layer at each op point to ensure convergence (slower)
  local_xfoil_options = xfoil_options
  local_xfoil_options%reinitialize        = .true.  

  call run_op_points (seed_foil, xfoil_geom_options, xfoil_options,        &
                      flap_spec, flap_degrees, &
                      op_points_spec, op_points_result)



  ! Further geo constraints check with xfoil values

  if(check_geometry .or. (size(geo_targets) > 0)) then
  ! get airfoil geometry info from xfoil    
    call xfoil_set_airfoil (seed_foil)        
    call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)
  end if 

  if(check_geometry ) then
  ! Too large panel angles
    maxpanang = xfoil_geometry_amax() 
    if (maxpanang > 30.d0) then
      write(text,'(F8.4)') maxpanang
      text = adjustl(text)
      write(*,*) "Max panel angle: "//trim(text)
      call ask_stop("Seed airfoil panel angles are too large. Try adjusting "//&
                    "xfoil_paneling_options.")
    end if
  ! Free memory
    deallocate(x_interp)
    deallocate(zt_interp)
    deallocate(zb_interp)
    deallocate(thickness)
  end if


! Evaluate seed value of geomtry targets and scale factor 
    
  do i = 1, size(geo_targets)

    select case (trim(geo_targets(i)%type))
      case ('Thickness')                 ! take foil thickness calculated above
        seed_value = maxt
        ref_value  = maxt
        correction = 1.5d0                 ! thickness is less sensible to changes
      case ('Camber')                    ! take xfoil camber from  above
        seed_value = maxc
        ref_value  = maxc
        correction = 0.70               ! camber is quite sensible to changes
      case default
        call my_stop("Unknown target_type '"//trim(geo_targets(i)%type))
    end select

    geo_targets(i)%seed_value      = seed_value
    geo_targets(i)%reference_value = ref_value

    ! target value negative?  --> take current seed value * |target_value| 
    if (geo_targets(i)%target_value <= 0.d0)      &
        geo_targets(i)%target_value = seed_value * abs(geo_targets(i)%target_value)
    
    tar_value = geo_targets(i)%target_value

    ! will scale objective to 1 ( = no improvement) 
    geo_targets(i)%scale_factor = 1 / ( ref_value + abs(tar_value - seed_value) & 
                                                    * correction)

  end do 


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
      call my_stop( "Error: operating point "//trim(text)//" has Cl <= 0. "//     &
                 "Cannot use "//trim(opt_type)//" optimization "// &
                 "in this case.")
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
      seed_value = op%cd
      if (op_spec%target_value < 0.d0) &
          op_spec%target_value = seed_value * abs(op_spec%target_value)

      dist = ABS (op_spec%target_value - op%cd)
      if (dist < 0.000004d0) dist = 0d0  ! little threshold to achieve target

      checkval   = op_spec%target_value + dist

    elseif (trim(opt_type) == 'target-glide') then
      seed_value = op%cl / op%cd
      if (op_spec%target_value < 0.d0) &
          op_spec%target_value = seed_value * abs(op_spec%target_value)

      dist = ABS (op_spec%target_value - seed_value)
      if (dist < 0.001d0) dist = 0d0   ! little threshold to achieve target
      correction = 0.7d0               ! glide ration is quite sensible to changes
      checkval   = op_spec%target_value + dist * correction

    elseif (trim(opt_type) == 'target-lift') then
      seed_value = op%cl
      if (op_spec%target_value < 0.d0) &
          op_spec%target_value = seed_value * abs(op_spec%target_value)
      ! add a constant base value to the lift difference so the relative change won't be to high
      correction = 0.8d0               ! lift is quite sensible to changes
      checkval   = 1.d0 + ABS (op_spec%target_value - op%cl) * correction

    elseif (trim(opt_type) == 'target-moment') then
      seed_value = op%cm
      if (op_spec%target_value < 0.d0) &
          op_spec%target_value = seed_value * abs(op_spec%target_value) 
      ! add a base value (Clark y or so ;-) to the moment difference so the relative change won't be to high
      checkval   = ABS (op_spec%target_value - op%cm) + 0.05d0

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

subroutine check_and_smooth_surface (show_details, do_smoothing, foil, overall_quality)

  use vardef,             only: airfoil_type
  use airfoil_evaluation, only: curv_top_spec, curv_bot_spec
  use airfoil_operations, only: assess_surface, smooth_it
  use airfoil_operations, only: get_max_te_curvature, rebuild_airfoil

  logical, intent(in)       :: show_details, do_smoothing
  type (airfoil_type), intent (inout)  :: foil
  integer, intent(out)       :: overall_quality

  integer             :: top_quality, bot_quality, istart, iend, iend_spikes
  character (80)      :: text1, text2
  logical             :: done_smoothing
  doubleprecision     :: curv_threshold, spike_threshold

  done_smoothing        = .false.

!  ------------ analyze & smooth  top -----

  curv_threshold  = curv_top_spec%curv_threshold
  spike_threshold = curv_top_spec%spike_threshold
  istart          = curv_top_spec%nskip_LE
  iend            = size(foil%xt) - curv_top_spec%nskip_TE_revers
  iend_spikes     = size(foil%xt) - curv_top_spec%nskip_TE_spikes

  if (show_details) then 
    write (*,'(3x)', advance = 'no') 
    call print_colored (COLOR_NOTE, 'Using curv_threshold =')
    call print_colored_r (5,'(F5.2)', Q_BAD, curv_threshold) 
    call print_colored (COLOR_NOTE, ', spike_threshold =')
    call print_colored_r (5,'(F5.2)', Q_BAD, spike_threshold) 
    call print_colored (COLOR_NOTE, ' for detection')
    write (*,*)
  end if

  top_quality = 0
  bot_quality = 0
 
  call assess_surface (show_details, '- Top side', &
                       istart, iend, iend_spikes, &
                       curv_threshold, spike_threshold, foil%xt, foil%zt, top_quality)

  if (top_quality >= Q_BAD .or. do_smoothing) then 

    call smooth_it (.false., spike_threshold, foil%xt, foil%zt)
    top_quality     = 0 
    done_smoothing  = .true.
    call assess_surface (show_details, 'smoothed', &
                         istart, iend, iend_spikes, &
                         curv_threshold, spike_threshold, foil%xt, foil%zt, top_quality)
  end if

!  ------------ analyze & smooth  bot -----

  curv_threshold  = curv_bot_spec%curv_threshold
  spike_threshold = curv_bot_spec%spike_threshold
  istart          = curv_bot_spec%nskip_LE
  iend            = size(foil%xb) - curv_bot_spec%nskip_TE_revers
  iend_spikes     = size(foil%xb) - curv_bot_spec%nskip_TE_spikes

  call assess_surface (show_details, '- Bot side', &
                       istart, iend, iend_spikes, &
                       curv_threshold, spike_threshold, foil%xb, foil%zb, bot_quality)
  
  if (bot_quality >= Q_BAD .or. do_smoothing) then 

    call smooth_it (.false., spike_threshold, foil%xb, foil%zb)
    bot_quality     = 0 
    done_smoothing  = .true.
    call assess_surface (show_details, 'smoothed', &
                         istart, iend, iend_spikes, &
                         curv_threshold, spike_threshold, foil%xb, foil%zb, bot_quality)
  end if

  overall_quality = int((top_quality + bot_quality)/2)

! When smoothed - Rebuild foil out of smoothed polyline 
  if (done_smoothing) then
    call rebuild_airfoil ((foil%xt), (foil%xb), (foil%zt), (foil%zb), foil)
  end if 

! ... printing stuff 

  if (show_details) then
    call print_colored (COLOR_NOTE, '   ')
  else
    if (done_smoothing .and. .not. do_smoothing) then
      call print_colored (COLOR_NORMAL, '   Smoothing airfoil due to bad surface quality')
      write (*,*)
    Elseif (done_smoothing .and. do_smoothing) then
      call print_colored (COLOR_NOTE, '   Smoothing airfoil')
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
    if (done_smoothing) then
      call print_colored (COLOR_NOTE,'Even the surface quality of the smoothed airfoil is not good. ')
    else
      call print_colored (COLOR_NOTE,'The surface quality of the airfoil is not good. ')
    end if
    call print_colored (COLOR_WARNING,'Better choose another seed foil ...')
  else
    call print_colored (COLOR_ERROR,' The'//trim(text1)//' surface is'//trim(text2)//' not really suitable for optimization')
  end if  
  write (*,*) 

end subroutine


!-------------------------------------------------------------------------
! Evaluates and sets the best values for surface thresholds and constraints
!-------------------------------------------------------------------------

subroutine auto_curvature_constraints (show_details, foil)

  use vardef,             only: airfoil_type
  use airfoil_evaluation, only: curv_top_spec, curv_bot_spec

  type (airfoil_type), intent (inout)  :: foil
  logical, intent (in)            :: show_details
  
  double precision, dimension(:), allocatable :: xt, xb, yt, yb

  write (*,'(" - ", A)') 'Auto_curvature: Best values for curvature constraints'

  xt = foil%xt
  xb = foil%xb
  yt = foil%zt
  yb = foil%zb

! -------------- Top Side -----

  if (show_details) call print_note_only ('- Top side',3)

  call auto_curvature_threshold_polyline ('top', show_details, xt, yt, curv_top_spec)
  call auto_spike_threshold_polyline     (       show_details, xt, yt, curv_top_spec)
  call auto_te_curvature_polyline        (       show_details, xt, yt, curv_top_spec)
  
! -------------- Top Side -----
                            
  if (show_details) call print_note_only ('- Bop side',3)

  call auto_curvature_threshold_polyline ('bot', show_details, xb, yb, curv_bot_spec)
  call auto_spike_threshold_polyline     (       show_details, xb, yb, curv_bot_spec)
  call auto_te_curvature_polyline        (       show_details, xb, yb, curv_bot_spec)
 
end subroutine auto_curvature_constraints


!-------------------------------------------------------------------------
! Evaluates the best value for curvature thresholds of polyline
!    depending on max_curv_reverse defined by user 
!-------------------------------------------------------------------------

subroutine auto_curvature_threshold_polyline (info, show_details, x,y , c_spec)

  use math_deps,          only: min_threshold_for_reversals, count_reversals
  use math_deps,          only: count_spikes
  use airfoil_evaluation, only: curvature_polyline_specification_type, NSKIP_TE

  character(*),     intent(in) :: info
  logical, intent (in)         :: show_details
  double precision, dimension (:), intent(in) :: x,y 
  type (curvature_polyline_specification_type), intent (inout)  :: c_spec

  double precision    :: curv_threshold
  integer             :: max_curv_reverse
  double precision    :: min_threshold, max_threshold 
  integer             :: istart, iend, nreversals, quality_threshold
  character (2)       :: s2a, s2b
  character (16)      :: label

  min_threshold = 0.01d0
  max_threshold = 4.0d0

  max_curv_reverse = c_spec%max_curv_reverse
  curv_threshold   = c_spec%curv_threshold

! Is there a reversal close to TE? - which is quite often ...If yes, skip TE for ever
  
  istart = size(x) - NSKIP_TE
  iend   = size(x)  
  if (count_reversals (istart, iend, x, y, curv_threshold)  > 0) then 
    c_spec%nskip_TE_Revers = NSKIP_TE
    if (show_details) &
      call print_note ('Found reversal close to trailing edge. ' //&
                       'TE will be ignored for reversal detection.', 9)
  end if

! How many reversals do we have now? more than user specified as a constraint?

  istart = c_spec%nskip_LE
  iend   = size(x) - c_spec%nskip_TE_revers
  nreversals = count_reversals (istart, iend, x, y, curv_threshold)

  if (nreversals > max_curv_reverse) then
    write(s2a,'(I2)') nreversals
    write(s2b,'(I2)') max_curv_reverse
    call print_warning ( &
        'The current seed airfoil has '// trim(adjustl(s2a)) // ' reversals on '//trim(info)//&
        ' - but max_curv_reverse_'//trim(info)//' ist set to '//trim(adjustl(s2b)), 9)
    call print_note_only ( &
        'This will lead to a high curvature threshold value to fulfil this constraint.', 9)
    call print_note_only (& 
        'Better choose another seed airfoil which fits to the reversal constraints.', 9)
    write (*,*)  
    ! Call ask_stop ('')
  end if 

! now get smallest threshold for max_reversals defined by user 

  curv_threshold = min_threshold_for_reversals (istart, iend, x, y, &
                             min_threshold, max_threshold, max_curv_reverse)
  
! ... and give a little more threshold to breeze
  curv_threshold = curv_threshold * 1.2d0     

  c_spec%curv_threshold = curv_threshold

! Print it all 

  if (show_details) then 
    quality_threshold  = r_quality (curv_threshold, 0.015d0, 0.03d0, 0.2d0)
    label = 'curv_threshold'
    call print_colored (COLOR_PALE, '         '//label//' =') 
    call print_colored_r (5,'(F5.2)', quality_threshold, curv_threshold) 
    if (quality_threshold > Q_BAD) then
      call print_note_only ('The contour will have some reversals within this high treshold', 3)
    else
      write(s2a,'(I2)') max_curv_reverse
      call print_note_only ('Optimal value for '//trim(adjustl(s2a))//&
                            ' reversals based on seed airfoil',3)
    end if 
  end if 

end subroutine auto_curvature_threshold_polyline

!-------------------------------------------------------------------------
! Evaluates the best value for curvature thresholds of polyline
!    depending on spike_threshold defined by user 
!-------------------------------------------------------------------------

subroutine auto_spike_threshold_polyline (show_details, x,y , c_spec)

  use math_deps,          only: min_threshold_for_spikes, count_spikes
  use airfoil_evaluation, only: curvature_polyline_specification_type, NSKIP_TE

  logical, intent (in)         :: show_details
  double precision, dimension (:), intent(in) :: x,y 
  type (curvature_polyline_specification_type), intent (inout)  :: c_spec

  double precision    :: spike_threshold
  double precision    :: min_threshold, max_threshold, ok_threshold
  integer             :: istart, iend, nspikes, quality_threshold, ok_nspikes
  character (3)       :: s3
  character (16)      :: label

  min_threshold = 0.1d0
  max_threshold = 1.0d0
  ok_threshold  = 0.3d0
  ok_nspikes    = 5
  spike_threshold = c_spec%spike_threshold

! Is there a spike close to TE? - which is quite often ...If yes, skip TE for ever

  istart = size(x) - NSKIP_TE
  iend   = size(x)  
  if (count_spikes (istart, iend, x, y, spike_threshold)  > 0) then 
    c_spec%nskip_TE_spikes = NSKIP_TE
    if (show_details) & 
      call print_note ('Found spike close to trailing edge. ' //&
     'TE will be ignored for bump detection.', 9)
  end if


! How many Spikes do we have with current threshold defined by user / default?
  istart = c_spec%nskip_LE
  iend   = size(x) - c_spec%nskip_TE_spikes
  nspikes = count_spikes (istart, iend, x, y, spike_threshold)
  ! write (*,*) '------ ', istart, iend , spike_threshold, nspikes

! now get smallest threshold to achieve this number of spikes
  spike_threshold = min_threshold_for_spikes (istart, iend, x, y, &
                    min_threshold, max_threshold, nspikes)

! ... and give a little more threshold to breeze
  c_spec%spike_threshold = spike_threshold * 1.2d0 

! Max Spikes - allow at least max_curv_reverse or seed spikes as max number of spikes 

  nspikes = max (nspikes, c_spec%max_curv_reverse)
  ! ... overwrite from input file by user? 
  c_spec%max_spikes = max (nspikes, c_spec%max_spikes)

! activate bump detection (reversal of 3rd derivative) if values are ok

  if (c_spec%spike_threshold <= ok_threshold .and. c_spec%max_spikes <= ok_nspikes) then 
    c_spec%check_curvature_bumps = .true.
  else
    c_spec%check_curvature_bumps = .false.
  end if

! Print it all 

  if (show_details) then 
    quality_threshold  = r_quality (c_spec%spike_threshold, (min_threshold * 1.2d0), ok_threshold, 0.8d0)
    label = 'spike_threshold'
    call print_colored (COLOR_PALE, '         '//label//' =') 
    call print_colored_r (5,'(F5.2)', quality_threshold, c_spec%spike_threshold) 

    if (c_spec%max_spikes == 0) then 
      call print_colored (COLOR_NOTE, '   There will be no spikes.')
    else
      write (s3,'(I3)') c_spec%max_spikes
      call print_colored (COLOR_NOTE, '   There will be max '//trim(adjustl(s3)) //' spike(s).')
    end if

    if (c_spec%check_curvature_bumps) then 
      call print_colored (COLOR_NOTE, " Good value - activating ")
      call print_colored (COLOR_FEATURE, "bump detetction ...")
      write (*,*)
    else
      call print_note_only ("Values not good enough for auto bump detetction", 1)
    end if 
  end if 

end subroutine auto_spike_threshold_polyline



!-------------------------------------------------------------------------
! Evaluates the best value for curvature at TE of polyline
!-------------------------------------------------------------------------

subroutine auto_te_curvature_polyline (show_details, x,y , c_spec)

  use airfoil_operations, only: get_max_te_curvature
  use airfoil_evaluation, only: curvature_polyline_specification_type

  logical, intent (in)         :: show_details
  double precision, dimension (:), intent(in) :: x,y 
  type (curvature_polyline_specification_type), intent (inout)  :: c_spec

  double precision            :: max_te_curvature 
  integer                     :: quality_te
  character (16)              :: label

  max_te_curvature = get_max_te_curvature (x, y) * 1.2d0       ! little more to breath during opt.

  c_spec%max_te_curvature = max_te_curvature

! Print it all 

  if (show_details) then 
    quality_te      = r_quality (max_te_curvature, 0.2d0, 1d0, 10d0)
    label = 'max_te_curvature'
    call print_colored (COLOR_PALE, '         '//label//' =') 
    call print_colored_r (5,'(F5.2)', quality_te, max_te_curvature) 
    if (quality_te > Q_BAD) then
      call print_note_only ('Like seed airfoil the airfoil will have a geometric spoiler at TE', 3)
    else
      call print_note_only ('Smallest value based on seed airfoil trailing edge curvature',3)
    end if 
  end if 

end subroutine auto_te_curvature_polyline


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
  
    write (*,*) 
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


!------------------------------------------------------------------------------
! Asks user to turn off forced transition
!------------------------------------------------------------------------------

function ask_forced_transition()

  character :: ask_forced_transition
  logical :: valid_choice

! Get user input

  valid_choice = .false.
  do while (.not. valid_choice)
  
    write(*,*)
    write(*,'(A)') 'Warning: using max-xtr optimization but xtript or xtripb'
    write(*,'(A)', advance='no') 'is less than 1. Set them to 1 now? (y/n): '
    read(*,'(A)') ask_forced_transition

    if ( (ask_forced_transition == 'y') .or.                                  &
         (ask_forced_transition == 'Y') ) then
      valid_choice = .true.
      ask_forced_transition = 'y'
      write(*,*)
      write(*,*) "Setting xtript and xtripb to 1."
    else if ( (ask_forced_transition == 'n') .or.                             &
         (ask_forced_transition == 'N') ) then
      valid_choice = .true.
      ask_forced_transition = 'n'
    else
      write(*,'(A)') 'Please enter y or n.'
      valid_choice = .false.
    end if

  end do

end function ask_forced_transition

!-----------------------------------------------------------------------------
! Checks surface x,y for violations of curvature contraints 
!     reversals > max_curv_reverse
! and handles user response  
!-----------------------------------------------------------------------------

subroutine  check_handle_curve_violations (info, x, y, c)

  use airfoil_evaluation, only : curvature_polyline_specification_type
  use airfoil_operations, only : show_reversals_highlows
  use math_deps, only : count_reversals


  character(*),                   intent(in) :: info
  double precision, dimension(:), intent(in) :: x, y
  type(curvature_polyline_specification_type), intent(in) :: c

  integer :: n, max_rev, nreverse_violations, istart, iend, nreverse

  istart = c%nskip_LE
  iend   = size(x) - c%nskip_TE_revers

! How many reversals?  ... 
  nreverse = count_reversals (istart, iend, x, y, c%curv_threshold)  
  nreverse_violations  = max(0,(nreverse - c%max_curv_reverse))

  ! Exit if everything is ok 
  if (nreverse_violations > 0) then 

    write (*,*)
    call print_warning ("Curvature violations on " // trim(info))
    write (*,*)

    if (nreverse_violations > 0) then 
      n       = nreverse_violations + c%max_curv_reverse
      max_rev = c%max_curv_reverse
      write (*,'(10x,A,I2,A,I2)')"Found ",n, " Reversal(s) where max_curv_reverse is set to ", max_rev
    end if 

    write (*,*)
    write (*,'(10x,A)') 'The Optimizer may not found a solution with this inital violation.'
    write (*,'(10x,A)') 'Either increase max_... or ..._threshold (not recommended) or'
    write (*,'(10x,A)') 'choose another seed airfoil. Find details in geometry plot of the viszualizer.'
    call ask_stop('')
  end if

end subroutine check_handle_curve_violations

!-----------------------------------------------------------------------------
! Checks trailing edga curvature x,y for violations max_te_crvature
! and handles user response  
!-----------------------------------------------------------------------------

subroutine  check_te_curvature_violations (info, x, y, c)

  use airfoil_evaluation, only : curvature_polyline_specification_type
  use airfoil_operations, only : get_max_te_curvature
  use math_deps, only : count_reversals


  character(*),                   intent(in) :: info
  double precision, dimension(:), intent(in) :: x, y
  type(curvature_polyline_specification_type), intent(in) :: c

  double precision  :: cur_te_curvature
  character (8)     :: text

  cur_te_curvature = get_max_te_curvature (x, y)
  if (cur_te_curvature  > c%max_te_curvature) then 
    write(text,'(F8.4)') cur_te_curvature
    call ask_stop("Curvature of "//trim(adjustl(text))// &
                  " on "//trim(info)//" at trailing edge violates max_te_curvature constraint.")
  end if 

end subroutine check_te_curvature_violations

!-----------------------------------------------------------------------------
! Set airfoil thickness and camber according to defined geo targets 
!   and/or thickness/camber constraints
!-----------------------------------------------------------------------------

subroutine preset_airfoil_to_targets (show_details, foil, geo_targets, &
                                min_thickness, max_thickness, min_camber, max_camber) 

  use vardef,             only: airfoil_type
  use airfoil_evaluation, only: geo_target_type
  use xfoil_driver,       only: xfoil_set_thickness_camber, xfoil_set_airfoil
  use xfoil_driver,       only: xfoil_get_geometry_info

  logical, intent (in)           :: show_details 
  type (airfoil_type), intent (inout)  :: foil
  type (geo_target_type), dimension(:), intent (in)  :: geo_targets
  doubleprecision, intent(inout) :: min_thickness, max_thickness, min_camber, max_camber

  type (airfoil_type) :: new_foil
  doubleprecision     :: maxt, xmaxt, maxc, xmaxc, new_camber, new_thick
  character (10)      :: cvalue
  integer             :: i, nptt, nptb, ngeo_targets
  logical             :: foil_changed 

  foil_changed = .false.
  ngeo_targets = size(geo_targets)

  new_thick  = 0d0
  new_camber = 0d0

  if (ngeo_targets > 0) then 

  ! Set thickness / Camber of seed airfoil according geo targets, adjust constraints

    do i= 1, ngeo_targets

      select case (trim(geo_targets(i)%type))

        case ('Thickness')                   

          new_thick = geo_targets(i)%target_value
          foil_changed = .true.

          if (show_details) then
            write (cvalue,'(F6.2)')  (new_thick * 100)
            call print_note_only ('- Scaling thickness to target value '// trim(adjustl(cvalue))//'%')
          end if

        case ('Camber')                      

          new_camber = geo_targets(i)%target_value
          foil_changed = .true.

          if (show_details) then
            write (cvalue,'(F6.2)')  (new_camber * 100)
            call print_note_only ('- Scaling camber to target value '// trim(adjustl(cvalue))//'%')
          end if

      end select

    end do
    call xfoil_set_thickness_camber (foil, new_thick, 0d0, new_camber, 0d0, new_foil)

  else

  ! Set thickness / Camber of seed airfoil according constraints

    call xfoil_set_airfoil (foil)        
    call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)

    if (maxt > max_thickness) then

      new_thick = max_thickness *0.95d0
      call xfoil_set_thickness_camber (foil, new_thick, 0d0, 0d0, 0d0, new_foil)
      foil_changed = .true.

      if (show_details) then
        write (cvalue,'(F6.2)')  (new_thick * 100)
        call print_note_only ('- Scaling thickness according constraint to '// trim(adjustl(cvalue))//'%')
      end if 

    elseif (maxt < min_thickness) then 

      new_thick = min_thickness *1.05d0
      call xfoil_set_thickness_camber (foil, new_thick, 0d0, 0d0, 0d0, new_foil)
      foil_changed = .true.

      if (show_details) then
        write (cvalue,'(F6.2)')  (new_thick * 100)
        call print_note_only ('- Scaling thickness according constraint to '// trim(adjustl(cvalue))//'%')
      end if 

    end if 

    if (maxc > max_camber) then

      new_camber = max_camber *0.95d0
      call xfoil_set_thickness_camber (foil, 0d0, new_camber, 0d0, 0d0, new_foil)
      foil_changed = .true.

      if (show_details) then
        write (cvalue,'(F6.2)')  (new_camber * 100)
        call print_note_only ('- Scaling camber according constraint to '// trim(adjustl(cvalue))//'%')
      end if
      
    elseif (maxc < min_camber) then 

      new_camber = min_camber *1.05d0
      call xfoil_set_thickness_camber (foil, 0d0, new_camber, 0d0, 0d0, new_foil)
      foil_changed = .true.

      if (show_details) then
        write (cvalue,'(F6.2)')  (new_camber * 100)
        call print_note_only ('- Scaling camber according constraint to '// trim(adjustl(cvalue))//'%')
      end if

    end if 

  end if

  if (foil_changed) then

! Now rebuild foil out of new coordinates  ----------------------
  ! Sanity check - new_foil may not have different number of points
    if (foil%npoint /= new_foil%npoint) then
      call my_stop ('Number of points changed during thickness/camber modification')
    end if

    foil%z = new_foil%z
    nptt = size(foil%zt,1)
    nptb = size(foil%zb,1)

  ! get new upper and lower z-coordinates from modified airfoil 
    do i = 1, nptt
      foil%zt(i) = new_foil%z(nptt-i+1)      ! start from LE - top reverse - to LE
    end do
    do i = 1, nptb 
      foil%zb(i) = new_foil%z(nptt+i-1)      ! start from LE - bottom - to TE
    end do
  end if
  
end subroutine preset_airfoil_to_targets


!-----------------------------------------------------------------------------
! Check airfoil thickness and camber according constraints
!-----------------------------------------------------------------------------

subroutine check_thickness_camber (foil,  &
                                min_thickness, max_thickness, min_camber, max_camber) 

  use vardef,             only: airfoil_type
  use xfoil_driver,       only: xfoil_set_thickness_camber, xfoil_set_airfoil
  use xfoil_driver,       only: xfoil_get_geometry_info


  type (airfoil_type), intent (inout)  :: foil
  doubleprecision, intent(inout) :: min_thickness, max_thickness, min_camber, max_camber

  doubleprecision     :: maxt, xmaxt, maxc, xmaxc
  character (10)      :: text

  call xfoil_set_airfoil (foil)        
  call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)

  if (maxt < min_thickness) then
    write(text,'(F8.4)') maxt
    text = adjustl(text)
    call ask_stop("Seed airfoil violates min_thickness constraint with thickness: "//trim(text))
  end if

  if (maxt > max_thickness) then
    write(text,'(F8.4)') maxt
    text = adjustl(text)
    call ask_stop("Seed airfoil violates max_thickness constraint with thickness: "//trim(text))
  end if

  if (maxc > max_camber) then
    write(text,'(F8.4)') maxc
    text = adjustl(text)
    call ask_stop("Seed airfoil violates max_camber constraint with camber: "//trim(text))
  end if

  if (maxc < min_camber) then
    write(text,'(F8.4)') maxc
    text = adjustl(text)
    call ask_stop("Seed airfoil violates min_camber constraint with camber: "//trim(text))
  end if

end subroutine check_thickness_camber


end module input_sanity

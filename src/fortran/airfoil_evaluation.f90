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

module airfoil_evaluation

! Sets up and evaluates the objective function for an airfoil design

  use vardef
  use xfoil_driver, only : xfoil_options_type, xfoil_geom_options_type

  implicit none

  public :: objective_function, objective_function_nopenalty

  public :: write_function, write_function_restart_cleanup 

  type(xfoil_options_type),      public :: xfoil_options
  type(xfoil_geom_options_type), public :: xfoil_geom_options
  
  double precision, dimension(max_op_points), public :: op_seed_value


  private 

! jx-mod "Additional infos for user info during optimization"
  type op_opt_info_type
    double precision :: obj                    ! the "objective function value" of this point
    double precision :: weighting              ! and its weighting 
    double precision :: change                 ! the relative change of the result value eg cd oder cl/cd
    character (20)   :: penalty_info           ! info string about  penalty occured for this point
  end type op_opt_info_type

  type geo_opt_info_type
    double precision :: obj                    ! the "objective function value" of this geo target
    double precision :: weighting              ! and its weighting 
    double precision :: change                 ! the relative change of the result value eg thickness
  end type geo_opt_info_type

  double precision :: best_obj_func_value = 1d0  ! keep the current best value to show improvement

  contains

!=============================================================================80
!
! Generic objective function.  Selects either aero_objective_function or
! matchfoil_objective_function depending on whether match_foils = .true. or
! not.
!
!=============================================================================80
function objective_function(designvars, evaluate_only_geometry)

  double precision, dimension(:), intent(in) :: designvars
  logical, intent(in), optional :: evaluate_only_geometry

  double precision :: objective_function

  if (match_foils) then
    objective_function = matchfoil_objective_function(designvars)
  else
    if (present(evaluate_only_geometry)) then
      objective_function = aero_objective_function(designvars, &
         include_penalty=.true., evaluate_only_geometry=evaluate_only_geometry)
    else
      objective_function = aero_objective_function(designvars)
    end if
  end if

end function objective_function

!=============================================================================80
!
! Objective function with option to not add penalty value (used for seed
! airfoil)
!
!=============================================================================80
function objective_function_nopenalty(designvars)

  double precision, dimension(:), intent(in) :: designvars
  double precision :: objective_function_nopenalty

  if (match_foils) then
    objective_function_nopenalty = matchfoil_objective_function(designvars)
  else
    objective_function_nopenalty =                                             &
                    aero_objective_function(designvars, include_penalty=.false.)
  end if

end function objective_function_nopenalty


!=============================================================================80
!
!  Objective function
!
!  Input: design variables (modes for top and bottom shape functions)
!         include_penalty: optional input to enable/disable penalty function
!  Output: objective function value based on airfoil performance
!
!=============================================================================80
function aero_objective_function(designvars, include_penalty, evaluate_only_geometry)

  use math_deps,       only : interp_vector, curvature, derv1f1, derv1b1

  ! jx-mod 
  use airfoil_operations, only : assess_surface, smooth_it,        &
                                 show_camb_thick_of_current
  use airfoil_operations, only : my_stop
  use math_deps,          only : interp_point, derivation_at_point

  use parametrization, only : top_shape_function, bot_shape_function,          &
                              create_airfoil, create_airfoil_camb_thick
  use xfoil_driver,    only : run_xfoil, xfoil_geometry_amax,                  &
                              xfoil_geometry_info, xfoil_set_airfoil

  double precision, dimension(:), intent(in) :: designvars
  logical, intent(in), optional :: include_penalty, evaluate_only_geometry
  double precision :: aero_objective_function

  double precision, dimension(max(size(xseedt,1),size(xseedb,1))) :: x_interp, &
                                               zt_interp, zb_interp, thickness
  double precision, dimension(size(xseedt,1)) :: zt_new
  double precision, dimension(size(xseedb,1)) :: zb_new
  double precision, dimension(size(xseedt,1)) :: curvt
  double precision, dimension(size(xseedb,1)) :: curvb
  double precision, dimension(naddthickconst) :: add_thickvec
  integer :: nmodest, nmodesb, nptt, nptb, i, dvtbnd1, dvtbnd2, dvbbnd1,       &
             dvbbnd2, nptint
  double precision :: penaltyval
  double precision :: tegap, growth1, growth2, maxgrowth, len1, len2
  double precision :: maxt, xmaxt, maxc, xmaxc
  double precision :: panang1, panang2, maxpanang, heightfactor, cur_te_curvature
  double precision, dimension(noppoint) :: lift, drag, moment, viscrms, alpha, &
                                           xtrt, xtrb
  double precision, dimension(noppoint) :: actual_flap_degrees
  double precision :: increment, curv1, curv2
  integer :: nreversalst, nreversalsb, ndvs
  double precision :: gapallow, maxthick, ffact
  integer :: flap_idx, dvcounter
  double precision, parameter :: epsexit = 1.0D-04
  double precision, parameter :: epsupdate = 1.0D-08
  double precision :: pi
  logical :: penalize, only_geo_penalty

  double precision :: geo_objective_function
  double precision :: ref_value, tar_value, cur_value, slope
  character(100)   :: penalty_info
  type(op_opt_info_type),  dimension (max_op_points) :: op_opt_info
  type(geo_opt_info_type), dimension (max_op_points) :: geo_opt_info
  double precision :: perturbation_bot, perturbation_top

! Enable / disable penalty function

  penalize = .true.
  if (present(include_penalty)) then
    if (.not. include_penalty) penalize = .false.
  end if

! Enable / disable penalty function

  if (present(evaluate_only_geometry)) then
    only_geo_penalty = evaluate_only_geometry
  else
    only_geo_penalty = .false.
  end if

!----------------------------------------------------------------------------------------------------
! Build airfoil to evaluate out of seed airfoil plus shape functions applied
!----------------------------------------------------------------------------------------------------


  pi = acos(-1.d0)
  nmodest = size(top_shape_function,1)
  nmodesb = size(bot_shape_function,1)
  nptt = size(xseedt,1)
  nptb = size(xseedb,1)

! Set modes for top and bottom surfaces

  if (trim(shape_functions) == 'naca') then
    dvtbnd1 = 1
    dvtbnd2 = nmodest
    dvbbnd2 = nmodest + nmodesb
    dvbbnd1 = dvtbnd2 + 1
  else if (trim(shape_functions) == 'camb-thick') then
    dvtbnd1 = 1
    dvtbnd2 = nmodest
    dvbbnd1 = 1
    dvbbnd2 = dvtbnd2
  else
    dvtbnd1 = 1
    dvtbnd2 = nmodest*3
    dvbbnd2 = nmodest*3 + nmodesb*3
    dvbbnd1 = dvtbnd2 + 1
  end if
  
! Overwrite lower DVs for symmetrical airfoils or camb-thickness-shaping
! (they are not used)

  if (symmetrical) then
    dvbbnd1 = 1
    dvbbnd2 = dvtbnd2
  end if

  if (trim(shape_functions) == 'camb-thick') then
    ! Create new airfoil by changing camber and thickness of seed airfoil.
    call create_airfoil_camb_thick(xseedt, zseedt, xseedb, zseedb,             &
                      designvars(dvtbnd1:dvtbnd2), zt_new, zb_new)
  else 
    ! Create top and bottom surfaces by perturbation of seed airfoil 
    call create_airfoil(xseedt, zseedt, xseedb, zseedb,                        &
                      designvars(dvtbnd1:dvtbnd2), designvars(dvbbnd1:dvbbnd2),&
                      zt_new, zb_new, shape_functions, symmetrical)
  end if


! Format coordinates in a single loop in derived type. Also remove translation
! and scaling to ensure Cm_x=0.25 doesn't change.

  do i = 1, nptt
    curr_foil%x(i) = xseedt(nptt-i+1)/foilscale - xoffset
    curr_foil%z(i) = zt_new(nptt-i+1)/foilscale - zoffset
  end do
  do i = 1, nptb-1
    curr_foil%x(i+nptt) = xseedb(i+1)/foilscale - xoffset
    curr_foil%z(i+nptt) = zb_new(i+1)/foilscale - zoffset
  end do


!----------------------------------------------------------------------------------------------------
! Check geometry contraints  - resulting in penalties added to objective function 
!----------------------------------------------------------------------------------------------------


  penaltyval   = 0.d0
  penalty_info = ''                            ! user info on the type of penalities given

  maxgrowth = 0.d0

  len1 = sqrt((curr_foil%x(2)-curr_foil%x(1))**2.d0 +                          &
              (curr_foil%z(2)-curr_foil%z(1))**2.d0)
  do i = 2, nptt + nptb - 2
    len2 = sqrt((curr_foil%x(i+1)-curr_foil%x(i))**2.d0 +                      &
                (curr_foil%z(i+1)-curr_foil%z(i))**2.d0)
    growth1 = len2/len1
    growth2 = len1/len2
    if (max(growth1,growth2) > maxgrowth) maxgrowth = max(growth1,growth2)
    len1 = len2
  end do

! Penalty for too large growth rate

  penaltyval = penaltyval + max(0.d0,maxgrowth-growth_allowed)/1.d0
  if (max(0.d0,maxgrowth-growth_allowed)/1.d0 > 0.d0) penalty_info = trim(penalty_info) // ' maxGrowth'

! Penalty for too blunt leading edge

  panang1 = atan((zt_new(2)-zt_new(1))/(xseedt(2)-xseedt(1))) *                &
            180.d0/acos(-1.d0)
  panang2 = atan((zb_new(1)-zb_new(2))/(xseedb(2)-xseedb(1))) *                &
            180.d0/acos(-1.d0)
  maxpanang = max(panang2,panang1)

  penaltyval = penaltyval + max(0.d0,maxpanang-89.99d0)/0.01d0
  if (max(0.d0,maxpanang-89.99d0)/0.01d0 > 0.d0) penalty_info = trim(penalty_info) // ' bluntLE'

! Penalty for too sharp leading edge

  penaltyval = penaltyval + max(0.d0,abs(panang1-panang2)-20.d0)/5.d0
  if (max(0.d0,abs(panang1-panang2)-20.d0) > 0.d0) penalty_info = trim(penalty_info) // ' sharpLE'

! Penalty for TE panel problem 
!    In the current Hicks Henne shape functions implementation, the last panel is
!    forced to become TE which can lead to a thick TE area with steep last panel(s)
!       (see create_shape ... do j = 2, npt-1 ...)
!    so the curvature (2nd derivative) at the last 10 panels is checked

  if (check_curvature) then
    cur_te_curvature = maxval (abs(curvature(11, xseedt(nptt-10:nptt), zt_new(nptt-10:nptt))))
    if (cur_te_curvature  > max_te_curvature) then 
      penalty_info = trim(penalty_info) // ' TEmaxAngle'
      penaltyval = penaltyval + cur_te_curvature
    end if 

    cur_te_curvature = maxval (abs(curvature(11, xseedb(nptb-10:nptb), zb_new(nptb-10:nptb))))
    if (cur_te_curvature  > max_te_curvature) then 
      penalty_info = trim(penalty_info) // ' TEmaxAngle'
      penaltyval = penaltyval + cur_te_curvature
    end if 
  end if 

! Interpolate bottom surface to xseedt points (to check thickness)

  if (xseedt(nptt) <= xseedb(nptb)) then
    nptint = nptt
    call interp_vector(xseedb, zb_new, xseedt, zb_interp(1:nptt))
    x_interp(1:nptt) = xseedt
    zt_interp(1:nptt) = zt_new  
  else
    nptint = nptb
    call interp_vector(xseedt, zt_new, xseedb, zt_interp(1:nptb))
    x_interp(1:nptb) = xseedb
    zb_interp(1:nptb) = zb_new
  end if

! Compute thickness parameters

  tegap = zt_new(nptt) - zb_new(nptb)
  maxthick = 0.d0
  heightfactor = tan(min_te_angle*acos(-1.d0)/180.d0/2.d0)

  do i = 2, nptint - 1

!   Thickness array and max thickness

    thickness(i) = zt_interp(i) - zb_interp(i)
    if (thickness(i) > maxthick) maxthick = thickness(i)

!   Check if thinner than specified wedge angle on back half of airfoil

    if (xseedt(i) > 0.5d0) then
      gapallow = tegap + 2.d0 * heightfactor * (x_interp(nptint) -             &
                                                x_interp(i))
      penaltyval = penaltyval + max(0.d0,gapallow-thickness(i))/0.1d0
      if (max(0.d0,gapallow-thickness(i))/0.1d0 > 0.d0) penalty_info = trim(penalty_info) // ' minTEAngle'
    end if

  end do

  if (naddthickconst > 0) then
    call interp_vector(x_interp, thickness,                                    &
                       addthick_x(1:naddthickconst), add_thickvec)

    do i = 1, naddthickconst
      penaltyval = penaltyval + max(0.d0,addthick_min(i)-add_thickvec(i))/0.1d0
      penaltyval = penaltyval + max(0.d0,add_thickvec(i)-addthick_max(i))/0.1d0
      if (max(0.d0,add_thickvec(i)-addthick_max(i))/0.1d0 > 0.d0) penalty_info = trim(penalty_info) // ' addThickMax'
      if (max(0.d0,addthick_min(i)-add_thickvec(i))/0.1d0 > 0.d0) penalty_info = trim(penalty_info) // ' addThickMin'
    end do
  end if

! Penalties for max thickness too low or high

  penaltyval = penaltyval + max(0.d0,min_thickness-maxthick)/0.1d0
  penaltyval = penaltyval + max(0.d0,maxthick-max_thickness)/0.1d0
  if (max(0.d0,min_thickness-maxthick)/0.1d0 > 0.d0) penalty_info = trim(penalty_info) // ' minThick'
  if (max(0.d0,maxthick-max_thickness)/0.1d0 > 0.d0) penalty_info = trim(penalty_info) // ' maxThick'

! Check for curvature reversals

  if (check_curvature) then

!   Compute curvature on top and bottom

    curvt = curvature(nptt, xseedt, zt_new)
    curvb = curvature(nptb, xseedb, zb_new)

!   Check number of reversals that exceed the threshold

    nreversalst = 0
    curv1 = 0.d0
    do i = 2, nptt - 1
      if (abs(curvt(i)) >= curv_threshold) then
        curv2 = curvt(i)
        if (curv2*curv1 < 0.d0) nreversalst = nreversalst + 1
        curv1 = curv2
      end if
    end do

    nreversalsb = 0
    curv1 = 0.d0
    do i = 2, nptb - 1
      if (abs(curvb(i)) >= curv_threshold) then
        curv2 = curvb(i)
        if (curv2*curv1 < 0.d0) nreversalsb = nreversalsb + 1
        curv1 = curv2
      end if
    end do

    penaltyval = penaltyval + max(0.d0,dble(nreversalst-max_curv_reverse_top))
    penaltyval = penaltyval + max(0.d0,dble(nreversalsb-max_curv_reverse_bot))
    if (max(0.d0,dble(nreversalst-max_curv_reverse_top)) > 0.d0) penalty_info = trim(penalty_info) // ' maxReversal'
    if (max(0.d0,dble(nreversalsb-max_curv_reverse_bot)) > 0.d0) penalty_info = trim(penalty_info) // ' maxReversal'

  end if

! Check that number of flap optimize points are correct

  ndvs = size(designvars,1)
  if (nflap_optimize /= (ndvs - dvbbnd2)) then
    write(*,*) "Wrong number of design variables for flap deflections."
    write(*,*) "Please report this bug."
    stop
  end if

! Get actual flap angles based on design variables
! Also add a penalty for flap deflections outside the specified bounds

  ffact = initial_perturb/(max_flap_degrees - min_flap_degrees)
  actual_flap_degrees(1:noppoint) = flap_degrees(1:noppoint)
  dvcounter = dvbbnd2 + 1
  do i = 1, nflap_optimize
    flap_idx = flap_optimize_points(i)
    actual_flap_degrees(flap_idx) = designvars(dvcounter)/ffact
    penaltyval = penaltyval +                                                  &
                 max(0.d0,actual_flap_degrees(flap_idx)-max_flap_degrees)
    penaltyval = penaltyval +                                                  &
                 max(0.d0,min_flap_degrees-actual_flap_degrees(flap_idx))
    dvcounter = dvcounter + 1
  end do


! next checks need xfoil geo routines...

  call xfoil_set_airfoil(curr_foil)
  call xfoil_geometry_info(maxt, xmaxt, maxc, xmaxc)
  maxpanang = xfoil_geometry_amax()

! Add penalty for too large panel angle
!     Due to numerical issues (?) it happens, that the final maxpanang ist greater 25.

  if (max(0.0d0,maxpanang-30.d0) > 0.d0) then
    ! strong penality - will abort 
    penaltyval = penaltyval + max(0.0d0,maxpanang-30.d0)/5.d0
  else
    if ((max(0.0d0,maxpanang-29.5d0) > 0.d0) .and. penalize) then
      ! do not check if camb-thick - it couldn't be solved...
      if (trim(shape_functions) /= 'camb-thick') then
          ! weak penalty to avoid getting too close to cut off at 25.0 
        penaltyval   = penaltyval + max(0.0d0,maxpanang-29.5d0) * 2.d0  *  epsupdate  ! max 1%
        penalty_info = trim(penalty_info) // ' MxAng'
      end if 
    end if
  end if

! Add penalty for camber outside of constraints

  penaltyval = penaltyval + max(0.d0,maxc-max_camber)/0.025d0
  penaltyval = penaltyval + max(0.d0,min_camber-maxc)/0.025d0
  if (max(0.d0,maxc-max_camber) > 0.d0) penalty_info = trim(penalty_info) // ' MxCmb'
  if (max(0.d0,min_camber-maxc) > 0.d0) penalty_info = trim(penalty_info) // ' MiCmb'


! Exit if penalties are too high (airfoil is already now a looser)
!   ... or if only geometry constraints should be checjed (for initial designs)

  if (( (penaltyval > epsexit) .and. penalize ) .or. only_geo_penalty) then
    aero_objective_function = 1d0 + penaltyval*1.0D+06
    return
  end if

!----------------------------------------------------------------------------------------------------
! Smoothing  - evaluate contribution to gow objective function 
!----------------------------------------------------------------------------------------------------

  geo_objective_function  = 0.d0                

  if (do_smoothing) then

    if (show_details .and. penalize ) then 
       write (*,*)
       call assess_surface ('Top', .true., max_curv_reverse_top, xseedt, zt_new, nreversalst, perturbation_top)
       call assess_surface ('Bot', .true., max_curv_reverse_bot, xseedb, zb_new, nreversalsb, perturbation_bot)
    else   
      ! Check surface for pertubation (ups and downs) and number of reversals
      call assess_surface ('Top', .false., max_curv_reverse_top, xseedt, zt_new, nreversalst, perturbation_top)
      call assess_surface ('Bot', .false., max_curv_reverse_bot, xseedb, zb_new, nreversalsb, perturbation_bot)
    end if 

    ! Calculate geometry objective based on the assessed quality (pertubation)
      ! add a empirical base value - pertubation can be quite volatile
    cur_value = (10.d0 + perturbation_top + perturbation_bot)
    increment = cur_value * scale_pertubation
    geo_objective_function = geo_objective_function +  increment * weighting_smoothing 
    if (show_details .and. penalize ) then
      write (*,'(31x,A,F8.2,A)') 'Contribution of surface assessment: ', (1d0 -increment) * weighting_smoothing * 100d0,'%'
      write (*,*) 
    end if 
  end if


!----------------------------------------------------------------------------------------------------
! Finally evaluate aerodynamic objective function  
!----------------------------------------------------------------------------------------------------

! Init infos for show_details entertainment 

  op_opt_info%obj          = 1d0               
  op_opt_info%weighting    = 0d0               
  op_opt_info%change       = -999d0               
  op_opt_info%penalty_info = ''    

! Analyze airfoil at requested operating conditions with Xfoil

  call run_xfoil(curr_foil, xfoil_geom_options, op_point(1:noppoint),          &
                 op_mode(1:noppoint), re(1:noppoint), ma(1:noppoint),          &
                 use_flap, x_flap, y_flap, y_flap_spec,                        &
                 actual_flap_degrees(1:noppoint), xfoil_options, lift, drag,   &
                 moment, viscrms, alpha, xtrt, xtrb, ncrit_pt)

 

! Determine if op points need to be checked for xfoil consistency
! jx-mod --> removed all re-check stuff!


! Get objective function contribution from aerodynamics 
!    (aero performance times normalized weight)

  aero_objective_function = 0.d0

  do i = 1, noppoint

!   Objective function evaluation

    if (trim(optimization_type(i)) == 'min-sink') then

!     Maximize Cl^1.5/Cd

      if (lift(i) > 0.d0) then
        increment = drag(i)/lift(i)**1.5d0*scale_factor(i)
      else
        increment = 1.D9   ! Big penalty for lift <= 0
      end if
      cur_value  = lift(i)**1.5d0 / drag(i) 

    elseif (trim(optimization_type(i)) == 'max-glide') then

!     Maximize Cl/Cd

      if (lift(i) > 0.d0) then
        increment = drag(i)/lift(i)*scale_factor(i)
      else
        increment = 1.D9   ! Big penalty for lift <= 0
      end if
      cur_value  = lift(i) / drag(i) 

    elseif (trim(optimization_type(i)) == 'min-drag') then

!     Minimize Cd

      increment = drag(i)*scale_factor(i)
      cur_value = drag(i) 

    elseif (trim(optimization_type(i)) == 'target-drag') then

! jx-mod Minimize difference between target cd value and current value 
    
      increment = (target_value(i) + ABS (target_value(i)-drag(i))) * scale_factor(i)
      cur_value = drag(i) 

    elseif (trim(optimization_type(i)) == 'target-lift') then

! jx-mod Minimize difference between target cl value and current value 
!        Add a base value to the lift difference
    
      increment = (1.d0 + ABS (target_value(i)-lift(i))) * scale_factor(i) 
      cur_value = lift(i)

    elseif (trim(optimization_type(i)) == 'target-moment') then

! jx-mod Minimize difference between target moment value and current value 
!        Add a base value (Clark y or so ;-) to the moment difference
!        so the relative change won't be to high
      increment = (ABS (target_value(i)-moment(i))+ 0.05d0)*scale_factor(i)
      cur_value = moment(i)

    elseif (trim(optimization_type(i)) == 'max-lift') then

!     Maximize Cl (at given angle of attack)

      if (lift(i) > 0.d0) then
        increment = scale_factor(i)/lift(i)
      else
        increment = 1.D9   ! Big penalty for lift <= 0
      end if
      cur_value = lift(i)

    elseif (trim(optimization_type(i)) == 'max-xtr') then

!     Maximize laminar flow on top and bottom (0.1 factor to ensure no
!     division by 0)

      increment = scale_factor(i)/(0.5d0*(xtrt(i)+xtrb(i))+0.1d0)
      cur_value = 0.5d0*(xtrt(i)+xtrb(i))

      ! jx-mod Following optimization based on slope of the curve of op_point
!         convert alpha in rad to get more realistic slope values
!         convert slope in rad to get a linear target 
!         factor eg 4.d0*pi to adjust range of objective function (not negative)

    elseif (trim(optimization_type(i)) == 'max-lift-slope') then

!     Maximize dCl/dalpha (0.1 factor to ensure no division by 0)

      slope     = derivation_at_point (noppoint, i, (alpha * pi/180.d0) , lift)
      increment = scale_factor(i) / (atan(abs(slope))  + 2.d0*pi)
      cur_value = atan(abs(slope))
      ! relative angle value changes use 90 degree as base value
      op_opt_info(i)%change = (cur_value- op_seed_value(i)) / (pi/2d0)

    elseif (trim(optimization_type(i)) == 'min-lift-slope') then

!     jx-mod  New: Minimize dCl/dalpha e.g. to reach clmax at alpha(i) 
      slope     = derivation_at_point (noppoint, i, (alpha * pi/180.d0) , lift)
      increment = scale_factor(i) * (atan(abs(slope)) + 2.d0*pi)
      cur_value = atan(abs(slope))
      ! relative angle value changes use 90 degree as base value
      op_opt_info(i)%change = (cur_value- op_seed_value(i)) / (pi/2d0)

    elseif (trim(optimization_type(i)) == 'min-glide-slope') then

!     jx-mod  New: Minimize d(cl/cd)/dcl e.g. to reach best glide at alpha(i) 
      slope     = derivation_at_point (noppoint, i,  (lift * 20d0), (lift/drag))
      increment = scale_factor(i) * (atan(abs(slope))  + 2.d0*pi)
      cur_value = atan(abs(slope))  
      ! relative angle value changes use 90 degree as base value
      op_opt_info(i)%change = (cur_value- op_seed_value(i)) / (pi/2d0)

    else

      write(*,*)
      write(*,*) "Error: requested optimization_type not recognized."
      stop

    end if

!   Add contribution to the objective function

    aero_objective_function = aero_objective_function + weighting(i)*increment

!   jx-mod Save contribution of this op_point for user entertainment
    op_opt_info(i)%obj       = increment
    op_opt_info(i)%weighting = weighting(i)
    if (op_opt_info(i)%change == -999d0)                     &
      op_opt_info(i)%change    = (cur_value / op_seed_value(i)) - 1d0

  end do

! Add penalty for unconverged points

  do i = 1, noppoint
    penaltyval = penaltyval + max(0.d0,viscrms(i)-1.0D-04)/1.0D-04
    if ( max(0.d0,viscrms(i)-1.0D-04) > 0.d0) op_opt_info(i)%penalty_info = ' Visc'
  end do

! Add penalty for too low moment

  do i = 1, noppoint
  ! jx-mod Geo targets -no penalty when special op point type 
    if (trim(optimization_type(i)) /= 'target-moment') then
      if (trim(moment_constraint_type(i)) /= 'none')  then
        penaltyval = penaltyval + max(0.d0,min_moment(i)-moment(i))/0.1d0
      end if
    end if
  end do


!----------------------------------------------------------------------------------------------------
! Evaluate geometric objective function  
!----------------------------------------------------------------------------------------------------

  ! Evaluate current value of geomtry targets 
  do i = 1, ngeo_targets

    select case (trim(geo_targets(i)%type))
      case ('zTop')           ! get z_value top side 
        cur_value = interp_point(x_interp, zt_interp, geo_targets(i)%x)
      case ('zBot')           ! get z_value bot side
        cur_value = interp_point(x_interp, zb_interp, geo_targets(i)%x)
      case ('Thickness')      ! take foil thickness calculated above
        cur_value = maxthick
      case ('Camber')         ! take foil camber from xfoil above
        cur_value = maxc
      case default
        call my_stop("Unknown target_type '"//trim(geo_targets(i)%type))
    end select

    ref_value = geo_targets(i)%reference_value
    tar_value = geo_targets(i)%target_value

    ! scale objective to 1 ( = no improvement) 
    increment = (ref_value + abs(tar_value - cur_value)) * geo_targets(i)%scale_factor 
    geo_objective_function = geo_objective_function + geo_targets(i)%weighting * increment

    geo_opt_info(i)%obj        = increment
    geo_opt_info(i)%weighting  = geo_targets(i)%weighting
    geo_opt_info(i)%change     = (cur_value / geo_targets(i)%seed_value) - 1d0

  end do 



! Geo objective  - add to aero target for final objective function

  aero_objective_function = aero_objective_function + geo_objective_function

  
! Show surface quality for entertainment and info
!          about objectives at the end of iteration 
  if (show_details .and. penalize) then
    call show_op_point_contributions ( aero_objective_function,                          & 
                                       noppoint,     op_opt_info,                        &
                                       ngeo_targets, geo_opt_info,                       &
                                       (penaltyval*1.0D+06), penalty_info )                          
  end if


! Finally handle penalties - Add all penalties to objective function, and make them very large

  if (penalize ) then
    aero_objective_function = aero_objective_function + penaltyval*1.0D+06
  end if                           


end function aero_objective_function




!=============================================================================80
!
! Objective function for matching one airfoil to another (for testing shape
! functions, optimization algorithms, etc.).  Assumes x-values of points line
! up; this should be handled before optimizing.
!
!=============================================================================80
function matchfoil_objective_function(designvars)

  use parametrization, only : top_shape_function, bot_shape_function,          &
                              create_airfoil, create_airfoil_camb_thick
  use math_deps,       only : norm_2, curvature

  double precision, dimension(:), intent(in) :: designvars
  double precision :: matchfoil_objective_function

  double precision, dimension(size(xseedt,1)) :: zt_new
  double precision, dimension(size(xseedb,1)) :: zb_new
  integer :: nmodest, nmodesb, nptt, nptb, dvtbnd, dvbbnd

  double precision :: penaltyval, match_delta, cur_te_curvature
  character (80) :: penalty_info

  penaltyval   = 0d0
  penalty_info = ''

  nmodest = size(top_shape_function,1)
  nmodesb = size(bot_shape_function,1)
  nptt = size(xseedt,1)
  nptb = size(xseedb,1)

! Set modes for top and bottom surfaces

  if ((trim(shape_functions) == 'naca')  .or. &
      (trim(shape_functions) == 'camb-thick')) then
    dvtbnd = nmodest
    dvbbnd = nmodest + nmodesb
  else
    dvtbnd = nmodest*3
    dvbbnd = nmodest*3 + nmodesb*3
  end if



  if (trim(shape_functions) == 'camb-thick') then
    ! Create new airfoil by changing camber and thickness of seed airfoil
    call create_airfoil_camb_thick(xseedt, zseedt, xseedb, zseedb,             &
                      designvars(1:dvtbnd), zt_new, zb_new)
  else 
    ! Create top and bottom surfaces by perturbation of seed airfoil 
    call create_airfoil(xseedt, zseedt, xseedb, zseedb, designvars(1:dvtbnd),  &
                        designvars(dvtbnd+1:dvbbnd), zt_new, zb_new,           &
                        shape_functions, .false.)
  end if

! Penalty for TE panel problem 
!    In the current Hicks Henne shape functions implementation, the last panel is
!    forced to become TE which can lead to a thick TE area with steep last panel
!       (see create_shape ... do j = 2, npt-1 ...)
!    so the curvature (2nd derivative) at the last 10 panels is checked

  if (check_curvature) then
    cur_te_curvature = maxval (abs(curvature(11, xseedt(nptt-10:nptt), zt_new(nptt-10:nptt))))
    if (cur_te_curvature  > max_te_curvature) then 
      penalty_info = trim(penalty_info) // ' TEat'
      penaltyval = penaltyval + cur_te_curvature
    end if 

    cur_te_curvature = maxval (abs(curvature(11, xseedb(nptb-10:nptb), zb_new(nptb-10:nptb))))
    if (cur_te_curvature  > max_te_curvature) then 
      penalty_info = trim(penalty_info) // ' TEab'
      penaltyval = penaltyval + cur_te_curvature
    end if 
  end if 

  if (penaltyval > 0d0) then 
    matchfoil_objective_function = max (penaltyval, 2.d0)
    ! write (*,*) 'Penalty ', trim(penalty_info)
  else

! Evaluate the new airfoil, (not-> changed)  counting fixed LE and TE points

    match_delta = norm_2(zt_new(2:nptt-1) - zmatcht(2:nptt-1)) + &
                  norm_2(zb_new(2:nptb-1) - zmatchb(2:nptb-1))
    if (match_delta < 1d-20)  match_delta = 1d-1 

    ! Scale result to initial value 1.
    matchfoil_objective_function = match_delta * match_foils_scale_factor

  end if

end function matchfoil_objective_function

!=============================================================================80
!
! Generic function to write designs. Selects either 
! write_airfoil_optimization_progress or write_matchfoil_optimization_progress
! depending on whether match_foils = .true. or not.
!
!=============================================================================80
function write_function(designvars, designcounter)

  double precision, dimension(:), intent(in) :: designvars
  integer, intent(in) :: designcounter
  integer :: write_function

  if (match_foils) then
    write_function = write_matchfoil_optimization_progress(designvars,         &
                                                           designcounter)
  else
    write_function = write_airfoil_optimization_progress(designvars,           &
                                                         designcounter)
  end if

end function write_function

!=============================================================================80
!
! Writes airfoil coordinates and polars to files during optimization
!
!=============================================================================80
function write_airfoil_optimization_progress(designvars, designcounter)

  use math_deps,          only : interp_vector 
  use airfoil_operations, only : smooth_it
  use airfoil_operations, only : airfoil_write_to_unit


  use parametrization, only : top_shape_function, bot_shape_function,          &
                              create_airfoil, create_airfoil_camb_thick
  use xfoil_driver,    only : run_xfoil, xfoil_geometry_info

  double precision, dimension(:), intent(in) :: designvars
  integer, intent(in) :: designcounter
  integer :: write_airfoil_optimization_progress

  double precision, dimension(size(xseedt,1)) :: zt_new
  double precision, dimension(size(xseedb,1)) :: zb_new
  integer :: nmodest, nmodesb, nptt, nptb, i, dvtbnd1, dvtbnd2, dvbbnd1,       &
             dvbbnd2 
  double precision, dimension(noppoint) :: alpha, lift, drag, moment, viscrms, &
                                           xtrt, xtrb
  double precision, dimension(noppoint) :: actual_flap_degrees
  double precision :: ffact, maxt, xmaxt, maxc, xmaxc
  integer :: ndvs, flap_idx, dvcounter
 
  character(100) :: foilfile, polarfile, text, title
  character(8) :: maxtchar, xmaxtchar, maxcchar, xmaxcchar
  integer :: foilunit, polarunit

  nmodest = size(top_shape_function,1)
  nmodesb = size(bot_shape_function,1)
  nptt = size(xseedt,1)
  nptb = size(xseedb,1)

! Set modes for top and bottom surface

  if (trim(shape_functions) == 'naca') then
    dvtbnd1 = 1
    dvtbnd2 = nmodest
    dvbbnd2 = nmodest + nmodesb
    dvbbnd1 = dvtbnd2 + 1
  else if (trim(shape_functions) == 'camb-thick') then
    dvtbnd1 = 1
    dvtbnd2 = nmodest
    dvbbnd1 = 1
    dvbbnd2 = dvtbnd2
  else
    dvtbnd1 = 1
    dvtbnd2 = nmodest*3
    dvbbnd2 = nmodest*3 + nmodesb*3
    dvbbnd1 = dvtbnd2 + 1
  end if

! Overwrite lower DVs for symmetrical airfoils (they are not used)

  if (symmetrical) then
    dvbbnd1 = 1
    dvbbnd2 = dvtbnd2
  end if

! Format coordinates in a single loop in derived type. Also remove translation
! and scaling to ensure Cm_x=0.25 doesn't change.

  if (trim(shape_functions) == 'camb-thick') then
    ! Create new airfoil by changing camber and thickness of seed airfoil
    call create_airfoil_camb_thick(xseedt, zseedt, xseedb, zseedb,             &
                         designvars(dvtbnd1:dvtbnd2), zt_new, zb_new)
  else 
    call create_airfoil(xseedt, zseedt, xseedb, zseedb,                          &
                        designvars(dvtbnd1:dvtbnd2), designvars(dvbbnd1:dvbbnd2),&
                        zt_new, zb_new, shape_functions, symmetrical)
  end if

  
! jx-mod Smoothing - begin ---------------------------------------------------------

  if (do_smoothing) then 
    call smooth_it (xseedt, zt_new)
    call smooth_it (xseedb, zb_new)
  end if 

! Format coordinates in a single loop in derived type

  do i = 1, nptt
    curr_foil%x(i) = xseedt(nptt-i+1)/foilscale - xoffset
    curr_foil%z(i) = zt_new(nptt-i+1)/foilscale - zoffset
  end do
  do i = 1, nptb-1
    curr_foil%x(i+nptt) = xseedb(i+1)/foilscale - xoffset
    curr_foil%z(i+nptt) = zb_new(i+1)/foilscale - zoffset
  end do

! jx-mod Smoothing
!        Restore the original, not smoothed seed airfoil to
!        ...design_coordinates.dat to show it in visualizer

  if (designcounter == 0) then
    do i = 1, nptt
      curr_foil%z(i)      = zseedt_not_smoothed(nptt-i+1)/foilscale - zoffset
    end do
    do i = 1, nptb-1
      curr_foil%z(i+nptt) = zseedb_not_smoothed(i+1)/foilscale - zoffset
    end do
  end if


! Check that number of flap optimize points are correct

  ndvs = size(designvars,1)
  if (nflap_optimize /= (ndvs - dvbbnd2)) then
    write(*,*) "Wrong number of design variables for flap deflections."
    write(*,*) "Please report this bug."
    stop
  end if

! Get actual flap angles based on design variables

  ffact = initial_perturb/(max_flap_degrees - min_flap_degrees)
  actual_flap_degrees(1:noppoint) = flap_degrees(1:noppoint)
  dvcounter = dvbbnd2 + 1
  do i = 1, nflap_optimize
    flap_idx = flap_optimize_points(i)
    actual_flap_degrees(flap_idx) = designvars(dvcounter)/ffact
    dvcounter = dvcounter + 1
  end do

! Analyze airfoil at requested operating conditions with Xfoil

  call run_xfoil(curr_foil, xfoil_geom_options, op_point(1:noppoint),          &
                 op_mode(1:noppoint), re(1:noppoint), ma(1:noppoint),          &
                 use_flap, x_flap, y_flap, y_flap_spec,                        &
                 actual_flap_degrees(1:noppoint), xfoil_options, lift, drag,   &
                 moment, viscrms, alpha, xtrt, xtrb, ncrit_pt)

             
! Get geometry info

  call xfoil_geometry_info(maxt, xmaxt, maxc, xmaxc)
  write(maxtchar,'(F8.5)') maxt
  maxtchar = adjustl(maxtchar)
  write(xmaxtchar,'(F8.5)') xmaxt
  xmaxtchar = adjustl(xmaxtchar)
  write(maxcchar,'(F8.5)') maxc
  maxcchar = adjustl(maxcchar)
  write(xmaxcchar,'(F8.5)') xmaxc
  xmaxcchar = adjustl(xmaxcchar)

! Set output file names and identifiers

  foilfile = trim(output_prefix)//'_design_coordinates.dat'
  polarfile = trim(output_prefix)//'_design_polars.dat'

  foilunit = 13
  polarunit = 14

! Open files and write headers, if necessary

  if (designcounter == 0) then

!   Header for coordinate file

    write(*,*) "Writing coordinates for seed airfoil to file "//               &
               trim(foilfile)//" ..."
    open(unit=foilunit, file=foilfile, status='replace')
    write(foilunit,'(A)') 'title="Airfoil coordinates"'

!  Add 2nd and 3rd derivative to
!        ...design_coordinates.dat to show it in visualizer
    write(foilunit,'(A)') 'variables="x" "z" "2nd derivative" "3rd derivative"'

    title =  'zone t="Seed airfoil, maxt='//trim(maxtchar)//&
             ', xmaxt='//trim(xmaxtchar)//', maxc='//&
              trim(maxcchar)//', xmaxc='//trim(xmaxcchar)//'"'

!   Header for polar file

    write(*,*) "Writing polars for seed airfoil to file "//                    &
               trim(polarfile)//" ..."
    open(unit=polarunit, file=polarfile, status='replace')
    write(polarunit,'(A)') 'title="Airfoil polars"'

! jx-mod Entertainment
!        Add current flap angle to polars to show it in visualizer
    write(polarunit,'(A)') 'variables="alpha" "cl" "cd" "cm" "xtrt" "xtrb" "flapangle"'
    write(polarunit,'(A)') 'zone t="Seed airfoil polar"'

  else

!   Format design counter as string

    write(text,*) designcounter
    text = adjustl(text)

!   Open coordinate file and write zone header

    write(*,*) "  Writing coordinates for design number "//trim(text)//        &
               " to file "//trim(foilfile)//" ..."
    open(unit=foilunit, file=foilfile, status='old', position='append', err=900)
    title =  'zone t="Airfoil, maxt='//trim(maxtchar)//&
             ', xmaxt='//trim(xmaxtchar)//', maxc='//&
              trim(maxcchar)//', xmaxc='//trim(xmaxcchar)//'", '//&
             'SOLUTIONTIME='//trim(text)

    ! Open polar file and write zone header
    
    write(*,*) "  Writing polars for design number "//trim(text)//             &
               " to file "//trim(polarfile)//" ..."
    open(unit=polarunit, file=polarfile, status='old', position='append',      &
         err=901)
    write(polarunit,'(A)') 'zone t="Polars", SOLUTIONTIME='//trim(text)

  end if

! Write coordinates to file

  call  airfoil_write_to_unit (foilunit, title, curr_foil, .True.)


! Write polars to file

  do i = 1, noppoint
    ! Add current flap angle to polars to show it in visualizer
    write(polarunit,'(6ES14.6, 1ES14.3)') alpha(i), lift(i), drag(i), moment(i), &
                                 xtrt(i), xtrb(i), actual_flap_degrees (i)
  end do

! Close output files

  close(foilunit)
  close(polarunit)

! Set return value (needed for compiler)

  write_airfoil_optimization_progress = 0
  return

! Warning if there was an error opening design_coordinates file

900 write(*,*) "Warning: unable to open "//trim(foilfile)//". Skipping ..."
  write_airfoil_optimization_progress = 1
  return

! Warning if there was an error opening design_coordinates file

901 write(*,*) "Warning: unable to open "//trim(polarfile)//". Skipping ..."
  write_airfoil_optimization_progress = 2
  return

end function write_airfoil_optimization_progress

!=============================================================================80
!
! Writes airfoil coordinates to foil during optimization to match one airfoil
! to another.
!
!=============================================================================80
function write_matchfoil_optimization_progress(designvars, designcounter)

  use parametrization,    only : top_shape_function, bot_shape_function,          &
                                create_airfoil, create_airfoil_camb_thick
  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_geometry_info
  use airfoil_operations, only : airfoil_write_to_unit

  double precision, dimension(:), intent(in) :: designvars
  integer, intent(in) :: designcounter
  integer :: write_matchfoil_optimization_progress

  double precision, dimension(size(xseedt,1)) :: zt_new
  double precision, dimension(size(xseedb,1)) :: zb_new
  integer :: i, nmodest, nmodesb, nptt, nptb, dvtbnd, dvbbnd
  double precision :: maxt, xmaxt, maxc, xmaxc
  character(8) :: maxtchar, xmaxtchar, maxcchar, xmaxcchar


  character(100) :: foilfile, text, title
  integer :: foilunit

  nmodest = size(top_shape_function,1)
  nmodesb = size(bot_shape_function,1)
  nptt = size(xseedt,1)
  nptb = size(xseedb,1)

! Design 0 is seed airfoil to output - take the original values 

  if (designcounter == 0) then
    zt_new = zseedt
    zb_new = zseedb
  else

! Set modes for top and bottom surfaces

    if ((trim(shape_functions) == 'naca')  .or. &
        (trim(shape_functions) == 'camb-thick')) then
      dvtbnd = nmodest
      dvbbnd = nmodest + nmodesb
    else
      dvtbnd = nmodest*3
      dvbbnd = nmodest*3 + nmodesb*3
    end if

! Format coordinates in a single loop in derived type. Also remove translation
! and scaling to ensure Cm_x=0.25 doesn't change.

    if (trim(shape_functions) == 'camb-thick') then
      call create_airfoil_camb_thick(xseedt, zseedt, xseedb, zseedb,             &
                        designvars(1:dvtbnd), zt_new, zb_new)
    else 
      call create_airfoil(xseedt, zseedt, xseedb, zseedb, designvars(1:dvtbnd),  &
                        designvars(dvtbnd+1:dvbbnd), zt_new, zb_new,             &
                        shape_functions, .false.)
    end if
  end if 


  ! do *not* Format coordinates in a single loop in derived type
  !     to have the real working foil in visualizer
  do i = 1, nptt
    curr_foil%x(i) = xseedt(nptt-i+1)!/foilscale - xoffset
    curr_foil%z(i) = zt_new(nptt-i+1)!/foilscale - zoffset
  end do
  do i = 1, nptb-1
    curr_foil%x(i+nptt) = xseedb(i+1)!/foilscale - xoffset
    curr_foil%z(i+nptt) = zb_new(i+1)!/foilscale - zoffset
  end do

  call xfoil_set_airfoil(curr_foil)
  call xfoil_geometry_info(maxt, xmaxt, maxc, xmaxc)

  write(maxtchar,'(F8.5)') maxt
  maxtchar = adjustl(maxtchar)
  write(xmaxtchar,'(F8.5)') xmaxt
  xmaxtchar = adjustl(xmaxtchar)
  write(maxcchar,'(F8.5)') maxc
  maxcchar = adjustl(maxcchar)
  write(xmaxcchar,'(F8.5)') xmaxc
  xmaxcchar = adjustl(xmaxcchar)

! Set output file names and identifiers

  foilfile = trim(output_prefix)//'_design_coordinates.dat'
  foilunit = 13

! Open file and write header, if necessary

  if (designcounter == 0) then

!   New File: Header for coordinate file & Seed foil 

    write(*,*) "Writing coordinates for seed airfoil to file "//               &
               trim(foilfile)//" ..."
    open(unit=foilunit, file=foilfile, status='replace')
    write(foilunit,'(A)') 'title="Airfoil coordinates"'
    write(foilunit,'(A)') 'variables="x" "z"'
    title = 'zone t="Seed airfoil", maxt='//trim(maxtchar)//&
            ', xmaxt='//trim(xmaxtchar)//', maxc='//&
             trim(maxcchar)//', xmaxc='//trim(xmaxcchar)
  else

!   Append to file: Header for design foil coordinates

    write(text,*) designcounter
    text = adjustl(text)

    write(*,*) "  Writing coordinates for design number "//trim(text)//        &
               " to file "//trim(foilfile)//" ..."
    open(unit=foilunit, file=foilfile, status='old', position='append', err=910)
    title =  'zone t="Airfoil, maxt='//trim(maxtchar)//&
             ', xmaxt='//trim(xmaxtchar)//', maxc='//&
              trim(maxcchar)//', xmaxc='//trim(xmaxcchar)//'", '//&
             'SOLUTIONTIME='//trim(text)
  end if

  call  airfoil_write_to_unit (foilunit, title, curr_foil, .True.)

  close(foilunit)


  if (designcounter == 0) then
! Append the coordinates of the match foil when seed foil is written
    write_matchfoil_optimization_progress = write_matchfoil_coordinates ()
  else
! Set return value (needed for compiler)
    write_matchfoil_optimization_progress = 0
  end if 

  return

! Warning if there was an error opening design_coordinates file

910 write(*,*) "Warning: unable to open "//trim(foilfile)//". Skipping ..."
  write_matchfoil_optimization_progress = 1
  return

end function write_matchfoil_optimization_progress


!=============================================================================80
!
! Writes airfoil coordinates of match_foil at the beginning of optimization 
!
!=============================================================================80
function write_matchfoil_coordinates ()

  use memory_util, only : allocate_airfoil
  use airfoil_operations, only : airfoil_write_to_unit


  character(100) :: foilfile
  integer :: foilunit
  integer :: write_matchfoil_coordinates
  integer :: i, nptt, nptb
  type(airfoil_type) :: tmpfoil


  foilfile = trim(output_prefix)//'_design_coordinates.dat'
  foilunit = 13

  nptt = size(xmatcht,1)
  nptb = size(xmatchb,1)

  tmpfoil%npoint = nptt + nptb - 1
  call allocate_airfoil(tmpfoil)

  ! do *not* Format coordinates in a single loop in derived type
  !     to have the real working foil in visualizer
  do i = 1, nptt
    tmpfoil%x(i) = xmatcht(nptt-i+1)
    tmpfoil%z(i) = zmatcht(nptt-i+1)
  end do
  do i = 1, nptb-1
    tmpfoil%x(i+nptt) = xmatchb(i+1)
    tmpfoil%z(i+nptt) = zmatchb(i+1)
  end do

  write(*,*) "Writing coordinates for match airfoil to file "//trim(foilfile)//" ..."

  open(unit=foilunit, file=foilfile, status='old', position='append', err=910)
  call  airfoil_write_to_unit (foilunit, 'zone t="Match airfoil"', tmpfoil, .True.)
  close(foilunit)

  write_matchfoil_coordinates = 0

  return

! Warning if there was an error opening design_coordinates file

910 write(*,*) "Warning: unable to open "//trim(foilfile)//". Skipping ..."
  write_matchfoil_coordinates = 1
  return

end function write_matchfoil_coordinates

!=============================================================================80
!
! Cleans up unused designs written prior to a restart
!
!=============================================================================80
function write_function_restart_cleanup(restart_status, global_search,         &
                                        local_search)

  character(*), intent(in) :: restart_status, global_search, local_search
  integer :: write_function_restart_cleanup

  integer :: restunit, ioerr, step, designcounter, foilunit, polarunit,        &
             histunit, ncoord
  integer :: i, j
  double precision, dimension(:,:), allocatable :: x, z, alpha, lift, drag,    &
                                                   moment, xtrt, xtrb
  double precision, dimension(:), allocatable :: fmin, relfmin, rad
  character(150), dimension(:), allocatable :: zoneinfo
  character(100) :: restfile, foilfile, polarfile, text
  character(11) :: stepchar
  character(20) :: fminchar, radchar
  character(25) :: relfminchar
  ! jx-mod Smoothing read/write 2nd and 3rd derivative 
  double precision, dimension(:,:), allocatable :: deriv2, deriv3


! Print status

  write(*,*) 'Cleaning up unused designs written after restart save ...'

  restunit = 12
  foilunit = 13
  polarunit = 14
  histunit = 15

! Read last written design from restart file

  if (trim(restart_status) == 'global_optimization') then
    if (trim(global_search) == 'particle_swarm') then
      restfile = 'restart_pso_'//trim(output_prefix)
    else if (trim(global_search) == 'genetic_algorithm') then
      restfile = 'restart_ga_'//trim(output_prefix)
    end if
  else
    if (trim(local_search) == 'simplex') then
      restfile = 'restart_simplex_'//trim(output_prefix)
    end if
  end if

  open(unit=restunit, file=restfile, status='old', form='unformatted',         &
       iostat=ioerr)
  if (ioerr /= 0) then
    write_function_restart_cleanup = 1
    return
  end if
  read(restunit) step
  read(restunit) designcounter
  close(restunit)

! Allocate size of data arrays

  ncoord = size(xseedt,1) + size(xseedb,1) - 1
  allocate(x(ncoord,designcounter+1))
  allocate(z(ncoord,designcounter+1))
  allocate(alpha(noppoint,designcounter+1))
  allocate(lift(noppoint,designcounter+1))
  allocate(drag(noppoint,designcounter+1))
  allocate(moment(noppoint,designcounter+1))
  allocate(xtrt(noppoint,designcounter+1))
  allocate(xtrb(noppoint,designcounter+1))
  allocate(zoneinfo(designcounter+1))
  allocate(fmin(step))
  allocate(relfmin(step))
  allocate(rad(step))
  ! jx-mod Smoothing read/write 2nd and 3rd derivative 
  allocate(deriv2(ncoord,designcounter+1))
  allocate(deriv3(ncoord,designcounter+1))


! Open coordinates file

  foilfile = trim(output_prefix)//'_design_coordinates.dat'
  open(unit=foilunit, file=foilfile, status='old', iostat=ioerr)
  if (ioerr /= 0) then
    write_function_restart_cleanup = 2
    return
  end if

! Skip file header

  read(foilunit,*)
  read(foilunit,*)

! Read coordinates for each airfoil

  do i = 1, designcounter + 1
  
!   Read zone header

    read(foilunit,'(A)') zoneinfo(i)

!   Read coordinates
    do j = 1, ncoord
      ! jx-mod Smoothing 2nd and 3rd derivative when reading the file
      !read(foilunit,'(2F14.6)') x(j,i), z(j,i)
      read(foilunit,'(2F12.7,2G18.8)') x(j,i), z(j,i), deriv2(j,i), deriv3(j,i)
    end do

  end do

! Close coordinates file

  close(foilunit)

! Re-write coordinates file without the unused designs

  open(unit=foilunit, file=foilfile, status='replace')
  write(foilunit,'(A)') 'title="Airfoil coordinates"'
  write(foilunit,'(A)') 'variables="x" "z"'
  do i = 0, designcounter
!   Write zone header

    write(foilunit,'(A)') trim(zoneinfo(i+1))

!   Write coordinates

    do j = 1, ncoord
      ! jx-mod Smoothing Additional 2nd and 3rd derivative when writing the file
      write(foilunit,'(2F12.6,2G18.7)') x(j,i+1), z(j,i+1), deriv2(j,i+1), deriv3(j,i+1)
    end do
  end do

! Close coordinates file

  close(foilunit)

! Open history file

  open(unit=histunit, file='optimization_history.dat', status='old',           &
       iostat=ioerr)
  if (ioerr /= 0) then
    write_function_restart_cleanup = 3
    return
  end if

! Skip file header

  read(histunit,*)

! Read optimizer data at each iteration

  do i = 1, step
    read(histunit,*) j, fmin(i), relfmin(i), rad(i)
  end do

! Close history file

  close(histunit)

! Re-write history file without the unused iterations

  open(unit=histunit, file='optimization_history.dat', status='replace')
  write(histunit,'(A)') "Iteration  Objective function  "//&
                        "% Improvement over seed  Design radius"
  do i = 1, step
    write(stepchar,'(I11)') i
    write(fminchar,'(F14.10)') fmin(i)
    write(relfminchar,'(F14.10)') relfmin(i)
    write(radchar,'(ES14.6)') rad(i)
    write(histunit,'(A11,A20,A25,A20)') adjustl(stepchar), adjustl(fminchar),  &
                                        adjustl(relfminchar), adjustl(radchar)
  end do

! Close history file

  close(histunit)

! Return now if we're matching airfoils (no aero data)

  if (match_foils) then
    deallocate(x)
    deallocate(z)
    deallocate(alpha)
    deallocate(lift)
    deallocate(drag)
    deallocate(moment)
    deallocate(xtrt)
    deallocate(xtrb)
    deallocate(fmin)
    deallocate(relfmin)
    deallocate(rad)
    write(*,*) 'Finished cleaning up unused designs.'
    write_function_restart_cleanup = 0
    return
  end if

! Open polars file

  polarfile = trim(output_prefix)//'_design_polars.dat'
  open(unit=polarunit, file=polarfile, status='old', iostat=ioerr)
  if (ioerr /= 0) then
    write_function_restart_cleanup = 4
    return
  end if

! Skip file header

  read(polarunit,*)
  read(polarunit,*)

! Read polars for each airfoil

  do i = 1, designcounter + 1
  
!   Skip zone header

    read(polarunit,*)

!   Read polars

    do j = 1, noppoint
      read(polarunit,'(6ES14.6)') alpha(j,i), lift(j,i), drag(j,i),            &
                                  moment(j,i), xtrt(j,i), xtrb(j,i)
    end do

  end do

! Close polars file

  close(polarunit)

! Re-write polars file without the unused designs

  open(unit=polarunit, file=polarfile, status='replace')
  write(polarunit,'(A)') 'title="Airfoil polars"'
  write(polarunit,'(A)') 'variables="cl" "cd"'
  do i = 0, designcounter
!   Write zone header

    if (i == 0) then
      write(polarunit,'(A)') 'zone t="Seed airfoil polar"'
    else
      write(text,*) i
      text = adjustl(text)
      write(polarunit,'(A)') 'zone t="Polars", SOLUTIONTIME='//trim(text)
    end if

!   Write polars

    do j = 1, noppoint
      write(polarunit,'(6ES14.6)') alpha(j,i+1), lift(j,i+1), drag(j,i+1),     &
                                   moment(j,i+1), xtrt(j,i+1), xtrb(j,i+1)
    end do
  end do

! Close polars file

  close(polarunit)

! Deallocate data arrays

  deallocate(x)
  deallocate(z)
  deallocate(lift)
  deallocate(drag)
  deallocate(moment)
  deallocate(zoneinfo)
  deallocate(xtrt)
  deallocate(xtrb)
  deallocate(fmin)
  deallocate(relfmin)
  deallocate(rad)
  ! jx-mod Smoothing read/write 2nd and 3rd derivative 
  deallocate(deriv2)
  deallocate(deriv3)


! Print status

  write(*,*) 'Finished cleaning up unused designs.'
  write(*,*)

  write_function_restart_cleanup = 0

end function write_function_restart_cleanup


!------------------------------------------------------------------------------
! Shows contribution of each operating point and geotargets to the overall 
! objective function during optimization  
!------------------------------------------------------------------------------

subroutine show_op_point_contributions ( obj_func_value,    &
                noppoint, op_opt_info,                      &
                ngeo,     geo_opt_info,                     &
                penalty, penalty_info ) 

  use os_util, only: COLOR_BAD, COLOR_GOOD, COLOR_NORMAL, COLOR_HIGH
  use os_util, only: print_colored
 
  double precision, intent (in) :: obj_func_value, penalty
  integer         , intent (in) :: noppoint, ngeo
  character(100)  , intent (in) :: penalty_info
  type(op_opt_info_type),  dimension (noppoint), intent (in) :: op_opt_info
  type(geo_opt_info_type), dimension (noppoint), intent (in) :: geo_opt_info

  integer :: i, outcolor 
  double precision :: total_improvement, contribution

  character(20) :: outstring

  total_improvement = (1.d0-obj_func_value) 

! write op info header 

  write (*,'(4x, 24x)', advance = 'no') 
  do i = 1, noppoint
    write (*,'(A6,I02,A1)', advance = 'no') '    op',i,''
  end do 
  write (*,'(10x)', advance = 'no')
  do i = 1, ngeo
    write (*,'(A6,I02,A1)', advance = 'no') '   geo',i,''
  end do 
  write (*,*)

! write relative changes in % per op_point

  write (*,'(4x, A23)', advance = 'no') "       Relative changes"
  do i = 1, noppoint
    write (*,'(SP, F8.1,A1)', advance = 'no') op_opt_info(i)%change * 100.d0,'%'
  end do 
  write (*,'(10x)', advance = 'no')
  do i = 1, ngeo
    write (*,'(SP, F8.2,A1)', advance = 'no') geo_opt_info(i)%change * 100.d0,'%'
  end do 
  write (*,*)

! write contribuiton in % per op_point and geo_targets for the overall improvment

  if (obj_func_value < best_obj_func_value) then 
    outcolor = COLOR_GOOD
    best_obj_func_value = obj_func_value
  else 
    outcolor = COLOR_NORMAL
  end if
  write (*,'(4x, A14,F8.4,A2)', advance = 'no') "Improvement:"
  write (outstring,'(F8.4,A1)') total_improvement *100.d0,'%'
  call print_colored (outcolor, trim(outstring))
     
  do i = 1, noppoint
    contribution = (1- op_opt_info(i)%obj) * op_opt_info(i)%weighting *100.d0
    write (outstring,'(F8.2,A1)') contribution,'%'
    if (contribution >= 0.5d0) then
      outcolor = COLOR_GOOD
    elseif (contribution <= -0.5d0) then
      outcolor = COLOR_BAD
    else
      outcolor = COLOR_NORMAL
    end if
    call print_colored (outcolor, trim(outstring))
  end do 
  write (*,'(10x)', advance = 'no')
  do i = 1, ngeo
    contribution = (1- geo_opt_info(i)%obj) * geo_opt_info(i)%weighting *100.d0
    write (outstring,'(F8.2,A1)') contribution,'%'
    if (contribution >= 0.5d0) then
      outcolor = COLOR_GOOD
    elseif (contribution <= -0.5d0) then
      outcolor = COLOR_BAD
    else
      outcolor = COLOR_NORMAL
    end if
    call print_colored (outcolor, trim(outstring))
  end do 
  write (*,*)

! write penaltiy infos per op_point which happend 

  if (penalty > 0d0) then
    write (*,'(4x, A14,F8.1,A1)', advance = 'no') "    Penalty:", penalty * (-1.d0) * 100.d0,'%'
    do i = 1, noppoint
      write (*,'(A9)', advance = 'no') trim(op_opt_info(i)%penalty_info)
    end do 
    write (*,'(10x)', advance = 'no')
    write (*,'(A9)', advance = 'no') trim(penalty_info)
    write (*,*)
  end if

  write (*,*)

end subroutine show_op_point_contributions


end module airfoil_evaluation

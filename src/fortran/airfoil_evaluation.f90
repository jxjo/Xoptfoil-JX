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
  use xfoil_driver, only : op_point_specification_type, re_type
  use os_util

  implicit none 
  
  public

! Defines a geometric target eg thickness of the optimization 

  type geo_target_type  
    character(30) :: type                               ! eg 'zBot' zTop'
    double precision :: x                               ! x-value of target
    double precision :: target_value                    ! target value to achieve
    double precision :: seed_value                      ! the value of the seed airfoil
    double precision :: reference_value                 ! to scale improvement (depends on type)
    double precision :: weighting                       ! weighting within objective function
    double precision :: scale_factor                    ! scale for objective function
  end type geo_target_type

  public :: objective_function, objective_function_nopenalty
  public :: write_function 
  public :: create_airfoil_form_design, get_flap_degrees_from_design
  double precision, parameter    :: OBJ_XFOIL_FAIL = 55.55d0
  double precision, parameter    :: OBJ_GEO_FAIL   = 1000d0

! -------------------------------------------------------

! Parms for geometry constraints
  double precision    :: min_thickness, max_thickness, min_te_angle,              &
                         growth_allowed, min_camber, max_camber
  integer             :: naddthickconst
  integer, parameter  :: max_addthickconst = 10
  double precision, dimension(max_addthickconst) :: addthick_x, addthick_min,  &
                                                    addthick_max
! Parms for moment constraints
  character(8), dimension(max_op_points) :: moment_constraint_type
  double precision, dimension(max_op_points) :: min_moment

! Parms for curvature control  
  logical :: check_curvature, auto_curvature, do_smoothing
  integer :: max_curv_reverse_top, max_curv_reverse_bot
  integer :: max_curv_highlow_top, max_curv_highlow_bot
  double precision :: curv_threshold, spike_threshold, highlow_threshold
  double precision :: max_te_curvature

! Geo targets 
  integer :: ngeo_targets
  integer, parameter :: max_geo_targets = 10
  type(geo_target_type), dimension(max_geo_targets) :: geo_targets
                        
! Parms for operating point specification
  integer :: noppoint
  type (op_point_specification_type), dimension (:), allocatable :: op_points_spec 

! Match foil mode
  type(airfoil_type) :: foil_to_match 
  logical :: match_foils
  double precision :: match_foils_scale_factor 


  type(xfoil_options_type)       :: xfoil_options
  type(xfoil_geom_options_type)  :: xfoil_geom_options
  

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

  double precision            :: objective_function, geo_penalty, aero, geo
  type(airfoil_type)          :: foil
  double precision, dimension(noppoint) :: actual_flap_degrees
  logical                     :: only_geometry

  only_geometry = present(evaluate_only_geometry) .and. evaluate_only_geometry
  
  objective_function = 0d0
  geo_penalty        = 0d0
  aero               = 0d0
  geo                = 0d0

! create the foil to evaluate out of seed foil + current design (shape functions)
  call create_airfoil_form_design (seed_foil, designvars, foil)

! Objective function - special treatment for match_foil mode

  if (match_foils) then
!   check the geometry for violations
    geo_penalty = geo_penalty_function (foil, actual_flap_degrees)

    objective_function = matchfoil_objective_function(foil) + geo_penalty

! Objective function - the master 

  else

!   if flaps activated, the flap angle at an op will be part of the design space
    call get_flap_degrees_from_design (designvars, actual_flap_degrees)

!   check the geometry for violations
    geo_penalty = geo_penalty_function (foil, actual_flap_degrees)

!   if yes - further evaluation is not needed
    if(only_geometry .or. (geo_penalty > OBJ_GEO_FAIL)) then 
      objective_function = 1d0 + geo_penalty
    else
!     finally we've reached the core - evaluate the foil ...
      aero = aero_objective_function (foil, actual_flap_degrees)
      geo  = geo_objective_function  (foil)

      if(aero == OBJ_XFOIL_FAIL) then
        objective_function = aero                     ! return just fail value for further detection
      else
        objective_function = aero + geo + geo_penalty
      end if

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
  double precision         :: objective_function_nopenalty, aero, geo
  type(airfoil_type)       :: foil
  double precision, dimension(noppoint) :: actual_flap_degrees

  objective_function_nopenalty = 0d0
  aero               = 0d0
  geo                = 0d0

! create the foil to evaluate out of seed foil + current design (shape functions)
  call create_airfoil_form_design (seed_foil, designvars, foil)

! special treatment for match_foil mode
  if (match_foils) then
    objective_function_nopenalty = matchfoil_objective_function(foil)
  else

!   if flaps activated, the flap angle at an op will be part of the design space
    call get_flap_degrees_from_design (designvars, actual_flap_degrees)

!   finally we've reached the core - do xfoil calculations...
    aero = aero_objective_function (foil, actual_flap_degrees)
    geo  = geo_objective_function  (foil)

    if(aero == OBJ_XFOIL_FAIL) then
      objective_function_nopenalty = aero       ! return just fail value for further detection
    else
      objective_function_nopenalty = aero + geo
    end if

  end if

end function objective_function_nopenalty


!=============================================================================80
!
!  Geometric Penalty function
!
!  Asses geometry of airfoil to find violations of geometric constraints
!  A penality value will be added for each vilation
!
!  Input:  foil to evaluate
!          optional flap angles per op_point
!  Output: penality value for geomtric violations
!          = 0 everything is perfect
!
!=============================================================================80
function geo_penalty_function(foil, actual_flap_degrees)

  use math_deps,          only : interp_vector, curvature, derv1f1, derv1b1
  use airfoil_operations, only : get_curv_violations, get_max_te_curvature
  use airfoil_operations, only : my_stop
  use xfoil_driver,       only : xfoil_geometry_amax, xfoil_set_airfoil, &
                                 xfoil_get_geometry_info

  type(airfoil_type), intent(in)    :: foil
  double precision, dimension(:), intent(in)  :: actual_flap_degrees
  double precision                  :: geo_penalty_function

  double precision, dimension(max(size(foil%xt,1),size(foil%xb,1))) :: x_interp, &
                                               zt_interp, zb_interp, thickness
  double precision, dimension(naddthickconst) :: add_thickvec

  double precision :: penaltyval
  character(100)   :: penalty_info
  double precision :: tegap, growth1, growth2, maxgrowth, len1, len2
  double precision :: maxt, xmaxt, maxc, xmaxc
  double precision :: panang1, panang2, maxpanang, heightfactor, cur_te_curvature
  double precision :: gapallow, maxthick
  integer          :: nreverse_violations, nhighlow_violations
  integer          :: nptt, nptb, i, nptint
  double precision :: pi


  pi = acos(-1.d0)
  nptt = size(foil%xt,1)
  nptb = size(foil%xb,1)

!----------------------------------------------------------------------------------------------------
! Check geometry contraints  - resulting in penalties added  
!----------------------------------------------------------------------------------------------------

  penaltyval   = 0.d0
  penalty_info = ''                            ! user info on the type of penalities given

  maxgrowth = 0.d0

  len1 = sqrt((foil%x(2)-foil%x(1))**2.d0 +                          &
              (foil%z(2)-foil%z(1))**2.d0)
  do i = 2, nptt + nptb - 2
    len2 = sqrt((foil%x(i+1)-foil%x(i))**2.d0 +                      &
                (foil%z(i+1)-foil%z(i))**2.d0)
    growth1 = len2/len1
    growth2 = len1/len2
    if (max(growth1,growth2) > maxgrowth) maxgrowth = max(growth1,growth2)
    len1 = len2
  end do

! Penalty for too large growth rate

  penaltyval = penaltyval + max(0.d0,maxgrowth-growth_allowed)
  if (max(0.d0,maxgrowth-growth_allowed)/1.d0 > 0.d0) penalty_info = trim(penalty_info) // ' maxGrowth'

! Penalty for too blunt leading edge

  panang1 = atan((foil%zt(2)-foil%zt(1))/(foil%xt(2)-foil%xt(1))) *                &
            180.d0/acos(-1.d0)
  panang2 = atan((foil%zb(1)-foil%zb(2))/(foil%xb(2)-foil%xb(1))) *                &
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
    call get_max_te_curvature (nptt, foil%xt, foil%zt, cur_te_curvature)
    if (cur_te_curvature  > max_te_curvature) then 
      penalty_info = trim(penalty_info) // ' TEmaxCurv'
      penaltyval = penaltyval + cur_te_curvature
    end if 

    call get_max_te_curvature (nptb, foil%xb, foil%zb, cur_te_curvature)
    if (cur_te_curvature  > max_te_curvature) then 
      penalty_info = trim(penalty_info) // ' TEmaxCurv'
      penaltyval = penaltyval + cur_te_curvature
    end if 
  end if 

! Interpolate bottom surface to foil%xt points (to check thickness)

  if (foil%xt(nptt) <= foil%xb(nptb)) then
    nptint = nptt
    call interp_vector(foil%xb, foil%zb, foil%xt, zb_interp(1:nptt))
    x_interp(1:nptt) = foil%xt
    zt_interp(1:nptt) = foil%zt  
  else
    nptint = nptb
    call interp_vector(foil%xt, foil%zt, foil%xb, zt_interp(1:nptb))
    x_interp(1:nptb) = foil%xb
    zb_interp(1:nptb) = foil%zb
  end if

! Compute thickness parameters

  tegap = foil%zt(nptt) - foil%zb(nptb)
  maxthick = 0.d0
  heightfactor = tan(min_te_angle*acos(-1.d0)/180.d0/2.d0)

  do i = 2, nptint - 1

!   Thickness array and max thickness

    thickness(i) = zt_interp(i) - zb_interp(i)
    if (thickness(i) > maxthick) maxthick = thickness(i)

!   Check if thinner than specified wedge angle on back half of airfoil

    if (foil%xt(i) > 0.5d0) then
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


! Check for curvature reversals and high lows

  if (check_curvature) then

!   Top side 

    call get_curv_violations (foil%xt, foil%zt, & 
                              curv_threshold, highlow_threshold, & 
                              max_curv_reverse_top, max_curv_highlow_top,   &
                              nreverse_violations, nhighlow_violations)

    if (nreverse_violations > 0 ) then 
      penaltyval = penaltyval + nreverse_violations
      penalty_info = trim(penalty_info) // ' maxReversal'
    end if 
    if (nhighlow_violations > 0 ) then 
      penaltyval = penaltyval + nhighlow_violations
      penalty_info = trim(penalty_info) // ' maxHighLow'
    end if 

!   Bottom side - 

    call get_curv_violations (foil%xb, foil%zb, & 
                              curv_threshold, highlow_threshold, & 
                              max_curv_reverse_bot, max_curv_highlow_bot,   &
                              nreverse_violations, nhighlow_violations)

    if (nreverse_violations > 0 ) then 
      penaltyval = penaltyval + nreverse_violations
      penalty_info = trim(penalty_info) // ' maxReversal'
    end if 
    if (nhighlow_violations > 0 ) then 
      penaltyval = penaltyval + nhighlow_violations
      penalty_info = trim(penalty_info) // ' maxHighLow'
    end if 

  end if


! Penalty for flap deflections outside the specified bounds

  if (use_flap) then
    do i = 1, noppoint
      penaltyval = penaltyval +                                                  &
                  max(0.d0,actual_flap_degrees(i)-max_flap_degrees)
      penaltyval = penaltyval +                                                  &
                  max(0.d0,min_flap_degrees-actual_flap_degrees(i))
    end do
  end if


! next checks need xfoil geo routines...

  call xfoil_set_airfoil (foil)        ! Xfoil_set calcs amax...
  call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)
  maxpanang = xfoil_geometry_amax() 

! Add penalty for too large panel angle
!     Due to numerical issues (?) it happens, that the final maxpanang ist greater 25.

  if (max(0.0d0,maxpanang-30.d0) > 0.d0) then
    ! strong penality - will abort 
    penaltyval = penaltyval + max(0.0d0,maxpanang-30.d0)/5.d0
    penalty_info = trim(penalty_info) // ' MxAng'
  else
    if (max(0.0d0,maxpanang-29.5d0) > 0.d0) then
      ! do not check if camb-thick - it couldn't be solved...
      if ((trim(shape_functions) /= 'camb-thick') .and. &
          (trim(shape_functions) /= 'camb-thick-plus')) then
          ! weak penalty to avoid getting too close to cut off at 25.0 
        penaltyval   = penaltyval + max(0.0d0,maxpanang-29.5d0) * 2.d0 
        penalty_info = trim(penalty_info) // ' MxAng'
      end if 
    end if
  end if

! Add penalty for camber outside of constraints

  penaltyval = penaltyval + max(0.d0,maxc-max_camber)/0.025d0
  penaltyval = penaltyval + max(0.d0,min_camber-maxc)/0.025d0
  if (max(0.d0,maxc-max_camber) > 0.d0) penalty_info = trim(penalty_info) // ' MxCmb'
  if (max(0.d0,min_camber-maxc) > 0.d0) penalty_info = trim(penalty_info) // ' MiCmb'


! geo penalties are quite high to distinguish from "normal" objective value
  geo_penalty_function = penaltyval*5.0D+04

  
!  if (show_details .and. (penaltyval > 0d0)) then
!    write (*,'(4x, A14,F11.1,5x, A)') "    Penalty:", &
!          geo_penalty_function, trim(penalty_info)
!  end if

end function geo_penalty_function



!=============================================================================80
!
!  Objective function as result of geometric evaluation
!
!  Input: foil to evaluate
!  Output: objective function value based on airfoil performance
!
!=============================================================================80
function geo_objective_function(foil)

  use airfoil_operations, only : my_stop
  use math_deps,          only : interp_vector, interp_point
  use xfoil_driver,       only : xfoil_geometry_amax, xfoil_set_airfoil, &
                                 xfoil_get_geometry_info

  type(airfoil_type), intent(in)    :: foil
  double precision                  :: geo_objective_function

  integer          :: nptt, nptb, i, nptint
  double precision :: ref_value, tar_value, cur_value, increment
  double precision :: maxt, xmaxt, maxc, xmaxc
  double precision, dimension(max(size(foil%xt,1),size(foil%xb,1))) :: x_interp, &
                      zt_interp, zb_interp

  geo_objective_function  = 0.d0  

! Interpolate bottom, top surface to foil%xt points (to check thickness)

  nptt = size(foil%xt,1)
  nptb = size(foil%xb,1)

  if (foil%xt(nptt) <= foil%xb(nptb)) then
    nptint = nptt
    call interp_vector(foil%xb, foil%zb, foil%xt, zb_interp(1:nptt))
    x_interp(1:nptt) = foil%xt
    zt_interp(1:nptt) = foil%zt  
  else
    nptint = nptb
    call interp_vector(foil%xt, foil%zt, foil%xb, zt_interp(1:nptb))
    x_interp(1:nptb) = foil%xb
    zb_interp(1:nptb) = foil%zb
  end if

! get airfoil geometry info from xfoil    

  call xfoil_set_airfoil (foil)        
  call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)

! Evaluate current value of geomtry targets 
  do i = 1, ngeo_targets

    select case (trim(geo_targets(i)%type))
      case ('zTop')                      ! get z_value top side 
        cur_value = interp_point(x_interp, zt_interp, geo_targets(i)%x)
      case ('zBot')                      ! get z_value bot side
        cur_value = interp_point(x_interp, zb_interp, geo_targets(i)%x)
      case ('Thickness')                 ! take foil camber from xfoil above
        cur_value = maxt
      case ('Camber')                    ! take foil camber from xfoil above
        cur_value = maxc
      case default
        call my_stop("Unknown target_type '"//trim(geo_targets(i)%type))
    end select

    ref_value = geo_targets(i)%reference_value
    tar_value = geo_targets(i)%target_value

    ! scale objective to 1 ( = no improvement) 
    increment = (ref_value + abs(tar_value - cur_value)) * geo_targets(i)%scale_factor 

    geo_objective_function = geo_objective_function + geo_targets(i)%weighting * increment

  end do
  
end function geo_objective_function


! #exp-dynamic mb-mod dynamic weighting
!=============================================================================80
!
!  subroutine for setting dynamic weighting
!
!  Input: lift, drag and moment as results from xfoil-calculations
!  Output: internally sets new values for weighting of all target-type oppoints
!
!=============================================================================80
function dynamic_weighting_function(op_points_result, fixed_weighting)

  use xfoil_driver,       only : op_point_result_type

  type(op_point_result_type), dimension(:), intent(in) :: op_points_result
  double precision, dimension(noppoint),intent(in) :: fixed_weighting

  double precision, dimension(noppoint) :: curr_deviation_abs
  double precision, dimension(noppoint) :: dynamic_weighting
  double precision :: sum_weightings, medium_deviation_abs 
  type(dynamic_weighting_type) :: dynamic_weighting_function
  integer          :: i, num
  logical          :: debug_on
  type(op_point_specification_type) :: op_spec
  type(op_point_result_type)        :: op

  character(15)    :: opt_type


  ! Initialization
  num = 0
  medium_deviation_abs = 0.0d0
  debug_on = .false.

  ! Evaluate all oppoints, calculate dynamic weighting (not normalized yet)
  do i = 1, noppoint

    curr_deviation_abs(i) = 0.0d0

    op_spec  = op_points_spec(i) 
    opt_type = op_spec%optimization_type

    if (trim(opt_type) == 'target-drag') then
      curr_deviation_abs(i) = ABS((op_spec%target_value- op%cd ) * op_spec%scale_factor)
      ! fixed weighting is used as an override-factor for dynamic weighting factor.
      dynamic_weighting(i) = curr_deviation_abs(i) * dynamic_weighting_p_factor * fixed_weighting(i)
      num = num + 1
    
    elseif (trim(opt_type) == 'target-lift') then
      curr_deviation_abs(i) = (ABS (op_spec%target_value- op%cl ))* op_spec%scale_factor
      dynamic_weighting(i) = curr_deviation_abs(i) * dynamic_weighting_p_factor * fixed_weighting(i)
      num = num + 1
    
    elseif (trim(opt_type) == 'target-moment') then
      curr_deviation_abs(i) = (ABS (op_spec%target_value- op%cm ))* op_spec%scale_factor
      dynamic_weighting(i) = curr_deviation_abs(i) * dynamic_weighting_p_factor * fixed_weighting(i)
      num = num + 1
       
    else
      ! no dynamic weighting for this oppoint
      dynamic_weighting(i) = fixed_weighting(i)
    end if
    
    ! accumulate curr_deviation
    medium_deviation_abs = medium_deviation_abs + curr_deviation_abs(i)
  end do

  ! calculate medium deviation of all evaluated oppoints
  dynamic_weighting_function%medium_deviation_abs = medium_deviation_abs / num

  ! normalize all weightings and write them to the result data-structure
  sum_weightings = sum(dynamic_weighting(1:noppoint))
  do i = 1, noppoint
    dynamic_weighting_function%weighting(i) = dynamic_weighting(i) / sum_weightings
    if (debug_on .eqv. .true.) then
      write (*,*) 'normalized weighting:', op_spec%weighting
    end if
  end do

end function dynamic_weighting_function


!==============================================================================
!==============================================================================
!
!  Objective function as result of aerodynamic evaluation
!
!  Input: foil to evaluate
!         optional flap angles per op_point
!  Output: objective function value based on airfoil performance
!
!==============================================================================
!==============================================================================

function aero_objective_function(foil, actual_flap_degrees)

  use airfoil_operations, only : my_stop
  use math_deps,          only : derivation_at_point
  use xfoil_driver,       only : run_op_points, xfoil_set_airfoil, op_point_result_type 

  type(airfoil_type), intent(in)    :: foil
  double precision, dimension(:), intent(in)  :: actual_flap_degrees
  double precision                  :: aero_objective_function

  type(op_point_specification_type) :: op_spec
  type(op_point_result_type)        :: op
  type(op_point_result_type), dimension(:), allocatable :: op_points_result

  integer          :: i
  double precision :: pi
  double precision :: cur_value, slope, increment, dist
  character(15)    :: opt_type


! #exp-dynamic mb-mod dynamic weighting
  type(dynamic_weighting_type) :: dynamic_weighting_result
  
  pi = acos(-1.d0)

! Analyze airfoil at requested operating conditions with Xfoil

  call xfoil_set_airfoil (foil) 
  
  xfoil_options%show_details = .false.      ! switch off because of multi-threading

  call run_op_points (foil, xfoil_geom_options, xfoil_options,        &
                      use_flap, flap_spec, actual_flap_degrees(1:noppoint), &
                      op_points_spec, op_points_result)

  xfoil_options%show_details = show_details  

! Early exit if an op_point didn't converge - further calculations wouldn't make sense

  do i = 1, noppoint
    if (.not. op_points_result(i)%converged) then 
      aero_objective_function = 55.55d0
      return
    end if
  end do

  if (dynamic_weighting) then
!   perform dynamic weighting of target-type oppoints, thus calculate local weightings
!   that may differ from the globally fixed weightings. This has to be done
!   _before_ the following loop !
    dynamic_weighting_result = dynamic_weighting_function(op_points_result, op_points_spec%weighting)
  end if

! Get objective function contribution from aerodynamics 
!    (aero performance times normalized weight)

  aero_objective_function = 0.d0

  do i = 1, noppoint

    op_spec  = op_points_spec(i)
    op       = op_points_result(i) 
    opt_type = op_spec%optimization_type


!   Objective function evaluation

    if (trim(opt_type) == 'min-sink') then

!     Maximize Cl^1.5/Cd

      if (op%cl > 0.d0) then
        increment = op%cd/op%cl**1.5d0 * op_spec%scale_factor
      else
        increment = 1.D9   ! Big penalty for lift <= 0
      end if
      cur_value  = op%cl**1.5d0 / op%cd

    elseif (trim(opt_type) == 'max-glide') then

!     Maximize Cl/Cd

      if (op%cl > 0.d0) then
        increment = op%cd / op%cl * op_spec%scale_factor
      else
        increment = 1.D9   ! Big penalty for lift <= 0
      end if
      cur_value  = op%cl / op%cd 

    elseif (trim(opt_type) == 'min-drag') then

!     Minimize Cd

      increment = op%cd * op_spec%scale_factor
      cur_value = op%cd 

    elseif (trim(opt_type) == 'target-drag') then

! Minimize difference between target cd value and current value 
    
      dist = ABS (op_spec%target_value - op%cd)
      if (dist < 0.000004d0) dist = 0d0  ! little threshold to achieve target

      increment = (op_spec%target_value + dist) * op_spec%scale_factor 
      cur_value = op%cd 

    elseif (trim(opt_type) == 'target-lift') then

! jx-mod Minimize difference between target cl value and current value 
!        Add a base value to the lift difference
    
      increment = (1.d0 + ABS (op_spec%target_value-op%cl)) * op_spec%scale_factor 
      cur_value = op%cl

    elseif (trim(opt_type) == 'target-moment') then

! jx-mod Minimize difference between target moment value and current value 
!        Add a base value (Clark y or so ;-) to the moment difference
!        so the relative change won't be to high
      increment = (ABS (op_spec%target_value - op%cm) + 0.05d0) * op_spec%scale_factor
      cur_value = op%cm

    elseif (trim(opt_type) == 'max-lift') then

!     Maximize Cl (at given angle of attack)

      if (op%cl > 0.d0) then
        increment = op_spec%scale_factor / op%cl
      else
        increment = 1.D9   ! Big penalty for lift <= 0
      end if
      cur_value = op%cl

    elseif (trim(opt_type) == 'max-xtr') then

!     Maximize laminar flow on top and bottom (0.1 factor to ensure no
!     division by 0)

      increment = op_spec%scale_factor/(0.5d0*(op%xtrt + op%xtrb)+0.1d0)
      cur_value = 0.5d0*(op%xtrt + op%xtrb)

      ! jx-mod Following optimization based on slope of the curve of op_point
!         convert alpha in rad to get more realistic slope values
!         convert slope in rad to get a linear target 
!         factor eg 4.d0*pi to adjust range of objective function (not negative)

    elseif (trim(opt_type) == 'max-lift-slope') then

!     Maximize dCl/dalpha (0.1 factor to ensure no division by 0)

      slope = derivation_at_point (i, (op_points_result%alpha * pi/180.d0) , &
                                      (op_points_result%cl))
      increment = op_spec%scale_factor / (atan(abs(slope))  + 2.d0*pi)
      cur_value = atan(abs(slope))

    elseif (trim(opt_type) == 'min-lift-slope') then

!     New: Minimize dCl/dalpha e.g. to reach clmax at alpha(i) 
      slope = derivation_at_point (i, (op_points_result%alpha * pi/180.d0) , &
                                      (op_points_result%cl))

      increment = op_spec%scale_factor * (atan(abs(slope)) + 2.d0*pi)
      cur_value = atan(abs(slope))

    elseif (trim(opt_type) == 'min-glide-slope') then

!     New: Minimize d(cl/cd)/dcl e.g. to reach best glide at alpha(i) 
      slope = derivation_at_point (i, (op_points_result%cl * 20d0), &
                                      (op_points_result%cl/op_points_result%cd))

      increment = op_spec%scale_factor * (atan(abs(slope))  + 2.d0*pi)
      cur_value = atan(abs(slope))  

    else

      write(*,*)
      write(*,*) "Error: requested optimization_type not recognized."
      stop

    end if

!   Add contribution to the objective function
    if (dynamic_weighting) then
!     use the locally calculated dynamic weighting
      aero_objective_function = aero_objective_function &
                                + dynamic_weighting_result%weighting(i)*increment
    else
!     use weighting that is fixed and globally the same
      aero_objective_function = aero_objective_function + op_spec%weighting*increment
    end if

  end do
! We made it ...

  aero_objective_function = aero_objective_function 

end function aero_objective_function



!=============================================================================80
!
! Objective function for matching one airfoil to another (for testing shape
! functions, optimization algorithms, etc.).  Assumes x-values of points line
! up; this should be handled before optimizing.
!
!=============================================================================80
function matchfoil_objective_function(foil)

  use math_deps,       only : norm_2

  type(airfoil_type), intent(in)    :: foil
  double precision :: matchfoil_objective_function
  double precision :: match_delta
  integer          :: nptt, nptb

  nptt = size(foil%xt,1)
  nptb = size(foil%xb,1)

! Evaluate the new airfoil, (not-> changed)  counting fixed LE and TE points

  match_delta = norm_2(foil%zt(2:nptt-1) - foil_to_match%zt(2:nptt-1)) + &
                norm_2(foil%zb(2:nptb-1) - foil_to_match%zb(2:nptb-1))
  if (match_delta < 1d-10)  match_delta = 1d-1 

  ! Scale result to initial value 1.
  matchfoil_objective_function = match_delta * match_foils_scale_factor

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

!===============================================================================
!
! Create an airfoil out of a seed airfoil and a design (shape functions)  
!
!===============================================================================

subroutine create_airfoil_form_design (seed, designvars, foil)

  use vardef,             only: airfoil_type
  use vardef,             only: shape_functions
  use airfoil_operations, only: rebuild_airfoil
  use parametrization,    only: create_airfoil_camb_thick
  use parametrization,    only: create_airfoil_camb_thick_plus
  use parametrization,    only: create_airfoil
  use parametrization,    only: top_shape_function, bot_shape_function
  

  type(airfoil_type), intent(in)              :: seed
  type(airfoil_type), intent(out)             :: foil
  double precision, dimension(:), intent(in)  :: designvars

  integer :: nmodest, nmodesb, dvtbnd1, dvtbnd2, dvbbnd1, dvbbnd2
  integer :: dvtbnd1_cambthick, dvtbnd2_cambthick !#exp-HH-plus
  double precision, dimension(size(seed%xt,1)) :: zt_new
  double precision, dimension(size(seed%xb,1)) :: zb_new
  double precision, dimension(size(seed%xt,1)) :: zt_new_cambthick !#exp-HH-plus
  double precision, dimension(size(seed%xb,1)) :: zb_new_cambthick !#exp-HH-plus


! Build airfoil to evaluate out of seed airfoil plus shape functions applied

  nmodest = size(top_shape_function,1)
  nmodesb = size(bot_shape_function,1)

! Set modes for top and bottom surfaces

  if (trim(shape_functions) == 'naca') then
    dvtbnd1 = 1
    dvtbnd2 = nmodest
    dvbbnd2 = nmodest + nmodesb
    dvbbnd1 = dvtbnd2 + 1
  else if ((trim(shape_functions) == 'camb-thick') .or. &
           (trim(shape_functions) == 'camb-thick-plus')) then
    dvtbnd1 = 1
    dvtbnd2 = nmodest
    dvbbnd1 = 1
    dvbbnd2 = dvtbnd2
  else if (trim(shape_functions) == 'hicks-henne-plus') then !#exp-HH-plus
    dvtbnd1_cambthick = 1
    dvtbnd2_cambthick = 6 
    dvtbnd1 = dvtbnd2_cambthick + 1
    ! camb-thick uses the first "2 modes" of top-surface (6 parameters)
    dvtbnd2 = dvtbnd2_cambthick + (nmodest-2)*3 
    dvbbnd1 = dvtbnd2 + 1
    dvbbnd2 = nmodest*3 + nmodesb*3 
  else
    dvtbnd1 = 1
    dvtbnd2 = nmodest*3
    dvbbnd2 = nmodest*3 + nmodesb*3
    dvbbnd1 = dvtbnd2 + 1
  end if
  
! Overwrite lower DVs for symmetrical airfoils or camb-thickness-shaping
! (they are not used)

  if (seed%symmetrical) then
    dvbbnd1 = 1
    dvbbnd2 = dvtbnd2
    foil%symmetrical = .true.
  end if
  
  if (trim(shape_functions) == 'camb-thick') then
    ! Create new airfoil by changing camber and thickness of seed airfoil.
    call create_airfoil_camb_thick(seed%xt, seed%zt, seed%xb, seed%zb,       &
                      designvars(dvtbnd1:dvtbnd2), zt_new, zb_new)
  else if (trim(shape_functions) == 'camb-thick-plus') then
    ! Create new airfoil by changing camber and thickness of seed airfoil, 
    ! top and bottom seperately
    call create_airfoil_camb_thick_plus(seed%xt, seed%zt, seed%xb, seed%zb,       &
                      designvars(dvtbnd1:dvtbnd2), zt_new, zb_new)
  else if (trim(shape_functions) == 'hicks-henne-plus') then !#exp-HH-plus
    ! Create new airfoil by _first_ changing camber and thickness of seed airfoil 
    call create_airfoil_camb_thick(seed%xt, seed%zt, seed%xb, seed%zb,       &
                      designvars(dvtbnd1_cambthick:dvtbnd2_cambthick),       &
                      zt_new_cambthick, zb_new_cambthick)
    ! Change airfoil _second_ by perturbation of the just created camb- / 
    ! thick- adjusted airfoil
    call create_airfoil(seed%xt, zt_new_cambthick, seed%xb, zb_new_cambthick,    &
                      designvars(dvtbnd1:dvtbnd2), designvars(dvbbnd1:dvbbnd2),&
                               zt_new, zb_new, shape_functions, seed%symmetrical)                   
                      
  else 
    ! Create top and bottom surfaces by perturbation of seed airfoil 
    call create_airfoil(seed%xt, seed%zt, seed%xb, seed%zb,                      &
                      designvars(dvtbnd1:dvtbnd2), designvars(dvbbnd1:dvbbnd2),&
                      zt_new, zb_new, shape_functions, seed%symmetrical)
  end if

! Rebuild airfoil out of new top and bottom surface

  call rebuild_airfoil (seed%xt, seed%xb, zt_new, zb_new, foil)


end subroutine create_airfoil_form_design



!===============================================================================
! Get actual flap angles out of a design   
!===============================================================================

subroutine get_flap_degrees_from_design (designvars, actual_flap_degrees)

  use vardef,             only: shape_functions
  use vardef,             only: initial_perturb
  use vardef,             only: use_flap, nflap_optimize, flap_optimize_points
  use vardef,             only: max_flap_degrees, min_flap_degrees, flap_degrees
  use parametrization,    only: top_shape_function, bot_shape_function

  double precision, dimension(:), intent(in) :: designvars
  double precision, dimension(noppoint), intent(out) :: actual_flap_degrees
 
  integer :: nmodest, nmodesb, dvtbnd1, dvtbnd2, dvbbnd1, dvbbnd2, ndvs, dvcounter
  integer :: dvtbnd1_cambthick, dvtbnd2_cambthick !#exp-HH-plus
  integer :: i, flap_idx
  double precision :: ffact


  actual_flap_degrees = 0d0

  if (.not. use_flap) return

! Build airfoil to evaluate out of seed airfoil plus shape functions applied

  nmodest = size(top_shape_function,1)
  nmodesb = size(bot_shape_function,1)

! Set modes for top and bottom surfaces

  if (trim(shape_functions) == 'naca') then
    dvtbnd1 = 1
    dvtbnd2 = nmodest
    dvbbnd2 = nmodest + nmodesb
    dvbbnd1 = dvtbnd2 + 1
  else if ((trim(shape_functions) == 'camb-thick') .or. &
           (trim(shape_functions) == 'camb-thick-plus')) then
    dvtbnd1 = 1
    dvtbnd2 = nmodest
    dvbbnd1 = 1
    dvbbnd2 = dvtbnd2
  else if (trim(shape_functions) == 'hicks-henne-plus') then !#exp-HH-plus
    dvtbnd1_cambthick = 1
    dvtbnd2_cambthick = 6 
    dvtbnd1 = dvtbnd2_cambthick + 1
    ! camb-thick uses the first "2 modes" of top-surface (6 parameters)
    dvtbnd2 = dvtbnd2_cambthick + (nmodest-2)*3 
    dvbbnd1 = dvtbnd2 + 1
    dvbbnd2 = nmodest*3 + nmodesb*3 
  else
    dvtbnd1 = 1
    dvtbnd2 = nmodest*3
    dvbbnd2 = nmodest*3 + nmodesb*3
    dvbbnd1 = dvtbnd2 + 1
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

end subroutine get_flap_degrees_from_design 


!=============================================================================80
!
! Writes airfoil coordinates and polars to files during optimization
!
!=============================================================================80
function write_airfoil_optimization_progress(designvars, designcounter)

  use math_deps,          only : interp_vector 
  use airfoil_operations, only : airfoil_write_to_unit
  use xfoil_driver,       only : run_op_points, op_point_result_type
  use xfoil_driver,       only : xfoil_get_geometry_info, xfoil_set_airfoil

  double precision, dimension(:), intent(in) :: designvars
  integer, intent(in) :: designcounter
  integer :: write_airfoil_optimization_progress

  type(airfoil_type)       :: foil
  integer :: i

  type(op_point_specification_type) :: op_spec
  type(op_point_result_type)        :: op
  type(op_point_result_type), dimension(:), allocatable :: op_points_result

  double precision, dimension(noppoint) :: actual_flap_degrees
  double precision :: maxt, xmaxt, maxc, xmaxc
 
  character(100) :: foilfile, polarfile, text, title
  character(8) :: maxtchar, xmaxtchar, maxcchar, xmaxcchar
  integer :: foilunit, polarunit
  logical :: xfoil_reinitialize

  write(text,*) designcounter
  text = adjustl(text)

  if (designcounter == 0) then
    write (*,'(4x,A)') '-> Writing seed airfoil as design #'  //&
           trim(text)//' to file '//trim(output_prefix)//'[...].dat'
  else
    write (*,'(2x,A)', advance ='no') '-> Writing design '
    call  print_colored (COLOR_HIGH,'#'//trim(text))
    write (*,*)
  end if

! Design 0 is seed airfoil to output - take the original values 
!     Smoothing - Restore the original, not smoothed seed airfoil to
!                 ...design_coordinates.dat to show it in visualizer
  xfoil_reinitialize = xfoil_options%reinitialize

  if (designcounter == 0) then
    foil = seed_foil_not_smoothed

  ! ensure convergence for seed airfoil - see also check_seed
    xfoil_options%reinitialize = .true. 

! Design > 0 - Build current foil out seed foil and current design 
  else 
    call create_airfoil_form_design (seed_foil, designvars, foil)
  end if

! Get actual flap angles based on design variables
  call get_flap_degrees_from_design (designvars, actual_flap_degrees)

  if (show_details) write (*,*) 

! Analyze airfoil at requested operating conditions with Xfoil

  call run_op_points (foil, xfoil_geom_options, xfoil_options,        &
                      use_flap, flap_spec, actual_flap_degrees, &
                      op_points_spec, op_points_result)


  xfoil_options%reinitialize = xfoil_reinitialize 
             
! Get geometry info 
  call xfoil_set_airfoil (foil)   ! last set could have been a flaped version     
  call xfoil_get_geometry_info(maxt, xmaxt, maxc, xmaxc)
               
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

    open(unit=foilunit, file=foilfile, status='replace')
    write(foilunit,'(A)') 'title="Airfoil coordinates"'

!  Add 2nd and 3rd derivative to
!        ...design_coordinates.dat to show it in visualizer
    write(foilunit,'(A)') 'variables="x" "z" "2nd derivative" "3rd derivative"'

    title =  'zone t="Seed airfoil, maxt='//trim(maxtchar)//&
             ', xmaxt='//trim(xmaxtchar)//', maxc='//&
              trim(maxcchar)//', xmaxc='//trim(xmaxcchar)//'"'

!   Header for polar file

    open(unit=polarunit, file=polarfile, status='replace')
    write(polarunit,'(A)') 'title="Airfoil polars"'

!   Add current flap angle to polars to show it in visualizer
    write(polarunit,'(A)') 'variables="alpha" "cl" "cd" "cm" "xtrt" "xtrb" "flapangle"'
    write(polarunit,'(A)') 'zone t="Seed airfoil polar"'

  else

!   Open coordinate file and write zone header

    open(unit=foilunit, file=foilfile, status='old', position='append', err=900)
    title =  'zone t="Airfoil, maxt='//trim(maxtchar)//&
             ', xmaxt='//trim(xmaxtchar)//', maxc='//&
              trim(maxcchar)//', xmaxc='//trim(xmaxcchar)//'", '//&
             'SOLUTIONTIME='//trim(text)

!   Open polar file and write zone header

    open(unit=polarunit, file=polarfile, status='old', position='append',      &
         err=901)
    write(polarunit,'(A)') 'zone t="Polars", SOLUTIONTIME='//trim(text)

  end if

! Write coordinates to file

  call  airfoil_write_to_unit (foilunit, title, foil, .True.)

! Write polars to file

  do i = 1, noppoint

    op = op_points_result(i) 

    if (.not. op%converged) then 
      write(text,*) i
      text = adjustl(text)
      call print_error ('  Error: Op '//trim(text) // &
                        ' not converged in final calculation (this should not happen...)')
    end if 
    ! Add current flap angle to polars to show it in visualizer
    write(polarunit,'(6ES14.6, 1ES14.3)') op%alpha, op%cl, op%cd, op%cm, &
                                          op%xtrt, op%xtrb, actual_flap_degrees (i)
  end do

! Close output files

  close(foilunit)
  close(polarunit)

  if (show_details .and. (designcounter > 0)) then 
    call show_op_optimization_progress  (op_points_result) 
    call show_geo_optimization_progress (foil) 
  end if

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

!------------------------------------------------------------------------------
!
! Prints op results during optimization 
!       ! work in progress !
!------------------------------------------------------------------------------
subroutine show_op_optimization_progress(op_points_result) 

  use xfoil_driver,       only : op_point_result_type

  type target_info_type
    integer           :: i 
    character (2)     :: variable
    double precision  :: delta 
    integer           :: how_close
  end type 

  type(op_point_result_type), dimension(:),  intent(in) :: op_points_result

  type(op_point_result_type)        :: op
  type (target_info_type), dimension (noppoint) :: target_info
  character(15) :: opt_type
  integer :: i, j, nt

  nt = 0

  ! currently only aero targets support - if no targets - return 

  do i= 1, noppoint

    opt_type = op_points_spec(i)%optimization_type
    op       = op_points_result(i) 

    if (trim(opt_type (1:6)) == 'target') then
      nt = nt + 1

      target_info(nt)%i = i

      select case  (trim(opt_type))
        case ('target-drag')
          target_info(nt)%variable  = 'cd'
          target_info(nt)%delta     = op%cd - op_points_spec(i)%target_value
          target_info(nt)%how_close = r_quality (abs(target_info(nt)%delta), 0.000005d0, 0.0002d0, 0.005d0)
        case ('target-lift')
          target_info(nt)%variable  = 'cl'
          target_info(nt)%delta     = op%cl - op_points_spec(i)%target_value
          target_info(nt)%how_close = r_quality (abs(target_info(nt)%delta), 0.01d0, 0.05d0, 0.5d0)
        case ('target-moment')
          target_info(nt)%variable = 'cm'
          target_info(nt)%delta     = op%cm - op_points_spec(i)%target_value
          target_info(nt)%how_close = r_quality (abs(target_info(nt)%delta), 0.005d0, 0.02d0, 0.05d0)
        end select

    end if
  end do
  if(nt == 0 ) return

  ! print headline 

  write (*,*)
  write (*,'(18x)', advance = 'no') 
  do j= 1, nt
    write (*,'(4x,"Op",I2)', advance = 'no') target_info(j)%i 
  end do
  write (*,*)

  ! print 2. headline 

  write (*,'(4x, 3x,A8,3x)', advance = 'no') 'Distance'
  do j= 1, nt
    write (*,'(   A8)', advance = 'no') target_info(j)%variable
  end do
  write (*,*)
  
  ! now  print values
  
  write (*,'(4x,3x,A11)', advance = 'no') 'from target'
  do j= 1, nt 
    if (target_info(j)%how_close == Q_Good) then 
      call print_colored_s (target_info(j)%how_close, '     hit') 
    else
      if (target_info(j)%delta >= 0) then
        call print_colored_r (8,'(F6.5)', target_info(j)%how_close, target_info(j)%delta) 
      else
        call print_colored_r (8,'(F7.5)', target_info(j)%how_close, target_info(j)%delta) 
      end if
    end if
  end do
  write (*,*)

  write (*,*)
      
end 

!------------------------------------------------------------------------------
!
! Prints geo targets results during optimization 
!       ! work in progress !
!------------------------------------------------------------------------------
subroutine show_geo_optimization_progress(foil) 

  use airfoil_operations, only : my_stop

  use math_deps,          only : interp_vector, interp_point
  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_get_geometry_info

  type(airfoil_type), intent(in)    :: foil

  integer          :: nptt, nptb, i, nptint, how_close
  double precision :: tar_value, cur_value
  double precision :: maxt, xmaxt, maxc, xmaxc
  double precision, dimension(max(size(foil%xt,1),size(foil%xb,1))) :: x_interp, &
                      zt_interp, zb_interp
  ! character(25)    :: outstring


! Interpolate bottom, top surface to foil%xt points (to check thickness)

  nptt = size(foil%xt,1)
  nptb = size(foil%xb,1)

  if (foil%xt(nptt) <= foil%xb(nptb)) then
    nptint = nptt
    call interp_vector(foil%xb, foil%zb, foil%xt, zb_interp(1:nptt))
    x_interp(1:nptt) = foil%xt
    zt_interp(1:nptt) = foil%zt  
  else
    nptint = nptb
    call interp_vector(foil%xt, foil%zt, foil%xb, zt_interp(1:nptb))
    x_interp(1:nptb) = foil%xb
    zb_interp(1:nptb) = foil%zb
  end if

! get airfoil geometry info from xfoil    

  call xfoil_set_airfoil (foil)        
  call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)

! Evaluate current value of geomtry targets 
  do i = 1, ngeo_targets

    select case (trim(geo_targets(i)%type))
      case ('zTop')                      ! get z_value top side 
        cur_value = interp_point(x_interp, zt_interp, geo_targets(i)%x)
      case ('zBot')                      ! get z_value bot side
        cur_value = interp_point(x_interp, zb_interp, geo_targets(i)%x)
      case ('Thickness')                 ! take foil camber from xfoil above
        cur_value = maxt
      case ('Camber')                    ! take foil camber from xfoil above
        cur_value = maxc
      case default
        call my_stop("Unknown target_type '"//trim(geo_targets(i)%type))
    end select

    tar_value    = geo_targets(i)%target_value
    how_close    = r_quality (abs ((cur_value - tar_value) / tar_value), 0.0005d0, 0.02d0, 0.1d0)

    ! Geo target 1 Tickness  0.07732
    ! Geo target 2   Camber  0.01587
    !                        "colored"

    write (*,'(4x,3x,A10,I2,A11)', advance = 'no') 'Geo target',i, trim(geo_targets(i)%type)
  
    ! write (outstring, '(F9.5)') tar_value
    ! call print_colored (COLOR_NOTE,   trim(outstring))
    call print_colored_r (10,'(F7.5)', how_close, cur_value) 

    write (*,*) 

  end do

  if (ngeo_targets > 0) write (*,*) 
  
end subroutine show_geo_optimization_progress

!=============================================================================80
!
! Writes airfoil coordinates to foil during optimization to match one airfoil
! to another.
!
!=============================================================================80
function write_matchfoil_optimization_progress(designvars, designcounter)

  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_get_geometry_info
  use airfoil_operations, only : airfoil_write_to_unit, rebuild_airfoil

  double precision, dimension(:), intent(in) :: designvars
  integer, intent(in) :: designcounter
  integer :: write_matchfoil_optimization_progress

  type(airfoil_type)       :: foil
  double precision :: maxt, xmaxt, maxc, xmaxc
  character(8) :: maxtchar, xmaxtchar, maxcchar, xmaxcchar


  character(100) :: foilfile, text, title
  integer :: foilunit

  write(text,*) designcounter
  text = adjustl(text)

  if (designcounter == 0) then
    write (*,'(4x,A)') '-> Writing seed airfoil to file '//trim(output_prefix)//'[...].dat'
  else
    write (*,'(2x,A)', advance ='no') '-> Writing design '
    call  print_colored (COLOR_HIGH,'#'//trim(text))
    write (*,*)
  end if


! Design 0 is seed airfoil to output - take the original values 
!     Smoothing - Restore the original, not smoothed seed airfoil to
!                 ...design_coordinates.dat to show it in visualizer
  if (designcounter == 0) then
    foil = seed_foil
! Design > 0 - Build current foil out seed foil and current design 
  else 
    call create_airfoil_form_design (seed_foil, designvars, foil)
  end if

  call xfoil_set_airfoil (foil)
  call xfoil_get_geometry_info(maxt, xmaxt, maxc, xmaxc)
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
    open(unit=foilunit, file=foilfile, status='replace')
    write(foilunit,'(A)') 'title="Airfoil coordinates"'
    write(foilunit,'(A)') 'variables="x" "z"'
    title = 'zone t="Seed airfoil", maxt='//trim(maxtchar)//&
            ', xmaxt='//trim(xmaxtchar)//', maxc='//&
             trim(maxcchar)//', xmaxc='//trim(xmaxcchar)
  else

!   Append to file: Header for design foil coordinates
    open(unit=foilunit, file=foilfile, status='old', position='append', err=910)
    title =  'zone t="Airfoil, maxt='//trim(maxtchar)//&
             ', xmaxt='//trim(xmaxtchar)//', maxc='//&
              trim(maxcchar)//', xmaxc='//trim(xmaxcchar)//'", '//&
             'SOLUTIONTIME='//trim(text)
  end if

  call  airfoil_write_to_unit (foilunit, title, foil, .True.)

  close(foilunit)


! Append the coordinates of the match foil when seed foil is written
  if (designcounter == 0) then
    write (*,'(4x,A)') '-> Writing foil to match to file '//trim(output_prefix)//'[...].dat'

    open(unit=foilunit, file=foilfile, status='old', position='append', err=910)
    call  airfoil_write_to_unit (foilunit, 'zone t="Match airfoil"', foil_to_match, .True.)
    close(foilunit)  
  end if 

  write_matchfoil_optimization_progress = 0

  return

! Warning if there was an error opening design_coordinates file

910 write(*,*) "Warning: unable to open "//trim(foilfile)//". Skipping ..."
  write_matchfoil_optimization_progress = 1
  return

end function write_matchfoil_optimization_progress


end module airfoil_evaluation

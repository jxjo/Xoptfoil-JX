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
  use xfoil_driver, only : op_point_result_type
  use os_util

  implicit none 
  
  public
 
! Defines a geometric target eg thickness of the optimization 

  type geo_target_type  
    character(30) :: type                       ! eg 'Thickness'
    double precision :: target_value            ! target value to achieve
    double precision :: seed_value              ! the value of the seed airfoil
    double precision :: reference_value         ! to scale improvement (depends on type)
    double precision :: scale_factor            ! scale for objective function

    double precision :: weighting               ! weighting within objective function
    double precision :: weighting_user          ! original weighting entered by user
    logical          :: dynamic_weighting       ! dynamic weighting for this point 
    logical          :: extra_punch             !  - this op got an extra weighting punch
    double precision :: weighting_user_cur      !  - info: cuurent scaled user weighting
    double precision :: weighting_user_prv      !  - info: previous scaled user weighting
  end type geo_target_type

  type geo_result_type              
    double precision   :: maxt, xmaxt, maxc, xmaxc ! Thickness, camber etc of airfoil
  end type geo_result_type                             


! Parameters for dynamic weighting within objective function evaluation

  type dynamic_weighting_specification_type                              
    logical          :: active                  ! do dynamic weighting
    double precision :: min_weighting           ! min. value of weighting e.g. 0.5
    double precision :: max_weighting           ! max. value of weighting e.g. 4
    double precision :: extra_punch             ! extra weighting punch if deviation is too high
    integer          :: frequency               ! recalc weighting every n designs
    integer          :: start_with_design       ! dynamic weighting will start with design #...
  end type dynamic_weighting_specification_type

  type dynamic_variable_type
    double precision  :: dev  
    double precision  :: weighting, new_weighting 
  end type 
  

! Parameters for curvature control in case of Hicks-Henne

  type curvature_specification_type              
    logical          :: check_curvature         ! check curvature during optimization
    logical          :: auto_curvature          ! best thresholds will be determined
    logical          :: do_smoothing            ! Smooting of seed before optimization
  end type curvature_specification_type                             
 
  type curvature_polyline_specification_type
    logical          :: check_curvature_bumps   ! check for bumps (3rd derivative reversals)
    double precision :: curv_threshold          ! threshold to detetc reversals of curvature
    double precision :: spike_threshold         ! threshold to detetc reversals of 3rd dervi
    integer          :: max_curv_reverse        ! max. number of reversals 
    integer          :: max_spikes              ! max. number of spikes 
    double precision :: max_te_curvature        ! max. curvature at trailing edge
    integer          :: nskip_LE = 5            ! no of ponts to skip when scanning
    integer          :: nskip_TE_spikes = 0     ! no of ponts to skip when scanning
    integer          :: nskip_TE_revers = 0     !   ... will be set with auto_curve 
  end type curvature_polyline_specification_type                           


! Public functions and global variables 

  public :: objective_function, objective_function_nopenalty
  public :: write_progress
  public :: create_airfoil_form_design, get_flap_degrees_from_design
  public :: match_side_objective_function
  double precision, parameter    :: OBJ_XFOIL_FAIL = 55.55d0
  double precision, parameter    :: OBJ_GEO_FAIL   = 1000d0

! -------------------------------------------------------

! Parms for geometry constraints
  logical             :: check_geometry = .true.
  double precision    :: min_thickness, max_thickness, min_te_angle,              &
                         growth_allowed, min_camber, max_camber
  integer             :: naddthickconst
  integer, parameter  :: max_addthickconst = 10
  double precision, dimension(max_addthickconst) :: addthick_x, addthick_min,  &
                                                    addthick_max
! Parms for moment constraints
  logical             :: check_moment_constraints = .true. 
  character(8), dimension(max_op_points) :: moment_constraint_type
  double precision, dimension(max_op_points) :: min_moment

! Parms for curvature control  
  type (curvature_specification_type) :: curv_spec
  type (curvature_polyline_specification_type) :: curv_top_spec
  type (curvature_polyline_specification_type) :: curv_bot_spec

  integer, parameter             :: NSKIP_TE = 3  !no of points to be skipped at TE

! Geo targets 
  integer, parameter :: max_geo_targets = 10
  type(geo_target_type), dimension (:), allocatable  :: geo_targets

  double precision   :: airfoil_te_gap            ! trailing edge gap of (seed) airfoil
                                                  ! = -1 : no hange will be done
  
! Parms for operating point specification
  integer :: noppoint
  type (op_point_specification_type), dimension (:), allocatable :: op_points_spec 
  type (dynamic_weighting_specification_type)  :: dynamic_weighting_spec 

! Match foil mode
  type(airfoil_type) :: foil_to_match 
  logical :: match_foils
  double precision :: match_foils_scale_factor 

! Bezier - match to airfoil side  
  double precision, allocatable  :: side_to_match_x (:), side_to_match_y (:)

! Xfoil options
  type(xfoil_options_type)       :: xfoil_options
  type(xfoil_geom_options_type)  :: xfoil_geom_options

! Save the best result of evaluation for write_design (+ dynamic weighting)
!    so no extra run_xfoil is needed
  double precision, private       :: best_objective = 1d0
  type(airfoil_type), private     :: best_foil
  type(op_point_result_type), dimension(:), allocatable, private :: best_op_points_result


  contains


!=============================================================================80
!
! Generic objective function.  Selects either aero_objective_function or
! matchfoil_objective_function depending on whether match_foils = .true. or
! not.
!
!=============================================================================80
function objective_function(designvars, evaluate_only_geometry)

  use xfoil_driver,       only : op_point_result_type

  double precision, dimension(:), intent(in) :: designvars
  logical, intent(in), optional :: evaluate_only_geometry

  double precision            :: objective_function, geo_penalty, aero, geo
  type(airfoil_type)          :: foil
  double precision, dimension(noppoint) :: actual_flap_degrees
  logical                     :: only_geometry
  type(op_point_result_type), dimension(:), allocatable :: op_points_result

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
      aero = aero_objective_function (foil, actual_flap_degrees, op_points_result)
      geo  = geo_objective_function  (foil)

      if(aero == OBJ_XFOIL_FAIL) then
        objective_function = aero                     ! return just fail value for further detection
      else
        objective_function = aero + geo + geo_penalty
      end if

!   Save the best result to be written latte at write_design ...
!$omp critical
      if (objective_function < best_objective) then
        best_objective = objective_function 
        best_foil = foil 
        best_op_points_result = op_points_result
      end if 
!$omp end critical
    
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

  use xfoil_driver,       only : op_point_result_type

  double precision, dimension(:), intent(in) :: designvars
  double precision         :: objective_function_nopenalty, aero, geo
  type(airfoil_type)       :: foil
  double precision, dimension(noppoint) :: actual_flap_degrees
  type(op_point_result_type), dimension(:), allocatable :: op_points_result

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
    aero = aero_objective_function (foil, actual_flap_degrees, op_points_result)
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
  use math_deps,          only : count_reversals, derivative2, derivative3
  use xfoil_driver,       only : xfoil_geometry_amax, xfoil_set_airfoil, &
                                 xfoil_get_geometry_info
  use airfoil_operations, only : get_max_te_curvature

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
  double precision :: panang1, panang2, maxpanang, heightfactor
  double precision :: gapallow, maxthick
  integer          :: istart, iend, nreverse, nspikes
  integer          :: nreverse_violations, nspike_violations
  integer          :: nptt, nptb, i, nptint
  double precision :: pi
  type(curvature_polyline_specification_type) :: c

  pi = acos(-1.d0)
  nptt = size(foil%xt,1)
  nptb = size(foil%xb,1)

  geo_penalty_function = 0d0
  penaltyval   = 0.d0
  penalty_info = ''                            ! user info on the type of penalities given

! ----------------------------------------------------------------------
! Check for curvature constraints (when using Hicks-Henne)
! ----------------------------------------------------------------------

  if (curv_spec%check_curvature) then

!   Top side - How many reversals?  ... 

    c = curv_top_spec

    istart = c%nskip_LE
    iend   = size(foil%xt) - c%nskip_TE_revers

    nreverse = count_reversals (istart, iend, derivative2(foil%xt, foil%zt), c%curv_threshold)  
    nreverse_violations  = max(0,(nreverse - c%max_curv_reverse))

    if (nreverse_violations > 0 ) then 
      penaltyval = penaltyval + nreverse_violations
      penalty_info = trim(penalty_info) // ' maxReversal'
    end if 

  !   ....How many spikes = Reversals of 3rd derivation = Bumps of curvature

    if (c%check_curvature_bumps) then 
      iend   = size(foil%xt) - c%nskip_TE_spikes
      nspikes = count_reversals (istart, iend, derivative3(foil%xt, foil%zt), c%spike_threshold)
      nspike_violations  = max(0,(nspikes - c%max_spikes))
    else
      nspike_violations  = 0
    end if
    if (nspike_violations > 0 ) then 
      penaltyval = penaltyval + nspike_violations
      penalty_info = trim(penalty_info) // ' maxSpike'
    end if 

  ! TE curvature? 
  !    In the current Hicks Henne shape functions implementation, the last panel is
  !    forced to become TE which can lead to a thick TE area with steep last panel(s)
  !       (see create_shape ... do j = 2, npt-1 ...)
  !    so the curvature (2nd derivative) at the last 10 panels is checked

    if (get_max_te_curvature (foil%xt, foil%zt)  > c%max_te_curvature) then 
      penalty_info = trim(penalty_info) // ' TEmaxCurv ' // strf('(F4.2)', get_max_te_curvature (foil%xt, foil%zt))
      penaltyval = penaltyval + c%max_te_curvature
    end if 


! Bottom side - 

    if (.not. foil%symmetrical) then 

      c = curv_bot_spec 

    ! How many reversals?  ... 

      istart = c%nskip_LE
      iend   = size(foil%xb) - c%nskip_TE_revers
    
      nreverse = count_reversals (istart, iend, derivative2(foil%xb, foil%zb), c%curv_threshold)  
      nreverse_violations  = max(0,(nreverse - c%max_curv_reverse))
      if (nreverse_violations > 0 ) then 
        penaltyval = penaltyval + nreverse_violations
        penalty_info = trim(penalty_info) // ' maxReversal'
      end if 

    ! How many spikes = Rversals of 3rd derivation = Bumps of curvature
      if (c%check_curvature_bumps) then 
        iend   = size(foil%xb) - c%nskip_TE_spikes
        nspikes = count_reversals (istart, iend, derivative3(foil%xb, foil%zb), c%spike_threshold)
        nspike_violations  = max(0,(nspikes - c%max_spikes))
      else
        nspike_violations  = 0
      end if
      if (nspike_violations > 0 ) then 
        penaltyval = penaltyval + nspike_violations
        penalty_info = trim(penalty_info) // ' maxSpike'
      end if 
    
    ! Penalty for TE panel problem 

      if (get_max_te_curvature (foil%xb, foil%zb)  > c%max_te_curvature) then 
        penalty_info = trim(penalty_info) // ' TEmaxCurv'
        penaltyval = penaltyval + c%max_te_curvature
      end if 
    end if

  ! geo penalties are quite high to distinguish from "normal" objective value
    geo_penalty_function = penaltyval*5.0D+04

    if (penaltyval > 0d0) then
      ! if (show_details) write (*,'(4x, A14,F11.1,5x, A)') "    Penalty:", &
      !                              geo_penalty_function, trim(penalty_info)
      return
    end if
  end if


!----------------------------------------------------------------------------------------------------
! Check geometry contraints  - resulting in penalties added  
!----------------------------------------------------------------------------------------------------

  if (.not. check_geometry) return       ! early exit 


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


! Penalty for flap deflections outside the specified bounds

  if (flap_spec%use_flap) then
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

  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_get_geometry_info

  double precision                :: geo_objective_function

  type(airfoil_type), intent(in)  :: foil
  type(geo_result_type)           :: geo_result

! get airfoil geometry info from xfoil    

  call xfoil_set_airfoil (foil)        
  call xfoil_get_geometry_info (geo_result%maxt, geo_result%xmaxt, &
                                geo_result%maxc, geo_result%xmaxc)

  geo_objective_function = geo_objective_function_on_results (geo_result )
  
end function geo_objective_function


!-----------------------------------------------------------------------------
!  Geo objective function as result of geometry evaluation
!
!  Input: geo_targets
!         geo_result  from xfoil geometrie evaluation
!-----------------------------------------------------------------------------

function geo_objective_function_on_results (geo_result, eval_only_dynamic_ops )

  use math_deps,          only : derivation_at_point

  double precision :: geo_objective_function_on_results

  type(geo_result_type), intent(in) :: geo_result
  logical,  intent(in), optional :: eval_only_dynamic_ops

  integer          :: i
  double precision :: ref_value, tar_value, cur_value, increment, geo, correction
  logical          :: eval_all

  geo  = 0.d0 
  
  if (present(eval_only_dynamic_ops)) then
    eval_all = .not. eval_only_dynamic_ops
  else
    eval_all = .true.
  end if

! Evaluate current value of geomtry targets 
  do i = 1, size(geo_targets)

    if (eval_all .or. geo_targets(i)%dynamic_weighting ) then

      select case (trim(geo_targets(i)%type))
        case ('Thickness')                 ! take foil camber from xfoil above
          cur_value  = geo_result%maxt
          correction = 1.5d0               ! thickness is less sensible to changes
        case ('Camber')                    ! take foil camber from xfoil above
          cur_value  = geo_result%maxc
          correction = 0.7d0               ! camber is quite sensible to changes
        case default
          call my_stop("Unknown target_type '"//trim(geo_targets(i)%type))
      end select

      ref_value = geo_targets(i)%reference_value
      tar_value = geo_targets(i)%target_value

      ! scale objective to 1 ( = no improvement) 
      increment = (ref_value + abs(tar_value - cur_value) * correction) &
                 * geo_targets(i)%scale_factor 
      geo = geo +  geo_targets(i)%weighting * increment

    end if 

  end do

  geo_objective_function_on_results = geo 

end function geo_objective_function_on_results



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

function aero_objective_function(foil, actual_flap_degrees,op_points_result)

  use xfoil_driver,       only : run_op_points, xfoil_set_airfoil, op_point_result_type

  type(airfoil_type), intent(in)    :: foil
  double precision, dimension(:), intent(in)  :: actual_flap_degrees
  double precision                  :: aero_objective_function

  type(op_point_result_type), dimension(:), allocatable, intent(out) :: op_points_result
  type(xfoil_options_type)          :: local_xfoil_options

  integer          :: i


! Analyze airfoil at requested operating conditions with Xfoil

  call xfoil_set_airfoil (foil) 
  
  local_xfoil_options = xfoil_options
  local_xfoil_options%show_details        = .false.  ! switch off because of multi-threading
  local_xfoil_options%exit_if_unconverged = .true.   ! speed up if an op point uncoverges

  call run_op_points (foil, xfoil_geom_options, local_xfoil_options,        &
                      flap_spec, actual_flap_degrees, &
                      op_points_spec, op_points_result)


! Early exit if an op_point didn't converge - further calculations wouldn't make sense

  do i = 1, size(op_points_spec,1)
    if (.not. op_points_result(i)%converged) then 
      aero_objective_function = OBJ_XFOIL_FAIL
      return
    end if
  end do


! Get objective function contribution from aerodynamics 
!    (aero performance times normalized weight)

  aero_objective_function = aero_objective_function_on_results (op_points_result)

end function aero_objective_function


!-----------------------------------------------------------------------------
!
!  Objective function as result of aerodynamic evaluation
!
!  Input: op points specification
!         op points results from xfoil calculation
!  Output: objective function value based on airfoil performance
!
!-----------------------------------------------------------------------------

function aero_objective_function_on_results (op_points_result, eval_only_dynamic_ops)

  use math_deps,          only : derivation_at_point
  use xfoil_driver,       only : op_point_result_type

  type(op_point_result_type), dimension(:), intent(in) :: op_points_result
  logical,  intent(in), optional :: eval_only_dynamic_ops

  double precision                  :: aero_objective_function_on_results
  type(op_point_specification_type) :: op_spec
  type(op_point_result_type)        :: op
  integer          :: i
  double precision :: pi
  double precision :: cur_value, slope, increment, dist, correction
  character(15)    :: opt_type
  logical          :: eval_all

  pi = acos(-1.d0)
  noppoint = size(op_points_spec)  

  if (present(eval_only_dynamic_ops)) then
    eval_all = .not. eval_only_dynamic_ops
  else
    eval_all = .true.
  end if

! Get objective function contribution from aerodynamics 
!    (aero performance times normalized weight)

  aero_objective_function_on_results = 0.d0

  do i = 1, noppoint


    op_spec  = op_points_spec(i)
    op       = op_points_result(i) 
    opt_type = op_spec%optimization_type

    if (eval_all .or. (op_spec%dynamic_weighting .and. (.not. eval_all))) then
 
    ! Objective function evaluation

      if (trim(opt_type) == 'min-sink') then

      ! Maximize Cl^1.5/Cd

        if (op%cl > 0.d0) then
          increment = (op%cd / op%cl**1.5d0) * op_spec%scale_factor
        else
          increment = 1.D9   ! Big penalty for lift <= 0
        end if
        cur_value  = op%cl**1.5d0 / op%cd

      elseif (trim(opt_type) == 'max-glide') then

      ! Maximize Cl/Cd

        if (op%cl > 0.d0) then
          increment = op%cd / op%cl * op_spec%scale_factor
        else
          increment = 1.D9   ! Big penalty for lift <= 0
        end if
        cur_value  = op%cl / op%cd 

      elseif (trim(opt_type) == 'min-drag') then

      ! Minimize Cd

        increment = op%cd * op_spec%scale_factor
        cur_value = op%cd 

      elseif (trim(opt_type) == 'target-drag') then

      ! Minimize difference between target cd value and current value

        cur_value = op%cd

        if (op_spec%allow_improved_target) then
          dist = max (0d0, (cur_value - op_spec%target_value))
        else 
          dist = abs(cur_value - op_spec%target_value)
          if (dist < 0.000004d0) dist = 0d0         ! little threshold to achieve target
        end if 

        increment = (op_spec%target_value + dist) * op_spec%scale_factor 

      elseif (trim(opt_type) == 'target-glide') then

      ! minimize difference between target glide ratio and current glide ratio 
      
        cur_value = op%cl / op%cd

        if (op_spec%allow_improved_target) then
          dist = max (0d0, (op_spec%target_value - cur_value))
        else 
          dist = abs(cur_value - op_spec%target_value)
          if (dist < 0.01d0) dist = 0d0         ! little threshold to achieve target
        end if 

        correction = 0.7d0               ! glide ration is quite sensible to changes
        increment = (op_spec%target_value + dist * correction) * op_spec%scale_factor 

      elseif (trim(opt_type) == 'target-lift') then

      ! Minimize difference between target cl value and current value 
      !    Add a base value to the lift difference
      
        cur_value = op%cl

        if (op_spec%allow_improved_target) then
          dist = max (0d0, (op_spec%target_value - cur_value))
        else 
          dist = abs(cur_value - op_spec%target_value)
          if (dist < 0.001d0) dist = 0d0         ! little threshold to achieve target
        end if 

        correction = 0.8d0               ! lift is quite sensible to changes
        increment = (1.d0 + dist * correction)  * op_spec%scale_factor 

      elseif (trim(opt_type) == 'target-moment') then

      ! Minimize difference between target moment value and current value 
      !        Add a base value (Clark y or so ;-) to the moment difference
      !        so the relative change won't be to high
        cur_value = op%cm

        if (op_spec%allow_improved_target) then
          dist = max (0d0, (op_spec%target_value - cur_value))
        else 
          dist = abs(cur_value - op_spec%target_value)
          if (dist < 0.001d0) dist = 0d0         ! little threshold to achieve target
        end if 

        increment = (dist + 0.05d0) * op_spec%scale_factor

      elseif (trim(opt_type) == 'max-lift') then

      ! Maximize Cl (at given angle of attack)

        if (op%cl > 0.d0) then
          increment = op_spec%scale_factor / op%cl
        else
          increment = 1.D9   ! Big penalty for lift <= 0
        end if
        cur_value = op%cl

      elseif (trim(opt_type) == 'max-xtr') then

      ! Maximize laminar flow on top and bottom (0.1 factor to ensure no
      !   division by 0)

        increment = op_spec%scale_factor/(0.5d0*(op%xtrt + op%xtrb)+0.1d0)
        cur_value = 0.5d0*(op%xtrt + op%xtrb)

      ! Following optimization based on slope of the curve of op_point
      !         convert alpha in rad to get more realistic slope values
      !         convert slope in rad to get a linear target 
      !         factor eg 4.d0*pi to adjust range of objective function (not negative)

      elseif (trim(opt_type) == 'max-lift-slope') then

      ! Maximize dCl/dalpha (0.1 factor to ensure no division by 0)

        slope = derivation_at_point (i, (op_points_result%alpha * pi/180.d0) , &
                                        (op_points_result%cl))
        increment = op_spec%scale_factor / (atan(abs(slope))  + 2.d0*pi)
        cur_value = atan(abs(slope))

      elseif (trim(opt_type) == 'min-lift-slope') then

      ! Minimize dCl/dalpha e.g. to reach clmax at alpha(i) 
        slope = derivation_at_point (i, (op_points_result%alpha * pi/180.d0) , &
                                        (op_points_result%cl))

        increment = op_spec%scale_factor * (atan(abs(slope)) + 2.d0*pi)
        cur_value = atan(abs(slope))

      elseif (trim(opt_type) == 'min-glide-slope') then

      ! Minimize d(cl/cd)/dcl e.g. to reach best glide at alpha(i) 
        slope = derivation_at_point (i, (op_points_result%cl * 20d0), &
                                        (op_points_result%cl/op_points_result%cd))

        increment = op_spec%scale_factor * (atan(abs(slope))  + 2.d0*pi)
        cur_value = atan(abs(slope))  

      else

        write(*,*)
        write(*,*) "Error: requested optimization_type not recognized."
        stop

      end if

      aero_objective_function_on_results = aero_objective_function_on_results &
                                          + op_spec%weighting * increment
    end if
  end do

end function aero_objective_function_on_results



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
  !if (match_delta < 1d-10)  match_delta = 1d-1 

  ! Scale result to initial value 1.
  matchfoil_objective_function = match_delta * match_foils_scale_factor

end function matchfoil_objective_function


!-----------------------------------------------------------------------------
!
! Objective function for matching a bezier curve to an airfoil side 
!
!-----------------------------------------------------------------------------
function match_side_objective_function(dv, dummy)

  !! objective function for adapt bezier to foil side
  
  use airfoil_shape_bezier, only : bezier_eval_y_on_x, dv_to_bezier

  double precision, intent(in) :: dv(:)
  logical, intent(in), optional :: dummy
  double precision :: match_side_objective_function 

  double precision, allocatable :: px(:), py(:), design_y(:), devi (:), base(:)
  double precision, allocatable :: target_x(:), target_y(:)
  double precision :: te_gap, shift
  integer     :: i, ncoord, step, nTarg
  
  if (dummy) then 
  end if 

  ncoord = size(side_to_match_x)

  ! remap bezier control points out of design variables 

  te_gap = side_to_match_y (ncoord)                     ! last bezier point fixed to te gap 
  call dv_to_bezier (dv, te_gap, px, py)

  ! sanity check - nelder mead could have crossed to bounds or changed to order of control points 

  if (minval(px) < 0d0 .or. maxval(px) > 1.0) then 
    match_side_objective_function = 9999d0              ! penalty crossed bounds
    return
  end if 

  do i = 2, size(px) - 1
    if (((px(i+1) - px(i)) < 0d0) .or. ((px(i+1) - px(i)) < 0.03d0)) then  
      match_side_objective_function = 9999d0            ! penalty changed order or too close 
      return
    end if 
  end do 

  if (abs(py(2)) < 0.006d0) then 
    match_side_objective_function = 9999d0              ! penalty LE too sharp (tangent too short) 
    return
  end if 


  ! evaluate bezier for all x coord of side to match 
  !     in module: side_to_match_x (:), side_to_match_y (:)

  i = 1
  if (ncoord > 120) then
    step = 6
  elseif (ncoord > 80) then
    step = 5
  else 
    step = 3
  end if 

  allocate (target_x(ncoord))
  allocate (target_y(ncoord))

  nTarg = 0
  do i = 1, ncoord, step 
    nTarg = nTarg + 1
    target_x(nTarg) = side_to_match_x(i)
    target_y(nTarg) = side_to_match_y(i)
  end do 


  target_x = target_x(1:nTarg) 
  target_y = target_y(1:nTarg) 
  allocate (design_y(nTarg))
  allocate (devi(nTarg))
  allocate (base(nTarg))

  do i = 1, nTarg 
    design_y(i) = bezier_eval_y_on_x (px, py, target_x(i), epsilon=1d-9)
    ! devi (i) = abs (side_to_match_y(i) - design_y(i))
  end do 
  
  devi = abs (design_y - target_y)
  base = abs(target_y)

  ! move base so targets with a small base (at TE) don't become overweighted 
  shift =  maxVal (base) * 0.4

  match_side_objective_function = norm2 (devi / (base+shift))

end function match_side_objective_function



subroutine write_progress (designvars, designcounter)
  !! Generic function to write designs. Selects either match_foils or 'normaö'

  double precision, dimension(:), intent(in) :: designvars
  integer, intent(in) :: designcounter
  integer :: write_stat                     ! currently not used 

  if (match_foils) then
    write_stat = write_matchfoil_optimization_progress(designvars, designcounter)
  else
    write_stat = write_airfoil_optimization_progress(designvars, designcounter)
  end if

end subroutine write_progress


!===============================================================================
!
! Create an airfoil out of a seed airfoil and designvars 
!
!===============================================================================

subroutine create_airfoil_form_design (seed, dv, foil)

  use vardef,               only: airfoil_type
  use vardef,               only: shape_functions
  use airfoil_operations,   only: rebuild_airfoil, split_foil_at_00
  use parametrization,      only: create_airfoil_camb_thick
  use parametrization,      only: create_airfoil_camb_thick_plus
  use parametrization,      only: create_airfoil_hicks_henne
  use parametrization,      only: create_airfoil_bezier
  use parametrization,      only: top_shape_function, bot_shape_function
  
  type(airfoil_type), intent(in)              :: seed
  type(airfoil_type), intent(out)             :: foil
  double precision, dimension(:), intent(in)  :: dv

  integer :: nmodest, nmodesb, dvtbnd1, dvtbnd2, dvbbnd1, dvbbnd2
  double precision, dimension(size(seed%xt,1)) :: zt_new
  double precision, dimension(size(seed%xb,1)) :: zb_new

  double precision, allocatable :: dv_top(:), dv_bot(:)

! Build airfoil to evaluate out of seed airfoil plus shape functions applied

  nmodest = size(top_shape_function,1)      ! = nfunctions_top (other name space) 
  nmodesb = size(bot_shape_function,1)

! Set modes for top and bottom surfaces

  if ((trim(shape_functions) == 'camb-thick') .or. &
      (trim(shape_functions) == 'camb-thick-plus')) then
    dvtbnd1 = 1
    dvtbnd2 = nmodest
    dvbbnd1 = 1
    dvbbnd2 = dvtbnd2
  elseif (trim(shape_functions) == 'bezier') then 
    dvtbnd1 = 1
    dvtbnd2 = nmodest
    dvbbnd2 = nmodest + nmodesb 
    dvbbnd1 = dvtbnd2 + 1
  else
    dvtbnd1 = 1
    dvtbnd2 = nmodest*3
    dvbbnd2 = nmodest*3 + nmodesb*3
    dvbbnd1 = dvtbnd2 + 1
  end if
  
! Overwrite lower DVs for symmetrical airfoils 

  if (seed%symmetrical) then
    dvbbnd1 = 1
    dvbbnd2 = dvtbnd2
    foil%symmetrical = .true.
  else
    foil%symmetrical = .false.
  end if

! Extract designvars for top and bot 

  dv_top = dv(dvtbnd1:dvtbnd2)
  dv_bot = dv(dvbbnd1:dvbbnd2)

  if (trim(shape_functions) == 'camb-thick') then

    ! Create new airfoil by changing camber and thickness of seed airfoil.
    call create_airfoil_camb_thick(seed%xt, seed%zt, seed%xb, seed%zb,       &
                                   dv_top, zt_new, zb_new)
    call rebuild_airfoil (seed%xt, seed%xb, zt_new, zb_new, foil)

  else if (trim(shape_functions) == 'camb-thick-plus') then

    ! Create new airfoil by changing camber and thickness of seed airfoil, 
    ! top and bottom seperately
    call create_airfoil_camb_thick_plus(seed%xt, seed%zt, seed%xb, seed%zb,       &
                                        dv_top, zt_new, zb_new)                      
    call rebuild_airfoil (seed%xt, seed%xb, zt_new, zb_new, foil)

  else if (trim(shape_functions) == 'bezier') then
    
    ! Bezier: create new airfoil by control points 
    ! - it is *not* dependant on seed airfoil  
    call create_airfoil_bezier (seed%zt, seed%zb, dv_top, dv_bot, foil)
    call split_foil_at_00 (foil)        ! maybe upper and lower will be needed

  else 

    ! Create top and bottom surfaces by perturbation of seed airfoil 
    call create_airfoil_hicks_henne(seed%xt, seed%zt, seed%xb, seed%zb,                      &
                        dv_top, dv_bot,&
                        zt_new, zb_new, shape_functions, seed%symmetrical)
    call rebuild_airfoil (seed%xt, seed%xb, zt_new, zb_new, foil)

  end if

end subroutine create_airfoil_form_design



!===============================================================================
! Get actual flap angles out of a design   
!===============================================================================

subroutine get_flap_degrees_from_design (designvars, actual_flap_degrees)

  use vardef,             only: shape_functions
  use vardef,             only: initial_perturb
  use vardef,             only: flap_spec, nflap_optimize, flap_optimize_points
  use vardef,             only: max_flap_degrees, min_flap_degrees, flap_degrees
  use parametrization,    only: top_shape_function, bot_shape_function

  double precision, dimension(:), intent(in) :: designvars
  double precision, dimension(noppoint), intent(out) :: actual_flap_degrees
 
  integer :: nmodest, nmodesb, dvtbnd1, dvtbnd2, dvbbnd1, dvbbnd2, ndvs, dvcounter
  integer :: i, flap_idx
  double precision :: ffact


  actual_flap_degrees = 0d0

  if (.not. flap_spec%use_flap) return

! Build airfoil to evaluate out of seed airfoil plus shape functions applied

  nmodest = size(top_shape_function,1)
  nmodesb = size(bot_shape_function,1)

! Set modes for top and bottom surfaces

  if ((trim(shape_functions) == 'camb-thick') .or. &
           (trim(shape_functions) == 'camb-thick-plus')) then
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




function write_airfoil_optimization_progress(designvars, designcounter)

  !! Writes airfoil coordinates and op Points results to files during optimization
  !!   designcounter = 0 will start new files 

  use math_deps,          only : interp_vector, min_threshold_for_reversals, derivative3
  use airfoil_operations, only : airfoil_write_to_unit, assess_surface
  use xfoil_driver,       only : run_op_points, op_point_result_type
  use xfoil_driver,       only : xfoil_get_geometry_info, xfoil_set_airfoil

  double precision, intent(in)  :: designvars (:)
  integer, intent(in)           :: designcounter
  integer :: write_airfoil_optimization_progress

  type(airfoil_type)                :: foil
  type(xfoil_options_type)          :: local_xfoil_options
  type(geo_result_type)             :: geo_result
  type(op_point_result_type), allocatable :: op_points_result (:)

  double precision, dimension(noppoint) :: actual_flap_degrees
 
  character(:), allocatable :: foil_file, bezier_file, op_points_file
  integer        :: foil_unit, bezier_unit, op_points_unit
  logical        :: dynamic_done
 

  if (designcounter == 0) then
    call print_colored (COLOR_NORMAL,' - Writing design #0 being seed airfoil')
  else
    call print_colored (COLOR_NORMAL,' -> Writing design #'//stri(designcounter))
  end if
  write (*,*)
  if (show_details .and. (designcounter > 0)) write (*,*) 

! Design 0 is seed airfoil to output - take the original values 
  if (designcounter == 0) then 
    foil = seed_foil
! Design > 0 - Build current foil out seed foil and current design 
  else 
    call create_airfoil_form_design (seed_foil, designvars, foil)
    foil%name = output_prefix
  end if

! Get actual flap angles based on design variables

  call get_flap_degrees_from_design (designvars, actual_flap_degrees)

! Try to get xfoil result for foil from "save best" in objective function

  if (allocated(best_op_points_result)) then 
  ! Sanity check - Is the "best" really our current foil
    if (abs(sum(foil%z) - sum(best_foil%z)) < 1d-10 ) then  ! use epsilon (num issues with symmetrical) 
      op_points_result = best_op_points_result
    else
      best_objective = 1d0                 ! reset best store - something wrong...?
    end if 
  end if 

! There is no stored result - so re-calc for this foil
  if (.not. allocated(op_points_result)) then 

  ! Analyze airfoil at requested operating conditions with Xfoil

    local_xfoil_options = xfoil_options
    local_xfoil_options%show_details        = .false.  
    local_xfoil_options%exit_if_unconverged = .false.  ! we need all op points
    if (designcounter == 0) then
      local_xfoil_options%reinitialize = .false.       ! strange: reinit leeds sometimes to not converged
      local_xfoil_options%show_details = .false.
    end if 
    call run_op_points (foil, xfoil_geom_options, local_xfoil_options,  &
                        flap_spec, actual_flap_degrees, &
                        op_points_spec, op_points_result)
  end if 
     
! Set output file names and identifiers

  foil_file       = design_subdir//'Design_Coordinates.csv'
  op_points_file  = design_subdir//'Design_OpPoints.csv'
  bezier_file     = design_subdir//'Design_Beziers.csv'

  foil_unit       = 13
  op_points_unit  = 14
  bezier_unit     = 15

! Open files of design data ...

  if (designcounter == 0) then

  ! ... design=0 (seed airfoil), write header and opPoint specifications

    open(unit=op_points_unit, file=op_points_file, status='replace', err=901)
    call write_design_op_points_header (op_points_unit)

    if (shape_functions == 'bezier') then 
      open(unit=bezier_unit, file= bezier_file, status='replace', err=902)
      call write_design_bezier_header (bezier_unit, foil)
    else 
      open(unit=foil_unit,   file=foil_file,    status='replace', err=900)
      call write_design_coord_header (foil_unit, foil)
    end if 

  else

  ! ... design > 0 

    open (unit=op_points_unit, file=op_points_file, status='old', position='append', err=901)
    if (shape_functions == 'bezier') then 
      open (unit=bezier_unit,  file= bezier_file,  status='old', position='append', err=902)
    else
      open (unit=foil_unit,    file=foil_file,     status='old', position='append', err=900)
    end if 

  end if

! Write design data 

  call write_design_op_points_data (op_points_unit, designcounter, op_points_spec, op_points_result, flap_degrees)
  if (shape_functions == 'bezier') then 
    call write_design_bezier_data   (bezier_unit, designcounter, foil)
  else 
    call write_design_coord_data (foil_unit, designcounter, foil)
  end if 

! done 

  close (foil_unit)
  close (op_points_unit)
  close (bezier_unit)


! ----- Actions when new design was found --------------------------------------

! Evaluate geometry results of current design foil

  call xfoil_set_airfoil (foil)        
  call xfoil_get_geometry_info (geo_result%maxt, geo_result%xmaxt, &
                                geo_result%maxc, geo_result%xmaxc)


! Dynamic Weighting of op points and geo targets

  if (dynamic_weighting_spec%active) then 
    call do_dynamic_weighting (designcounter, dynamic_weighting_spec, & 
                               op_points_result, geo_result, dynamic_done)
  else
    dynamic_done = .false.
  end if

  if (show_details .and. (designcounter > 0)) then 
    call show_optimization_progress  (op_points_result, geo_result, dynamic_done) 
  end if

! Testing:  Write op points deviation to file
!  call write_op_results (designcounter, op_points_result, geo_result) 
!  call write_designvars (designcounter, designvars) 
!  call print_designvars (designcounter, designvars) 

  write_airfoil_optimization_progress = 0

  return

! File I/O Warnings 

900 call print_warning ("Warning: unable to open "//foil_file//". Skipping ...")
  write_airfoil_optimization_progress = 1
  return
901 call print_warning ("Warning: unable to open "//op_points_file//". Skipping ...")
  write_airfoil_optimization_progress = 2
  return
902 call print_warning ("Warning: unable to open "//bezier_file//". Skipping ...")
  write_airfoil_optimization_progress = 3
  return

end function write_airfoil_optimization_progress



subroutine write_design_op_points_header (iunit)
  !! write csv header of op points data 
  integer, intent(in) :: iunit
  write (iunit, '(A5,";",A4,8(";",A11)";",A9)') '  No', "iOp", "alpha", "cl", "cd", "cm", "xtrt", "xtrb", "dist", "dev", "flap"
end subroutine



subroutine write_design_op_points_data (iunit, design, op_points_specification, op_points_result, flap)
  !! write csv op points result  
  integer, intent(in)                     :: iunit, design
  type(op_point_specification_type), intent(in)  :: op_points_specification (:)
  type(op_point_result_type), intent(in)  :: op_points_result (:)
  double precision, intent(in)            :: flap (:)

  type(op_point_result_type)              :: op
  type(op_point_specification_type)       :: op_spec 
  integer                     :: i,how_good
  double precision            :: dist, dev

  do i = 1, noppoint
    op = op_points_result(i) 
    op_spec = op_points_specification(i) 
    call get_op_improvement(op_spec, op, dist, dev, how_good)

    if (.not. op%converged) then 
      call print_error ('  Op '//stri(i)//' not converged in final calculation (this should not happen...)')
    end if 
    write(iunit,'(I5,";",I4, 8(";",F11.6), ";",F9.4)') & 
                design, i, op%alpha, op%cl, op%cd, op%cm, op%xtrt, op%xtrb, dist, dev, flap (i)
  end do
end subroutine



subroutine write_op_spec (filename, iunit, op_points_specification, xfoil_ncrit)
  !! write csv op points specification   
  character(:), allocatable, intent(in)   :: filename
  integer, intent(in)                     :: iunit
  type(op_point_specification_type), intent(in)  :: op_points_specification (:)
  double precision, intent(in)            :: xfoil_ncrit 

  type(op_point_specification_type) :: op_spec
  double precision                  :: ncrit 
  integer                           :: i

  open(unit=iunit, file=filename, status='replace', err=900)

  write (iunit, '(" ",A)',   advance='no') 'iOp'
  write (iunit, '("; ",A)',  advance='no') 'spec_cl'
  write (iunit, '("; ",A)',  advance='no') 'value'
  write (iunit, '("; ",A)',  advance='no') 're_number'
  write (iunit, '("; ",A)',  advance='no') 're_type'
  write (iunit, '("; ",A)',  advance='no') 'ma_number'
  write (iunit, '("; ",A)',  advance='no') 'ma_type'
  write (iunit, '("; ",A)',  advance='no') 'ncrit'
  write (iunit, '("; ",A)',  advance='no') 'optimization_type'
  write (iunit, '("; ",A)',  advance='no') 'target_value'
  write (iunit, '("; ",A)',  advance='no') 'allow_improved_target'
  write (iunit, '("; ",A)',  advance='no') 'weighting_user'
  write (iunit, '("; ",A)',  advance='no') 'dynamic_weighting'
  write (iunit,*)

  do i = 1, size (op_points_specification)

    op_spec = op_points_specification(i) 

    if (op_spec%ncrit /= -1d0) then 
      ncrit = op_spec%ncrit
    else
      ncrit = xfoil_ncrit
    end if 

    write (iunit, '(I4)',         advance='no') i
    write (iunit, '(";",L4)',     advance='no') op_spec%spec_cl
    write (iunit, '(";",F11.6)',  advance='no') op_spec%value
    write (iunit, '(";",F11.0)',  advance='no') op_spec%re%number
    write (iunit, '(";",I4)',     advance='no') op_spec%re%type
    write (iunit, '(";",F11.0)',  advance='no') op_spec%ma%number
    write (iunit, '(";",I4)',     advance='no') op_spec%ma%type
    write (iunit, '(";",F7.1)',   advance='no') ncrit
    write (iunit, '(";",A15)',    advance='no') op_spec%optimization_type
    write (iunit, '(";",F11.6)',  advance='no') op_spec%target_value
    write (iunit, '(";",L4)',     advance='no') op_spec%allow_improved_target
    write (iunit, '(";",F7.2)',   advance='no') op_spec%weighting_user
    write (iunit, '(";",L4)',     advance='no') op_spec%dynamic_weighting
    write (iunit,*)

  end do

  close (iunit)

  return

  ! File I/O Warnings 

900 call print_warning ("Warning: unable to open "//filename//". Skipping ...")
  return

end subroutine



subroutine write_design_coord_header (iunit, foil)
  !! write csv header of design airfoil coordinates 

  integer, intent(in) :: iunit
  type(airfoil_type), intent(in)  :: foil
  integer     :: i
  write (iunit, '(A5,";",A15, ";",A6)', advance='no') '  No', 'Name', 'Coord'
  do i = 1,size(foil%x)
    write (iunit, '(";",I10)', advance='no') i
  end do 
  write (iunit,*)

end subroutine



subroutine write_design_coord_data (iunit, design, foil)
  !! write csv design data - coordinates of airfoil

  integer, intent(in) :: iunit, design
  type(airfoil_type), intent(in)  :: foil
  character(:), allocatable :: name 
  integer     :: i

  name = design_foil_name (design, foil)
  write (iunit, '(I5,";",A,";",A5)', advance='no') design, name, 'x'
  do i = 1,size (foil%x)
    write (iunit, '(";",F10.7)', advance='no') foil%x(i)
  end do 
  write (iunit,*)

  write (iunit, '(I5,";",A,";",A5)', advance='no') design, name, 'y'
  do i = 1,size (foil%x)
    write (iunit, '(";",F10.7)', advance='no') foil%z(i)
  end do 
  write (iunit,*)

end subroutine



subroutine write_design_bezier_header (iunit, foil)
  !! write csv header of bezier design data 

  integer, intent(in) :: iunit
  type(airfoil_type), intent(in)  :: foil
  integer     :: i
  write (iunit, '(A5,";",A15, ";",A5)', advance='no') '  No', 'Name', 'Side'
  do i = 1,max(foil%bezier_spec%ncpoints_top, foil%bezier_spec%ncpoints_bot)
    write (iunit, '(2(";",A12))', advance='no') 'p'//stri(i)//'x', 'p'//stri(i)//'y'
  end do 
  write (iunit,*)

end subroutine



subroutine write_design_bezier_data (iunit, design, foil)
  !! write csv design data - coordinates of control points

  integer, intent(in) :: iunit, design
  type(airfoil_type), intent(in)  :: foil
  type(bezier_spec_type)    :: bez_spec
  character(:), allocatable :: name 
  integer     :: i

  bez_spec = foil%bezier_spec
  name = design_foil_name (design, foil)
  write (iunit, '(I5,";",A,";",A5)', advance='no') design, name, 'Top'
  do i = 1,bez_spec%ncpoints_top
    write (iunit, '(2(";",F12.8))', advance='no') bez_spec%px_top(i), bez_spec%py_top(i)
  end do 
  write (iunit,*)
  write (iunit, '(I5,";",A,";",A5)', advance='no') design, name, 'Bot'
  do i = 1,bez_spec%ncpoints_bot
    write (iunit, '(2(";",F12.8))', advance='no') bez_spec%px_bot(i), bez_spec%py_bot(i)
  end do 
  write (iunit,*)

end subroutine


function design_foil_name (design, foil)
  !! name of airfoil design when writing design to file 
  integer, intent(in) :: design
  type(airfoil_type), intent(in)  :: foil
  character(:), allocatable       :: design_foil_name

  if (design == 0) then                             ! seed foil 
    design_foil_name = trim(foil%name)    
  else                                              ! design foils
    if (len_trim (foil%name) > 15) then
      design_foil_name = foil%name (1:15) // '__'
    else
      design_foil_name = trim(foil%name)
    end if 
    design_foil_name = design_foil_name // '~'//stri(design)
  end if 

end function


!------------------------------------------------------------------------------
!
!  Dynamic weighting
!
!   recalc weighting of each op-point depending on its deviation to target value
!  
!   Returns new weighting in op_points_spec
!
!------------------------------------------------------------------------------
subroutine do_dynamic_weighting (designcounter, dyn_weight_spec, &
                                 op_points_result, geo_result, &
                                 dynamic_done) 

  use xfoil_driver,       only : op_point_result_type, op_point_specification_type
  use math_deps,          only : median

  integer, intent(in) :: designcounter
  type(dynamic_weighting_specification_type), intent(in)  :: dyn_weight_spec
  type(op_point_result_type), dimension(:), intent(in)    :: op_points_result
  type(geo_result_type),  intent(in)                      :: geo_result
  logical, intent(out) :: dynamic_done

  type(dynamic_variable_type), dimension(:), allocatable :: dyn_ops, dyn_geos
  doubleprecision, dimension(:), allocatable             :: dyn_devs

  integer                           :: i, ndyn, j, nop, ngeo_targets
  doubleprecision                   :: avg_dev, sum_weighting_user, median_dev, weighting_diff
  doubleprecision                   :: new_dyn_obj_fun, cur_dyn_obj_fun, scale_dyn_obj_fun
  doubleprecision                   :: min_new_weighting,max_new_weighting, min_weighting, max_weighting
  logical                           :: show_dev

  doubleprecision, parameter        :: EXTRA_PUNCH_THRESHOLD = 1.5d0
  doubleprecision, parameter        :: SUPER_PUNCH_THRESHOLD = 3.0d0
  doubleprecision, parameter        :: REDUCTION = 1.3d0

  nop    = size(op_points_spec)
  ngeo_targets = size(geo_targets)

  allocate (dyn_ops(nop))
  allocate (dyn_geos(ngeo_targets))

  dynamic_done = .false. 
  show_dev     = .false.
  
! dyn weighting only if design counter matches frequency
  
  if ((designcounter < dyn_weight_spec%start_with_design) .or. & 
      (mod(designcounter- dyn_weight_spec%start_with_design, dyn_weight_spec%frequency) /= 0)) then
    return
  end if

! get all the data from op points xfoil result and geo targets which are relevant

  ndyn = 0 
  call collect_dyn_ops_data (op_points_result, dyn_ops, ndyn)
  call collect_dyn_geo_data (geo_result,       dyn_geos,ndyn)
  if(ndyn == 0 ) return

! average and median of all deviations 

  allocate (dyn_devs(ndyn))

  j = 0
  do i= 1, nop
    if (op_points_spec(i)%dynamic_weighting) then
      j = j + 1
      dyn_devs(j) = abs( dyn_ops(i)%dev)
    end if
  end do 
  do i= 1, ngeo_targets
    if (geo_targets(i)%dynamic_weighting) then
      j = j + 1
      dyn_devs(j) = abs( dyn_geos(i)%dev)
    end if
  end do 
  avg_dev    = sum    ( dyn_devs (1:j)) / j
  median_dev = median ( dyn_devs (1:j)) 

  write (*,*) 
  call print_colored (COLOR_FEATURE,' - Dynamic Weighting')
  call print_colored (COLOR_PALE,' of '//stri(ndyn)//' targets' //&
                                 ' having an average deviation of '//strf('(F4.1)', avg_dev)//'%' //&
                                 ' and a median of '//strf('(F4.1)', median_dev)//'%')
                            
  write (*,*) 
  write (*,*) 

! Dynamic weighting ----------------------------------------------------------

  min_new_weighting = 9999d0
  max_new_weighting = 0d0

  if (designcounter == 0) then 
    ! first initial design: start with user defined / default weighting
    dyn_ops%new_weighting = dyn_ops%weighting
  else

  ! 1. first guess of new weighting of relevant op_points and new objective function
  !    weighting is proportional to the deviation to target compared to average deviation

    do i= 1, nop
      if (op_points_spec(i)%dynamic_weighting) then

        dyn_ops(i)%new_weighting = abs(dyn_ops(i)%dev) / (avg_dev) 

        min_new_weighting = min (dyn_ops(i)%new_weighting, min_new_weighting)
        max_new_weighting = max (dyn_ops(i)%new_weighting, max_new_weighting)
      end if
    end do 
  
    do i= 1, ngeo_targets
      if (geo_targets(i)%dynamic_weighting) then

        dyn_geos(i)%new_weighting = abs(dyn_geos(i)%dev) / avg_dev 

        min_new_weighting = min (dyn_geos(i)%new_weighting, min_new_weighting)
        max_new_weighting = max (dyn_geos(i)%new_weighting, max_new_weighting)
      end if
    end do 

  ! 1b. Reduce weighting range when average deviation getting close to 0 
  !         to avoid oscillation  

    min_weighting     = dyn_weight_spec%min_weighting  
    max_weighting     = dyn_weight_spec%max_weighting

    weighting_diff = max_weighting - min_weighting

    if (avg_dev < 0.1d0) then 
      weighting_diff = weighting_diff / REDUCTION ** 2 
    elseif (avg_dev < 0.5d0) then 
      weighting_diff = weighting_diff / REDUCTION 
    end if 
    max_weighting     = min_weighting + weighting_diff
    if (show_dev) write (*,'(8x,A,2F5.2)') '- Min / Max     weighting  ',min_weighting, max_weighting 
    if (show_dev) write (*,'(8x,A,2F5.2)') '- Min / Max new weighting  ',min_new_weighting, max_new_weighting 

  
  ! 2. Scale weighting of each op point to defined weighting range 

    do i= 1, nop
      if (op_points_spec(i)%dynamic_weighting) then

        dyn_ops(i)%new_weighting = min_weighting + &
                      (dyn_ops(i)%new_weighting - min_new_weighting) * &
                      ((max_weighting-min_weighting) / (max_new_weighting-min_new_weighting)) 

        if (show_dev) write (*,'(8x, A,I2,3x, A,F6.2,A,F5.1)', advance='no') '- Op ', i, &
                 ' dev:',dyn_ops(i)%dev,'  weight:', dyn_ops(i)%new_weighting

    ! 2a. Outlier - give a super extra punch if deviation is too far away from average deviation

        if ((abs(dyn_ops(i)%dev) > (avg_dev * SUPER_PUNCH_THRESHOLD )) .and. &
                 (avg_dev > 0.3d0)) then

          dyn_ops(i)%new_weighting = dyn_ops(i)%new_weighting * dyn_weight_spec%extra_punch **2
          op_points_spec(i)%extra_punch = .true.
          if (show_dev) write (*,'(A, F5.1)') ' +punch:', dyn_ops(i)%new_weighting

    ! 2b. Bad Op - give a extra punch if deviation is quite far away from average deviation

          elseif ((abs(dyn_ops(i)%dev) > (avg_dev * EXTRA_PUNCH_THRESHOLD )) .and. &
                (avg_dev > 0.1d0)) then

          dyn_ops(i)%new_weighting = dyn_ops(i)%new_weighting * dyn_weight_spec%extra_punch
          op_points_spec(i)%extra_punch = .true.
          if (show_dev) write (*,'(A, F5.1)') '  punch:', dyn_ops(i)%new_weighting

    ! 2c. Former bad Op - also give a good, at least medium weighting if op had a punch before
    !                     to avoid oscillation of results 

        elseif (op_points_spec(i)%extra_punch) then 
          dyn_ops(i)%new_weighting = & 
              max (dyn_ops(i)%new_weighting, (max_weighting * 0.9d0))
          op_points_spec(i)%extra_punch = .false.
          if (show_dev) write (*,'(A, F5.1)') '   post:', dyn_ops(i)%new_weighting

        else
          op_points_spec(i)%extra_punch = .false.
          if (show_dev) write (*,*)
        end if
      end if
    end do 

    do i= 1, ngeo_targets
      if (geo_targets(i)%dynamic_weighting) then

      ! 2b. Scale current deviation range to defined weighting range and set weighting for op point
        dyn_geos(i)%new_weighting = min_weighting + &
                      (dyn_geos(i)%new_weighting - min_new_weighting) * &
                      ((max_weighting-min_weighting) / (max_new_weighting-min_new_weighting)) 
                      
        if (show_dev) write (*,'(8x, A,I2,3x, A,F6.2,A,F5.1)', advance='no') '- Geo', i, &
                 ' dev:',dyn_geos(i)%dev,'  weight:', dyn_geos(i)%new_weighting

      ! 3b. give an extra punch if deviation is too far away from average deviation

        if ((abs(dyn_geos(i)%dev) > (median_dev * EXTRA_PUNCH_THRESHOLD )) .and. &
                (avg_dev  > 0.2d0)) then
          dyn_geos(i)%new_weighting = dyn_geos(i)%new_weighting * dyn_weight_spec%extra_punch
          geo_targets(i)%extra_punch = .true.
          if (show_dev) write (*,'(A, F5.1)') '  punch:', dyn_geos(i)%new_weighting

        elseif (geo_targets(i)%extra_punch) then 
        ! ... also give a good, at least medium weighting if op had a punch before
        !     to avoid oscillation of results 
          dyn_geos(i)%new_weighting = & 
              max (dyn_geos(i)%new_weighting, (max_weighting + min_weighting) / 2d0)
          geo_targets(i)%extra_punch = .false.
          if (show_dev) write (*,'(A, F5.1)') '   post:', dyn_geos(i)%new_weighting
        else
          geo_targets(i)%extra_punch = .false.
          if (show_dev) write (*,*)
        end if
      end if
    end do 

  ! 4. Multiply weighting by user defined weighting - default = 1 but user may overwrite  

    do i= 1, nop
      if (op_points_spec(i)%dynamic_weighting) &
        dyn_ops(i)%new_weighting = dyn_ops(i)%new_weighting * op_points_spec(i)%weighting_user 
    end do 

    do i= 1, ngeo_targets
      if (geo_targets(i)%dynamic_weighting) &
        dyn_geos(i)%new_weighting = dyn_geos(i)%new_weighting * geo_targets(i)%weighting_user
    end do 

  ! 5. cur and new objective function with new (raw) weightings)

    cur_dyn_obj_fun = aero_objective_function_on_results (op_points_result, .true.) + &
                      geo_objective_function_on_results (geo_result, .true. )

    op_points_spec%weighting = dyn_ops%new_weighting
    geo_targets%weighting    = dyn_geos%new_weighting

    new_dyn_obj_fun = aero_objective_function_on_results (op_points_result, .true.) + &
                      geo_objective_function_on_results  (geo_result, .true. )


  ! 6. Done - scale and assign new weightings to current optimization weightings


    scale_dyn_obj_fun = cur_dyn_obj_fun / new_dyn_obj_fun

    do i= 1, nop
      if (op_points_spec(i)%dynamic_weighting) &
        op_points_spec(i)%weighting = dyn_ops(i)%new_weighting * scale_dyn_obj_fun
    end do
    do i= 1, ngeo_targets
      if (geo_targets(i)%dynamic_weighting) &
        geo_targets(i)%weighting    = dyn_geos(i)%new_weighting * scale_dyn_obj_fun
    end do

  ! 7. Store data for user information 
                         
    sum_weighting_user = sum(op_points_spec%weighting_user) + &
                         sum(geo_targets%weighting_user)
 
    op_points_spec%weighting_user_prv = dyn_ops%weighting        * sum_weighting_user
    op_points_spec%weighting_user_cur = op_points_spec%weighting * sum_weighting_user

    geo_targets%weighting_user_prv    = dyn_geos%weighting       * sum_weighting_user
    geo_targets%weighting_user_cur    = geo_targets%weighting    * sum_weighting_user

    dynamic_done = .true. 

  end if

end subroutine


!------------------------------------------------------------------------------
!  get the data for dynamic op points from op points spec and xfoil results
!------------------------------------------------------------------------------
subroutine collect_dyn_ops_data (op_points_result, dyn_ops, ndyn)

  use xfoil_driver,       only : op_point_result_type, op_point_specification_type

  type(op_point_result_type), dimension(:), intent(in)         :: op_points_result
  type(dynamic_variable_type), dimension(:), intent(inout)     :: dyn_ops
  integer, intent(inout)           :: ndyn

  type(op_point_specification_type) :: op_spec
  type(op_point_result_type)        :: op
  character(15)                     :: opt_type
  integer                           :: i
  doubleprecision                   :: dist, cur_value
  
  dyn_ops%weighting     = op_points_spec%weighting
  dyn_ops%new_weighting = op_points_spec%weighting
  dyn_ops%dev = 0d0

  do i= 1, size(op_points_spec)

    opt_type = op_points_spec(i)%optimization_type
    op_spec  = op_points_spec(i)
    op       = op_points_result(i) 

    if (op%converged) then

      if (op_spec%dynamic_weighting) ndyn = ndyn + 1

      select case  (trim(opt_type))

        case ('target-drag')

          cur_value = op%cd

          if (op_spec%allow_improved_target) then
            dist = max (0d0, (cur_value - op_spec%target_value))
          else 
            dist = abs(cur_value - op_spec%target_value)
            if (dist < 0.000004d0) dist = 0d0         ! little threshold to achieve target
          end if 
    
          dyn_ops(i)%dev = dist / op_spec%target_value * 100d0

        case ('target-glide')

          cur_value = op%cl / op%cd

          if (op_spec%allow_improved_target) then
            dist = max (0d0, (op_spec%target_value - cur_value))
          else 
            dist = abs(cur_value - op_spec%target_value)
            if (dist < 0.01d0) dist = 0d0         ! little threshold to achieve target
          end if 
  
          dyn_ops(i)%dev = dist / op_spec%target_value * 100d0

        case ('target-lift')

          cur_value = op%cl

          if (op_spec%allow_improved_target) then
            dist = max (0d0, (op_spec%target_value - cur_value))
          else 
            dist = abs(cur_value - op_spec%target_value)
            if (dist < 0.001d0) dist = 0d0         ! little threshold to achieve target
          end if 
    
          dyn_ops(i)%dev = dist / (1d0 + op_spec%target_value) * 100d0

        case ('target-moment')

          cur_value = op%cm

          if (op_spec%allow_improved_target) then
            dist = max (0d0, (op_spec%target_value - cur_value))
          else 
            dist = abs(cur_value - op_spec%target_value)
            if (dist < 0.001d0) dist = 0d0         ! little threshold to achieve target
          end if 
  
          dyn_ops(i)%dev  = dist / (0.05d0 + op_spec%target_value) * 100d0

      end select
    end if
  end do

end subroutine collect_dyn_ops_data

!------------------------------------------------------------------------------
!  get the data for dynamic op points from geo targets spec and geo results
!------------------------------------------------------------------------------
subroutine collect_dyn_geo_data (geo_result, dyn_geos, ndyn)

  type(geo_result_type), intent(in)                        :: geo_result
  type(dynamic_variable_type), dimension(:), intent(inout) :: dyn_geos
  integer, intent(inout)           :: ndyn

  integer                           :: i
  doubleprecision                   :: dist
  
  dyn_geos%weighting     = geo_targets%weighting
  dyn_geos%new_weighting = geo_targets%weighting
  dyn_geos%dev = 0d0

  do i= 1, size(geo_targets)

      if (geo_targets(i)%dynamic_weighting) ndyn = ndyn + 1

      select case  (trim(geo_targets(i)%type))
        case ('Thickness')
          dist = geo_result%maxt - geo_targets(i)%target_value       ! positive is worse
          
        case ('Camber')
          dist = geo_result%maxc - geo_targets(i)%target_value       ! positive is worse
      end select
      dyn_geos(i)%dev = dist / geo_targets(i)%target_value * 100d0
  end do

end subroutine collect_dyn_geo_data


!------------------------------------------------------------------------------
! Prints op results during optimization (show_details) 
!------------------------------------------------------------------------------

subroutine show_optimization_progress (op_points_result, geo_result, &
                                       dynamic_done) 

  use xfoil_driver,       only : op_point_result_type

  type(op_point_result_type), dimension(:),  intent(in) :: op_points_result
  type(geo_result_type),  intent(in)                    :: geo_result
  logical, intent(out)               :: dynamic_done  

  type(op_point_result_type)        :: op
  type(op_point_specification_type) :: op_spec
  type(geo_target_type)             :: geo_spec
  integer             :: i, intent
  character (30)      :: s
  doubleprecision     :: val

  intent = 10
  call print_colored (COLOR_PALE, repeat(' ',intent))

  call print_colored (COLOR_PALE, 'Op'//'   ')
  call print_colored (COLOR_PALE, 'spec')
  call print_colored (COLOR_PALE, ' cl  '//'  ')
  call print_colored (COLOR_PALE, ' al  '//'  ')
  call print_colored (COLOR_PALE, ' cd   '//'  ')
  call print_colored (COLOR_PALE, 'glide'//'    ')
  call print_improvement_info (0, 'Type Base  deviat/improv')
  if (dynamic_done) then
    call print_dynamic_weighting_info (5, 'Dynamic Weighting')
  elseif (bubble_detected (op_points_result)) then 
    call print_bubble_info (3, 'Ttr bubble   Btr bubble ')
  end if 
  write (*,*)

! All op points - one per line -------------------------

  do i = 1, size(op_points_result)

    op      = op_points_result(i)
    op_spec = op_points_spec(i)

    call print_colored (COLOR_PALE, repeat(' ',intent))

    write (s,'(I2)') i 
    call print_colored (COLOR_PALE, trim(s)//'   ')
  ! --
    if (op_spec%spec_cl) then 
      s = 'cl'
    else
      s = 'al' 
    end if 
    call print_colored (COLOR_PALE, trim(s)//'  ')
  ! --
    write (s,'(F5.2)') op%cl
    call print_colored (COLOR_PALE, trim(s)//'  ')
  ! --
    write (s,'(F5.2)') op%alpha
    call print_colored (COLOR_PALE, trim(s)//'  ')
  ! --
    write (s,'(F6.5)') op%cd
    call print_colored (COLOR_PALE, trim(s)//'  ')
  ! --
    if (op%cl > 0.05d0) then 
      if ((op%cl/op%cd) > 99.9d0) then 
        write (s,'(F5.1)') op%cl/op%cd
      else
        write (s,'(F5.2)') op%cl/op%cd
      end if 
      call print_colored (COLOR_PALE, trim(s)//'    ')
    else 
      call print_colored (COLOR_PALE, '   - '//'    ')
    end if
  ! --
    call print_improvement_info (0, '', op_spec, op)
  ! --
    if (dynamic_done) then 
      call print_dynamic_weighting_info (5,'', op_spec)
    elseif (bubble_detected (op_points_result)) then 
      call print_bubble_info (3, '', op)
    end if 

    write (*,*)
  end do


! All Geo targets - one per line -------------------------

  if (size(geo_targets) > 0) write (*,*) 

  do i = 1, size(geo_targets)

    geo_spec = geo_targets(i)
    call print_colored (COLOR_PALE, repeat(' ',intent))

    write (s,'(I2)') i 
    call print_colored (COLOR_PALE, trim(s)//'   ')
  ! --
    call print_colored (COLOR_PALE, geo_spec%type(1:9))
  ! --
    call print_colored (COLOR_PALE, '   ')
    if (trim(geo_spec%type) == 'Thickness') then 
      val = geo_result%maxt
    elseif (trim(geo_spec%type) == 'Camber') then 
      val = geo_result%maxc
    else
      val = 0d0
    end if 
    write (s,'(F7.5)') val 
    call print_colored (COLOR_PALE, trim(s))
    call print_colored (COLOR_PALE, '                ')
  ! --
    call print_geo_improvement_info (0, '', geo_spec, geo_result)
  ! --
    if (dynamic_done) call print_geo_dynamic_weighting_info (5, '', geo_spec)

    write (*,*)
  end do

  write (*,*)

end subroutine show_optimization_progress


!-- width = 17 -----------------------------------------------------------------
subroutine print_dynamic_weighting_info (intent, header, op_spec)

  character (*), intent(in) :: header
  type(op_point_specification_type), intent(in), optional :: op_spec
  integer, intent(in) :: intent
  doubleprecision     :: old_val, val
  character (30)      :: s

  call print_colored (COLOR_PALE, repeat(' ',intent))
  if (present(op_spec)) then 
    val     = op_spec%weighting_user_cur
    old_val = op_spec%weighting_user_prv
    write (s,'(F3.1)') old_val
    call print_colored (COLOR_PALE, trim(s))
    if (op_spec%dynamic_weighting) then
      call print_colored (COLOR_PALE, ' -> ')
      write (s,'(F3.1)') val
      if (abs(old_val - val) / old_val > 0.1d0 ) then 
        call print_colored (COLOR_FEATURE, trim(s))
      else
        call print_colored (COLOR_PALE, trim(s))
      end if
      if (op_spec%extra_punch) then
        call print_colored (COLOR_FEATURE, '*') 
      else
        call print_colored (COLOR_HIGH, ' ') 
      end if
    else
      call print_colored (COLOR_PALE, '    ')
      call print_colored (COLOR_PALE, 'fix ') 
    end if
    call print_colored ( COLOR_PALE, '    ') 
  else
    write (s,'(A)') header
    call print_colored (COLOR_PALE, s (1:17))
  end if
 
end subroutine print_dynamic_weighting_info



subroutine print_improvement_info (intent, header, op_spec, op)
  !! print improvement of a single opPoint

  use xfoil_driver,       only : op_point_result_type
  character (*), intent(in) :: header
  type(op_point_specification_type), intent(in), optional :: op_spec
  type(op_point_result_type),        intent(in), optional :: op
  integer, intent(in) :: intent
  doubleprecision     :: dist, dev, value_base
  integer             :: how_good
  character (5)       :: base
  character (4)       :: opt_type
  character (30)      :: s

  value_base = 1d0

  call print_colored (COLOR_PALE, repeat(' ',intent))

  if (present(op_spec)) then 

  ! calculate distance and improvement   

    call get_op_improvement (op_spec, op, dist, dev, how_good)

  ! nice text for output

    if (trim(op_spec%optimization_type (1:6)) == 'target') then
      opt_type = 'targ'

      select case  (op_spec%optimization_type)
        case ('target-drag')
          value_base = 0.01d0
          base  = 'cd'
        case ('target-glide')
          value_base = 10d0
          base  = 'glide'
        case ('target-lift')
          value_base = 1d0
          base = 'cl'
        case ('target-moment')
          value_base = 0.1d0
          base = 'cm'
      end select

    else
      select case  (op_spec%optimization_type)
        case ('min-sink')
          opt_type = 'max'
          base = 'climb'                                       ! scale_factor = seed value
          value_base = 10d0
        case ('max-glide')
          opt_type = 'max'
          base = 'cl/cd'                                       ! scale_factor = seed value
          value_base = 10d0
        case ('min-drag')
          opt_type = 'min'
          base = 'cd'                                          ! scale_factor = seed value
          value_base = 0.01d0
        case ('max-lift')
          opt_type = 'max'
          base = 'cl'                                          ! scale_factor = seed value
          value_base = 1d0
        case ('max-xtr')
          opt_type = 'max'
          base = 'xtr'                                         ! scale_factor = seed value
          value_base = 1d0
        case default
          opt_type = 'n.a.'
          base  = ' '
          value_base = 1d0
      end select
    end if

  ! output all the stuff

    call print_colored (COLOR_PALE, opt_type//' ')
    call print_colored (COLOR_PALE, base//' ')

    if (trim (opt_type) /= 'n.a.') then 
      if (value_base == 10d0) then 
        call print_colored_r (7,'(SP,F7.2)', -1, dist) 
      elseif (value_base == 1d0) then 
        call print_colored_r (7,'(SP,F7.3)', -1, dist) 
      elseif (value_base == 0.1d0) then 
        call print_colored_r (7,'(SP,F7.4)', -1, dist) 
      elseif (value_base == 0.01d0) then 
        call print_colored_r (7,'(SP,F7.5)', -1, dist) 
      else 
        call print_colored_r (7,'(SP,F7.3)', -1, dist) 
      end if 

      call print_colored (COLOR_PALE, '  ')
        if (abs(dev) < 9.95d0) then 
          call print_colored_r (4,'(SP,F4.1)', how_good, dev) 
        else
          call print_colored_i (4, how_good, nint(dev)) 
        end if
        call print_colored_s (               how_good, '%') 
    else
      call print_colored (COLOR_PALE, repeat(' ',14))
    end if

  else 

  ! print header text 

    write (s,'(A)') header
    call print_colored (COLOR_PALE, s (1:25))
  end if 

end subroutine print_improvement_info 



subroutine get_op_improvement (op_spec, op, dist, dev, how_good)
  !! calculate improvement of a single opPoint
  !! returns distance in base type units and
  !!         deviation in % 

  use xfoil_driver,       only : op_point_result_type
  type(op_point_specification_type), intent(in) :: op_spec
  type(op_point_result_type),        intent(in) :: op
  doubleprecision, intent(out ) :: dist, dev
  integer, intent(out )         :: how_good

  doubleprecision       :: improv

  if (trim(op_spec%optimization_type (1:6)) == 'target') then

    select case  (op_spec%optimization_type)
      case ('target-drag')
        dist  = op_spec%target_value - op%cd                ! negative is worse
        dev   = dist / op_spec%target_value * 100d0
      case ('target-glide')
        dist  = op%cl /op%cd - op_spec%target_value         ! negative is worse
        dev   = dist / op_spec%target_value * 100d0
      case ('target-lift')  
        dist = op%cl - op_spec%target_value                  ! negative is worse
        dev  = dist / (1d0 + op_spec%target_value) * 100d0
      case ('target-moment')
        dist = op%cm - op_spec%target_value                 ! negative is worse
        dev  = dist / (op_spec%target_value + 0.1d0) * 100d0 ! cm could be 0
    end select

    if (op_spec%allow_improved_target .and. dev >= 0d0) then
      how_good = Q_GOOD
    else
      how_good = r_quality (abs(dev), 0.1d0, 2d0, 10d0)      ! in percent
    end if

  else
    select case  (op_spec%optimization_type)
      case ('min-sink')
        dist = op%cl**1.5d0 / op%cd - op_spec%scale_factor   
        dev  = dist / op_spec%scale_factor * 100d0
        improv = dev                                         ! positive is good
      case ('max-glide')
        dist = op%cl / op%cd - op_spec%scale_factor          ! positive is good
        dev  = dist / op_spec%scale_factor * 100d0
        improv = dev                                         ! positive is good
      case ('min-drag')
        dist = op%cd - 1d0 / op_spec%scale_factor                  
        dev  = -dist * op_spec%scale_factor * 100d0
        improv = dev                                         ! negative is good
      case ('max-lift')
        dist = op%cl - op_spec%scale_factor                  
        dev  = dist / op_spec%scale_factor * 100d0
        improv = dev                                         ! positive is good
      case ('max-xtr')
        dist = 0.5d0*(op%xtrt + op%xtrb) + 0.1d0 -  op_spec%scale_factor  
        dev  = dist / op_spec%scale_factor * 100d0
        improv = dev                                         ! positive is good
      case default
        dist  = 0d0           
        dev   = 0d0
        improv = 0d0                                          
    end select
    if (improv <= 0d0) then 
      how_good = Q_BAD
    elseif (improv < 5d0) then 
      how_good = Q_OK
    elseif (improv >= 5d0) then 
      how_good = Q_GOOD
    else
      how_good = Q_BAD
    end if 
  end if

end subroutine get_op_improvement 


!-- width = 26 -----------------------------------------------------------------
subroutine print_geo_improvement_info (intent, header, geo_spec, geo_result)

  use xfoil_driver,       only : op_point_result_type
  character (*), intent(in) :: header
  type(geo_target_type), intent(in), optional :: geo_spec
  type(geo_result_type),        intent(in), optional :: geo_result
  integer, intent(in) :: intent
  doubleprecision     :: dist, dev
  integer             :: how_good
  character (5)       :: base
  character (4)       :: opt_type
  character (30)      :: s

  call print_colored (COLOR_PALE, repeat(' ',intent))

  if (present(geo_spec)) then 
    select case  (trim(geo_spec%type))
      case ('Thickness')
        base  = 'y '
        dist  = geo_result%maxt - geo_spec%target_value
      case ('Camber')  
        base = 'y '
        dist = geo_result%maxc  - geo_spec%target_value   
      case default
        base = '  '
        dist = 0d0
        dev  = 0d0
    end select
    opt_type = 'targ'
  ! --
    call print_colored (COLOR_PALE, opt_type//' ')
  ! --
    call print_colored (COLOR_PALE, base)
    call print_colored (COLOR_PALE, ' ')
  ! --
    dev   = dist / geo_spec%target_value * 100d0
    how_good = r_quality (abs(dev), 0.07d0, 2d0, 10d0)   ! in percent
    call print_colored_r (7,'(SP,F7.5)', -1, dist) 
  ! --
    call print_colored (COLOR_PALE, '  ')
    if (how_good == Q_Good) then 
      call print_colored_s (how_good, '  hit') 
    else
      if (abs(dev) < 10.0d0) then 
        call print_colored_r (4,'(SP,F4.1)', how_good, dev) 
      else
        call print_colored_r (4,'(SP,F4.0)', how_good, dev) 
      end if
      call print_colored_s (               how_good, '%') 
    end if

  else 
    write (s,'(A)') header
    call print_colored (COLOR_PALE, s (1:28))
  end if 
end subroutine print_geo_improvement_info 


!-- width = 17 -----------------------------------------------------------------
subroutine print_geo_dynamic_weighting_info (intent, header, geo_spec)

  character (*), intent(in) :: header
  type(geo_target_type), intent(in), optional :: geo_spec
  integer, intent(in) :: intent
  doubleprecision     :: old_val, val
  character (30)      :: s

  call print_colored (COLOR_PALE, repeat(' ',intent))
  if (present(geo_spec)) then 
    val     = geo_spec%weighting_user_cur
    old_val = geo_spec%weighting_user_prv
    write (s,'(F3.1)') old_val
    call print_colored (COLOR_PALE, trim(s))
    if (geo_spec%dynamic_weighting) then
      call print_colored (COLOR_PALE, ' -> ')
      write (s,'(F3.1)') val
      if (abs(old_val - val) / old_val > 0.1d0 ) then 
        call print_colored (COLOR_FEATURE, trim(s))
      else
        call print_colored (COLOR_PALE, trim(s))
      end if
      if (geo_spec%extra_punch) then
        call print_colored (COLOR_FEATURE, '*') 
      else
        call print_colored (COLOR_HIGH, ' ') 
      end if
    else
      call print_colored (COLOR_PALE, '    '//'fix ') 
    end if
    call print_colored ( COLOR_PALE, '      ') 
  else
    write (s,'(A)') header
    call print_colored (COLOR_PALE, s (1:17))
  end if
 
end subroutine print_geo_dynamic_weighting_info

!-- width = 20 -----------------------------------------------------------------
subroutine print_bubble_info (intent, header, op)

  use xfoil_driver,       only : op_point_result_type

  character (*), intent(in) :: header
  type(op_point_result_type),        intent(in), optional :: op
  integer, intent(in) :: intent
  character (30)      :: s
  character (3)       :: from, to, xtr

  call print_colored (COLOR_PALE, repeat(' ',intent))

  !  >Ttr bubble   Btr bubble < 
  !  >.99 .88-.96  .72 .88-.95< 
  if (present(op)) then 

    if ((.not. op%bubblet%found) .and. (.not. op%bubbleb%found)) return
    
    ! call  print_colored (COLOR_WARNING,'Bubbles')
    if (op%bubblet%found) then 
      if (op%xtrt < 1d0) then 
        write (xtr ,'(F3.2)') min (op%xtrt, 0.99d0) 
      else
        xtr = ' - ' 
      end if 
      write (from,'(F3.2)') op%bubblet%xstart   
      if (op%bubblet%xend < 1d0) then 
        write (to  ,'(F3.2)') min (op%bubblet%xend, 0.99d0)   
      else
        write (to  ,'(F3.1)') op%bubblet%xend   
      end if  
      call  print_colored (COLOR_PALE,xtr // ' ' // from //'-'//to//'  ')
    else 
      call  print_colored (COLOR_PALE,repeat (' ', 13))
    end if 
    if (op%bubbleb%found) then 
      if (op%xtrb < 1d0) then 
        write (xtr ,'(F3.2)') min (op%xtrb, 0.99d0) 
      else
        xtr = ' - ' 
      end if 
      write (from,'(F3.2)') op%bubbleb%xstart    
      if (op%bubbleb%xend < 1d0) then 
        write (to  ,'(F3.2)') min (op%bubbleb%xend, 0.99d0)   
      else
        write (to  ,'(F3.1)') op%bubbleb%xend   
      end if  
      call  print_colored (COLOR_PALE,xtr // ' ' // from //'-'//to)
    else
      call  print_colored (COLOR_PALE,repeat (' ', 11))
    end if 
  else
    write (s,'(A)') header
    call print_colored (COLOR_PALE, s (1:24))
  end if
 
end subroutine print_bubble_info

!---------------------------------------------------------------------
function bubble_detected (op_points_result)

  use xfoil_driver,       only : op_point_result_type

  logical :: bubble_detected
  type(op_point_result_type), dimension(:),  intent(in) :: op_points_result
  integer :: i

  bubble_detected = .false.
  do i = 1, size(op_points_result)
    if (op_points_result(i)%bubblet%found .or. op_points_result(i)%bubbleb%found) then
      bubble_detected = .true.
      return
    end if
  end do

end function

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


  character(:), allocatable :: foil_file, title
  integer :: foil_unit


  ! Set output file names and identifiers

  foil_file = design_subdir//'Design_Coordinates.dat'
  foil_unit = 13

  if (designcounter == 0) then
    call print_colored (COLOR_NORMAL,' - Writing seed airfoil')
  else
    write (*,'(2x,A)', advance ='no') '-> Writing design '
    call  print_colored (COLOR_NORMAL,'#'//stri(designcounter))
  end if
  write (*,*)


! Design 0 is seed airfoil to output - take the original values 
!     Smoothing - Restore the original, not smoothed seed airfoil to
!                 ...design_coordinates.dat to show it in visualizer
  if (designcounter == 0) then
    foil = seed_foil
! Design > 0 - Build current foil out seed foil and current design 
  else 
    call create_airfoil_form_design (seed_foil, designvars, foil)
    foil%name = output_prefix
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

! Open file and write header, if necessary

  if (designcounter == 0) then

!   New File: Header for coordinate file & Seed foil 
    open(unit=foil_unit, file=foil_file, status='replace')
    write(foil_unit,'(A)') 'title="Airfoil coordinates"'
    write(foil_unit,'(A)') 'variables="x" "z"'
    title =  'zone t="Seed airfoil, '//'name='//trim(foil%name)//', maxt='//trim(maxtchar)//&
             ', xmaxt='//trim(xmaxtchar)//', maxc='//&
              trim(maxcchar)//', xmaxc='//trim(xmaxcchar)//'"'
    
  else

!   Append to file: Header for design foil coordinates
    open(unit=foil_unit, file=foil_file, status='old', position='append', err=910)
    title =  'zone t="Airfoil, '//'name='//trim(adjustl(foil%name))//', maxt='//trim(maxtchar)//&
             ', xmaxt='//trim(xmaxtchar)//', maxc='//&
              trim(maxcchar)//', xmaxc='//trim(xmaxcchar)//'", '//&
             'SOLUTIONTIME='//stri(designcounter)
  end if

  call  airfoil_write_to_unit (foil_unit, title, foil)

  close(foil_unit)


! Append the coordinates of the match foil when seed foil is written
  if (designcounter == 0) then
    call print_colored (COLOR_NORMAL,' - Writing airfoil to match')
    write (*,*)
    title = 'zone t="Match airfoil, '//'name='//trim(foil_to_match%name)//'"'
    open(unit=foil_unit, file=foil_file, status='old', position='append', err=910)
    call  airfoil_write_to_unit (foil_unit, title, foil_to_match)
    close(foil_unit)  
  end if 

  write_matchfoil_optimization_progress = 0

  return

! Warning if there was an error opening design_coordinates file

910 write(*,*) "Warning: unable to open "//trim(foil_file)//". Skipping ..."
  write_matchfoil_optimization_progress = 1
  return

end function write_matchfoil_optimization_progress



!------------------------------------------------------------------------------
!  For analysis...
!     Write variables of actual design to file for further analysis
!------------------------------------------------------------------------------
subroutine write_designvars (designcounter, designvars) 

  use vardef,             only: shape_functions
  use vardef,             only: initial_perturb, min_bump_width

  integer, intent(in) :: designcounter
  double precision, dimension(:), intent(in) :: designvars

  integer             :: i
  character(:), allocatable :: filename
  integer             :: iunit, nvars
  integer             :: nmodes, counter1
  double precision    :: st, t1, t2, t1fact, t2fact

  nvars = size(designvars,1)

! Initial design: Open file and write header

  filename = design_subdir//'particleswarm_designs.csv'
  iunit = 13
  if (designcounter == 0) then 
    open(unit=iunit, file=filename, status='replace')
!    write(iunit,'(A)') 'Number of variables: '//stri(nvars)
    write(iunit,'(A)') 'location  width  strength'
    close (iunit)
    return
  else
    open(unit=iunit, file=filename, status='old', position='append')
  end if

! Write iteration number and the design variables to file

  write(iunit,'(I4,A)', advance ='no') designcounter, ';'

  if (trim(shape_functions) == 'hicks-henne') then

    nmodes = size(designvars,1)/3
    t1fact = initial_perturb/(1.d0 - 0.001d0)
    t2fact = initial_perturb/(10.d0 - min_bump_width)

    do i = 1, nmodes

!     Extract strength, bump location, and width

      counter1 = 3*(i-1)
      st = designvars(counter1+1)
      t1 = designvars(counter1+2)/t1fact       ! location
      t2 = designvars(counter1+3)/t2fact       ! width

!     Check for problems with bump location and width parameters

      if (t1 <= 0.d0) t1 = 0.001d0      
      if (t1 >= 1.d0) t1 = 0.999d0
      if (t2 <= 0.d0) t2 = 0.001d0

!        power1 = log10(0.5d0)/log10(t1)
!        do j = 2, npt-1
!          xs = (x(j)-xle)/chord
!          shape_function(i,j) = st*sin(pi*xs**power1)**t2  
!        end do

!      write(iunit,'(3(F10.6,A))', advance ='no') 0.5d0** (log10(t1)/log10(0.5d0)), ";", (10d0-t2)/10d0, ";", st
      write(iunit,'(3(F10.6,A))', advance ='no') t1, ";", (10d0-t2)/10d0, ";", st
      if (i < nmodes) then 
        write(iunit,'(A)', advance ='no') ";  "
      else
        write(iunit, *) 
      end if 

!      write(iunit,'(3(F10.7,A))', advance ='no') designvars(counter1+2), ";", designvars(counter1+3), ";", designvars(counter1+1)
!      if (i < nmodes) then 
!        write(iunit,'(A)', advance ='no') ";  "
!      else
!        write(iunit, *) 
!      end if 

    end do 

  else
    do i = 1, nvars
      write(iunit,'(es13.5)', advance ='no') designvars(i)
      if (i < nvars) then 
        write(iunit,'(A)', advance ='no') ";"
      else
        write(iunit, *) 
      end if 
    end do
  end if 

! Close the file 

  close(iunit)

end subroutine write_designvars



!------------------------------------------------------------------------------
!  For analysis...
!     Write variables of actual design to file for further analysis
!------------------------------------------------------------------------------
subroutine print_designvars (designcounter, designvars) 

  use vardef,             only: shape_functions
  use vardef,             only: initial_perturb, min_bump_width

  integer, intent(in) :: designcounter
  double precision, dimension(:), intent(in) :: designvars

  integer             :: i
  integer             :: nvars
  integer             :: nmodes, counter1
  double precision    :: st, t1, t2, t1fact, t2fact

  i = designcounter               ! dummy

  nvars = size(designvars,1)

! Initial design: Open file and write header

  write(*,'(10x,A)') 'Hicks-Henne (location  "width"  strength*100)'
  write(*,'(10x)', advance ='no') 

  if (trim(shape_functions) == 'hicks-henne') then

    nmodes = size(designvars,1)/3
    t1fact = initial_perturb/(1.d0 - 0.001d0)
    t2fact = initial_perturb/(10.d0 - min_bump_width)

    do i = 1, nmodes

!     Extract strength, bump location, and width

      counter1 = 3*(i-1)
      st = designvars(counter1+1)
      t1 = designvars(counter1+2)/t1fact       ! location
      t2 = designvars(counter1+3)/t2fact       ! width

!     Check for problems with bump location and width parameters

      if (t1 <= 0.d0) t1 = 0.001d0      
      if (t1 >= 1.d0) t1 = 0.999d0
      if (t2 <= 0.d0) t2 = 0.001d0

!        power1 = log10(0.5d0)/log10(t1)
!        do j = 2, npt-1
!          xs = (x(j)-xle)/chord
!          shape_function(i,j) = st*sin(pi*xs**power1)**t2  
!        end do

      write(*,'(F5.2,F5.2,F6.2)', advance ='no') t1, (10d0-t2)/10d0, st * 100d0
      if (i < nmodes) then 
        write(*,'(3x)', advance ='no')
      else
        write(*, *) 
      end if 

    end do 

  else
    do i = 1, nvars
      write(*,'(es10.4)', advance ='no') designvars(i)
      if (i < nvars) then 
        write(*,'(3x)', advance ='no') 
      else
        write(*, *) 
      end if 
    end do
  end if 

end subroutine print_designvars

end module airfoil_evaluation

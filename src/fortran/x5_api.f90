!------------------------------------------------------------------------------------------
!
!  Interface module of Xoptfoil-JX to Xflr5. Provides
!
!   - x5_init                           ... init xfoil, eval seed
!   - x5_init_xy                        ...   based on x,y coordinates
!   - x5_eval_objective_function        ... of an airfoil
!   - x5_eval_objective_function_xy     ...   based on x,y coordinates
!
!   This file is part of XOPTFOIL-JX.
!                       Copyright (C) 2017-2019 Daniel Prosser
!                       Copyright (C) 2021      Jochen Guenzel
!
!------------------------------------------------------------------------------------------

module x5_api

  ! maybe needed  use ISO_C_BINDING
  
    implicit none

    public :: x5_init                               ! init eval based on seed foil 
    public :: x5_init_xy                            ! ... with xy coordinates
    public :: x5_eval_objective_function            ! eval objective function for a foil 
    public :: x5_eval_objective_function_xy         ! ... with xy coordinates
    
    private

contains

!------------------------------------------------------------------------------------------
!  Init Xfoil, eval seed_airfoil
!------------------------------------------------------------------------------------------

subroutine x5_init_xy (input_file, np, x, y)

  use vardef,             only : airfoil_type
  doubleprecision, dimension (np), intent(in)  :: x, y
  integer, intent(in)           :: np
  character (255), intent(in)   :: input_file

  type(airfoil_type) :: foil

  foil%npoint = np
  foil%x      = x
  foil%z      = y

  call x5_init (input_file, foil)

end subroutine x5_init_xy

!------------------------------------------------------------------------------------------
!  Init Xfoil, eval seed_airfoil
!------------------------------------------------------------------------------------------

subroutine x5_init (input_file, seed_foil_in)

  use vardef,             only : airfoil_type, seed_foil
  use vardef,             only : flap_spec, flap_degrees
  use xfoil_driver,       only : xfoil_init, xfoil_defaults
  use airfoil_evaluation, only : xfoil_options, xfoil_geom_options
  use airfoil_evaluation, only : do_smoothing, auto_curvature, check_curvature, check_geometry
  use airfoil_evaluation, only : op_points_spec, noppoint
  use input_sanity,       only : check_seed
  use input_output,       only : read_xfoil_options_inputs, read_op_points_spec
  use input_output,       only : read_xfoil_paneling_inputs
  

  type(airfoil_type), intent(in)  :: seed_foil_in
  character (255), intent(in)     :: input_file
  logical                         :: show_details

  flap_degrees (:)    = 0.d0                      ! no flaps used, needed for check_seed
  flap_spec%use_flap  = .false. 

  show_details = .true.

  call read_op_points_spec        (input_file, 0, noppoint, op_points_spec) 
  call read_xfoil_options_inputs  (input_file, 0, show_details, xfoil_options)
  call read_xfoil_paneling_inputs (input_file, 0,               xfoil_geom_options)

  call xfoil_init()
  call xfoil_defaults(xfoil_options)

  do_smoothing          = .false.
  auto_curvature        = .false.
  check_curvature       = .false.
  check_geometry        = .false.

  seed_foil = seed_foil_in

  call check_seed()

end subroutine x5_init


!------------------------------------------------------------------------------------------
!  Evaluate objective function of a foil 
!------------------------------------------------------------------------------------------

function x5_eval_objective_function_xy  (np, x, y)

  use vardef,             only : airfoil_type
  doubleprecision, dimension (np), intent(in)  :: x, y
  integer, intent(in)          :: np
  doubleprecision              :: x5_eval_objective_function_xy
 
  type(airfoil_type) :: foil

  foil%npoint = np
  foil%x      = x
  foil%z      = y

  x5_eval_objective_function_xy = x5_eval_objective_function (foil)

end function x5_eval_objective_function_xy

!------------------------------------------------------------------------------------------
!  Evaluate objective function of a foil 
!------------------------------------------------------------------------------------------

function x5_eval_objective_function (foil)

  use vardef, only             : airfoil_type
  use airfoil_evaluation, only : aero_objective_function, op_points_spec

  doubleprecision                 :: x5_eval_objective_function
  type(airfoil_type), intent(in)  :: foil

  doubleprecision, dimension(:), allocatable :: actual_flap_degrees

  allocate (actual_flap_degrees(size(op_points_spec)))
  actual_flap_degrees = 0d0

  x5_eval_objective_function = aero_objective_function(foil, actual_flap_degrees)

end function x5_eval_objective_function




end module
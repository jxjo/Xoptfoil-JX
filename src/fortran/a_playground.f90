! different playground and tests

! --------- objective functions ---------------------

module my_objectives
  implicit none
  private
  
  public :: my_objective_function, match_y_objectiveFn
  public :: target_x, target_y 

  doubleprecision, allocatable :: target_x (:), target_y(:) 

contains

function match_y_objectiveFn (dv, dummy)

  use airfoil_shape_bezier, only : bezier_eval_y_on_x, dv_to_bezier

  double precision, intent(in) :: dv(:)
  logical, intent(in), optional :: dummy
  double precision :: match_y_objectiveFn 

  double precision, allocatable :: px(:), py(:), design_y(:)
  double precision :: te_gap 
  integer     :: i 

  allocate (design_y(size(target_x)))

  te_gap = 0.00015d0 
  call dv_to_bezier (dv, te_gap, px, py)

  do i = 1, size(target_x)
    design_y(i) = bezier_eval_y_on_x (px, py, target_x(i))
  end do 
  match_y_objectiveFn = norm2 (target_y - design_y)

end function 


function my_objective_function (dv, dummy)
  double precision, intent(in) :: dv(:)
  double precision :: my_objective_function 
  logical, intent(in), optional :: dummy
  my_objective_function = dv(1) ** 2 + dv(2) ** 2 
 end function 

end module 

! --------- test simplex  -----------------------------

module simplex_test

  implicit none
  private
  public :: test_simplex 
  
contains

  subroutine test_simplex () 

    use simplex_search, only : simplexsearch, ds_options_type 
    use my_objectives,  only : my_objective_function 

    use vardef,         only : design_subdir, DESIGN_SUBDIR_POSTFIX
    use os_util,        only : make_directory

    double precision      :: xmin(2), x0(2)
    double precision      :: fmin, f0_ref
    integer               :: steps, fevals
    type(ds_options_type) :: ds_options

    write (*,*) 
    write (*,*) "-- test_simplex"

    ! Create subdirectory for all the design files 

    design_subdir = 'a_playground' // DESIGN_SUBDIR_POSTFIX
    call make_directory (trim(design_subdir))
    design_subdir = trim(design_subdir) // '/'

    ds_options%tol   = 1d-5
    ds_options%maxit = 50
    ds_options%write_designs = .false.
    ds_options%relative_fmin_report = .true.

    x0 = [1,2]

    call simplexsearch(xmin, fmin, steps, fevals, my_objective_function, &
                  x0, .false. , f0_ref, ds_options)

    write (*,*) "   Finished: ", xmin, fmin, steps, fevals

  end subroutine 

end module



! --------- test adapt bezier to x,y   -----------------------------

module adapt_bezier_test

  implicit none
  private
  public :: test_adapt_bezier  
  
contains

  subroutine test_adapt_bezier () 

    use simplex_search, only : simplexsearch, ds_options_type 
    use my_objectives,  only : match_y_objectiveFn, target_x, target_y 

    use vardef,         only : design_subdir, DESIGN_SUBDIR_POSTFIX
    use os_util,        only : make_directory

    double precision      :: xmin(5), x0(5)
    double precision      :: fmin, f0_ref
    integer               :: steps, fevals, i
    type(ds_options_type) :: ds_options

    write (*,*) 
    write (*,*) "-- test_adapt_bezier"

    ! Create subdirectory for all the design files 

    design_subdir = 'a_playground' // DESIGN_SUBDIR_POSTFIX
    call make_directory (trim(design_subdir))
    design_subdir = trim(design_subdir) // '/'

    ds_options%tol   = 1d-12
    ds_options%maxit = 1000
    ds_options%write_designs = .false.
    ds_options%relative_fmin_report = .true.

    ! --- setup targets 
    ! airfoil JX-GT-15v2
    target_x = [0.02, 0.05, 0.12, 0.21, 0.31, 0.42, 0.53, 0.64, 0.75, 0.85, 0.95]
    target_y = [0.01845, 0.02962, 0.04344, 0.05161, 0.05456, 0.05320, 0.04839, 0.04062, 0.03016, 0.01889, 0.00656]

    ! --- setup design variables  
    x0 = [0.04047, 0.33333, 0.05462, 0.66667, 0.03831]

    call simplexsearch(xmin, fmin, steps, fevals, match_y_objectiveFn, &
                  x0, .false. , f0_ref, ds_options)

    write(*,"(' design Vars: ',i2,'   tolerance: ',e8.1)") size (x0), ds_options%tol 
    write(*,"('    finished: ',i4,' steps')") steps
    write(*,"('        xmin: ',100f9.5)") ( xmin(i), i=1,size(xmin) )
    write(*,"('        fmin: ',f9.6)") fmin

  end subroutine 

end module


! --------------------------------------

program a_playground

  use simplex_test
  use adapt_bezier_test
  use airfoil_shape_bezier, only : test_bezier

  write (*,*) '----- Playground -------'
  write (*,*) 

  !call test_simplex () 
  !call test_adapt_bezier () 
  call test_bezier () 

end program 
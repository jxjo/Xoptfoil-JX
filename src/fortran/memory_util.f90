!  This file is part of XOPTFOIL-JX
!  Copyright (C) 2017-2019 Daniel Prosser, (C) 2023 Jochen Guenzel 

module memory_util

! Module containing utilities for memory management

  implicit none

  contains

  
subroutine allocate_airfoil_data()
  !! allocate shape functions for Hicks Henne + init xfoil memory (!) 
  !  this is kind of strange with modes (=dvs) and shape functions and ...
  !  #todo clean up 

  use xfoil_driver,       only : xfoil_init
  use vardef,             only : nparams_top, nparams_bot, shape_functions,    &
                                 seed_foil
  use parametrization,    only : create_shape_functions

  double precision, dimension(:), allocatable :: modest, modesb

! Allocate shape function setup arrays
  if ((trim(shape_functions) == 'camb-thick') .or.                          &
      (trim(shape_functions) == 'camb-thick-plus')) then
    allocate(modest(nparams_top))
    modest(:) = 0.d0

  else if (trim(shape_functions) == 'bezier') then
    ! each function (=control point) has x,y as variable - except point 2 (LE tangent) 
    ! (for Bezier shape functions are not used ... we have to do it anyway )
    allocate(modest(nparams_top))
    allocate(modesb(nparams_bot))
    modest(:) = 0.d0
    modesb(:) = 0.d0

  else
    ! Hicks-Henne width, location, height 
    allocate(modest(nparams_top*3))
    allocate(modesb(nparams_bot*3))
    modest(:) = 0.d0
    modesb(:) = 0.d0
  end if

! Allocate private memory for airfoil optimization on each thread

!$omp parallel default(shared)

! For Hicks-Henne it will just allocate shape functions.

  call create_shape_functions(seed_foil%xt, seed_foil%xb, modest, modesb,                  &
                              shape_functions, first_time=.true.)


! Allocate memory for xfoil

  call xfoil_init()

!$omp end parallel


end subroutine allocate_airfoil_data



subroutine deallocate_airfoil_data()
  !! deallocate shape functions (HH) and xfoil internal data
  use parametrization,    only : deallocate_shape_functions
  use xfoil_driver,       only : xfoil_cleanup

!$omp parallel default(shared)

  call deallocate_shape_functions()
  call xfoil_cleanup()

!$omp end parallel

end subroutine deallocate_airfoil_data




subroutine allocate_optimal_design (symmetrical, optdesign)
  !! allocate and optimal designs which are the dvs of the best design achieved 

  use vardef,             only : nparams_top, nparams_bot, nflap_optimize
  use vardef,             only : shape_functions   

  logical, intent(in)                :: symmetrical
  double precision, allocatable, intent(out)  :: optdesign (:)

  integer       :: nshapedvtop, nshapedvbot

! Allocate optimal solution array - size will be important

  if (trim(shape_functions) == 'camb-thick') then
    !Use a fixed number of 6 designvariables for airfoil-generation.
    !These are camber, thickness, camber-location, thickness-location,
    !LE radius and blending-range
    nshapedvtop = 6
    nshapedvbot = 0
  else if (trim(shape_functions) == 'camb-thick-plus') then
    !Use a fixed number of 12 designvariables for airfoil-generation.
    !Top and Bottom are treated seperately
    nshapedvtop = 12
    nshapedvbot = 0
  else if (trim(shape_functions) == 'bezier') then
    ! Bezier - design variable equals x or y coordinate of (moveable) control points
    nshapedvtop = nparams_top 
    nshapedvbot = nparams_bot 
  else
    !Hicks-Henne - per param: position, width, height 
    nshapedvtop = nparams_top * 3
    nshapedvbot = nparams_bot * 3
  end if

  if (.not. symmetrical) then
    allocate(optdesign(nshapedvtop+nshapedvbot+nflap_optimize))
  else
    allocate(optdesign(nshapedvtop+nflap_optimize))
  end if



end subroutine 


subroutine allocate_constrained_dvs (symmetrical, constrained_dvs)
  !! allocate and init constrained_dvs which is the index list of dvs 
  !! with constraints (bounds) 

  use vardef,             only : nparams_top, nparams_bot, nflap_optimize
  use vardef,             only : shape_functions   

  logical, intent(in)                :: symmetrical
  integer, allocatable, intent(out)  :: constrained_dvs (:)

  integer       :: nbot_actual, counter, idx, i 

! The number of bottom shape functions actually used (0 for symmetrical)
  if (symmetrical) then
    nbot_actual = 0
  else
    nbot_actual = nparams_bot
  end if
  
! Set design variables with side constraints

  if ((trim(shape_functions) == 'camb-thick') .or. &
      (trim(shape_functions) == 'camb-thick-plus')) then

  ! For camb-thick, we will only constrain the flap deflection

    allocate(constrained_dvs(nflap_optimize))
    counter = 0
    do i = 1, nflap_optimize
      counter = counter + 1
      constrained_dvs(counter) = i
    end do
  
  else if (trim(shape_functions) == 'bezier') then

  ! For bezier, all design variables will be constraint to xmin, xmax

    allocate(constrained_dvs(nparams_top + nbot_actual + nflap_optimize))
    counter = 0
    do i = 1, nparams_top + nbot_actual
      counter = counter + 1
      idx = i                  
      constrained_dvs(counter) = idx
    end do

    do i = nparams_top + nbot_actual + 1, nparams_top + nbot_actual + nflap_optimize
      counter = counter + 1
      constrained_dvs(counter) = i
    end do

  else

  ! For Hicks-Henne, also constrain bump locations and width

    allocate(constrained_dvs(2*nparams_top + 2*nbot_actual + nflap_optimize))
    counter = 0
    do i = 1, nparams_top + nbot_actual
      counter = counter + 1
      idx = 3*(i-1) + 2      ! DV index of bump location, shape function i
      constrained_dvs(counter) = idx
      counter = counter + 1
      idx = 3*(i-1) + 3      ! Index of bump width, shape function i
      constrained_dvs(counter) = idx
    end do
    do i = 3*(nparams_top + nbot_actual) + 1,                             &
           3*(nparams_top + nbot_actual) + nflap_optimize
      counter = counter + 1
      constrained_dvs(counter) = i
    end do

  end if

end subroutine 

end module memory_util

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

! ------------------------------------------------


program main

!         Main program for airfoil optimization
!                 
!                 Modules Hirarchy 
!
!                  main   / worker
!  input_sanity    input_output   optimization_driver
!                  airfoil_preparation
! particle_swarm  genectic_algorithm   simplex_search
!                 airfoil_evaluation
!      airfoil_operations   polar_operations
!                    memory_util
!                  parametrization
!       airfoil_shape_bezier optimization_util 
!                    math_deps
!                   xfoil_driver
!             xfoil   os_util  vardef
!

  use os_util
  use vardef
  use input_output,         only : read_inputs, read_clo
  use particle_swarm,       only : pso_options_type
  use genetic_algorithm,    only : ga_options_type
  use simplex_search,       only : ds_options_type
  use airfoil_evaluation,   only : xfoil_geom_options, match_foils
  use airfoil_operations,   only : get_seed_airfoil
  use airfoil_operations,   only : repanel_and_normalize_airfoil
  use airfoil_operations,   only : split_foil_at_00, airfoil_write
  use memory_util,          only : allocate_airfoil_data, deallocate_airfoil_data
  use memory_util,          only : allocate_optimal_design, allocate_constrained_dvs                                 
  use input_sanity,         only : check_seed, check_inputs
  use airfoil_preparation,  only : preset_airfoil_to_targets
  use airfoil_preparation,  only : matchfoils_preprocessing, transform_to_bezier_based 
  use optimization_driver,  only : optimize, write_final_design
  use optimization_util,    only : reset_run_control, delete_run_control
  use airfoil_shape_bezier, only : bezier_spec_type, ncp_to_ndv, ndv_to_ncp
  use xfoil_driver,         only : xfoil_init, xfoil_cleanup


  implicit none

#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION ""
#endif

  type(airfoil_type) :: original_foil, final_foil
  type(pso_options_type) :: pso_options
  type(ga_options_type)  :: ga_options
  type(bezier_spec_type) :: bezier_spec
  double precision, allocatable :: optdesign (:)
  integer, allocatable          :: constrained_dvs (:)
  integer           :: steps, fevals
  double precision  :: f0, fmin
  logical           :: symmetrical
  character(80)     :: global_search, seed_airfoil_type, matchfoil_file
  character(80)     :: airfoil_file
  character(:), allocatable :: input_file


  !-------------------------------------------------------------------------------
  
  write(*,'(A)')
  call print_colored (COLOR_FEATURE,' Xoptfoil-JX')

  write(*,'(A)') '             The Airfoil Optimizer            v'//trim(PACKAGE_VERSION)
  write(*,'(A)') 

  ! Handle multithreading - be careful with screen output in multi-threaded code parts
  !   macro OPENMP is set in CMakeLists.txt as _OPENMP is not set by default 
#ifdef OPENMP 
  call set_number_of_threads()
#endif

  ! Set default names and read command line arguments
  
  input_file = 'inputs.inp'
  output_prefix = 'optfoil'
  call read_clo(input_file, output_prefix,'Xoptfoil-JX')

  ! Create subdirectory for all the design files, clean existing files 

  design_subdir = output_prefix // DESIGN_SUBDIR_POSTFIX
  call make_directory (design_subdir, .false.)
  design_subdir = design_subdir // '/'

  call delete_file (output_prefix//'.dat')              ! the final airfoil 
  call delete_file (output_prefix//'.bez')              ! ... could have been bezier 


! Read inputs from namelist file

  npan_fixed = 200                      ! for optimizing npan is fixed ...
  call read_inputs(input_file, global_search,      &
                   seed_airfoil_type, airfoil_file, nparams_top, nparams_bot,  &
                   pso_options, ga_options, matchfoil_file, symmetrical) 
  write (*,*) 
  call check_inputs(global_search, pso_options)
  
  
  ! Delete existing run_control file and rewrite it - most possible errors should be passed

  call reset_run_control()


  ! Load seed airfoil into memory, repanel, normalize, rotate if not bezier based 

  call get_seed_airfoil (seed_airfoil_type, airfoil_file, original_foil)

  if (original_foil%bezier_based) then 
    ! Bezier based already in perfect quality - no action needed - leave it at is!
    seed_foil = original_foil  
    call airfoil_write (trim(seed_foil%name)//'.dat', trim(seed_foil%name), seed_foil)             
  else 
    ! repanel and normalize 
    call repanel_and_normalize_airfoil (original_foil, xfoil_geom_options, symmetrical, seed_foil) 
  end if

  ! ... to have run_xfoil results equal airfoil external results

  xfoil_geom_options%npan = seed_foil%npoint    ! will use this constant value now


  ! Prepare Airfoil based on optimization shape type  

  if (trim(shape_functions) == 'bezier' .and. seed_foil%bezier_based) then 

      ! ignore 'bezier_options' - take seed bezier definition  
      nparams_top = ncp_to_ndv(seed_foil%bezier_spec%ncpoints_top) 
      nparams_bot = ncp_to_ndv(seed_foil%bezier_spec%ncpoints_bot) 
      write(*,*)  
      call print_note ("Using number of Bezier control points from seed airfoil. "// &
                       "Values in 'bezier_options' will be ignored.")
      call print_text ("Also no preprocessing of seed airfoil will be done.", 7)

  else

    if (trim(shape_functions) == 'bezier') then 
    ! a new bezier "match foil" is generated to be new seed 
      bezier_spec%ncpoints_top = ndv_to_ncp (nparams_top)
      bezier_spec%ncpoints_bot = ndv_to_ncp (nparams_bot)
      seed_foil%name = trim(seed_foil%name) // '_bezier'
      call transform_to_bezier_based (bezier_spec, seed_foil%npoint, seed_foil)
    end if 
  end if  


  ! Set up for matching airfoils 
  if (match_foils) then
    call matchfoils_preprocessing  (matchfoil_file)
  end if


  ! Allocate optimal solutions, constraints, airfoils, xfoil for optimization 
  
  call allocate_constrained_dvs (symmetrical, constrained_dvs)
  call allocate_optimal_design  (symmetrical, optdesign)
  call allocate_airfoil_data()


  ! Make sure seed airfoil passes constraints - final checks, prepare objective function 
  !  - get scaling factors for operating points with xfoil, 

  call check_seed()


  ! Optimize
  
  call optimize (global_search, constrained_dvs, pso_options, ga_options,      &
                 optdesign, f0, fmin, steps, fevals)

  ! Notify of total number of steps and function evals

  write(*,*)
  write(*,*) 'Optimization complete. Totals: '
  write(*,'(/,A, I5, A, I7)') '  Steps:', steps, '   Objective function evaluations:', fevals

  ! Write final design and summary

  call write_final_design(optdesign, f0, fmin, final_foil)

  write(*,*)

  
  ! remove run_control file after optimization 

  call delete_run_control()


end program main


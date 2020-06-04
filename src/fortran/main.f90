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

program main

! Main program for airfoil optimization

  use omp_lib             ! to switch off multi threading
  use vardef
  use input_output,        only : read_inputs, read_clo
  use naca,                only : naca_options_type
  use particle_swarm,      only : pso_options_type
  use genetic_algorithm,   only : ga_options_type
  use simplex_search,      only : ds_options_type
  use airfoil_evaluation,  only : xfoil_geom_options, xfoil_options
  use airfoil_operations,  only : get_seed_airfoil, split_airfoil
  use memory_util,         only : deallocate_airfoil, allocate_airfoil_data,   &
                                  deallocate_airfoil_data
  use input_sanity,        only : check_seed
  use optimization_driver, only : matchfoils_preprocessing, optimize,          &
                                  write_final_design
  use polar_operations,    only : check_and_do_polar_generation
  use os_util,             only : print_note, print_warning


  implicit none

#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION ""
#endif

  type(airfoil_type) :: buffer_foil
  character(80) :: search_type, global_search, local_search, seed_airfoil,     &
                   airfoil_file, matchfoil_file
  character(80) :: input_file, text
  type(naca_options_type) :: naca_options
  type(pso_options_type) :: pso_options
  type(ga_options_type) :: ga_options
  type(ds_options_type) :: ds_options
  integer :: steps, fevals, nshapedvtop, nshapedvbot,        &
             restart_write_freq 
  double precision, dimension(:), allocatable :: optdesign
  integer, dimension(:), allocatable :: constrained_dvs
  double precision :: f0, fmin
  logical :: restart

!-------------------------------------------------------------------------------
  
  write(*,'(A)')
  write(*,'(A)') 'Xoptfoil-JX  The Airfoil Optimizer                  Version '//trim(PACKAGE_VERSION)
  write(*,'(A)') 
  write(*,'(A)') '         (c) 2017-2019 Daniel Prosser (original Xoptfoil)'
  write(*,'(A)') '         (c) 2019-2020 Jochen Guenzel, Matthias Boese'
  write(*,'(A)') 

! Set default names and read command line arguments
  
  input_file = 'inputs.txt'
  output_prefix = 'optfoil'
  call read_clo(input_file, output_prefix,'Xoptfoil-JX')

! Read inputs from namelist file

  call read_inputs(input_file, search_type, global_search, local_search,       &
                   seed_airfoil, airfoil_file, nparams_top, nparams_bot,       &
                   restart, restart_write_freq, constrained_dvs, naca_options, &
                   pso_options, ga_options, ds_options, matchfoil_file,        &
                   xfoil_geom_options, xfoil_options)

! Load seed airfoil into memory, including transformations and smoothing

  call get_seed_airfoil(seed_airfoil, airfoil_file, naca_options, buffer_foil)

! Split up seed airfoil into upper and lower surfaces

  call split_airfoil(buffer_foil, xseedt, xseedb, zseedt, zseedb, symmetrical)

! Deallocate the buffer airfoil (no longer needed)

  call deallocate_airfoil(buffer_foil)

! Allocate optimal solution

  if (trim(shape_functions) == 'naca') then
    nshapedvtop = nparams_top
    nshapedvbot = nparams_bot
  else if (trim(shape_functions) == 'camb-thick') then
    !Use a fixed number of 6 designvariables for airfoil-generation.
    !These are camber, thickness, camber-location, thickness-location,
    !LE radius and blending-range
    nshapedvtop = 6
    nshapedvbot = 0
  else
    nshapedvtop = nparams_top*3
    nshapedvbot = nparams_bot*3
  end if
  if (.not. symmetrical) then
    allocate(optdesign(nshapedvtop+nshapedvbot+nflap_optimize))
  else
    allocate(optdesign(nshapedvtop+nflap_optimize))
  end if

! Allocate memory for airfoil analysis

  call allocate_airfoil_data()

! Set up for matching airfoils

  if (match_foils) then
    call matchfoils_preprocessing(matchfoil_file)
  else
    write(*,*) "Optimizing for requested operating points."
    write(*,*)
  end if

! Switch off multi threading when using show_details because the output to the console 
!   would be corrupted ...
!   macro OPENMP is set in CMakeLists.txt as _OPENMP is not set by default (..?) 
#ifdef OPENMP                       
  if (omp_get_max_threads() > 1) then 
    if (show_details ) then 
      call print_warning ("Because of option 'show_details' CPU multi threading will be switched off")
      write(*,*)
      call omp_set_num_threads( 1 )
    else
      write (text,'(I2,A)') omp_get_max_threads(),' CPU threads will be used during optimization' 
      call print_note (text)
      write(*,*)
    end if 
  end if 
#else
  text = 'dummy' 
#endif

! Smoothing ---- save original seed surface before smoothing
!                ... to show original data later in visualizer 

  allocate(zseedt_not_smoothed(size(zseedt)))
  allocate(zseedb_not_smoothed(size(zseedb)))
  zseedt_not_smoothed = zseedt
  zseedb_not_smoothed = zseedb

  
! Make sure seed airfoil passes constraints, and get scaling factors for
! operating points

  call check_seed()

! Optimize
  
  call optimize(search_type, global_search, local_search, constrained_dvs,     &
                pso_options, ga_options, ds_options, restart,                  &
                restart_write_freq, optdesign, f0, fmin, steps, fevals)

! Notify of total number of steps and function evals

  write(*,*)
  write(*,*) 'Optimization complete. Totals: '
  write(*,'(/,A, I5, A, I7)') '  Steps:', steps, '   Objective function evaluations:', fevals

! Write final design and summary

  call write_final_design(optdesign, f0, fmin, shape_functions)

! Generate polars for final airfoil if defind in input file - 
!   - curr_foil was set in write_final_design 

  call check_and_do_polar_generation (input_file, output_prefix, curr_foil)

! Deallocate memory
  call deallocate_airfoil_data()
  deallocate(xseedt)
  deallocate(xseedb)
  deallocate(zseedt)
  deallocate(zseedb)
  deallocate(optdesign)
  if (allocated(constrained_dvs)) deallocate(constrained_dvs)


! jx-mod Smoothing ---- Deallocate save arrays
  deallocate(zseedt_not_smoothed)
  deallocate(zseedb_not_smoothed)

end program main

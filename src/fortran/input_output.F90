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

module input_output

! Module with subroutines for reading and writing of files

  use os_util

  implicit none

  contains

!=============================================================================80
!
! Subroutine to read inputs from namelist file
!
!=============================================================================80
subroutine read_inputs(input_file, search_type, global_search, local_search,   &
                       seed_airfoil, airfoil_file, nfunctions_top,             &
                       nfunctions_bot, restart, restart_write_freq,            &
                       constrained_dvs, naca_options, pso_options, ga_options, &
                       ds_options, matchfoil_file, symmetrical)

  use vardef
  use airfoil_evaluation
  use particle_swarm,     only : pso_options_type
  use genetic_algorithm,  only : ga_options_type
  use simplex_search,     only : ds_options_type
  use airfoil_operations, only : my_stop
  use xfoil_driver,       only : xfoil_geom_options_type
  use naca,               only : naca_options_type
  use math_deps,          only : sort_vector
  use os_util,            only : print_note


  character(*), intent(in) :: input_file 
  character(80), intent(out) :: search_type, global_search, local_search,      &
                                seed_airfoil, airfoil_file, matchfoil_file
  integer, intent(out) :: nfunctions_top, nfunctions_bot
  logical, intent(out) :: symmetrical
  integer, dimension(:), allocatable, intent(inout) :: constrained_dvs
  type(naca_options_type), intent(out) :: naca_options
  type(pso_options_type), intent(out) :: pso_options
  type(ga_options_type), intent(out) :: ga_options
  type(ds_options_type), intent(out) :: ds_options

! Op_point specification 
!  character(7),     dimension(max_op_points)  :: op_mode
!  character(15),    dimension(max_op_points)  :: optimization_type
!  double precision, dimension(max_op_points)  :: op_point, weighting, scale_factor, &
!                                                 ncrit_pt, target_value, reynolds, mach
!  double precision :: re_default
!  logical          :: re_default_as_resqrtcl
!  type(op_point_specification_type) :: op_spec


  integer, dimension(max_addthickconst) :: sort_idxs
  double precision, dimension(max_addthickconst) :: temp_thickmin, temp_thickmax
  logical :: feasible_init,        &
             restart, write_designs, reflexed,                   &
             pso_write_particlefile, repanel
  integer :: restart_write_freq, pso_pop, pso_maxit, simplex_maxit,  &
             npan, feasible_init_attempts
  integer :: ga_pop, ga_maxit
  double precision :: maxt, xmaxt, maxc, xmaxc, design_cl, a, leidx
  double precision :: pso_tol, simplex_tol
  double precision :: cvpar, cterat, ctrrat, xsref1, xsref2, xpref1, xpref2
  double precision :: feasible_limit
  double precision :: ga_tol, parent_fraction, roulette_selection_pressure,    &
                      tournament_fraction, crossover_range_factor,             &
                      mutant_probability, chromosome_mutation_rate,            &
                      mutation_range_factor
  integer :: nbot_actual
  integer :: i, iunit, ioerr, iostat1, counter, idx
  character(30) :: text
  character(3) :: family
  character(20) :: pso_convergence_profile
  character(10) :: parents_selection_method

  ! Geo targets 
  double precision :: sum_weightings
  double precision, dimension(max_geo_targets) :: x_pos, target_geo
  double precision, dimension(max_geo_targets) :: weighting_geo
  character(30), dimension(max_geo_targets) :: target_type

  ! jx-mod show/suppress extensive echo of input parms
  logical :: echo_input_parms
  
  namelist /optimization_options/ search_type, global_search, local_search,    &
            seed_airfoil, airfoil_file, shape_functions, nfunctions_top,       &
            nfunctions_bot, initial_perturb, min_bump_width, restart,          &
            restart_write_freq, write_designs,                                 &
            show_details, echo_input_parms

  namelist /constraints/ min_thickness, max_thickness, moment_constraint_type, &
                         min_moment, min_te_angle,                             &
                         check_curvature, auto_curvature,                      &
                         max_curv_reverse_top, max_curv_reverse_bot,           &
                         max_curv_highlow_top, max_curv_highlow_bot,           &
                         curv_threshold, symmetrical, min_flap_degrees,        &
                         max_flap_degrees, min_camber, max_camber,             &
                         naddthickconst, addthick_x, addthick_min, addthick_max, &
                         max_te_curvature, highlow_threshold
  namelist /naca_airfoil/ family, maxt, xmaxt, maxc, xmaxc, design_cl, a,      &
                          leidx, reflexed
  namelist /initialization/ feasible_init, feasible_limit,                     &
                            feasible_init_attempts
  namelist /particle_swarm_options/ pso_pop, pso_tol, pso_maxit,               &
                                    pso_convergence_profile,                   &
                                    pso_write_particlefile,                    &
                                    pso_options       ! allow direct manipulation
  namelist /genetic_algorithm_options/ ga_pop, ga_tol, ga_maxit,               &
            parents_selection_method, parent_fraction,                         &
            roulette_selection_pressure, tournament_fraction,                  &
            crossover_range_factor, mutant_probability,                        &
            chromosome_mutation_rate, mutation_range_factor
  namelist /simplex_options/ simplex_tol, simplex_maxit
  namelist /xfoil_paneling_options/ npan, cvpar, cterat, ctrrat, xsref1,       &
            xsref2, xpref1, xpref2, repanel
  namelist /matchfoil_options/ match_foils, matchfoil_file
  namelist /smoothing_options/ do_smoothing, spike_threshold 
  namelist /geometry_targets/ ngeo_targets, target_type, x_pos, target_geo,  &
            weighting_geo 


! Open input file

  iunit = 12
  open(unit=iunit, file=input_file, status='old', iostat=ioerr)
  if (ioerr /= 0)                                                              &
    call my_stop('Could not find input file '//trim(input_file)//'.')

! Set defaults for main namelist options

  search_type = 'global_and_local'
  global_search = 'particle_swarm'
  local_search = 'simplex'
  seed_airfoil = 'naca'
  airfoil_file = ''
  shape_functions = 'hicks-henne'
  min_bump_width = 0.1d0
  nfunctions_top = 4
  nfunctions_bot = 4
  initial_perturb = 0.025d0
  restart = .false.
  restart_write_freq = 0              ! default: switch off write restart files
  write_designs = .true.

! Show more infos  / supress echo
  show_details = .false. 
  echo_input_parms = .false.
 
! Read main namelist options

  rewind(iunit)
  read(iunit, iostat=iostat1, nml=optimization_options)
  call namelist_check('optimization_options', iostat1, 'warn')

! Error checking and setting search algorithm options

  if (trim(search_type) /= 'global_and_local' .and. trim(search_type) /=       &
      'global' .and. trim(search_type) /= 'local')                             &
    call my_stop("search_type must be 'global_and_local', 'global', "//   &
                 "or 'local'.")

! In case of 'camb-thick' set number of top functions to a fixed number of 6
  if (trim(shape_functions) == 'camb-thick' ) then
    nfunctions_top = 6
    nfunctions_bot = 0
! In case of 'camb-thick-plus' set number of top functions to a fixed number of 12    
  else if (trim(shape_functions) == 'camb-thick-plus' ) then
    nfunctions_top = 12
    nfunctions_bot = 0
  end if

! jx-mod get seed airfoil file from command file 

  airfoil_file = read_cl_airfoil_file (airfoil_file)


  ! mb-mod dynamic-weighting
  dynamic_weighting  = .false.
  dynamic_weighting_p_factor = 80.d0



! Read operating conditions

  call read_op_points_spec('', iunit, noppoint, op_points_spec)

  !if (echo_input_parms) call echo_op_points_spec  (op_points_spec)
  call echo_op_points_spec  (op_points_spec) 

! Read flap spec in operating conditions - flaps to optimize

  call read_flap_inputs   ('', iunit, flap_spec, flap_degrees, flap_selection)

  nflap_optimize = 0
  if (flap_spec%use_flap .and. (.not. match_foils)) then
    do i = 1, noppoint
      if (flap_selection(i) == 'optimize') then
        nflap_optimize = nflap_optimize + 1
        flap_optimize_points(nflap_optimize) = i
      end if
    end do
  end if


! Geo targets - start read and weight options---------------------

  ngeo_targets = 0
  target_type (:) = ''
  x_pos(:) = 0.d0 
  target_geo(:) = 0.d0 
  weighting_geo(:) = 0.0 

  rewind(iunit)
  read(iunit, iostat=iostat1, nml=geometry_targets)
  call namelist_check('geometry_targets', iostat1, 'warn')

  do i = 1, ngeo_targets
    geo_targets(i)%type         = target_type(i)
    geo_targets(i)%target_value = target_geo(i)
    geo_targets(i)%x            = x_pos(i)
    geo_targets(i)%reference_value   = 0.d0
    geo_targets(i)%weighting    = weighting_geo(i)
  end do   


  
! Modify normalize weightings for operating points
!          now includis geo targets and smoothing progress

  sum_weightings = sum(op_points_spec%weighting_user)               &
                 + sum(weighting_geo(1:ngeo_targets))

  op_points_spec%weighting = op_points_spec%weighting_user / sum_weightings
  geo_targets%weighting    = geo_targets%weighting         / sum_weightings


! Read curvature constraints

  call read_geo_constraints_inputs ('', iunit, &
                                  check_curvature, auto_curvature,  &
                                  max_te_curvature,    &
                                  max_curv_reverse_top, max_curv_reverse_bot, &
                                  max_curv_highlow_top, max_curv_highlow_bot, &
                                  curv_threshold,highlow_threshold)


! Read constraints

  min_thickness = 0.04d0
  max_thickness = 1000.d0
  min_camber = -0.1d0
  max_camber = 0.1d0
  moment_constraint_type(:) = 'none'
  min_moment(:) = -1.d0
  min_te_angle = 2.d0

  symmetrical = .false.
  min_flap_degrees = -5.d0
  max_flap_degrees = 15.d0
  naddthickconst = 0
  addthick_x(:) = 0.01d0
  addthick_min(:) = -1000.d0
  addthick_max(:) = 1000.d0

  rewind(iunit)
  read(iunit, iostat=iostat1, nml=constraints)
  call namelist_check('constraints', iostat1, 'warn')

  ! Sort thickness constraints in ascending x/c order
  if (naddthickconst > 0) then
    call sort_vector(addthick_x(1:naddthickconst), sort_idxs(1:naddthickconst))
    temp_thickmin = addthick_min
    temp_thickmax = addthick_max
    do i = 1, naddthickconst
      addthick_min(i) = temp_thickmin(sort_idxs(i))
      addthick_max(i) = temp_thickmax(sort_idxs(i))
    end do
  end if



! Smoothing - start read options-----------------------------------------
  
  spike_threshold = 0.8d0
  do_smoothing    = .false.         ! now default - smoothing will be forced if 
                                    !               quality of surface is bad
  rewind(iunit)
  read(iunit, iostat=iostat1, nml=smoothing_options)




! Set defaults for naca airfoil options
 
  family = '4'
  maxt = 0.1d0
  xmaxt = 0.3d0
  maxc = 0.d0
  xmaxc = 0.3d0
  design_cl = 0.3d0
  a = 1.d0
  leidx = 6.d0
  reflexed = .false.

! Read naca airfoil options and put them into derived type

  if ( (seed_airfoil == 'naca') .or. (seed_airfoil == 'NACA') .or.             &
       (seed_airfoil == 'Naca') ) then
    rewind(iunit)
    read(iunit, iostat=iostat1, nml=naca_airfoil)
    call namelist_check('naca_airfoil', iostat1, 'warn')

    naca_options%family = family
    naca_options%maxt = maxt
    naca_options%xmaxt = xmaxt
    naca_options%maxc = maxc
    naca_options%xmaxc = xmaxc
    naca_options%design_cl = design_cl 
    naca_options%a = a
    naca_options%leidx = leidx
    naca_options%reflexed = reflexed
  end if

! Set default initialization options

  feasible_init = .true. 
  feasible_limit = 5.0D+04
  feasible_init_attempts = 1000

! Read initialization parameters

  rewind(iunit)
  read(iunit, iostat=iostat1, nml=initialization)
  call namelist_check('initialization', iostat1, 'warn')

! Set default particle swarm options

  pso_pop = 40
  pso_tol = 1.D-04
  pso_maxit = 700
  pso_write_particlefile = .false.

  if ((trim(shape_functions) == 'camb-thick' ) .or. &
      (trim(shape_functions) == 'camb-thick-plus')) then
    pso_convergence_profile = 'quick_camb_thick'
  else
    pso_convergence_profile = 'exhaustive'
  end if

! Set default genetic algorithm options

  ga_pop = 80
  ga_tol = 1.D-04
  ga_maxit = 700
  parents_selection_method = 'tournament'
  parent_fraction = 0.5d0
  roulette_selection_pressure = 8.d0
  tournament_fraction = 0.025d0
  crossover_range_factor = 0.5d0
  mutant_probability = 0.4d0
  chromosome_mutation_rate = 0.01d0
  mutation_range_factor = 0.2d0

! Set default simplex search options

  simplex_tol = 1.0D-05
  simplex_maxit = 1000

  if (trim(search_type) == 'global_and_local' .or. trim(search_type) ==        &
      'global') then

!   The number of bottom shape functions actually used (0 for symmetrical)

    if (symmetrical) then
      nbot_actual = 0
    else
      nbot_actual = nfunctions_bot
    end if
  
!   Set design variables with side constraints

    if ((trim(shape_functions) == 'naca')  .or. &
        (trim(shape_functions) == 'camb-thick') .or. &
        (trim(shape_functions) == 'camb-thick-plus')) then

!     For NACA / camb-thick, we will only constrain the flap deflection

      allocate(constrained_dvs(nflap_optimize))
      counter = 0
      do i = nfunctions_top + nbot_actual + 1,                                 &
             nfunctions_top + nbot_actual + nflap_optimize
        counter = counter + 1
        constrained_dvs(counter) = i
      end do
          
    else

!     For Hicks-Henne, also constrain bump locations and width
 
      allocate(constrained_dvs(2*nfunctions_top + 2*nbot_actual +              &
                               nflap_optimize))
      counter = 0
      do i = 1, nfunctions_top + nbot_actual
        counter = counter + 1
        idx = 3*(i-1) + 2      ! DV index of bump location, shape function i
        constrained_dvs(counter) = idx
        counter = counter + 1
        idx = 3*(i-1) + 3      ! Index of bump width, shape function i
        constrained_dvs(counter) = idx
      end do
      do i = 3*(nfunctions_top + nbot_actual) + 1,                             &
             3*(nfunctions_top + nbot_actual) + nflap_optimize
        counter = counter + 1
        constrained_dvs(counter) = i
      end do

    end if

    if (trim(global_search) == 'particle_swarm') then

!     Read PSO options and put them into derived type

      rewind(iunit)
      read(iunit, iostat=iostat1, nml=particle_swarm_options)
      call namelist_check('particle_swarm_options', iostat1, 'warn')
      pso_options%pop = pso_pop
      pso_options%tol = pso_tol
      pso_options%maxspeed = initial_perturb
      pso_options%maxit = pso_maxit
      pso_options%write_particlefile= pso_write_particlefile
      pso_options%convergence_profile = pso_convergence_profile

      pso_options%feasible_init = feasible_init
      pso_options%feasible_limit = feasible_limit
      pso_options%feasible_init_attempts = feasible_init_attempts
      pso_options%write_designs = write_designs
      if (.not. match_foils) then
        pso_options%relative_fmin_report = .true.
      else
        pso_options%relative_fmin_report = .false.
      end if
 
    else if (trim(global_search) == 'genetic_algorithm') then

!     Read genetic algorithm options and put them into derived type

      rewind(iunit)
      read(iunit, iostat=iostat1, nml=genetic_algorithm_options)
      call namelist_check('genetic_algorithm_options', iostat1, 'warn')
      ga_options%pop = ga_pop
      ga_options%tol = ga_tol
      ga_options%maxit = ga_maxit
      ga_options%parents_selection_method = parents_selection_method
      ga_options%parent_fraction = parent_fraction
      ga_options%roulette_selection_pressure = roulette_selection_pressure
      ga_options%tournament_fraction = tournament_fraction
      ga_options%crossover_range_factor = crossover_range_factor
      ga_options%mutant_probability = mutant_probability
      ga_options%chromosome_mutation_rate = chromosome_mutation_rate
      ga_options%mutation_range_factor = mutation_range_factor
      ga_options%feasible_init = feasible_init
      ga_options%feasible_limit = feasible_limit
      ga_options%feasible_init_attempts = feasible_init_attempts
      ga_options%write_designs = write_designs
      if (.not. match_foils) then
        ga_options%relative_fmin_report = .true.
      else
        ga_options%relative_fmin_report = .false.
      end if

    else
      call my_stop("Global search type '"//trim(global_search)//               &
                   "' is not available.")
    end if
  end if

  if (trim(search_type) == 'global_and_local' .or. trim(search_type) ==        &
      'local') then

    if (trim(local_search) == 'simplex') then

!     Read simplex search options and put them into derived type

      rewind(iunit)
      read(iunit, iostat=iostat1, nml=simplex_options)
      call namelist_check('simplex_options', iostat1, 'warn')
      ds_options%tol = simplex_tol
      ds_options%maxit = simplex_maxit
      ds_options%write_designs = write_designs
      if (.not. match_foils) then
        ds_options%relative_fmin_report = .true.
      else
        ds_options%relative_fmin_report = .false.
      end if

    else
      call my_stop("Local search type '"//trim(local_search)//   &
                   "' is not available.")
    end if

  end if 

! Read and set default xfoil run options

  call  read_xfoil_options_inputs  ('', iunit, show_details, xfoil_options)


! Set default xfoil  paneling options

  call read_xfoil_paneling_inputs  ('', iunit, xfoil_geom_options)


! Option to match seed airfoil to another instead of aerodynamic optimization

  match_foils = .false.
  matchfoil_file = 'none'
  rewind(iunit)
  read(iunit, iostat=iostat1, nml=matchfoil_options)

! Close the input file

  close(iunit)

! Echo namelist options for checking purposes

! jx-mod  (I did it ... ;-) )
  if (.not. echo_input_parms) goto 10000

  write(*,*)
  write(*,*) 'Echoing program options:'
  write(*,*)

! Optimization options namelist

  write(*,'(A)') " &optimization_options"
  write(*,*) " search_type = '"//trim(search_type)//"'"
  write(*,*) " global_search = '"//trim(global_search)//"'"
  write(*,*) " local_search = '"//trim(local_search)//"'"
  write(*,*) " seed_airfoil = '"//trim(seed_airfoil)//"'"
  write(*,*) " airfoil_file = '"//trim(airfoil_file)//"'"
  write(*,*) " shape_functions = '"//trim(shape_functions)//"'"
  write(*,*) " min_bump_width = ", min_bump_width
  write(*,*) " nfunctions_top = ", nfunctions_top
  write(*,*) " nfunctions_bot = ", nfunctions_bot
  write(*,*) " initial_perturb = ", initial_perturb
  write(*,*) " restart = ", restart
  write(*,*) " restart_write_freq = ", restart_write_freq
  write(*,*) " write_designs = ", write_designs
! jx-mod Show more infos during optimization
  write(*,*) " show_details = ", show_details
! jx-mod     
  write(*,*) " echo_input_parms = ", echo_input_parms 

  write(*,'(A)') " /"
  write(*,*)

! Operating conditions namelist
! #todo
!  write(*,'(A)') " &operating_conditions"
!  write(*,*) " noppoint = ", noppoint
!  write(*,*) " use_flap = ", use_flap
!  write(*,*) " x_flap = ", x_flap
!  write(*,*) " y_flap = ", y_flap
!  write(*,*) " y_flap_spec = "//trim(y_flap_spec)
!  write(*,*) " re_default = ", re_default
!  write(*,*) " re_default_as_resqrtcl = ", re_default_as_resqrtcl
! mb-mod dynamic-weighting
!  write(*,*) " dynamic_weighting = ", dynamic_weighting
!  write(*,*) " dynamic_weighting_p_factor = ", dynamic_weighting_p_factor
!  write(*,*)

  write(*,*)

! Constraints namelist

  write(*,'(A)') " &constraints"
  write(*,*) " min_thickness = ", min_thickness
  write(*,*) " max_thickness = ", max_thickness
  write(*,*) " min_te_angle = ", min_te_angle
  write(*,*) " max_te_curvature = ", max_te_curvature
  write(*,*) " check_curvature = ", check_curvature
  write(*,*) " auto_curvature = ", auto_curvature
  write(*,*) " max_curv_reverse_top = ", max_curv_reverse_top
  write(*,*) " max_curv_reverse_bot = ", max_curv_reverse_bot
  write(*,*) " max_curv_highlow_top = ", max_curv_highlow_top
  write(*,*) " max_curv_highlow_bot = ", max_curv_highlow_bot
  write(*,*) " highlow_threshold = ", highlow_threshold
  write(*,*) " curv_threshold = ", curv_threshold
  write(*,*) " symmetrical = ", symmetrical
  write(*,*) " min_flap_degrees = ", min_flap_degrees
  write(*,*) " max_flap_degrees = ", max_flap_degrees
  write(*,*) " min_camber = ", min_camber
  write(*,*) " max_camber = ", max_camber
  write(*,*) " naddthickconst = ", naddthickconst
  do i = 1, naddthickconst
    write(text,*) i
    text = adjustl(text)
    write(*,*) " addthick_x("//trim(text)//") = ", addthick_x(i)
    write(*,*) " addthick_min("//trim(text)//") = ", addthick_min(i)
    write(*,*) " addthick_max("//trim(text)//") = ", addthick_max(i)
  end do
  write(*,'(A)') " /"
  write(*,*)

! jx-mod Smoothing - echo namelist

  write(*,'(A)') " &smoothing_options"
  write(*,*) " do_smoothing = ", do_smoothing
  write(*,*) " spike_threshold = ", spike_threshold
  write(*,'(A)') " /"
  write(*,*)

! jx-mod Geo targets - echo namelist

  write(*,'(A)') " &geometry_targets"

  do i = 1, ngeo_targets
    write(text,*) i
    text = adjustl(text)
    write(*,*) " target_type("//trim(text)//") = ",   geo_targets(i)%type
    write(*,*) " target_value("//trim(text)//") = ",  geo_targets(i)%target_value
    write(*,*) " x_pos("//trim(text)//") = ",         geo_targets(i)%x
    write(*,*) " weighting_geo("//trim(text)//") = ", geo_targets(i)%weighting
    if (i < noppoint) write(*,*)
  end do   
  write(*,'(A)') " /"
  write(*,*)

! NACA namelist

  write(*,'(A)') " &naca"
  write(*,*) " family = "//trim(adjustl(family))
  write(*,*) " maxt = ", maxt
  write(*,*) " xmaxt = ", xmaxt
  write(*,*) " maxc = ", maxc
  write(*,*) " xmaxc = ", xmaxc
  write(*,*) " design_cl = ", design_cl
  write(*,*) " a = ", a
  write(*,*) " leidx = ", leidx
  write(*,*) " reflexed = ", reflexed
  write(*,'(A)') " /"
  write(*,*)

! Initialization namelist

  write(*,'(A)') " &initialization"
  write(*,*) " feasible_init = ", feasible_init
  write(*,*) " feasible_limit = ", feasible_limit
  write(*,*) " feasible_init_attempts = ", feasible_init_attempts
  write(*,'(A)') " /"
  write(*,*)

! Optimizer namelists

  if (trim(search_type) == 'global_and_local' .or. trim(search_type) ==        &
      'global') then

    if (trim(global_search) == 'particle_swarm') then

!     Particle swarm namelist

      write(*,'(A)') " &particle_swarm_options"
      write(*,*) " pso_pop = ", pso_options%pop
      write(*,*) " pso_tol = ", pso_options%tol
      write(*,*) " pso_maxit = ", pso_options%maxit
      write(*,*) " pso_convergence_profile = ", pso_options%convergence_profile
      write(*,'(A)') " /"
      write(*,*)

    else if (trim(global_search) == 'genetic_algorithm') then

!     Genetic algorithm options

      write(*,'(A)') " &genetic_algorithm_options"
      write(*,*) " ga_pop = ", ga_options%pop
      write(*,*) " ga_tol = ", ga_options%tol
      write(*,*) " ga_maxit = ", ga_options%maxit
      write(*,*) " parents_selection_method = ",                               &
                 ga_options%parents_selection_method
      write(*,*) " parent_fraction = ", ga_options%parent_fraction 
      write(*,*) " roulette_selection_pressure = ",                            &
                 ga_options%roulette_selection_pressure
      write(*,*) " tournament_fraction = " , ga_options%tournament_fraction
      write(*,*) " crossover_range_factor = ", ga_options%crossover_range_factor
      write(*,*) " mutant_probability = ", ga_options%mutant_probability
      write(*,*) " chromosome_mutation_rate = ",                               &
                 ga_options%chromosome_mutation_rate
      write(*,*) " mutation_range_factor = ", ga_options%mutation_range_factor
      write(*,'(A)') " /"
      write(*,*)

    end if

  end if

  if (trim(search_type) == 'global_and_local' .or. trim(search_type) ==        &
      'local') then

    if(trim(local_search) == 'simplex') then

!     Simplex search namelist

      write(*,'(A)') " &simplex_options"
      write(*,*) " simplex_tol = ", ds_options%tol
      write(*,*) " simplex_maxit = ", ds_options%maxit
      write(*,'(A)') " /"
      write(*,*)

    end if

  end if


! Xfoil paneling options namelist

  write(*,'(A)') " &xfoil_paneling_options"
  write(*,*) " npan = ", xfoil_geom_options%npan
  write(*,*) " cvpar = ", xfoil_geom_options%cvpar
  write(*,*) " cterat = ", xfoil_geom_options%cterat
  write(*,*) " ctrrat = ", xfoil_geom_options%ctrrat
  write(*,*) " xsref1 = ", xfoil_geom_options%xsref1
  write(*,*) " xsref2 = ", xfoil_geom_options%xsref2
  write(*,*) " xpref1 = ", xfoil_geom_options%xpref1
  write(*,*) " xpref2 = ", xfoil_geom_options%xpref2
  write(*,'(A)') " /"
  write(*,*)

! Matchfoil options

  write(*,'(A)') " &matchfoil_options"
  write(*,*) " match_foils = ", match_foils
  write(*,*) " matchfoil_file = '"//trim(matchfoil_file)//"'"
  write(*,'(A)') " /"
  write(*,*)


! jx-mod  (I did it ... ;-)
  10000 continue


! Check that inputs are reasonable

! Optimization settings

  if (trim(seed_airfoil) /= 'from_file' .and.                                  &
      trim(seed_airfoil) /= 'naca')                                            &
    call my_stop("seed_airfoil must be 'from_file' or 'naca'.")
  if (trim(shape_functions) /= 'hicks-henne' .and.                             &
      trim(shape_functions) /= 'hicks-henne-plus' .and.                        &  !#exp-HH-plus 
      trim(shape_functions) /= 'camb-thick' .and.                              &
      trim(shape_functions) /= 'camb-thick-plus' .and.                         &
      trim(shape_functions) /= 'naca')                                         &
    call my_stop("shape_functions must be 'hicks-henne', hicks-henne-plus', 'camb-thick', 'camb-thick-plus' or 'naca'.") !#exp-HH-plus
  if ((nfunctions_top < 0) .and.   & 
      trim(shape_functions) /= 'camb-thick' .and.                              &
      trim(shape_functions) /= 'camb-thick-plus')                              &
    call my_stop("nfunctions_top must be >= 0.")
  if ((nfunctions_bot < 0) .and. &
       trim(shape_functions) /= 'camb-thick' .and.                             &
       trim(shape_functions) /= 'camb-thick-plus')                             &
    call my_stop("nfunctions_bot must be >= 0.")
  if (initial_perturb <= 0.d0)                                                 &
    call my_stop("initial_perturb must be > 0.")
  if ((min_bump_width <= 0.d0) .and.                                           &
      trim(shape_functions) /= 'camb-thick' .and.                              &
      trim(shape_functions) /= 'camb-thick-plus')                              &
    call my_stop("min_bump_width must be > 0.")


! No more restart
  if (restart) &
    call my_stop("The restart option is no more supported in Xoptfoil-JX. "//  &
    "Please remove this option from input file.")




! Constraints

  if ((trim(shape_functions) == 'camb-thick') .or. &
  (trim(shape_functions) == 'camb-thick-plus')) then
    ! in case of camb_thick checking of curvature makes no sense
    if (check_curvature) then 
      call print_note ("Because of shape function 'camb-thick' curvature ckecking "// &
                       "will be switched off during optimization")
      check_curvature = .false. 
      auto_curvature  = .false. 
    end if 
  end if 


  if (check_curvature ) then 
    if (curv_threshold <= 0.d0)    call my_stop("curv_threshold must be > 0.")
    if (highlow_threshold < 0.01d0) call my_stop("highlow_threshold must be >= 0.01")
    if (max_curv_reverse_top < 0)  call my_stop("max_curv_reverse_top must be >= 0.")
    if (max_curv_reverse_bot < 0)  call my_stop("max_curv_reverse_bot must be >= 0.")
    if (max_curv_highlow_top < 0)  call my_stop("max_curv_highlow_top must be >= 0.")
    if (max_curv_highlow_bot < 0)  call my_stop("max_curv_highlow_bot must be >= 0.")
    if (max_te_curvature < 0.d0)   call my_stop("max_te_curvature must be >= 0.")
  end if 

  if (min_thickness <= 0.d0) call my_stop("min_thickness must be > 0.")
  if (max_thickness <= 0.d0) call my_stop("max_thickness must be > 0.")
  if (min_thickness >= max_thickness)                                          &
    call my_stop("min_thickness must be < max_thickness.")
  do i = 1, noppoint
    if (trim(moment_constraint_type(i)) /=  'none')                               &
      call my_stop("Moment constraints are no more supported in Xoptfoil-JX. "//  &
                    "Please use target_moment instead")
  end do
  if (min_te_angle < 0.d0) call my_stop("min_te_angle must be >= 0.")
  if (symmetrical)                                                             &
    call print_note ("Mirroring top half of seed airfoil for symmetrical constraint.")
  if (min_flap_degrees >= max_flap_degrees)                                    &
    call my_stop("min_flap_degrees must be < max_flap_degrees.")
  if (min_flap_degrees <= -90.d0)                                              &
    call my_stop("min_flap_degrees must be > -90.")
  if (max_flap_degrees >= 90.d0)                                               &
    call my_stop("max_flap_degrees must be < 90.")
  if (min_camber >= max_camber)                                                &
    call my_stop("min_camber must be < max_camber.")
  
  if (naddthickconst > max_addthickconst) then
     write(text,*) max_addthickconst
     text = adjustl(text)
     call my_stop("naddthickconst must be <= "//trim(text)//".")
  end if
  do i = 1, naddthickconst
    if (addthick_x(i) <= 0.d0) call my_stop("addthick_x must be > 0.")
    if (addthick_x(i) >= 1.d0) call my_stop("addthick_x must be < 1.")
    if (addthick_min(i) >= addthick_max(i))                                    &
      call my_stop("addthick_min must be < addthick_max.")
  end do

! jx-mod Smoothing - check options 

  if (do_smoothing) then
    if ( spike_threshold < 0.1d0 )                                             &
      call my_stop ("spike_threshold must be >= 0.1")
  end if

! jx-mod Geo targets - check options

  do i = 1, ngeo_targets

    if (((trim(geo_targets(i)%type) /= 'zBot' .and.                            &
      trim(geo_targets(i)%type) /= 'zTop') .and.                               &
      trim(geo_targets(i)%type) /= 'Camber') .and.                             &
      trim(geo_targets(i)%type) /= 'Thickness')                                &
      call my_stop("target type must be 'zBot', 'zTop', 'Camber',"//           &
                   " or 'Thickness'.")
    if ((geo_targets(i)%x <= 0.d0 .or.                                         &
        geo_targets(i)%x >= 1.d0) .and.                                        &
        trim(geo_targets(i)%type) /= 'Camber' .and.                            &
        trim(geo_targets(i)%type) /= 'Thickness')                              &
      call my_stop("x of geometry target position must be > 0 and < 1.")
 end do   


! Naca airfoil options

  select case (adjustl(family))
    case ('4', '4M', '5', '63', '64', '65', '66', '67', '63A', '64A', '65A')
      continue
    case default
      call my_stop("Unrecognized NACA airfoil family.")
  end select
  if (maxt <= 0.d0) call my_stop("maxt must be > 0.")
  if ( (xmaxt < 0.d0) .or. (xmaxt > 1.d0) )                                    &
    call my_stop("xmaxt must be >= 0 and <= 1.")
  if ( (xmaxc < 0.d0) .or. (xmaxc > 1.d0) )                                    &
    call my_stop("xmaxc must be >= 0 and <= 1.")
  if ( (a < 0.d0) .or. (a > 1.d0) )                                            &
    call my_stop("a must be >= 0 and <= 1.")
  if (leidx <= 0.d0) call my_stop("leidx must be > 0.")

! Initialization options
    
  if ((feasible_limit <= 0.d0) .and. feasible_init)                            &
    call my_stop("feasible_limit must be > 0.")
  if ((feasible_init_attempts < 1) .and. feasible_init)                        &
    call my_stop("feasible_init_attempts must be > 0.")

! Optimizer options

  if (trim(search_type) == 'global' .or.                                       &
       trim(search_type) == 'global_and_local') then

    if (trim(global_search) == 'particle_swarm') then

!     Particle swarm options

      if (pso_pop < 1) call my_stop("pso_pop must be > 0.")
      if (pso_tol <= 0.d0) call my_stop("pso_tol must be > 0.")
      if (pso_maxit < 1) call my_stop("pso_maxit must be > 0.")  
      if ( (trim(pso_convergence_profile) /= "quick") .and.                    &
           (trim(pso_convergence_profile) /= "exhaustive") .and.               &
           (trim(pso_convergence_profile) /= "quick_camb_thick"))                       &
        call my_stop("pso_convergence_profile must be 'exhaustive' "//&
                     "or 'quick' or 'quick_camb_thick'.")

    else if (trim(global_search) == 'genetic_algorithm') then

!     Genetic algorithm options

      if (ga_pop < 1) call my_stop("ga_pop must be > 0.")
      if (ga_tol <= 0.d0) call my_stop("ga_tol must be > 0.")
      if (ga_maxit < 1) call my_stop("ga_maxit must be > 0.")
      if ( (trim(parents_selection_method) /= "roulette") .and.                &
           (trim(parents_selection_method) /= "tournament") .and.              &
           (trim(parents_selection_method) /= "random") )                      &
        call my_stop("parents_selection_method must be 'roulette', "//&
                     "'tournament', or 'random'.")
      if ( (parent_fraction <= 0.d0) .or. (parent_fraction > 1.d0) )           &
        call my_stop("parent_fraction must be > 0 and <= 1.")
      if (roulette_selection_pressure <= 0.d0)                                 &
        call my_stop("roulette_selection_pressure must be > 0.")
      if ( (tournament_fraction <= 0.d0) .or. (tournament_fraction > 1.d0) )   &
        call my_stop("tournament_fraction must be > 0 and <= 1.")
      if (crossover_range_factor < 0.d0)                                       &
        call my_stop("crossover_range_factor must be >= 0.")
      if ( (mutant_probability < 0.d0) .or. (mutant_probability > 1.d0) )      &
        call my_stop("mutant_probability must be >= 0 and <= 1.") 
      if (chromosome_mutation_rate < 0.d0)                                     &
        call my_stop("chromosome_mutation_rate must be >= 0.")
      if (mutation_range_factor < 0.d0)                                        &
        call my_stop("mutation_range_factor must be >= 0.")

    end if

  end if

  if (trim(search_type) == 'local' .or.                                        &
       trim(search_type) == 'global_and_local') then

!   Simplex options

    if (simplex_tol <= 0.d0) call my_stop("simplex_tol must be > 0.")
    if (simplex_maxit < 1) call my_stop("simplex_maxit must be > 0.")  
  
  end if

  
end subroutine read_inputs



!=============================================================================
! Read operating points specification from input file 
!   (separated from read_inputs to be more modular)
!=============================================================================

subroutine read_op_points_spec  (input_file, or_iunit, noppoint, op_points_spec)

  use xfoil_driver,       only : op_point_specification_type
  use vardef,             only : max_op_points
  use airfoil_operations, only : my_stop


  character(*), intent(in)  :: input_file 
  integer, intent(in)       :: or_iunit
  integer, intent(out)      :: noppoint
  type(op_point_specification_type), dimension(:), allocatable, intent(out)  :: op_points_spec

! Op_point specification 
  character(7),     dimension(max_op_points)  :: op_mode
  character(15),    dimension(max_op_points)  :: optimization_type
  double precision, dimension(max_op_points)  :: op_point, weighting, &
                                                 ncrit_pt, target_value, reynolds, mach

  double precision :: re_default
  logical          :: re_default_as_resqrtcl
  type(op_point_specification_type) :: op

  integer               :: i, iunit, ioerr, iostat1
  character(10)         :: text

  namelist /operating_conditions/ noppoint, op_mode, op_point, reynolds, mach,   &
            target_value, weighting, optimization_type, ncrit_pt,                & 
            re_default_as_resqrtcl, re_default


  ! Set defaults for operating conditions and constraints

  noppoint = 1

  re_default = 100000d0
  re_default_as_resqrtcl = .false.
  reynolds(:) = -1.d0                         ! value in input file

  op_mode(:) = 'spec-cl'
  op_point(:) = 0.d0
  optimization_type(:) = 'min-drag'
  mach(:) = 0.d0
  weighting(:) = 1.d0
  ncrit_pt(:) = -1.d0
  target_value(:) = -1.d3 


! (Open input file) , read options

  if (trim(input_file) == '') then
    iunit = or_iunit
    rewind(iunit)
    read(iunit, iostat=iostat1, nml=operating_conditions)
  else
    iunit = 12
    open(unit=iunit, file=input_file, status='old', iostat=ioerr)
    if (ioerr /= 0) call my_stop('Could not find input file '//trim(input_file)//'.')
    read(iunit, iostat=iostat1, nml=operating_conditions)
    close(iunit)
  end if
  call namelist_check('operating_conditions', iostat1, 'err')

! store op_point specification in data structure ------------------------

  allocate (op_points_spec(noppoint)) 

  do i = 1, noppoint
    op_points_spec(i)%spec_cl = (op_mode(i) == 'spec-cl')
    op_points_spec(i)%value   = op_point(i)
    
    op_points_spec(i)%ncrit = ncrit_pt(i)    
    op_points_spec(i)%optimization_type = optimization_type (i)
    op_points_spec(i)%target_value = target_value (i)
    
    if (reynolds(i) /= -1.d0) then
      op_points_spec(i)%re%number  = reynolds(i)
      op_points_spec(i)%re%type    = 1
    else                                    ! take default Re number
      op_points_spec(i)%re%number  = re_default
      if (re_default_as_resqrtcl) then
        op_points_spec(i)%re%type    = 2
      else
        op_points_spec(i)%re%type    = 1
      end if
    end if
    op_points_spec(i)%ma%number  = mach(i)   ! mach number only Type 1
    op_points_spec(i)%ma%type    = 1

    op_points_spec(i)%weighting_user  = weighting (i)
  end do 

!  Modify normalize weightings for operating points
!  ... will be re-normalized if there are geo_targets...!
  op_points_spec%weighting    = op_points_spec%weighting_user / sum(op_points_spec%weighting_user)
  op_points_spec%scale_factor = 1d0


! Check input data  ------------------------

  if (noppoint < 1) call my_stop("noppoint must be > 0")
  if (noppoint > max_op_points) then
     write(text,*) max_op_points
     text = adjustl(text)
     call my_stop("noppoints must be <= "//trim(text)//".")
  end if

  do i = 1, noppoint

    op  = op_points_spec(i) 

    if (op%re%number <= 0.d0) &
      call my_op_stop (i,op_points_spec, "reynolds must be > 0. Default value (re_default) could not be set")
    if (op%ma%number < 0.d0) &
      call my_op_stop (i,op_points_spec, "mach must be >= 0.")
    if (op%weighting_user <= 0.d0) &
      call my_op_stop (i,op_points_spec, "weighting must be > 0.")
    if (trim(op%optimization_type) /= 'min-drag' .and.                         &
      trim(op%optimization_type) /= 'max-glide' .and.                          &
      trim(op%optimization_type) /= 'min-sink' .and.                           &
      trim(op%optimization_type) /= 'max-lift' .and.                           &
      trim(op%optimization_type) /= 'target-moment' .and.                      &
      trim(op%optimization_type) /= 'target-drag' .and.                        &
      trim(op%optimization_type) /= 'target-lift' .and.                        &
      trim(op%optimization_type) /= 'max-xtr' .and.                            &
      trim(op%optimization_type) /= 'min-lift-slope' .and.                     &
      trim(op%optimization_type) /= 'min-glide-slope' .and.                    &
      trim(op%optimization_type) /= 'max-lift-slope')                          &
      call my_op_stop (i,op_points_spec, "optimization_type must be 'min-drag', 'max-glide', "//     &
                   "'min-sink', 'max-lift', 'max-xtr', 'target-moment', "//    &
                   "'target-drag', 'min-lift-slope', 'min-glide-slope'"//      &
                   " or 'max-lift-slope'.")
    if ((trim(op%optimization_type) == 'max-lift-slope') .and. (noppoint == 1))&
      call my_op_stop (i,op_points_spec, "at least two operating points are required for to "//      &
                   "maximize lift curve slope.")
    if ((trim(op%optimization_type) == 'min-lift-slope') .and. (noppoint == 1))&
      call my_op_stop (i,op_points_spec, "at least two, better three operating points are required"//&
                   " for to minimize lift curve slope.")
    if ((trim(op%optimization_type) == 'min-glide-slope') .and. (noppoint == 1))&
      call my_op_stop (i,op_points_spec, "at least two, better three operating points are required"//&
                   " for to minimize lift curve slope.")
    if ((op%ncrit <= 0.d0) .and. (op%ncrit /= -1d0)) &
      call my_op_stop (i,op_points_spec, "ncrit_pt must be > 0 or -1.")

    if (((trim(op%optimization_type) == 'target-moment') .and.                &
        (target_value(i)) == -1.d3) )                                         &
      call my_op_stop (i,op_points_spec, "No 'target-value' defined for "//  &
                 "for optimization_type 'target-moment'")
    if (((trim(op%optimization_type) == 'target-drag') .and.                  &
        (target_value(i)) == -1.d3) )                                         &
      call my_op_stop (i,op_points_spec, "No 'target-value' defined for "//  &
                     "for optimization_type 'target-drag'")
    if (((trim(op%optimization_type) == 'target-lift') .and.                  &
        (target_value(i)) == -1.d3) )                                         &
      call my_op_stop (i,op_points_spec, "No 'target-value' defined for "//  &
                     "for optimization_type 'target-lift'")
  end do

end subroutine read_op_points_spec


!=============================================================================
! Read xoptfoil input file to get flap setting options
!=============================================================================

subroutine read_flap_inputs  (input_file, or_iunit, flap_spec, flap_degrees, flap_selection) 

  use airfoil_operations, only : my_stop
  use vardef,             only : flap_spec_type

  character(*), intent(in)      :: input_file
  integer, intent(in)           :: or_iunit
  type(flap_spec_type), intent(out) :: flap_spec
  double precision, dimension(:), intent(inout) :: flap_degrees
  character(8),     dimension(:), intent(inout) :: flap_selection

  double precision              :: x_flap, y_flap
  character(3)                  :: y_flap_spec
  integer                       :: iostat1, iunit, i, ioerr
  logical                       :: use_flap

  namelist /operating_conditions/ use_flap, x_flap, y_flap, y_flap_spec, &
                                  flap_degrees, flap_selection

  ! Init default values 

  use_flap     = .false.                !currently dummy
  x_flap       = 0.75d0
  y_flap       = 0.d0
  y_flap_spec  = 'y/c'

  flap_degrees      = 0d0
  flap_selection    = 'specify'

  ! Open input file and read namelist from file

  if (trim(input_file) == '') then
    iunit = or_iunit
    rewind(iunit)
    read(iunit, iostat=iostat1, nml=operating_conditions)
  else
    iunit = 12
    open(unit=iunit, file=input_file, status='old', iostat=ioerr)
    if (ioerr /= 0) call my_stop('Could not find input file '//trim(input_file)//'.')
    read(iunit, iostat=iostat1, nml=operating_conditions)
    close(iunit)
  end if

  ! All parms optional - so no warning call namelist_check ...

  ! Check Input 

  if ((use_flap) .and. (x_flap <= 0.0)) call my_stop("x_flap must be > 0.")
  if ((use_flap) .and. (x_flap >= 1.0)) call my_stop("x_flap must be < 1.")
  if ((use_flap) .and. (y_flap_spec /= 'y/c') .and. (y_flap_spec /= 'y/t'))    &
    call my_stop("y_flap_spec must be 'y/c' or 'y/t'.")

  if ((y_flap_spec  /= 'y/c') .and. (y_flap_spec  /= 'y/t')) &
    call my_stop ("Vertical hinge definition must be 'y/c' or 'y/t'")
  do i = 1, size(flap_degrees)
    if (abs(flap_degrees(i)) > 70d0) &
      call my_stop ('Flap angle must be less than 70 degrees')
    if ((flap_selection(i) /= 'specify') .and. (flap_selection(i) /= 'optimize')) then
      call my_stop ("Flap selection must be 'spcify' or 'optimize')")
    end if 
  end do

  flap_spec%use_flap    = use_flap
  flap_spec%x_flap      = x_flap
  flap_spec%y_flap      = y_flap
  flap_spec%y_flap_spec = y_flap_spec

end subroutine read_flap_inputs


!-----------------------------------------------------------------------------
! Stops execution when there is an invalid op_point paramter
!-----------------------------------------------------------------------------

subroutine my_op_stop (iop, op_points_spec, message)

  use airfoil_operations, only : my_stop
  use xfoil_driver,       only : op_point_specification_type

  type(op_point_specification_type), dimension(:), allocatable, intent(in)  :: op_points_spec
  integer, intent (in)      :: iop
  character(*), intent(in)  :: message
  character(20)             :: op_string

  write (op_string, '(I2)') iop
  op_string = 'Op_point '// adjustl(trim(op_string)) 

  call echo_op_points_spec  (op_points_spec)

  call my_stop (trim(op_string) //': '// message)

end subroutine my_op_stop


!-----------------------------------------------------------------------------
! Echo input parms of operating points entered by user
!-----------------------------------------------------------------------------

subroutine echo_op_points_spec  (op_points_spec)

  use xfoil_driver,       only : op_point_specification_type

  type(op_point_specification_type), dimension(:), allocatable, intent(in)  :: op_points_spec

  integer               :: i, re_int
  character(10)         :: spec_char, target_value_char, ncrit_char
  type(op_point_specification_type) :: op


  !write (*,'(" - ",A)') 'Echo operating point definitions'
  write (*,*) 
  write (*,'(" - ",A2,":",A7,A6,A15,A9,A10,A5,A7,A11, A11)') &
                'No', 'Spec', 'Point', 'Opt. Type', ' Target', & 
                'Re', 'Type', 'ncrit', 'Weighting','normalized'
  
  do i = 1, size (op_points_spec)

    op = op_points_spec(i)

    if (op%spec_cl) then
      spec_char = 'cl'
    else
      spec_char = 'alpha'
    end if

    if (op%target_value == -1.d3 ) then 
      target_value_char = '-'
    else
      write (target_value_char,'(F9.5)') op%target_value
    end if

    if (op%ncrit == -1.d0 ) then 
      ncrit_char = '-'
    else
      write (ncrit_char,'(F7.1)') op%ncrit
    end if

    re_int = int (op%re%number)

    write (*,'(3x,I2,":",A7,F6.2,A15,A9,I10,I5,A7,F11.2, F11.2 )') &
          i, trim(spec_char), op%value, trim(op%optimization_type), trim(target_value_char), &
          re_int, op%re%type, trim(ncrit_char), op%weighting_user, op%weighting
  end do 


end subroutine echo_op_points_spec


!=============================================================================
! Read xoptfoil input file to geometric constraints
!=============================================================================

subroutine read_geo_constraints_inputs  (input_file, or_iunit, &
                                          check_curvature, auto_curvature,           &
                                          max_te_curvature,                           &
                                          max_curv_reverse_top, max_curv_reverse_bot, &
                                          max_curv_highlow_top, max_curv_highlow_bot, &
                                          curv_threshold,highlow_threshold)

  use airfoil_operations, only : my_stop

  character(*), intent(in)           :: input_file
  integer, intent(in)                :: or_iunit
  logical, intent(inout)             :: check_curvature, auto_curvature 
  double precision , intent(inout)   :: max_te_curvature,          &
                                        curv_threshold,highlow_threshold
  integer, intent(inout)             :: max_curv_reverse_top, max_curv_reverse_bot, &
                                        max_curv_highlow_top, max_curv_highlow_bot
  integer :: iostat1, iunit, ioerr

  namelist /constraints/ check_curvature, auto_curvature, &
                         highlow_threshold, curv_threshold, max_te_curvature, &
                         max_curv_reverse_top, max_curv_reverse_bot,  &
                         max_curv_highlow_top, max_curv_highlow_bot

  check_curvature      = .true.
  auto_curvature       = .true.
  max_te_curvature     = 10.d0                    ! more or less inactive by default
  max_curv_reverse_top = 0
  max_curv_reverse_bot = 0
  max_curv_highlow_top = 0
  max_curv_highlow_bot = 0
  curv_threshold       = 0.01d0
  highlow_threshold    = 0.02d0

  
  ! Open input file and read namelist from file

  if (trim(input_file) == '') then
    iunit = or_iunit
    rewind(iunit)
    read(iunit, iostat=iostat1, nml=constraints)
  else
    iunit = 12
    open(unit=iunit, file=input_file, status='old', iostat=ioerr)
    if (ioerr == 0) then
      read(iunit, iostat=iostat1, nml=constraints)
      close(iunit)
    end if
  end if
  ! call namelist_check('constraints', iostat1, 'warn')


end subroutine read_geo_constraints_inputs


!=============================================================================
! Read xoptfoil input file to xfoil_paneling_options
!   (separated from read_inputs to be more modular)
!=============================================================================

subroutine read_xfoil_paneling_inputs  (input_file, or_iunit, geom_options)

  use vardef,             only : npan_fixed
  use airfoil_operations, only : my_stop
  use xfoil_driver,       only : xfoil_geom_options_type

  character(*), intent(in) :: input_file
  integer, intent(in)      :: or_iunit
  type(xfoil_geom_options_type), intent(out) :: geom_options
  double precision :: cvpar, cterat, ctrrat, xsref1, xsref2, xpref1, xpref2

  integer :: npan
  integer :: iostat1, iunit, ioerr
  logical :: repanel

  namelist /xfoil_paneling_options/ npan, cvpar, cterat, ctrrat, xsref1,       &
            xsref2, xpref1, xpref2

  ! Init default values for xfoil options

  if (npan_fixed > 0) then 
    npan   = npan_fixed     ! if npan_fixed is set - this is the one
  else             
    npan   = 200            ! a real default
  end if             

  cvpar  = 1.d0
  cterat = 0.d0             ! if set to normal value 0.15d0, the curvature at TE panel
                            !   tends to flip away and have tripple value (bug in xfoil) 
                            !   with a very small value the panel gets wider and the quality better
  ctrrat = 0.2d0
  xsref1 = 1.d0
  xsref2 = 1.d0
  xpref1 = 1.d0
  xpref2 = 1.d0

  repanel = .false.          ! repanel for each design before running xfoil

  
! Open input file and read namelist from file

  if (trim(input_file) == '') then
    iunit = or_iunit
    rewind(iunit)
    read(iunit, iostat=iostat1, nml=xfoil_paneling_options)
  else
    iunit = 12
    open(unit=iunit, file=input_file, status='old', iostat=ioerr)
    if (ioerr == 0) then
      read(iunit, iostat=iostat1, nml=xfoil_paneling_options)
      close(iunit)
    end if
  end if
  
! Put xfoil options into derived types

  if (npan_fixed > 0 .and. (npan /= npan_fixed)) then 
    npan = npan_fixed 
    call print_note ("Number of panels (npan) is fixed for optimizations and can't be hanged")
  end if 

  if (npan < 20) call my_stop("npan must be >= 20.")
  if (cvpar <= 0.d0) call my_stop("cvpar must be > 0.")
  if (cterat < 0.d0) call my_stop("cterat must be >= 0.")
  if (ctrrat <= 0.d0) call my_stop("ctrrat must be > 0.")
  if (xsref1 < 0.d0) call my_stop("xsref1 must be >= 0.")
  if (xsref2 < xsref1) call my_stop("xsref2 must be >= xsref1")
  if (xsref2 > 1.d0) call my_stop("xsref2 must be <= 1.")
  if (xpref1 < 0.d0) call my_stop("xpref1 must be >= 0.")
  if (xpref2 < xpref1) call my_stop("xpref2 must be >= xpref1")
  if (xpref2 > 1.d0) call my_stop("xpref2 must be <= 1.")


  geom_options%repanel = repanel
  geom_options%npan   = npan
  geom_options%cvpar  = cvpar
  geom_options%cterat = cterat
  geom_options%ctrrat = ctrrat
  geom_options%xsref1 = xsref1
  geom_options%xsref2 = xsref2
  geom_options%xpref1 = xpref1
  geom_options%xpref2 = xpref2

end subroutine read_xfoil_paneling_inputs

!=============================================================================
! Read xfoil_run_options input
!=============================================================================

subroutine read_xfoil_options_inputs  (input_file, or_iunit, show_details, xfoil_options)

  use xfoil_driver, only : xfoil_options_type
  use airfoil_operations, only : my_stop

  character(*), intent(in) :: input_file 
  integer, intent(in)      :: or_iunit
  logical,      intent(in) :: show_details
  type(xfoil_options_type), intent(out)    :: xfoil_options

  logical :: viscous_mode, silent_mode, fix_unconverged, reinitialize
  integer :: bl_maxit
  double precision :: ncrit, xtript, xtripb, vaccel
  integer :: iunit, ioerr, iostat1

  namelist /xfoil_run_options/ ncrit, xtript, xtripb, viscous_mode,            &
  silent_mode, bl_maxit, vaccel, fix_unconverged, reinitialize


  ! Set default xfoil aerodynamics

  ncrit = 9.d0
  xtript = 1.d0
  xtripb = 1.d0
  viscous_mode = .true.
  silent_mode = .true.
  bl_maxit = 40             ! reduced to 40 as above the potential result is rarely usable..
  vaccel = 0.005d0          ! the original value of 0.01 leads to too many non convergences at 
                            !   higher lift --> reduced 
  fix_unconverged = .true.
  reinitialize = .false.    ! as run_xfoil is improved, this will speed up the xfoil calcs

! Read xfoil options

  if (trim(input_file) == '') then
    iunit = or_iunit
    rewind(iunit)
    read(iunit, iostat=iostat1, nml=xfoil_run_options)
  else
    iunit = 12
    open(unit=iunit, file=input_file, status='old', iostat=ioerr)
    if (ioerr == 0) then
      read(iunit, iostat=iostat1, nml=xfoil_run_options)
      close(iunit)
    end if
  end if

! XFoil run options

  if (ncrit < 0.d0) call my_stop("ncrit must be >= 0.")
  if (xtript < 0.d0 .or. xtript > 1.d0)                                        &
    call my_stop("xtript must be >= 0. and <= 1.")
  if (xtripb < 0.d0 .or. xtripb > 1.d0)                                        &
    call my_stop("xtripb must be >= 0. and <= 1.")
  if (bl_maxit < 1) call my_stop("bl_maxit must be > 0.")
  if (vaccel < 0.d0) call my_stop("vaccel must be >= 0.")


  ! Put xfoil options into derived types

  xfoil_options%ncrit = ncrit
  xfoil_options%xtript = xtript
  xfoil_options%xtripb = xtripb
  xfoil_options%viscous_mode = viscous_mode
  xfoil_options%silent_mode = silent_mode
  xfoil_options%maxit = bl_maxit
  xfoil_options%vaccel = vaccel
  xfoil_options%fix_unconverged = fix_unconverged
  xfoil_options%exit_if_unconverged = .false.
  xfoil_options%reinitialize = reinitialize
  xfoil_options%show_details = show_details

end subroutine read_xfoil_options_inputs

!-----------------------------------------------------------------------------
! Echo xfoil_run_options input
!-----------------------------------------------------------------------------

subroutine echo_xfoil_options_inputs  (xfoil_options)

  use xfoil_driver, only : xfoil_options_type
  type(xfoil_options_type), intent(out)    :: xfoil_options

  write(*,'(A)') " &xfoil_run_options"
  write(*,*) " ncrit = ", xfoil_options%ncrit
  write(*,*) " xtript = ", xfoil_options%xtript
  write(*,*) " xtripb = ", xfoil_options%xtripb
  write(*,*) " viscous_mode = ", xfoil_options%viscous_mode
  write(*,*) " silent_mode = ", xfoil_options%silent_mode
  write(*,*) " bl_maxit = ", xfoil_options%maxit
  write(*,*) " vaccel = ", xfoil_options%vaccel
  write(*,*) " fix_unconverged = ", xfoil_options%fix_unconverged
  write(*,*) " reinitialize = ", xfoil_options%reinitialize
  write(*,'(A)') " /"
  write(*,*)

end subroutine 

!=============================================================================80
!
! Prints error and stops or warns for bad namelist read
!
!=============================================================================80
subroutine namelist_check(nmlname, errcode, action_missing_nml)

  use os_util,            only : print_note
  use airfoil_operations, only : my_stop
 

  character(*), intent(in) :: nmlname
  integer, intent(in) :: errcode
  character(*), intent(in) :: action_missing_nml

  character (20) :: out_string

  if (errcode < 0) then
    if (trim(action_missing_nml) == 'warn') then
      call print_note ('Namelist '//trim(nmlname)//& 
                      ' not found in input file. Using default values.')
    else
      call my_stop ('Error: namelist '//trim(nmlname)//&
                     ' is required and was not found in input file.')
    end if
  else if (errcode == 2) then
    if (trim(action_missing_nml) == 'warn') then
      call print_note ('No input file. Using default values for namelist '// trim(nmlname))
    else
      call my_stop ('Error: No input file. Namelist '//trim(nmlname)//&
                     ' is required for operation.')
    end if
  else if (errcode > 0) then
    write (out_string,'(I5)') errcode
    out_string = ' (err='//trim(adjustl(out_string))//')'
    call my_stop ('Error: unrecognized variable in namelist '//trim(nmlname)//trim(out_string))
  else
    continue
  end if

end subroutine namelist_check

!=============================================================================80
!
! Reads command line arguments for input file name and output file prefix
!
!=============================================================================80
subroutine read_clo(input_file, output_prefix, exename)

  use airfoil_operations, only : my_stop
  use os_util, only: print_error

  character(*), intent(inout) :: input_file, output_prefix
  character(*), intent(in), optional :: exename

  character(80) :: arg, exeprint
  integer i, nargs
  logical getting_args

  if (present(exename)) then
    exeprint = exename
  else
    exeprint = "xoptfoil"
  end if 

  nargs = iargc()
  if (nargs > 0) then
    getting_args = .true.
  else
    getting_args = .false.
  end if

  i = 1
  do while (getting_args)
    call getarg(i, arg) 

    if (trim(arg) == "-i") then
      if (i == nargs) then
        call my_stop("Must specify an input file with -i option.")
      else
        call getarg(i+1, input_file)
        i = i+2
      end if
    else if (trim(arg) == "-o") then
      if (i == nargs) then
        call my_stop("Must specify an output prefix with -o option.")
      else
        call getarg(i+1, output_prefix)
        i = i+2
      end if
! jx-mod new default re number
    else if (trim(arg) == "-r") then
      if (i == nargs) then
        call my_stop("Must specify a re value for -r option.")
      else
        call getarg(i+1, arg)
        i = i+2
      end if
! jx-mod new seed airfoil 
    else if (trim(arg) == "-a") then
      if (i == nargs) then
        call my_stop("Must specify filename of seed airfoil for -a option.")
      else
        call getarg(i+1, arg)
        i = i+2
      end if
    else if ( (trim(arg) == "-h") .or. (trim(arg) == "--help") ) then
      call print_usage(exeprint)
      stop
    else
      call print_error ("Unrecognized option "//trim(arg)//".")
      call print_usage (exeprint)
      stop 1
    end if

    if (i > nargs) getting_args = .false.
  end do

end subroutine read_clo


!=============================================================================80
!
! Prints usage information
!
!=============================================================================80
subroutine print_usage(exeprint)

  character(*), intent(in) :: exeprint

  write(*,'(A)') 
  write(*,'(A)') '         (c) 2017-2019 Daniel Prosser (original Xoptfoil)'
  write(*,'(A)') '         (c) 2019-2021 Jochen Guenzel, Matthias Boese'
  write(*,'(A)')
  write(*,'(A)') "Usage: "//trim(exeprint)//" [OPTION]"
  write(*,'(A)')
  write(*,'(A)') "Options:"
  write(*,'(A)') "  -i input_file     Specify a non-default input file"
  write(*,'(A)') "  -o output_prefix  Specify a non-default output prefix"
  write(*,'(A)') "  -r xxxxxx         Specify a default reynolds number (re_default)"
  write(*,'(A)') "  -a airfoil_file   Specify filename of seed airfoil"
  write(*,'(A)') "  -h, --help        Display usage information and exit"
  write(*,'(A)')
  write(*,'(A)') "Refer to the Xoptfoil    user guide and" 
  write(*,'(A)') "             Xoptfoil-JX reference guide for further help."
  write(*,'(A)')
  write(*,'(A)') "Development page: https://github.com/jxjo/Xoptfoil"
  write(*,'(A)') "Report bugs using the issue reporting system "
  write(*,'(A)')

end subroutine print_usage



! jx-mod -----------------------------------------------------------------
! Reads command line argument -r for default reynolds number
! ------------------------------------------------------------------------

function read_cl_re_default (re_default)

  use airfoil_operations, only : my_stop

  double precision, intent(in) :: re_default
  double precision :: read_cl_re_default

  character(80) :: arg, re_as_char
  integer i, nargs
  logical getting_args

  read_cl_re_default = re_default

  nargs = iargc()
  if (nargs > 0) then
    getting_args = .true.
  else
    getting_args = .false.
  end if

  i = 1
  do while (getting_args)
    call getarg(i, arg) 

    if (trim(arg) == "-r") then
      if (i == nargs) then
        call my_stop("Must specify a reynolds number with -r option.")
      else
        call getarg(i+1, re_as_char)
        read (re_as_char,*) read_cl_re_default
        if (read_cl_re_default == 0d0)     &
           call my_stop("-r option has no valid reynolds numer")
        exit 
      end if
    end if
    i = i + 1
    if (i > nargs) getting_args = .false.
  end do

end function read_cl_re_default

! jx-mod -----------------------------------------------------------------
! Reads command line argument -a for seed airfoil
! ------------------------------------------------------------------------

function read_cl_airfoil_file (file_name)

  use airfoil_operations, only : my_stop

  character(80), intent(in) :: file_name
  character(80) :: read_cl_airfoil_file

  character(80) :: arg
  integer i, nargs
  logical getting_args

  read_cl_airfoil_file = file_name

  nargs = iargc()
  if (nargs > 0) then
    getting_args = .true.
  else
    getting_args = .false.
  end if

  i = 1
  do while (getting_args)
    call getarg(i, arg) 

    if (trim(arg) == "-a") then
      if (i == nargs) then
        call my_stop("Must specify a airfoil filename with -a option.")
      else
        call getarg(i+1, arg)
        read_cl_airfoil_file = arg
      end if
    end if
    i = i + 1
    if (i > nargs) getting_args = .false.
  end do

end function read_cl_airfoil_file

end module input_output

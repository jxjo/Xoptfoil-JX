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

module optimization_driver

! Contains subroutines to set options and conditions for optimization and to
! issue optimizer calls

  use os_util

  implicit none

  contains

!=============================================================================80
!
! Preprocessing for non-aerodynamic optimization
!
!=============================================================================80
subroutine matchfoils_preprocessing(matchfoil_file)

  use vardef,             only : airfoil_type, seed_foil
  use airfoil_evaluation, only : foil_to_match, xfoil_geom_options
  use airfoil_operations, only : get_seed_airfoil,  rebuild_airfoil
  use airfoil_operations, only : repanel_and_normalize_airfoil, split_foil_at_00
  use math_deps,          only : interp_vector, transformed_arccos
  use naca,               only : naca_options_type
  use xfoil_driver,       only : xfoil_set_thickness_camber, xfoil_get_geometry_info, xfoil_set_airfoil
  
  character(*), intent(in) :: matchfoil_file

  type(airfoil_type) :: original_foil_to_match
  type(naca_options_type) :: dummy_naca_options
  integer :: pointst, pointsb, npan_sav, i, n
  double precision, dimension(:), allocatable :: zttmp, zbtmp, xmatcht, xmatchb, zmatcht, zmatchb
  double precision :: maxt, xmaxt, maxc, xmaxc

! Load airfoil to match

  call get_seed_airfoil('from_file', matchfoil_file, dummy_naca_options, original_foil_to_match)

  call print_note_only ('Preparing '//trim(original_foil_to_match%name)//' to be matched by '//&
                        trim(seed_foil%name),3)

  if(trim(seed_foil%name) == trim(original_foil_to_match%name)) then

  ! Seed and match foil are equal. Reduce thickness ... 

    call print_note ('Match foil and seed foil are the same. '// &
                     'The thickness of the match foil will be reduced bei 10%.', 3)

    call xfoil_set_airfoil (seed_foil)        
    call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)
    call xfoil_set_thickness_camber (seed_foil, maxt * 0.9d0 , 0d0, 0d0, 0d0, foil_to_match)
    call split_foil_at_00(foil_to_match)
    foil_to_match%name = seed_foil%name

  else

  ! Repanel to npan_fixed points and normalize to get LE at 0,0 and TE (1,0) and split

    call repanel_and_normalize_airfoil (original_foil_to_match, xfoil_geom_options, .false., foil_to_match)

  ! Interpolate x-vals of foil to match to seed airfoil points to x-vals 
  !    - so the z-values can later be compared

    xmatcht = foil_to_match%xt
    xmatchb = foil_to_match%xb
    zmatcht = foil_to_match%zt
    zmatchb = foil_to_match%zb
  
    pointst = size(seed_foil%xt,1)
    pointsb = size(seed_foil%xb,1)
    allocate(zttmp(pointst))
    allocate(zbtmp(pointsb))
    zttmp(pointst) = zmatcht(size(zmatcht,1))
    zbtmp(pointsb) = zmatchb(size(zmatchb,1))
    call interp_vector(xmatcht, zmatcht, seed_foil%xt(1:pointst-1),                    &
                      zttmp(1:pointst-1))
    call interp_vector(xmatchb, zmatchb, seed_foil%xb(1:pointsb-1),                    &
                      zbtmp(1:pointsb-1))

  ! Re-set coordinates of foil to match from interpolated points
                      
    call rebuild_airfoil(seed_foil%xt, seed_foil%xb, zttmp, zbtmp, foil_to_match)
  
  end if 

end subroutine matchfoils_preprocessing



!=============================================================================80
!
! Subroutine to drive the optimization
!
!=============================================================================80
subroutine optimize(search_type, global_search, local_search, constrained_dvs, &
                    pso_options, ga_options, ds_options, restart,              &
                    restart_write_freq, optdesign, f0_ref, fmin, steps, fevals)

  use vardef,             only : shape_functions, nflap_optimize,              &
                                 initial_perturb, min_flap_degrees,            &
                                 max_flap_degrees, flap_degrees,               &
                                 flap_optimize_points, min_bump_width,         &
                                 output_prefix, design_subdir 
  use particle_swarm,     only : pso_options_type, particleswarm
  use genetic_algorithm,  only : ga_options_type, geneticalgorithm
  use simplex_search,     only : ds_options_type, simplexsearch
  use airfoil_evaluation, only : objective_function,                           &
                                 objective_function_nopenalty, write_function

  character(*), intent(in) :: search_type, global_search, local_search
  type(pso_options_type), intent(in) :: pso_options
  type(ga_options_type), intent(in) :: ga_options
  type(ds_options_type), intent(in) :: ds_options
  double precision, dimension(:), intent(inout) :: optdesign
  double precision, intent(out) :: f0_ref, fmin
  integer, intent(in) :: restart_write_freq
  integer, dimension(:), intent(in) :: constrained_dvs
  integer, intent(out) :: steps, fevals
  logical, intent(in) :: restart

  integer :: counter, nfuncs, ndv
  double precision, dimension(size(optdesign,1)) :: xmin, xmax, x0
  double precision :: t1fact, t2fact, ffact
  logical :: restart_temp, write_designs
  integer :: stepsg, fevalsg, stepsl, fevalsl, i, oppoint, stat,               &
             iunit, ioerr, designcounter
  character(100) :: restart_status_file, histfile
  character(19) :: restart_status
  character(14) :: stop_reason

! Delete existing run_control file and rewrite it

  iunit = 23
  open(unit=iunit, file='run_control', status='replace')
  close(iunit)

! Delete existing optimization history for visualizer to start new

  histfile  = trim(design_subdir)//'Optimization_History.dat'
  
  if (.not. restart) then 
    iunit = 17
    open(unit=iunit, file=trim(histfile), status='replace')
    close(iunit)
  end if 
    
! Restart status file setup

  iunit = 15
  restart_status_file = 'restart_status_'//trim(output_prefix)

! Perform optimization: global, local, or global + local

  stepsg = 0
  fevalsg = 0
  stepsl = 0
  fevalsl = 0
  designcounter = 0

  ndv = size(optdesign,1)

! Scale all variables to have a range of initial_perturb
  t1fact = initial_perturb/(1.d0 - 0.001d0)             ! HH location
  t2fact = initial_perturb/(10.d0 - min_bump_width)     ! HH width
  ffact = initial_perturb/(max_flap_degrees - min_flap_degrees)

! Set initial design

  if ((trim(shape_functions) == 'naca') .or. &
      (trim(shape_functions) == 'camb-thick') .or. &
      (trim(shape_functions) == 'camb-thick-plus')) then     
  !----------naca / camb-thick ----------

    nfuncs = ndv - nflap_optimize

!   Mode strength = 0 (aka seed airfoil)

    x0(1:nfuncs) = 0.d0

!   Seed flap deflection as specified in input file

    do i = nfuncs + 1, ndv
      oppoint = flap_optimize_points(i-nfuncs)
      x0(i) = flap_degrees(oppoint)*ffact
    end do
  else
  !------------hicks-henne-------------
    
    nfuncs = (ndv - nflap_optimize)/3

!   Bump strength = 0 (aka seed airfoil)
    do i = 1, nfuncs
      counter = 3*(i-1)
      x0(counter+1) = 0.d0
      x0(counter+2) = 0.5d0*t1fact
      x0(counter+3) = 1.d0*t2fact
    end do
    do i = 3*nfuncs+1, ndv
      oppoint = flap_optimize_points(i-3*nfuncs)
      x0(i) = flap_degrees(oppoint)*ffact
    end do

  end if

! Compute f0_ref, ignoring penalties for violated constraints

  f0_ref = objective_function_nopenalty(x0) 

  
! Set default restart status (global or local optimization) from user input

  if (trim(search_type) == 'global_and_local' .or. trim(search_type) ==    &
      'global') then
    restart_status = 'global_optimization'
  else
    restart_status = 'local_optimization'
  end if


! Design coordinates/polars output handling

  write_designs = .false.
  if ( (trim(search_type) == 'global_and_local') .or.                          &
       (trim(search_type) == 'global') ) then
    if (trim(global_search) == 'particle_swarm')    write_designs = pso_options%write_designs
    if (trim(global_search) == 'genetic_algorithm') write_designs = ga_options%write_designs
  else
    if (ds_options%write_designs) write_designs = .true.
  end if
    
! Write seed airfoil coordinates and polars to file

  if (write_designs) then
    stat = write_function(x0, 0) 
  end if

! Set temporary restart variable

  restart_temp = restart

! Global optimization

  if (trim(restart_status) == 'global_optimization') then

!   Set up mins and maxes
    
    if ((trim(shape_functions) == 'naca') .or. &
        (trim(shape_functions) == 'camb-thick') .or. &
        (trim(shape_functions) == 'camb-thick-plus')) then

      nfuncs = ndv - nflap_optimize

      xmin(1:nfuncs) = -0.5d0*initial_perturb
      xmax(1:nfuncs) = 0.5d0*initial_perturb
      xmin(nfuncs+1:ndv) = min_flap_degrees*ffact
      xmax(nfuncs+1:ndv) = max_flap_degrees*ffact

    else

      nfuncs = (ndv - nflap_optimize)/3

      do i = 1, nfuncs
        counter = 3*(i-1)
        xmin(counter+1) = -initial_perturb/2.d0
        xmax(counter+1) = initial_perturb/2.d0
        xmin(counter+2) = 0.0001d0*t1fact
        xmax(counter+2) = 1.d0*t1fact
        xmin(counter+3) = min_bump_width*t2fact
        xmax(counter+3) = 10.d0*t2fact
      end do
      do i = 3*nfuncs+1, ndv
        xmin(i) = min_flap_degrees*ffact
        xmax(i) = max_flap_degrees*ffact
      end do

    end if

    if (trim(global_search) == 'particle_swarm') then

!     Particle swarm optimization
      call particleswarm(optdesign, fmin, stepsg, fevalsg, objective_function, &
                         x0, xmin, xmax, .true., f0_ref, constrained_dvs,      &
                         pso_options, designcounter, stop_reason, write_function)

    else if (trim(global_search) == 'genetic_algorithm') then

!     Genetic algorithm optimization

      call geneticalgorithm(optdesign, fmin, stepsg, fevalsg,                  &
                            objective_function, x0, xmin, xmax, .true.,        &
                            f0_ref, constrained_dvs, ga_options, restart_temp, &
                            restart_write_freq, designcounter, stop_reason,    &
                            write_function)

    end if

!   Update restart status and turn off restarting for local search

    if ( (stop_reason == "completed") .and.                                    &
         (trim(search_type) == 'global_and_local') )                           &
        restart_status = 'local_optimization'
    restart_temp = .false.

  end if

! Local optimization

  if (restart_status == 'local_optimization') then

    if (trim(local_search) == 'simplex') then

!     Simplex optimization

      if (trim(search_type) == 'global_and_local') then
        x0 = optdesign  ! Copy x0 from global search result
      end if

      call simplexsearch(optdesign, fmin, stepsl, fevalsl, objective_function, &
                         x0, .true., f0_ref, ds_options, restart_temp,         &
                         restart_write_freq, designcounter, stepsg,            &
                         write_function)

    end if

  end if

! Total number of steps and function evaluations

  steps = stepsg + stepsl
  fevals = fevalsg + fevalsl

! Write stop_monitoring command to run_control file

  iunit = 23
  open(unit=iunit, file='run_control', status='old', position='append',        &
       iostat=ioerr)
  if (ioerr /= 0) open(unit=iunit, file='run_control', status='new')
  write(iunit,'(A)') "stop_monitoring"
  close(iunit)

end subroutine optimize

!=============================================================================80
!
! Writes final airfoil design to a file
!    Returns final airfoil 
!
!=============================================================================80
subroutine write_final_design(optdesign, f0, fmin, final_airfoil)

  use vardef
  use airfoil_operations, only : airfoil_write
  use xfoil_driver,       only : run_op_points, op_point_result_type
  use xfoil_driver,       only : op_point_specification_type
  use airfoil_evaluation, only : create_airfoil_form_design, get_flap_degrees_from_design
  use airfoil_evaluation, only : xfoil_geom_options, xfoil_options, noppoint
  use airfoil_evaluation, only : op_points_spec, match_foils

  double precision, dimension(:), intent(in) :: optdesign
  double precision, intent(in)               :: f0, fmin
  type(airfoil_type), intent(out)            :: final_airfoil

  type(op_point_specification_type) :: op_spec
  type(op_point_result_type)        :: op
  type(op_point_result_type), dimension(:), allocatable :: op_points_result
  double precision, dimension(noppoint) :: actual_flap_degrees
  integer :: i, iunit
  character(80) :: output_file, aero_file
  character(20) :: flapnote
  double precision :: ncrit

  
! Rebuild foil out final design and seed airfoil

  call create_airfoil_form_design (seed_foil, optdesign, final_airfoil)
  
  final_airfoil%name   = output_prefix

! Use Xfoil to analyze final design

  if (.not. match_foils) then

!   Get actual flap angles based on design variables

    call get_flap_degrees_from_design (optdesign, actual_flap_degrees)

!   Run xfoil for requested operating points

    call run_op_points (final_airfoil, xfoil_geom_options, xfoil_options,        &
                        flap_spec, actual_flap_degrees,  &
                        op_points_spec, op_points_result)

!   Write summary to screen and file

    aero_file  = trim(design_subdir)//'Performance_Summary.dat'

    iunit = 13
    open(unit=iunit, file=aero_file, status='replace')

    write(*,*)
    write(*    ,'(A)') " Optimal airfoil performance summary"
    write(iunit,'(A)') " Optimal airfoil performance summary"
    write(*    ,'(A)') ""
    write(iunit,'(A)') ""

! i    alpha     CL        CD       Cm    Top Xtr Bot Xtr   Re      Mach     ncrit     flap 
! --  ------- -------- --------- -------- ------- ------- -------- -------- ------- -----------
!  7  -1.400   0.0042   0.00513  -0.0285  0.7057  0.2705  6.00E+04   0.000     9.1    5.23 spec
! I2    F8.3    F9.4     F10.5     F9.4    F8.4    F8.4     ES9.2     F8.3     F7.1    F6.2  

    write (iunit,'(A)') " i   alpha     CL        CD       Cm    Top Xtr Bot Xtr   Re      Mach    ncrit     flap"
    write (iunit,'(A)') " -- ------- -------- --------- -------- ------- ------- ------- -------- ------- -----------"
    write (*    ,'(A)') " i   alpha     CL        CD       Cm    Top Xtr Bot Xtr   Re      Mach    ncrit     flap"
    write (*    ,'(A)') " -- ------- -------- --------- -------- ------- ------- ------- -------- ------- -----------"

    do i = 1, noppoint

      op_spec  = op_points_spec(i)
      op       = op_points_result(i) 

      if (flap_spec%use_flap) then
        write (flapnote, '(F6.2)') actual_flap_degrees(i)
        if (flap_selection(i) == "specify") then
          flapnote = trim(flapnote) //" spec"
        else
          flapnote = trim(flapnote) //" opt"
        end if 
      else
        flapnote = "   -"
      end if 

      if (op_spec%ncrit == -1d0) then 
        ncrit = xfoil_options%ncrit
      else
        ncrit = op_spec%ncrit
      end if 

      write (iunit,  "(I2,   F8.3,   F9.4,    F10.5, F9.4,   F8.4,   F8.4, ES9.2     F8.3     F7.1, 3X, A)") &
        i, op%alpha, op%cl, op%cd, op%cm, op%xtrt, op%xtrb, &
        op_spec%re%number, op_spec%ma%number, ncrit, trim(flapnote)
      write (*    ,  "(I2,   F8.3,   F9.4,    F10.5, F9.4,   F8.4,   F8.4, ES9.2     F8.3     F7.1, 3X, A)") &
        i, op%alpha, op%cl, op%cd, op%cm, op%xtrt, op%xtrb, &
        op_spec%re%number, op_spec%ma%number, ncrit, trim(flapnote)

    end do

    write(*,*)
    write(*,'(A,F8.4,A1)', advance='no') " Objective function improvement over seed: "
    call  print_colored_r (9, '(F8.4,"%")', Q_GOOD, ((f0 - fmin)/f0*100.d0))

    write(iunit,*)
    write(iunit,'(A43F8.4A1)') " Objective function improvement over seed: ",  &
                           (f0 - fmin)/f0*100.d0, "%" 

    close(iunit)

    write(*,*)
    call print_note_only ("- Writing summary to "//trim(aero_file))


  else
    call write_matchfoil_summary (final_airfoil%zt, final_airfoil%zb)
  end if

! Write airfoil to file

  output_file = trim(output_prefix)//'.dat'
  call airfoil_write(output_file, output_prefix, final_airfoil)

end subroutine write_final_design


!-----------------------------------------------------------------------------
! Write some data of the final match foil 
!-----------------------------------------------------------------------------
subroutine write_matchfoil_summary (zt_new, zb_new)

  use vardef,             only : seed_foil
  use airfoil_evaluation, only : foil_to_match
  use math_deps,          only : median
  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_get_geometry_info
  
  double precision, dimension(size(seed_foil%xt,1)), intent(in) :: zt_new
  double precision, dimension(size(seed_foil%xb,1)), intent(in) :: zb_new
  double precision :: max_dzt, max_dzb, avg_dzt, avg_dzb, max_dzt_rel, max_dzb_rel
  double precision :: xmax_dzt, xmax_dzb, median_dzt, median_dzb
  double precision :: maxt, xmaxt, maxc, xmaxc
  integer          :: imax_dzt, imax_dzb

! Max Delta and position on top and bot 

  max_dzt  = maxval(abs (zt_new - foil_to_match%zt))
  max_dzb  = maxval(abs (zb_new - foil_to_match%zb))
  imax_dzt = maxloc(abs (zt_new - foil_to_match%zt),1)
  imax_dzb = maxloc(abs (zb_new - foil_to_match%zb),1)
  xmax_dzt = seed_foil%xt(imax_dzt)
  xmax_dzb = seed_foil%xb(imax_dzb)

! rel. deviation  

  call xfoil_set_airfoil (foil_to_match)        
  call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)

  max_dzt_rel = (max_dzt / maxt) * 100.d0
  max_dzb_rel = (max_dzb / maxt) * 100.d0
 
! absolute average and median of deltas  

  avg_dzt  = sum (abs(zt_new - foil_to_match%zt)) / size(seed_foil%xt,1)
  avg_dzb  = sum (abs(zb_new - foil_to_match%zb)) / size(seed_foil%xb,1)

  median_dzt  = median (zt_new - foil_to_match%zt)
  median_dzb  = median (zb_new - foil_to_match%zb)

  write(*,*)
  write(*,'(A)') " Match airfoil deviation summary"
  write(*,*)
  write(*,'(A)') "      Delta of y-coordinate between best design and match airfoil surface"
  write(*,*)
  write(*,'(A)') "               average      median   max delta      at  of thickness"

  write (*,'(A10)', advance = 'no') "   top:"
  write (*,'(F12.7,F12.7,F12.7,F8.4,F10.3,A1)') avg_dzt, median_dzt, max_dzt, xmax_dzt, max_dzt_rel,'%'
  write (*,'(A10)', advance = 'no') "   bot:"
  write (*,'(F12.7,F12.7,F12.7,F8.4,F10.3,A1)') avg_dzb, median_dzb, max_dzb, xmax_dzb, max_dzb_rel,'%'
  write(*,*)

end subroutine write_matchfoil_summary

end module optimization_driver

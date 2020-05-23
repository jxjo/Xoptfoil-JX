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

module particle_swarm

! Module containing particle swarm optimization routine

  implicit none

! Options type definition for PSO

  type pso_options_type

    integer :: pop                ! particle swarm population size
    double precision :: tol       ! tolerance in max radius of designs before
                                  !   triggering a stop condition
    double precision :: maxspeed  ! Max speed allowed for particles
    integer :: maxit              ! Max steps allowed before stopping
    logical :: feasible_init      ! Whether to enforce initially feasible
                                  !   designs
    double precision :: feasible_limit
                                  ! Max objective function value below which
                                  !   initial designs are considered feasible
    integer :: feasible_init_attempts
                                  ! Number of attempts to try to get a feasible
                                  !   initial design
    logical :: write_designs      ! Whether to write best design each time it
                                  !   changes
    logical :: relative_fmin_report
                                  ! If .true., reports improvement over seed
                                  !   design. Otherwise, reports fmin itself.
    character(20) :: convergence_profile
                                  ! 'exhaustive' or 'quick' or 'quick_camb_thick
                                  ! exhaustive takes onger but finds better 
                                  ! solutions 
    logical :: write_particlefile ! Whether to write particle-values for each
                                  ! iteration to file

  end type pso_options_type

  contains

!=============================================================================80
!
! Particle swarm optimization routine. Recommended as a first step to determine
! the vicinity of the global optimum, followed by a local search to hone in.
!
!=============================================================================80
subroutine particleswarm(xopt, fmin, step, fevals, objfunc, x0, xmin, xmax,    &
                         given_f0_ref, f0_ref, constrained_dvs, pso_options,   &
                         restart, restart_write_freq, designcounter,           &
                         stop_reason, converterfunc)

  use math_deps,         only : norm_2
  use optimization_util, only : init_random_seed, initial_designs,             &
                                design_radius, write_design, read_run_control

  use os_util,           only: COLOR_GOOD, COLOR_NORMAL, print_colored

  double precision, dimension(:), intent(inout) :: xopt
  double precision, intent(out) :: fmin
  integer, intent(out) :: step, fevals

  interface
    double precision function objfunc(x, evaluate_only_geometry)
      double precision, dimension(:), intent(in) :: x
      logical, intent(in), optional :: evaluate_only_geometry
    end function
  end interface
  
  double precision, dimension(:), intent(in) :: x0, xmin, xmax
  double precision, intent(inout) :: f0_ref
  integer, dimension(:), intent(in) :: constrained_dvs
  logical, intent(in) :: given_f0_ref, restart
  type (pso_options_type), intent(in) :: pso_options
  integer, intent(in) :: restart_write_freq
  integer, intent(out) :: designcounter
  character(14), intent(out) :: stop_reason

  optional :: converterfunc
  interface
    integer function converterfunc(x, designcounter)
      double precision, dimension(:), intent(in) :: x
      integer, intent(in) :: designcounter
    end function
  end interface

  integer :: nconstrained, i, j, fminloc, var, stat, restartcounter, iunit,    &
             ioerr, k, ncommands, particleunit
  double precision :: c1, c2, whigh, wlow, convrate, maxspeed, wcurr, mincurr, &
                      f0, radius
  double precision, dimension(pso_options%pop) :: objval, minvals, speed
  double precision, dimension(size(xmin,1)) :: randvec1, randvec2
  double precision, dimension(size(xmin,1),pso_options%pop) :: dv, vel,        &
                                                               bestdesigns
  logical :: use_x0, converged, signal_progress, new_history_file
  character(11) :: stepchar
  character(20) :: fminchar, radchar
  character(25) :: relfminchar, outstring
  character(80), dimension(20) :: commands
  

  nconstrained = size(constrained_dvs,1)

! PSO tuning variables

  if (trim(pso_options%convergence_profile) == "quick") then

    c1 = 1.2d0         ! particle-best trust factor
    c2 = 1.2d0         ! swarm-best trust factor
    whigh = 1.4d0      ! starting inertial parameter
    wlow = 0.6d0       ! ending inertial parameter
    convrate = 0.05d0  ! inertial parameter reduction rate

  else if (trim(pso_options%convergence_profile) == "exhaustive") then

    c1 = 1.4d0         ! particle-best trust factor
    c2 = 1.0d0         ! swarm-best trust factor
    whigh = 1.8d0      ! starting inertial parameter
    wlow = 0.8d0       ! ending inertial parameter
    convrate = 0.02d0  ! inertial parameter reduction rate

  else if (trim(pso_options%convergence_profile) == "quick_camb_thick") then

    c1 = 1.0d0         ! particle-best trust factor
    c2 = 1.6d0         ! swarm-best trust factor
    whigh = 1.2d0      ! starting inertial parameter
    wlow = 0.02d0       ! ending inertial parameter
    convrate = 0.01d0   ! inertial parameter reduction rate

  else
    write(*,*) "Error in particleswarm: convergence mode should be"//          &
               "'exhaustive' or 'quick' or 'quick_camb_thick'."
    stop
  end if

! Speed limits

  maxspeed = abs(pso_options%maxspeed)
  if (maxspeed > maxval(xmax - xmin)) then
    maxspeed = maxval(xmax - xmin)
  elseif (maxspeed < 1.0D-14) then
    maxspeed = maxval(xmax - xmin)
  end if

! Get f0 (reference seed design objective function)

  if (given_f0_ref) then
    f0 = f0_ref
  else 
    f0 = objfunc(x0)
    f0_ref = f0
  end if

! Open particle file
  call pso_open_particlefile(pso_options%write_particlefile, particleunit)
    
!$omp parallel default(shared) private(i, j, var)

! Initialize a random seed

  call init_random_seed()

! Set up initial designs

  if (.not. restart) then
    use_x0 = .true.
    call initial_designs(dv, objval, fevals, objfunc, xmin, xmax, use_x0, x0,  &
                         pso_options%feasible_init, pso_options%feasible_limit,&
                         pso_options%feasible_init_attempts)
  end if

!$omp master
  
 ! Set up or read other initialization data

  if (.not. restart) then

!   Initial velocities which may be positive or negative

    call random_number(vel)
    vel = 2.d0*maxspeed*(vel - 0.5d0)

!   Matrix of best designs for each particle and vector of their values

    bestdesigns = dv
    minvals = objval

!   Global and local best so far

    fmin = f0
    mincurr = minval(objval,1)
    fminloc = minloc(objval,1)
    xopt = dv(:,fminloc)
  
!   Counters
  
    step = 0
    designcounter = 0

!   Inertial parameter

    wcurr = whigh

  else

!   Read restart data from file

    call pso_read_restart(step, designcounter, dv, objval, vel, speed,         &
                          bestdesigns, minvals, wcurr)

!   Global and local best so far

    fmin = minval(minvals,1)
    fminloc = minloc(minvals,1)
    xopt = bestdesigns(:,fminloc)
    mincurr = minval(objval,1)

  end if

! Write initial design-values to file
  call pso_write_particlefile(particleunit, dv, vel)
  
! Open file for writing iteration history

  iunit = 17
  new_history_file = .false.
  if (step == 0) then
    new_history_file = .true.
  else
    open(unit=iunit, file='optimization_history.dat', status='old',            &
         position='append', iostat=ioerr)
    if (ioerr /= 0) then
      write(*,*) 
      write(*,*) "Warning: did not find existing optimization_history.dat file."
      write(*,*) "A new one will be written, but old data will be lost."
      write(*,*)
      new_history_file = .true.
    end if
  end if
  if (new_history_file) then
    open(unit=iunit, file='optimization_history.dat', status='replace')
    if (pso_options%relative_fmin_report) then
      write(iunit,'(A)') "Iteration  Objective function  "//&
                         "% Improvement over seed  Design radius"
    else
      write(iunit,'(A)') "Iteration  Objective function  Design radius"
    end if
    flush(iunit)
  end if

! Begin optimization

  restartcounter = 1
  converged = .false.
  write(*,*) 'Particle swarm optimization progress:'

!$omp end master
!$omp barrier

  optimization_loop: do while (.not. converged)

!$omp master

!   Increase iteration counter

    step = step + 1

!$omp end master
!$omp barrier

!$omp do

!   Update each particle's position, evaluate objective function, etc.

    do i = 1, pso_options%pop

!     Impose speed limit

      if (speed(i) > maxspeed) then
        vel(:,i) = maxspeed*vel(:,i)/speed(i)
      end if

!     Update position and bring back to side constraints if necessary

      dv(:,i) = dv(:,i) + vel(:,i)
      do j = 1, nconstrained
        var = constrained_dvs(j)
        if (dv(var,i) < xmin(var)) then
          dv(var,i) = xmin(var)
          call random_number(speed(i))
          vel(var,i) = -speed(i)*vel(var,i)
        elseif (dv(var,i) > xmax(var)) then
          dv(var,i) = xmax(var)
          call random_number(speed(i))
          vel(var,i) = -speed(i)*vel(var,i)
        end if
      end do

!     Evaluate objective function and update local best design if appropriate

      objval(i) = objfunc(dv(:,i))
      if (objval(i) < minvals(i)) then
        minvals(i) = objval(i)
        bestdesigns(:,i) = dv(:,i)
      end if

    end do

!$omp end do

!$omp master

!   Update best overall design, if appropriate

    mincurr = minval(objval,1)
    fminloc = minloc(objval,1)
    if (mincurr < fmin) then
      xopt = dv(:,fminloc)
      fmin = mincurr
      signal_progress = .true.
    else
      signal_progress = .false.
    end if

!$omp end master
!$omp barrier

!$omp do

!   Update velocity of each particle

    do i = 1, pso_options%pop
      call random_number(randvec1)
      call random_number(randvec2)
      vel(:,i) = wcurr*vel(:,i) + c1*randvec1*(bestdesigns(:,i) - dv(:,i)) +   &
                                  c2*randvec2*(xopt - dv(:,i))
      speed(i) = norm_2(vel(:,i))
    end do

!$omp end do

!$omp master

!   Reduce inertial parameter

    wcurr = wcurr - convrate*(wcurr - wlow)

!   Display progress 

    radius = design_radius(dv)
    write (*,*)
    write(*,'(1x,A,I5)'   , advance ='no') 'Iteration: ', step
    write(*,'(A20,ES10.3)', advance ='no') 'Design radius: ', radius
    write(*,'(A30,F9.6)'  , advance ='no') 'Objective function: ', fmin
    if (pso_options%relative_fmin_report) then
      write(*,'(A)', advance ='no') '  --> '
      write(outstring,'(F9.6,A1)') (f0 - fmin)/f0*100.d0, '%'
      if (signal_progress) then 
        call print_colored (COLOR_GOOD,   trim(outstring))
      else 
        call print_colored (COLOR_NORMAL, trim(outstring))
      end if
    end if 
    write (*,*)

!   Write design to file if requested
!   converterfunc is an optional function supplied to convert design variables
!     into something more useful.  If not supplied, the design variables
!     themselves are written to a file.

    if ( (signal_progress) .and. (pso_options%write_designs) ) then
      designcounter = designcounter + 1
      if (present(converterfunc)) then
        stat = converterfunc(xopt, designcounter)
      else
        call write_design('particleswarm_designs.dat', 'old', xopt,            &
                          designcounter)
      end if
    end if

!   Write iteration history

    write(stepchar,'(I11)') step
    write(fminchar,'(F14.10)') fmin
    write(radchar,'(ES14.6)') radius
    if (pso_options%relative_fmin_report) then
      write(relfminchar,'(F14.10)') (f0 - fmin)/f0*100.d0
      write(iunit,'(A11,A20,A25,A20)') adjustl(stepchar), adjustl(fminchar),   &
                                       adjustl(relfminchar), adjustl(radchar)
    else
      write(iunit,'(A11,2A20)') adjustl(stepchar), adjustl(fminchar),          &
                                adjustl(radchar)
    end if
    flush(iunit)
    
!   Evaluate convergence

    if ( (radius > pso_options%tol) .and. (step < pso_options%maxit) ) then
      converged = .false.
    else
      converged = .true.
      stop_reason = "completed"
      if (step == pso_options%maxit) then
        write(*,*) 'Warning: PSO optimizer forced to exit due to the max number'
        write(*,*) '         of iterations being reached.'
      end if
    end if 

!   Write restart file if appropriate and update restart counter

    if (restartcounter == restart_write_freq) then
      call pso_write_restart(step, designcounter, dv, objval, vel, speed,      &
                             bestdesigns, minvals, wcurr)
      restartcounter = 1
    else
      restartcounter = restartcounter + 1
    end if

! Write particle-values to file
    call pso_write_particlefile(particleunit, dv, vel)

!   Check for commands in run_control file

    call read_run_control(commands, ncommands)
    do k = 1, ncommands
      if (trim(commands(k)) == "stop") then
        converged = .true.
        stop_reason = "stop_requested"
        write(*,*) 'Cleaning up: stop command encountered in run_control.'
      end if
    end do

!$omp end master
!$omp barrier

  end do optimization_loop

!$omp end parallel

! Calculate number of function evaluations
      
  fevals = fevals + step*pso_options%pop

! Close iteration history file

  close(iunit)

 ! Close particle file
  call pso_close_particlefile(particleunit)

! Write restart at end of optimization

  if ((restartcounter /= 1) .and. (restart_write_freq /=0))                    &
    call pso_write_restart(step, designcounter, dv, objval, vel, speed,        &
                           bestdesigns, minvals, wcurr)

end subroutine particleswarm

!=============================================================================80
!
! Particle swarm restart write routine
!
!=============================================================================80
subroutine pso_write_restart(step, designcounter, dv, objval, vel, speed,      &
                             bestdesigns, minvals, wcurr)

  use vardef, only : output_prefix

  integer, intent(in) :: step, designcounter
  double precision, dimension(:,:), intent(in) :: dv, vel, bestdesigns
  double precision, dimension(:), intent(in) :: objval, speed, minvals
  double precision, intent(in) :: wcurr

  character(100) :: restfile
  integer :: iunit
  
! Status notification

  restfile = 'restart_pso_'//trim(output_prefix)
  write(*,*) '  Writing PSO restart data to file '//trim(restfile)//' ...'

! Open output file for writing

  iunit = 13
  open(unit=iunit, file=restfile, status='replace', form='unformatted')
  
! Write restart data

  write(iunit) step
  write(iunit) designcounter
  write(iunit) dv
  write(iunit) objval
  write(iunit) vel
  write(iunit) speed
  write(iunit) bestdesigns
  write(iunit) minvals
  write(iunit) wcurr

! Close restart file

  close(iunit)

! Status notification

  write(*,*) '  Successfully wrote PSO restart file.'

end subroutine pso_write_restart

!=============================================================================80
!
! Particle swarm restart read routine
! 
!=============================================================================80
subroutine pso_read_restart(step, designcounter, dv, objval, vel, speed,       &
                            bestdesigns, minvals, wcurr)

  use vardef, only : output_prefix

  integer, intent(out) :: step, designcounter
  double precision, dimension(:,:), intent(inout) :: dv, vel, bestdesigns
  double precision, dimension(:), intent(inout) :: objval, speed, minvals
  double precision, intent(out) :: wcurr

  character(100) :: restfile
  integer :: iunit, ioerr

! Status notification

  restfile = 'restart_pso_'//trim(output_prefix)
  write(*,*) 'Reading PSO restart data from file '//trim(restfile)//' ...'

! Open output file for reading

  iunit = 13
  open(unit=iunit, file=restfile, status='old', form='unformatted',            &
       iostat=ioerr)
  if (ioerr /= 0) then
    write(*,*) 'Error: could not find input file '//trim(restfile)//'.'
    write(*,*)
    stop
  end if
  
! Read restart data

  read(iunit) step
  read(iunit) designcounter
  read(iunit) dv
  read(iunit) objval
  read(iunit) vel
  read(iunit) speed
  read(iunit) bestdesigns
  read(iunit) minvals
  read(iunit) wcurr

! Close restart file

  close(iunit)

! Status notification

  write(*,*) 'Successfully read PSO restart data.'
  write(*,*)

end subroutine pso_read_restart
 

!=============================================================================80
!
! Opens a particle file
! WORK IN PROGRESS
!=============================================================================80
subroutine pso_open_particlefile(write_particlefile, particleunit)
  
  logical, intent(in) ::write_particlefile
  integer, intent(inout) :: particleunit
  character(100) :: particlefile
  integer :: ioerr

  if (write_particlefile) then

    ! Set particle file name and identifiers
    particleunit = 20
    particlefile = 'particles.csv'
    write(*,*) "particleswarm: writing particle-values to file "//               &
               trim(particlefile)//" ..."
    open(unit=particleunit, file=particlefile, status='replace', iostat=ioerr)
    if (ioerr /= 0) then
      write(*,*) "Error, file-open particles.csv failed !"
      particleunit = 0
      return
    end if
  else
    particleunit = 0
  end if

  end subroutine pso_open_particlefile

!=============================================================================80
!
! Writes particle values to file
! WORK IN PROGRESS
!=============================================================================80
subroutine pso_write_particlefile(particleunit, dv, vel)

  double precision, dimension(:,:), intent(in) :: dv, vel
  integer, intent(inout) :: particleunit
  integer:: nvars, pop, count1, count2

if (particleunit == 20) then
!TODO MB particle
  nvars = size(dv,1)
  pop = size(dv,2)
  do count1 = 1, pop
    do count2 = 1, nvars
      ! Write all particle-values without linefeed 
      write(particleunit,'(2F12.6)', advance='NO') dv(count2,count1)
      write(particleunit, '(A)', advance='NO') ";"
      write(particleunit,'(2F12.6)', advance='NO') vel(count2,count1)
      !Separator between particle-values
      write(particleunit, '(A)', advance='NO') ";"
    end do
    ! Separator between particles
    write(particleunit, '(A)', advance='NO') ";"
  end do
  ! Write linefeed
  write(particleunit, *) ""
end if

end subroutine pso_write_particlefile

!=============================================================================80
! 
! Closes a particle file
! WORK IN PROGRESS
!=============================================================================80
subroutine pso_close_particlefile(particleunit)

  integer, intent(inout) :: particleunit
     
  ! Close file
  if (particleunit == 20) then
    flush(particleunit)
    close(particleunit)
    particleunit = 0
  end if
    
  end subroutine pso_close_particlefile

end module particle_swarm

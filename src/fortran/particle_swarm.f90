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

  use os_util 

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

    integer :: max_retries = 1           ! max. number of retries a single 
                                         ! particle tries to get a valid geometry
    logical :: auto_retry = .true.       ! do auto retry of a single 
                                         ! particle tries to get a valid geometry
    integer :: auto_frequency = 100      ! #how often should auto_retry be tested 

    double precision :: c1 = 0d0         ! particle-best trust factor
    double precision :: c2 = 0d0         ! swarm-best trust factor
    double precision :: whigh = 0d0      ! starting inertial parameter
    double precision :: wlow = 0d0       ! ending inertial parameter
    double precision :: convrate = 0d0   ! inertial parameter reduction rate

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
                         designcounter,           &
                         stop_reason, converterfunc)

  use math_deps,         only : norm_2
  use optimization_util, only : init_random_seed, initial_designs,             &
                                design_radius, write_design, read_run_control
  use airfoil_evaluation, only : OBJ_XFOIL_FAIL, OBJ_GEO_FAIL
  use vardef,            only : design_subdir


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
  logical, intent(in) :: given_f0_ref
  type (pso_options_type), intent(in) :: pso_options
  integer, intent(out) :: designcounter
  character(14), intent(out) :: stop_reason

  optional :: converterfunc
  interface
    integer function converterfunc(x, designcounter_dummy)
      double precision, dimension(:), intent(in) :: x
      integer, intent(in) :: designcounter_dummy
    end function
  end interface

  integer :: nconstrained, i, j, fminloc, var, stat, iunit,    &
             ioerr, k, ncommands, particleunit
  double precision :: c1, c2, whigh, wlow, convrate, maxspeed, wcurr, mincurr, &
                      f0, radius
  double precision, dimension(pso_options%pop) :: objval, minvals, speed
  double precision, dimension(size(xmin,1)) :: randvec1, randvec2
  double precision, dimension(size(xmin,1),pso_options%pop) :: dv, vel,        &
                                                               bestdesigns
  logical :: use_x0, converged, signal_progress, new_history_file
  character(11) :: stepchar
  character(20) :: fminchar, radchar, text
  character(25) :: relfminchar
  character(80) :: histfile
  character(80), dimension(20) :: commands
  integer       :: i_retry, max_retries, ndone                      
  
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

!    c1 = 2.0d0         ! particle-best trust factor
!    c2 = 1.0d0         ! swarm-best trust factor
!    whigh = 1.5d0      ! starting inertial parameter
!    wlow = 0.7d0       ! ending inertial parameter
!    convrate = 0.03d0  ! inertial parameter reduction rate

  else if (trim(pso_options%convergence_profile) == "quick_camb_thick") then

    c1 = 1.0d0         ! particle-best trust factor
    c2 = 1.6d0         ! swarm-best trust factor
    whigh = 1.2d0      ! starting inertial parameter
    wlow = 0.02d0       ! ending inertial parameter
!    convrate = 0.01d0   ! inertial parameter reduction rate
    convrate = 0.025d0   ! inertial parameter reduction rate

  else
    write(*,*) "Error in particleswarm: convergence mode should be"//          &
               "'exhaustive' or 'quick' or 'quick_camb_thick'."
    stop
  end if

! experimental: to allow direct manipulation of parms in inputs 
  if (pso_options%c1 > 0d0)       c1        = pso_options%c1
  if (pso_options%c2 > 0d0)       c2        = pso_options%c2
  if (pso_options%whigh > 0d0)    whigh     = pso_options%whigh
  if (pso_options%wlow > 0d0)     wlow      = pso_options%wlow
  if (pso_options%convrate > 0d0) convrate  = pso_options%convrate
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

!$omp parallel default(shared) private(i, j, var, i_retry)

! Initialize a random seed

  call init_random_seed()

! Set up initial designs

  use_x0 = .true.
  call initial_designs(dv, objval, fevals, objfunc, xmin, xmax, use_x0, x0,  &
                        pso_options%feasible_init, pso_options%feasible_limit,&
                        pso_options%feasible_init_attempts)

!$omp master
  
! Set up initialization data


! Initial velocities which may be positive or negative

  call random_number(vel)
  vel = 2.d0*maxspeed*(vel - 0.5d0)

! Matrix of best designs for each particle and vector of their values

  bestdesigns = dv
  minvals = objval

! Global and local best so far

  fmin = f0
  mincurr = minval(objval,1)
  fminloc = minloc(objval,1)
  xopt = dv(:,fminloc)
  
! Counters
  
  step = 0
  designcounter = 0

! Inertial parameter

  wcurr = whigh



! Write initial design-values to file
  call pso_write_particlefile(particleunit, dv, vel)
  
! Open file for writing iteration history

  histfile  = trim(design_subdir)//'Optimization_History.dat'

  iunit = 17
  new_history_file = .false.
  if (step == 0) then
    new_history_file = .true.
  else
    open(unit=iunit, file=trim(histfile), status='old',            &
         position='append', iostat=ioerr)
    if (ioerr /= 0) then
      write(*,*) 
      write(*,*) "Warning: did not find "//trim(histfile)//" file."
      write(*,*) "A new one will be written, but old data will be lost."
      write(*,*)
      new_history_file = .true.
    end if
  end if
  if (new_history_file) then
    open(unit=iunit, file=trim(histfile), status='replace')
    if (pso_options%relative_fmin_report) then
      write(iunit,'(A)') "Iteration  % Improvement over seed  Design radius"
    else
      write(iunit,'(A)') "Iteration  Objective function  Design radius"
    end if
    flush(iunit)
  end if

! Begin optimization

  converged = .false.

  max_retries = initial_max_retries (pso_options) 

  write(*,*)
  write(*,*)
  call  print_colored (COLOR_FEATURE, ' - Particle swarm ')
  write (text,'(I3)') pso_options%pop
  call  print_colored (COLOR_NORMAL, 'with '//trim(adjustl(text))// ' members will now try its best ...')
  write(*,*)
  write(*,*)
  call show_optimization_header  (pso_options, max_retries)

!$omp end master
!$omp barrier

  optimization_loop: do while (.not. converged)

!$omp master

!   Increase iteration counter

    step = step + 1

    if (pso_options%auto_retry) &
      max_retries = auto_max_retries (pso_options, step, max_retries, objval) 

    call show_iteration_header (step, max_retries)


!$omp end master
    ndone= 0
!$omp barrier

!   Use OMP DYNAMIC (and not STATIC which is default) so every thread will take a new "i" 
!   when it finished its task. In summary this is much faster as calculation time differs
!   very much depending if a xfoil calculation will be made or not (geo constraints!)      
!$omp do SCHEDULE(DYNAMIC)

!   Update each particle's position, evaluate objective function, etc.

    do i = 1, pso_options%pop

!     Impose speed limit
      if (speed(i) > maxspeed) then
        vel(:,i) = maxspeed*vel(:,i)/speed(i)
      end if

      i_retry = 0

!     Evaluate objecte function - if this returns a geometry violation retry to get a valid
!                                 evaluation 

      do                            ! ... max_retries  

        ! Update position and bring back to side constraints if necessary
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

  !     Evaluate objective function
        objval(i) = objfunc(dv(:,i))

  !     Valid result --> proceed
        if (objval(i) < OBJ_GEO_FAIL) exit

!        if (i_retry >= pso_options%max_retries) exit
        if (i_retry >= max_retries) exit

        i_retry = i_retry + 1

  !     Invalid result - particles violated geometry - try again with new velocity
   
        dv(:,i) = dv(:,i) - vel(:,i)
        call random_number(speed(i))
        vel(:,i) = speed(i) * vel(:,i) 
  
      end do 

!     Display some info about success of single particle 
      !call show_particle_info (fmin,  minvals(i), objval(i))        

!$OMP ATOMIC  
      ndone = ndone + 1
      call show_particles_progress (pso_options%pop, ndone)

    end do    !--------------------------------------------------------------------------

!$omp end do

!$omp master

!   Display some info about success of single particle 

    call show_particles_info (fmin, minvals, objval)        

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

    ! Update  best design if appropriate
      if (objval(i) < minvals(i)) then
        minvals(i) = objval(i)
        bestdesigns(:,i) = dv(:,i)
      end if

      call random_number(randvec1)
      call random_number(randvec2)

    ! The incredible Particle Swarm formula ...
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

    call show_iteration_result (radius, fmin, f0, signal_progress, & 
                                pso_options%relative_fmin_report)

!   Write design to file if requested
!   converterfunc is an optional function supplied to convert design variables
!     into something more useful.  If not supplied, the design variables
!     themselves are written to a file.

    if ( (signal_progress) .and. (pso_options%write_designs) ) then
      designcounter = designcounter + 1
      if (present(converterfunc)) then
        stat = converterfunc(xopt, designcounter)
      else
        call write_design('particleswarm_designs.dat', 'old', xopt, designcounter)
      end if
    else
      write (*,*)
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
        write (*,*)
        call print_warning ('PSO optimizer stopped due to the max number of iterations being reached.')
      end if
    end if 

!   Write particle-values to file
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


end subroutine particleswarm

 

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


!------------------------------------------------------------------------------
! Gets the inital value for particle max:retries depending on input parms
!------------------------------------------------------------------------------

  function initial_max_retries  (pso_options)

    type (pso_options_type), intent(in) :: pso_options
    integer            :: initial_max_retries          
  
    if (pso_options%auto_retry) then 
      initial_max_retries = pso_options%max_retries
    else
      initial_max_retries = pso_options%max_retries
    end if
  
  end function initial_max_retries
  
!------------------------------------------------------------------------------
! Adopt max_retries of particles according to iteration steps and 
!   percentage of geometry failed particles 
!------------------------------------------------------------------------------

function auto_max_retries (pso_options, step, cur_max_retries, objval) 

  use airfoil_evaluation, only : OBJ_GEO_FAIL

  type (pso_options_type), intent(in) :: pso_options
  integer             :: auto_max_retries
  integer, intent(in) :: step, cur_max_retries
  double precision, dimension(:), intent(in) :: objval

  integer          :: ngeo_fails 
  integer          :: nparticles, i
  double precision :: fail_percentage

  auto_max_retries = cur_max_retries

  if (mod(step, pso_options%auto_frequency) /= 0) then      ! new value only when frequency
    return        
  end if 

  ! Count no of geometry fails of particles 
  ngeo_fails = 0
  nparticles = size(objval) 
  do i = 1, nparticles
    if (objval(i) >= OBJ_GEO_FAIL) ngeo_fails = ngeo_fails + 1    ! no valid design  
  end do 

  fail_percentage = 100d0 * ngeo_fails / nparticles 

  !write (*,'(/,A,F4.0,A,/)') '---- New Auto_retry at ', fail_percentage,'%'
  if (fail_percentage > 75d0) then 
    auto_max_retries = pso_options%max_retries
  elseif (fail_percentage < 50d0) then 
    auto_max_retries = 0
  end if
  
end function auto_max_retries


!------------------------------------------------------------------------------
! Shows user info - header of optimization out 
!------------------------------------------------------------------------------

subroutine  show_optimization_header  (pso_options, max_retries)

  type (pso_options_type), intent(in) :: pso_options
  integer, intent(in) :: max_retries           

  logical            :: show_improvement, auto_retry
  integer            :: nparticles           
  character(200)     :: blanks = ' '
  character (1)      :: s1
  character (5)      :: s5
  character(:), allocatable     :: var_string

  show_improvement = pso_options%relative_fmin_report
  auto_retry       = pso_options%auto_retry
  nparticles       = pso_options%pop

  write(*,'(3x)', advance = 'no')
  call  print_colored (COLOR_NOTE, "Particle result:  '")
  call  print_colored (COLOR_GOOD, "+")
  call  print_colored (COLOR_NOTE, "' new swarm best  '+' personal best"//&
                                   "  '-' not better  '")
  call  print_colored (COLOR_ERROR, "x")
  call  print_colored (COLOR_NOTE, "' xfoil no conv  ' ' geometry failed")
  write (*,*)

  write (s1,'(I1)') max_retries
  if (auto_retry ) then                                       
    write(*,'(/,3x)', advance = 'no')
    call print_colored (COLOR_NOTE, "Auto retry ")
    call print_colored (COLOR_NOTE, "of particle having failed geometry - starting with retry="//s1//"")
    write (s5,'(I5)') pso_options%auto_frequency

    call print_colored (COLOR_NOTE, " for next "//trim(adjustl(s5))// " iterations.")
    write (*,*)
  elseif (max_retries > 0 ) then                                       
    write(*,'(/,3x)', advance = 'no')
    call  print_colored (COLOR_NOTE, "Retry of a particle having failed geometry (retry="//s1//")")
    write (*,*)
  end if
  write(*,*)

  var_string = 'Particles result' // blanks (len('Particles result') : nparticles)
  write(*,'(3x,A6,3x, A, A,          1x,A6,   5x)', advance ='no') &
          'Iterat','Progress   ', var_string,'Radius'
  
  if (show_improvement) then
    write(*,'(A11)') 'Improvement'
  else
    write(*,'(A9)') 'Objective'
    write (*,*)
  end if

end subroutine show_optimization_header


!------------------------------------------------------------------------------
! Shows user info about result of a single iteration 
!------------------------------------------------------------------------------

subroutine  show_iteration_header (step, max_retries)

  integer, intent(in)          :: step, max_retries
  character (1) :: s1

  write(*,'(3x,I5,A1)', advance ='no') step, ':'

  if (max_retries == 0) then 
    write (*,'(1x,2x)',    advance ='no') 
  else
    write (s1,'(I1)') max_retries
    call print_colored (COLOR_NOTE,  ' r'//s1)
  end if


end subroutine  show_iteration_header

!------------------------------------------------------------------------------
! Shows user info about result of a single iteration 
!------------------------------------------------------------------------------

subroutine  show_iteration_result (radius, fmin, f0, improved, show_improvement)

  double precision, intent(in)  :: radius ,fmin, f0 
  logical, intent(in)           :: show_improvement, improved
  character(25)                 :: outstring

  write (outstring,'(ES9.1)') radius
  if (improved) then 
    call  print_colored (COLOR_NORMAL, trim(outstring))
  else
    call  print_colored (COLOR_NOTE,   trim(outstring))
  end if

  if (show_improvement) then
    write (outstring,'(SP, 3x, F9.5,A1)') (f0 - fmin)/f0*100.d0, '%'
    if (improved) then 
      call  print_colored (COLOR_GOOD, trim(outstring))
    else 
      call  print_colored (COLOR_NOTE, trim(outstring))
    end if
  else
    write (outstring,'(3x,F9.6,A1)') fmin
    if (improved) then 
      call  print_colored (COLOR_NORMAL, trim(outstring))
    else
      call  print_colored (COLOR_NOTE,   trim(outstring))
    end if
  end if 

end subroutine  show_iteration_result


!------------------------------------------------------------------------------
! Shows user info about sucess of a single particle
!------------------------------------------------------------------------------

subroutine  show_particle_info (overall_best, personal_best, objval)

  use airfoil_evaluation, only : OBJ_XFOIL_FAIL, OBJ_GEO_FAIL

  double precision, intent(in)  :: overall_best, personal_best, objval
  integer       :: color 
  Character (1) :: sign 

  if (objval < overall_best) then 
    color = COLOR_GOOD                           ! better then current best
    sign  = '+'
  else if (objval == OBJ_XFOIL_FAIL) then     
    color = COLOR_ERROR                          ! no xfoil convergence
    sign  = 'x'
  else if (objval < personal_best) then 
    color = COLOR_NOTE                         ! best of particle up to now
    sign  = '+'
  else if (objval >= OBJ_GEO_FAIL) then 
    color = COLOR_NOTE                           ! no valid design 
    sign  = '.'
  else  
    color = COLOR_NOTE                         ! no improvement
    sign  = '-'
  end if 

  call print_colored (color, sign)            
  
end subroutine show_particle_info


!------------------------------------------------------------------------------
! Shows user info about sucess of a single particle
!------------------------------------------------------------------------------

subroutine  show_particles_info (overall_best, personal_best, objval)

  use airfoil_evaluation, only : OBJ_XFOIL_FAIL, OBJ_GEO_FAIL

  double precision, intent(in)  :: overall_best
  double precision, dimension (:), intent(in)  :: personal_best, objval
  integer       :: color, i, ibest 
  Character (1) :: sign 

  call print_colored (COLOR_NOTE, ' ')

  ibest = minloc(objval,1)

  do i = 1, size(objval)
    if (objval(i) < overall_best .and. i == ibest) then 
      color = COLOR_GOOD                           ! better then current best
      sign  = '+'
    else if (objval(i) == OBJ_XFOIL_FAIL) then     
      color = COLOR_ERROR                          ! no xfoil convergence
      sign  = 'x'
    else if (objval(i) < personal_best(i)) then 
      color = COLOR_NOTE                         ! best of particle up to now
      sign  = '+'
    else if (objval(i) >= OBJ_GEO_FAIL) then 
      color = COLOR_NOTE                           ! no valid design 
      sign  = ' '
    else  
      color = COLOR_NOTE                         ! no improvement
      sign  = '-'
    end if 

    call print_colored (color, sign)            

  end do 
  
end subroutine show_particles_info


!------------------------------------------------------------------------------
! Shows user info about progress of particles work 
!------------------------------------------------------------------------------

subroutine  show_particles_progress (nparticles, ndone)

  integer, intent(in)  :: nparticles, ndone

  if (mod(ndone,(nparticles/10)) == 0) call print_colored (COLOR_NOTE, '.') 
  
end subroutine show_particles_progress

end module particle_swarm

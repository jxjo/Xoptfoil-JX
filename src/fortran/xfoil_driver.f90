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

module xfoil_driver

! Contains subroutines to use XFoil to analyze an airfoil

  implicit none

  type xfoil_options_type

    double precision :: ncrit          !Critical ampl. ratio
    double precision :: xtript, xtripb !Trip locations
    logical :: viscous_mode                       
    logical :: silent_mode             !Toggle xfoil screen write
    logical :: repanel                 ! = true (default) do re-paneling (PANGEN)
                                       ! before xfoil is called for aero calcs 
    logical :: show_details            ! show some user entertainment during xfoil loop
    integer :: maxit                   !Iterations for BL calcs
    double precision :: vaccel         !Xfoil BL convergence accelerator
    logical :: fix_unconverged         !Reinitialize to fix unconverged pts.
    logical :: reinitialize            !Reinitialize BLs at every operating
                                       !  point (recommended for optimization)

  end type xfoil_options_type

  type xfoil_geom_options_type   

    integer :: npan
    double precision :: cvpar, cterat, ctrrat, xsref1, xsref2, xpref1, xpref2

  end type xfoil_geom_options_type


  type value_statistics_type   
                                       ! for drag(lift) outlier detetction 
    logical :: no_check                ! deactivate detection e.g. when flaps are set
    integer :: nvalue                  ! total numer of values tested
    double precision :: minval         ! the smallest value up to now 
    double precision :: maxval         ! the biggest value up to now 
    double precision :: meanval        ! the average value up to now 

  end type value_statistics_type

  type (value_statistics_type), dimension(:), allocatable, private :: drag_statistics

  contains


!=============================================================================80
!
! Subroutine to smooth an airfoil using Xfoil's PANGEN subroutine
!
!=============================================================================80
subroutine smooth_paneling(foilin, npoint, foilout, opt_geom_options)

  use xfoil_inc
  use vardef, only : airfoil_type

  type(airfoil_type), intent(in) :: foilin
  integer, intent(in) :: npoint
  type(airfoil_type), intent(out) :: foilout
  type(xfoil_geom_options_type), intent(in), optional :: opt_geom_options

  type(xfoil_geom_options_type) :: geom_options
  integer :: i
  logical :: needs_cleanup

! Some things that need to be allocated for XFoil PANGEN

  needs_cleanup = .false.
  if (.not. allocated(W1)) then
    allocate(W1(6*IQX))
    allocate(W2(6*IQX))
    allocate(W3(6*IQX))
    allocate(W4(6*IQX))
    allocate(W5(6*IQX))
    allocate(W6(6*IQX))
    needs_cleanup = .true.
  end if

! Set some things that Xfoil may need to do paneling

  PI = 4.d0*atan(1.d0)
  HOPI = 0.5d0/PI
  QOPI = 0.25d0/PI
  SIG(:) = 0.d0
  NW = 0
  AWAKE = 0.d0
  LWDIJ = .false.
  LIPAN = .false.
  LBLINI = .false.
  WAKLEN = 1.d0
  GAM(:) = 0.d0
  SIGTE = 0.d0
  GAMTE = 0.d0
  SIGTE_A = 0.d0
  GAMTE_A = 0.d0
  SILENT_MODE = .TRUE.

  ! Set geometry options for output airfoil

  if (.not. present (opt_geom_options)) then 
    ! set xoptfoil standard values 
    geom_options%npan = npoint
    geom_options%cvpar = 1.d0
  ! jx-mod If set to geom_options%cterat = 0.15d0 the curvature at TE panel
  !     tends to flip away and have tripple value (bug in xfoil) 
  !     with a very small value the panel gets wider and the quality better
    geom_options%cterat = 0.0d0
    geom_options%ctrrat = 0.2d0
    geom_options%xsref1 = 1.d0
    geom_options%xsref2 = 1.d0
    geom_options%xpref1 = 1.d0
    geom_options%xpref2 = 1.d0
  else 
    geom_options = opt_geom_options
    ! npoint overwrites if set 
    if (npoint > 0) geom_options%npan = npoint
  end if 

! Set xfoil airfoil and paneling options

  call xfoil_set_airfoil(foilin)
  call xfoil_set_paneling(geom_options)

! Smooth paneling with PANGEN

  call PANGEN(.NOT. SILENT_MODE)

! Put smoothed airfoil coordinates into derived type

  foilout%npoint = geom_options%npan
  allocate(foilout%x(foilout%npoint))
  allocate(foilout%z(foilout%npoint))
  do i = 1, foilout%npoint
    foilout%x(i) = X(i)
    foilout%z(i) = Y(i)
  end do

! Deallocate memory that is not needed anymore

  if (needs_cleanup) then
    deallocate(W1)
    deallocate(W2)
    deallocate(W3)
    deallocate(W4)
    deallocate(W5)
    deallocate(W6)
  end if
  
end subroutine smooth_paneling

!=============================================================================80
!
! Subroutine to apply a flap deflection to the buffer airfoil and set it as the
! current airfoil.  For best results, this should be called after PANGEN.
!
!=============================================================================80
subroutine xfoil_apply_flap_deflection(xflap, yflap, y_flap_spec, degrees)

  use xfoil_inc
 
  double precision, intent(in) :: xflap, yflap, degrees
  character(3), intent(in) :: y_flap_spec
  
  integer y_flap_spec_int

  if (y_flap_spec == 'y/c') then
    y_flap_spec_int = 0
  else
    y_flap_spec_int = 1
  end if

! Apply flap deflection

  ! caution: FLAP will change y_flap a little --> ()
  call FLAP((xflap), (yflap), y_flap_spec_int, degrees)

end subroutine xfoil_apply_flap_deflection

!=============================================================================80
!
! Subroutine to get Cl, Cd, Cm for an airfoil from Xfoil at given operating
! conditions.  Reynolds numbers and mach numbers should be specified for each
! operating point.  Additionally, op_mode determines whether each point is run
! at a constant alpha or cl - use 'spec-al' for specified alpha and 'spec-cl'
! for specified cl.  
! 
! Outputs:
!   alpha, Cl, Cd, Cm each operating point
!   op_converged of viscous calculations each operating point
!
!=============================================================================80
subroutine run_xfoil(foil, geom_options, operating_points, op_modes,           &
                     re, ma, use_flap, x_flap, y_flap,                         &
                     y_flap_spec, flap_degrees, xfoil_options,                 &
                     op_converged, lift, drag, moment, alpha, xtrt, xtrb,      &
                     ncrit_per_point)

  use xfoil_inc
  use vardef,    only : airfoil_type, re_type
  use os_util,   only: print_colored, COLOR_WARNING, COLOR_NOTE, COLOR_ERROR


  type(airfoil_type), intent(in) :: foil
  type(xfoil_geom_options_type), intent(in) :: geom_options
  double precision, dimension(:), intent(in) :: operating_points,  flap_degrees
  type(re_type), dimension(:), intent(in) :: re, ma
  double precision, intent(in) :: x_flap, y_flap
  character(3), intent(in) :: y_flap_spec
  logical, intent(in) :: use_flap
  character(7), dimension(:), intent(in) :: op_modes
  type(xfoil_options_type), intent(in) :: xfoil_options
  double precision, dimension(size(operating_points,1)), intent(out) ::  & 
              lift, drag, moment, alpha, xtrt, xtrb
  logical, dimension(:), intent(out) :: op_converged
  double precision, dimension(:), intent(in), optional :: ncrit_per_point

  integer :: i, noppoint
  integer :: iretry, nretry
  double precision :: newpoint, prev_op_delta, op_delta, prev_flap_degree
  logical:: point_fixed, show_details, flap_changed

  lift(:)   = 0.d0
  drag(:)   = 0.d0
  moment(:) = 0.d0
  alpha(:)  = 0.d0
  xtrt(:)   = 0.d0
  xtrb(:)   = 0.d0
  op_converged (:) = .true.

  prev_op_delta = 0d0
  flap_changed = .false.
  prev_flap_degree = flap_degrees (1) 
  show_details = xfoil_options%show_details

  noppoint = size(operating_points,1)

! Check to make sure xfoil is initialized

  if (.not. allocated(AIJ)) then
    write(*,*) "Error: xfoil is not initialized!  Call xfoil_init() first."
    stop
  end if

! jx-mod init statistics for out lier detection the first time and when polar changes
  if (.not. allocated(drag_statistics)) then
    call init_statistics (size(operating_points,1), drag_statistics)
  else if (size(drag_statistics) /= size(operating_points,1)) then 
    call init_statistics (size(operating_points,1), drag_statistics)
  end if


! Set default Xfoil parameters
  call xfoil_defaults(xfoil_options)

! Set paneling options
  call xfoil_set_paneling(geom_options)


! Run xfoil for requested operating points
!
! Rules for initialization of xfoil boundary layer - xfoil_init_BL 
!
!   xfoil_options%reinitialize = 
!   .true.    init will bed one before *every* xfoil calculation 
!   .false.   init will be done only
!             - at the first op_point
!             - when the flap angle is changed (new foil is set)
!             - when a point didn't converge
!             - when the direction of alpha or cl changes along op points

  if (show_details) then 
    write (*,'(7x,A)',advance = 'no') 'Xfoil  '
    if (xfoil_options%repanel)      write (*,'(A)',advance = 'no') 'repanel '
    if (xfoil_options%reinitialize) write (*,'(A)',advance = 'no') 'init_BL '
  end if

  run_oppoints: do i = 1, noppoint

!   print newline if output gets too long
    if (show_details .and.( mod(i,25) == 0)) write (*,'(/,7x,A)',advance = 'no') '       '

!   if flpas are activated, check if the angle has changed to reinit foil

    if(use_flap .and. (flap_degrees(i) /= prev_flap_degree)) then
      flap_changed = .true.
      prev_flap_degree = flap_degrees(i)
    else
      flap_changed = .false.
    end if 

!   set airfoil, apply flap deflection, init BL if needed
    if (flap_changed .or. (i == 1)) then

      ! set airfoil into xfoil buffer
      call xfoil_set_airfoil(foil)              ! "restore" current airfoil

      ! apply flap only if set to non zero degrees
      if (flap_changed) then
        call xfoil_apply_flap_deflection(x_flap, y_flap, y_flap_spec, flap_degrees(i))
      end if     

      ! repanel geometry only if requested...
      if (xfoil_options%repanel) call PANGEN(.not. SILENT_MODE)
      ! In case of flaps (or first op) always init boundary layer 
      call xfoil_init_BL (show_details)

    else
      ! Init BL always if set in parameters 
      if (xfoil_options%reinitialize) then 
        call xfoil_init_BL (.false.)
      else
        ! Init BL if the direction of alpha or cl changes along op points
        op_delta = operating_points(i) - operating_points(i-1)
        if ((prev_op_delta * op_delta) < 0d0) then 
          call xfoil_init_BL (show_details)
        end if 
        prev_op_delta = op_delta
      end if
    end if


!   Support Type 1 and 2 re numbers  
    REINF1 = re(i)%number
    RETYP  = re(i)%type 
    MATYP  = ma(i)%type 
    call MINFSET(ma(i)%number)

!   Set compressibility parameters from MINF
    CALL COMSET

!   Set ncrit per point
    if (present(ncrit_per_point)) ACRIT = ncrit_per_point(i)


!   Now finally run xfoil at op_point
    call run_xfoil_op_point (op_modes(i), operating_points(i), xfoil_options%viscous_mode, &
                             xfoil_options%maxit, show_details, & 
                             op_converged(i), lift(i), drag(i), moment(i), alpha(i),xtrt(i),xtrb(i))


!   Handling of unconverged points
    if (op_converged(i)) then
      if (is_out_lier (drag_statistics(i), drag(i))) then
        op_converged(i) = .false.
        if (show_details) call print_colored (COLOR_WARNING, 'flip')
      else if (lift_changed (op_modes(i), operating_points(i), lift(i))) then
        op_converged(i) = .false.
        if (show_details) call print_colored (COLOR_WARNING, 'lift')
        if (show_details) write (*,'(A)',advance = 'no') 'lift'
      end if 
    end if

    if (.not. op_converged(i) .and. xfoil_options%fix_unconverged) then

      if (show_details) write (*,'(A)',advance = 'no') '['

!     Try to initialize BL at new point (in the direction away from stall)

      newpoint = operating_points(i) - 0.2d0*abs(operating_points(i))*sign(    &
                                                 1.d0, operating_points(i))
      if (newpoint == 0.d0) newpoint = 0.1d0

      ! always init BL to get this new point to start from for fix,,
      call xfoil_init_BL (show_details .and. (.not. xfoil_options%reinitialize))
      call run_xfoil_op_point (op_modes(i), newpoint, xfoil_options%viscous_mode, &
                               xfoil_options%maxit, show_details , & 
                               op_converged(i), lift(i), drag(i), moment(i), alpha(i),xtrt(i),xtrb(i))

!     Now try to run again at the old operating point increasing RE a little ...

      iretry = 1
      nretry = 3
      point_fixed = .false.

      do while (.not. point_fixed .and. (iretry <= nretry)) 

        if (xfoil_options%reinitialize) call xfoil_init_BL (.false.)

        call run_xfoil_op_point (op_modes(i), operating_points(i), xfoil_options%viscous_mode, &
                                 xfoil_options%maxit, show_details, & 
                                 op_converged(i), lift(i), drag(i), moment(i),alpha(i),xtrt(i),xtrb(i))
                              
        if (.not. op_converged(i)    & 
            .or. (is_out_lier (drag_statistics(i), drag(i)))  &
            .or. (lift_changed (op_modes(i), operating_points(i), lift(i)))) then 

        ! increase a little RE to converge and try again
          REINF1 =  REINF1 * 1.002d0
        ! Re-init the second try
          call xfoil_init_BL (show_details .and. (.not. xfoil_options%reinitialize))

        else 
          point_fixed = .true.
        end if 

        iretry = iretry + 1

      end do 

      if(show_details) then 
        write (*,'(A)',advance = 'no') ']'
        if (point_fixed) then 
          call print_colored (COLOR_NOTE, 'fixed')
        else
          call print_colored (COLOR_ERROR,  'x')
        end if  
      end if 

!     no fix achieved - reinit BL (for the next op) - set converged flag to .false.
      if(.not. point_fixed) then
        if (.not. xfoil_options%reinitialize) call xfoil_init_BL (show_details)
        op_converged(i) = .false.
      end if
    end if
    

  end do run_oppoints


! Print warnings about unconverged points
!        Update statistics

  if(show_details) write (*,*) 

  do i = 1, noppoint
    if (.not. is_out_lier (drag_statistics(i), drag(i))) then 
      call update_statistic (drag_statistics(i), drag(i))
    end if 

    ! jx-mod Support Type 1 and 2 re numbers - cl may not be negative  
    if ((re(i)%type == 2) .and. (lift(i) <= 0d0) .and. op_converged(i)) then 
      write (*,*)
      write(*,'(31x,A,I2,A, F6.2)') "Warning: Negative lift for Re-Type 2 at" // &
       " op",i," - cl:",lift(i)
    end if 

  end do

end subroutine run_xfoil


!===============================================================================
!
! Runs Xfoil at a specified op_point which is either
!  - at an angle of attack
!  - at an lift coefficient
!
! Assumes airfoil geometry, reynolds number, and mach number have already been 
! set in Xfoil.
!
!===============================================================================

subroutine run_xfoil_op_point (op_mode, op_point, viscous_mode,       &
                               maxit, show_details,                   &
                               converged, lift, drag, moment, alpha, xtrt, xtrb)

  use xfoil_inc
  use os_util, only: print_colored, COLOR_WARNING, COLOR_NORMAL


  character(7), intent(in)      :: op_mode  
  double precision, intent(in)  :: op_point
  logical, intent(in)           :: viscous_mode, show_details
  integer, intent(in)           :: maxit
  logical, intent(out)          :: converged
  double precision, intent(out) :: lift, drag, moment, alpha, xtrt, xtrb

  integer       :: niter_needed  
  character(20) :: outstring 

  ! Inviscid calculations for specified angle of attack

  if (trim(op_mode) == 'spec-al') then
    LALFA = .TRUE.
    ALFA = op_point * DTOR
    call SPECAL
  elseif (trim(op_mode) == 'spec-cl') then
    LALFA = .FALSE.
    ALFA = 0.d0
    CLSPEC = op_point
    call SPECCL
  else
    write(*,*)
    write(*,*) "Error in xfoil_driver: op_mode must be 'spec-al' or 'spec-cl'"
    write(*,*)
    stop
  end if

  if (abs(ALFA-AWAKE) .GT. 1.0E-5) LWAKE  = .false.
  if (abs(ALFA-AVISC) .GT. 1.0E-5) LVCONV = .false.
  if (abs(MINF-MVISC) .GT. 1.0E-5) LVCONV = .false.

  ! Viscous calculations (if requested)

  converged = .true. 

  if (viscous_mode) then 
    
    call VISCAL(maxit, niter_needed)

    ! coverged? 

    if (niter_needed > maxit) then 
      converged = .false.
    ! RMSBL equals to viscrms() formerly used...
    else if (.not. LVCONV .or. (RMSBL > 1.D-4)) then 
      converged = .false.
    else 
      converged = .true.
    end if

  end if
   
  ! Outputs

  lift = CL
  moment = CM
  alpha = ALFA/DTOR
  if (viscous_mode) then
    drag = CD
    xtrt  = XOCTR(1)
    xtrb  = XOCTR(2)
  else
    drag = CDP
  end if

  ! Final check for NaNs

  if (isnan(lift)) then
    lift = -1.D+08
    converged = .false.
  end if
  if (isnan(drag)) then
    drag = 1.D+08
    converged = .false.
  end if
  if (isnan(moment)) then
    moment = -1.D+08
    converged = .false.
  end if

  if(show_details) then 
    write (outstring,'(I4)') niter_needed
    if (converged) then
      call print_colored (COLOR_NORMAL,  ' ' // trim(adjustl(outstring)))
    else
      call print_colored (COLOR_WARNING, ' ' // trim(adjustl(outstring)))
    end if
  end if

end subroutine run_xfoil_op_point


!=============================================================================80
!
! Allocates xfoil variables that may be too big for the stack in OpenMP
!
!=============================================================================80
subroutine xfoil_init()

  use xfoil_inc

! Allocate variables that may be too big for the stack in OpenMP

  allocate(AIJ(IQX,IQX))
  allocate(BIJ(IQX,IZX))
  allocate(DIJ(IZX,IZX))
  allocate(CIJ(IWX,IQX))
  allocate(IPAN(IVX,ISX))
  allocate(ISYS(IVX,ISX))
  allocate(W1(6*IQX))
  allocate(W2(6*IQX))
  allocate(W3(6*IQX))
  allocate(W4(6*IQX))
  allocate(W5(6*IQX))
  allocate(W6(6*IQX))
  allocate(VTI(IVX,ISX))
  allocate(XSSI(IVX,ISX))
  allocate(UINV(IVX,ISX))
  allocate(UINV_A(IVX,ISX))
  allocate(UEDG(IVX,ISX))
  allocate(THET(IVX,ISX))
  allocate(DSTR(IVX,ISX))
  allocate(CTAU(IVX,ISX))
  allocate(MASS(IVX,ISX))
  allocate(TAU(IVX,ISX))
  allocate(DIS(IVX,ISX))
  allocate(CTQ(IVX,ISX))
  allocate(DELT(IVX,ISX))
  allocate(TSTR(IVX,ISX))
  allocate(USLP(IVX,ISX))
  allocate(VM(3,IZX,IZX))
  allocate(VA(3,2,IZX))
  allocate(VB(3,2,IZX))
  allocate(VDEL(3,2,IZX))

end subroutine xfoil_init

!=============================================================================80
!
! Initializes xfoil variables
!
!=============================================================================80
subroutine xfoil_defaults(xfoil_options)

  use xfoil_inc

  type(xfoil_options_type), intent(in) :: xfoil_options

  N = 0
  SILENT_MODE = xfoil_options%silent_mode
  PI = 4.d0*atan(1.d0)
  HOPI = 0.5d0/PI
  QOPI = 0.25d0/PI
  DTOR = PI/180.d0
  QINF = 1.d0
  SIG(:) = 0.d0
  QF0(:) = 0.d0
  QF1(:) = 0.d0
  QF2(:) = 0.d0
  QF3(:) = 0.d0
  NW = 0
  RETYP = 1
  MATYP = 1
  GAMMA = 1.4d0
  GAMM1 = GAMMA - 1.d0
  XCMREF = 0.25d0
  YCMREF = 0.d0
  LVISC = xfoil_options%viscous_mode
  AWAKE = 0.d0
  AVISC = 0.d0
  ITMAX = xfoil_options%maxit
  LWDIJ = .false.
  LIPAN = .false.
  LBLINI = .false.
  ACRIT = xfoil_options%ncrit
  IDAMP = 0
  XSTRIP(1) = xfoil_options%xtript
  XSTRIP(2) = xfoil_options%xtripb
  VACCEL = xfoil_options%vaccel
  WAKLEN = 1.d0
  PSIO = 0.d0
  GAMU(:,:) = 0.d0
  GAM(:) = 0.d0
  SIGTE = 0.d0
  GAMTE = 0.d0
  SIGTE_A = 0.d0
  GAMTE_A = 0.d0
  APANEL(:) = 0.d0

! Set boundary layer calibration parameters

  call BLPINI

end subroutine xfoil_defaults

!=============================================================================80
!
! Sets airfoil for xfoil into buffer and current airfoil
!
!=============================================================================80
subroutine xfoil_set_airfoil(foil)

  use xfoil_inc, only : XB, YB, NB, SB, XBP, YBP 
  use vardef,    only : airfoil_type
  type(airfoil_type), intent(in) :: foil

! Set foil into xfoil buffer foil
  NB = foil%npoint
  XB(1:NB) = foil%x
  YB(1:NB) = foil%z

  CALL SCALC(XB,YB,SB,NB)
  CALL SEGSPL(XB,XBP,SB,NB)
  CALL SEGSPL(YB,YBP,SB,NB)

! Also copy buffer airfoil to xfoil current foil. This is also made in PANGEN -
!        ... but PANGEN shouldn't always be called before xfoil calculations
  call ABCOPY (.true.)

end subroutine xfoil_set_airfoil


!=============================================================================80
!
! Sets xfoil paneling options
!
!=============================================================================80
subroutine xfoil_set_paneling(geom_options)

  use xfoil_inc, only : NPAN, CVPAR, CTERAT, CTRRAT, XSREF1, XSREF2, XPREF1,   &
                        XPREF2

  type(xfoil_geom_options_type), intent(in) :: geom_options

  NPAN = geom_options%npan
  CVPAR = geom_options%cvpar
  CTERAT = geom_options%cterat
  CTRRAT = geom_options%ctrrat
  XSREF1 = geom_options%xsref1
  XSREF2 = geom_options%xsref2
  XPREF1 = geom_options%xpref1
  XPREF2 = geom_options%xpref2
  
end subroutine xfoil_set_paneling

!=============================================================================80
!
! Deallocates memory in xfoil
!
!=============================================================================80
subroutine xfoil_cleanup()

  use xfoil_inc

! Deallocate variables

  deallocate(AIJ)
  deallocate(BIJ)
  deallocate(DIJ)
  deallocate(CIJ)
  deallocate(IPAN)
  deallocate(ISYS)
  deallocate(W1)
  deallocate(W2)
  deallocate(W3)
  deallocate(W4)
  deallocate(W5)
  deallocate(W6)
  deallocate(VTI)
  deallocate(XSSI)
  deallocate(UINV)
  deallocate(UINV_A)
  deallocate(UEDG)
  deallocate(THET)
  deallocate(DSTR)
  deallocate(CTAU)
  deallocate(MASS)
  deallocate(TAU)
  deallocate(DIS)
  deallocate(CTQ)
  deallocate(DELT)
  deallocate(TSTR)
  deallocate(USLP)
  deallocate(VM)
  deallocate(VA)
  deallocate(VB)
  deallocate(VDEL)

end subroutine xfoil_cleanup


!------------------------------------------------------------------------------
!
! jx-mod xfoil extensions
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Retrieve AMAX from Xfoil  
!     PANGEN or ABCOPY has to be done first to have the value calculated
!------------------------------------------------------------------------------

function xfoil_geometry_amax()

  use xfoil_inc, only : AMAX
  double precision :: xfoil_geometry_amax

  xfoil_geometry_amax = AMAX

end function xfoil_geometry_amax

!------------------------------------------------------------------------------
! Reset xfoil_driver e.g. for an new polar 
!------------------------------------------------------------------------------
subroutine xfoil_driver_reset ()

  if (allocated(drag_statistics))  deallocate (drag_statistics)

end subroutine xfoil_driver_reset 

!------------------------------------------------------------------------------
! Init Boundary layer of xfoil viscous calculation  
!------------------------------------------------------------------------------
subroutine xfoil_init_BL (show_details)

  use xfoil_inc, only : LIPAN, LBLINI
  use os_util, only: print_colored, COLOR_NOTE

  logical, intent(in) :: show_details

  LIPAN  = .false.
  LBLINI = .false.

  if(show_details) call print_colored (COLOR_NOTE, ' i')

end subroutine xfoil_init_BL 

!------------------------------------------------------------------------------
! Scale max thickness and camber and their positions of foil 
!        using xfoil THKCAM and HIPNT
!
!   f_thick  - scaling factor for thickness
!   d xthick - delta x for max thickness x-position
!   f_camb   - scaling factor for camber
!   d_xcamb  - delta x for max camber position
!
! 
! ** Note ** 
!
! Before calling this subroutine, "smooth_paneling()" (which uses xfoil PANGEN)
! should be done on foil to avoid strange artefacts at the leading edge.
! XFOIL>HIPNT (moving thickness highpoint) is very sensible and behaves badly
! if the LE curvature does not fit to the spline algorithm
!------------------------------------------------------------------------------
subroutine xfoil_scale_thickness_camber (infoil, f_thick, d_xthick, f_camb, d_xcamb, outfoil)

  use xfoil_inc, only : AIJ
  use vardef,    only : airfoil_type

  type(airfoil_type), intent(in)  :: infoil
  type(airfoil_type), intent(out) :: outfoil
  double precision, intent(in) :: f_thick, d_xthick, f_camb, d_xcamb
  double precision :: thick, xthick, camb, xcamb

! Check to make sure xfoil is initialized
  if (.not. allocated(AIJ)) then
    write(*,*) "Error: xfoil is not initialized!  Call xfoil_init() first."
    stop
  end if
! Set xfoil airfoil and prepare globals, get current thickness
  call xfoil_set_airfoil (infoil)
  call xfoil_get_geometry_info  (thick, xthick, camb, xcamb) 


! Run xfoil to change thickness and camber and positions

  IF ((d_xcamb /= 0.d0) .or. (d_xthick /= 0.d0))  &
    call HIPNT  (xcamb + d_xcamb, xthick + d_xthick)
  IF ((f_thick /= 1.d0) .or. (f_camb /= 1.d0))  &
    call THKCAM (f_thick, f_camb)

                
! retrieve outfoil from xfoil buffer

  call xfoil_reload_airfoil(outfoil)

end subroutine xfoil_scale_thickness_camber

!------------------------------------------------------------------------------
! Set max thickness and camber and their positions of foil 
!        using xfoil THKCAM and HIPNT
!
!   maxt  - new thickness
!   xmaxt - new max thickness x-position
!   maxc  - new camber
!   xmaxc - new max camber position
!
! 
! ** Note ** 
!
! Before calling this subroutine, "smooth_paneling()" (which uses xfoil PANGEN)
! should be done on foil to avoid strange artefacts at the leading edge.
! XFOIL>HIPNT (moving thickness highpoint) is very sensible and behaves badly
! if the LE curvature does not fit to the spline algorithm
!------------------------------------------------------------------------------
subroutine xfoil_set_thickness_camber (infoil, maxt, xmaxt, maxc, xmaxc, outfoil)

  use xfoil_inc, only : AIJ
  use vardef,    only : airfoil_type

  type(airfoil_type), intent(in)  :: infoil
  type(airfoil_type), intent(out) :: outfoil

  double precision, intent(in) :: maxt, xmaxt, maxc, xmaxc
  double precision :: CFAC,TFAC, thick, xthick, camb, xcamb

! Check to make sure xfoil is initialized
  if (.not. allocated(AIJ)) then
    write(*,*) "Error: xfoil is not initialized!  Call xfoil_init() first."
    stop
  end if

! Set xfoil airfoil and prepare globals, get current thickness
  call xfoil_set_airfoil (infoil)
  call xfoil_get_geometry_info  (thick, xthick, camb, xcamb) 

! Run xfoil to change thickness and camber 
  CFAC = 1.0
  TFAC = 1.0
  IF(camb .NE.0.0 .AND. maxc.NE.999.0) CFAC = maxc / camb
  IF(thick.NE.0.0 .AND. maxt.NE.999.0) TFAC = maxt / thick
  call THKCAM ( TFAC, CFAC)

! Run xfoil to change highpoint of thickness and camber 
  call HIPNT (xmaxc, xmaxt)

! Recalc values ...
  call xfoil_get_geometry_info (thick, xthick, camb, xcamb) 

! retrieve outfoil from xfoil buffer
  call xfoil_reload_airfoil(outfoil)

end subroutine xfoil_set_thickness_camber



!------------------------------------------------------------------------------
! Scale LE radius 
!        using xfoil LERAD
! In:
!   infoil      - foil to scale LE
!   f_radius    - scaling factor for LE radius
!   x_blend     - blending distance/c from LE
! Out:
!   new_radius  - new LE radius
!   outfoil     = modified foil
! 
! ** Note ** 
!
! Before calling this subroutine, "smooth_paneling()" (which uses xfoil PANGEN)
! should be done on foil to avoid strange artefacts at the leading edge.
!------------------------------------------------------------------------------
subroutine xfoil_scale_LE_radius (infoil, f_radius, x_blend, outfoil)

  use xfoil_inc, only : AIJ, RADBLE
  use vardef,    only : airfoil_type

  type(airfoil_type), intent(in)  :: infoil
  double precision, intent(in) :: f_radius, x_blend
  double precision  :: new_radius
  type(airfoil_type), intent(out) :: outfoil

! Check to make sure xfoil is initialized
  if (.not. allocated(AIJ)) then
    write(*,*) "Error: xfoil is not initialized!  Call xfoil_init() first."
    stop
  end if

! Set xfoil airfoil and prepare globals, get current thickness
  call xfoil_set_airfoil (infoil)

! Run xfoil to change thickness and camber and positions
  IF ((f_radius /= 1.d0))  call LERAD (f_radius,x_blend, new_radius) 

! Update xfoil globals
  RADBLE = new_radius

  call xfoil_reload_airfoil(outfoil)

end subroutine xfoil_scale_LE_radius


!-------------------------------------------------------------------------
! gets buffer airfoil thickness, camber .. positions
!-------------------------------------------------------------------------
subroutine xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc) 
 
  use xfoil_inc
  Real*8, intent(out) :: maxt, xmaxt, maxc, xmaxc
  Real*8 :: TYMAX
  
!--- find the current buffer airfoil camber and thickness
  CALL GETCAM(XCM,YCM,NCM,XTK,YTK,NTK,                  &
              XB,XBP,YB,YBP,SB,NB )
  CALL GETMAX(XCM,YCM,YCMP,NCM,xmaxc,maxc)
  CALL GETMAX(XTK,YTK,YTKP,NTK,xmaxt,TYMAX)

  maxt = 2.0 * TYMAX

end subroutine xfoil_get_geometry_info



!-------------------------------------------------------------------------
! Reloads airfoil from xfoil buffer foil
!-------------------------------------------------------------------------
subroutine xfoil_reload_airfoil(foil)

  use xfoil_inc, only : XB, YB, NB
  use vardef,    only : airfoil_type

  type(airfoil_type), intent(inout) :: foil

  if (allocated (foil%x))  deallocate (foil%x)
  if (allocated (foil%z))  deallocate (foil%z)
  allocate(foil%x(NB))
  allocate(foil%z(NB))

  foil%npoint = NB
  foil%x = XB(1:NB)
  foil%z = YB(1:NB)
  
end subroutine xfoil_reload_airfoil

!--JX-mod  --------------------------------------------------------------------
! 
!  Toolfunctions to handle out lier (flip) detection of drag and lift 
!
!------------------------------------------------------------------------------

subroutine init_statistics (npoints, value_statistics)

  type ( value_statistics_type), dimension (:), allocatable, intent (inout) :: value_statistics
  integer, intent (in) :: npoints 
  integer :: i
  
  if (allocated(value_statistics))  deallocate (value_statistics)

  allocate (value_statistics(npoints))
  do i = 1, npoints
    value_statistics(i)%nvalue   = 0
    value_statistics(i)%no_check = .false.
    value_statistics(i)%minval   = 0.d0
    value_statistics(i)%maxval   = 0.d0
    value_statistics(i)%meanval  = 0.d0
  end do 

end subroutine init_statistics
!------------------------------------------------------------------------------
subroutine update_statistic (value_statistic, new_value)

  type ( value_statistics_type), intent (inout) :: value_statistic
  doubleprecision, intent (in) :: new_value 
  
  value_statistic%minval  = min (value_statistic%minval, new_value) 
  value_statistic%maxval  = max (value_statistic%maxval, new_value)
  value_statistic%meanval = (value_statistic%meanval * value_statistic%nvalue + new_value) / &
                            (value_statistic%nvalue + 1)
  value_statistic%nvalue  = value_statistic%nvalue + 1

end subroutine update_statistic
!------------------------------------------------------------------------------
function is_out_lier (value_statistic, check_value)

  type ( value_statistics_type), intent (in) :: value_statistic
  doubleprecision, intent (in) :: check_value
  logical :: is_out_lier 
  doubleprecision :: out_lier_tolerance, value_tolerance

  is_out_lier = .false. 
  out_lier_tolerance = 0.4

  if(value_statistic%nvalue > 0 .and. (.not. value_statistic%no_check)) then           !do we have enough values to check? 

    value_tolerance    = abs(check_value - value_statistic%meanval)/max(0.0001d0, value_statistic%meanval) 
    is_out_lier = (value_tolerance > out_lier_tolerance )  

  end if 

end function is_out_lier
!------------------------------------------------------------------------------
subroutine show_out_lier (ipoint, value_statistic, check_value)

  type ( value_statistics_type), intent (in) :: value_statistic
  doubleprecision, intent (in) :: check_value
  integer, intent (in) :: ipoint

  write (*,'( 30x, A,A,I2,A,F8.6, A,F8.6)') 'Out lier - ', 'op', ipoint, ": ", check_value, & 
              '    meanvalue: ', value_statistic%meanval

end subroutine show_out_lier
!------------------------------------------------------------------------------

!----Check if lift has changed although it should be fix with spec_cl ---------
function lift_changed (op_mode, op_point, lift)

  doubleprecision, intent (in) :: op_point, lift
  character (*), intent (in) :: op_mode
  logical :: lift_changed

  if ((op_mode == 'spec_cl') .and. (abs(lift - op_point) > 0.01d0)) then
    lift_changed = .true.
  else
    lift_changed = .false.
  end if 

end function lift_changed
!------------------------------------------------------------------------------

end module xfoil_driver




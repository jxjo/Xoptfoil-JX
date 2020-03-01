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

  contains

!=============================================================================80
!
! Subroutine to smooth an airfoil using Xfoil's PANGEN subroutine
!
!=============================================================================80
subroutine smooth_paneling(foilin, npoint, foilout)

  use xfoil_inc
  use vardef, only : airfoil_type

  type(airfoil_type), intent(in) :: foilin
  integer, intent(in) :: npoint
  type(airfoil_type), intent(out) :: foilout
  
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

  geom_options%npan = npoint
  geom_options%cvpar = 1.d0
  geom_options%cterat = 0.15d0
  geom_options%ctrrat = 0.2d0
  geom_options%xsref1 = 1.d0
  geom_options%xsref2 = 1.d0
  geom_options%xpref1 = 1.d0
  geom_options%xpref2 = 1.d0

! Set xfoil airfoil and paneling options

  call xfoil_set_airfoil(foilin)
  call xfoil_set_paneling(geom_options)

! Smooth paneling with PANGEN

  call PANGEN(.NOT. SILENT_MODE)

! Put smoothed airfoil coordinates into derived type

  foilout%npoint = npoint
  allocate(foilout%x(npoint))
  allocate(foilout%z(npoint))
  do i = 1, npoint
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

  call FLAP(xflap, yflap, y_flap_spec_int, degrees)

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
!   viscrms: rms for viscous calculations (check for convergence)
!
!=============================================================================80
subroutine run_xfoil(foil, geom_options, operating_points, op_modes,           &
                     reynolds_numbers, mach_numbers, use_flap, x_flap, y_flap, &
                     y_flap_spec, flap_degrees, xfoil_options, lift, drag,     &
                     moment, viscrms, alpha, xtrt, xtrb, ncrit_per_point)

  use xfoil_inc
  use vardef,    only : airfoil_type

  type(airfoil_type), intent(in) :: foil
  type(xfoil_geom_options_type), intent(in) :: geom_options
  double precision, dimension(:), intent(in) :: operating_points,              &
                                                reynolds_numbers, mach_numbers,&
                                                flap_degrees
  double precision, intent(in) :: x_flap, y_flap
  character(3), intent(in) :: y_flap_spec
  logical, intent(in) :: use_flap
  character(7), dimension(:), intent(in) :: op_modes
  type(xfoil_options_type), intent(in) :: xfoil_options
  double precision, dimension(size(operating_points,1)), intent(out) :: lift,  &
                                                           drag, moment, viscrms
  double precision, dimension(size(operating_points,1)), intent(out),          &
                                                   optional :: alpha, xtrt, xtrb
  double precision, dimension(:), intent(in), optional :: ncrit_per_point

  integer :: i, noppoint
  logical, dimension(size(operating_points,1)) :: point_converged, point_fixed 
  double precision :: newpoint
  character(30) :: text
  character(150) :: message

  if (.not. xfoil_options%silent_mode) then
    write(*,*) 
    write(*,*) 'Analyzing aerodynamics using the XFOIL engine ...'
  end if 

! Check to make sure xfoil is initialized

  if (.not. allocated(AIJ)) then
    write(*,*) "Error: xfoil is not initialized!  Call xfoil_init() first."
    stop
  end if

! Set default Xfoil parameters

  call xfoil_defaults(xfoil_options)

  point_converged(:) = .true.
  point_fixed(:) = .false.

  noppoint = size(operating_points,1)

! Set paneling options

  call xfoil_set_paneling(geom_options)

! Set airfoil and smooth paneling

  if (.not. use_flap) then
    call xfoil_set_airfoil(foil)
    call PANGEN(.not. SILENT_MODE)
  end if

! Run xfoil for requested operating points

  lift(:) = 0.d0
  drag(:) = 0.d0
  moment(:) = 0.d0
  viscrms(:) = 0.d0

! Run xfoil for requested operating points

  run_oppoints: do i = 1, noppoint

!   Reset airfoil, smooth paneling, and apply flap deflection

    if (use_flap) then
      call xfoil_set_airfoil(foil)
      call PANGEN(.not. SILENT_MODE)
      call xfoil_apply_flap_deflection(x_flap, y_flap, y_flap_spec,            &
                                       flap_degrees(i))
    end if

    REINF1 = reynolds_numbers(i)
    call MINFSET(mach_numbers(i))

    if (xfoil_options%reinitialize) then
      LIPAN = .false.
      LBLINI = .false.
    end if

!   Set compressibility parameters from MINF

    CALL COMSET

!   Set ncrit per point

    if (present(ncrit_per_point)) ACRIT = ncrit_per_point(i)

    if (op_modes(i) == 'spec-al') then

      call xfoil_specal(operating_points(i), xfoil_options%viscous_mode,       &
                        xfoil_options%maxit, lift(i), drag(i), moment(i))

    elseif (op_modes(i) == 'spec-cl') then

      call xfoil_speccl(operating_points(i), xfoil_options%viscous_mode,       &
                        xfoil_options%maxit, lift(i), drag(i), moment(i))

    else

      write(*,*)
      write(*,*) "Error in xfoil_driver: op_mode must be 'spec-al' or 'spec-cl'"
      write(*,*)
      stop

    end if

!   Get optional outputs

    if (present(alpha)) alpha(i) = ALFA/DTOR
    if (present(xtrt)) xtrt(i) = XOCTR(1)
    if (present(xtrb)) xtrb(i) = XOCTR(2)

!   Handling of unconverged points

    if (xfoil_options%viscous_mode .and. .not. LVCONV .and.                    &
        xfoil_options%fix_unconverged) then

      point_converged(i) = .false.

!     Try to initialize BL at new point (in the direction away from stall)

      newpoint = operating_points(i) - 0.5d0*abs(operating_points(i))*sign(    &
                                                 1.d0, operating_points(i))
      if (newpoint == 0.d0) newpoint = 0.1d0

      LIPAN = .false.
      LBLINI = .false.
      if (op_modes(i) == 'spec-al') then
        call xfoil_specal(newpoint, xfoil_options%viscous_mode,                & 
                          xfoil_options%maxit, lift(i), drag(i), moment(i))
      else
        call xfoil_speccl(newpoint, xfoil_options%viscous_mode,                & 
                          xfoil_options%maxit, lift(i), drag(i), moment(i))
      end if

!     Now try to run again at the old operating point

      if (op_modes(i) == 'spec-al') then
        call xfoil_specal(operating_points(i), xfoil_options%viscous_mode,     &
                          xfoil_options%maxit, lift(i), drag(i), moment(i))
      else
        call xfoil_speccl(operating_points(i), xfoil_options%viscous_mode,     &
                          xfoil_options%maxit, lift(i), drag(i), moment(i))
      end if

      if (LVCONV) point_fixed(i) = .true.

      if (present(alpha)) alpha(i) = ALFA/DTOR
      if (present(xtrt)) xtrt(i) = XOCTR(1)
      if (present(xtrb)) xtrb(i) = XOCTR(2)

    end if

!   Convergence check

    viscrms(i) = RMSBL

  end do run_oppoints

! Final check for NaNs

  do i = 1, noppoint
    if (isnan(lift(i))) then
      lift(i) = -1.D+08
      viscrms(i) = 1.D+08
    end if
    if (isnan(drag(i))) then
      drag(i) = 1.D+08
      viscrms(i) = 1.D+08
    end if
    if (isnan(moment(i))) then
      moment(i) = -1.D+08
      viscrms(i) = 1.D+08
    end if
    if (isnan(viscrms(i))) then
      viscrms(i) = 1.D+08
    end if
  end do

! Print warnings about unconverged points

  if (.not. xfoil_options%silent_mode) then

    write(*,*)

    do i = 1, noppoint
  
      write(text,*) i
      text = adjustl(text)
  
      if (point_converged(i)) then
        message = 'Operating point '//trim(text)//' converged.'
      elseif (.not. point_converged(i) .and. point_fixed(i)) then
        message = 'Operating point '//trim(text)//' initially did not '//      &
                  'converge but was fixed.'
      elseif (.not. point_converged(i) .and. .not. point_fixed(i)) then
        message = 'Operating point '//trim(text)//' initially did not '//      &
                  'converge and was not fixed.'
      end if
  
      write(*,*) trim(message)
  
    end do
  end if

end subroutine run_xfoil

!=============================================================================80
!
! Runs Xfoil at a specified angle of attack
! Assumes airfoil geometry, reynolds number, and mach number have already been 
! set in Xfoil.
!
!=============================================================================80
subroutine xfoil_specal(angle_of_attack, viscous_mode, maxit, lift, drag,      &
                        moment)

  use xfoil_inc

  double precision, intent(in) :: angle_of_attack
  logical, intent(in) :: viscous_mode
  integer, intent(in) :: maxit
  double precision, intent(out) :: lift, drag, moment

! Inviscid calculations for specified angle of attack

  LALFA = .TRUE.
  ALFA = angle_of_attack*DTOR
  call SPECAL
  if (abs(ALFA-AWAKE) .GT. 1.0E-5) LWAKE  = .false.
  if (abs(ALFA-AVISC) .GT. 1.0E-5) LVCONV = .false.
  if (abs(MINF-MVISC) .GT. 1.0E-5) LVCONV = .false.

! Viscous calculations (if requested)

  if (viscous_mode) call VISCAL(maxit)

! Outputs

  lift = CL
  moment = CM
  if (viscous_mode) then
    drag = CD
  else
    drag = CDP
  end if

end subroutine xfoil_specal

!=============================================================================80
!
! Runs Xfoil at a specified lift coefficient
! Assumes airfoil geometry, reynolds number, and mach number have already been 
! set in Xfoil.
!
!=============================================================================80
subroutine xfoil_speccl(cl_spec, viscous_mode, maxit, lift, drag, moment)

  use xfoil_inc

  double precision, intent(in) :: cl_spec
  logical, intent(in) :: viscous_mode
  integer, intent(in) :: maxit
  double precision, intent(out) :: lift, drag, moment

! Inviscid calculations for specified lift coefficient

  LALFA = .FALSE.
  ALFA = 0.d0
  CLSPEC = cl_spec
  call SPECCL
  if (abs(ALFA-AWAKE) .GT. 1.0E-5) LWAKE  = .false.
  if (abs(ALFA-AVISC) .GT. 1.0E-5) LVCONV = .false.
  if (abs(MINF-MVISC) .GT. 1.0E-5) LVCONV = .false.

! Viscous calculations (if requested)

  if (viscous_mode) call VISCAL(maxit)

! Outputs

  lift = CL
  moment = CM
  if (viscous_mode) then
    drag = CD
  else
    drag = CDP
  end if

end subroutine xfoil_speccl

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
! Sets airfoil for xfoil
!
!=============================================================================80
subroutine xfoil_set_airfoil(foil)

  use xfoil_inc, only : XB, YB, NB
  use vardef,    only : airfoil_type

  type(airfoil_type), intent(in) :: foil

  NB = foil%npoint
  XB(1:NB) = foil%x
  YB(1:NB) = foil%z

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

!=============================================================================80
!
! Gets thickness and camber information for the current airfoil
!
!=============================================================================80
subroutine xfoil_geometry_info(maxt, xmaxt, maxc, xmaxc)

  use xfoil_inc, only : THICKB, XTHICKB, CAMBR, XCAMBR

  double precision, intent(out) :: maxt, xmaxt, maxc, xmaxc

  maxt = THICKB
  xmaxt = XTHICKB
  maxc = CAMBR
  xmaxc = XCAMBR 

end subroutine xfoil_geometry_info


!------------------------------------------------------------------------------
!
! jx-mod xfoil extensions
!
!------------------------------------------------------------------------------

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
  call xfoil_set_buffer_airfoil (infoil)
  call xfoil_get_geometry_info  (thick, xthick, camb, xcamb) 


! Run xfoil to change thickness and camber and positions

  IF ((f_thick /= 1.d0) .or. (f_camb /= 1.d0))  &
    call THKCAM (f_thick, f_camb)
  IF ((d_xcamb /= 0.d0) .or. (d_xthick /= 0.d0))  &
    call HIPNT  (xcamb + d_xcamb, xthick + d_xthick)

! Recalc values ...
  call xfoil_get_geometry_info  (thick, xthick, camb, xcamb) 
  
!  WRITE(*,1010) f_thick, d_xthick, f_camb, d_xcamb
!1010 FORMAT(/' -tmp print- Fac thickness = ',F7.4,'  dx   = ',F6.3, &
!                      '    Fac camber    = ',F7.4,'  dx   = ',F6.3)

!  WRITE(*,1000) thick, xthick, camb, xcamb
!1000 FORMAT( ' -tmp print- New thickness = ',F7.4,'  at x = ',F6.3, &
!                      '    New camber    = ',F7.4,'  at x = ',F6.3)
                   
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
  call xfoil_set_buffer_airfoil (infoil)
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

  WRITE(*,1000) thick, xthick, camb, xcamb
1000 FORMAT(/' --tmp print --  New thickness = ',F7.4,'  at x = ',F6.3, &
                   '    New camber    = ',F7.4,'  at x = ',F6.3)

! retrieve outfoil from xfoil buffer
  call xfoil_reload_airfoil(outfoil)

end subroutine xfoil_set_thickness_camber


!-------------------------------------------------------------------------
! Sets buffer airfoil for xfoil - initalize segments etc...
!-------------------------------------------------------------------------
subroutine xfoil_set_buffer_airfoil (foil)

  use xfoil_inc, only : XB, YB, NB, SB, XBP, YBP 
  use vardef,    only : airfoil_type
  type(airfoil_type), intent(in) :: foil

  NB = foil%npoint
  XB(1:NB) = foil%x
  YB(1:NB) = foil%z

  CALL SCALC(XB,YB,SB,NB)
  CALL SEGSPL(XB,XBP,SB,NB)
  CALL SEGSPL(YB,YBP,SB,NB)

end subroutine xfoil_set_buffer_airfoil



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

  allocate(foil%x(NB))
  allocate(foil%z(NB))
  foil%npoint = NB
  foil%x = XB(1:NB)
  foil%z = YB(1:NB)
  
end subroutine xfoil_reload_airfoil



!------------------------------------------------------------------------------
end module xfoil_driver




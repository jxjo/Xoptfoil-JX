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

module parametrization

! Contains subroutines to create an airfoil shape from design variables

  implicit none

! Shape functions for creating airfoil shapes (top and bottom)

  double precision, dimension(:,:), pointer :: top_shape_function
  double precision, dimension(:,:), pointer :: bot_shape_function

!$omp threadprivate(top_shape_function)
!$omp threadprivate(bot_shape_function)

  contains

!=============================================================================80
!
! Allocates memory for shape functions
!
!=============================================================================80
subroutine allocate_shape_functions(nmodest, nmodesb, npointst, npointsb)

  integer, intent(in) :: nmodest, nmodesb, npointst, npointsb

  allocate(top_shape_function(nmodest,npointst))
  allocate(bot_shape_function(nmodesb,npointsb))

end subroutine allocate_shape_functions

!=============================================================================80
!
! Deallocates memory for shape functions
!
!=============================================================================80
subroutine deallocate_shape_functions

  deallocate(top_shape_function)
  deallocate(bot_shape_function)

end subroutine deallocate_shape_functions

!=============================================================================80
!
! Creates shape functions for top and bottom surfaces
! shapetype may be 'naca', 'camb-thick', 'camb-thick-plus' or 'hicks-henne'
! For Hicks-Henne shape functions, number of elements in modes must be a 
! multiple of 3.
!=============================================================================80
subroutine create_shape_functions(xtop, xbot, modestop, modesbot, shapetype,   &
                                  first_time)

  double precision, dimension(:), intent(in) :: xtop, xbot, modestop, modesbot
  character(*), intent(in) :: shapetype
  logical, intent(in) :: first_time

  integer :: nmodestop, nmodesbot, ntop, nbot

  ntop = size(xtop,1)
  nbot = size(xbot,1)

  if (trim(shapetype) == 'naca') then
    nmodestop = size(modestop,1)
    nmodesbot = size(modesbot,1)
  else if ((trim(shapetype) == 'camb-thick') .or. &
           (trim(shapetype) == 'camb-thick-plus')) then
    nmodestop = size(modestop,1)
    nmodesbot = 0
  else
    nmodestop = size(modestop,1)/3
    nmodesbot = size(modesbot,1)/3
  end if

  if (first_time) then

!   Allocate shape functions

    call allocate_shape_functions(nmodestop, nmodesbot, ntop, nbot)

!   Initialize shape functions

    top_shape_function(:,:) = 0.d0
    bot_shape_function(:,:) = 0.d0

  end if

  if ((.not. first_time) .or. (trim(shapetype) == 'naca')) then

!   Create shape functions for top

    call create_shape(xtop, modestop, shapetype, top_shape_function)

!   Create shape functions for bottom

    call create_shape(xbot, modesbot, shapetype, bot_shape_function)

  end if

end subroutine create_shape_functions

!=============================================================================80
!
! Populates shape function arrays
! For Hicks-Hene shape functions, number of elements in modes must be a 
! multiple of 3.
! For camber-thickness shape functions, number of elements in modes must be a 
! multiple of 4, there is only 1 shape-function with 4 parameters.
!
!=============================================================================80
subroutine create_shape(x, modes, shapetype, shape_function)

  use vardef, only : initial_perturb, min_bump_width

  double precision, dimension(:), intent(in) :: x, modes
  character(*), intent(in) :: shapetype
  double precision, dimension(:,:), intent(inout) :: shape_function

  integer :: npt, nmodes, i, j, counter1, counter2
  double precision :: power1, power2, dvscale, st, t1, t2, t1fact, t2fact, pi
  double precision :: chord, xle, xs

  npt = size(x,1)
  chord = x(npt) - x(1)
  xle = x(1)

  shape_switch: if (trim(shapetype) == 'naca') then

    nmodes = size(modes,1)

!   Create naca shape functions

    do j = 1, npt
      xs = (x(j)-xle)/chord
      shape_function(1,j) = sqrt(xs) - xs
    end do

    counter1 = 1
    counter2 = 1

    do i = 2, nmodes

!     Whole-powered shapes

      if (counter2 == 1) then

        power1 = dble(counter1)
        do j = 1, npt
          xs = (x(j)-xle)/chord
          shape_function(i,j) = xs**(power1)*(1.d0 - xs)
        end do
        counter2 = 2

!     Fractional-powered shapes

      else

        power1 = 1.d0/dble(counter1 + 2)
        power2 = 1.d0/dble(counter1 + 1)
        do j = 1, npt
          xs = (x(j)-xle)/chord
          shape_function(i,j) = xs**power1 - xs**power2
        end do
        counter2 = 1
        counter1 = counter1 + 1
       
      end if

    end do

!   Normalize shape functions

    do i = 1, nmodes
      dvscale = 1.d0/abs(maxval(shape_function(i,:)))
      shape_function(i,:) = shape_function(i,:)*dvscale
    end do

  elseif (trim(shapetype) == 'hicks-henne') then
      
    nmodes = size(modes,1)/3
    t1fact = initial_perturb/(1.d0 - 0.001d0)
    t2fact = initial_perturb/(10.d0 - min_bump_width)
    pi = acos(-1.d0)

    do i = 1, nmodes

!     Extract strength, bump location, and width

      counter1 = 3*(i-1)
      st = modes(counter1+1)
      t1 = modes(counter1+2)/t1fact       ! location
      t2 = modes(counter1+3)/t2fact       ! width

!     Check for problems with bump location and width parameters

      if (t1 <= 0.d0) t1 = 0.001d0      
      if (t1 >= 1.d0) t1 = 0.999d0
      if (t2 <= 0.d0) t2 = 0.001d0

!     Create shape function

      power1 = log10(0.5d0)/log10(t1)
      do j = 2, npt-1
        xs = (x(j)-xle)/chord
        shape_function(i,j) = st*sin(pi*xs**power1)**t2

      end do

    end do

    !write (*,'(1x,4(A7,F10.6),A)')  &
    !'meanloc ', meanloc, &
    !'meanwid ', meanwid, &
    !'meanxmi ', meanxmi, &
    !'meanxma ', meanxma, &
    !' '  

  else

    write(*,*)
    write(*,*) 'Shape function '//trim(shapetype)//' not recognized.'
    write(*,*)
    stop

  end if shape_switch

end subroutine create_shape

!=============================================================================80
!
! Creates an airfoil surface by perturbing an input "seed" airfoil
!
!=============================================================================80
subroutine create_airfoil(xt_seed, zt_seed, xb_seed, zb_seed, modest, modesb,  &
                          zt_new, zb_new, shapetype, symmetrical)

  double precision, dimension(:), intent(in) :: xt_seed, zt_seed, xb_seed,     &
                                                zb_seed
  double precision, dimension(:), intent(in) :: modest, modesb
  double precision, dimension(:), intent(inout) :: zt_new, zb_new
  character(*), intent(in) :: shapetype
  logical, intent(in) :: symmetrical

  integer :: i, nmodest, nmodesb, npointst, npointsb
  double precision :: strength

  if (trim(shapetype) == 'naca') then
    nmodest = size(modest,1)
    nmodesb = size(modesb,1)
  else if ((trim(shapetype) == 'camb-thick') .or. &
           (trim(shapetype) == 'camb-thick-plus')) then
    nmodest = size(modest,1)
    nmodesb = 0
  else
    nmodest = size(modest,1)/3
    nmodesb = size(modesb,1)/3
  end if
  npointst = size(zt_seed,1)
  npointsb = size(zb_seed,1)

! Create shape functions for Hicks-Henne

  if (trim(shapetype) == 'hicks-henne') then
    call create_shape_functions(xt_seed, xb_seed, modest, modesb, shapetype,   &
                                first_time=.false.)
  end if

! Top surface

  zt_new = zt_seed
  do i = 1, nmodest
    if ((trim(shapetype) == 'naca') .or. &
        (trim(shapetype) == 'camb-thick') .or. &
        (trim(shapetype) == 'camb-thick-plus')) then
      strength = modest(i)
    else
      strength = 1.d0
    end if
    zt_new = zt_new + strength*top_shape_function(i,1:npointst)
  end do

! Bottom surface

  if (.not. symmetrical) then
    zb_new = zb_seed
    do i = 1, nmodesb
      if (trim(shapetype) == 'naca') then
        strength = modesb(i)
      else
        strength = 1.d0   ! Hicks-Henne: strength is part of the shape function
      end if
      zb_new = zb_new + strength*bot_shape_function(i,1:npointsb)
    end do

! For symmetrical airfoils, just mirror the top surface

  else
    do i = 1, npointsb
      zb_new(i) = -zt_new(i)
    end do
  end if

end subroutine create_airfoil



!=============================================================================
! jx-mod routines for the new shapeType to modify thickness and camber 


!-----------------------------------------------------------------------------
!
! Modify thickness and camber and their positions of 
!   the seed foil defined by xt_seed, zt_seed, xb_seed, zb_seed
!   to the new values defined in modes 
!
! Returns the new foil defined by zt_new, zb_new
!-------------------------------------------------------------------------------
subroutine create_airfoil_camb_thick (xt_seed, zt_seed, xb_seed, zb_seed, modes, &
                                      zt_new, zb_new )

  use vardef,       only : airfoil_type
  use xfoil_driver, only : xfoil_scale_thickness_camber, xfoil_scale_LE_radius
                                   
  double precision, dimension(:), intent(in) :: xt_seed, zt_seed, xb_seed, zb_seed
  double precision, dimension(:), intent(in) :: modes
  double precision, dimension(:), intent(inout) :: zt_new, zb_new
  
  integer :: i,  nptt, nptb
  type(airfoil_type) :: seed_foil, new_foil_1, new_foil_2
  double precision :: f_thick,d_xthick,f_camb,d_xcamb
  double precision :: f_radius, x_blend 

! Rebuild seed airfoil out of top and bottom coordinates
  nptt = size(zt_seed,1)
  nptb = size(zb_seed,1)

  seed_foil%npoint = nptt + nptb - 1  
  allocate(seed_foil%x(seed_foil%npoint))
  allocate(seed_foil%z(seed_foil%npoint))

  do i = 1, nptt
    seed_foil%x(i) = xt_seed(nptt-i+1)
    seed_foil%z(i) = zt_seed(nptt-i+1)
  end do
  do i = 1, nptb-1
    seed_foil%x(i+nptt) = xb_seed(i+1)
    seed_foil%z(i+nptt) = zb_seed(i+1)
  end do

! Change thickness, camber ... according to new values hidden in modes
  f_camb   = 1.d0 + 10.d0 * modes(1) 
  f_thick  = 1.d0 + 5.d0 * modes(2)
  d_xcamb  = 4.d0 * modes(3)
  d_xthick = 4.d0 * modes(4)

  call xfoil_scale_thickness_camber (seed_foil, f_thick,d_xthick,f_camb,d_xcamb, new_foil_1)

  ! Change LE radius ... according to new values hidden in modes
  f_radius = 1.d0 + 3.d0 * modes(5)
  x_blend  = max (0.02d0, (5.d0 * modes(6) + 0.1d0))
  call xfoil_scale_LE_radius (new_foil_1, f_radius, x_blend, new_foil_2)

  ! Sanity check - new_foil may not have different number of points
  if (seed_foil%npoint /= new_foil_2%npoint) then
    write(*,'(A)') 'Error: Number of points changed during thickness/camber modification'
    stop 1
  end if

! get new upper and lower z-coordinates from modified airfoil 
  do i = 1, nptt
    zt_new(i) = new_foil_2%z(nptt-i+1)      ! start from LE - top reverse - to LE
  end do
  do i = 1, nptb 
    zb_new(i) = new_foil_2%z(nptt+i-1)      ! start from LE - bottom - to TE
  end do

! Clean up
  deallocate(seed_foil%x)
  deallocate(seed_foil%z)
  deallocate(new_foil_1%x)
  deallocate(new_foil_1%z)
  deallocate(new_foil_2%x)
  deallocate(new_foil_2%z)
  
end subroutine create_airfoil_camb_thick

!-----------------------------------------------------------------------------
!
! Modify thickness and camber and their positions of 
!   the seed foil defined by xt_seed, zt_seed, xb_seed, zb_seed
!   to the new values defined in modes 
!
! Top and Bottom are treated sperately
!
! Returns the new foil defined by zt_new, zb_new
!-------------------------------------------------------------------------------
subroutine create_airfoil_camb_thick_plus (xt_seed, zt_seed, xb_seed, zb_seed, modes, &
  zt_new, zb_new )

use vardef,       only : airfoil_type
use xfoil_driver, only : xfoil_scale_thickness_camber, xfoil_scale_LE_radius

double precision, dimension(:), intent(in) :: xt_seed, zt_seed, xb_seed, zb_seed
double precision, dimension(:), intent(in) :: modes
double precision, dimension(:), intent(inout) :: zt_new, zb_new

integer :: i,  nptt, nptb
type(airfoil_type) :: seed_foil, new_foil_1, new_foil_2, new_foil_3, new_foil_4
double precision :: f_thick,d_xthick,f_camb,d_xcamb
double precision :: f_radius, x_blend 

! Rebuild seed airfoil out of top and bottom coordinates
nptt = size(zt_seed,1)
nptb = size(zb_seed,1)

seed_foil%npoint = nptt + nptb - 1  
allocate(seed_foil%x(seed_foil%npoint))
allocate(seed_foil%z(seed_foil%npoint))

do i = 1, nptt
seed_foil%x(i) = xt_seed(nptt-i+1)
seed_foil%z(i) = zt_seed(nptt-i+1)
end do
do i = 1, nptb-1
seed_foil%x(i+nptt) = xb_seed(i+1)
seed_foil%z(i+nptt) = zb_seed(i+1)
end do

! Top: Change thickness, camber ... according to new values hidden in modes
f_camb   = 1.d0 + 10.d0 * modes(1) 
f_thick  = 1.d0 + 5.d0 * modes(2)
d_xcamb  = 4.d0 * modes(3)
d_xthick = 4.d0 * modes(4)

call xfoil_scale_thickness_camber (seed_foil, f_thick,d_xthick,f_camb,d_xcamb, new_foil_1)

! Change LE radius ... according to new values hidden in modes
f_radius = 1.d0 + 3.d0 * modes(5)
! max() will deliver the bigger value of both arguments, so the minimum possible value
! in this case is 0.005 / 0,5 percent
x_blend  = max (0.005, modes(6)) 


call xfoil_scale_LE_radius (new_foil_1, f_radius, x_blend, new_foil_2)

! Sanity check - new_foil may not have different number of points
if (seed_foil%npoint /= new_foil_2%npoint) then
write(*,'(A)') 'Error: Number of points changed during thickness/camber modification'
stop 1
end if

! Bottom: Change thickness, camber ... according to new values hidden in modes
f_camb   = 1.d0 + 10.d0 * modes(7) 
f_thick  = 1.d0 + 5.d0 * modes(8)
d_xcamb  = 4.d0 * modes(9)
d_xthick = 4.d0 * modes(10)

call xfoil_scale_thickness_camber (seed_foil, f_thick,d_xthick,f_camb,d_xcamb, new_foil_3)

! Change LE radius ... according to new values hidden in modes
f_radius = 1.d0 + 3.d0 * modes(11)
x_blend  = max (0.005, modes(6)) 
call xfoil_scale_LE_radius (new_foil_1, f_radius, x_blend, new_foil_4)

! Sanity check - new_foil may not have different number of points
if (seed_foil%npoint /= new_foil_4%npoint) then
write(*,'(A)') 'Error: Number of points changed during thickness/camber modification'
stop 1
end if


! get new upper and lower z-coordinates from modified airfoil 
do i = 1, nptt
zt_new(i) = new_foil_2%z(nptt-i+1)      ! start from LE - top reverse - to LE
end do
do i = 1, nptb 
zb_new(i) = new_foil_4%z(nptt+i-1)      ! start from LE - bottom - to TE
end do

! Clean up
deallocate(seed_foil%x)
deallocate(seed_foil%z)
deallocate(new_foil_1%x)
deallocate(new_foil_1%z)
deallocate(new_foil_2%x)
deallocate(new_foil_2%z)
deallocate(new_foil_3%x)
deallocate(new_foil_3%z)
deallocate(new_foil_4%x)
deallocate(new_foil_4%z)

end subroutine create_airfoil_camb_thick_plus

end module parametrization

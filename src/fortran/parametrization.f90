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

  use os_util
 
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
! shapetype may be 'camb-thick', 'camb-thick-plus' or 'hicks-henne'
! For Hicks-Henne shape functions, number of elements in modes must be a 
! multiple of 3.
!=============================================================================80
subroutine create_shape_functions(xtop, xbot, modestop, modesbot, shapetype,   &
                                  first_time)

  double precision, dimension(:), intent(in) :: xtop, xbot, modestop, modesbot
  character(*), intent(in) :: shapetype
  logical, intent(in) :: first_time

  integer :: nmodestop, nmodesbot, ntop, nbot

  if ((trim(shapetype) == 'camb-thick') .or. &
      (trim(shapetype) == 'camb-thick-plus')) then
    nmodestop = size(modestop,1)
    nmodesbot = 0
    ntop = 1                                      ! dummy 
    nbot = 1
  elseif (trim(shapetype) == 'bezier') then 
    nmodestop = size(modestop,1)
    nmodesbot = size(modesbot,1)
    ntop = 1                                      ! dummy
    nbot = 1
  else
    nmodestop = size(modestop,1)/3
    nmodesbot = size(modesbot,1)/3
    ntop = size(xtop,1)
    nbot = size(xbot,1)
    end if

  if (first_time) then

!   Allocate shape functions
    ! #todo  for bezier and camb-thick only the size of shape functions is needed 
    !        - they do not use shape functions approach ... 
    call allocate_shape_functions(nmodestop, nmodesbot, ntop, nbot)

!   Initialize shape functions
    top_shape_function(:,:) = 0.d0
    bot_shape_function(:,:) = 0.d0

  end if

  if (.not. first_time) then

!   Only Hicks-Henne
!   Create shape functions for top and bot 
    call create_shape(xtop, modestop, shapetype, top_shape_function)
    call create_shape(xbot, modesbot, shapetype, bot_shape_function)

  end if

end subroutine create_shape_functions



!=============================================================================80
!
! Populates shape function arrays
! For Hicks-Hene shape functions, number of elements in modes must be a 
! multiple of 3.
! For camber-thickness and bezier this is dummy 
!
!=============================================================================80
subroutine create_shape(x, modes, shapetype, shape_function)

  use vardef, only : initial_perturb, min_bump_width

  double precision, dimension(:), intent(in) :: x, modes
  character(*), intent(in) :: shapetype
  double precision, dimension(:,:), intent(inout) :: shape_function

  integer :: npt, nmodes, i, j, counter1
  double precision :: power1, st, t1, t2, t1fact, t2fact, pi
  double precision :: chord, xle, xs

  npt = size(x,1)
  chord = x(npt) - x(1)
  xle = x(1)

  shape_switch: if (trim(shapetype) == 'hicks-henne') then 
      
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
subroutine create_airfoil_hicks_henne (xt_seed, zt_seed, xb_seed, zb_seed, modest, modesb,  &
                                      zt_new, zb_new, shapetype, symmetrical)

  use math_deps,          only : transformed_arccos

  double precision, dimension(:), intent(in) :: xt_seed, zt_seed, xb_seed,     &
                                                zb_seed
  double precision, dimension(:), intent(in) :: modest, modesb
  double precision, dimension(:), intent(inout) :: zt_new, zb_new
  character(*), intent(in) :: shapetype
  logical, intent(in) :: symmetrical

  integer :: i, nmodest, nmodesb, npointst, npointsb
  double precision :: strength

  nmodest = size(modest,1)/3
  nmodesb = size(modesb,1)/3
  npointst = size(zt_seed,1)
  npointsb = size(zb_seed,1)

! Create shape functions for Hicks-Henne

  call create_shape_functions(xt_seed, xb_seed, modest, modesb, shapetype,   &
                              first_time=.false.)  

! Top surface

  zt_new = zt_seed
  do i = 1, nmodest
    strength = 1.d0
    zt_new = zt_new + strength*top_shape_function(i,1:npointst)
  end do

! Bottom surface

  if (.not. symmetrical) then
    zb_new = zb_seed
    do i = 1, nmodesb
      strength = 1.d0   ! Hicks-Henne: strength is part of the shape function
      zb_new = zb_new + strength*bot_shape_function(i,1:npointsb)
    end do

  ! For symmetrical airfoils, just mirror the top surface

  else
    do i = 1, npointsb
      zb_new(i) = -zt_new(i)
    end do
  end if

end subroutine create_airfoil_hicks_henne


!-----------------------------------------------------------------------------
!
! Create airfoil from bezier design variables 
!
!-------------------------------------------------------------------------------
subroutine create_airfoil_bezier (seed_zt, seed_zb, dv_top, dv_bot, foil) 
  !! Create airfoil from bezier design variables
  ! 
  !  - seed z_coordinates are only needed to determine TE gap 
  !  - design variables dv are conveted back to bezier control points 
  !    to generate bezier curve 

  use vardef,                 only : airfoil_type
  use airfoil_shape_bezier,   only : bezier_spec_type
  use airfoil_shape_bezier,   only : dv_to_bezier, bezier_eval_airfoil

  double precision, dimension(:), intent(in) :: seed_zt, seed_zb, dv_top, dv_bot
  type(airfoil_type), intent(out) :: foil 

  double precision, allocatable :: px_top(:), py_top(:), px_bot(:), py_bot(:)
  type(bezier_spec_type)  :: bez_spec
  double precision        :: te_gap
  integer                 :: npt, npb, npoint

  ! retrieve bezier control points 

  npt    = size(seed_zt)
  te_gap = seed_zt(npt)

  call dv_to_bezier (dv_top, te_gap, px_top, py_top)

  ! write(*,"('px_top: ',100f8.4)") ( px_top(i), i=1,size(px_top) )
  ! write(*,"('py_top: ',100f8.4)") ( py_top(i), i=1,size(py_top))

  npb    = size(seed_zb)
  te_gap = seed_zb(npb)

  call dv_to_bezier (dv_bot, te_gap, px_bot, py_bot)

  ! write(*,"('px_bot: ',100f8.4)") ( px_bot(i), i=1,size(px_bot) )
  ! write(*,"('py_bot: ',100f8.4)") ( py_bot(i), i=1,size(py_bot) )
  ! write(*,*) 

  ! build airfoil with control points 

  npoint = npt + npb - 1

  bez_spec%ncpoints_top = size (px_top)
  bez_spec%ncpoints_bot = size (px_bot)  
  bez_spec%px_top = px_top
  bez_spec%py_top = py_top
  bez_spec%px_bot = px_bot
  bez_spec%py_bot = py_bot

  call bezier_eval_airfoil (bez_spec, npoint, foil%x, foil%z) 

  foil%npoint = size(foil%x)
  foil%bezier_based = .true.
  foil%bezier_spec  = bez_spec        ! could be useful to keep 

end subroutine create_airfoil_bezier 

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

  use vardef,             only : airfoil_type
  use xfoil_driver,       only : xfoil_scale_thickness_camber, xfoil_scale_LE_radius
                                   
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
  
  ! Especially Xfoils HIPNT tends to produce artefacts in curvature
  ! Smoothing should also be done for the seed airfoil 
  call smooth_foil (.false., 0.05d0, new_foil_2)

  ! Sanity check - new_foil may not have different number of points
  if (seed_foil%npoint /= new_foil_2%npoint) then
    call my_stop ('Number of points changed during thickness/camber modification')
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

  use vardef,             only : airfoil_type
  use xfoil_driver,       only : xfoil_scale_thickness_camber, xfoil_scale_LE_radius

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
    call my_stop ('Number of points changed during thickness/camber modification')
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
    call my_stop ('Number of points changed during thickness/camber modification')
  end if

  ! Especially Xfoils HIPNT tends to produce artefacts in curvature
  ! Smoothing should also be done for the seed airfoil 
  call smooth_foil (.false., 0.05d0, new_foil_2)
  call smooth_foil (.false., 0.05d0, new_foil_4)

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



!-------------------------------------------------------------------------------------
! Smooth an airfoil with its coordinate in foil%x and foil%z
!    ---   see details in smooth_it ---
!
! Returns  the smoothed airfoil  
!   - foil%x and foil%z
!   - the polylines foil%xb, foil%zb, foil%xt, foil%zt 
!
! *** this subroutine is in this module because of module hierarchy ***
!-------------------------------------------------------------------------------------
subroutine smooth_foil (show_details, spike_threshold, foil)

  use vardef,          only : airfoil_type
  use os_util,         only : my_stop
  use math_deps,       only : smooth_it
 
  logical, intent(in) :: show_details  
  double precision, intent(in) :: spike_threshold
  type(airfoil_type), intent(inout) :: foil

  integer :: i, iLE, pointst, pointsb

! find LE - it MUST be at 0,0 

  iLE = 0 
  do i = 1, size(foil%x)
    if (foil%x(i) == 0d0 .and. foil%z(i) == 0d0 ) then 
      iLE = i
      exit
    end if 
  end do 

  if (iLE == 0) then 
    call my_stop ("smooth_foil: No foil with LE at 0,0")
  end if 

! Split the foil into top and bot polyline
  pointst = ilE
  pointsb = size(foil%x) - iLE + 1


  if (allocated (foil%xt)) deallocate(foil%xt)
  if (allocated (foil%zt)) deallocate(foil%zt)
  allocate(foil%xt(pointst))
  allocate(foil%zt(pointst))

  do i = 1, pointst
    foil%xt(i) = foil%x(pointst-i+1)
    foil%zt(i) = foil%z(pointst-i+1)
  end do

  if (allocated (foil%xb)) deallocate(foil%xb)
  if (allocated (foil%zb)) deallocate(foil%zb)
  allocate(foil%xb(pointsb))
  allocate(foil%zb(pointsb))

  do i = 1, pointsb
    foil%xb(i) = foil%x(iLE+i-1)
    foil%zb(i) = foil%z(iLE+i-1)
  end do

! Now smooth both polylines
  call smooth_it (show_details, spike_threshold, foil%xt, foil%zt)
  call smooth_it (show_details, spike_threshold, foil%xb, foil%zb)

! and rebuild foil coordinates

  do i = 1, pointst
    foil%x(i) = foil%xt(pointst-i+1)
    foil%z(i) = foil%zt(pointst-i+1)
  end do
  do i = 1, pointsb-1
    foil%x(i+pointst) = foil%xb(i+1)
    foil%z(i+pointst) = foil%zb(i+1)
  end do

  end subroutine smooth_foil


end module parametrization

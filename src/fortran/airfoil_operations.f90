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

module airfoil_operations

! Performs transformations and other operations on airfoils

  use os_util 

  implicit none

! Coefficients for 5th-order polynomial (curve fit for leading edge)

  double precision, dimension(6) :: polynomial_coefs

  contains

!=============================================================================80
!
! Driver subroutine to read or create a seed airfoil
!
!=============================================================================80
subroutine get_seed_airfoil (seed_airfoil, airfoil_file, naca_options, foil )

  use vardef,       only : airfoil_type
  use naca,         only : naca_options_type, naca_456

  character(*), intent(in) :: seed_airfoil, airfoil_file
  type(naca_options_type), intent(in) :: naca_options
  type(airfoil_type), intent(out) :: foil

  integer :: pointsmcl

  if (trim(seed_airfoil) == 'from_file') then

!   Read seed airfoil from file

    write (*,'(/," - ", A)', advance = 'no') 'Reading airfoil '
    call print_colored (COLOR_HIGH,trim(airfoil_file))
    write (*,*)

    call load_airfoil(airfoil_file, foil)

  elseif (trim(seed_airfoil) == 'naca') then

!   Create NACA 4, 4M, 5, 6, or 6A series airfoil

    pointsmcl = 100
    call naca_456(naca_options, pointsmcl, foil)

  else

    write(*,*) "Error: seed_airfoil should be 'from_file' or 'naca'."
    write(*,*)
    stop

  end if

end subroutine get_seed_airfoil

!=============================================================================80
!
! Reads an airfoil from a file, loads it into the airfoil_type, sets ordering
! correctly
!
!=============================================================================80
subroutine load_airfoil(filename, foil)

  use vardef,      only : airfoil_type
  use memory_util, only : allocate_airfoil

  character(*), intent(in) :: filename
  type(airfoil_type), intent(out) :: foil

  logical :: labeled

  ! jx-mod additional check
  if (trim(filename) == '') then
    write (*,*) 
    call print_error ('Error: No airfoil file defined either in input file nor as command line argument')
    write(*,*)
    stop
  end if 

! Read number of points and allocate coordinates

  call airfoil_points(filename, foil%npoint, labeled)
  call allocate_airfoil(foil)

! Read airfoil from file

  call airfoil_read(filename, foil%npoint, labeled, foil%name, foil%x, foil%z)

! Change point ordering to counterclockwise, if necessary

  call cc_ordering(foil)

end subroutine load_airfoil

!=============================================================================80
!
! Subroutine to get number of points from an airfoil file and to determine
! whether it is labeled or plain.
!
!=============================================================================80
subroutine airfoil_points(filename, npoints, labeled)

  character(*), intent(in) :: filename
  integer, intent(out) :: npoints
  logical, intent(out) :: labeled

  integer :: iunit, ioerr
  double precision :: dummyx, dummyz

! Open airfoil file

  iunit = 12
  open(unit=iunit, file=filename, status='old', position='rewind', iostat=ioerr)
  if (ioerr /= 0) then
     call my_stop ('Cannot find airfoil file '//trim(filename),'stop')
  end if

! Read first line; determine if it is a title or not

  read(iunit,*,iostat=ioerr) dummyx, dummyz
  if (ioerr == 0) then
    npoints = 1
    labeled = .false.
  else
    npoints = 0
    labeled = .true.
  end if
  
! Read the rest of the lines

  do 
    read(iunit,*,end=500)
    npoints = npoints + 1
  end do

! Close the file

500 close(iunit)

end subroutine airfoil_points

!=============================================================================80
!
! Subroutine to read an airfoil.  Assumes the number of points is already known.
! Also checks for incorrect format.
!
!=============================================================================80
subroutine airfoil_read(filename, npoints, labeled, name, x, z)

  character(*), intent(in) :: filename
  character(*), intent(out) :: name
  integer, intent(in) :: npoints
  logical, intent(in) :: labeled
  double precision, dimension(:), intent(inout) :: x, z

  integer :: i, iunit, ioerr, nswitch
  double precision :: dir1, dir2

! Open airfoil file

  iunit = 12
  open(unit=iunit, file=filename, status='old', position='rewind', iostat=ioerr)
  if (ioerr /= 0) then
    call my_stop ('Error: cannot find airfoil file '//trim(filename),'stop')
  end if

! Read points from file

  name = ""
  if (labeled) read(iunit,'(A)') name
  name = trim(adjustl(name))

  do i = 1, npoints
    read(iunit,*,end=500,err=500) x(i), z(i)
  end do

! Close file

  close(iunit)

! Check that coordinates are formatted in a loop

  nswitch = 0
  dir1 = x(2) - x(1)
  do i = 3, npoints
    dir2 = x(i) - x(i-1)
    if (dir2 /= 0.d0) then
      if (dir2*dir1 < 0.d0) nswitch = nswitch + 1
      dir1 = dir2
    end if
  end do

  if (nswitch /= 1) then
!   Open the file again only to avoid error at label 500.
    open(unit=iunit, file=filename, status='old')
  else
    return
  end if

500 close(iunit)
  write (*,*)
  write (*,*)
  write(*,'(A)') "Incorrect format in "//trim(filename)//". File should"
  write(*,'(A)') "have x and y coordinates in 2 columns to form a single loop,"
  write(*,'(A)') "and there should be no blank lines.  See the user guide for"
  write(*,'(A)') "more information."
  call my_stop("Processing stopped","stop")

end subroutine airfoil_read

!=============================================================================80
!
! Changes airfoil point ordering to counterclockwise if necessary
!
!=============================================================================80
subroutine cc_ordering(foil)

  use vardef,    only : airfoil_type
  use math_deps, only : norm_2

  type(airfoil_type), intent(inout) :: foil

  double precision, dimension(foil%npoint) :: xtemp, ztemp
!  double precision, dimension(2) :: tevec1, tevec2
!  double precision :: len1, len2
  integer :: i, npoints

  npoints = foil%npoint

! jxmod vector based detection didn't work for strange s type airfoils

! Check if ordering needs to be switched

!  tevec1(1) = foil%x(2) - foil%x(1)
!  tevec1(2) = foil%z(2) - foil%z(1)
!  len1 = norm_2(tevec1)

!  tevec2(1) = foil%x(npoints-1) - foil%x(npoints)
!  tevec2(2) = foil%z(npoints-1) - foil%z(npoints)
!  len2 = norm_2(tevec2)

!  if ( (len1 == 0.d0) .or. (len2 == 0.d0) )                                    &
!    call my_stop("Panel with 0 length detected near trailing edge.")
!
!  tevec1 = tevec1/len1
!  tevec2 = tevec2/len2 

!  if (tevec1(2) < tevec2(2)) then
  if (foil%z(npoints) > foil%z(1)) then
    
    call print_warning ('Changing point ordering to counter-clockwise ...')
    
    xtemp = foil%x
    ztemp = foil%z
    do i = 1, npoints
      foil%x(i) = xtemp(npoints-i+1)
      foil%z(i) = ztemp(npoints-i+1)
    end do

  end if

end subroutine cc_ordering

!=============================================================================80
!
! Subroutine to find leading edge of airfoil
!
! Input: airfoil(X,Z)
! Output: le: index of point closest to leading edge
!         xle: x-location of leading edge
!         zle: z-location of leading edge
!         addpoint_loc: integer giving the position at which to add a new point
!            for the leading edge. +1 means the index after le, -1 means the
!            index before le, and 0 means no new point is needed (x(le), z(le) 
!            is exactly at the leading edge).
!
!=============================================================================80
subroutine le_find(x, z, le, xle, zle, addpoint_loc)

  use math_deps, only : norm_2

  double precision, dimension(:), intent(in) :: x, z
  integer, intent(out) :: le, addpoint_loc
  double precision, intent(out) :: xle, zle

  integer :: i, npt
  double precision, dimension(:), allocatable :: s, xp, zp
  double precision, dimension(2) :: r1, r2
  double precision :: sle, dist1, dist2, dot

  interface
    double precision function SEVAL(SS, X_dum, XS, S_dum, N)
      integer, intent(in) :: N
      double precision, intent(in) :: SS
      double precision, dimension(N), intent(in) :: X_dum, XS, S_dum
    end function SEVAL
  end interface 

  le = 0
  xle = 0d0
  zle = 0d0
  addpoint_loc = 0
  npt = size(x,1)

! Is LE already at 0,0? -> do not  use Xfoil LEFIND 

  do i = 2, (npt-1)
    if (x(i) == 0d0 .and. z(i) == 0d0) then 
      le = i
      return
    end if 
  end do 

! Get leading edge location from Xfoil

  allocate(s(npt))
  allocate(xp(npt))
  allocate(zp(npt))
  call SCALC(x, z, s, npt)
  call SEGSPL(x, xp, s, npt)
  call SEGSPL(z, zp, s, npt)
  call LEFIND(sle, x, xp, z, zp, s, npt, .true.)
  xle = SEVAL(sle, x, xp, s, npt)
  zle = SEVAL(sle, z, zp, s, npt)
  deallocate(s)
  deallocate(xp)
  deallocate(zp)

! Determine leading edge index and where to add a point

  npt = size(x,1)
  do i = 1, npt-1
    r1(1) = xle - x(i)
    r1(2) = zle - z(i)
    dist1 = norm_2(r1)
    if (dist1 /= 0.d0) r1 = r1/dist1

    r2(1) = xle - x(i+1)
    r2(2) = zle - z(i+1)
    dist2 = norm_2(r2)
    if (dist2 /= 0.d0) r2 = r2/dist2

    dot = dot_product(r1, r2)
    if (dist1 == 0.d0) then
      le = i
      addpoint_loc = 0
      !write (*,*) "p  i-1 " , i-1,   x(i-1), z(i-1)
      !write (*,*) "p  i   " , i,   x(i), z(i), dist1
      !write (*,*) "p  LE  " , le,  xle, zle, 0d0, addpoint_loc
      !write (*,*) "p  i+1 " , i+1, x(i+1), z(i+1), dist2
      exit
    else if (dist2 == 0.d0) then
      le = i+1
      addpoint_loc = 0
      exit
    else if (dot < 0.d0) then
      if (dist1 < dist2) then
        le = i
        addpoint_loc = 1
      else
        le = i+1
        addpoint_loc = -1
      end if
      exit
    end if
  end do

end subroutine le_find

!-----------------------------------------------------------------------------
!
! Repanel an airfoil with npoints and normalize it to get LE at 0,0 and
!    TE at 1.0 (upper and lower side may have a gap)  
!
! For normalization xfoils LEFIND is used to calculate the (virtual) LE of
!    the airfoil - then it's shifted, rotated, sclaed to be normalized.
!
! Bad thing: a subsequent xfoil LEFIND won't deliver TE at 0,0 but still with a little 
!    offset. SO this is iterated until the offset is smaller than epsilon
!
!-----------------------------------------------------------------------------
subroutine repanel_and_normalize_airfoil (in_foil, xfoil_geom_options, symmetrical, foil)

  use vardef,       only : airfoil_type         
  use math_deps,    only : norm_2
  use xfoil_driver, only : smooth_paneling, xfoil_geom_options_type

  type(airfoil_type), intent(in)    :: in_foil
  type(airfoil_type), intent(out)   :: foil
  type(xfoil_geom_options_type),intent(in)    :: xfoil_geom_options
  logical,            intent(in)    :: symmetrical

  type(airfoil_type)  :: tmp_foil
  integer             :: i, pointst, pointsb
  logical             :: le_fixed
  double precision, dimension(2) :: p, p_next

  ! iteration thresholds
  double precision, parameter    :: EPSILON = 1.d-12          ! distance xfoil LE to 0,0
  double precision, parameter    :: LE_PANEL_FACTOR = 0.4   ! lenght LE panel / length prev panel

  tmp_foil%npoint = xfoil_geom_options%npan
  allocate (tmp_foil%x(tmp_foil%npoint))  
  allocate (tmp_foil%z(tmp_foil%npoint))  

  foil%name = in_foil%name

  ! initial paneling to npoint_new
  call smooth_paneling(in_foil, 0, foil, xfoil_geom_options)
 
  call le_find(foil%x, foil%z, foil%leclose, foil%xle, foil%zle, foil%addpoint_loc)
  le_fixed = .false. 

  do i = 1,10

    call transform_airfoil(foil)

    call le_find(foil%x, foil%z, foil%leclose, foil%xle, foil%zle, foil%addpoint_loc)

    p(1) = foil%xle
    p(2) = foil%zle
    
    if (norm_2(p) < EPSILON) then
      le_fixed = .true. 
      exit 
    end if
    
    tmp_foil%x = foil%x
    tmp_foil%z = foil%z
    call smooth_paneling(tmp_foil, 0, foil, xfoil_geom_options)
    call le_find(foil%x, foil%z, foil%leclose, foil%xle, foil%zle, foil%addpoint_loc)
    
  end do

  ! reached a virtual LE which is closer to 0,0 than epsilon, set it to 0,0
  if (le_fixed) then 
    foil%xle = 0.d0
    foil%zle = 0.d0
    ! is the LE panel of closest point much! shorter than the next panel? 
    !       if yes, take this point to LE 0,0
    p(1)      = foil%x(foil%leclose)
    p(2)      = foil%z(foil%leclose)
    p_next(1) = foil%x(foil%leclose + 1) - foil%x(foil%leclose)
    p_next(2) = foil%z(foil%leclose + 1) - foil%z(foil%leclose)
    if ((norm_2(p) / norm_2(p_next)) < LE_PANEL_FACTOR) then
      foil%addpoint_loc = 0               ! will lead to no insertion of new point
      foil%x(foil%leclose) = 0d0
      foil%z(foil%leclose) = 0d0
    end if 
  else
    call print_warning ("Leading edge couln't be moved close to 0,0. Continuing ...",3)
    write (*,*)
  end if 

  ! now split airfoil to get upper and lower polyline 
  !     if there is a new leading added it will be added to the polylines

  foil%symmetrical = symmetrical

  call split_foil(foil)

  ! and rebuild normal x,z coordinates out of polylines to have (new) LE

  pointst = size(foil%xt,1)
  pointsb = size(foil%xb,1)

  foil%npoint = pointst + pointsb - 1

  deallocate (foil%x)
  deallocate (foil%z)
  allocate(foil%x(foil%npoint))
  allocate(foil%z(foil%npoint))

  do i = 1, pointst
    foil%x(i) = foil%xt(pointst-i+1)
    foil%z(i) = foil%zt(pointst-i+1)
  end do
  do i = 1, pointsb-1
    foil%x(i+pointst) = foil%xb(i+1)
    foil%z(i+pointst) = foil%zb(i+1)
  end do

  write (*,'(/," - ",A)', advance = 'no') 'Repaneling and normalizing.'

  if (foil%addpoint_loc /= 0) then 
    call print_colored (COLOR_NOTE, ' Leading edge added.')
  end if
  call print_colored (COLOR_NOTE, ' Airfoil will have '//stri(foil%npoint) //' Points')
  write (*,*) 


end subroutine repanel_and_normalize_airfoil


!-----------------------------------------------------------------------------
!
! Translates and scales an airfoil such that it has a 
!    length of 1 
!    leading edge is at the origin
!    chord is parallel to x-axis
! 
! transform_airfoil uses - must be set in le_find!
!    foil%xle       (virtuell LE as calculated in xfoil)
!    foil%zle
!    leclose        Index of point closest to (virtuell) LE
!    addpoint_loc      is (virtuell) LE before, at, after leclose 
!
!-----------------------------------------------------------------------------
subroutine transform_airfoil (foil)

  use vardef, only : airfoil_type

  type(airfoil_type), intent(inout) :: foil

  double precision :: xoffset, zoffset, foilscale_upper, foilscale_lower
  double precision :: angle, cosa, sina
  double precision, parameter ::EPSILON = 1d-10   ! ... when coordinate will be 0.0

  integer :: npoints, i, pointst, pointsb

  npoints = foil%npoint

! Translate so that the leading edge is at the origin

  do i = 1, npoints
    foil%x(i) = foil%x(i) - foil%xle
    foil%z(i) = foil%z(i) - foil%zle
  end do
  xoffset = -foil%xle
  zoffset = -foil%zle
  foil%xle = 0.d0
  foil%zle = 0.d0

! Rotate the airfoil so chord is on x-axis 

  angle = atan2 ((foil%z(1)+foil%z(npoints))/2.d0,(foil%x(1)+foil%x(npoints))/2.d0)
  cosa  = cos (-angle) 
  sina  = sin (-angle) 
  do i = 1, npoints
    foil%x(i) = foil%x(i) * cosa - foil%z(i) * sina
    foil%z(i) = foil%x(i) * sina + foil%z(i) * cosa
  end do

! Scale airfoil so that it has a length of 1 
! - there are mal formed airfoils with different TE on upper and lower
!   scale both to 1.0  

  foilscale_upper = 1.d0 / foil%x(1)
  foilscale_lower = 1.d0 / foil%x(npoints)

  call get_split_points(foil, pointst, pointsb)

  do i = 1, npoints
    if (i >= (npoints - pointsb)) then 
      foil%x(i) = foil%x(i)*foilscale_lower
      foil%z(i) = foil%z(i)*foilscale_lower
    else
      foil%x(i) = foil%x(i)*foilscale_upper
      foil%z(i) = foil%z(i)*foilscale_upper
    end if
  end do

  ! Ensure TE is at x=1

  If (foil%x(1) /= 1d0 .or. foil%x(npoints) /= 1d0) then 
    foil%x(1)       = 1d0
    foil%x(npoints) = 1d0
  end if 

  ! Force TE to 0.0 if z < epsilon 

  if (foil%z(1) == foil%z(npoints) .and. abs(foil%z(1)) < EPSILON) then 
    foil%z(1) = 0.d0
    foil%z(npoints) = 0.d0 
  end if 

end subroutine transform_airfoil

!=============================================================================80
!
! Subroutine to determine the number of points on the top and bottom surfaces of
! an airfoil
!
!=============================================================================80
subroutine get_split_points(foil, pointst, pointsb)

  use vardef, only : airfoil_type

  type(airfoil_type), intent(in) :: foil
  integer, intent(out) :: pointst, pointsb

  if (foil%addpoint_loc == 0) then
    pointst = foil%leclose
    pointsb = foil%npoint - foil%leclose + 1
  elseif (foil%addpoint_loc == -1) then
    pointst = foil%leclose 
    pointsb = foil%npoint - foil%leclose + 2
  else
    pointst = foil%leclose + 1
    pointsb = foil%npoint - foil%leclose + 1
  end if

! Modify for symmetrical airfoil (top surface will be mirrored)

  if (foil%symmetrical) pointsb = pointst

end subroutine get_split_points



!-----------------------------------------------------------------------------
! Split an airfoil into top (xt,zt) and bottom surface (xb,zb) polyline
!-----------------------------------------------------------------------------

subroutine split_foil(foil)

  use vardef, only : airfoil_type

  type(airfoil_type), intent(inout) :: foil
  
  integer i, boundst, boundsb, pointst, pointsb

  ! In le_find the "virtual" leading edge was determined 
  !    and checked if an additional le point has to be inserted to reflect the le
  !    dpeending on foil%addpoint_loc a new point will be inserted to 
  !    become the starting point (0,0) for top and bottom surface


  call get_split_points(foil, pointst, pointsb)

  if (foil%addpoint_loc == 0) then
    boundst = foil%leclose - 1
    boundsb = foil%leclose + 1
  elseif (foil%addpoint_loc == -1) then
    boundst = foil%leclose - 1
    boundsb = foil%leclose
  else
    boundst = foil%leclose
    boundsb = foil%leclose + 1
  end if

! Copy points for the top surface

  allocate(foil%xt(pointst))
  allocate(foil%zt(pointst))

  foil%xt(1) = foil%xle
  foil%zt(1) = foil%zle
  do i = 1, pointst - 1
    foil%xt(i+1) = foil%x(boundst-i+1)
    foil%zt(i+1) = foil%z(boundst-i+1)
  end do

! Copy points for the bottom surface

  if (.not. foil%symmetrical) then
    allocate(foil%xb(pointsb))
    allocate(foil%zb(pointsb))
    foil%xb(1) = foil%xle
    foil%zb(1) = foil%zle
    do i = 1, pointsb - 1
      foil%xb(i+1) = foil%x(boundsb+i-1)
      foil%zb(i+1) = foil%z(boundsb+i-1)
    end do
  else
    allocate(foil%xb(pointst))
    allocate(foil%zb(pointst))
    foil%xb =  foil%xt
    foil%zb = -foil%zt
  end if

end subroutine split_foil

!------------------------------------------------------------------------------
!
! Rebuild airfoil out of top and bottom surfaces
! 
!------------------------------------------------------------------------------

subroutine rebuild_airfoil(xt, xb, zt, zb, foil)

  use vardef, only        : airfoil_type
  use memory_util, only   : deallocate_airfoil

  type(airfoil_type), intent(inout) :: foil
  double precision, dimension(:), intent(in) :: xt, xb, zt, zb
  
  integer i, pointst, pointsb

  call deallocate_airfoil(foil)

  pointst = size(xt,1)
  pointsb = size(xb,1)

  foil%npoint = pointst + pointsb - 1

  allocate(foil%x(foil%npoint))
  allocate(foil%z(foil%npoint))
  allocate(foil%xb(size(xb,1)))
  allocate(foil%xt(size(xt,1)))
  allocate(foil%zb(size(xb,1)))
  allocate(foil%zt(size(xt,1)))

  foil%xb = xb
  foil%xt = xt
  foil%zb = zb
  foil%zt = zt

  do i = 1, pointst
    foil%x(i) = xt(pointst-i+1)
    foil%z(i) = zt(pointst-i+1)
  end do
  do i = 1, pointsb-1
    foil%x(i+pointst) = xb(i+1)
    foil%z(i+pointst) = zb(i+1)
  end do


end subroutine rebuild_airfoil

!=============================================================================80
!
! Writes an airfoil to a labeled file
!
!=============================================================================80
subroutine airfoil_write(filename, title, foil)

  use vardef,     only : airfoil_type

  character(*), intent(in) :: filename, title
  type(airfoil_type), intent(in) :: foil
  integer :: iunit

  write (*,'(" - ", A)', advance = 'no') 'Writing airfoil to '
  call print_colored (COLOR_HIGH,trim(filename))
  write (*,*)

! Open file for writing and out ...

  iunit = 13
  open  (unit=iunit, file=filename, status='replace')
  call  airfoil_write_to_unit (iunit, title, foil, .false.)
  close (iunit)

end subroutine airfoil_write

!-----------------------------------------------------------------------------
!
! Writes an airfoil with a title to iunit
!    --> central function for all foil coordinate writes
!
! write_derivatives = true: additional to x and y write derivative 2 and 3
!-----------------------------------------------------------------------------

subroutine airfoil_write_to_unit (iunit, title, foil, write_derivatives)

  use vardef,          only : airfoil_type
  use math_deps,       only : derivative2, derivative3 

  integer, intent(in) :: iunit
  character(*), intent(in) :: title
  type(airfoil_type), intent(in) :: foil
  logical, intent(in):: write_derivatives

  double precision, dimension(size(foil%x)) :: deriv2, deriv3
  integer :: i

! Add 2nd and 3rd derivative to
!        ...design_coordinates.dat to show it in visualizer
  if (write_derivatives) then
    deriv2 = derivative2 (foil%x, foil%z)
    deriv3 = derivative3 (foil%x, foil%z)
  end if

! Write label to file
  
  write(iunit,'(A)') trim(title)

! Write coordinates

  do i = 1, foil%npoint
    if (write_derivatives) then
      write(iunit,'(2F12.7,2G17.7)')  foil%x(i), foil%z(i), deriv2(i), deriv3(i)
    else
      write(iunit,'(2F12.7)')         foil%x(i), foil%z(i)
    end if
  end do


end subroutine airfoil_write_to_unit

!=============================================================================80
!
! Checks if a given character is a number
!
!=============================================================================80
function isnum(s)

  character, intent(in) :: s
  logical :: isnum

  select case (s)
    case ('0')
      isnum = .true.
    case ('1')
      isnum = .true.
    case ('2')
      isnum = .true.
    case ('3')
      isnum = .true.
    case ('4')
      isnum = .true.
    case ('5')
      isnum = .true.
    case ('6')
      isnum = .true.
    case ('7')
      isnum = .true.
    case ('8')
      isnum = .true.
    case ('9')
      isnum = .true.
    case default
      isnum = .false.
  end select

end function isnum


!------------------------------------------------------------------------------
! jx-mod - New high level functions
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Assess polyline (x,y) on surface quality (curves of2nd and 3rd derivation)
!    will return surface quality e.g. Q_GOOD
!    and print an info string like this '-----R---H--sss--' (show_details)
!------------------------------------------------------------------------------

subroutine assess_surface (show_details, info, &
                           istart, iend, iend_spikes, &
                           curv_threshold, spike_threshold, &
                           x, y, overall_quality)

  use math_deps, only: find_curv_reversals, find_curv_spikes, curvature

  logical, intent(in)      :: show_details
  character(*), intent(in) :: info
  integer, intent(in)      :: istart, iend, iend_spikes
  double precision, intent(in) :: curv_threshold, spike_threshold
  double precision, dimension(:), intent(in) :: x, y
  integer, intent(out)      :: overall_quality

  integer             :: nspikes, nreversals, npt
  double precision    :: cur_te_curvature
  integer             :: quality_spikes, quality_reversals
  integer             :: quality_te
  character (size(x)) :: result_info
  character (90)      :: result_out
  character (14)      :: label

  nreversals = 0
  nspikes    = 0
  npt        = size(x)

  result_info           = repeat ('-', npt ) 

! have a look at 3rd derivation ... 
  call find_curv_spikes   (istart, iend_spikes, spike_threshold, &
                                x, y, nspikes, result_info)

! have a look at 2nd derivation ... skip first 5 points at LE (too special there) 
  call find_curv_reversals(istart, iend, curv_threshold, &
                                x, y, nreversals, result_info)

  quality_spikes    = i_quality (nspikes, 2, 6, 40)
  quality_reversals = i_quality (nreversals, 2, 3, 10)
  overall_quality   = ior(quality_spikes, quality_reversals)

! check te curvature 
  cur_te_curvature = get_max_te_curvature (x,y )
  quality_te      = r_quality (cur_te_curvature, 0.2d0, 1d0, 10d0)
  ! te quality counts only half as too often it is bad ... 
  overall_quality = ior(overall_quality, (quality_te / 2))

 
  ! all the output ...

  if(show_details) then

    if (len(result_info) > len(result_out)) then
      result_out = '... ' // result_info ((len(result_info) - len(result_out) + 1 + 4):)
    else
      result_out = result_info
    end if 

    write (*,'(3x)', advance ='no') 
    write (label,'(A)') adjustl(trim(info)) 
    call print_colored (COLOR_NOTE, label)
    write (*,'(x,A)') result_out

    write (*,'(18x)', advance ='no')   
    call print_colored (COLOR_NOTE, 'Spikes')
    call print_colored_i (3, quality_spikes, nspikes)
    call print_colored (COLOR_NOTE, '     Reversals')
    call print_colored_i (3, quality_reversals, nreversals)
    call print_colored (COLOR_NOTE, '     Curvature at TE')
    call print_colored_r (5,'(F5.2)', quality_te, cur_te_curvature) 
    if (quality_te > Q_BAD) then
      call print_colored (COLOR_NOTE, '  (geometric spoiler at TE?)')
    end if 
    write(*,*)
  end if
                             
end subroutine assess_surface
 


!-------------------------------------------------------------------------
! get max. curvature at the end of polyline (= TE)
!-------------------------------------------------------------------------

function get_max_te_curvature (x,y) 

  use math_deps,        only: derivative2

  double precision   :: get_max_te_curvature 

  double precision, dimension(:), intent(in) :: x, y
  double precision, dimension(:), allocatable  :: curv

  integer            :: npt, ite
  
  npt    = size(x)
  ite    = npt - 3                !  "te" begins at index

  allocate (curv (npt))
  curv  = derivative2(x, y) 
  get_max_te_curvature =  maxval (abs(curv(ite: npt)))

end function get_max_te_curvature


!------------------------------------------------------------------------------
! Assess polyline (x,y) for reversals and HighLows
!    and print an info string like this '-----R---H--sss--'
!
!   info                Id-String to print for User e.g. 'Top surface'
!   reversal_threshold  minimum +- of curvature to detect reversal 
!   highlow_threshold   minimum height of a highlow to detect 
!   spike_threshold     minimum +- of 3rd derivation to detect reversal 
!
!------------------------------------------------------------------------------

  subroutine show_reversals_highlows (info, x, y, & 
                                     reversal_threshold, spike_threshold )

  use math_deps, only : find_curv_reversals, find_curv_spikes

  character(*), intent(in) :: info
  double precision, dimension(:), intent(in) :: x, y
  double precision, intent(in)   :: reversal_threshold, spike_threshold
  integer  :: nreversals, nspikes, istart, iend

  character (size(x)) :: result_info

  result_info = repeat ('-', size(x) ) 
  istart = 5                              !skip first 5 points at LE (too special there) 
  iend = size(x)
     

  ! have a look at 2nd derivation ...
  call find_curv_reversals    (istart, iend, reversal_threshold, &
                                x, y, nreversals, result_info)

  ! and a look at 3rd derivation ...
  call find_curv_spikes       (istart, iend, spike_threshold,&
                                 x, y, nspikes, result_info)
                              

  write (*,'(11x,A,1x,3(I2,A),A)') info//' ', nreversals, 'R ', &
            nspikes, 'S ','  '// result_info

end subroutine show_reversals_highlows


!------------------------------------------------------------------------------
! For testing 
!   Make sinus y-coordinate in foil 
!------------------------------------------------------------------------------

subroutine test_make_circle_foil (foil)

  use vardef,          only : airfoil_type

  type(airfoil_type), intent(inout) :: foil
  double precision  :: pi, last_x
  integer           :: i 

  pi = acos(-1.d0)

  last_x = 100d0

  do i = 1, size(foil%x) 

    if (foil%x(i) < last_x) then 
      foil%z(i) =   0.1d0 * (0.5d0 ** 2 - (foil%x(i) -0.5d0)**2) **0.5d0
    else 
      foil%z(i) = - 0.1d0 * (0.5d0 ** 2 - (foil%x(i) -0.5d0)**2) **0.5d0
    end if 

    last_x = foil%x(i)
  end do 


end subroutine test_make_circle_foil

end module airfoil_operations

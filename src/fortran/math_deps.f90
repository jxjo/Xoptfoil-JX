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

module math_deps

! Contains various math functions and numerical methods

  implicit none

  contains

!=============================================================================80
!
! Function to get x = inv(A)*C using gaussian elimination
!
!=============================================================================80
function lmult(A,C) result(X)

  double precision, dimension(:,:), intent(in) :: A
  double precision, dimension(:), intent(in) :: C
  double precision, dimension(size(C,1)) :: X
  double precision, dimension(size(C,1),size(C,1)+1) :: Q
  integer :: N, i, j, R
  double precision :: elim, pivot, rscale, rsum, eps
  eps = 1D-16

! Initialize

  N = size(C,1)
  if (size(A,1) /= N .or. size(A,2) /= N) then
    write(*,*)
    write(*,*) 'Error: for A*X = C and size(C) = Nx1, size(A) must be NxN'
    write(*,*)
    stop
  end if
  X(:) =  0.d0
  Q(:,1:N) = A(:,:)
  Q(:,N+1) = C(:)

! Gaussian elimination loop to put in upper triangular form

  do R = 1, N-1
    pivot = Q(R,R)
    do i = R+1, N
      elim = Q(i,R)
      if (abs(elim) > eps) then
        rscale = elim/pivot
        Q(i,:) = Q(i,:) - rscale*Q(R,:)
      end if
    end do
  end do

! Solution loop

  do i = N, 1, -1
    rsum = Q(i,N+1)
    do j = N, i+1, -1
      if (abs(Q(i,j)) > eps) rsum = rsum - Q(i,j)*X(j)
    end do
    if (Q(i,i) == 0) then
      write(*,*)
      write(*,*) 'Error in lmult: singular matrix.'
      stop
    else
      X(i) = rsum/Q(i,i)
    end if
  end do

end function lmult

!=============================================================================80
!
! Normal distribution function, used for small spacing at ends and greater in
! the middle
!
!=============================================================================80
function normal_dist(x, sig, mu) result(val)

  double precision, intent(in) :: x, sig, mu
  double precision val, pi

  pi = acos(-1.d0)
  val = 1.d0/(sig*sqrt(2.d0*pi))*exp(-(x-mu)**2.d0/(2.d0*sig**2.d0))

end function normal_dist

!=============================================================================80
!
! Vector norm (since not all compilers may include it by default)
!
!=============================================================================80
function norm_2(vector) result(val)

  double precision, dimension(:), intent(in) :: vector
  double precision :: val
  integer :: nelem, i

! Determine size

  nelem = size(vector)

! Get vector norm

  val = 0.d0
  do i = 1, nelem
    val = val + vector(i)**2.d0
  end do
  val = sqrt(val)

end function norm_2

!=============================================================================80
!
! Interpolates a vector y with original coordinates x to a new set of
! coordinates xnew
!
!=============================================================================80
subroutine interp_vector(x, y, xnew, ynew)

  double precision, dimension(:), intent(in) :: x, y, xnew
  double precision, dimension(:), intent(inout) :: ynew

  logical :: isbtwn
  integer :: i, pt1, npt, nptnew

  npt = size(x,1)
  nptnew = size(xnew,1)

  pt1 = 1
  do i = 1, nptnew

!   Find interpolants

    isbtwn = .false.
    do while (.not. isbtwn .and. (pt1 < npt))
      isbtwn = between(x(pt1), xnew(i), x(pt1+1))
      if (.not. isbtwn) then
        pt1 = pt1 + 1
        if (pt1 == npt) then
          write(*,*)
          write(*,*) 'Warning: could not find interpolants.'
          write(*,*) 'x: ', xnew(i), 'xmax: ', x(npt)
          stop
        end if
      end if
    end do

!   Interpolate points

    ynew(i) = interp1(x(pt1), x(pt1+1), xnew(i), y(pt1), y(pt1+1))

  end do

end subroutine interp_vector

!=============================================================================80
!
! Interpolates between two points
!
!=============================================================================80
function interp1(x1, x2, x, y1, y2) result(y)

  double precision, intent(in) :: x1, x2, x, y1, y2
  double precision y

  y = y1 + (y2 - y1)*(x - x1)/(x2 - x1)

end function interp1

!=============================================================================80
!
! Determines if B is between A and C
!
!=============================================================================80
function between(A, B, C) result(test)

  double precision, intent(in) :: A, B, C
  logical test

  if ((B >= A) .and. (B <= C)) then
    test = .true.
  else
    test = .false.
  end if 

end function between

!=============================================================================80
!
! Computes curvature for a function gam(s) = x(s) + y(s)
!
!=============================================================================80
function curvature(npt, x, y)

  integer, intent(in) :: npt
  double precision, dimension(npt), intent(in) :: x, y
  double precision, dimension(npt) :: curvature

  integer :: i
  double precision, dimension(npt) :: svec
  double precision :: se, se2
  double precision :: xe, ye, xe2, ye2
  double precision :: xs, ys, xs2, ys2

! Airfoil length vector s 

  svec(1) = 0.d0
  do i = 2, npt
    svec(i) = svec(i-1) + sqrt((x(i)-x(i-1))**2.d0 + (y(i)-y(i-1))**2.d0)
  end do

! Compute first and second derivatives and curvature vector

  do i = 1, npt

    if (i == 1) then

!     Grid metric ds/de and d2s/de2

      se = derv1f(svec(i+2), svec(i+1), svec(i), 1.d0)
      se2 = derv2f(svec(i+2), svec(i+1), svec(i), 1.d0)

!     Derivatives of x and y with respect to the grid parameter e

      xe = derv1f(x(i+2), x(i+1), x(i), 1.d0)
      ye = derv1f(y(i+2), y(i+1), y(i), 1.d0)
      xe2 = derv2f(x(i+2), x(i+1), x(i), 1.d0)
      ye2 = derv2f(y(i+2), y(i+1), y(i), 1.d0)

    elseif (i == npt) then

!     Grid metric ds/de and d2s de2

      se = derv1b(svec(i-2), svec(i-1), svec(i), 1.d0)
      se2 = derv2b(svec(i-2), svec(i-1), svec(i), 1.d0)

!     Derivatives of x and y with respect to the grid parameter e

      xe = derv1b(x(i-2), x(i-1), x(i), 1.d0)
      ye = derv1b(y(i-2), y(i-1), y(i), 1.d0)
      xe2 = derv2b(x(i-2), x(i-1), x(i), 1.d0)
      ye2 = derv2b(y(i-2), y(i-1), y(i), 1.d0)
      
    else

!     Grid metric ds/de and d2s de2

      se = derv1c(svec(i+1), svec(i-1), 1.d0)
      se2 = derv2c(svec(i+1), svec(i), svec(i-1), 1.d0)

!     Derivatives of x and y with respect to the grid parameter e

      xe = derv1c(x(i+1), x(i-1), 1.d0)
      ye = derv1c(y(i+1), y(i-1), 1.d0)
      xe2 = derv2c(x(i+1), x(i), x(i-1), 1.d0)
      ye2 = derv2c(y(i+1), y(i), y(i-1), 1.d0)

    end if

!   Derivatives of x and y with respect to surface length s

    xs = 1.d0/se * xe
    ys = 1.d0/se * ye
    xs2 = 1.d0/se**2.d0 * (xe2 - se2/se*xe)
    ys2 = 1.d0/se**2.d0 * (ye2 - se2/se*ye)

!   Curvature

    curvature(i) = (xs*ys2 - ys*xs2) / (xs**2.d0 + ys**2.d0)**1.5d0

  end do

end function curvature

!=============================================================================80
!
! Forward difference approximation for first derivative (1st  order)
!
!=============================================================================80
function derv1f1(u_plus1, u, h)

  double precision, intent(in) :: u_plus1, u, h
  double precision :: derv1f1

  derv1f1 = (u_plus1 - u)/h 

end function derv1f1

!=============================================================================80
!
! Forward difference approximation for first derivative (2nd order)
!
!=============================================================================80
function derv1f(u_plus2, u_plus1, u, h)

  double precision, intent(in) :: u_plus2, u_plus1, u, h
  double precision :: derv1f

  derv1f = (-3.d0*u + 4.d0*u_plus1 - u_plus2) / (2.d0*h)

end function derv1f

!=============================================================================80
!
! Backward difference approximation for first derivative (1st order)
!
!=============================================================================80
function derv1b1(u_minus1, u, h)

  double precision, intent(in) :: u_minus1, u, h
  double precision :: derv1b1

  derv1b1 = (u - u_minus1)/h

end function derv1b1

!=============================================================================80
!
! Backward difference approximation for first derivative (2nd order)
!
!=============================================================================80
function derv1b(u_minus2, u_minus1, u, h)

  double precision, intent(in) :: u_minus2, u_minus1, u, h
  double precision :: derv1b

  derv1b = (3.d0*u - 4.d0*u_minus1 + u_minus2) / (2.d0*h)

end function derv1b

!=============================================================================80
!
! Central difference approximation for first derivative (2nd order)
!
!=============================================================================80
function derv1c(u_plus, u_minus, h)

  double precision, intent(in) :: u_plus, u_minus, h
  double precision :: derv1c

  derv1c = (u_plus - u_minus) / (2.d0*h)

end function derv1c

!=============================================================================80
!
! Forward difference approximation for second-order derivative
!
!=============================================================================80
function derv2f(u_plus2, u_plus, u, h)

  double precision, intent(in) :: u_plus2, u_plus, u, h
  double precision :: derv2f

  derv2f = (u - 2.d0*u_plus + u_plus2) / h**2.d0

end function derv2f

!=============================================================================80
!
! Backward difference approximation for second-order derivative
!
!=============================================================================80
function derv2b(u_minus2, u_minus, u, h)

  double precision, intent(in) :: u_minus2, u_minus, u, h
  double precision :: derv2b

  derv2b = (u - 2.d0*u_minus + u_minus2) / h**2.d0

end function derv2b

!=============================================================================80
!
! Central difference approximation for second-order derivative
!
!=============================================================================80
function derv2c(u_plus, u, u_minus, h)

  double precision, intent(in) :: u_plus, u, u_minus, h
  double precision :: derv2c

  derv2c = (u_plus - 2.d0*u + u_minus) / h**2.d0

end function derv2c

!=============================================================================80
!
! Generates a pseudo-random integer in the specified range
!
!=============================================================================80
function random_integer(low, high)

  integer, intent(in) :: low, high
  integer :: random_integer

  double precision :: randdble

! Generate a random number in the range (0, 1)

  call random_number(randdble)

! Scale, translate, and convert to integer

  random_integer = low + floor(randdble*dble(high - low + 1))

end function random_integer

!=============================================================================80
!
! Generates a pseudo-random double precision number in the specified range
!
!=============================================================================80
function random_double(low, high)

  double precision, intent(in) :: low, high
  double precision :: random_double

  double precision :: randdble

! Generate a random number in the range (0, 1)

  call random_number(randdble)

! Scale and translate

  random_double = low + randdble*(high - low)

end function random_double

!=============================================================================80
!
! Swaps two elements of vector
!
!=============================================================================80
subroutine swap_double(vec, idx0, idx1)

  double precision, dimension(:), intent(inout) :: vec
  integer, intent(in) :: idx0, idx1

  double precision :: t1, t2

  t1 = vec(idx0)
  t2 = vec(idx1)
  vec(idx0) = t2
  vec(idx1) = t1

end subroutine swap_double

subroutine swap_int(vec, idx0, idx1)

  integer, dimension(:), intent(inout) :: vec
  integer, intent(in) :: idx0, idx1

  integer :: t1, t2

  t1 = vec(idx0)
  t2 = vec(idx1)
  vec(idx0) = t2
  vec(idx1) = t1

end subroutine swap_int

!=============================================================================80
!
! Sorts a vector via bubble sort. Optionally records map of indices relative to
! input vector.
!
!=============================================================================80
subroutine sort_vector(vec, idxs)

  double precision, dimension(:), intent(inout) :: vec
  integer, dimension(:), intent(inout), optional :: idxs

  integer :: nelem, i, sortcounter
  logical :: sorted

! Set up indexing array

  nelem = size(vec,1)
  if (present(idxs)) then
    do i = 1, nelem
      idxs(i) = i
    end do
  end if

! Bubble sorting algorithm

  sorted = .false.
  do while (.not. sorted)

    sortcounter = 0
    do i = 1, nelem-1
      if (vec(i+1) < vec(i)) then
        call swap_double(vec, i, i+1)
        sortcounter = sortcounter + 1
        if (present(idxs)) call swap_int(idxs, i, i+1)
      end if
    end do
    if (sortcounter == 0) sorted = .true.

  end do

end subroutine sort_vector

!=============================================================================80
!
! jx-mod Smoothing - Additional functions 
!
!=============================================================================80

!------------------------------------------------------------------------------
! smooth polyline (x,y) with a Chaikin corner cutting see
!     https://www.codeproject.com/Articles/1093960/D-Polyline-Vertex-Smoothing
!------------------------------------------------------------------------------

subroutine smooth_it_Chaikin (i_start, i_end, tension, niterations, x, y)

  integer, intent(in) :: i_start, i_end, niterations
  double precision, dimension(:), intent(inout) :: x, y
  double precision, intent(in) :: tension

  double precision, dimension(:), allocatable :: x_in, y_in
  double precision, dimension(:), allocatable :: x_out, y_out
  double precision  :: cuttingDist
  integer :: i, npoints

    
  ! the tension factor defines a scale between corner cutting distance in segment half length,
  ! i.e. between 0.05 and 0.45. The opposite corner will be cut by the inverse
  ! (i.e. 1-cutting distance) to keep symmetry.
  ! with a tension value of 0.5 this amounts to 0.25 = 1/4 and 0.75 = 3/4,
  ! the original Chaikin values

  cuttingDist = 0.05d0 + (tension*0.4d0)
  npoints = i_end - i_start + 1

  allocate (x_in(npoints))
  allocate (y_in(npoints))

  ! cut the area to smooth out of the original polyline
  x_in = x(i_start : i_end)
  y_in = y(i_start : i_end) 
  npoints = i_end - i_start + 1
  
  do i = 1, niterations
    call getSmootherChaikin(x_in, y_in, cuttingDist, x_out, y_out)
    x_in = x_out
    y_in = y_out
  end do
  
  ! replace the area to smooth in original polyline with the smoothed result
  call interp_vector(x_in, y_in, x(i_start : i_end), y(i_start : i_end))

end subroutine smooth_it_Chaikin

! Core smoothing function 
Subroutine getSmootherChaikin(x, y, cuttingDist, x_smooth, y_smooth)
    
  double precision, dimension(:), intent(in) :: x, y
  double precision, dimension(:), allocatable, intent(out) :: x_smooth, y_smooth
  double precision, intent(in) :: cuttingDist
  double precision :: local_cuttingDist, delta_cutdist

  integer :: i, is, np_smooth, npt
  
  npt       = size(x)
  np_smooth = (npt-1)*2+1

  allocate (x_smooth(np_smooth))
  allocate (y_smooth(np_smooth))

  ! The original Chaikin is modifiied to have a much smaller cutting distance
  ! at the first points of the smoothed range to achieve a continuos transition
  ! from non smoothed to smoothed. Especially critical in LE with high curvature!
  delta_cutdist     = cuttingDist / 25
  local_cuttingDist = delta_cutdist       ! start value for cutting distance
                                          ! will grow up to 'cuttingDist'

  ! always add the first point - this won't be changed
  x_smooth(1) = x(1)
  y_smooth(1) = y(1)

  is = 1
  do i = 1, (npt-1)

    is = is + 1
    x_smooth(is) = (1-local_cuttingDist) * x(i) + local_cuttingDist * x(i+1)
    y_smooth(is) = (1-local_cuttingDist) * y(i) + local_cuttingDist * y(i+1)

    is = is + 1
    x_smooth(is) = local_cuttingDist * x(i) + (1-local_cuttingDist) * x(i+1)
    y_smooth(is) = local_cuttingDist * y(i) + (1-local_cuttingDist) * y(i+1)

    if (local_cuttingDist < cuttingDist) then     ! increase cutting distance going backwards from LE
      local_cuttingDist = local_cuttingDist + delta_cutdist
      delta_cutdist = delta_cutdist * 1.0d0       ! ramp up cutting_dist smoothly
    else  
      local_cuttingDist = cuttingDist
    end if 

  end do

  ! always add the last point so it never will be changed
  x_smooth(np_smooth) = x(npt)
  y_smooth(np_smooth) = y(npt)

end subroutine


!------------------------------------------------------------------------------
! Counts reversals and high/lows of 2nd derivative of polyline (x,y)
!    
! To find the "real" reversals only curve value greater curve_threshold
!    are taken to check against the change from + to -
! To find high/lows all high/lows of 2nd derivation with a distance
!    greater highlow_treshold are taken
!
! result_info holds a string of npoints for user entertainment 
!------------------------------------------------------------------------------
subroutine find_curvature_reversals(npt, i_start, highlow_treshold, curve_threshold, x, y, nhighlows2, nreversals2, result_info)

  integer, intent(in) :: npt, i_start
  double precision, intent(in) :: curve_threshold, highlow_treshold
  double precision, dimension(npt), intent(in) :: x, y
  integer, intent(out) :: nreversals2, nhighlows2
  character (npt), intent(inout) :: result_info

  double precision, dimension(npt) :: deriv2
  double precision :: curv1, d_minmax, d_cur
  integer :: i, i_first, nhighs, nlows, i_firstHighLow, i_max, i_min
                                        ! strange: Looking at the derivative plot
                                        ! highs and lows are just the opposite
                                        ! --> hack the character (don't know the reason)
  character (1) ::  reversal_sign = 'R', high_point = 'L', low_point = 'H', prev_highlow

  nreversals2 = 0
  nhighs = 0
  nlows = 0
  nhighlows2 = 0

  curv1 = 0.d0
  i_first = max (i_start, 1)        
  i_firstHighLow = i_first +1       ! ... highLow detection needs point (i-1)

  deriv2 = derivation2(npt, x, y)   ! get 2nd derivation 

  i_min = i_firstHighLow
  i_max = i_firstHighLow
  prev_highlow = ' '

  ! find the highs and lows in 2nd derivation - dist between must be > highlow_treshold 
  do i = i_firstHighLow, npt

    ! low detect
    if ( deriv2(i) - deriv2(i-1)> 0.d0 ) then             !.. going up now
      d_minmax = abs(deriv2(i_max) - deriv2(i_min))       ! diff between last min-max
      d_cur    = abs(deriv2(i_min) - deriv2(i))           ! diff between current -lastmin
      if (((d_minmax > highlow_treshold) .and. (d_cur > highlow_treshold)) & 
         .and. (prev_highlow /= low_point)) then          ! no succeding lows
        if (prev_highlow /= ' ') then                     ! skip count for very first point
          ! we passed a low - the curve is going highlow_treshold up
          nlows = nlows + 1
          result_info (i_min:i_min) = low_point
        end if 
        prev_highlow = low_point
        i_max = i
      else
        if (deriv2(i) >deriv2(i_max)) i_max = i
      end if
    ! high detect
    else                                                  ! going down
      d_minmax = abs(deriv2(i_max) - deriv2(i_min))       ! diff betwenn min-max
      d_cur    = abs(deriv2(i_max) - deriv2(i))           ! diff current-max
      if (((d_minmax > highlow_treshold) .and. (d_cur > highlow_treshold)) &
        .and. (prev_highlow /= high_point)) then          ! no succeding highs
        ! we passed a high - the curve is going highlow_treshold down
        if (prev_highlow /= ' ') then                     ! skip count for very first point
          nhighs = nhighs + 1
          result_info (i_max:i_max) = high_point
        end if  
        prev_highlow = high_point
        i_min = i
      else
        if (deriv2(i) < deriv2(i_min)) i_min = i
      end if
    end if  
  end do
  nhighlows2 = nlows + nhighs

  ! get the real reversals with curve values from + to - 
  !    just the same as in the original function 
  !    (npt-1) --> don't take last spike value as reversal 
  do i = i_first, npt-1
    if (abs(deriv2(i)) >= curve_threshold) then
      if (deriv2(i) * curv1 < 0.d0) then 
        nreversals2 = nreversals2 + 1
        result_info (i:i) = reversal_sign
      end if
      curv1 = deriv2(i)
    end if
  end do

end subroutine find_curvature_reversals


!------------------------------------------------------------------------------
! counts der curvature spikes of polyline (x,y)
!     which a reversals of the third derivation 
!------------------------------------------------------------------------------
subroutine find_curvature_spikes(npt, i_start, spike_threshold, x, y, nspikes, result_info)

  integer, intent(in) :: npt, i_start
  double precision, intent(in) :: spike_threshold
  double precision, dimension(npt), intent(in) :: x, y
  integer, intent(out) :: nspikes
  character (npt), intent(inout) :: result_info

  double precision, dimension(npt) :: deriv3
  double precision :: prev_deriv3
  integer :: i, i_first
  character (1) :: spikeSign = 's'

  nspikes = 0

  i_first = max (i_start, 1)            
  prev_deriv3 = 0.d0
  deriv3 = derivation3(npt, x, y)

  do i = i_first, npt
    if (abs(deriv3(i)) >= spike_threshold) then
      if (deriv3(i) * prev_deriv3 < 0.d0) then
        nspikes = nspikes + 1
        result_info (i:i) = spikeSign
      end if
      prev_deriv3 = deriv3(i)
    end if
  end do

end subroutine find_curvature_spikes

!------------------------------------------------------------------------------
! get third derivative of polyline (x,y)
!     ! only approx because 1st derivative of 2nd derivative is used 
!------------------------------------------------------------------------------
function derivation3(npt, x, y)

  integer, intent(in) :: npt
  double precision, dimension(npt), intent(in) :: x, y
  double precision, dimension(npt) :: derivation3

  derivation3 = derivation1(npt, x, derivation2(npt, x, y))
 
end function derivation3

!------------------------------------------------------------------------------
! get second derivative of polyline (x,y)
!     the original Xoptfoil function is used (after some false tries ;-)) 
!------------------------------------------------------------------------------
function derivation2(npt, x, y)

  integer, intent(in) :: npt
  double precision, dimension(npt), intent(in) :: x, y
  double precision, dimension(npt) :: derivation2

  ! --> use Dans original function 
  derivation2 = curvature(npt, x, y)

end function derivation2

!------------------------------------------------------------------------------
! get first derivative of polyline (x,y)
!     using Backward, center, forward adapted difference approximation 
!     based on "A simple finite-difference grid with non-constant intervals"
!               by HILDING SUNDQVIST
!     and on "http://web.media.mit.edu/~crtaylor/calculator.html"
!------------------------------------------------------------------------------
function derivation1(npt, x, y)

  integer, intent(in) :: npt
  double precision, dimension(npt), intent(in) :: x, y
  double precision, dimension(npt) :: derivation1
  integer :: i
  double precision :: h_minus, h, hr
!  double precision :: h_2minus, h_plus
 
  do i = 1, npt
    if (i == 1) then                                                 ! forward
! jx-mod formula for forward is wrong! - take simple form
!      h      = x(i+1) - x(i)
!      h_plus = x(i+2) - x(i+1)
!      hr      = h_plus / h 
!      derivation1(i) = (-y(i+2) - 3.d0*hr*hr*y(i) + 4.d0*(1-hr*hr)*y(i+1))/ (h_plus * (1.d0 +hr)) 
      derivation1(i) = (y(i+1) - y(i))/ (x(i+1) - x(i)) 
    else if (i ==npt) then                                           ! backward
! jx-mod formula for backward is wrong! - take simple form
!      h_minus  = x(i) - x(i-1)
!      h_2minus = x(i-1) - x(i-2)
!      hr      = h_minus / h_2minus 
!      derivation1(i) = (3.d0*y(i) + hr*hr*y(i-2) -4.d0*(1-hr*hr)*y(i-1))/ (h_minus * (1.d0 +hr)) 
      derivation1(i) = (y(i) - y(i-1))/ (x(i) - x(i-1))
    else                                                             ! center
      h       = x(i+1) - x(i)
      h_minus = x(i) - x(i-1)
      hr      = h / h_minus 
      derivation1(i) = (y(i+1) - hr*hr*y(i-1) -(1-hr*hr)*y(i))/ (h * (1.d0 +hr)) 
    end if 
  end do

end function derivation1

!------------------------------------------------------------------------------
! get first derivative of a polyline (x,y) at point i
!     using Backward, center, forward adapted difference approximation 
!     For center the mean of backward and forward approximation is taken
!     which could be more precise ...
!------------------------------------------------------------------------------
function derivation_at_point (npt, i, x, y)

  integer, intent(in) :: npt, i 
  double precision, dimension(npt), intent(in) :: x, y
  double precision :: derivation_at_point

  derivation_at_point = 0.d0
  if (i < npt) then
    ! Forward
    if (x(i+1) > x(i)) then
      derivation_at_point = (y(i+1) - y(i))/(x(i+1)-x(i))
    else
      derivation_at_point = (y(i) - y(i+1))/(x(i)-x(i+1))
    end if
  end if

  if (i > 1) then
    ! Backward
    if (x(i) > x(i-1)) then
      derivation_at_point = derivation_at_point + (y(i) - y(i-1))/(x(i)-x(i-1))
    else
      derivation_at_point = derivation_at_point + (y(i-1) - y(i))/(x(i-1)-x(i))
    end if
  end if
  
  ! center - tage mean value of both 
  if ( (i < npt) .and. (i > 1) )               &
  derivation_at_point = derivation_at_point/2.d0 

end function derivation_at_point

!------------------------------------------------------------------------------
! Interpolate a point (xnew, ynew) within a vector (x,y) - returns ynew
!------------------------------------------------------------------------------
function interp_point(x, y, xnew)

  double precision, dimension(:), intent(in) :: x, y 
  double precision, intent(in)  :: xnew
  double precision :: interp_point
  
  logical :: isbtwn
  integer :: pt1, npt

  npt = size(x,1)
  pt1 = 1

! Find interpolants
  isbtwn = .false.
  do while (.not. isbtwn .and. (pt1 < npt))
    isbtwn = between(x(pt1), xnew, x(pt1+1))
    if (.not. isbtwn) then
      pt1 = pt1 + 1
      if (pt1 == npt) then
        write(*,*)
        write(*,*) 'Warning: could not find interpolants.'
        write(*,*) 'x: ', xnew, 'xmax: ', x(npt)
        stop
      end if
    end if
  end do

! Interpolate points
  interp_point = interp1(x(pt1), x(pt1+1), xnew, y(pt1), y(pt1+1))

end function interp_point

!=============================================================================80
! jx-mod Smoothing - End Additional functions 
!=============================================================================80


end module math_deps

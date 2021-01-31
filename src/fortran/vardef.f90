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

module vardef

! Data structures for airfoil optimization code

  implicit none

  type airfoil_type
    character(80) :: name                               ! name of the airfoil
    integer :: npoint                                   ! number of points
    double precision, dimension(:), allocatable :: x, z ! airfoil coordinates
 
    double precision :: xle, zle                        ! leading edge coords
    integer :: leclose                                  ! index closest to LE
    integer :: addpoint_loc                             ! whether to add point for LE before or after leclose
                                                        ! coordinates of top and bot side
    double precision, dimension(:), allocatable :: xb, xt, zb, zt 

  end type airfoil_type

! jx-mod Geo targets - type
  type geo_target_type
    character(30) :: type                               ! eg 'zBot' zTop'
    double precision :: x                               ! x-value of target
    double precision :: target_value                    ! target value to achieve
    double precision :: seed_value                      ! the value of the seed airfoil
    double precision :: reference_value                 ! to scale improvement (depends on type)
    double precision :: weighting                       ! weighting within objective function
    double precision :: scale_factor                    ! scale for objective function
  end type geo_target_type

! jx-mod Reynolds-Type to handle Type 1 (fixed speedI and Type 2 (fixed lift) 
!        also used for Mach Number 
  type re_type
    double precision :: number                           ! Reynolds Number
    integer          :: type                             ! Type 
  end type re_type

! Global variables (mainly needed to preserve generality of optimization
! routines)

  integer :: npan_fixed = 0          ! set a fixed number of panels which can't be changed

  integer :: noppoint
  integer, parameter :: max_op_points = 30
  character(7), dimension(max_op_points) :: op_mode
  character(8), dimension(max_op_points) :: flap_selection
  double precision, dimension(max_op_points) :: op_point,      &
                                 flap_degrees, weighting, scale_factor, ncrit_pt
  type (re_type), dimension(max_op_points)   :: re, ma

  double precision, dimension(max_op_points) :: target_value

  double precision :: x_flap, y_flap
  character(3) :: y_flap_spec
  logical :: use_flap
  character(15), dimension(max_op_points) :: optimization_type
  integer :: nflap_optimize          ! Number of operating points where flap 
                                     !   setting will be optimized
  integer, dimension(max_op_points) :: flap_optimize_points

  type(airfoil_type) :: seed_foil, seed_foil_not_smoothed

  double precision :: min_thickness, max_thickness, min_te_angle,              &
                      growth_allowed, min_flap_degrees, max_flap_degrees,      &
                      min_camber, max_camber
  double precision :: max_te_curvature

  logical :: check_curvature, auto_curvature
  double precision :: curv_threshold
  integer :: max_curv_reverse_top, max_curv_reverse_bot
  integer :: max_curv_highlow_top, max_curv_highlow_bot

  character(8), dimension(max_op_points) :: moment_constraint_type
  double precision, dimension(max_op_points) :: min_moment
  character(16) :: shape_functions

! Match foil mode
  type(airfoil_type) :: foil_to_match
  logical :: match_foils
  double precision :: match_foils_scale_factor 
  
  logical :: symmetrical

  integer :: nparams_top, nparams_bot
  double precision :: initial_perturb
  double precision :: min_bump_width

  character(80) :: output_prefix

  integer :: naddthickconst
  integer, parameter :: max_addthickconst = 10
  double precision, dimension(max_addthickconst) :: addthick_x, addthick_min,  &
                                                    addthick_max

! jx-mod Geo targets - new vars
  integer :: ngeo_targets
  integer, parameter :: max_geo_targets = 10
  type(geo_target_type), dimension(max_geo_targets) :: geo_targets
                        
! jx-mod Show more infos during optimization
  logical :: show_details

! jx-mod Smoothing - parameters for smoothing 
  double precision :: spike_threshold, highlow_threshold
  logical :: do_smoothing

! mb-mod dynamic weighting
  type dynamic_weighting_type
    double precision, dimension(max_op_points) :: weighting  
    double precision :: medium_deviation_abs
  end type dynamic_weighting_type

  logical :: dynamic_weighting
  double precision :: dynamic_weighting_p_factor

end module vardef

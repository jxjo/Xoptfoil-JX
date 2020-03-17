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
    integer :: npoint
    double precision, dimension(:), allocatable :: x, z ! Airfoil coordinates
    double precision :: xle, zle                        ! Leading edge coords
    integer :: leclose                                  ! Index closest to LE
    integer :: addpoint_loc                             ! Whether to add point 
                                                        !  for LE before or
                                                        !  after leclose
  end type airfoil_type

! jx-mod Geo targets - type
  type geo_target_type
    character(10) :: type                               ! eg 'zBot' zTop'
    double precision :: x                               ! x-value of target
    double precision :: target_value                    ! target value to achieve
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

  integer :: noppoint
  integer, parameter :: max_op_points = 30
  double precision, dimension(:), allocatable :: xseedt, xseedb, zseedt, zseedb
  character(7), dimension(max_op_points) :: op_mode
  character(8), dimension(max_op_points) :: flap_selection
  double precision, dimension(max_op_points) :: op_point,      &
                                 flap_degrees, weighting, scale_factor, ncrit_pt
  type (re_type), dimension(max_op_points)   :: re, ma

! jx-mod Aero targets - new Option: target_value for op_mode target-moment and target-drag
  double precision, dimension(max_op_points) :: target_value

  double precision :: x_flap, y_flap
  character(3) :: y_flap_spec
  logical :: use_flap
  character(15), dimension(max_op_points) :: optimization_type
  integer :: nflap_optimize          ! Number of operating points where flap 
                                     !   setting will be optimized
  integer, dimension(max_op_points) :: flap_optimize_points
  double precision :: xoffset, zoffset, foilscale

  type(airfoil_type) :: curr_foil
  double precision :: min_thickness, max_thickness, min_te_angle,              &
                      growth_allowed, min_flap_degrees, max_flap_degrees,      &
                      min_camber, max_camber
  double precision :: curv_threshold
  integer :: max_curv_reverse_top, max_curv_reverse_bot
  character(8), dimension(max_op_points) :: moment_constraint_type
  double precision, dimension(max_op_points) :: min_moment
  character(11) :: shape_functions
  double precision, dimension(:), allocatable :: xmatcht, xmatchb, zmatcht,    &
                                                 zmatchb
  logical :: match_foils
  logical :: check_curvature
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
  double precision :: spike_threshold, highlow_treshold
  logical :: do_smoothing
  double precision :: scale_pertubation, weighting_smoothing
  double precision, dimension(:), allocatable :: zseedt_not_smoothed, zseedb_not_smoothed
  
!$omp threadprivate(curr_foil)

end module vardef

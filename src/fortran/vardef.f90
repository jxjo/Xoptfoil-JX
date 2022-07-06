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

  implicit none

! Specify an airfoil
  type airfoil_type 
    character(80) :: name                               ! name of the airfoil
    integer :: npoint                                   ! number of points
    double precision, dimension(:), allocatable :: x, z ! airfoil coordinates
    double precision :: xle, zle                        ! leading edge coords
    integer :: leclose                                  ! index closest to LE
    integer :: addpoint_loc                             ! whether to add point for LE 
    logical :: symmetrical                              ! airfoil symmetrical? -> bot equals top side
    double precision, dimension(:), allocatable :: xb, xt, zb, zt 
    ! character, dimension(:), allocatable :: curve_info  ! #todo Info on rversals, spikes...
  end type airfoil_type

! Specify flap
  type flap_spec_type
    logical          :: use_flap
    double precision :: x_flap, y_flap 
    character(3)     :: y_flap_spec
  end type flap_spec_type
  
! --------------------------------------------------------------------------------

! Global variables (mainly needed to preserve generality of optimization routines)

  integer :: npan_fixed = 0          ! set a fixed number of panels which can't be changed
  integer, parameter :: max_op_points = 30

! Parms for flap handling
  type (flap_spec_type) :: flap_spec
  character(8), dimension(max_op_points) :: flap_selection
  double precision, dimension(max_op_points) :: flap_degrees
  double precision :: min_flap_degrees, max_flap_degrees
  integer :: nflap_optimize          ! Number of op_poins where flap will be optimized
  integer, dimension(max_op_points) :: flap_optimize_points


! Parms for shaping geomtery 
  integer :: nparams_top, nparams_bot
  double precision :: initial_perturb
  double precision :: min_bump_width
  character(16)    :: shape_functions

! Airfoils
  type(airfoil_type) :: seed_foil, seed_foil_not_smoothed

! Show more infos during optimization
  logical :: show_details

! Subdirectory for all the mep design files
  character (*), parameter :: DESIGN_SUBDIR_POSTFIX = '_temp'
  character (255)          :: design_subdir
  character (80)           :: output_prefix


! Global Constants
  double precision, parameter :: NOT_DEF_D = -99999d0
  double precision, parameter :: NOT_DEF_I = -99999

end module vardef

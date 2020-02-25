! ***************************************************************
!
!
!   work in progress
!
!
! ***************************************************************



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

program xfoil_worker

! jx-mod ---------------------------------------------------------------------
!
! Runs xfoil by for an airfoil, but still uses the same input file as xoptfoil
!    - Modify thickness and camber
!    - (generate polar file for xflr5)
! jx-mod ---------------------------------------------------------------------

  use vardef
  use input_output,       only : read_inputs, read_clo
  use particle_swarm,     only : pso_options_type
  use genetic_algorithm,  only : ga_options_type
  use simplex_search,     only : ds_options_type
  use airfoil_evaluation, only : xfoil_options, xfoil_geom_options
  use memory_util,        only : deallocate_airfoil
  use airfoil_operations, only : load_airfoil
  use naca,               only : naca_options_type, naca_456
  use xfoil_driver,       only : run_xfoil, xfoil_geometry_info, xfoil_init,   &
                                 xfoil_cleanup

  implicit none

  type(airfoil_type) :: foil
  character(80) :: search_type, global_search, local_search, seed_airfoil,     &
                   airfoil_file, matchfoil_file
  character(80) :: input_file
  type(naca_options_type) :: naca_options
  type(pso_options_type) :: pso_options
  type(ga_options_type) :: ga_options
  type(ds_options_type) :: ds_options
  integer, dimension(:), allocatable :: constrained_dvs
  integer :: restart_write_freq, i
  logical :: restart
  double precision, dimension(:), allocatable :: alpha, lift, drag, moment,    &
                                                 viscrms, xtrt, xtrb
  double precision :: maxt, xmaxt, maxc, xmaxc
  character(30) :: text

! Set default names and read command line arguments

  input_file = 'inputs.txt'
  output_prefix = 'foil'
  call read_clo(input_file, output_prefix, "xfoil_worker")

! Read inputs from namelist file

  call read_inputs(input_file, search_type, global_search, local_search,       &
                   seed_airfoil, airfoil_file, nparams_top, nparams_bot,       &
                   restart, restart_write_freq, constrained_dvs, naca_options, &
                   pso_options, ga_options, ds_options, matchfoil_file)

! Allocate some things

  allocate(alpha(noppoint))
  allocate(lift(noppoint))
  allocate(drag(noppoint))
  allocate(moment(noppoint))
  allocate(viscrms(noppoint))
  allocate(xtrt(noppoint))
  allocate(xtrb(noppoint))

! Get airfoil to analyze, but don't do any transformations

  if (trim(seed_airfoil) == "from_file") then
    call load_airfoil(airfoil_file, foil)
  else if (trim(seed_airfoil) == "naca") then
    call naca_456(naca_options, 200, foil)
  end if

! Allocate xfoil variables

  call xfoil_init()

! jx-mod Testfunction for thickness

  call test_set_thickness_camber (foil)

! Run xfoil

!  call run_xfoil(foil, xfoil_geom_options, op_point(1:noppoint),               &
!                 op_mode(1:noppoint), reynolds(1:noppoint), mach(1:noppoint),  &
!                 use_flap, x_flap, y_flap, y_flap_spec,                        &
!                 flap_degrees(1:noppoint), xfoil_options, lift, drag, moment,  &
!                 viscrms, alpha, xtrt, xtrb, ncrit_pt(1:noppoint))


  
! Deallocate xfoil variables

  call xfoil_cleanup()

! Deallocate some things

  call deallocate_airfoil(foil)
  deallocate(alpha)
  deallocate(lift)
  deallocate(drag)
  deallocate(moment)
  deallocate(viscrms)
  deallocate(xtrt)
  deallocate(xtrb)

end program xfoil_worker


! jx-mod Testing purposes 


subroutine test_set_thickness_camber (foil)

  use vardef,    only : airfoil_type, output_prefix
  use xfoil_driver,       only : xfoil_set_thickness_camber, smooth_paneling
  use xfoil_driver,       only : xfoil_scale_thickness_camber
  use airfoil_operations, only : airfoil_write

  type(airfoil_type), intent(in) :: foil
  double precision :: maxt, xmaxt, maxc, xmaxc
  double precision :: f_thick, d_xthick, f_camb, d_xcamb
  character(80) :: output_file
  type(airfoil_type) ::foilsmoothed, outfoil
  integer :: i
  character(len=5) :: charI

  maxt = 9.d-2
  xmaxt = 30.d-2
  maxc = 2.5d-2
  xmaxc = 45.d-2

  f_thick  =  0.95d0
  d_xthick = -0.02d0
  f_camb   =  0.95d0
  d_xcamb  = -0.05d0

  ! Use Xfoil to smooth airfoil paneling
  call smooth_paneling(foil, 200, foilsmoothed)

  call xfoil_set_thickness_camber (foilsmoothed, maxt, xmaxt, maxc, xmaxc, outfoil)

  foilsmoothed = outfoil
  
  do i = 1, 6

    write(charI,"(I0)") i
    output_file = trim(output_prefix)//trim(charI) //'.dat'
    call airfoil_write(output_file, trim(output_prefix)//trim(charI), outfoil)

    call xfoil_scale_thickness_camber (foilsmoothed, f_thick, d_xthick, f_camb, d_xcamb, outfoil)

    f_thick  =  f_thick - 0.05d0
    d_xthick = d_xthick - 0.02d0
    f_camb   =   f_camb - 0.05d0
    d_xcamb  =  d_xcamb - 0.04d0
  
  end do 

end subroutine test_set_thickness_camber 

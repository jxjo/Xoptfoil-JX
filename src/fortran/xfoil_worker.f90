!-------------------------------------------------------------------------
!
!   Xfoil-Worker
!
!   Utility Functions based on xfoil to complement Xoptfoil-JX
!   work in progress
!   Xfoil-Worker uses an Xoptfoil input-file to get the paramters.
!     only a few sections are needed
!   
!   -o polars    generate polar (set)for an airfoil
!                inputs.txt namelists used: polar_generation, xfoil_run_options        
!   
!-------------------------------------------------------------------------

!  This file is part of XOPTFOIL-JX.

!  XOPTFOIL(-JX) is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.
!  XOPTFOIL(-JX) is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!  You should have received a copy of the GNU General Public License
!  along with XOPTFOIL.  If not, see <http://www.gnu.org/licenses/>.
!  Copyright (C) 2017-2019 Daniel Prosser
!  Copyright (C) 2020      Jochen Guenzel

program xfoil_worker

! jx-mod ---------------------------------------------------------------------
!
! Runs xfoil by for an airfoil, but still uses the same input file as xoptfoil
!    - Modify thickness and camber
!    - (generate polar file for xflr5)
! jx-mod ---------------------------------------------------------------------

  use vardef,             only : airfoil_type
  use memory_util,        only : deallocate_airfoil
  use airfoil_operations, only : load_airfoil, my_stop, airfoil_write
  use xfoil_driver,       only : xfoil_init, xfoil_cleanup 
  use xfoil_driver,       only : xfoil_set_buffer_airfoil, xfoil_reload_airfoil
  use polar_operations,   only : check_and_do_polar_generation


  implicit none

  type(airfoil_type) :: foil
  character(80)      :: input_file, output_prefix, action, airfoil_filename


! Set default names and read command line arguments

  input_file        = 'inputs.txt'
  output_prefix     = 'foil'
  action            = ''
  airfoil_filename  = ''
  call read_worker_clo(input_file, output_prefix, airfoil_filename, action)

  if (trim(action) == "") &
    call my_stop("Must specify an action for the worker with -w option.")
  if (trim(airfoil_filename) == "") &
    call my_stop("Must specify an airfoil file with the -a option.")

! Do actions according command line option

  select case (trim(action)) 
    case ('polar')

    ! Load airfoil defined in command line 
      call load_airfoil(airfoil_filename, foil)

    ! Allocate xfoil variables
      call xfoil_init()

    ! Do work - generate poalrs in subdirectory ".\<output_prfix>_polars\*.*
      call check_and_do_polar_generation (input_file, output_prefix, foil)

    ! Deallocate xfoil variables
      call xfoil_cleanup()
      call deallocate_airfoil (foil)

    case ('test')

      ! Test for change max thickness location 
      airfoil_filename = 'JX-FXrcn-15.dat'
      output_prefix = 'FX-thick-test'
      call load_airfoil(airfoil_filename, foil)
      call xfoil_init()
      call xfoil_set_buffer_airfoil (foil)

      call HIPNT (0.3d0, 0.25d0)

      call xfoil_reload_airfoil(foil)
      call airfoil_write (trim(output_prefix)//'.dat', output_prefix, foil)
    
      call xfoil_cleanup()
      call deallocate_airfoil (foil)


    case default

      call print_worker_usage()


  end select 

end program xfoil_worker

!-------------------------------------------------------------------------
! Reads command line arguments for input file name and output file prefix
!-------------------------------------------------------------------------

subroutine read_worker_clo(input_file, output_prefix, airfoil_name, action)

  use airfoil_operations, only : my_stop

  character(*), intent(inout) :: input_file, output_prefix, action, airfoil_name

  character(80) :: arg
  integer i, nargs
  logical getting_args

  nargs = iargc()
  if (nargs > 0) then
    getting_args = .true.
  else
    getting_args = .false.
  end if

  i = 1
  do while (getting_args)
    call getarg(i, arg) 

    if (trim(arg) == "-i") then
      if (i == nargs) then
        call my_stop("Must specify an input file with -i option.")
      else
        call getarg(i+1, input_file)
        i = i+2
      end if
    else if (trim(arg) == "-o") then
      if (i == nargs) then
        call my_stop("Must specify an output prefix with -o option.")
      else
        call getarg(i+1, output_prefix)
        i = i+2
      end if
    else if (trim(arg) == "-r") then
      if (i == nargs) then
        call my_stop("Must specify a re value for -r option.")
      else
        call getarg(i+1, arg)
        i = i+2
      end if
    else if (trim(arg) == "-a") then
      if (i == nargs) then
        call my_stop("Must specify filename of seed airfoil for -a option.")
      else
        call getarg(i+1, airfoil_name)
        i = i+2
      end if
    else if (trim(arg) == "-w") then
      if (i == nargs) then
        call my_stop("Must specify an action for the worker e.g. polar")
      else
        call getarg(i+1, action)
        i = i+2
      end if
!    else if ( (trim(arg) == "-v") .or. (trim(arg) == "--version") ) then
!      call print_version()
!      stop
    else if ( (trim(arg) == "-h") .or. (trim(arg) == "--help") ) then
      call print_worker_usage
      stop
    else
      write(*,'(A)') "Unrecognized option "//trim(arg)//"."
      call print_worker_usage
      stop 1
    end if

    if (i > nargs) getting_args = .false.
  end do

end subroutine read_worker_clo

!-------------------------------------------------------------------------
! Print usage information of worker
!-------------------------------------------------------------------------

subroutine print_worker_usage()

  write(*,'(A)') "Usage: Xfoil_worker -w worker_action [OPTION]"
  write(*,'(A)')
  write(*,'(A)') "  -w worker_action Specify an action e.g. polar"
  write(*,'(A)')
  write(*,'(A)') "Options:"
  write(*,'(A)') "  -i input_file     Specify a non-default input file"
  write(*,'(A)') "  -o output_prefix  Specify a non-default output prefix"
  write(*,'(A)') "  -r xxxxxx         Specify a default reynolds number (re_default)"
  write(*,'(A)') "  -a airfoil_file   Specify filename of seed airfoil"
  write(*,'(A)') "  -h, --help        Display usage information and exit"
!  write(*,'(A)') "  -v, --version     Display Xoptfoil version and exit"
  write(*,'(A)')
  write(*,'(A)') "Refer to the PDF reference guide for complete input help."
  write(*,'(A)')

end subroutine print_worker_usage


!===========================================================================
! jx-mod Testing purposes 
!===========================================================================

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

  ! Use Xfoil to smooth airfoil paneling
  ! call smooth_paneling(foil, 200, foilsmoothed)

  call xfoil_set_thickness_camber (foilsmoothed, maxt, xmaxt, maxc, xmaxc, outfoil)

  foilsmoothed = outfoil
  f_thick  =  0.95d0
  d_xthick = -0.02d0
  f_camb   =  0.95d0
  d_xcamb  = -0.05d0
  
!  do i = 1, 6
!
!    write(charI,"(I0)") i
!    output_file = trim(output_prefix)//trim(charI) //'.dat'
!    call airfoil_write(output_file, trim(output_prefix)//trim(charI), outfoil)
!
!    call xfoil_scale_thickness_camber (foilsmoothed, f_thick, d_xthick, f_camb, d_xcamb, outfoil)
!
!    f_thick  =  f_thick - 0.05d0
!    d_xthick = d_xthick - 0.02d0
!    f_camb   =   f_camb - 0.05d0
!    d_xcamb  =  d_xcamb - 0.04d0
!  
!  end do 

end subroutine test_set_thickness_camber 

subroutine test_set_LE_radius (foil)

  use vardef,    only : airfoil_type, output_prefix
  use xfoil_driver,       only : smooth_paneling
  use xfoil_driver,       only : xfoil_scale_LE_radius
  use airfoil_operations, only : airfoil_write

  type(airfoil_type), intent(in) :: foil
  double precision :: f_radius, x_blend
  character(80) :: output_file
  type(airfoil_type) ::foilsmoothed, outfoil
  integer :: i
  character(len=5) :: charI

  f_radius  =  1.0d0
  x_blend  =  0.1d0

  ! Use Xfoil to smooth airfoil paneling
  call smooth_paneling(foil, 200, foilsmoothed)

  do i = 1, 6

    call xfoil_scale_LE_radius (foilsmoothed, f_radius, x_blend, outfoil)

    write (*,*) i, 'LE scaled by ', f_radius, '    x-blending  ', x_blend
  
    write(charI,"(I0)") i
    output_file = trim(output_prefix)//trim(charI) //'.dat'
    call airfoil_write(output_file, trim(output_prefix)//trim(charI), outfoil)

    f_radius  =  f_radius + 0.05d0
    x_blend = x_blend + 0.0d0
  
  end do 

end subroutine test_set_LE_radius

subroutine test_color () 

  use os_util, only: print_colored
  call print_colored (4 , 'Ist das Grün?')

end subroutine test_color



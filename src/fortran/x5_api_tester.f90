!------------------------------------------------------------------------------------------
!
!   X5_API_Tester      - Test the Xflr5 API to Xoptfoil-JX
!
!   This file is part of XOPTFOIL-JX.
!                       Copyright (C) 2017-2019 Daniel Prosser
!                       Copyright (C) 2021      Jochen Guenzel
!
!------------------------------------------------------------------------------------------

program x5_api_tester

  use vardef,             only : airfoil_type
  use os_util 
  use X5_API 
  use xfoil_driver,       only : xfoil_init, xfoil_cleanup


  use airfoil_operations, only : load_airfoil
  
  implicit none

#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION ""
#endif

  type(airfoil_type)      :: foil
  character(255)          :: input_file,airfoil_filename, action
  doubleprecision         :: obj, thickness_scale, scale_and_eval
  integer                 :: i

! Read command line options
  input_file        = 'inputs.txt'
  action            = ''
  airfoil_filename  = 'JX-GS-15.dat'
  call read_tester_clo(input_file, airfoil_filename, action)

! Let's start
  write (*,'(1x)', advance = 'no') 
  call print_colored (COLOR_PROGRAM,'Tester on '//trim(airfoil_filename))
  write (*,'(3x,A,A,3x)', advance = 'no') '- using ',trim(input_file)  

! Load airfoil defined in command line 
  call load_airfoil(airfoil_filename, foil)

! Init API 
  call x5_init (input_file, foil)


! Test: Eval objective function of seed ------------------------------------------

  obj = x5_eval_objective_function (foil)
  write (*,'(" - ",A,F9.6)') 'Seed objective function = ', obj

! Test: Loop single threaded change thickness -------------------------------------

  write (*,*)
  write (*,'(" - ",A)') 'Loop single htreaded  changing thickness ...'

  thickness_scale = 1d0
  do i = 1, 30
    thickness_scale = 1d0 + i * 0.001d0
    obj = scale_and_eval (thickness_scale, foil)
    write (*,'(5x,I4,3x,A,F5.3,5x,A,F9.6)') i, 'Thickness * ',thickness_scale, &
           '-> Objective function = ', obj
  end do


! Test: Loop parallel change thickness -------------------------------------------

  write (*,*)
  write (*,'(" - ",A)') 'Loop parallel changing thickness ...'

  call xfoil_cleanup()
  thickness_scale = 1d0
!$omp parallel default(shared) private(i, thickness_scale, obj)
  call xfoil_init()
!$omp do 
  do i = 1, 100
    thickness_scale = 1d0 + i * 0.001d0
    obj = scale_and_eval (thickness_scale, foil)
    write (*,'(5x,I4,3x,A,F5.3,5x,A,F9.6)') i, 'Thickness * ',thickness_scale, &
           '-> Objective function = ', obj
  end do
!$omp end do
!$omp end parallel

end program x5_api_tester




function scale_and_eval (scale, foil)

  use vardef,             only : airfoil_type
  use xfoil_driver,       only : xfoil_scale_thickness_camber
  use X5_API 

  type(airfoil_type), intent(in)  :: foil
  doubleprecision, intent(in)     :: scale
  doubleprecision                 :: scale_and_eval
  type(airfoil_type)      :: test_foil

  call xfoil_scale_thickness_camber (foil, scale, 0d0, 1d0, 0d0, test_foil)
  scale_and_eval = x5_eval_objective_function (test_foil) 

end function 


!-------------------------------------------------------------------------
! Reads command line arguments 
!-------------------------------------------------------------------------

subroutine read_tester_clo(input_file, airfoil_name, action)

  use airfoil_operations, only : my_stop
  use os_util

  character(*), intent(inout) :: input_file,airfoil_name, action

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
    else
      call print_error ("Unrecognized option: "//trim(arg))
      write (*,*)
      call print_tester_usage
      stop 1
    end if

    if (i > nargs) getting_args = .false.
  end do


end subroutine read_tester_clo

!-------------------------------------------------------------------------
! Print usage information of worker
!-------------------------------------------------------------------------

subroutine print_tester_usage()

write(*,'(A)') 'X5_API_Tester      Version '//trim(PACKAGE_VERSION)//  &
'              (c) 2021 Jochen Guenzel'
write(*,'(A)')
write(*,'(A)') "Usage: Xfoil_worker [-w worker_action] Parameters"
write(*,'(A)')
write(*,'(A)') "Parameters:"
write(*,'(A)') "  -i input_file     Specify an input file (default: 'inputs.txt')"
write(*,'(A)') "  -a airfoil_file   Specify filename of seed airfoil"
write(*,'(A)')
write(*,'(A)') "  -w an_action      ... do a certain test'"
write(*,'(A)')

end subroutine print_tester_usage


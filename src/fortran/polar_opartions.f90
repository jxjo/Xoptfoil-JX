!  This file is part of XOPTFOIL-JX.

!  XOPTFOIL-JX is a modified version of ...
!  XOPTFOIL is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.

!  Copyright (C) XOPTFOIL 2017-2019 Daniel Prosser
!  Copyright (C) XOPTFOIL-JX 2019-2020 Jochen Guenzel

module polar_operations

! Contains subroutines to create and write xfoil based polars

  implicit none

  type op_point_type
    double precision :: value, lift, drag, moment, viscrms, alpha, &
                        xtrt, xtrb, reynolds
  end type op_point_type


  type polar_type
    character(50) :: airfoil_name       ! Name of airfoil
    character(50) :: file_name          ! Name of polar file name 
    character(5)  :: type               ! Type of this polar whiche can be 
                                        ! 'Type1' - fixed speed or 'Type2' - fixed lift
    double precision :: reynolds        ! reynolds number of this polar (re*sqrt(cl) if Type2)
    double precision :: mach            ! mach number of this polar (mach*sqrt(cl) if Type2)
    double precision :: ncrit           ! ncrit of polar
    character(7)  :: base_value_type    ! base value of polar either 'spec_al' or 'spec_cl'
    double precision :: start_value     ! polar starting from ...
    double precision :: end_value       ! ... to end value ...
    double precision :: increment       ! ... incremented by 

    integer :: n_op_points              ! number of all op_poins of this polar
    type(op_point_type), dimension (:), allocatable :: op_points !array with all calculated op_points
   end type polar_type

contains

!=============================================================================
! Initialize polar data structure based calculated number of op_points
!=============================================================================

subroutine init_polar (polar)

  type (polar_type), intent (inout) :: polar
  double precision :: cur_value
  integer :: i 

! calc number of op_points 

  polar%n_op_points = get_n_op_points (polar) 
  if (polar%n_op_points >= 0) then
    allocate (polar%op_points(polar%n_op_points))
  else
    write (*,*) "Error: No valid value boundaries for polar"
    stop
  endif

! build this special xflr5 filename  T1_Re0.400_M0.00_N9.0.txt 

  polar%file_name = build_filename (polar)

! init op data points of polar

  if ((polar%type =='Type2') .and. (polar%base_value_type == 'spec_al')) &
    write (*,*) "Error: Type2 polar based on alpha currently not supported. Sorry!"
  cur_value = polar%start_value

  do i = 1, polar%n_op_points

    polar%op_points(i)%value    = cur_value
    if(polar%type =='Type1') then               ! Type1 polar
      polar%op_points(i)%reynolds = polar%reynolds
      if (polar%base_value_type == 'spec_al') then
        polar%op_points(i)%alpha = cur_value
      else
        polar%op_points(i)%lift  = cur_value
      end if 
    else                                        ! Type2 polar
      polar%op_points(i)%reynolds = polar%reynolds / (abs(cur_value) ** 0.5d0)                                       
      polar%op_points(i)%lift     = cur_value
    end if 
    cur_value = cur_value + polar%increment

end do

end subroutine init_polar



!=============================================================================
! Calculate polar for airfoil
!=============================================================================

subroutine calculate_polar (foil, polar)

  use vardef,             only : airfoil_type
  use xfoil_driver,       only : run_xfoil
  use airfoil_evaluation, only : xfoil_options, xfoil_geom_options

  type (polar_type), intent (inout) :: polar
  type (airfoil_type), intent (in)  :: foil

  character(7),     dimension(polar%n_op_points) :: op_modes
  double precision, dimension(polar%n_op_points) :: mach_numbers, flap_degrees
  double precision :: x_flap, y_flap
  character(3) :: y_flap_spec
  logical :: use_flap
  integer :: i

  write (*,'(/,A, F7.0)') ' Calculating polar '//polar%type//' Re = ', polar%reynolds

  op_modes (:)     = polar%base_value_type
  mach_numbers (:) = 0.d0
  flap_degrees (:) = 0.d0 
  use_flap         = .false. 
  x_flap           = 0.d0
  y_flap           = 0.d0
  y_flap_spec      = 'y/c'

  call run_xfoil(foil, xfoil_geom_options, polar%op_points%value,  op_modes,     &
    polar%op_points%reynolds, mach_numbers, use_flap, x_flap, y_flap,            &
    y_flap_spec, flap_degrees, xfoil_options,                                    &
    polar%op_points%lift, polar%op_points%drag, polar%op_points%moment,          &
    polar%op_points%viscrms, polar%op_points%alpha, polar%op_points%xtrt, polar%op_points%xtrb)

end subroutine calculate_polar



!------------------------------------------------------------------------------
! Write polar data in xfoil format to out_unit
!------------------------------------------------------------------------------
subroutine write_polar_data (out_unit, polar)

  type (polar_type), intent (in) :: polar
  integer,           intent (in) :: out_unit

  type (op_point_type) :: op
  integer              :: i 

! xflr5 example
! -
!  alpha     CL        CD       CDp       Cm    Top Xtr Bot Xtr   Cpmin    Chinge    XCp    
! ------- -------- --------- --------- -------- ------- ------- -------- --------- ---------
!  -1.400   0.0042   0.00513   0.00057  -0.0285  0.7057  0.2705  -0.9363   0.0000   7.0438
!   F8.3    F9.4     F10.5     F10.5     F9.4    F8.4    F8.4     F9.4     F9.4     F9.4     

  write (out_unit,'(A)') "  alpha     CL        CD       CDp       Cm    Top Xtr Bot Xtr   Cpmin    Chinge    XCp"
  write (out_unit,'(A)') " ------- -------- --------- --------- -------- ------- ------- -------- --------- ---------"

  do i = 1, polar%n_op_points

    op = polar%op_points(i)
    write (out_unit,  "(   F8.3,   F9.4,    F10.5,    F10.5,    F9.4,   F8.4,   F8.4,    F9.4,    F9.4,     F9.4)") &
                        op%alpha, op%lift, op%drag,    0d0,  op%moment,op%xtrt,op%xtrb,  0d0,     0d0,      0d0

  end do 

end subroutine write_polar_data



!------------------------------------------------------------------------------
! Write polar header in xfoil/xflr5 format to out_unit
!------------------------------------------------------------------------------
subroutine write_polar_header (out_unit, polar)

  type (polar_type), intent (in) :: polar
  integer,           intent (in) :: out_unit

! Example xflr5
!-
!xflr5 v6.47
!
! Calculated polar for: JX FXrcn 15
!
! 1 1 Reynolds number fixed          Mach number fixed         
!
! xtrf =   1.000 (top)        1.000 (bottom)
! Mach =   0.000     Re =     0.400 e 6     Ncrit =   9.000
!
!-
  write (out_unit,'(A)') "Xfoil_worker v1.0"
  write (out_unit,*)
  write (out_unit,'(A)') " Calculated polar for: "//trim(polar%airfoil_name)
  write (out_unit,*)
  if (polar%type == 'Type1' ) then 
    write (out_unit,'(A)') " 1 1 Reynolds number fixed          Mach number fixed"
  else
    write (out_unit,'(A)') " 2 2 Reynolds number ~ 1/sqrt(CL)   Mach number ~ 1/sqrt(CL)"
  end if 
  write (out_unit,*) 
  write (out_unit,'(A)') " xtrf =   1.000 (top)        1.000 (bottom)"
  write (out_unit,'(A,F7.3,5X,A,F9.3,A,5X,A,F7.3 )')                     &
                     " Mach = ",polar%mach,'Re = ',(polar%reynolds/1.d6),' e 6','Ncrit = ',polar%ncrit
  write (out_unit,*)
  
end subroutine write_polar_header

!-----------------------------------------------------------------------------
! calculate number of op_points of a polar based on start, end, increment
!-----------------------------------------------------------------------------
function  get_n_op_points (polar)

    type (polar_type), intent (in) :: polar
    integer :: get_n_op_points
    double precision :: cur_value, end_value
  
    get_n_op_points = 0 
    cur_value       = polar%start_value
    end_value       = polar%end_value + 1.d-6    ! due to double prec compare
  
    do while (cur_value <= end_value)
      get_n_op_points = get_n_op_points + 1
      cur_value = cur_value +  polar%increment
    end do 
  
  end function get_n_op_points
  
!-----------------------------------------------------------------------------
! build filename for polar file in this special xflr5 format
!   example: T1_Re0.400_M0.00_N9.0.txt 
!-----------------------------------------------------------------------------
  function  build_filename (polar)

    type (polar_type), intent (in) :: polar
    character (25) :: build_filename
    character (5)  :: temp_String
  
    if(polar%type =='Type1') then 
        build_filename  = 'T1'
    else 
        build_filename  = 'T2'
    end if 

    build_filename  = trim(build_filename)  // '_Re'
    write (temp_String, '(F5.3)') polar%reynolds / 1.d6
    build_filename  = trim(build_filename)  // trim(temp_String)

    build_filename  = trim(build_filename)  // '_M'
    write (temp_String, '(F4.2)') polar%mach
    build_filename  = trim(build_filename)  // trim(temp_String)

    build_filename  = trim(build_filename)  // '_N'
    write (temp_String, '(F3.1)') polar%ncrit
    build_filename  = trim(build_filename)  // trim(temp_String)

    build_filename  = trim(build_filename)  // '.txt'

  end function build_filename

!------------------------------------------------------------------------------
end module polar_operations
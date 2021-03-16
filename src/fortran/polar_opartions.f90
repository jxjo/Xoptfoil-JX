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

  use xfoil_driver,       only : re_type, op_point_specification_type
  use xfoil_driver,       only : op_point_result_type

  implicit none

  type polar_type
    character(250)   :: airfoil_name    ! Name of airfoil
    character(250)   :: file_name       ! Name of polar file name 
    type(re_type)    :: re              ! Re number of this polar (re*sqrt(cl) if Type2)
    type(re_type)    :: ma              ! Ma number of this polar (mach*sqrt(cl) if Type2)
    double precision :: ncrit           ! ncrit of polar
    logical          :: spec_cl         ! base value of polar either cl or alpha
    double precision :: start_value     ! polar starting from ...
    double precision :: end_value       ! ... to end value ...
    double precision :: increment       ! ... incremented by 
    integer :: n_op_points              ! number of all op_poins of this polar
    type(op_point_specification_type), dimension (:), allocatable :: &
                        op_points_spec  !array with specified op_points
    type(op_point_result_type), dimension (:), allocatable :: & 
                        op_points       !array with all calculated op_points
   end type polar_type

   integer :: MAXPOLARS = 30            ! max number of polars
   integer :: MAXOPS    = 100           ! max number of operating points of a polar 

contains


!=============================================================================
! High level entry for polar generation to kepp all the polar suff local to module
!
! - read input file for namelist &polar_generation
! - get polar definitions
! - calculate polars for foil with foilname
! - write each polar to a file 
!=============================================================================

subroutine check_and_do_polar_generation (input_file, output_prefix, foil)

  use vardef,             only : airfoil_type
  use xfoil_driver,       only : xfoil_geom_options_type, xfoil_options_type
  use input_output,       only : read_xfoil_options_inputs, read_xfoil_paneling_inputs
 
  character(*), intent(in)          :: input_file, output_prefix
  type (airfoil_type), intent (in)  :: foil

  type (polar_type), dimension (MAXPOLARS) :: polars
  type (xfoil_geom_options_type) :: xfoil_geom_options
  type (xfoil_options_type)      :: xfoil_options
  integer  :: npolars

  call read_xfoil_options_inputs  (input_file, 0, .true., xfoil_options)
  call read_polar_inputs          (input_file, xfoil_options, foil%name, npolars, polars)

  if (npolars > 0) then
    call read_xfoil_paneling_inputs (input_file, 0, xfoil_geom_options)
    call generate_polar_files (output_prefix, foil, npolars, polars, &
                               xfoil_geom_options, xfoil_options)
  end if

end subroutine check_and_do_polar_generation


 
!=============================================================================
! Generate and write to file all 'npolars' 'polars' for an airfoil
!
! Each polar will be written in a single file in xfoil text format
! in the subdirectory 'foilname_polars'.
!
! The name of the file is aligned to xflr5 polar file naming
!=============================================================================

subroutine generate_polar_files (output_prefix, foil, npolars, polars, &
                                xfoil_geom_options, xfoil_options)

  use vardef,             only : airfoil_type
  use os_util,            only : make_directory
  use xfoil_driver,       only : xfoil_geom_options_type, xfoil_options_type

  type (polar_type), dimension (MAXPOLARS), intent (inout) :: polars
  type (airfoil_type), intent (in)  :: foil
  integer, intent (in)              :: npolars
  character (*), intent(in)         :: output_prefix
  type (xfoil_geom_options_type), intent(in) :: xfoil_geom_options
  type (xfoil_options_type), intent(in)      :: xfoil_options

  
  integer :: i
  character (255) :: polars_subdirectory

! Create subdir for polar files if not exist
  polars_subdirectory = trim(output_prefix)//'_polars'
  call make_directory (trim(polars_subdirectory))

  ! calc and write all polars

  if (npolars > 1) then
    write (*,'(A,I2,A)') 'A total of ',npolars,' polars will be generated '//  &
                            'for airfoil '//trim(foil%name)
  else
    write (*,'(A,I2,A)') 'One polar will be generated '//  &
                            'for airfoil '//trim(foil%name)
  end if

  do i = 1, npolars

    write (*,*) 
    write (*,'(" - ",A,I1,A, I7,A)') 'Calculating polar Type ',polars(i)%re%type,', Re=',  &
          int(polars(i)%re%number)
    call init_polar (polars(i))

    call calculate_polar (foil, polars(i), xfoil_geom_options, xfoil_options)

    write (*,'(" - ",A, F7.0)') 'Writing polar to '//trim(polars_subdirectory)//'/'//trim(polars(i)%file_name)

    open(unit=13, file= trim(polars_subdirectory)//'/'//trim(polars(i)%file_name), status='replace')
    call write_polar_header (13, polars(i))
    call write_polar_data   (13, polars(i))
    close (13)

    deallocate (polars(i)%op_points)

  end do 

end subroutine generate_polar_files

!=============================================================================
! Read xoptfoil input file to get polars (definition) and xfoil run options
!   (separated from read_inputs to be more modular)
!=============================================================================

subroutine read_polar_inputs  (input_file, xfoil_options, foil_name, npolars, polars)

  use airfoil_operations, only : my_stop
  use input_output,       only : read_cl_re_default
  use input_output,       only : namelist_check
  use xfoil_driver,       only : xfoil_options_type

  type (polar_type), dimension (MAXPOLARS), intent (out) :: polars
  type(xfoil_options_type), intent(in)    :: xfoil_options

  character(*), intent(in) :: input_file, foil_name
  integer , intent(out)    :: npolars

  logical         :: generate_polars                         ! .true. .false. 
  integer         :: type_of_polar                           ! 1 or 2 
  character (7)   :: op_mode                                 ! 'spec-al' 'spec_cl'
  double precision, dimension (MAXPOLARS) :: polar_reynolds  ! 40000, 70000, 100000
  double precision, dimension (3)  :: op_point_range         ! -1.0, 10.0, 0.5

  integer :: istat, iunit, i

  namelist /polar_generation/ generate_polars, type_of_polar, polar_reynolds,   &
                              op_mode, op_point_range

! Init default values for polars

  npolars         = 0
  generate_polars = .false.
  type_of_polar   = 1
  op_mode         = 'spec-al'
  op_point_range  = 0d0
  polar_reynolds  = 0d0

! Open input file and read namelist from file

  iunit = 12
  open(unit=iunit, file=input_file, status='old', iostat=istat)

  if (istat == 0) then

    read (iunit, iostat=istat, nml=polar_generation)
    if (generate_polars) then 
      call namelist_check('polar_generation', istat, 'warn')
    end if
    close (iunit)
  else
    call my_stop('Could not find input file '//trim(input_file)//'.')
  end if
  
  if (.not. generate_polars) return 

! if there are no re numbers in input file take from command line
  if (polar_reynolds(1) == 0d0) then
    polar_reynolds(1) =  read_cl_re_default (0d0) 
  end if

! Input sanity

  if ((op_mode /= 'spec-al') .and. (op_mode /= 'spec-cl')) then
    call my_stop ("op_mode must be 'spec-cl' or 'spec-al'")
  end if
  if ((type_of_polar /= 1) .and. (type_of_polar /= 2)) then 
    call my_stop ("Type of polars must be either 'Type1' or 'Type2'")
  end if 
  if ((op_point_range(2) - op_point_range(1)) <= 0d0 ) then 
    call my_stop ("End of polar op_point_range must be higher than the start.")
  end if
  if (( op_point_range(1) + op_point_range(3)) >= op_point_range(2) ) then 
    call my_stop ("Start of polar op_point_range + increment should be end of op_point_range.")
  end if
  if (polar_reynolds(1) == 0d0) then 
    call my_stop ("No Reynolds number found - either in input file nor as command line parameter.")
  end if

  
! Init polar definitions with input 

  do i = 1, size(polar_reynolds)
    if (polar_reynolds(i) > 1000d0) then 
      polars(i)%airfoil_name    = trim(foil_name)
      polars(i)%spec_cl         = (op_mode == 'spec_cl')
      polars(i)%start_value     = op_point_range (1)
      polars(i)%end_value       = op_point_range (2)
      polars(i)%increment       = op_point_range (3)
      polars(i)%ma%number       = 0.0d0                   ! currently not supported
      polars(i)%ma%type         = 1                       ! currently not supported
      polars(i)%re%number       = polar_reynolds(i)
      polars(i)%re%type         = type_of_polar
      polars(i)%ncrit           = xfoil_options%ncrit
      npolars                   = i
    end if
  end do

end subroutine read_polar_inputs



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
    allocate (polar%op_points_spec(polar%n_op_points))
  else
    write (*,*) "Error: No valid value boundaries for polar"
    stop
  endif

! build this special xflr5 filename  T1_Re0.400_M0.00_N9.0.txt 

  polar%file_name = build_filename (polar)

! init op data points of polar

  cur_value = polar%start_value

  do i = 1, polar%n_op_points

    polar%op_points_spec(i)%value    = cur_value
    polar%op_points_spec(i)%spec_cl  = polar%spec_cl
    polar%op_points_spec(i)%re       = polar%re
    polar%op_points_spec(i)%ma       = polar%ma
    polar%op_points_spec(i)%ncrit    = polar%ncrit

    cur_value = cur_value + polar%increment

end do

end subroutine init_polar



!=============================================================================
! Calculate polar for airfoil
!=============================================================================

subroutine calculate_polar (foil, polar, xfoil_geom_options, xfoil_options)

  use vardef,             only : airfoil_type
  use vardef,             only : flap_spec_type
  use xfoil_driver,       only : run_op_points, xfoil_driver_reset
  use xfoil_driver,       only : xfoil_geom_options_type, xfoil_options_type
  use airfoil_evaluation, only : show_op_bubbles

  type (polar_type), intent (inout) :: polar
  type (airfoil_type), intent (in)  :: foil
  type (xfoil_geom_options_type), intent(in) :: xfoil_geom_options
  type (xfoil_options_type), intent(in)      :: xfoil_options

  double precision, dimension(polar%n_op_points) :: flap_degrees
  type(flap_spec_type) :: flap_spec               ! dummy - no flaps used

  flap_degrees (:)    = 0.d0 
  flap_spec%use_flap  = .false. 

  ! reset out lier detection tect. for a new polar 
  call xfoil_driver_reset

  call run_op_points (foil, xfoil_geom_options, xfoil_options,        &
                      flap_spec, flap_degrees, &
                      polar%op_points_spec, polar%op_points)

  ! #exp-bubble 
  call show_op_bubbles (polar%op_points_spec, polar%op_points) 

end subroutine calculate_polar



!------------------------------------------------------------------------------
! Write polar data in xfoil format to out_unit
!------------------------------------------------------------------------------
subroutine write_polar_data (out_unit, polar)

  use xfoil_driver,       only : op_point_result_type, op_point_specification_type

  type (polar_type), intent (in) :: polar
  integer,           intent (in) :: out_unit

  type (op_point_result_type) :: op
  integer              :: i 

! xflr5 example
! -
!  alpha     CL        CD       CDp       Cm    Top Xtr Bot Xtr   Cpmin    Chinge    XCp    
! ------- -------- --------- --------- -------- ------- ------- -------- --------- ---------
!  -1.400   0.0042   0.00513   0.00057  -0.0285  0.7057  0.2705  -0.9363   0.0000   7.0438
!   F8.3    F9.4     F10.5     F10.5     F9.4    F8.4    F8.4     F9.4     F9.4     F9.4     

  write (out_unit,'(A)') "  alpha     CL        CD       CDp       Cm    Top Xtr Bot Xtr "
  write (out_unit,'(A)') " ------- -------- --------- --------- -------- ------- ------- "

  do i = 1, polar%n_op_points

    op = polar%op_points(i)
    if (op%converged) then
      write (out_unit,  "(   F8.3,   F9.4,    F10.5,    F10.5,    F9.4,   F8.4,   F8.4)") &
                          op%alpha, op%cl, op%cd,    0d0,  op%cm,op%xtrt,op%xtrb
    else
      write(*,'(15x,A,I2,A, F6.2)') "Warning: No convergence - Skipped writing of" // &
      " op",i," - ",polar%op_points_spec(i)%value
    end if

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
  write (out_unit,'(A)') "Xoptfoil-JX"
  write (out_unit,*)
  write (out_unit,'(A)') " Calculated polar for: "//trim(polar%airfoil_name)
  write (out_unit,*)
  if (polar%re%type == 1 ) then 
    write (out_unit,'(A)') " 1 1 Reynolds number fixed          Mach number fixed"
  else
    write (out_unit,'(A)') " 2 2 Reynolds number ~ 1/sqrt(CL)   Mach number ~ 1/sqrt(CL)"
  end if 
  write (out_unit,*) 
  write (out_unit,'(A)') " xtrf =   1.000 (top)        1.000 (bottom)"
  write (out_unit,'(A,F7.3,5X,A,F9.3,A,5X,A,F7.3 )')                     &
                     " Mach = ",polar%ma%number,'Re = ',(polar%re%number/1.d6),' e 6','Ncrit = ',polar%ncrit
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
  
    if(polar%re%type == 1) then 
        build_filename  = 'T1'
    else 
        build_filename  = 'T2'
    end if 

    build_filename  = trim(build_filename)  // '_Re'
    write (temp_String, '(F5.3)') polar%re%number / 1.d6
    build_filename  = trim(build_filename)  // trim(temp_String)

    build_filename  = trim(build_filename)  // '_M'
    write (temp_String, '(F4.2)') polar%ma%number
    build_filename  = trim(build_filename)  // trim(temp_String)

    build_filename  = trim(build_filename)  // '_N'
    if (polar%ncrit < 10d0) then 
      write (temp_String, '(F3.1)') polar%ncrit
    else
      write (temp_String, '(F3.0)') polar%ncrit
    end if

    build_filename  = trim(build_filename)  // trim(temp_String)

    build_filename  = trim(build_filename)  // '.txt'

  end function build_filename

!------------------------------------------------------------------------------
end module polar_operations
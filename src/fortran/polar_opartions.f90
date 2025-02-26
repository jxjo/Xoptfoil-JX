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
  use os_util


  implicit none

  type polar_type
    character(250)   :: airfoil_name    ! Name of airfoil
    character(250)   :: file_name       ! Name of polar file name 
    character(250)   :: add_info        ! additional info string in polar file 
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

  integer, parameter :: MAXPOLARS = 30            ! max number of polars
  integer, parameter :: MAXOPS    = 100           ! max number of operating points of a polar 

   ! Parms for operating point specification
  integer, private :: npolars
  type (polar_type), dimension (MAXPOLARS), private :: polars


contains

 
!=============================================================================
! Generate and write to file all 'npolars' 'polars' for an airfoil
!
! Each polar will be written in a single file in xfoil text format
! in the subdirectory 'foilname_polars'.
!
! The name of the file is aligned to xflr5 polar file naming
!=============================================================================

subroutine generate_polar_files (show_details, subdirectory, foil, xfoil_geom_options, xfoil_options)

  use vardef,             only : airfoil_type, flap_spec_type
  use os_util,            only : make_directory
  use xfoil_driver,       only : xfoil_geom_options_type, xfoil_options_type
  use xfoil_driver,       only : op_point_result_type, run_op_points 
  use xfoil_driver,       only : xfoil_driver_reset_statistic
  use xfoil_driver,       only : xfoil_driver_push_statistic, xfoil_driver_pop_statistic

  type (airfoil_type), intent (in)  :: foil
  logical, intent(in)               :: show_details
  character (*), intent(in)         :: subdirectory
  type (xfoil_geom_options_type), intent(in) :: xfoil_geom_options
  type (xfoil_options_type), intent(in)      :: xfoil_options

  double precision, dimension(:), allocatable :: flap_degrees
  type(flap_spec_type) :: flap_spec               ! dummy - no flaps used
  type(op_point_result_type), dimension(:), allocatable :: op_points_result
  integer :: i
  character (255) :: polars_subdirectory, out_string

  flap_spec%use_flap  = .false. 

  if (trim(subdirectory) == '' ) then 
    polars_subdirectory = ''
  else
    polars_subdirectory = trim(subdirectory) // '\'
  end if

  ! calc and write all polars

  call xfoil_driver_push_statistic          ! hack - save xfoil_driver outlier statistics

  do i = 1, npolars


    if (allocated(op_points_result))  deallocate (op_points_result)
    if (allocated(flap_degrees))      deallocate (flap_degrees)
    allocate (flap_degrees(polars(i)%n_op_points))
    flap_degrees (:)    = 0.d0 
  
    ! reset out lier detection tect. for a new polar 
    call xfoil_driver_reset_statistic
 
    if (show_details) then 
      write (out_string,'(A,I1,A, I7)') 'Generating polar Type ',polars(i)%re%type,', Re=',  &
                                        int(polars(i)%re%number)
      call print_note_only ('- ' //trim(out_string))
      call print_colored (COLOR_NOTE, '   ')
    end if 

    call run_op_points (foil, xfoil_geom_options, xfoil_options,        &
                        flap_spec, flap_degrees, &
                        polars(i)%op_points_spec, op_points_result)
  
    call xfoil_driver_reset_statistic

    if (show_details) then 
      write (out_string,'(A,I1,A, I7,A)') 'Writing polar Type ',polars(i)%re%type,', Re=',  &
            int(polars(i)%re%number), ' to '//trim(polars_subdirectory)//'...'
      call print_colored (COLOR_NORMAL, ' - ' //trim(out_string))
      write (*,*) 
      write (*,*) 
    end if 

    open(unit=13, file= trim(polars_subdirectory)//trim(polars(i)%file_name), status='replace')
    call write_polar_header (13, polars(i))
    call write_polar_data   (13, op_points_result)
    close (13)

  end do 

  call xfoil_driver_pop_statistic        ! hack - retrieve xfoil_driver outlier statistics

end subroutine generate_polar_files

!=============================================================================
! Read xoptfoil input file to get polars (definition)
!   (separated from read_inputs to be more modular)
! Init polar data structure in this odule
!
!   - re_default for polar definitions with no Reynolds
!   - nrit for the polar xfoil calculation
!   - name of foil
! Returns:  - Polar has to be generated 
!=============================================================================

subroutine read_init_polar_inputs  (input_file, or_iunit, re_default, ncrit, foil_name, &
                                    generate_polar)

!  use input_output,       only : namelist_check
  use xfoil_driver,       only : xfoil_options_type

  integer, intent(in)           :: or_iunit
  type (re_type), intent(in)    :: re_default
  double precision, intent(in)  :: ncrit 
  character(*), intent(in)      :: input_file, foil_name
  logical, intent(out)          :: generate_polar            ! .true. .false. 

  integer         :: type_of_polar                           ! 1 or 2 
  character (7)   :: op_mode                                 ! 'spec-al' 'spec_cl'
  double precision, dimension (MAXPOLARS) :: polar_reynolds  ! 40000, 70000, 100000
  double precision, dimension (3)  :: op_point_range         ! -1.0, 10.0, 0.5
  logical                          :: generate_polars        ! compatibility to older version 

  integer :: istat, iunit, i

  namelist /polar_generation/ generate_polar, generate_polars, type_of_polar, polar_reynolds,   &
                              op_mode, op_point_range

! Init default values for polars

  npolars         = 0
  generate_polar  = .false.
  generate_polars = .false.
  type_of_polar   = -1
  op_mode         = 'spec-al'
  op_point_range  = (/ -2d0, 10d0 , 1.0d0 /)
  polar_reynolds  = 0d0

  istat           = 0

! Open input file and read namelist from file

  if (trim(input_file) == '') then
    iunit = or_iunit
    rewind(iunit)
  else
    iunit = 12
    open(unit=iunit, file=input_file, status='old', iostat=istat)
  end if

  if (istat == 0) then

    read (iunit, iostat=istat, nml=polar_generation)
    if (generate_polar) then 
      !call namelist_check('polar_generation', istat, 'warn')
    end if

    if (trim(input_file) /= '') close (iunit)
  else
    call my_stop('Could not find input file '//trim(input_file)//'.')
  end if
  
  generate_polar = generate_polar .or. generate_polars    ! compatibility to older version

  if (.not. generate_polar) return 

! if there are no re numbers in input file take default
  if (polar_reynolds(1) == 0d0) then 
    polar_reynolds(1) = re_default%number 
  end if
  if (type_of_polar == -1) then 
    type_of_polar     = re_default%type
  end if 


! Input sanity

  if ((op_mode /= 'spec-al') .and. (op_mode /= 'spec-cl')) then
    call my_stop ("&polar_generation: op_mode must be 'spec-cl' or 'spec-al'")
  end if
  if ((type_of_polar /= 1) .and. (type_of_polar /= 2)) then 
    call my_stop ("&polar_generation: Type of polars must be either '1' or '2'")
  end if 
  if ((op_point_range(2) - op_point_range(1)) <= 0d0 ) then 
    call my_stop ("&polar_generation: End of polar op_point_range must be higher than the start.")
  end if
  if (( op_point_range(1) + op_point_range(3)) >= op_point_range(2) ) then 
    call my_stop ("&polar_generation: Start of polar op_point_range + increment should be end of op_point_range.")
  end if


! Init polar definitions with input 

  npolars = 0
  do i = 1, size(polar_reynolds)
    if (polar_reynolds(i) > 1000d0) then 
      npolars = npolars + 1
      polars(npolars)%airfoil_name    = trim(foil_name)
      polars(npolars)%spec_cl         = (op_mode == 'spec-cl')
      polars(npolars)%start_value     = op_point_range (1)
      polars(npolars)%end_value       = op_point_range (2)
      polars(npolars)%increment       = op_point_range (3)
      polars(npolars)%ma%number       = 0.0d0                   ! currently not supported
      polars(npolars)%ma%type         = 1                       ! currently not supported
      polars(npolars)%re%number       = polar_reynolds(i)
      polars(npolars)%re%type         = type_of_polar
      polars(npolars)%ncrit           = ncrit
    end if
  end do

  if (npolars > 0) then 
    call init_polars
  else
    call my_stop ("&polar_generation: No Reynolds number found - either in input file nor as command line parameter.")
  end if 

end subroutine read_init_polar_inputs


!=============================================================================
! Initialize the global polar data structure 
!=============================================================================

subroutine init_polars ()

  integer :: ipol, i
  double precision :: cur_value

  do ipol = 1, npolars

  ! calc number of op_points 

    polars(ipol)%n_op_points = get_n_op_points (polars(ipol)) 
    if (polars(ipol)%n_op_points >= 0) then
      if (allocated(polars(ipol)%op_points))      deallocate (polars(ipol)%op_points)
      if (allocated(polars(ipol)%op_points_spec)) deallocate (polars(ipol)%op_points_spec)
      ! only spec - op_points will be allocated in xfoil driver ...
      allocate (polars(ipol)%op_points_spec(polars(ipol)%n_op_points))
    else
      write (*,*) "Error: No valid value boundaries for polar"
      stop
    endif

  ! build this special xflr5 filename  T1_Re0.400_M0.00_N9.0.txt 

    polars(ipol)%file_name = build_filename (polars(ipol))

  ! init op data points of polar
  !
  ! if polar is running from eg -5 to +10, split the polar in
  !   0     ... -5
  !   (0+x) ... + 10
  ! to ensure xfoil convergence (starting with a high negative value is critical)

    i = 0 

    if (polars(ipol)%start_value < 0d0) then 

    ! go downward from smallest-1 to start value 

      if (polars(ipol)%end_value > 0d0) then 
        cur_value =  smallest_op_point (polars(ipol)) - polars(ipol)%increment
      else 
        cur_value =  smallest_op_point (polars(ipol)) 
      end if 

      do while (cur_value >= polars(ipol)%start_value)
        i = i + 1
        polars(ipol)%op_points_spec(i)%value    = cur_value
        polars(ipol)%op_points_spec(i)%spec_cl  = polars(ipol)%spec_cl
        polars(ipol)%op_points_spec(i)%re       = polars(ipol)%re
        polars(ipol)%op_points_spec(i)%ma       = polars(ipol)%ma
        polars(ipol)%op_points_spec(i)%ncrit    = polars(ipol)%ncrit
        cur_value = cur_value - polars(ipol)%increment
      end do       
    end if 

    if (polars(ipol)%end_value > 0d0) then 

      ! go upward from smallest to end value 

      cur_value = smallest_op_point (polars(ipol))
      do while (cur_value <= polars(ipol)%end_value + 1.d-6)
        i = i + 1
        polars(ipol)%op_points_spec(i)%value    = cur_value
        polars(ipol)%op_points_spec(i)%spec_cl  = polars(ipol)%spec_cl
        polars(ipol)%op_points_spec(i)%re       = polars(ipol)%re
        polars(ipol)%op_points_spec(i)%ma       = polars(ipol)%ma
        polars(ipol)%op_points_spec(i)%ncrit    = polars(ipol)%ncrit
        cur_value = cur_value + polars(ipol)%increment
      end do       
    end if 

  end do

end subroutine init_polars

!------------------------------------------------------------------------------
! Set info for polar filename and additional info like "Design 123"
!------------------------------------------------------------------------------
subroutine set_polar_info (foil_name, file_name, add_info)

  character (*), intent(in) :: foil_name, file_name, add_info

  integer     :: i

  if (size(polars) > 0) then 
    do i = 1, size(polars)
      polars(i)%airfoil_name = trim(foil_name)
      polars(i)%file_name = trim(file_name)
      polars(i)%add_info  = trim(add_info)
    end do 
  else
    call my_stop("Internal error: No polar initilaized for generate_polar.")
  end if 

end subroutine set_polar_info


!------------------------------------------------------------------------------
! Write polar data in xfoil format to out_unit
!------------------------------------------------------------------------------
subroutine write_polar_data (out_unit, op_points_result)

  use xfoil_driver,       only : op_point_result_type, op_point_specification_type

  type (op_point_result_type), dimension (:), intent (in) :: op_points_result
  integer,           intent (in) :: out_unit

  type (op_point_result_type) :: op
  integer              :: i 
  character (100)      :: text_out
  logical              :: has_warned = .false.

! xflr5 example
! -
!  alpha     CL        CD       CDp       Cm    Top Xtr Bot Xtr   Cpmin    Chinge    XCp    
! ------- -------- --------- --------- -------- ------- ------- -------- --------- ---------
!  -1.400   0.0042   0.00513   0.00057  -0.0285  0.7057  0.2705  -0.9363   0.0000   7.0438
!   F8.3    F9.4     F10.5     F10.5     F9.4    F8.4    F8.4     F9.4     F9.4     F9.4     

  write (out_unit,'(A)') "  alpha     CL        CD       CDp       Cm    Top Xtr Bot Xtr "
  write (out_unit,'(A)') " ------- -------- --------- --------- -------- ------- ------- "

  do i = 1, size(op_points_result)

    op = op_points_result(i)
    if (op%converged) then
      write (out_unit,  "(   F8.3,   F9.5,    F10.6,    F10.5,    F9.4,   F8.4,   F8.4)") &
                          op%alpha, op%cl, op%cd,    0d0,  op%cm,op%xtrt,op%xtrb
    else
      write(text_out,'(A,F5.2,A)') "alpha =",op%alpha," not converged in polar generation. Skipping op point"
      call print_warning (trim(text_out),3)
      has_warned = .true. 
    end if

  end do 

  if (has_warned) write (*,*) 
  
end subroutine write_polar_data



!------------------------------------------------------------------------------
! Write polar header in xfoil/xflr5 format to out_unit
!------------------------------------------------------------------------------
subroutine write_polar_header (out_unit, polar)

  type (polar_type), intent (in) :: polar
  integer,           intent (in) :: out_unit


! Example xflr5
!-
!Xoptfoil-JX Design_Polar 77
!
! Calculated polar for: JX FXrcn 15
!
! 1 1 Reynolds number fixed          Mach number fixed         
!
! xtrf =   1.000 (top)        1.000 (bottom)
! Mach =   0.000     Re =     0.400 e 6     Ncrit =   9.000
!
!-

  if (trim(polar%airfoil_name) == '') &
    call my_stop ('Internal error: Xfoil polar to write has no name.')

  write (out_unit,'(A)') "Xoptfoil-JX" // " " // trim(polar%add_info)
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
! the smallest (absolute) value of operating points of polar 
!-----------------------------------------------------------------------------
  function  smallest_op_point (polar)

    type (polar_type), intent (in) :: polar
    double precision :: smallest_op_point
    double precision :: cur_value, end_value
  
    smallest_op_point = polar%start_value
    cur_value       = polar%start_value
    end_value       = polar%end_value + 1.d-6    ! due to double prec compare
  
    do while (cur_value <= end_value)
      if (abs (cur_value) < abs(smallest_op_point)) then
        smallest_op_point = cur_value
      end if 
      cur_value = cur_value +  polar%increment
    end do 
  
  end function smallest_op_point

  
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
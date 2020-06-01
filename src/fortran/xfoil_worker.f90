!-------------------------------------------------------------------------
!
!   Xfoil-Worker  *work in progress*
!
!   Utility Functions based on xfoil to complement Xoptfoil-JX
!   
!   Xfoil-Worker uses an Xoptfoil input-file to get the paramters.
!     only a few sections are needed
!   
!   -o polars    generate polar (set)for an airfoil
!                inputs.txt namelists used: polar_generation, xfoil_run_options        
!   
!   -o smooth    repanels and optionally smoothes an airfoil
!                inputs.txt namelists used: xfoil_paneling_options, xfoil_run_options        
!                                           smoothing_options 
!   This file is part of XOPTFOIL-JX.
!                       Copyright (C) 2017-2019 Daniel Prosser
!                       Copyright (C) 2020      Jochen Guenzel
!-------------------------------------------------------------------------

program xfoil_worker

  use vardef,             only : airfoil_type
  use memory_util,        only : deallocate_airfoil
  use airfoil_operations, only : load_airfoil, my_stop, airfoil_write
  use xfoil_driver,       only : xfoil_init, xfoil_cleanup 
  use xfoil_driver,       only : xfoil_set_buffer_airfoil, xfoil_reload_airfoil
  use polar_operations,   only : check_and_do_polar_generation

  implicit none

#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION ""
#endif

  type(airfoil_type) :: foil
  character(255)     :: input_file, output_prefix, airfoil_filename
  character(20)      :: action

  write(*,'(A)') 
  write(*,'(A)') 'Xfoil_Worker      Version '//trim(PACKAGE_VERSION)//  &
                 '              (c) 2020 Jochen Guenzel'

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

    case ('smooth')

      ! Test for change max thickness location 
      call load_airfoil(airfoil_filename, foil)
      call xfoil_init()
    ! Repanel and optionally smooth 
      call repanel_smooth (input_file, output_prefix, foil)
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
! Repanels and optionally smoothes foil based on settings in 'input file'
!-------------------------------------------------------------------------

subroutine repanel_smooth (input_file, output_prefix, seed_foil)

  use vardef,             only : airfoil_type
  use vardef,             only : do_smoothing
  use vardef,             only : spike_threshold, highlow_treshold, curv_threshold
  use xfoil_driver,       only : xfoil_geom_options_type, smooth_paneling
  use airfoil_operations, only : airfoil_write, le_find, transform_airfoil, get_split_points
  use airfoil_operations, only : split_airfoil, assess_surface, smooth_it
  use polar_operations,   only : read_xfoil_paneling_inputs, read_smoothing_inputs


  character(*), intent(in)          :: input_file, output_prefix
  type (airfoil_type), intent (in)  :: seed_foil

  double precision    :: xoffset, zoffset, foilscale 
  integer             :: pointst, pointsb
  double precision, dimension(:), allocatable :: xt, xb, zt, zb, zt_smoothed, zb_smoothed
  type (airfoil_type) :: foil_smoothed, foil
  type (xfoil_geom_options_type) :: geom_options

! Read inputs file to get xfoil paneling options  

  call read_xfoil_paneling_inputs  (input_file, geom_options)

! Repanel seed airfoil with xfoil PANGEN 

  write (*,'(1x, A,A,A,I3,A)') 'Repaneling ',trim(seed_foil%name), ' with ',geom_options%npan,' Points'
  call smooth_paneling(seed_foil, npoint, foil, geom_options)

! Calculate leading edge information

  call le_find(foil%x, foil%z, foil%leclose,                        &
               foil%xle, foil%zle, foil%addpoint_loc)
! Translate and scale

  call transform_airfoil(foil, xoffset, zoffset, foilscale)

! Split up seed airfoil into upper and lower surfaces

  call get_split_points(foil, pointst, pointsb, .false.)
  allocate(xt(pointst))
  allocate(zt(pointst))
  allocate(xb(pointsb))
  allocate(zb(pointsb))
  allocate(zt_smoothed(pointst))
  allocate(zb_smoothed(pointsb))
  call split_airfoil(foil, xt, xb, zt, zb, .false.)

! Rebuid foil - with spitting npoints are increased by one

  foil%npoint = pointst + pointsb - 1
  deallocate(foil%x)
  deallocate(foil%z)
  allocate(foil%x(foil%npoint))
  allocate(foil%z(foil%npoint))

  do i = 1, pointst
    foil%x(i) = xt(pointst-i+1)/foilscale - xoffset
    foil%z(i) = zt(pointst-i+1)/foilscale - zoffset
  end do
  do i = 1, pointsb-1
    foil%x(i+pointst) = xb(i+1)/foilscale - xoffset
    foil%z(i+pointst) = zb(i+1)/foilscale - zoffset
  end do

! Smooth it ?

  call read_smoothing_inputs  (input_file, do_smoothing, spike_threshold, &
                               highlow_treshold, curv_threshold)

  if (do_smoothing) then 

    write (*,*) 
    write (*,'(1x,A)') 'Before smoothing ...'
    call assess_surface ('Top', xt, zt)
    call assess_surface ('Bot', xb, zb)

    zt_smoothed = zt
    zb_smoothed = zb

    call smooth_it (xt, zt_smoothed) 
    call smooth_it (xb, zb_smoothed)

    write (*,'(1x,A)') 'Ater smoothing ...'
    call assess_surface ('Top', xt, zt_smoothed)
    call assess_surface ('Bot', xb, zb_smoothed)

  ! Format coordinates in a single loop in derived type

    foil_smoothed%npoint = pointst + pointsb - 1
    allocate(foil_smoothed%x(foil_smoothed%npoint))
    allocate(foil_smoothed%z(foil_smoothed%npoint))

    do i = 1, pointst
      foil_smoothed%x(i) = xt(pointst-i+1)/foilscale - xoffset
      foil_smoothed%z(i) = zt_smoothed(pointst-i+1)/foilscale - zoffset
    end do
    do i = 1, pointsb-1
      foil_smoothed%x(i+pointst) = xb(i+1)/foilscale - xoffset
      foil_smoothed%z(i+pointst) = zb_smoothed(i+1)/foilscale - zoffset
    end do

    call airfoil_write (trim(output_prefix)//'.dat', output_prefix, foil_smoothed)

  else                        ! no smoothing write repaneld foil 

    write (*,*) 
    write (*,'(1x,A)') 'No smoothing activated'
    call airfoil_write (trim(output_prefix)//'.dat', output_prefix, foil)

  end if 

! Write all airfoils to _design_coordinates using XOptfoil format for visualizer
  
  call write_design_coordinates (output_prefix, 0, seed_foil)
  foil%name = trim (seed_foil%name) // '-repaneled'
  call write_design_coordinates (output_prefix, 1, foil)

  if (do_smoothing) &
    foil_smoothed%name = trim (seed_foil%name) // '-smoothed'
    call write_design_coordinates (output_prefix, 2, foil_smoothed)

end subroutine repanel_smooth

!-------------------------------------------------------------------------
! writes 'output_prefix'_design_coordinates for foil
!-------------------------------------------------------------------------


subroutine write_design_coordinates (output_prefix, designcounter, curr_foil)

  use vardef,             only : airfoil_type
  use airfoil_operations, only : airfoil_write_to_unit
  use xfoil_driver,    only : xfoil_geometry_info, xfoil_set_airfoil

  character(*), intent(in)          :: output_prefix
  integer, intent(in)               :: designcounter
  type (airfoil_type), intent (in)  :: curr_foil

  double precision :: maxt, xmaxt, maxc, xmaxc
  character(100) :: foilfile, text, title
  character(8)   :: maxtchar, xmaxtchar, maxcchar, xmaxcchar
  integer :: foilunit
             
! Get geometry info

  call xfoil_set_airfoil(curr_foil)
  call xfoil_geometry_info(maxt, xmaxt, maxc, xmaxc)
  write(maxtchar,'(F8.5)') maxt
  maxtchar = adjustl(maxtchar)
  write(xmaxtchar,'(F8.5)') xmaxt
  xmaxtchar = adjustl(xmaxtchar)
  write(maxcchar,'(F8.5)') maxc
  maxcchar = adjustl(maxcchar)
  write(xmaxcchar,'(F8.5)') xmaxc
  xmaxcchar = adjustl(xmaxcchar)

! Set output file names and identifiers

  foilfile = trim(output_prefix)//'_design_coordinates.dat'
  foilunit = 13

! Open files and write headers, if necessary

  if (designcounter == 0) then

!   Header for coordinate file

    write(*,*) "Writing "//trim(curr_foil%name)//" coordinates for seed airfoil to file "//               &
               trim(foilfile)//" ..."
    open(unit=foilunit, file=foilfile, status='replace')
    write(foilunit,'(A)') 'title="Airfoil coordinates"'

!   Add 2nd and 3rd derivative to
!        ...design_coordinates.dat to show it in visualizer
    write(foilunit,'(A)') 'variables="x" "z" "2nd derivative" "3rd derivative"'

    title =  'zone t="Seed airfoil, maxt='//trim(maxtchar)//&
             ', xmaxt='//trim(xmaxtchar)//', maxc='//&
              trim(maxcchar)//', xmaxc='//trim(xmaxcchar)//'"'
  else

!   Open coordinate file and write zone header

    write(text,*) designcounter
    text = adjustl(text)

    write(*,*) "Writing "//trim(curr_foil%name)//" coordinates for design number "//trim(text)//        &
               " to file "//trim(foilfile)//" ..."
    open(unit=foilunit, file=foilfile, status='old', position='append', err=900)
    title =  'zone t="Airfoil, maxt='//trim(maxtchar)//&
             ', xmaxt='//trim(xmaxtchar)//', maxc='//&
              trim(maxcchar)//', xmaxc='//trim(xmaxcchar)//'", '//&
             'SOLUTIONTIME='//trim(text)

  end if

! Write coordinates to file

  call  airfoil_write_to_unit (foilunit, title, curr_foil, .True.)

! Close output files

  close(foilunit)

  return

! Warning if there was an error opening design_coordinates file

900 write(*,*) "Warning: unable to open "//trim(foilfile)//". Skipping ..."
  write_airfoil_optimization_progress = 1
  return

end subroutine write_design_coordinates


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

  write(*,'(A)')
  write(*,'(A)') "Usage: Xfoil_worker -w worker_action [Options]"
  write(*,'(A)')
  write(*,'(A)') "  -w polars         Generate polars of 'airfoil_file'"
  write(*,'(A)') "  -w smooth         Repanel and smooth 'airfoil_file'"
  write(*,'(A)')
  write(*,'(A)') "Options:"
  write(*,'(A)') "  -i input_file     Specify an input file (default: 'inputs.txt')"
  write(*,'(A)') "  -o output_prefix  Specify an output prefix (default: 'foil')"
  write(*,'(A)') "  -r xxxxxx         Specify a default reynolds number (re_default)"
  write(*,'(A)') "  -a airfoil_file   Specify filename of seed airfoil"
  write(*,'(A)') "  -h, --help        Display usage information and exit"
  write(*,'(A)')
  write(*,'(A)') "Refer to the PDF reference guide for complete input help."
  write(*,'(A)')

end subroutine print_worker_usage


!===========================================================================
! jx-mod Testing purposes 
!===========================================================================

subroutine test_set_thickness_camber (foil)

  use vardef,    only : airfoil_type
  use xfoil_driver,       only : xfoil_set_thickness_camber, smooth_paneling
  use xfoil_driver,       only : xfoil_scale_thickness_camber
  use airfoil_operations, only : airfoil_write

  type(airfoil_type), intent(in) :: foil
  double precision :: maxt, xmaxt, maxc, xmaxc
  type(airfoil_type) :: outfoil

  maxt = 9.d-2
  xmaxt = 30.d-2
  maxc = 2.5d-2
  xmaxc = 45.d-2

  ! Use Xfoil to smooth airfoil paneling
  ! call smooth_paneling(foil, 200, foilsmoothed)

  call xfoil_set_thickness_camber (foil, maxt, xmaxt, maxc, xmaxc, outfoil)

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


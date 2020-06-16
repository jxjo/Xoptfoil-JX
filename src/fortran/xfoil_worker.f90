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
  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_reload_airfoil
  use polar_operations,   only : check_and_do_polar_generation

  implicit none

#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION ""
#endif

  type(airfoil_type) :: foil
  character(255)     :: input_file, output_prefix, airfoil_filename
  character(20)      :: action
  logical            :: visualizer

  write(*,'(A)') 
  write(*,'(A)') 'Xfoil_Worker      Version '//trim(PACKAGE_VERSION)//  &
                 '              (c) 2020 Jochen Guenzel'

! Set default names and read command line arguments

  input_file        = 'inputs.txt'
  output_prefix     = ''
  action            = ''
  airfoil_filename  = ''
  visualizer        = .false.
  call read_worker_clo(input_file, output_prefix, airfoil_filename, action, visualizer)

  if (trim(action) == "") &
    call my_stop("Must specify an action for the worker with -w option.")
  if (trim(airfoil_filename) == "") &
    call my_stop("Must specify an airfoil file with the -a option.")

! Load airfoil defined in command line 
  call load_airfoil(airfoil_filename, foil)

! Allocate xfoil variables
  call xfoil_init()

! Do actions according command line option

  select case (trim(action)) 

    case ('polar')        ! Generate polars in subdirectory ".\<output_prefix>_polars\*.*

      if (trim(output_prefix) == '') & 
        output_prefix = airfoil_filename (1:(index (airfoil_filename,'.') - 1))

        call check_and_do_polar_generation (input_file, output_prefix, foil)

    case ('norm')         ! Repanel, Normalize into "<output_prefix>.dat"

      if (trim(output_prefix) == '') & 
        output_prefix = airfoil_filename (1:(index (airfoil_filename,'.') - 1))//'-norm'

        call repanel_smooth (input_file, output_prefix, foil, visualizer, .false.)

    case ('smooth')       ! Repanel, Normalize and smooth into "<output_prefix>.dat"

      if (trim(output_prefix) == '') & 
        output_prefix = airfoil_filename (1:(index (airfoil_filename,'.') - 1))//'-smoothed'

      call repanel_smooth (input_file, output_prefix, foil, visualizer, .true.)
  
    case ('test')         ! Test for change max thickness location 
      
      call xfoil_set_airfoil (foil)
      call HIPNT (0.3d0, 0.25d0)
      call xfoil_reload_airfoil(foil)
      call airfoil_write (trim(output_prefix)//'.dat', output_prefix, foil)

    case default

      call print_worker_usage()

  end select 

  call xfoil_cleanup()
  call deallocate_airfoil (foil)


end program xfoil_worker

!-------------------------------------------------------------------------
! Repanels and optionally smoothes foil based on settings in 'input file'
!-------------------------------------------------------------------------

subroutine repanel_smooth (input_file, output_prefix, seed_foil, visualizer, do_smoothing)

  use vardef,             only : airfoil_type
  use vardef,             only : spike_threshold, highlow_treshold, curv_threshold
  use xfoil_driver,       only : xfoil_geom_options_type
  use airfoil_operations, only : airfoil_write, transform_airfoil, get_split_points
  use airfoil_operations, only : split_airfoil, assess_surface, smooth_it, rebuild_airfoil
  use airfoil_operations, only : repanel_and_normalize_airfoil
  use polar_operations,   only : read_xfoil_paneling_inputs, read_smoothing_inputs


  character(*), intent(in)          :: input_file, output_prefix
  type (airfoil_type), intent (inout)  :: seed_foil
  logical, intent(in)               :: do_smoothing, visualizer

  double precision, dimension(:), allocatable :: xt, xb, zt, zb, zt_smoothed, zb_smoothed
  type (airfoil_type) :: foil_smoothed, foil
  type (xfoil_geom_options_type) :: geom_options

! Read inputs file to get xfoil paneling options  

  write (*,*)
  call read_xfoil_paneling_inputs  (input_file, geom_options)

! Repanel seed airfoil with xfoil PANGEN 

  call repanel_and_normalize_airfoil (seed_foil, geom_options%npan, foil)

! Now split and rebuild to add a real  LE point at 0,0 

  call split_airfoil   (foil, xt, xb, zt, zb, .false.)
  call rebuild_airfoil (xt, xb, zt, zb, foil)

! Smooth it ?

  if (do_smoothing) then 

    write(*,*)
    call read_smoothing_inputs (input_file, spike_threshold, highlow_treshold, curv_threshold)

    write (*,'(/,1x,A)') 'Smoothing Top surface ...'
    zt_smoothed = zt
    call smooth_it (.true., xt, zt_smoothed) 

    write (*,'(/,1x,A)') 'Smoothing Bottom surface ...'
    zb_smoothed = zb
    call smooth_it (.true., xb, zb_smoothed)

  ! Rebuild foil and write to file

    call rebuild_airfoil (xt, xb, zt_smoothed, zb_smoothed, foil_smoothed)

    foil_smoothed%name = output_prefix
    call airfoil_write   (trim(output_prefix)//'.dat', trim(foil_smoothed%name), foil_smoothed)

  else                        ! no smoothing write repaneld foil 

    foil%name   = output_prefix
    call airfoil_write   (trim(output_prefix)//'.dat', trim(foil%name), foil)

  end if 

! Write all airfoils to _design_coordinates using XOptfoil format for visualizer
  
  if (visualizer) then 
    call write_design_coordinates (output_prefix, 0, seed_foil)
    foil%name   = 'normalized'
    call write_design_coordinates (output_prefix, 1, foil)
    if (do_smoothing) &
      foil_smoothed%name   = 'smoothed'
      call write_design_coordinates (output_prefix, 2, foil_smoothed)
  end if 

end subroutine repanel_smooth

!-------------------------------------------------------------------------
! writes 'output_prefix'_design_coordinates for foil
!-------------------------------------------------------------------------


subroutine write_design_coordinates (output_prefix, designcounter, curr_foil)

  use vardef,             only : airfoil_type
  use airfoil_operations, only : airfoil_write_to_unit
  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_get_geometry_info

  character(*), intent(in)          :: output_prefix
  integer, intent(in)               :: designcounter
  type (airfoil_type), intent (in)  :: curr_foil

  double precision :: maxt, xmaxt, maxc, xmaxc
  character(100) :: foilfile, text, title
  character(8)   :: maxtchar, xmaxtchar, maxcchar, xmaxcchar
  integer :: foilunit
             
! Get geometry info

  call xfoil_set_airfoil (curr_foil)
  call xfoil_get_geometry_info(maxt, xmaxt, maxc, xmaxc)

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

subroutine read_worker_clo(input_file, output_prefix, airfoil_name, action, visualizer)

  use airfoil_operations, only : my_stop

  character(*), intent(inout) :: input_file, output_prefix, action, airfoil_name
  logical,      intent(inout) :: visualizer

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
    else if (trim(arg) == "-v") then
      visualizer = .true.
      i = i+1
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
  write(*,'(A)') "  -w norm           Repanel, normalize 'airfoil_file'"
  write(*,'(A)') "  -w smooth         Repanel, normalize, smooth 'airfoil_file'"
  write(*,'(A)')
  write(*,'(A)') "Options:"
  write(*,'(A)') "  -i input_file     Specify an input file (default: 'inputs.txt')"
  write(*,'(A)') "  -o output_prefix  Specify an output prefix (default: 'foil')"
  write(*,'(A)') "  -r xxxxxx         Specify a default reynolds number (re_default)"
  write(*,'(A)') "  -a airfoil_file   Specify filename of seed airfoil"
  write(*,'(A)') "  -v                Generate file 'design_coordinates' for visualizer"
  write(*,'(A)') "  -h, --help        Display usage information and exit"
  write(*,'(A)')
  write(*,'(A)') "Refer to the worker reference guide for complete input help."
  write(*,'(A)')

end subroutine print_worker_usage



!-----------------------------------------------------------------------------
! Write a single polyline with its derivatives  to file 
!       - for testing - 
!-----------------------------------------------------------------------------
subroutine write_polyline (info, npoints, x, y)

  use math_deps,       only : derivation2, derivation3 


  integer, intent(in) :: npoints
  character(*), intent(in) :: info
  double precision, dimension(npoints), intent(in) :: x, y
  double precision, dimension(npoints) :: deriv2, deriv3

  integer :: iunit, i
  character(100) :: smoothfile

  deriv2 = derivation2 (size(x), x, y)
  deriv3 = derivation3 (size(x), x, y)


  iunit = 21 
  smoothfile = trim(info)//'_derivatives.dat'

  open(unit=iunit, file=smoothfile, status='replace', err=901)

  write(iunit,*) 'x, y, 2nd_deriv, 3rd_deriv'
  do i = 1, npoints
    write(iunit,'(2F12.7,2G17.7)')  x(i), y(i), deriv2(i), deriv3(i)
  end do

  close (iunit)
  return

  901 write(*,*) "Warning: unable to open "//trim(smoothfile)//". Skipping ..."
  return

end subroutine write_polyline


!-----------------------------------------------------------------------------
! Write smoothed derivations to file 
!       local csv file to visualize in Excel smoothing results 
!-----------------------------------------------------------------------------
subroutine write_deriv_to_file (info, npoints, x, y, deriv2, deriv3, & 
  y_smoothed, deriv2_smoothed, deriv3_smoothed)

  use math_deps, only : curvature

  integer, intent(in) :: npoints
  character(*), intent(in) :: info
  double precision, dimension(npoints), intent(in) :: x, y, deriv2, deriv3
  double precision, dimension(npoints), intent(in) :: y_smoothed, deriv2_smoothed, deriv3_smoothed

  integer :: smoothunit, i
  character(100) :: smoothfile

  smoothunit = 21 
  smoothfile = trim(info)//'_smoothed_derivations.csv'

  !write(*,*) "  Writing smoothed derivations for "//trim(info)//             &
  !" to file "//trim(smoothfile)//" ..."
  open(unit=smoothunit, file=smoothfile, status='replace', err=901)

  write(smoothunit,*) 'x, y, y 2nd deriv, y 3rd deriv,'                                 //  &
            'y_smooth, y_smooth-y ,  y_smooth 2nd deriv, y_smooth 3rd deriv'
  do i = 1, npoints
  write(smoothunit, '(7(G16.8, A),G16.8)') x(i),', ', y(i), ', ', deriv2(i),', ',deriv3(i),', ', &
            y_smoothed(i), ', ', (y_smoothed(i)-y(i)),', ',deriv2_smoothed(i),', ',deriv3_smoothed(i)
  end do
  close (smoothunit)
  return

  901 write(*,*) "Warning: unable to open "//trim(smoothfile)//". Skipping ..."
  return

end subroutine write_deriv_to_file


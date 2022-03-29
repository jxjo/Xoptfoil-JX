!-------------------------------------------------------------------------
!
!   Xfoil-Worker  
!
!   Utility Functions based on xfoil to complement Xoptfoil-JX
!   
!   Xfoil-Worker uses an Xoptfoil input-file to get the paramters.
!     only a few sections are needed
!   
!   This file is part of XOPTFOIL-JX.
!                       Copyright (C) 2017-2019 Daniel Prosser
!                       Copyright (C) 2021      Jochen Guenzel
!-------------------------------------------------------------------------

program xfoil_worker

  use vardef,             only : airfoil_type 
  use memory_util,        only : deallocate_airfoil
  use os_util 
  use airfoil_operations, only : load_airfoil, airfoil_write,le_find
  use xfoil_driver,       only : xfoil_init, xfoil_cleanup, xfoil_options_type
  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_reload_airfoil, xfoil_defaults

  implicit none

#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION ""
#endif

  type(airfoil_type) :: foil, blend_foil
  type (xfoil_options_type) :: xfoil_options

  character(255)     :: input_file, output_prefix, airfoil_filename, second_airfoil_filename
  character(20)      :: action, value_argument
  logical            :: visualizer, outname_auto

  write(*,'(A)') 

! Set default names and read command line arguments

  input_file        = ''
  output_prefix     = ''
  action            = ''
  airfoil_filename  = ''
  second_airfoil_filename = ''
  visualizer        = .false.
  call read_worker_clo(input_file, output_prefix, airfoil_filename, action, & 
                       second_airfoil_filename, value_argument, visualizer)

  if (trim(action) == "") &
    call my_stop("Must specify an action for the worker with -w option.")
  if (trim(airfoil_filename) == "") &
    call my_stop("Must specify an airfoil file with the -a option.")

! Let's start

  write (*,'(1x)', advance = 'no') 
  call print_colored (COLOR_FEATURE,'Worker on '//trim(airfoil_filename))

  write (*,'(3x,A,A,3x)', advance = 'no') '-',trim(action)  

! Load airfoil defined in command line 
  call load_airfoil(airfoil_filename, foil)

! Allocate xfoil variables
  call xfoil_init()
  xfoil_options%silent_mode = .true.
  call xfoil_defaults(xfoil_options)

! Set name of output file - from command line or auto?  
  if (trim(output_prefix) == '') then
    output_prefix = airfoil_filename (1:(index (airfoil_filename,'.', back = .true.) - 1))
    outname_auto = .true.
  else
    outname_auto = .false.
  end if

! Do actions according command line option

  select case (trim(action)) 

    case ('polar')        ! Generate polars in subdirectory ".\<output_prefix>_polars\*.*

      call check_and_do_polar_generation (input_file, output_prefix, foil)

    case ('norm')         ! Repanel, Normalize into "<output_prefix>.dat"

      call repanel_smooth (input_file, outname_auto, output_prefix, foil, visualizer, .false.)

    case ('smooth')       ! Repanel, Normalize and smooth into "<output_prefix>.dat"

      call repanel_smooth (input_file, outname_auto, output_prefix, foil, visualizer, .true.)
  
    case ('flap')         ! Repaneland set flap into "<output_prefix>.dat"

      call set_flap (input_file, outname_auto, output_prefix, foil, visualizer)

    case ('check')        ! Check the curvature quality of airfoil surface

      call check_foil_curvature (input_file, output_prefix, foil, visualizer)

    case ('set')         ! set geometry value like thickness etc...
      
      call set_geometry_value (input_file, outname_auto, output_prefix, foil, value_argument, visualizer)

    case ('blend')         ! blend two airfoils...
      
      if (trim(second_airfoil_filename) == "") &
        call my_stop("Must specify a second airfoil file with the -a2 option.")

      call load_airfoil(second_airfoil_filename, blend_foil)
      call blend_foils (input_file, outname_auto, output_prefix, foil, blend_foil, value_argument, visualizer)

    case default

      write (*,*)
      call print_error ("Unknown action '"//trim(action)//"' defined for paramter '-w'")
      call print_worker_usage()

  end select 
 
  write (*,*) 

  call xfoil_cleanup()
  call deallocate_airfoil (foil)

end program xfoil_worker



!-------------------------------------------------------------------------
! Generate polars
! - read input file for namelist &polar_generation
! - get polar definitions
! - calculate polars for foil with foilname
! - write each polar to a file 
!-------------------------------------------------------------------------

subroutine check_and_do_polar_generation (input_file, output_prefix, foil)

  use vardef,             only : airfoil_type
  use xfoil_driver,       only : xfoil_geom_options_type, xfoil_options_type, re_type
  use input_output,       only : read_xfoil_options_inputs, read_xfoil_paneling_inputs
  use input_output,       only : read_cl_re_default
  use polar_operations,   only : read_init_polar_inputs, generate_polar_files
  use os_util

 
  character(*), intent(in)          :: input_file, output_prefix
  type (airfoil_type), intent (in)  :: foil

  type (xfoil_geom_options_type) :: xfoil_geom_options
  type (xfoil_options_type)      :: xfoil_options
  type (re_type)                 :: re_default
  logical                        :: generate_polar
  character (255)                :: polars_subdirectory

 
  call read_xfoil_options_inputs  (input_file, 0, xfoil_options)

! Read and set options for polar generation for each new design (generate_polar = .true.) 

  re_default%number = read_cl_re_default (0d0) 
  re_default%type   = 1

  call read_init_polar_inputs (input_file, 0, re_default, xfoil_options%ncrit, &
                              foil%name, generate_polar)


  if (generate_polar) then

    call read_xfoil_paneling_inputs (input_file, 0, xfoil_geom_options)
    xfoil_options%show_details = .true.
    write (*,*)
    write (*,*)

    ! Create subdir for polar files if not exist
    polars_subdirectory = trim(output_prefix)//'_polars'
    call make_directory (trim(polars_subdirectory))

    ! Generate polars in this subdir 
    call generate_polar_files (.true., trim(polars_subdirectory), foil, xfoil_geom_options, xfoil_options)

  end if

end subroutine check_and_do_polar_generation



!-------------------------------------------------------------------------
! Setting thickness of foil
!-------------------------------------------------------------------------

subroutine set_geometry_value (input_file, outname_auto, output_prefix, seed_foil, &
                               value_argument, visualizer)

  use vardef,             only: airfoil_type
  use os_util
  use xfoil_driver,       only : xfoil_set_thickness_camber, xfoil_set_te_gap
  use xfoil_driver,       only : xfoil_geom_options_type
  use airfoil_operations, only : airfoil_write
  use airfoil_operations, only : repanel_and_normalize_airfoil   
  use parametrization,    only : smooth_foil  
  use input_output,       only : read_xfoil_paneling_inputs
  
  character(*), intent(in)     :: output_prefix, value_argument, input_file
  type (airfoil_type), intent (inout)  :: seed_foil
  logical, intent(in)          :: visualizer, outname_auto

  type (airfoil_type) :: foil, foil_smoothed
  type (xfoil_geom_options_type)  :: geom_options
  character (20)      :: value_str
  character (2)       :: value_type
  character (255)     :: outname
  double precision    :: value_number

  write (*,*) 'Max thickness, camber or trailing edge gap ' 
  write (*,*) 

  call read_xfoil_paneling_inputs  (input_file, 0, geom_options)

  value_type = value_argument (1:(index (value_argument,'=') - 1))
  value_str  = value_argument ((index (value_argument,'=') + 1):)

  read (value_str ,*, iostat = ierr) value_number  

  if(ierr /= 0) & 
    call my_stop ("Wrong argument format '"//trim(value_argument)//"' in set command") 

  select case (trim(value_type))

    case ('t') 
      write (*,'(" - ",A)') 'Setting thickness to '//trim(adjustl(value_str))//'%'
      call xfoil_set_thickness_camber (seed_foil, (value_number / 100d0), 0d0, 0d0, 0d0, foil)

    case ('xt') 
      write (*,'(" - ",A)') 'Setting max. thickness position to '//trim(adjustl(value_str))//'%'
      call repanel_and_normalize_airfoil (seed_foil, geom_options, .false., foil_smoothed)
      call smooth_foil (.true., 0.1d0, foil_smoothed)

      call xfoil_set_thickness_camber (foil_smoothed, 0d0, (value_number / 100d0), 0d0, 0d0, foil)
      call smooth_foil (.false., 0.1d0, foil)

    case ('c') 
      write (*,'(" - ",A)') 'Setting camber to '//trim(adjustl(value_str))//'%'
      call xfoil_set_thickness_camber (seed_foil, 0d0, 0d0, (value_number / 100d0), 0d0, foil)

    case ('xc') 
      write (*,'(" - ",A)') 'Setting max. camber position to '//trim(adjustl(value_str))//'%'
      call repanel_and_normalize_airfoil (seed_foil, geom_options, .false., foil_smoothed)
      call smooth_foil (.true., 0.1d0, foil_smoothed)

      call xfoil_set_thickness_camber (foil_smoothed, 0d0, 0d0, 0d0, (value_number / 100d0), foil)
      call smooth_foil (.false., 0.1d0, foil)

    case ('te') 
      write (*,'(" - ",A)') 'Setting trailing edge gap to '//trim(adjustl(value_str))
      call xfoil_set_te_gap (seed_foil, (value_number / 100d0), 0.8d0, foil)

    case default
      call my_stop ('Unknown type for setting geometry')

  end select

  if (outname_auto) then 
    outname = trim(output_prefix) // '_' // (trim(value_type)) // "=" //trim(adjustl(value_str))
  else
    outname = trim(output_prefix)
  end if

  foil%name = trim(outname) 

  call airfoil_write   (trim(foil%name)//'.dat', trim(foil%name), foil)

  ! Write airfoil to _design_coordinates using Xoptfoil format for visualizer
  if (visualizer) then 
    call write_design_coordinates (trim(outname), 0, seed_foil)
    call write_design_coordinates (trim(outname), 1, foil)
  end if 

end subroutine set_geometry_value


!-------------------------------------------------------------------------
! Checks curvature quality of foil 
!-------------------------------------------------------------------------

subroutine check_foil_curvature (input_file, output_prefix, seed_foil, visualizer)

  use vardef,             only: airfoil_type
  use airfoil_evaluation, only: curv_spec, curv_top_spec, curv_bot_spec
  use airfoil_operations, only: split_foil, le_find
  use airfoil_operations, only: repanel_and_normalize_airfoil
  use input_sanity,       only: check_and_smooth_surface, auto_curvature_constraints
  use input_output,       only: read_xfoil_paneling_inputs, read_curvature_constraints_inputs
  use xfoil_driver,       only: xfoil_defaults, xfoil_options_type, xfoil_geom_options_type
  use xfoil_driver,       only: xfoil_set_airfoil, xfoil_get_geometry_info, get_te_gap
  use math_deps,          only: count_reversals, derivative2
  use os_util

  character(*), intent(in)     :: input_file
  character(*), intent(in)     :: output_prefix
  type (airfoil_type), intent (in)  :: seed_foil
  type (xfoil_geom_options_type)  :: geom_options
  logical, intent(in)          :: visualizer

  type (airfoil_type)          :: foil, tmp_foil, norm_foil, smooth_foil
  integer                      :: overall_quality, is, ie, nreversals
  double precision             :: curv_threshold, maxt, xmaxt, maxc, xmaxc

  write (*,*) 'Surface curvature with reversals and spikes'

  call read_xfoil_paneling_inputs  (input_file, 0, geom_options)

  tmp_foil = seed_foil

  call le_find      (seed_foil%x, seed_foil%z, tmp_foil%leclose, tmp_foil%xle, tmp_foil%zle, tmp_foil%addpoint_loc)
  call split_foil   (tmp_foil)

! Defaults

  call read_curvature_constraints_inputs (input_file, 0, curv_spec, curv_top_spec, curv_bot_spec)

!  ------------ seed airfoil data -----

  call xfoil_set_airfoil (seed_foil)        
  call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)

  write (*,*)
  call print_colored (COLOR_NOTE,'   ')
  call print_colored (COLOR_NOTE,&
      "Thickness "//strf('(F5.2)',maxt*100)//"% at "//strf('(F5.2)',xmaxt*100)//'%   |   ')
  call print_colored (COLOR_NOTE, &
         "Camber "//strf('(F5.2)',maxc*100)//"% at "//strf('(F5.2)',xmaxc*100)//'%   |   ')
  call print_colored (COLOR_NOTE, &
         "TE gap "//strf('(F5.2)',get_te_gap (seed_foil)*100)//"%")
  write (*,*)

!  ------------ analyze & smooth  -----

  ! do checks on repanel foil - also needed for LE point handling (!)
  call repanel_and_normalize_airfoil (tmp_foil, geom_options, .false., norm_foil)

  write(*,'(" - ",A)', advance='no') "Check_curvature and smooth."
  smooth_foil = norm_foil
  call check_and_smooth_surface (.true., .false., .true., smooth_foil, overall_quality)

!  ------------ Find best values  -----

  write (*,*) 
  write(*,'(" - ",A)') "Auto_curvature contraints for normalized airfoil"

  ! supress reversal warning in auto_curvature_constraints
  is = curv_top_spec%nskip_LE
  ie = size(norm_foil%zt) 
  curv_threshold = curv_top_spec%curv_threshold
  nreversals = count_reversals (is, ie, derivative2(norm_foil%xt, norm_foil%zt), curv_threshold) 

  curv_top_spec%max_curv_reverse = nreversals     
  call auto_curvature_constraints ('Top side', .true., norm_foil%xt, norm_foil%zt, curv_top_spec)

  is = curv_bot_spec%nskip_LE
  ie = size(norm_foil%zb) 
  curv_threshold = curv_bot_spec%curv_threshold
  nreversals = count_reversals (is, ie, derivative2(norm_foil%xb, norm_foil%zb), curv_threshold) 

  curv_bot_spec%max_curv_reverse = nreversals 
  call auto_curvature_constraints ('Bot side', .true., norm_foil%xb, norm_foil%zb, curv_bot_spec)

!  ------------ Write coordinates for visualizer  -----

  if (visualizer) then
    call write_design_coordinates (output_prefix, 0, seed_foil)
    norm_foil%name   = 'Normalized'
    call write_design_coordinates (output_prefix, 1, norm_foil)
    smooth_foil%name = 'Smoothed'
    call write_design_coordinates (output_prefix, 2, smooth_foil)
  end if 

end subroutine check_foil_curvature


!-------------------------------------------------------------------------
! Repanels and optionally smoothes foil based on settings in 'input file'
!-------------------------------------------------------------------------

subroutine repanel_smooth (input_file, outname_auto, output_prefix, seed_foil, visualizer, do_smoothing)

  use vardef,             only : airfoil_type
  use airfoil_evaluation, only : curv_spec, curv_top_spec, curv_bot_spec
  use xfoil_driver,       only : xfoil_geom_options_type
  use airfoil_operations, only : airfoil_write, transform_airfoil
  use input_sanity,       only : check_and_smooth_surface

  use airfoil_operations, only : repanel_and_normalize_airfoil, rebuild_airfoil
  use input_output,       only : read_curvature_constraints_inputs, read_xfoil_paneling_inputs


  character(*), intent(in)          :: input_file, output_prefix
  type (airfoil_type), intent (inout)  :: seed_foil
  logical, intent(in)               :: do_smoothing, visualizer, outname_auto

  type (airfoil_type) :: foil_smoothed, foil
  type (xfoil_geom_options_type) :: geom_options
  integer             :: overall_quality
  character (255)     :: outname


  if (do_smoothing) then 
    write (*,*) 'Repanel, normalize and smooth the airfoil'
  else
    write (*,*) 'Repanel and normalize the airfoil'
  end if 


! Read inputs file to get options needed 

  call read_xfoil_paneling_inputs  (input_file, 0, geom_options)

! Defaults

  call read_curvature_constraints_inputs (input_file, 0, curv_spec, curv_top_spec, curv_bot_spec)

! Prepare airfoil  - Repanel and split 

  call repanel_and_normalize_airfoil (seed_foil, geom_options, .false., foil)

! Smooth it 

  if (do_smoothing) then 

    if (outname_auto) then 
      outname = trim(output_prefix) // '-smoothed'
    else
      outname = trim(output_prefix)
    end if

    foil_smoothed = foil
    foil_smoothed%name = trim(outname)

    write(*,'(" - ",A)') "Smoothing..."
    call check_and_smooth_surface (.true., .true., .true., foil_smoothed, overall_quality)
  
    call airfoil_write   (trim(outname)//'.dat', trim(foil_smoothed%name), foil_smoothed)

  else                        ! no smoothing write repaneld foil 

    if (outname_auto) then 
      outname = trim(output_prefix) // '-norm'
    else
      outname = trim(output_prefix)
    end if

    foil%name   = trim(outname)
    call airfoil_write   (trim(outname)//'.dat', trim(foil%name), foil)

  end if 

! Write all airfoils to _design_coordinates using Xoptfoil format for visualizer
  
  if (visualizer) then 
    call write_design_coordinates (outname, 0, seed_foil)
    foil%name   = 'Normalized'
    call write_design_coordinates (outname, 1, foil)
    if (do_smoothing) then
      foil_smoothed%name   = 'Smoothed'
      call write_design_coordinates (outname, 2, foil_smoothed)
    end if
  end if 

end subroutine repanel_smooth


!-------------------------------------------------------------------------
! Blend to seed_foil a blend_foil by (value) % 
!-------------------------------------------------------------------------

subroutine blend_foils (input_file, outname_auto, output_prefix, seed_foil_in, blend_foil_in, value_argument, visualizer)

  use os_util
  use vardef,             only : airfoil_type
  use math_deps,          only : interp_vector
  use xfoil_driver,       only : xfoil_geom_options_type
  use xfoil_driver,       only : xfoil_apply_flap_deflection, xfoil_reload_airfoil
  use xfoil_driver,       only : xfoil_set_airfoil
  use airfoil_operations, only : airfoil_write
  use airfoil_operations, only : rebuild_airfoil
  use airfoil_operations, only : repanel_and_normalize_airfoil
  use input_output,       only : read_xfoil_paneling_inputs

  character(*), intent(in)          :: input_file, output_prefix, value_argument
  type (airfoil_type), intent (inout)  :: seed_foil_in, blend_foil_in
  logical, intent(in)               :: visualizer, outname_auto

  double precision, dimension(:), allocatable :: xt, xb, zt, zb, bxt, bxb, bzt, bzb
  double precision, dimension(:), allocatable :: zttmp, zbtmp, zt_blended, zb_blended
  type (airfoil_type) :: blended_foil, in_foil, blend_foil
  type (xfoil_geom_options_type) :: geom_options
  integer             :: pointst, pointsb
  double precision    :: blend_factor
  character (255)     :: outname


  write (*,*) 'The coordinates of two airfoils'

  read (value_argument ,*, iostat = ierr) blend_factor  

  if(ierr /= 0) & 
    call my_stop ("Wrong blend value format '"//trim(value_argument)//"' in blend command") 

! Argument could be in % or as a fraction 
  
  if (blend_factor > 1) blend_factor = blend_factor / 100d0
  if (blend_factor <0 .or. blend_factor > 1) & 
    call my_stop ("Blend value must be between 0 and 1.0 ( or 0 and 100)") 


! Read inputs file to get xfoil paneling options  

  call read_xfoil_paneling_inputs  (input_file, 0, geom_options)

! Prepare - Repanel both airfoils 

  call repanel_and_normalize_airfoil (seed_foil_in,  geom_options, .false., in_foil)
  call repanel_and_normalize_airfoil (blend_foil_in, geom_options, .false., blend_foil)

! Now split  in upper & lower side 

  xt = in_foil%xt
  xb = in_foil%xb
  zt = in_foil%zt
  zb = in_foil%zb

  bxt = blend_foil%xt
  bxb = blend_foil%xb
  bzt = blend_foil%zt
  bzb = blend_foil%zb

! Interpolate x-vals of blend_foil to match to seed airfoil points to x-vals 
!    - so the z-values can later be blended

  pointst = size(xt,1)
  pointsb = size(xb,1)
  allocate(zttmp(pointst))
  allocate(zbtmp(pointsb))
  call interp_vector(bxt, bzt, xt, zttmp)
  call interp_vector(bxb, bzb, xb, zbtmp)

! now blend the z-values of the two poylines to become the new one

  write (*,'(" - ",A, I3,A)') 'Blending '//trim(seed_foil_in%name)//' and '//&
           trim(blend_foil_in%name)//' with', int(blend_factor * 100),'%'
 
  zt_blended = (1d0 - blend_factor) * zt + blend_factor * zttmp
  zb_blended = (1d0 - blend_factor) * zb + blend_factor * zbtmp

! and build new foil 

  call rebuild_airfoil (xt, xb, zt_blended, zb_blended, blended_foil)

! Write airfoil to using Xoptfoil format for visualizer

  if (outname_auto) then 
    outname = trim(output_prefix) // '-blend'
  else
    outname = trim(output_prefix)
  end if

  blended_foil%name = trim(outname)
  call airfoil_write   (trim(outname)//'.dat', trim(blended_foil%name), blended_foil)


  if (visualizer) then 
    call write_design_coordinates (outname, 0, in_foil)
    call write_design_coordinates (outname, 1, blend_foil)
    call write_design_coordinates (outname, 2, blended_foil)
  end if 

end subroutine blend_foils

!-------------------------------------------------------------------------
! Repanels and set flaps of foil based on settings in 'input file'
!-------------------------------------------------------------------------

subroutine set_flap (input_file, outname_auto, output_prefix, seed_foil, visualizer)

  use os_util
  use vardef,             only : airfoil_type, flap_spec_type
  use xfoil_driver,       only : xfoil_geom_options_type
  use xfoil_driver,       only : xfoil_apply_flap_deflection, xfoil_reload_airfoil
  use xfoil_driver,       only : xfoil_set_airfoil
  use airfoil_operations, only : airfoil_write, transform_airfoil, get_split_points
  use airfoil_operations, only : repanel_and_normalize_airfoil
  use input_output,       only : read_flap_inputs, read_xfoil_paneling_inputs


  character(*), intent(in)          :: input_file, output_prefix
  type (airfoil_type), intent (inout)  :: seed_foil
  logical, intent(in)               :: visualizer, outname_auto

  type (airfoil_type)             :: foil, foil_flapped
  type (flap_spec_type)           :: flap_spec
  type (xfoil_geom_options_type)  :: geom_options
  double precision, dimension(50) :: flap_degrees
  character(8), dimension(50)     :: flap_selection

  character(20)       :: text_degrees
  integer             :: ndegrees
  character (255)     :: outname, text_out


! Read inputs file to get xfoil paneling options  

  call read_xfoil_paneling_inputs  (input_file, 0, geom_options)
  call read_flap_inputs            (input_file, 0, flap_spec, flap_degrees, flap_selection)

! flap set? 

  ndegrees = 0

  do i = size(flap_degrees), 1, -1
    if (flap_degrees(i) /= 0d0) then
      ndegrees = i
      exit
    end if
  end do

  if (ndegrees == 0) then 
    call my_stop ('No flap angles defined in input file')
  elseif (ndegrees == 1) then 
    write (*,'(A)', advance = 'no') 'Setting one flap position'
  else
    write (*,'(A,I2,A)', advance = 'no') 'Setting',ndegrees,' flap positions'
  end if
  write (*,'(1x,A,I2,A,F4.1,A)') 'at ', int (flap_spec%x_flap*1d2), &
                '% ('//flap_spec%y_flap_spec//'=', flap_spec%y_flap,')'


! Repanel seed airfoil with xfoil PANGEN 

  call repanel_and_normalize_airfoil (seed_foil, geom_options, .false., foil)

  call xfoil_set_airfoil(foil)

! Write airfoil to _design_coordinates using Xoptfoil format for visualizer

  if (outname_auto) then 
    outname = trim(output_prefix) // '-f' 
  else
    outname = trim(output_prefix)
  end if


  if (visualizer) then 
    foil%name   = output_prefix
    call write_design_coordinates (output_prefix, 0, foil)
  end if 

! Now set flap to all requested angles

  do i = 1, ndegrees

    if (int(flap_degrees(i))*10  == int(flap_degrees(i)*10d0)) then  !degree having decimal?
      write (text_degrees,'(SP,I3)') int (flap_degrees(i))
    else
      write (text_degrees,'(SP,F6.1)') flap_degrees(i)
    end if

    write (text_out,'(A,F4.1,A)') 'Setting flaps to '//trim(adjustl(text_degrees))//' degrees'
    call print_note_only ('- '//trim(text_out))

    call xfoil_set_airfoil(foil)
    call xfoil_apply_flap_deflection(flap_spec, flap_degrees(i))
    call xfoil_reload_airfoil(foil_flapped)

    if (outname_auto) then 
      outname = trim(output_prefix) // '-f' // trim(adjustl(text_degrees))
    else
      outname = trim(output_prefix)
    end if
  
    foil_flapped%name   = trim(outname) 
    call airfoil_write   (trim(outname)//'.dat', trim(foil_flapped%name), foil_flapped)

    ! Write airfoil to _design_coordinates using Xoptfoil format for visualizer
  
    if (visualizer) then 
      call write_design_coordinates (output_prefix, i, foil_flapped)
    end if 

  end do

end subroutine set_flap


!-------------------------------------------------------------------------
! writes 'output_prefix'_design_coordinates for foil
!-------------------------------------------------------------------------

subroutine write_design_coordinates (output_prefix, designcounter, foil)

  use vardef,             only : airfoil_type, DESIGN_SUBDIR_POSTFIX
  use airfoil_operations, only : airfoil_write_to_unit
  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_get_geometry_info
  use os_util


  character(*), intent(in)          :: output_prefix
  integer, intent(in)               :: designcounter
  type (airfoil_type), intent (in)  :: foil

  double precision :: maxt, xmaxt, maxc, xmaxc
  character(255) :: foilfile, text, title, design_subdir
  character(8)   :: maxtchar, xmaxtchar, maxcchar, xmaxcchar
  integer :: foilunit
             
! Get geometry info

  call xfoil_set_airfoil (foil)
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

! Create subdirectory for all the design files 

  design_subdir = trim(output_prefix) // DESIGN_SUBDIR_POSTFIX
  call make_directory (trim(design_subdir))
  design_subdir = trim(design_subdir) // '/'

  foilfile = trim(design_subdir)//'Design_Coordinates.dat'
  foilunit = 13

! Open files and write headers, if necessary

  if (designcounter == 0) then

!   Header for coordinate file

    call print_note_only ("- Writing coordinates for visualizer to "//trim(design_subdir)//'...')

    open(unit=foilunit, file=foilfile, status='replace')
    write(foilunit,'(A)') 'title="Airfoil coordinates"'

!   Add 2nd and 3rd derivative to
!        ...design_coordinates.dat to show it in visualizer
    write(foilunit,'(A)') 'variables="x" "z" "2nd derivative" "3rd derivative"'

    title =  'zone t="Seed airfoil, '//'name='//trim(foil%name)//', maxt='//trim(maxtchar)//&
             ', xmaxt='//trim(xmaxtchar)//', maxc='//&
              trim(maxcchar)//', xmaxc='//trim(xmaxcchar)//'"'
  else

!   Open coordinate file and write zone header

    write(text,*) designcounter
    text = adjustl(text)

    open(unit=foilunit, file=foilfile, status='old', position='append', err=900)
    title =  'zone t="Airfoil, '//'name='//trim(adjustl(foil%name))//', maxt='//trim(maxtchar)//&
             ', xmaxt='//trim(xmaxtchar)//', maxc='//&
              trim(maxcchar)//', xmaxc='//trim(xmaxcchar)//'", '//&
             'SOLUTIONTIME='//trim(text)

  end if

! Write coordinates to file

  call  airfoil_write_to_unit (foilunit, title, foil, .True.)

! Close output files

  close(foilunit)

  return

! Warning if there was an error opening design_coordinates file

900 Call print_warning ("Warning: unable to open "//trim(foilfile)//". Skipping ...")
  write_airfoil_optimization_progress = 1
  return

end subroutine write_design_coordinates


!-------------------------------------------------------------------------
! Reads command line arguments for input file name and output file prefix
!-------------------------------------------------------------------------

subroutine read_worker_clo(input_file, output_prefix, airfoil_name, action, &
                           second_airfoil_filename, value_argument, visualizer)

  use os_util

  character(*), intent(inout) :: input_file, output_prefix, action, airfoil_name, value_argument
  character(*), intent(inout) :: second_airfoil_filename
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
    else if (trim(arg) == "-a2") then
      if (i == nargs) then
        call my_stop("Must specify filename of second airfoil for -a2 option.")
      else
        call getarg(i+1, second_airfoil_filename)
        i = i+2
      end if
    else if (trim(arg) == "-w") then
      if (i == nargs) then
        call my_stop("Must specify an action for the worker e.g. polar")
      else
        call getarg(i+1, action)
        i = i+2
        if (trim(action) == 'set') then
          call getarg(i, value_argument)
          i = i+1
        elseif (trim(action) == 'blend') then
          call getarg(i, value_argument)
          i = i+1
        end if 
      end if
    else if (trim(arg) == "-v") then
      visualizer = .true.
      i = i+1
    else if ( (trim(arg) == "-h") .or. (trim(arg) == "--help") ) then
      call print_worker_usage
      stop
    else
      call print_error ("Unrecognized option: "//trim(arg))
      write (*,*)
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

  write(*,'(A)') 'Xfoil_Worker      Version '//trim(PACKAGE_VERSION)//  &
                 '              (c) 2021 Jochen Guenzel'
  write(*,'(A)')
  write(*,'(A)') "Usage: Xfoil_worker -w worker_action [Options]"
  write(*,'(A)')
  write(*,'(A)') "  -w polar          Generate polars of 'airfoil_file'"
  write(*,'(A)') "  -w norm           Repanel, normalize 'airfoil_file'"
  write(*,'(A)') "  -w smooth         Repanel, normalize, smooth 'airfoil_file'"
  write(*,'(A)') "  -w flap           Set flap of 'airfoil_file'"
  write(*,'(A)') "  -w check          Check the quality of surface curvature'"
  write(*,'(A)') "  -w set [arg]      Set geometry parameters where [arg]:"
  write(*,'(A)') "                       't=zz'  max. thickness in % chord"
  write(*,'(A)') "                       'xt=zz' max. thickness location in % chord"
  write(*,'(A)') "                       'c=zz'  max. camber in % chord"
  write(*,'(A)') "                       'xt=zz' max. camber location in % chord"
  write(*,'(A)') "                       'te=y'  trailing edge gap in % chord (80% blending)"
  write(*,'(A)') "  -w blend xx       Blend 'airfoil_file' with 'second_airfoil_file' by xx%"
  write(*,'(A)')
  write(*,'(A)') "Options:"
  write(*,'(A)') "  -i input_file     Specify an input file (default: 'inputs.txt')"
  write(*,'(A)') "  -o output_prefix  Specify an output prefix (default: 'foil')"
  write(*,'(A)') "  -r xxxxxx         Specify a default reynolds number (re_default)"
  write(*,'(A)') "  -a airfoil_file   Specify filename of seed airfoil"
  write(*,'(A)') "  -a2 airfoil_file  Specify filename of a second airfoil (for blending)"
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

  use math_deps,       only : derivative2, derivative3 


  integer, intent(in) :: npoints
  character(*), intent(in) :: info
  double precision, dimension(npoints), intent(in) :: x, y
  double precision, dimension(npoints) :: deriv2, deriv3

  integer :: iunit, i
  character(100) :: smoothfile

  deriv2 = derivative2 (x, y)
  deriv3 = derivative3 (x, y)


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


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
  use os_util 
  use airfoil_operations, only : load_airfoil, my_stop, airfoil_write,le_find
  use xfoil_driver,       only : xfoil_init, xfoil_cleanup, xfoil_options_type
  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_reload_airfoil, xfoil_defaults
  use polar_operations,   only : check_and_do_polar_generation

  implicit none

#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION ""
#endif

  type(airfoil_type) :: foil, blend_foil
  type (xfoil_options_type) :: xfoil_options

  character(255)     :: input_file, output_prefix, airfoil_filename, second_airfoil_filename
  character(20)      :: action, value_argument
  logical            :: visualizer

  write(*,'(A)') 

! Set default names and read command line arguments

  input_file        = 'inputs.txt'
  output_prefix     = ''
  action            = ''
  airfoil_filename  = ''
  visualizer        = .false.
  call read_worker_clo(input_file, output_prefix, airfoil_filename, action, & 
                       second_airfoil_filename, value_argument, visualizer)

  if (trim(action) == "") &
    call my_stop("Must specify an action for the worker with -w option.")
  if (trim(airfoil_filename) == "") &
    call my_stop("Must specify an airfoil file with the -a option.")

! Load airfoil defined in command line 
  call load_airfoil(airfoil_filename, foil)

! Allocate xfoil variables
  call xfoil_init()
  xfoil_options%silent_mode = .true.
  call xfoil_defaults(xfoil_options)



! Do actions according command line option

  select case (trim(action)) 

    case ('polar')        ! Generate polars in subdirectory ".\<output_prefix>_polars\*.*

      if (trim(output_prefix) == '') & 
        output_prefix = airfoil_filename (1:(index (airfoil_filename,'.', back = .true.) - 1))

      call check_and_do_polar_generation (input_file, output_prefix, foil)

    case ('norm')         ! Repanel, Normalize into "<output_prefix>.dat"

      if (trim(output_prefix) == '') & 
        output_prefix = airfoil_filename (1:(index (airfoil_filename,'.', back = .true.) - 1))//'-norm'

      call repanel_smooth (input_file, output_prefix, foil, visualizer, .false.)

    case ('smooth')       ! Repanel, Normalize and smooth into "<output_prefix>.dat"

      if (trim(output_prefix) == '') & 
        output_prefix = airfoil_filename (1:(index (airfoil_filename,'.', back = .true.) - 1))//'-smoothed'

      call repanel_smooth (input_file, output_prefix, foil, visualizer, .true.)
  
    case ('flap')         ! Repaneland set flap into "<output_prefix>.dat"

      if (trim(output_prefix) == '') & 
        output_prefix = airfoil_filename (1:(index (airfoil_filename,'.',back = .true.) - 1))//'-f'

      call set_flap (input_file, output_prefix, foil, visualizer)

    case ('check')        ! Check the curvature quality of airfoil surface

      if (trim(output_prefix) == '') & 
        output_prefix = airfoil_filename (1:(index (airfoil_filename,'.', back = .true.) - 1))
      call check_foil_curvature (input_file, output_prefix, foil, visualizer)

    case ('set')         ! set geometry value like thickness etc...
      
      if (trim(output_prefix) == '') & 
        output_prefix = airfoil_filename (1:(index (airfoil_filename,'.', back = .true.) - 1))
      call set_geometry_value (output_prefix, foil, value_argument, visualizer)

    case ('blend')         ! blend two airfoils...
      
      if (trim(output_prefix) == '') & 
        output_prefix = airfoil_filename (1:(index (airfoil_filename,'.', back = .true.) - 1)) // &
                        '-blend'//trim(adjustl(value_argument))

      call load_airfoil(second_airfoil_filename, blend_foil)
      call blend_foils (input_file, output_prefix, foil, blend_foil, value_argument, visualizer)

    case default

      write (*,*)
      call print_error ("Unknown action '"//trim(action)//"' defined for paramter '-w'")
      call print_worker_usage()

  end select 

  call xfoil_cleanup()
  call deallocate_airfoil (foil)


end program xfoil_worker


!-------------------------------------------------------------------------
! dev: test setting thickness of foil
!-------------------------------------------------------------------------

subroutine set_geometry_value (output_prefix, seed_foil, value_argument, visualizer)

  use vardef,             only: airfoil_type
  use os_util
  use xfoil_driver,       only : xfoil_set_thickness_camber
  use airfoil_operations, only : airfoil_write, my_stop

  character(*), intent(in)     :: output_prefix, value_argument
  type (airfoil_type), intent (inout)  :: seed_foil
  logical, intent(in)          :: visualizer

  type (airfoil_type) :: foil
  character (20)     :: value_str, label
  character (2)      :: value_type

  value_type = value_argument (1:(index (value_argument,'=') - 1))
  value_str  = value_argument ((index (value_argument,'=') + 1):)

  read (value_str ,*, iostat = ierr) value_number  

  if(ierr /= 0) & 
    call my_stop ("Wrong argument format '"//trim(value_argument)//"' in set command") 

  write (*,*) 

  select case (trim(value_type))

    case ('t') 
      write (*,'(1x,A)') 'Setting thickness to '//trim(adjustl(value_str))//'%'
      call xfoil_set_thickness_camber (seed_foil, (value_number / 100d0), 0d0, 0d0, 0d0, foil)

    case ('xt') 
      write (*,'(1x,A)') 'Setting max. thickness position to '//trim(adjustl(value_str))//'%'
      call xfoil_set_thickness_camber (seed_foil, 0d0, (value_number / 100d0), 0d0, 0d0, foil)

    case ('c') 
      write (*,'(1x,A)') 'Setting camber to '//trim(adjustl(value_str))//'%'
      call xfoil_set_thickness_camber (seed_foil, 0d0, 0d0, (value_number / 100d0), 0d0, foil)

    case ('xc') 
      write (*,'(1x,A)') 'Setting max. camber position to '//trim(adjustl(value_str))//'%'
      call xfoil_set_thickness_camber (seed_foil, 0d0, 0d0, 0d0, (value_number / 100d0), foil)

    case default
      call my_stop ('Unknown type for setting geometry')

  end select

  label     = '-' // (trim(value_type)) // trim(adjustl(value_str))
  foil%name = trim(output_prefix) // trim(label)

  call airfoil_write   (trim(foil%name)//'.dat', trim(foil%name), foil)

  ! Write airfoil to _design_coordinates using Xoptfoil format for visualizer
  if (visualizer) then 
    call write_design_coordinates (trim(output_prefix)//label, 0, seed_foil)
    call write_design_coordinates (trim(output_prefix)//label, 1, foil)
  end if 

end subroutine set_geometry_value


!-------------------------------------------------------------------------
! Checks curvature quality of foil 
!-------------------------------------------------------------------------

subroutine check_foil_curvature (input_file, output_prefix, seed_foil, visualizer)

  use vardef,             only: airfoil_type
  use airfoil_evaluation, only: curv_threshold, spike_threshold, highlow_threshold
  use airfoil_evaluation, only: max_curv_reverse_top, max_curv_reverse_bot
  use airfoil_evaluation, only: max_te_curvature, check_curvature, auto_curvature
  use airfoil_operations, only: split_foil, le_find, assess_surface, smooth_it
  use airfoil_operations, only: repanel_and_normalize_airfoil
  use math_deps,          only: nreversals_using_threshold
  use input_sanity,       only: check_and_smooth_surface, auto_curvature_constraints
  use input_output,       only: read_geo_constraints_inputs
  use xfoil_driver,       only: xfoil_defaults, xfoil_options_type, xfoil_geom_options_type
  use polar_operations,   only: read_xfoil_paneling_inputs
  use os_util

  character(*), intent(in)     :: input_file
  character(*), intent(in)     :: output_prefix
  type (airfoil_type), intent (in)  :: seed_foil
  logical, intent(in)          :: visualizer

  type (airfoil_type)          :: foil, tmp_foil

  tmp_foil = seed_foil

  call le_find      (seed_foil%x, seed_foil%z, tmp_foil%leclose, tmp_foil%xle, tmp_foil%zle, tmp_foil%addpoint_loc)
  call split_foil   (tmp_foil)

  ! Defaults

  check_curvature      = .true.
  auto_curvature       = .true.

  highlow_threshold     = 0.03d0
  curv_threshold        = 0.02d0
  max_te_curvature      = 0.2d0
  max_curv_reverse_top = 0
  max_curv_reverse_bot = 0
  max_curv_highlow_top = 0
  max_curv_highlow_bot = 0

  call  read_geo_constraints_inputs  (input_file, &
                                      check_curvature, auto_curvature,  &
                                      max_te_curvature,    &
                                      max_curv_reverse_top, max_curv_reverse_bot, &
                                      max_curv_highlow_top, max_curv_highlow_bot, &
                                      curv_threshold,highlow_threshold)
  spike_threshold = 0.8d0


!  ------------ analyze & smooth  -----

  ! do checks on repanel foil - also needed for LE point handling (!)
  call repanel_and_normalize_airfoil (tmp_foil, tmp_foil%npoint, .false., foil)
  call check_and_smooth_surface (.true., .true., foil)


!  ------------ set best values  -----

  call auto_curvature_constraints (.true., foil, &
                                curv_threshold, highlow_threshold, max_te_curvature, &
                                max_curv_highlow_top, max_curv_highlow_bot, &
                                max_curv_reverse_top, max_curv_reverse_bot)

  write(*,*)

  if (visualizer) then 
    call write_design_coordinates (output_prefix, 0, seed_foil)
    call write_design_coordinates (output_prefix, 1, foil)
  end if 

end subroutine check_foil_curvature


!-------------------------------------------------------------------------
! Repanels and optionally smoothes foil based on settings in 'input file'
!-------------------------------------------------------------------------

subroutine repanel_smooth (input_file, output_prefix, seed_foil, visualizer, do_smoothing)

  use vardef,             only : airfoil_type
  use airfoil_evaluation, only : spike_threshold, highlow_threshold, curv_threshold
  use airfoil_evaluation, only : check_curvature, auto_curvature
  use airfoil_evaluation, only : max_te_curvature
  use airfoil_evaluation, only : max_curv_reverse_top, max_curv_reverse_bot, &
                                 max_curv_highlow_top, max_curv_highlow_bot
  use xfoil_driver,       only : xfoil_geom_options_type
  use airfoil_operations, only : airfoil_write, transform_airfoil
  use airfoil_operations, only : assess_surface, smooth_it, rebuild_airfoil
  use airfoil_operations, only : repanel_and_normalize_airfoil
  use polar_operations,   only : read_xfoil_paneling_inputs
  use input_output,       only : read_geo_constraints_inputs


  character(*), intent(in)          :: input_file, output_prefix
  type (airfoil_type), intent (inout)  :: seed_foil
  logical, intent(in)               :: do_smoothing, visualizer

  double precision, dimension(:), allocatable :: zt_smoothed, zb_smoothed
  type (airfoil_type) :: foil_smoothed, foil
  type (xfoil_geom_options_type) :: geom_options

! Read inputs file to get options needed 

  call read_xfoil_paneling_inputs  (input_file, geom_options)

  ! Defaults

  check_curvature      = .true.
  auto_curvature       = .true.

  highlow_threshold     = 0.03d0
  curv_threshold        = 0.02d0
  max_te_curvature      = 0.2d0
  max_curv_reverse_top = 0
  max_curv_reverse_bot = 0
  max_curv_highlow_top = 0
  max_curv_highlow_bot = 0

  call read_geo_constraints_inputs (input_file, &
                                      check_curvature, auto_curvature,  &
                                      max_te_curvature,    &
                                      max_curv_reverse_top, max_curv_reverse_bot, &
                                      max_curv_highlow_top, max_curv_highlow_bot, &
                                      curv_threshold,highlow_threshold)
    spike_threshold       = 0.8d0

! Prepare airfoil  - Repanel and split 

  call repanel_and_normalize_airfoil (seed_foil, geom_options%npan, .false., foil)

! Smooth it 

  if (do_smoothing) then 

    write (*,'(/,1x,A)') 'Smoothing Top surface ...'
    zt_smoothed = foil%zt
    call smooth_it (.true., spike_threshold, foil%xt, zt_smoothed) 

    write (*,'(/,1x,A)') 'Smoothing Bottom surface ...'
    zb_smoothed = foil%zb
    call smooth_it (.true., spike_threshold, foil%xb, zb_smoothed)

  ! Rebuild foil and write to file

    call rebuild_airfoil (foil%xt, foil%xb, zt_smoothed, zb_smoothed, foil_smoothed)

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
    if (do_smoothing) then
      foil_smoothed%name   = 'smoothed'
      call write_design_coordinates (output_prefix, 2, foil_smoothed)
    end if
  end if 

end subroutine repanel_smooth


!-------------------------------------------------------------------------
! Blend to seed_foil a blend_foil by (value) % 
!-------------------------------------------------------------------------

subroutine blend_foils (input_file, output_prefix, seed_foil_in, blend_foil_in, value_argument, visualizer)

  use vardef,             only : airfoil_type
  use math_deps,          only : interp_vector
  use xfoil_driver,       only : xfoil_geom_options_type
  use xfoil_driver,       only : xfoil_apply_flap_deflection, xfoil_reload_airfoil
  use xfoil_driver,       only : xfoil_set_airfoil
  use airfoil_operations, only : airfoil_write
  use airfoil_operations, only : rebuild_airfoil, my_stop
  use airfoil_operations, only : repanel_and_normalize_airfoil
  use polar_operations,   only : read_xfoil_paneling_inputs

  character(*), intent(in)          :: input_file, output_prefix, value_argument
  type (airfoil_type), intent (inout)  :: seed_foil_in, blend_foil_in
  logical, intent(in)               :: visualizer

  double precision, dimension(:), allocatable :: xt, xb, zt, zb, bxt, bxb, bzt, bzb
  double precision, dimension(:), allocatable :: zttmp, zbtmp, zt_blended, zb_blended
  type (airfoil_type) :: blended_foil, in_foil, blend_foil
  type (xfoil_geom_options_type) :: geom_options
  integer       :: pointst, pointsb
  double precision :: blend_factor

  read (value_argument ,*, iostat = ierr) blend_factor  

  if(ierr /= 0) & 
    call my_stop ("Wrong blend value format '"//trim(value_argument)//"' in blend command") 

! Argument could be in % or as a fraction 
  
  if (blend_factor > 1) blend_factor = blend_factor / 100d0
  if (blend_factor <0 .or. blend_factor > 1) & 
    call my_stop ("Blend value must be between 0 and 1.0 ( or 0 and 100)") 


! Read inputs file to get xfoil paneling options  

  call read_xfoil_paneling_inputs  (input_file, geom_options)

! Prepare - Repanel both airfoils 

  call repanel_and_normalize_airfoil (seed_foil_in,  geom_options%npan, .false., in_foil)
  call repanel_and_normalize_airfoil (blend_foil_in, geom_options%npan, .false., blend_foil)

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

  write (*,'(/1x,A, I3,A)') 'Blending '//trim(seed_foil_in%name)//' and '//&
           trim(blend_foil_in%name)//' with', int(blend_factor * 100),'%'
 
  zt_blended = (1d0 - blend_factor) * zt + blend_factor * zttmp
  zb_blended = (1d0 - blend_factor) * zb + blend_factor * zbtmp

! and build new foil 

  call rebuild_airfoil (xt, xb, zt_blended, zb_blended, blended_foil)

! Write airfoil to _design_coordinates using Xoptfoil format for visualizer

  blended_foil%name = trim(output_prefix)
  call airfoil_write   (trim(blended_foil%name)//'.dat', trim(blended_foil%name), blended_foil)


  if (visualizer) then 
    call write_design_coordinates (blended_foil%name, 0, in_foil)
    call write_design_coordinates (blended_foil%name, 1, blend_foil)
    call write_design_coordinates (blended_foil%name, 2, blended_foil)
  end if 

end subroutine blend_foils

!-------------------------------------------------------------------------
! Repanels and set flaps of foil based on settings in 'input file'
!-------------------------------------------------------------------------

subroutine set_flap (input_file, output_prefix, seed_foil, visualizer)

  use vardef,             only : airfoil_type, flap_spec_type
  use xfoil_driver,       only : xfoil_geom_options_type
  use xfoil_driver,       only : xfoil_apply_flap_deflection, xfoil_reload_airfoil
  use xfoil_driver,       only : xfoil_set_airfoil
  use airfoil_operations, only : airfoil_write, transform_airfoil, get_split_points
  use airfoil_operations, only : assess_surface
  use airfoil_operations, only : repanel_and_normalize_airfoil
  use polar_operations,   only : read_xfoil_paneling_inputs, read_flap_inputs


  character(*), intent(in)          :: input_file, output_prefix
  type (airfoil_type), intent (inout)  :: seed_foil
  logical, intent(in)               :: visualizer

  type (airfoil_type)             :: foil, foil_flapped
  type (flap_spec_type)           :: flap_spec
  type (xfoil_geom_options_type)  :: geom_options
  double precision, dimension(50) :: flap_degrees
  character(20) :: text_degrees
  integer       :: ndegrees


! Read inputs file to get xfoil paneling options  

  call read_xfoil_paneling_inputs  (input_file, geom_options)
  call read_flap_inputs            (input_file, flap_spec, ndegrees, flap_degrees)

! Repanel seed airfoil with xfoil PANGEN 

  call repanel_and_normalize_airfoil (seed_foil, geom_options%npan, .false., foil)

  call xfoil_set_airfoil(foil)

! Write airfoil to _design_coordinates using Xoptfoil format for visualizer

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

    write (*,'(1x,A,I2,A,F4.1,A)') 'Setting flaps at ', int (x_flap*1d2), &
          '% ('//flap_spec%y_flap_spec//'=', &
          flap_spec%y_flap,') to '//trim(adjustl(text_degrees))//' degrees'

    call xfoil_set_airfoil(foil)
    call xfoil_apply_flap_deflection(flap_spec, flap_degrees(i))
    call xfoil_reload_airfoil(foil_flapped)

    foil_flapped%name   = trim(output_prefix) // trim(adjustl(text_degrees))
    call airfoil_write   (trim(foil_flapped%name)//'.dat', trim(foil_flapped%name), foil_flapped)

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

  use vardef,             only : airfoil_type
  use airfoil_operations, only : airfoil_write_to_unit
  use xfoil_driver,       only : xfoil_set_airfoil, xfoil_get_geometry_info

  character(*), intent(in)          :: output_prefix
  integer, intent(in)               :: designcounter
  type (airfoil_type), intent (in)  :: foil

  double precision :: maxt, xmaxt, maxc, xmaxc
  character(100) :: foilfile, text, title
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

  foilfile = trim(output_prefix)//'_design_coordinates.dat'
  foilunit = 13

! Open files and write headers, if necessary

  if (designcounter == 0) then

!   Header for coordinate file

    write(*,*) "Writing "//trim(foil%name)//" coordinates for seed airfoil to file "//               &
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

    write(*,*) "Writing "//trim(foil%name)//" coordinates for design number "//trim(text)//        &
               " to file "//trim(foilfile)//" ..."
    open(unit=foilunit, file=foilfile, status='old', position='append', err=900)
    title =  'zone t="Airfoil, maxt='//trim(maxtchar)//&
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

900 write(*,*) "Warning: unable to open "//trim(foilfile)//". Skipping ..."
  write_airfoil_optimization_progress = 1
  return

end subroutine write_design_coordinates


!-------------------------------------------------------------------------
! Reads command line arguments for input file name and output file prefix
!-------------------------------------------------------------------------

subroutine read_worker_clo(input_file, output_prefix, airfoil_name, action, &
                           second_airfoil_filename, value_argument, visualizer)

  use airfoil_operations, only : my_stop
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
                 '              (c) 2020 Jochen Guenzel'
  write(*,'(A)')
  write(*,'(A)') "Usage: Xfoil_worker -w worker_action [Options]"
  write(*,'(A)')
  write(*,'(A)') "  -w polar          Generate polars of 'airfoil_file'"
  write(*,'(A)') "  -w norm           Repanel, normalize 'airfoil_file'"
  write(*,'(A)') "  -w smooth         Repanel, normalize, smooth 'airfoil_file'"
  write(*,'(A)') "  -w flap           Set flap of 'airfoil_file'"
  write(*,'(A)') "  -w check          Check the quality of surface curvature'"
  write(*,'(A)') "  -w set [arg]      Set max thickness or max camber or their locations"
  write(*,'(A)') "                      where [arg]:  't=zz' or 'c=zz' or 'xt=zz' or 'xc='zz' in percent"
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


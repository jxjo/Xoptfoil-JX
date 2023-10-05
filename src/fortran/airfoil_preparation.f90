! MIT License
! Copyright (c) 2023 jxjo

!
! Functions for preparing the airfoil prior to optimization 
!

module airfoil_preparation
  use os_util

  implicit none
  private

  public :: preset_airfoil_to_targets
  public :: transform_to_bezier_based
  public :: matchfoils_preprocessing
    
contains


!-----------------------------------------------------------------------------



!-----------------------------------------------------------------------------


subroutine preset_airfoil_to_targets (show_detail, foil) 

  !! Set airfoil thickness and camber according to defined geo targets 
  !!   and/or thickness/camber constraints (in airfoil evaluation commons)

  ! * deactivated as it's difficult to fit into initialization (xfoil) within main 

  use vardef,             only: airfoil_type
  use xfoil_driver,       only: xfoil_set_thickness_camber, xfoil_set_airfoil
  use xfoil_driver,       only: xfoil_get_geometry_info
  use airfoil_evaluation, only: geo_targets
  use airfoil_evaluation, only: max_thickness, min_thickness, max_camber, min_camber

  logical, intent (in)           :: show_detail
  type (airfoil_type), intent (inout)  :: foil

  type (airfoil_type) :: new_foil
  doubleprecision     :: maxt, xmaxt, maxc, xmaxc, new_camber, new_thick
  character (10)      :: cvalue
  integer             :: i, nptt, nptb, ngeo_targets
  logical             :: foil_changed 

  ! Is presetting activated? 

  if (.true.) return 

  foil_changed = .false.
  ngeo_targets = size(geo_targets)

  new_thick  = 0d0
  new_camber = 0d0

  if (ngeo_targets > 0) then 

    ! Set thickness / Camber of seed airfoil according geo targets, adjust constraints

    do i= 1, ngeo_targets

      select case (trim(geo_targets(i)%type))

        case ('Thickness')                   

          new_thick = geo_targets(i)%target_value
          foil_changed = .true.

          if (show_detail) then
            write (cvalue,'(F6.2)')  (new_thick * 100)
            call print_text ('- Scaling thickness to target value '// trim(adjustl(cvalue))//'%')
          end if

        case ('Camber')                      

          new_camber = geo_targets(i)%target_value
          foil_changed = .true.

          if (show_detail) then
            write (cvalue,'(F6.2)')  (new_camber * 100)
            call print_text ('- Scaling camber to target value '// trim(adjustl(cvalue))//'%')
          end if

      end select

    end do
    call xfoil_set_thickness_camber (foil, new_thick, 0d0, new_camber, 0d0, new_foil)

  else

    ! Set thickness / Camber of seed airfoil according constraints

    call xfoil_set_airfoil (foil)        
    call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)

    if (maxt > max_thickness) then

      new_thick = max_thickness *0.95d0
      call xfoil_set_thickness_camber (foil, new_thick, 0d0, 0d0, 0d0, new_foil)
      foil_changed = .true.

      if (show_detail) then
        write (cvalue,'(F6.2)')  (new_thick * 100)
        call print_text ('- Scaling thickness according constraint to '// trim(adjustl(cvalue))//'%')
      end if 

    elseif (maxt < min_thickness) then 

      new_thick = min_thickness *1.05d0
      call xfoil_set_thickness_camber (foil, new_thick, 0d0, 0d0, 0d0, new_foil)
      foil_changed = .true.

      if (show_detail) then
        write (cvalue,'(F6.2)')  (new_thick * 100)
        call print_text ('- Scaling thickness according constraint to '// trim(adjustl(cvalue))//'%')
      end if 

    end if 

    if (maxc > max_camber) then

      new_camber = max_camber *0.95d0
      call xfoil_set_thickness_camber (foil, 0d0, new_camber, 0d0, 0d0, new_foil)
      foil_changed = .true.

      if (show_detail) then
        write (cvalue,'(F6.2)')  (new_camber * 100)
        call print_text ('- Scaling camber according constraint to '// trim(adjustl(cvalue))//'%')
      end if
      
    elseif (maxc < min_camber) then 

      new_camber = min_camber *1.05d0
      call xfoil_set_thickness_camber (foil, 0d0, new_camber, 0d0, 0d0, new_foil)
      foil_changed = .true.

      if (show_detail) then
        write (cvalue,'(F6.2)')  (new_camber * 100)
        call print_text ('- Scaling camber according constraint to '// trim(adjustl(cvalue))//'%')
      end if

    end if 

  end if

  if (foil_changed) then

    ! Now rebuild foil out of new coordinates  ----------------------

    ! Sanity check - new_foil may not have different number of points
    if (foil%npoint /= new_foil%npoint) then
      call my_stop ('Number of points changed during thickness/camber modification')
    end if

    foil%z = new_foil%z
    nptt = size(foil%zt,1)
    nptb = size(foil%zb,1)

    ! get new upper and lower z-coordinates from modified airfoil 
    do i = 1, nptt
      foil%zt(i) = new_foil%z(nptt-i+1)      ! start from LE - top reverse - to LE
    end do
    do i = 1, nptb 
      foil%zb(i) = new_foil%z(nptt+i-1)      ! start from LE - bottom - to TE
    end do
  end if
  
end subroutine preset_airfoil_to_targets



!-----------------------------------------------------------------------------


subroutine transform_to_bezier_based (bez_spec, npan, foil)

  !! Transform foil to bezier based using simplex optimization for best fit of bezier curves  

  use vardef,               only : airfoil_type
  use airfoil_operations,   only : is_normalized_coord, split_foil_at_00, airfoil_write
  use airfoil_shape_bezier, only : bezier_eval_airfoil, write_bezier_file
  use airfoil_shape_bezier, only : bezier_spec_type

  type (bezier_spec_type), intent (inout) :: bez_spec
  integer, intent (in)                    :: npan
  type (airfoil_type), intent (inout)     :: foil

  ! Sanity check 
 
  if (.not. is_normalized_coord(foil)) then 
    call my_stop ('Airfoil is not normalized prior to Bezier transform')
  else
    call split_foil_at_00 (foil) 
  end if  

  write (*,'(" - ", A)') 'Create Bezier based airfoil'

  ! Simplex optimization (nelder mead) for both sides  

  call match_bezier_for_side  ('Top', foil%xt, foil%zt, bez_spec%ncpoints_top, bez_spec%px_top, bez_spec%py_top)
  call match_bezier_for_side  ('Bot', foil%xb, foil%zb, bez_spec%ncpoints_bot, bez_spec%px_bot, bez_spec%py_bot)

  ! build airfoil out of Bezier curves 

  call bezier_eval_airfoil (bez_spec, (npan+1), foil%x, foil%z)

  foil%npoint = size(foil%x)
  call split_foil_at_00 (foil)                  ! prepare for further need 
  foil%bezier_spec = bez_spec
  foil%bezier_based = .true.

  ! Write new foil and Bezier definition to file  

  call airfoil_write   (trim(foil%name)//'.dat', trim(foil%name), foil)

  call print_colored (COLOR_NOTE, "   Writing bezier  to ")
  call print_colored (COLOR_HIGH,trim(foil%name)//'.bez')
  write (*,*)
  call write_bezier_file (trim(foil%name)//'.bez', trim(foil%name), bez_spec)


end subroutine transform_to_bezier_based



subroutine match_bezier_for_side  (side, match_x, match_y, np, px, py)
  !! adapt a bezier curve to one side of an airfoil 
  !
  !    px_, py_:  x or y coordinates of the bezier control points for top and bot side 
  !    npan:      number of coordinates of airfoil x,y 
  !    x,y:       returns x,y coordinates at u

  use os_util
  use simplex_search,       only : simplexsearch, ds_options_type 
  use airfoil_evaluation,   only : side_to_match_x, side_to_match_y, match_side_objective_function
  use airfoil_shape_bezier, only : get_initial_bezier, bezier_to_dv, dv_to_bezier
  use airfoil_shape_bezier, only : bezier_eval_y_on_x

  implicit none
  character(*),  intent(in)       :: side
  double precision, intent(in)    :: match_x(:), match_y(:)
  integer,  intent(in)            :: np
  double precision, allocatable, intent(out)    :: px(:), py(:)

  type (ds_options_type)          :: ds_options
  double precision, allocatable   :: xmin(:), dv0(:)
  double precision                :: deviation (size(match_y))
  double precision                :: fmin, f0_ref, te_gap, dev_max, dev_norm2, dev_max_at
  integer                         :: steps, fevals, ndv, i
  
  ! nelder mead (simplex) optimization

  ds_options%tol   = 1d-5
  ds_options%maxit = 5000

  ! --- setup targets in module airfoil_evaluation for objective function 
  
  side_to_match_x = match_x 
  side_to_match_y = match_y 

  ! --- initial estimate for control points based on 'side to match'   
  call get_initial_bezier (side_to_match_x, side_to_match_y, np, px, py)

  ! --- start vector of design variables dv0 
  call bezier_to_dv (px, py, dv0)

  ndv = size(dv0)
  ! write (*,'(" - ",A,A,A,I2,A,I2,A)') 'Bezier nelder mead optimization for ',side,' side (', &
  !                                      np, ' points,', ndv, ' variables)'
  write(*,'(3x)', advance = 'no')
  call  print_colored (COLOR_NOTE, 'Matching Bezier for '//side//' side ('//stri(np)//' points): ')

  xmin = dv0                                      ! just for allocation 
  xmin = 0d0                                      ! result array 
  call simplexsearch(xmin, fmin, steps, fevals, match_side_objective_function, &
                     dv0, .false. , f0_ref, ds_options)

  te_gap = py(np)
  call dv_to_bezier (xmin, te_gap, px, py)

  ! --- finished - calc results 

  do i = 1, size(match_y)
    deviation(i) = match_y(i) - bezier_eval_y_on_x (px, py, match_x(i))
  end do 
  dev_norm2  = norm2 (deviation)
  dev_max    = maxval(abs(deviation))
  dev_max_at = match_x(maxloc (abs(deviation),1))
  ! write (*,'("   ",A,I4,A,A,f8.6,A,f8.6,A,f6.4)') '... ', steps, ' iterations', & 
  !                                                 ' - norm2 deviation: ', dev_norm2, &
  !                                                 ', max deviation: ', dev_max,' at x/c: ', dev_max_at
  call  print_colored (COLOR_NOTE, '... '//stri(steps)//' iterations'//' - deviation: '//strf('(f8.6)',dev_norm2))
  write(*,*)

end subroutine match_bezier_for_side


!-----------------------------------------------------------------------------
 
subroutine matchfoils_preprocessing(matchfoil_file)

  !! Preprocessing for non-aerodynamic optimization
  !! Prepare foil to match 

  use vardef,             only : airfoil_type, seed_foil
  use airfoil_evaluation, only : foil_to_match, xfoil_geom_options
  use airfoil_operations, only : get_seed_airfoil,  rebuild_airfoil
  use airfoil_operations, only : repanel_and_normalize_airfoil, split_foil_at_00
  use math_deps,          only : interp_vector, transformed_arccos
  use xfoil_driver,       only : xfoil_set_thickness_camber, xfoil_get_geometry_info, xfoil_set_airfoil
  
  character(*), intent(in) :: matchfoil_file

  type(airfoil_type) :: original_foil_to_match
  integer :: pointst, pointsb
  double precision, dimension(:), allocatable :: zttmp, zbtmp, xmatcht, xmatchb, zmatcht, zmatchb
  double precision :: maxt, xmaxt, maxc, xmaxc

  ! Load airfoil to match

  call get_seed_airfoil('from_file', matchfoil_file, original_foil_to_match)

  call print_text ('Preparing '//trim(original_foil_to_match%name)//' to be matched by '//&
                        trim(seed_foil%name),3)

  if(trim(seed_foil%name) == trim(original_foil_to_match%name)) then

  ! Seed and match foil are equal. Reduce thickness ... 

    call print_note ('Match foil and seed foil are the same. '// &
                     'The thickness of the match foil will be reduced bei 10%.', 3)

    call xfoil_set_airfoil (seed_foil)        
    call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)
    call xfoil_set_thickness_camber (seed_foil, maxt * 0.9d0 , 0d0, 0d0, 0d0, foil_to_match)
    call split_foil_at_00(foil_to_match)
    foil_to_match%name = seed_foil%name

  else

  ! Repanel to npan_fixed points and normalize to get LE at 0,0 and TE (1,0) and split

    write (*,*) 
    call repanel_and_normalize_airfoil (original_foil_to_match, xfoil_geom_options, .false., foil_to_match)

  ! Interpolate x-vals of foil to match to seed airfoil points to x-vals 
  !    - so the z-values can later be compared

    xmatcht = foil_to_match%xt
    xmatchb = foil_to_match%xb
    zmatcht = foil_to_match%zt
    zmatchb = foil_to_match%zb
  
    pointst = size(seed_foil%xt,1)
    pointsb = size(seed_foil%xb,1)
    allocate(zttmp(pointst))
    allocate(zbtmp(pointsb))
    zttmp(pointst) = zmatcht(size(zmatcht,1))
    zbtmp(pointsb) = zmatchb(size(zmatchb,1))
    call interp_vector(xmatcht, zmatcht, seed_foil%xt(1:pointst-1),                    &
                      zttmp(1:pointst-1))
    call interp_vector(xmatchb, zmatchb, seed_foil%xb(1:pointsb-1),                    &
                      zbtmp(1:pointsb-1))

    ! Re-set coordinates of foil to match from interpolated points
                      
    call rebuild_airfoil(seed_foil%xt, seed_foil%xb, zttmp, zbtmp, foil_to_match)
  
  end if 

end subroutine matchfoils_preprocessing

end module 
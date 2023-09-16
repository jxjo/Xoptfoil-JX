! MIT License
! Copyright (c) 2023 jxjo

!
! Functions for preparing the airfoil prior to optimization 
!

module airfoil_preparation
  use os_util

  implicit none
  private

  public :: preset_airfoil_te_gap 
  public :: preset_airfoil_to_targets
  public :: transform_to_bezier_based
    
contains


!-----------------------------------------------------------------------------


subroutine preset_airfoil_te_gap (show_detail, foil, new_te_gap) 

  !! Set airfoil trailing edge gap to new gap value in % of c
  !!   A standard blending value x/c = 0.8 will be used 

  use vardef,             only: airfoil_type
  use xfoil_driver,       only: xfoil_set_te_gap, get_te_gap

  logical, intent (in)                :: show_detail
  type (airfoil_type), intent (inout) :: foil
  double precision, intent(in)        :: new_te_gap

  type (airfoil_type) :: new_foil
  integer             :: i, nptt, nptb

  doubleprecision, parameter  :: X_BLEND = 0.8d0

  ! Is there a trailing edge te gap? There should be ...

  if ((get_te_gap (foil) == 0d0) .and. (new_te_gap == -1d0) .and. show_detail) then 
    write(*,*)    
    call print_note ("The seed airfoil has no trailing edge gap." //&
                      " Set a gap of at least 0.03% "//&
                      "to improve xfoil viscous results.")
  end if 

  ! Should te gap changed? If no, return ... 
  if ((new_te_gap == -1d0) .or. ((new_te_gap / 100) == get_te_gap (foil))) return 

  ! Set te gap with xfoils TGAP

  if(show_detail) &
    call print_note_only ('- Setting trailing edge gap to '// strf('(F4.2)', new_te_gap)//'%')

  call xfoil_set_te_gap (foil , (new_te_gap / 100), X_BLEND, new_foil)

  ! Now rebuild foil out of new coordinates  ----------------------

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
  
end subroutine preset_airfoil_te_gap


!-----------------------------------------------------------------------------


subroutine preset_airfoil_to_targets (show_detail, foil) 

  !! Set airfoil thickness and camber according to defined geo targets 
  !!   and/or thickness/camber constraints (in airfoil evaluation commons)

  use vardef,             only: airfoil_type
  use xfoil_driver,       only: xfoil_set_thickness_camber, xfoil_set_airfoil
  use xfoil_driver,       only: xfoil_get_geometry_info
  use airfoil_evaluation, only: preset_seed_airfoil, geo_targets
  use airfoil_evaluation, only: max_thickness, min_thickness, max_camber, min_camber

  logical, intent (in)           :: show_detail
  type (airfoil_type), intent (inout)  :: foil

  type (airfoil_type) :: new_foil
  doubleprecision     :: maxt, xmaxt, maxc, xmaxc, new_camber, new_thick
  character (10)      :: cvalue
  integer             :: i, nptt, nptb, ngeo_targets
  logical             :: foil_changed 

  ! Is presetting activated? 

  if (.not. preset_seed_airfoil) return 

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
            call print_note_only ('- Scaling thickness to target value '// trim(adjustl(cvalue))//'%')
          end if

        case ('Camber')                      

          new_camber = geo_targets(i)%target_value
          foil_changed = .true.

          if (show_detail) then
            write (cvalue,'(F6.2)')  (new_camber * 100)
            call print_note_only ('- Scaling camber to target value '// trim(adjustl(cvalue))//'%')
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
        call print_note_only ('- Scaling thickness according constraint to '// trim(adjustl(cvalue))//'%')
      end if 

    elseif (maxt < min_thickness) then 

      new_thick = min_thickness *1.05d0
      call xfoil_set_thickness_camber (foil, new_thick, 0d0, 0d0, 0d0, new_foil)
      foil_changed = .true.

      if (show_detail) then
        write (cvalue,'(F6.2)')  (new_thick * 100)
        call print_note_only ('- Scaling thickness according constraint to '// trim(adjustl(cvalue))//'%')
      end if 

    end if 

    if (maxc > max_camber) then

      new_camber = max_camber *0.95d0
      call xfoil_set_thickness_camber (foil, 0d0, new_camber, 0d0, 0d0, new_foil)
      foil_changed = .true.

      if (show_detail) then
        write (cvalue,'(F6.2)')  (new_camber * 100)
        call print_note_only ('- Scaling camber according constraint to '// trim(adjustl(cvalue))//'%')
      end if
      
    elseif (maxc < min_camber) then 

      new_camber = min_camber *1.05d0
      call xfoil_set_thickness_camber (foil, 0d0, new_camber, 0d0, 0d0, new_foil)
      foil_changed = .true.

      if (show_detail) then
        write (cvalue,'(F6.2)')  (new_camber * 100)
        call print_note_only ('- Scaling camber according constraint to '// trim(adjustl(cvalue))//'%')
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
  use airfoil_operations,   only : is_normalized_coord, split_foil_at_00
  use airfoil_shape_bezier, only : bezier_eval_airfoil
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

  ! Simplex optimization (nelder mead) for both sides  

    call match_bezier_for_side  ('Top', foil%xt, foil%zt, bez_spec%ncpoints_top, bez_spec%px_top, bez_spec%py_top)
    call match_bezier_for_side  ('Bot', foil%xb, foil%zb, bez_spec%ncpoints_bot, bez_spec%px_bot, bez_spec%py_bot)

  ! build airfoil out of Bezier curves 

  call bezier_eval_airfoil (bez_spec, npan, foil%x, foil%z)

  foil%npoint = size(foil%x)

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
  ds_options%write_designs = .false.

  ! --- setup targets in module airfoil_evaluation for objective function 
  
  side_to_match_x = match_x 
  side_to_match_y = match_y 

  ! --- initial estimate for control points based on 'side to match'   
  call get_initial_bezier (side_to_match_x, side_to_match_y, np, px, py)

  ! --- start vector of design variables dv0 
  call bezier_to_dv (px, py, dv0)

  ndv = size(dv0)
  write (*,'(" - ",A,A,A,I2,A,I2,A)') 'Bezier nelder mead optimization for ',side,' side (', &
                                       np, ' points,', ndv, ' variables)'

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
  write (*,'("   ",A,I4,A,A,f8.6,A,f8.6,A,f6.4)') '... ', steps, ' iterations', & 
                                                  ' - norm2 deviation: ', dev_norm2, &
                                                  ', max deviation: ', dev_max,' at x/c: ', dev_max_at

end subroutine match_bezier_for_side


end module 
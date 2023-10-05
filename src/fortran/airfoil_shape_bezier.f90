! MIT License
! Copyright (c) 2023 jxjo

module airfoil_shape_bezier
!
! Functions for Bezier based airfoil generation, modification, ...
!
  implicit none
  private
 
! Bezier general 

  public :: bezier_eval 
  public :: bezier_eval_airfoil
  interface bezier_eval_1D
     module procedure bezier_eval_1D_array        ! eval array
     module procedure bezier_eval_1D_scalar       ! eval scalar
  end interface 

  public :: bezier_spec_type
  
! Bezier special functions  

  public :: bezier_eval_y_on_x, dv_to_bezier
  public :: bezier_to_dv, bezier_spec_to_dv, get_initial_bezier
  public :: ndv_to_ncp, ncp_to_ndv

! file function 

  public :: is_bezier_file, read_bezier_file, load_bezier_airfoil, write_bezier_file

! Bezier spec

  type bezier_spec_type  
    integer          :: ncpoints_top = 0           ! no of control points top side 
    integer          :: ncpoints_bot = 0           ! no of control points bottom side 
    double precision, allocatable :: px_top(:), py_top(:), px_bot(:), py_bot(:) ! control point coordinates 
  end type bezier_spec_type


! testing 

  public :: test_bezier  
  

contains

  subroutine bezier_eval (px, py, u, x, y, der)
    !! Bezier main function - evaluates u with control point coordinates px and py
    !!
    !!    px, py:  x or y coordinates of the bezier control points
    !!    u:    an array of bezier parameters 0..1 at which to return bezier value
    !!    der:  optional derivative - either 0,1 or 2 
    !!    x,y:  returns x,y coordinates at u
    double precision, intent(in)  :: px(:), py(:), u(:) 
    double precision, allocatable,intent(out) :: x(:), y(:) 
    integer, intent(in), optional :: der 

    x =  bezier_eval_1D_array (px, u, der)   
    y =  bezier_eval_1D_array (py, u, der )

  end subroutine 



  subroutine bezier_eval_airfoil (bezier_spec, npoint, x, y)
    !! evaluates airfoil coordinates x,y with control point coordinates px and py for top and bot 
    !
    !    bezier_spec:  coordinates of the bezier control points for top and bot side 
    !    npoint:       number of coordinates of airfoil x,y 
    !    x,y:          returns x,y coordinates at u
    type (bezier_spec_type), intent(in)       :: bezier_spec
    integer, intent(in)                       :: npoint 
    double precision, allocatable,intent(out) :: x(:), y(:) 

    double precision, allocatable :: x_top(:), y_top(:), x_bot(:), y_bot(:), u(:)
    integer                       :: npoint_top, npoint_bot, i

    npoint_bot = (npoint + 1) / 2
    npoint_top = (npoint + 1) - npoint_bot 

    ! generate top side 
    
    u = u_distribution_bezier (npoint_top)
    call bezier_eval (bezier_spec%px_top, bezier_spec%py_top, u, x_top, y_top) 
    
    ! generate bot side 
    
    u = u_distribution_bezier (npoint_bot)
    call bezier_eval (bezier_spec%px_bot, bezier_spec%py_bot, u, x_bot, y_bot) 

    ! build x,y from top and bot coordinates 

    allocate (x (npoint))
    allocate (y (npoint))
  
    do i = 1, npoint_top
      x(i) = x_top (npoint_top - i + 1)
      y(i) = y_top (npoint_top - i + 1)
    end do
    do i = 1, npoint_bot-1
      x (i + npoint_top) = x_bot (i + 1)
      y (i + npoint_top) = y_bot (i + 1)
    end do

  end subroutine 



  function bezier_eval_1D_scalar (px, u, der ) 
    !! Bezier core function - evaluates u with control point coordinates x or y
    !!
    !!    pxy:  either x or y coordinates of the bezier control points
    !!    u:    scalar  0..1 at which to return bezier value
    !!    der:  optional derivative - either 0,1 or 2 
    double precision, intent(in)    :: px(:)
    double precision, intent(in)    :: u 
    integer, intent(in), optional   :: der

    double precision                :: bezier_eval_1D_scalar
    double precision                :: uarray(1), bezier(1) 

    uarray = u
    bezier = bezier_eval_1D_array (px, uarray, der )

    bezier_eval_1D_scalar = bezier(1)

  end function 



  function bezier_eval_1D_array (px, u, der ) 
    !! Bezier core function - evaluates u with control point coordinates x or y
    !!
    !!    pxy:  either x or y coordinates of the bezier control points
    !!    u:    an array of normed arc length 0..1 at which to return bezier value
    !!    der:  optional derivative - either 0,1 or 2 
    double precision, intent(in)  :: px(:), u (:)
    integer, intent(in), optional :: der 

    double precision              :: bezier(size(u)), bezier_eval_1D_array (size(u))
    double precision, allocatable :: weights(:)
    integer     :: i, n, derivative

    bezier = 0d0

    if (.not. present(der)) then 
      derivative = 0 
    else 
      derivative = der 
    end if 

    ! # http://math.aalto.fi/~ahniemi/hss2012/Notes06.pdf 

    n = size(px) - 1                               ! n - degree of Bezier 
    weights = px
    bezier  = 0d0

    ! adjust point weights for derivatives 

    if (derivative > 0) then 
      weights = diff_1D (weights) * n               ! new weight = difference * n
      n = n - 1                                     ! lower 1 degree
    end if 
    if (derivative > 1) then 
      weights = diff_1D (weights) * n 
      n = n - 1
    end if 

    ! evaluate bernstein polynomials 
          
    do i = 1, size(weights)
      bezier = bezier + basisFunction (n, i, u) * weights(i)
    end do 

    bezier_eval_1D_array = bezier 

  end function 

 
  function bezier_eval_y_on_x (px, py, x, epsilon) 
    !! Evaluate the y value based on x 
    !!
    !!    px, py:  x or y coordinates of the bezier control points
    !!    x:    x value to evaluate y 
    !!    epsilon:  precision of y value - default 10d-10 
    !
    ! An interpolation is made to find u(x) - either linear (fast=True) or based on the curve

    double precision, dimension (:), intent(in)   :: px, py
    double precision, intent(in)            :: x
    double precision, intent(in), optional  :: epsilon
    double precision    :: bezier_eval_y_on_x

    double precision    :: eps, u0, xn, dxn, u 
    integer   :: i 

    ! optimize first / last x
    if (x == px(1)) then
      bezier_eval_y_on_x = py(1)
      return
    else if (x == px(size(px))) then 
      bezier_eval_y_on_x = py(size(py))
      return
    end if 

    if (.not. present(epsilon)) then 
      eps = 10d-10
    else 
      eps = epsilon 
    end if 

    ! define a good start value for newton iteration 
    if (x < 0.05) then                      
      u0 = 0.05
    else if (x > 0.95) then
      u0 = 0.95
    else 
      u0 = x
    end if  

    ! newton iteration to get bezier u value from x
    u = u0
    do i = 1, 50

      if (u > 1d0) u = 1d0                        ! ensure to stay within boundaries 
      if (u < 0d0) u = 1d-10 !0d0 

      xn  = bezier_eval_1D (px, u) - x            ! find root f(u) - x
      if ((abs(xn) < eps)) exit                   ! succeeded
      dxn = bezier_eval_1D (px, u, 1) 

      if (dxn == 0d0) then 
        if (xn /= 0d0) write (*,*) "Error: Bezier newton iteration with zero derivative"
        exit
      end if  

      u = u - xn / dxn 
    end do 
    ! finally get y from iterated u-value 
    bezier_eval_y_on_x = bezier_eval_1D (py, u)

    return 

  end function 


  ! ------------- file functions ----------------------------


  function is_bezier_file (filename)
    !! .true. if filename has ending '.bez'
    character(*),  intent(in) :: filename
    logical       :: is_bezier_file 
    character (4) :: extension 
    integer       :: length
    
    is_bezier_file = .false.
    length = len (trim(filename))
    if (length > 4) then
      extension = '.bez' !filename (length-3, length)
      if (extension == '.bez' .or. extension =='.BEZ') then
        is_bezier_file = .true.
      end if 
    end if  
  end function  


  subroutine load_bezier_airfoil (filename, npoint, name, x, y, bez_spec)
    !! read airfoil bezier file and eval x, y coordinates of airfoil with npoint  
    character(*),  intent(in) :: filename
    integer, intent(in)  :: npoint
    character (len=:), allocatable, intent(out) :: name 
    double precision, allocatable, intent(out)  :: x(:), y(:) 
    type(bezier_spec_type), intent(out)         :: bez_spec

    ! read control points top and bot side 
    
    call read_bezier_file (filename, 'Top', name, bez_spec%ncpoints_top, bez_spec%px_top, bez_spec%py_top )
    call read_bezier_file (filename, 'Bot', name, bez_spec%ncpoints_bot, bez_spec%px_bot, bez_spec%py_bot )
    
    ! build airfoil x,y from top and bot 

    call bezier_eval_airfoil (bez_spec, npoint, x, y)

  end subroutine


  
  subroutine read_bezier_file (filename, side, name, ncpoint, px, py)
    !! read a bezier definition from file - returns control points for top and bot 
    !
    ! # 'airfoil name'
    ! # Top Start
    ! # 0.0000000000000000 0.0000000000000000
    ! # ...
    ! # 1.0000000000000000 0.0000000000000000
    ! # Top End
    ! # Bottom Start
    ! # ...
    ! # Bottom End

    character(*),  intent(in)                   :: filename
    character (3), intent(in)                   :: side
    character (:), allocatable, intent(out)     :: name
    integer, intent(out)                        :: ncpoint
    double precision, allocatable, intent(out)  :: px(:), py(:) 
  
    double precision, dimension (100) :: px_tmp, py_tmp 
    integer :: iunit, ioerr, np, i
    logical :: do_read 
    character (255) :: in_buffer 
    character(:), allocatable :: in_line
  
    ! Open bezier definition file
  
    iunit = 12
    open(unit=iunit, file=filename, status='old', position='rewind', iostat=ioerr)
    if (ioerr /= 0) then
      write (*,*) 'Cannot find bezier definition file '//trim(filename)
      stop 1
    end if
   
    ! Read first line; determine if it is a title or not
  
    do_read = .false. 
    np = 0
    name = ''

    do i = 1, size(px_tmp) 
      read(iunit, '(A)',iostat=ioerr) in_buffer
      in_line = trim(in_buffer) 
      if (i == 1) then 
        name = in_line
      else if (in_line == 'Top Start'    .and. side == 'Top') then
        do_read = .true. 
      else if (in_line == 'Bottom Start' .and. side == 'Bot') then
        do_read = .true.
      else if (do_read .and. (in_line == 'Top End' .or. in_line == 'Bottom End')) then 
        exit  
      else if  (in_line == 'Bottom Start' .or. in_line == 'Bottom Start') then
        ! skip
      else if (do_read) then 
        np = np + 1
        read (in_line,*) px_tmp(np), py_tmp(np) 
      else 
        ! skip
      end if
    end do
  
    if (np> 0) then 
      px = px_tmp(1:np)
      py = py_tmp(1:np)
    else
      allocate( px( 0 ) ) 
      allocate( py( 0 ) )  
      write (*,*) 'Cannot read bezier definition file ' // trim(filename) // ' - Syntax error'
      stop 1
    end if 
  
    close(iunit)

    ncpoint = size(px)
    return 
  end subroutine


  subroutine write_bezier_file (filename, name, bez_spec)
    !! write a bezier definition of an airfoil to file
    !
    ! # 'airfoil name'
    ! # Top Start
    ! # 0.0000000000000000 0.0000000000000000
    ! # ...
    ! # 1.0000000000000000 0.0000000000000000
    ! # Top End
    ! # Bottom Start
    ! # ...trim(outname)//'.bez'
    ! # Bottom End

    character(*),  intent(in)               :: filename, name
    type(bezier_spec_type), intent(in)      :: bez_spec

    integer :: iunit, i

    iunit = 13
    open  (unit=iunit, file=filename, status='replace')

    write (iunit, '(A)') trim(name)
    write (iunit, '(A)') "Top Start"
    do i = 1, bez_spec%ncpoints_top
      write (iunit, '(2F14.10)') bez_spec%px_top(i), bez_spec%py_top(i)
    end do 
    write (iunit, '(A)') "Top End"

    write (iunit, '(A)') "Bottom Start"
    do i = 1, bez_spec%ncpoints_bot
      write (iunit, '(2F14.10)') bez_spec%px_bot(i), bez_spec%py_bot(i)
    end do 
    write (iunit, '(A)') "Bottom End"

    close (iunit)

  end subroutine


  ! ------------- design variables and bezier---------------------------

  
  function ndv_to_ncp (ndv)
    !! get number of control points from number of design variables 
    integer, intent(in) :: ndv
    integer             :: ndv_to_ncp
    ndv_to_ncp = 1 + 1 + (ndv - 1) / 2 + 1    ! add LE, TE and x of LE tangent 
  end function

  function ncp_to_ndv (ncp)
    !! get number of design variables from number of control points  
    integer, intent(in) :: ncp
    integer             :: ncp_to_ndv
    ncp_to_ndv = (ncp - 3) * 2  + 1           ! subtract LE, TE and x of LE tangent 
  end function



  subroutine dv_to_bezier (dv, te_gap, px, py)
    !! build bezier control points from design vars with a 'te_gap' 
    !
    !  p1   = 0     , 0
    !  p2   = 0     , dv(1)
    !  p3   = dv(2) , dv(3)
    !  p4   = dv(4) , dv(5)
    !  ...
    !  pn   = 1     , te_gap 
    !
    double precision, intent(in) :: dv(:)
    double precision, intent(in) :: te_gap
    double precision, allocatable, intent(out) :: px(:), py(:) 

    integer :: ndv, np, ip, idv

    ndv = size (dv)
    np  = ndv_to_ncp (ndv)                  ! calc number of control points 

    if (ndv < 3) then 
      write (*,*) 'Bezier: Number of design variables less than 3'
      stop 1
    end if 

    ! init new bezier control points 
    allocate (px (np))
    allocate (py (np))
    px = 0d0
    py = 0d0 

    ! map design vars to control point coordinates 
    py(2) = dv(1) 
    do ip = 3, np-1  
      idv = (ip - 2) * 2      
      px(ip) = dv(idv) 
      py(ip) = dv(idv+1) 
    end do  
    px(np) = 1d0
    py(np) = te_gap 
  
  end subroutine



  subroutine bezier_spec_to_dv (bez_spec, dv, dv_min, dv_max)
    !! get complete design vars from airfoil bezier_spec 
    !!     including min, max values for design vars  
    !
    !  p1   = 0     , 0
    !  p2   = 0     , dv(1)
    !  p3   = dv(2) , dv(3)
    !  p4   = dv(4) , dv(5)
    !  ...
    !  pn   = 1     , te_gap 
    !
    type(bezier_spec_type), intent(in)         :: bez_spec
    double precision, allocatable, intent(out) :: dv(:), dv_min(:), dv_max(:)

    double precision, allocatable :: dv_top(:), dv_min_top(:), dv_max_top(:)
    double precision, allocatable :: dv_bot(:), dv_min_bot(:), dv_max_bot(:)
    integer :: ncp
    call bezier_to_dv (bez_spec%px_top, bez_spec%py_top, dv_top)
    call bezier_to_dv (bez_spec%px_bot, bez_spec%py_bot, dv_bot)

    ncp = size(bez_spec%px_top)
    call bezier_to_dv_bounds (.true.,  ncp, dv_min_top, dv_max_top)
    ncp = size(bez_spec%px_bot)
    call bezier_to_dv_bounds (.false., ncp, dv_min_bot, dv_max_bot)

    dv     = [dv_top, dv_bot] 
    dv_min = [dv_min_top, dv_min_bot] 
    dv_max = [dv_max_top, dv_max_bot] 

  end subroutine bezier_spec_to_dv



  subroutine bezier_to_dv (px, py, dv)
    !! get design vars from bezier control points  
    !
    !  p1   = 0     , 0
    !  p2   = 0     , dv(1)
    !  p3   = dv(2) , dv(3)
    !  p4   = dv(4) , dv(5)
    !  ...
    !  pn   = 1     , te_gap 
    !
    double precision, allocatable, intent(out) :: dv(:)
    double precision, intent(in) :: px(:), py(:) 

    integer :: ndv, np, ip, idv

    np  = size (px)
    ndv = ncp_to_ndv (np) 

    if (ndv < 3) then 
      write (*,*) 'Bezier: Number of design variables less than 3.'
      stop 1
    end if 

    allocate (dv (ndv))
    dv = 0d0

    ! map control point coordinates to design vars

    ! start tangent  - only y 
    dv(1) = py(2)                                   

    ! normal control points 3..n-1 take x + y 
    idv = 2
    do ip = 3, np-1                                 
      dv(idv)     = px(ip) 

      idv = idv + 1 
      dv(idv)     = py(ip) 
      idv = idv + 1 
    end do  
  
  end subroutine


  subroutine bezier_to_dv_bounds (isTop, ncp, dv_min, dv_max)
    !! get the bounds of design vars from bezier control points  
    !  - see bezier_to_dv

    logical, intent(in) :: isTop
    integer, intent(in) :: ncp
    double precision, allocatable, intent(out) :: dv_min(:), dv_max(:)
    integer :: ndv, ip, idv

    ndv = ncp_to_ndv (ncp) 

    ! init new bezier control points 
    allocate (dv_min (ndv))
    dv_min = 0d0
    dv_max = dv_min 

    ! map control point coordinates to design vars

    ! start tangent  - only y 
    if (isTop) then 
      dv_min(1) = 0.01d0                           ! LE not too sharp 
      dv_max(1) = 0.08d0
    else
      dv_min(1) = -0.08d0                           
      dv_max(1) = -0.01d0
    end if 

    ! normal control points 3..n-1 take x + y 
    idv = 2
    do ip = 3, ncp-1                                 
      dv_min(idv) = 0.03d0                          ! x not too close to LE
      dv_max(idv) = 0.97d0                          ! x not too close to TE

      idv = idv + 1 
      if (isTop) then 
        dv_min(idv) = -0.05d0                       ! rough bounds 
        dv_max(idv) =  0.20d0                           
      else
        dv_min(idv) = -0.20d0                          
        dv_max(idv) =  0.05d0                           
      end if 
      idv = idv + 1 
    end do  
  
  end subroutine


  subroutine get_initial_bezier (x, y, np, px, py)
    !! get initial bezier control points x, y of an airfoil side 
    !!    x, y:  coordinates of an airfoil side
    !!    np:    number control points bezier should have 
    !!    px, py:  coordinates of bezier control points  

    use math_deps,          only : interp_vector

    double precision, intent(in) :: x(:), y(:)
    integer, intent(in) :: np
    double precision, allocatable, intent(out) :: px(:), py(:) 

    integer :: i, ip, ncoord
    double precision :: px2_dummy (1), py2 (1)
    double precision, allocatable :: px_coord (:), py_coord (:)

    if (np < 3) then 
      write (*,*) 'Bezier: Number of control points less than 3'
      stop 1
    end if 

    ! init new bezier control points 
    allocate (px (np))
    allocate (py (np))
    px = 0d0
    py = 0d0 
    ncoord = size(x)

    ! fix control points for le and te
    px(1)  = 0d0                                    ! le
    py(1)  = 0d0                                   
    px(np) = 1d0                                    ! te 
    py(np) = y(ncoord)                              ! set te gap       
    
    ! start tangent (point 2)  
    px(2) = 0d0                                     ! le tangent 
    px2_dummy = 0.1d0                               ! ... will retrieve y value at x=0.1
    call interp_vector(x, y, px2_dummy, py2)
    py(2) = py2(1)                                  ! le tangent 

    ! equally spaced control points in between le and te, py interpolated from y
    px_coord = linspace (0d0, 1d0, int(np-1))
    allocate (py_coord(size(px_coord)))
    call interp_vector(x, y, px_coord, py_coord)

    ip = 3                                          ! from 3rd point to np-1
    do i = 2, size(px_coord) - 1
      px(ip) = px_coord (i) 
      py(ip) = py_coord (i) 
      ip = ip + 1
    end do 
  
  end subroutine


  ! ------------- helper functions ----------------------------


  function Ni (n, i ) 
    !! Binomial Coefficients
    !   n:    number of points 
    !   i:    index 0..n-1  (fortran) 
    integer, intent(in)     :: n, i
    double precision  :: Ni
    ! Gamma(n+1) == fact(n) 
    Ni = gamma (real(n+1)) / (gamma (real(i+1)) * gamma(real(n-i+1)))   ! N = n! / (i! * (n-i)!)

  end function 

  function basisFunction(n, i1, u) 
    !! bernstein basis polynomial   
    !   def basisFunction (n, i, u):
    !       J = np.array (Ni(n, i) * (u ** i) * (1 - u) ** (n - i))
    !       return J 
    integer, intent(in)     :: n, i1 
    double precision, dimension(:), intent(in) :: u 
    double precision, dimension(size(u)) :: basisFunction 
    integer           :: i 

    i = i1 - 1                                    ! i = 0.. (n-1) 
    basisFunction = Ni (n,i) * ( u**i) * (1-u) ** (n-i)

  end function 


  function diff_1D (x) 
    !! difference of 1d array elements
    double precision, dimension(:), intent(in)  :: x
    double precision, dimension(size(x)-1)      :: diff_1D 
    integer :: i, n 

    n = size(x) 
    if (n > 1) then 
      do i = 1, n-1
        diff_1D (i) = x(i+1) - x(i)
      end do 
    end if 
  end function 


  function cosinus_distribution (nPoints)
    !! returns a special cosinus distibuted array of u between 0..1
    !
    !   Bezier needs a special u cosinus distribution as the ponts are bunched
    integer, intent(in)     :: nPoints 
    double precision, dimension(nPoints) :: cosinus_distribution

    double precision, dimension(nPoints) :: beta, u 
    double precision      :: pi, umin, umax

    pi = acos(-1.d0)

    ! special cosinus distribution with strong bunch at start and light bunch at end  
    !beta = linspace (0.15d0, 0.9d0, nPoints) * pi 
    beta = linspace (0.25d0, 0.7d0, nPoints) * pi 
    u    = (1d0 - cos(beta)) * 0.5

    ! normalize 
    umin = minval(u)
    umax = maxval(u)
    u = (u - umin) / (umax - umin)

    !ensure 0.0 and 1.0 
    u (1) = 0d0 
    u (nPoints) = 1d0

    cosinus_distribution = u

  end function 



  function u_distribution_bezier (nPoints)
    !! a special distribution for Bezier curve to achieve a similar bunching to splined airfoils
    integer, intent(in)     :: nPoints 

    double precision, dimension(nPoints)    :: u, u_distribution_bezier
    double precision, dimension(nPoints-1)  :: du
    double precision      :: te_du_end, te_du_growth, le_du_start, le_du_growth, du_ip
    integer               :: ip, nPanels

    ! for a constant du the resulting arc length of a curve section (panel) is proportional the 
    ! reverse of the curvature, so it fits naturally the need of airfoil paneling especially
    ! at LE. For LE and TE a little extra bunching is done ...

    te_du_end = 0.5d0                           ! size of last du compared to linear du
    te_du_growth = 1.4                          ! how fast panel size will grow 

    le_du_start = 0.70                          ! size of first du compared to linear du
    le_du_growth = 1.05                         ! how fast panel size will grow 

    nPanels = nPoints - 1
    u  = 0d0
    du = 1d0

    ! start from LE backward - increasing du 
    du_ip = le_du_start 
    ip = 1
    do while (du_ip < 1.0)
      du(ip) = du_ip
      ip = ip + 1
      du_ip = du_ip * le_du_growth
    end do

    ! run from TE forward - increasing du 
    du_ip = te_du_end
    ip = size(du) 
    do while (du_ip < 1.0)
      du(ip) = du_ip
      ip = ip - 1
      du_ip = du_ip * te_du_growth
    end do

    ! build u array and normalized to 0..1
    do ip = 1, nPanels
      u(ip+1) = u(ip) + du(ip) 
    end do 

    u_distribution_bezier = u / u(nPoints)  


  end function u_distribution_bezier



  function linspace (start, end, nPoints)
    !! returns a array of nPoints equally distanced starting from start to end
    !     - similar to numpy linspace
    double precision, intent(in)  :: start, end
    integer, intent(in)           :: nPoints

    double precision, dimension(nPoints) :: linspace
    double precision  :: x, delta
    integer           :: i 
    
    linspace = 0d0 
    if (start < end) then 
      delta = (end - start) / (nPoints - 1) 
      x = start
      do i = 1, npoints 
        linspace (i) = x
        x = x + delta 
      end do 
    end if 

  end function 

  

  ! ------------- bezier test routines ----------------------------

  subroutine test_bezier ()
    !! test of bezier implementation comparing to python bezier results 
    double precision, dimension(:), allocatable :: x,y
    double precision, dimension(:), allocatable :: px, py, u   
    integer :: i, n, j
    double precision :: checksum 

    integer         :: itime_start, itime_finish, rate
    doubleprecision :: time_diff


    px = [   0.0,  0.0, 0.3,   1.0]
    py = [   0.0, 0.06, 0.12,  0.0]

    u = [0.0, 0.25, 0.5, 0.75, 1.0]

    ! test bezier eval 2D 

    call bezier_eval (px, py, u, x, y)

    n = size(x)
    write(*,"('u: ',100f8.4)") ( u(i), i=1,n )
    write(*,"('x: ',100f8.4)") ( x(i), i=1,n )
    write(*,"('y: ',100f8.4)") ( y(i), i=1,n )

    checksum = sum(x) + sum(y) 
    checksum = real(int(checksum * 1d6)) / 1d6

    if (checksum == 2.0125d0) then                    ! ... from python Bezier 
      write(*,"(' --> passed')") 
    else 
      write(*,"('checksum :', f10.7, ' !failed!')") checksum
    end if 

    ! test bezier eval 2D - 1st derivative

    write (*,*) 
    call bezier_eval (px, py, u, x, y, 1)

    n = size(x)
    write(*,"('dx: ',100f8.4)") ( x(i), i=1,n )
    write(*,"('dy: ',100f8.4)") ( y(i), i=1,n )

    checksum = sum(x) + sum(y) 
    checksum = real(int(checksum * 1d7)) / 1d7

    if (checksum == 4.950000d0) then                    ! ... from python Bezier 
      write(*,"(' --> passed')") 
    else 
      write(*,"('checksum :', f10.7, ' !failed!')") checksum
    end if 

    ! test cosinus distribution 

    write (*,*) 
    u = u_distribution_bezier (10)
    n = size(u) 
    write(*,"('u distribution_bezier: ',100f8.4)") ( u(i), i=1,n )
    checksum = sum(u) 
    checksum = real(int(checksum * 1d6)) / 1d6
    if (checksum == 5.152202d0) then                    ! ... from python Bezier 
      write(*,"(' --> passed')") 
    else 
      write(*,"('checksum :', f10.7, ' !failed!')") checksum
    end if 

    ! eval y distribution 

    write (*,*) 
    x = linspace (0d0, 1d0, 10)
    y = x
    do i = 1, size(x) 
      y(i) = bezier_eval_y_on_x (px, py, x(i))
    end do 
    n = size(u) 
    write(*,"('x distribution: ',100f8.4)") ( x(i), i=1,n )
    write(*,"('y evaluated:    ',100f8.4)") ( y(i), i=1,n )
    checksum = sum(x) + sum(y) 
    checksum = real(int(checksum * 1d6)) / 1d6
    if (checksum == 5.414869d0) then                    ! ... from python Bezier 
      write(*,"(' --> passed')") 
    else 
      write(*,"('checksum :', f10.7, ' !failed!')") checksum
    end if 
  

    ! time check 

    ! integer         :: itime_start, itime_finish, rate
    ! doubleprecision :: time_diff

    call system_clock(count_rate=rate)
    call system_clock(itime_start)
    ! ............
    x = linspace (0d0, 1d0, 100)
    y = x
    do j = 1,10
      do i = 1, size(x) 
        y(i) = bezier_eval_y_on_x (px, py, x(i))
      end do 
    end do 

    ! ......
    call system_clock(itime_finish)
    time_diff = real (itime_finish-itime_start)/real(rate)
    write (*, '("Time = ",f8.5," seconds" )') time_diff

  end subroutine



end module
  
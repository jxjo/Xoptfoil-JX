!------------------------------------------------------------------------------------------
!
!  OS dependant utility functions like colored console output
!
!------------------------------------------------------------------------------------------


module os_util

#ifdef UNIX
#else
  use ISO_C_BINDING
#endif    

  implicit none

  integer, parameter, public  :: COLOR_GOOD    = 1
  integer, parameter, public  :: COLOR_BAD     = 2
  integer, parameter, public  :: COLOR_NORMAL  = 3
  integer, parameter, public  :: COLOR_HIGH    = 4
  integer, parameter, public  :: COLOR_ERROR   = 5
  integer, parameter, public  :: COLOR_WARNING = 6
  integer, parameter, public  :: COLOR_NOTE    = 7
  integer, parameter, public  :: COLOR_FEATURE = 8
  integer, parameter, public  :: COLOR_PALE    = 9
  integer, parameter, public  :: COLOR_FIXED   = 10

  integer, parameter, public  :: Q_GOOD     = 0         ! must be ordered
  integer, parameter, public  :: Q_OK       = 1
  integer, parameter, public  :: Q_BAD      = 2
  integer, parameter, public  :: Q_PROBLEM  = 4
  integer, parameter, public  :: Q_NEW      = 8

  private

  public :: stri, strf
  public :: print_colored
  public :: make_directory
  public :: my_stop
  public :: print_error
  public :: print_warning
  public :: print_note
  public :: print_text
  public :: print_colored_i
  public :: print_colored_r
  public :: print_colored_s
  public :: print_colored_rating
  public :: i_quality
  public :: r_quality
  
  public :: set_number_of_threads

  public :: delete_file

  interface print_colored
#ifdef UNIX
  module procedure print_colored
#else
  module procedure print_colored_windows
#endif    
  end interface

  interface make_directory
#ifdef UNIX
 module procedure make_directory_unix
#else
  module procedure make_directory_windows
#endif    
  end interface

!------------------------------------------------------------------------------------------
!  unix  specific 
!------------------------------------------------------------------------------------------
#ifdef UNIX

  character (4)  :: FOREGROUND_YELLOW      = '[33m'
  character (4)  :: FOREGROUND_MAGENTA_UX  = '[35m'
  character (4)  :: FOREGROUND_CYAN        = '[36m'
  character (4)  :: FOREGROUND_GRAY        = '[37m'
  character (4)  :: FOREGROUND_LIGHT_GREEN = '[92m'
  character (4)  :: FOREGROUND_LIGHT_RED   = '[91m'
  character (4)  :: FOREGROUND_LIGHT_BLUE  = '[94m'
  character (4)  :: FOREGROUND_BOLD        = '[1m'
  character (4)  :: FOREGROUND_DEFAULT     = '[39m'
  
!------------------------------------------------------------------------------------------
!  windows specific 
!------------------------------------------------------------------------------------------
#else
  integer, parameter, public :: BOOL = C_INT
  integer, parameter, public :: HANDLE = C_INTPTR_T
  integer, parameter, public :: ULONG = C_LONG
  integer, parameter, public :: SHORT = C_SHORT
  integer, parameter, public :: WORD = C_SHORT
  integer, parameter, public :: DWORD = C_LONG
  integer(DWORD), parameter, public :: STD_OUTPUT_HANDLE = -11
  integer(WORD), parameter, public :: FOREGROUND_BLUE = int(Z"1",WORD)
  integer(WORD), parameter, public :: FOREGROUND_GREEN = int(Z"2",WORD)
  integer(WORD), parameter, public :: FOREGROUND_RED = int(Z"4",WORD)
  integer(WORD), parameter, public :: FOREGROUND_MAGENTA= int(Z"5",WORD)
  integer(WORD), parameter, public :: FOREGROUND_INTENSITY = int(Z"8",WORD)
  integer(WORD), parameter, public :: BACKGROUND_BLUE = int(Z"10",WORD)
  integer(WORD), parameter, public :: BACKGROUND_GREEN = int(Z"20",WORD)
  integer(WORD), parameter, public :: BACKGROUND_RED = int(Z"40",WORD)
  integer(WORD), parameter, public :: BACKGROUND_INTENSITY = int(Z"80",WORD)
  integer(WORD), parameter, public :: COMMON_LVB_LEADING_BYTE = int(Z"100",WORD)
  integer(WORD), parameter, public :: COMMON_LVB_TRAILING_BYTE = int(Z"200",WORD)
  integer(WORD), parameter, public :: COMMON_LVB_GRID_HORIZONTAL = int(Z"400",WORD)
  integer(WORD), parameter, public :: COMMON_LVB_GRID_LVERTICAL = int(Z"800",WORD)
  integer(WORD), parameter, public :: COMMON_LVB_GRID_RVERTICAL = int(Z"1000",WORD)
  integer(WORD), parameter, public :: COMMON_LVB_REVERSE_VIDEO = int(Z"4000",WORD)

  type, bind(C), public :: T_COORD
    integer(SHORT) x
    integer(SHORT) y
  end type T_COORD
  type, bind(C), public :: T_SMALL_RECT
    integer(SHORT) Left
    integer(SHORT) Top
    integer(SHORT) Right
    integer(SHORT) Bottom
  end type T_SMALL_RECT
  type, bind(C), public :: T_CONSOLE_SCREEN_BUFFER_INFO
    type(T_COORD) dwSize
    type(T_COORD) dwCursorPosition
    integer(WORD) wAttributes
    type(T_SMALL_RECT) srWindow
    type(T_COORD) dwMaximumWindowSize
  end type T_CONSOLE_SCREEN_BUFFER_INFO

  public GetConsoleScreenBufferInfo
  interface
    function GetConsoleScreenBufferInfo(hConsoleOutput, &
        lpConsoleScreenBufferInfo) bind(C,name='GetConsoleScreenBufferInfo')
        import BOOL, HANDLE, T_CONSOLE_SCREEN_BUFFER_INFO
        implicit none
!gcc$ attributes stdcall :: GetConsoleScreenBufferInfo
        integer(BOOL) GetConsoleScreenBufferInfo
        integer(HANDLE), value :: hConsoleOutput
        type(T_CONSOLE_SCREEN_BUFFER_INFO) lpConsoleScreenBufferInfo
    end function GetConsoleScreenBufferInfo
  end interface

!   public GetStdHandle
  interface
    function GetStdHandle(nStdHandle) bind(C,name='GetStdHandle')
        import HANDLE, DWORD
        implicit none
!gcc$ attributes stdcall :: GetStdHandle
        integer(HANDLE) GetStdHandle
        integer(DWORD), value :: nStdHandle
    end function GetStdHandle
  end interface
!   public SetConsoleMode
  interface
    function SetConsoleMode(hConsoleHandle, dwMode) bind(C,name='SetConsoleMode')
        import HANDLE, DWORD, BOOL
        implicit none
!gcc$ attributes stdcall :: SetConsoleMode
        integer(BOOL) SetConsoleMode
        integer(HANDLE), value:: hConsoleHandle
        integer(DWORD), value :: dwMode
    end function SetConsoleMode
  end interface

  public SetConsoleTextAttribute
  interface
    function SetConsoleTextAttribute(hConsoleOutput, wAttributes) &
        bind(C,name='SetConsoleTextAttribute')
        import HANDLE, WORD, BOOL
        implicit none
!gcc$ attributes stdcall :: SetConsoleTextAttribute
        integer(BOOL) SetConsoleTextAttribute
        integer(HANDLE), value :: hConsoleOutput
        integer(WORD), value :: wAttributes
    end function SetConsoleTextAttribute
  end interface
#endif


  contains
  
!------------------------------------------------------------------------------------------
!  Print a string to console into a color defined by wAttributes 
!------------------------------------------------------------------------------------------
#ifdef UNIX

subroutine print_colored (color_typ, text)

  integer, intent (in) :: color_typ
  character(*),  intent (in) :: text
  character (20) :: color_string, normal_string

!$omp critical

  select case (color_typ)
    case (COLOR_GOOD)
      color_string = FOREGROUND_LIGHT_GREEN
    case (COLOR_BAD)
      color_string = FOREGROUND_LIGHT_RED
    case (COLOR_HIGH)
      color_string = FOREGROUND_BOLD
    case (COLOR_ERROR)
      color_string = FOREGROUND_LIGHT_RED
    case (COLOR_WARNING)
      color_string = FOREGROUND_YELLOW
    case (COLOR_NOTE)
      color_string = FOREGROUND_CYAN
    case (COLOR_PALE)
      color_string = FOREGROUND_GRAY
    case (COLOR_FIXED)
      color_string = FOREGROUND_MAGENTA_UX
    case default
      color_string = ''
  end select

  if (len(trim(color_string)) > 0) then 
    color_string  = achar(27) // color_string
    normal_string = achar(27) // FOREGROUND_DEFAULT
  else
    color_string  = ''
    normal_string = '' 
  end if

  write(*,'(A)', advance = 'no') trim(color_string) // text // trim(normal_string)

!$omp end critical

end subroutine print_colored

#else
  
subroutine print_colored_windows (color_typ, text)

  integer, intent (in) :: color_typ
  integer(WORD) :: wAttributes, color_attribute
  character(*),  intent (in) :: text
  
  integer(HANDLE) :: hConsoleOutput
  integer(BOOL) :: iresult
  type(T_CONSOLE_SCREEN_BUFFER_INFO) lpConsoleScreenBufferInfo

!$omp critical

  select case (color_typ)
    case (COLOR_GOOD)
      color_attribute = iany([FOREGROUND_GREEN, FOREGROUND_INTENSITY])
    case (COLOR_BAD)
      color_attribute = iany([FOREGROUND_RED])
    case (COLOR_HIGH)
      color_attribute = iany([FOREGROUND_RED, FOREGROUND_GREEN, FOREGROUND_BLUE, FOREGROUND_INTENSITY])
    case (COLOR_ERROR)
      color_attribute = iany([FOREGROUND_RED, FOREGROUND_INTENSITY])
    case (COLOR_WARNING)
      color_attribute = iany([FOREGROUND_RED, FOREGROUND_GREEN])
    case (COLOR_NOTE)
      color_attribute = iany([FOREGROUND_INTENSITY])
!     grey is better then thi light blue
!      color_attribute = iany([FOREGROUND_BLUE, FOREGROUND_GREEN])
    case (COLOR_PALE)
      color_attribute = iany([FOREGROUND_INTENSITY])
    case (COLOR_FIXED)
      color_attribute = iany([FOREGROUND_MAGENTA])
    case (COLOR_FEATURE)
      color_attribute = iany([FOREGROUND_BLUE, FOREGROUND_INTENSITY])
    case default
      color_attribute = iany([FOREGROUND_RED, FOREGROUND_GREEN, FOREGROUND_BLUE])
  end select

  wAttributes = int(color_attribute, 2) 
  
  hConsoleOutput = GetStdHandle(STD_OUTPUT_HANDLE)
  iresult = GetConsoleScreenBufferInfo(hConsoleOutput,lpConsoleScreenBufferInfo)
  
  iresult = SetConsoleTextAttribute(hConsoleOutput,wAttributes)
  write(*,'(A)', advance = 'no') text
  
  wAttributes = int(iany([FOREGROUND_RED, FOREGROUND_GREEN, FOREGROUND_BLUE]),2)
  ! Switch back to normal color instead of restoring old value 
  !        (problems with multi threaded screen write)
  ! iresult = SetConsoleTextAttribute(hConsoleOutput,lpConsoleScreenBufferInfo%wAttributes)
  iresult = SetConsoleTextAttribute(hConsoleOutput,wAttributes)

!$omp end critical

end subroutine print_colored_windows

#endif

!------------------------------------------------------------------------------------------
!  File functions 
!------------------------------------------------------------------------------------------

#ifdef UNIX

subroutine make_directory_unix (subdirectory, preserve_existing)
  character(*),  intent (in) :: subdirectory
  logical,  intent (in), optional :: preserve_existing
  integer         :: istat
  character (255) :: command

  if (.not. (present(preserve_existing) .and.preserve_existing)) then
    command = 'rmdir --ignore-fail-on-non-empty '//trim(subdirectory)
    istat = system (trim(command))
  end if 

  command = 'mkdir '//trim(subdirectory)
  istat = system (trim(command))

end subroutine make_directory_unix

#else

  subroutine make_directory_windows (subdirectory, preserve_existing)
    character(*),  intent (in) :: subdirectory
    logical,  intent (in), optional :: preserve_existing
    integer         :: istat
    character (255) :: command

    if (.not. (present(preserve_existing) .and.preserve_existing)) then
      command = 'if exist "'//trim(subdirectory)//'" rmdir "'//trim(subdirectory)//'" /s/q'
      istat = system (trim(command))
    end if 

    command = 'if not exist "'//trim(subdirectory)//'" mkdir "'//trim(subdirectory)//'"'
    istat = system (trim(command))

  end subroutine make_directory_windows

#endif


subroutine delete_file (file_path)
  character(*),  intent (in) :: file_path
  integer         :: stat

  open(unit=1234, iostat=stat, file=trim(file_path), status='old')
  if (stat == 0) close(1234, status='delete')  

end subroutine delete_file



!------------------------------------------------------------------------------------------
!  String functions - Integer  and Float to string 
!------------------------------------------------------------------------------------------

  pure function stri (a_int) result (as_string)
    !! integer to string 
    integer,  intent (in) :: a_int
    character (:), allocatable ::as_string

    as_string = repeat('*',10)
    write (as_string, '(I10)') a_int

    as_string = trim(adjustl(as_string))

  end function 

  pure function strf (format, a_float) result (as_string)
    !! real to string using format specifier 
    doubleprecision,  intent (in) :: a_float
    character (*),  intent (in) :: format
    character (:), allocatable ::as_string

    if (trim(format) == '') return

    as_string = repeat('*',20)
    write (as_string, format) a_float

    as_string = trim(adjustl(as_string))

  end function 

!------------------------------------------------------------------------------------------
!  Print colored error, warning, note strings to console
!------------------------------------------------------------------------------------------

subroutine print_error (text, intent)
  character(*),  intent (in) :: text
  integer,  intent (in), optional :: intent
  integer :: i
  i = 1
  if (present (intent)) then 
    if (intent >0 .and. intent <80) i = intent
  end if
  write(*,'(A)', advance = 'no') repeat(' ',i)
  call print_colored (COLOR_ERROR, trim(text))
  write (*,'(A)')
end subroutine print_error

subroutine print_warning (text, intent)
  character(*),  intent (in) :: text
  integer,  intent (in), optional :: intent
  integer :: i
  i = 1
  if (present (intent)) then 
    if (intent >0 .and. intent <80) i = intent
  end if
  write(*,'(A)', advance = 'no') repeat(' ',i)
  call print_colored (COLOR_WARNING, 'Warning: ')
  write (*,'(A)') trim(text)
end subroutine print_warning

subroutine print_note (text, intent)
  !! print a note with an initial 'Note:'
  character(*),  intent (in) :: text
  integer,  intent (in), optional :: intent
  integer :: i
  i = 1
  if (present (intent)) then 
    if (intent >0 .and. intent <80) i = intent
  end if
  call print_colored (COLOR_NOTE, repeat(' ',i) // 'Note: '//trim(text))
  write (*,*) 
end subroutine print_note

subroutine print_text (text, intent)
  !! print a note text with an optional intent
  character(*),  intent (in) :: text
  integer,  intent (in), optional :: intent
  integer :: i
  i = 1
  if (present (intent)) then 
    if (intent >0 .and. intent <80) i = intent
  end if
  call print_colored (COLOR_NOTE, repeat(' ',i) // trim(text))
  write (*,*)
end subroutine print_text

!------------------------------------------------------------------------------------------
! Stops and prints an error message, or just warns
!------------------------------------------------------------------------------------------

subroutine my_stop(message, stoptype) 

  character(*), intent(in) :: message
  character(4), intent(in), optional :: stoptype

  if ((.not. present(stoptype)) .or. (stoptype == 'stop')) then
    write(*,*)
    call print_error ('Error: '//message)
    write(*,*)
    stop 1
  else
    write(*,*)
    call print_warning ('Error: '//message)
    write(*,*)
  end if

end subroutine my_stop

!-------------------------------------------------------------------------
! prints the integer ivalue colored depending
!   on its quality (e.g. Q_OK)
!-------------------------------------------------------------------------
  
subroutine print_colored_i (strlen, quality, ivalue)
  
  integer, intent(in) :: ivalue, quality, strlen

  character (strlen)  :: str
  character (10)      :: tmp_str
  integer             :: color 

  if (strlen < 1) return 

  select case (quality)
    case (Q_GOOD)
      color = COLOR_GOOD
    case (Q_OK)
      color = COLOR_NORMAL
    case (Q_BAD)
      color = COLOR_WARNING
    case (Q_NEW)
      color = COLOR_FEATURE
    case (Q_PROBLEM)
      color = COLOR_BAD
    case default 
      color = COLOR_NOTE
  end select
  str = repeat('*',strlen)

  if ((ivalue >= 10**strlen) .or. (ivalue <= -10**(strlen-1))) then
    str = repeat('*',strlen)
  else
    write (tmp_str, '(I10)') ivalue
    str = tmp_str ((len(tmp_str)-strlen+1):len(tmp_str))
  endif

  call print_colored (color, str)
  
end subroutine print_colored_i

!-------------------------------------------------------------------------
! prints the real rvalue colored depending
!   on its quality (e.g. Q_OK)
!-------------------------------------------------------------------------
  
subroutine print_colored_r (strlen, format_string, quality, rvalue)
  
  double precision, intent(in) :: rvalue
  integer, intent(in)          :: strlen, quality
  character (*), intent(in)    :: format_string

  character (strlen)  :: str
  integer             :: color 

  select case (quality)
    case (Q_GOOD)
      color = COLOR_GOOD
    case (Q_OK)
      color = COLOR_NORMAL
    case (Q_BAD)
      color = COLOR_WARNING
    case (Q_NEW)
      color = COLOR_FEATURE
    case (Q_PROBLEM)
      color = COLOR_BAD
    case default 
      color = COLOR_NOTE
  end select

  write (str,format_string) rvalue
  str = adjustr(str)

  call print_colored (color, str)

end subroutine print_colored_r

!-------------------------------------------------------------------------
! evalutes the quality (constant Q_GOOD etc) of a integer value 
!-------------------------------------------------------------------------

function i_quality (value, lim_good, lim_ok, lim_bad)

  integer, intent(in) :: value, lim_good, lim_ok, lim_bad
  integer :: i_quality

  if (value < lim_good) then
    i_quality = Q_GOOD
  elseif (value < lim_ok) then
    i_quality = Q_OK
  elseif (value < lim_bad) then
    i_quality = Q_BAD
  else
    i_quality = Q_PROBLEM
  end if

end function i_quality
  
!-------------------------------------------------------------------------
! evalutes the quality (constant Q_GOOD etc) of a real value 
!-------------------------------------------------------------------------

function r_quality (value, lim_good, lim_ok, lim_bad)

  double precision, intent(in) :: value, lim_good, lim_ok, lim_bad
  integer :: r_quality

  if (value < lim_good) then
    r_quality = Q_GOOD
  elseif (value < lim_ok) then
    r_quality = Q_OK
  elseif (value < lim_bad) then
    r_quality = Q_BAD
  else
    r_quality = Q_PROBLEM
  end if

end function r_quality

!-------------------------------------------------------------------------
! prints the string colored depending  on its quality (e.g. Q_OK)
!-------------------------------------------------------------------------
  
subroutine print_colored_s (quality, str)
  
  integer, intent(in)       :: quality
  character (*), intent(in) :: str

  integer  :: color 

  select case (quality)
    case (Q_GOOD)
      color = COLOR_GOOD
    case (Q_OK)
      color = COLOR_NORMAL
    case (Q_BAD)
      color = COLOR_WARNING
    case (Q_NEW)
      color = COLOR_FEATURE
    case (Q_PROBLEM)
      color = COLOR_BAD
    case default 
      color = COLOR_NOTE
  end select

  call print_colored (color, str)
  
end subroutine print_colored_s

!-------------------------------------------------------------------------
! prints a colored rating based on quality (e.g. Q_OK) with a lenght of strlen
!-------------------------------------------------------------------------
  
subroutine print_colored_rating (strlen, quality)
  
  integer, intent(in)      :: quality, strlen

  character (strlen)  :: str, comment
  integer             :: color 

  select case (quality)
    case (Q_GOOD)
      color = COLOR_GOOD
      comment ='perfect'
    case (Q_OK)
      color = COLOR_NORMAL
      comment ='ok'
    case (Q_BAD)
      color = COLOR_WARNING
      comment ='bad'
    case (Q_NEW)
      color = COLOR_FEATURE
      comment ='new'
    case default 
      color = COLOR_BAD
      comment ='critical'
  end select
  write (str,'(A)') comment
  str = adjustl(str)
  call print_colored (color, str)
  
end subroutine print_colored_rating


!-------------------------------------------------------------------------
! OMP Utils  
!-------------------------------------------------------------------------

subroutine set_number_of_threads()
  !! Multithreading: set number of threads used for optimization 

  use omp_lib    
  
#ifdef OPENMP 
  integer         :: max_threads, used_threads

  max_threads = omp_get_max_threads()         ! omp utility function ...                     

  if (max_threads > 1) then 
    if (.false. ) then                        ! activate for testing purposes
      call print_warning ("Because of option 'show_details' CPU multi threading will be switched off")
      call omp_set_num_threads( 1 )
    else
      used_threads = max_threads
      if (max_threads > 10) then 
        used_threads = max_threads - 2
      elseif (max_threads > 3) then 
        used_threads = max_threads - 1
      else
        used_threads = max_threads
      end if 
      call omp_set_num_threads(used_threads)
      call print_note (stri(used_threads)//" of "//stri(max_threads)//" CPU threads will be used.")
    end if 
  end if
#endif

end subroutine set_number_of_threads  

!-------------------------------------------------------------------------
! measure time to run 
!-------------------------------------------------------------------------
  
! integer         :: itime_start, itime_finish, rate
! doubleprecision :: time_diff

! call system_clock(count_rate=rate)
! call system_clock(itime_start)
! ............
! call system_clock(itime_finish)
! time_diff = real (itime_finish-itime_start)/real(rate)
! print '("Time = ",f6.3," seconds"',time_diff


end module os_util
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

  integer, parameter, private :: COLOR_BLUE   = int(Z"1")
  integer, parameter, private :: COLOR_GREEN  = int(Z"2")
  integer, parameter, private :: COLOR_RED    = int(Z"4")
  integer, parameter, private :: COLOR_INTENS = int(Z"8")

  integer, parameter, public  :: COLOR_GOOD   = iany([COLOR_GREEN, COLOR_INTENS])
  integer, parameter, public  :: COLOR_BAD    = iany([COLOR_RED,   COLOR_INTENS])
  integer, parameter, public  :: COLOR_NORMAL = iany([COLOR_RED, COLOR_GREEN, COLOR_BLUE])
  integer, parameter, public  :: COLOR_HIGH   = iany([COLOR_NORMAL, COLOR_INTENS])

  private

  public :: print_colored
  public :: make_directory
  
  interface print_colored
#ifdef UNIX
  module procedure print_colored
#else
  module procedure print_colored_windows
#endif    
  end interface

  interface make_directory
#ifdef UNIX
#else
  module procedure make_directory_windows
#endif    
  end interface

!------------------------------------------------------------------------------------------
!  unix  specific 
!------------------------------------------------------------------------------------------
#ifdef UNIX
  
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
        import
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
        import
        implicit none
!gcc$ attributes stdcall :: GetStdHandle
        integer(HANDLE) GetStdHandle
        integer(DWORD), value :: nStdHandle
    end function GetStdHandle
  end interface
!   public SetConsoleMode
  interface
    function SetConsoleMode(hConsoleHandle, dwMode) bind(C,name='SetConsoleMode')
        import
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
        import
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

subroutine print_colored (color_attribute, text)

  integer, intent (in) :: color_attribute
  character(*),  intent (in) :: text

! jx-todo - color have to be implemented via ansi escape codes 
  write(*,'(A)', advance = 'no') text

end subroutine print_colored

#else
  
subroutine print_colored_windows (color_attribute, text)

  integer, intent (in) :: color_attribute
!  integer(WORD), intent (in) :: wAttributes
  integer(WORD) :: wAttributes
  character(*),  intent (in) :: text
  
  integer(HANDLE) :: hConsoleOutput
  integer(BOOL) :: iresult
  type(T_CONSOLE_SCREEN_BUFFER_INFO) lpConsoleScreenBufferInfo

  wAttributes = int(color_attribute, 2) 
  
  hConsoleOutput = GetStdHandle(STD_OUTPUT_HANDLE)
  iresult = GetConsoleScreenBufferInfo(hConsoleOutput,lpConsoleScreenBufferInfo)
  
  iresult = SetConsoleTextAttribute(hConsoleOutput,wAttributes)
  write(*,'(A)', advance = 'no') text
  
  iresult = SetConsoleTextAttribute(hConsoleOutput,lpConsoleScreenBufferInfo%wAttributes)

end subroutine print_colored_windows

#endif

!------------------------------------------------------------------------------------------
!  Create Directory
!------------------------------------------------------------------------------------------
#ifdef UNIX
#else

subroutine make_directory_windows (subdirectory)

  character(*),  intent (in) :: subdirectory

  integer         :: istat
  character (255) :: mkdir_command

  mkdir_command = 'if not exist '//trim(subdirectory)//' mkdir '//trim(subdirectory)
  istat = system (trim(mkdir_command))
  
end subroutine make_directory_windows

#endif
  
end module os_util
# Set up project
#
# Xoptfoil-JX  
#
# - added worker and X5 api tester 
# - added -cpp to gfortran to activate precompiler
# - added precompiler flag for OS - WIN32 or UNIX
# - set XOPTFOIL_VERSION in build_windows.bat 
# - removed xfoil_only and doc files etc
# - added static linking of libs so no runtime libs are needed

# Variables from build_windows / build_linux
#
#       INSTALLDIR                   e.g. %CD%\windows
#       XOPTFOIL_VERSION             e.g. 1.5.0beta
#       TARGET_OS                         UNIX or WIN

set(CMAKE_SYSTEM_NAME $ENV{TARGET_OS})

if(CMAKE_SYSTEM_NAME MATCHES "UNIX")
  set (OS_FLAG "-DUNIX=1")
  # under Unix do dynamic linking so additional libs are required at runtime 
  set (STATIC_FLAG "")
  message (STATUS "\n \n Building XFoptfoil-JX Version $ENV{XOPTFOIL_VERSION} for UNIX\n\n")
else (CMAKE_SYSTEM_NAME MATCHES "UNIX")
  set (OS_FLAG "-DWIN32=1")
  # under Window do static linking so no additional libs are required at runtime 
  set (STATIC_FLAG "-static")
  message (STATUS "\n \n Building XFoptfoil-JX Version $ENV{XOPTFOIL_VERSION} for Windows\n\n")
endif (CMAKE_SYSTEM_NAME MATCHES "UNIX")

# that's needed to avoid strange make behaviour when installing the exe
unset(CMAKE_SYSTEM_NAME)

cmake_minimum_required(VERSION 2.8.8)
project(Xoptfoil-JX)
enable_language(Fortran)

# Some default directories
set(DATADIR ${CMAKE_INSTALL_PREFIX}/share
    CACHE PATH "Directory to install shared data and examples."
)

# Determine compiler
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
message(STATUS "Fortran compiler: " ${CMAKE_Fortran_COMPILER})

# Option to build with/without OpenMP support
set(ENABLE_OPENMP TRUE
    CACHE BOOL "Whether to build with OpenMP support."
)

# Enable OpenMP by default for Release builds, but disable by default for Debug
if (CMAKE_BUILD_TYPE MATCHES "Release")
  set (OPENMP_FLAG "-fopenmp -DOPENMP=1")
  if (NOT ENABLE_OPENMP)
    MESSAGE(STATUS "Disabling OpenMP support since ENABLE_OPENMP=FALSE")
    set (OPENMP_FLAG "")
  endif (NOT ENABLE_OPENMP)
elseif (CMAKE_BUILD_TYPE MATCHES "Debug")
  set (OPENMP_FLAG "")
  if (ENABLE_OPENMP)
    MESSAGE(STATUS "Enabling OpenMP support for debug build since ENABLE_OPENMP=TRUE")
    set (OPENMP_FLAG "-fopenmp  -DOPENMP=1")
  endif (ENABLE_OPENMP)
endif (CMAKE_BUILD_TYPE MATCHES "Release")

# Default compiler flags
if (Fortran_COMPILER_NAME MATCHES "gfortran")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 ${OPENMP_FLAG} -cpp ${OS_FLAG} ${STATIC_FLAG}")
  set (CMAKE_Fortran_FLAGS_DEBUG "-g -fdump-core -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow -Wall ${OPENMP_FLAG} -cpp ${OS_FLAG} ${STATIC_FLAG}")
  set (Fortran_REAL8_FLAG "-fdefault-real-8")
elseif (Fortran_COMPILER_NAME MATCHES "ifort")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O3 ${OPENMP_FLAG}")
  set (CMAKE_Fortran_FLAGS_DEBUG "-g -warn all ${OPENMP_FLAG}")
  set (Fortran_REAL8_FLAG "-r8")
else (Fortran_COMPILER_NAME MATCHES "gfortran")
  message(FATAL_ERROR "Fortran compiler not supported.")
endif (Fortran_COMPILER_NAME MATCHES "gfortran")

# Preprocessor definitions
add_definitions(-DPACKAGE_VERSION=\"$ENV{XOPTFOIL_VERSION}\")

# Set source files (exclude programs here)
file(GLOB SOURCESDBLE "src/fortran/*.f90" "src/fortran/*.F90" "src/fortran/xfoil_deps/*.f90")
file(GLOB SOURCESREAL "src/fortran/xfoil_deps/*.f" "src/fortran/naca456_deps/*.f90")
list(REMOVE_ITEM SOURCESDBLE "${CMAKE_SOURCE_DIR}/src/fortran/main.f90")
#list(REMOVE_ITEM SOURCESDBLE "${CMAKE_SOURCE_DIR}/src/fortran/xfoil_only.f90")
list(REMOVE_ITEM SOURCESDBLE "${CMAKE_SOURCE_DIR}/src/fortran/xfoil_worker.f90")
list(REMOVE_ITEM SOURCESDBLE "${CMAKE_SOURCE_DIR}/src/fortran/x5_api_tester.f90")

# Real-8 flag for xfoil and naca456 source files
set_source_files_properties(${SOURCESREAL} PROPERTIES COMPILE_FLAGS ${Fortran_REAL8_FLAG})

# CMake "object libraries" which allow sharing object files for multiple executables
# https://cmake.org/Wiki/CMake/Tutorials/Object_Library
# (Introduced in CMake 2.8.8)
add_library(MYOBJLIB OBJECT ${SOURCESDBLE} ${SOURCESREAL})

# Build executables
add_executable("xoptfoil-JX"   $<TARGET_OBJECTS:MYOBJLIB> "src/fortran/main.f90")
add_executable("xfoil_worker"  $<TARGET_OBJECTS:MYOBJLIB> "src/fortran/xfoil_worker.f90")
add_executable("x5_api_tester" $<TARGET_OBJECTS:MYOBJLIB> "src/fortran/x5_api_tester.f90")

# Installation
install(TARGETS xoptfoil-JX xfoil_worker x5_api_tester DESTINATION bin)
install(FILES src/python/xoptfoil_visualizer-jx.py
        PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                    GROUP_READ             GROUP_EXECUTE
                    WORLD_READ             WORLD_EXECUTE
        DESTINATION bin)

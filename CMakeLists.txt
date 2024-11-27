# Minimum CMake version required
cmake_minimum_required(VERSION 3.10)

# Project name and language
project(
    Fyaml
    VERSION 0.1.0
    LANGUAGES Fortran)

include(CMakePrintHelpers)

# Set build type to Debug if not specified
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug)
endif()

cmake_print_variables(CMAKE_Fortran_COMPILER CMAKE_Fortran_COMPILER_ID CMAKE_BUILD_TYPE)

# Set default compiler options for Fortran
set(_fyaml_compiler_options "")

# Add debug flags for Fortran
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU|G95" AND CMAKE_BUILD_TYPE MATCHES "Debug")
  list(
    APPEND
    _fyaml_compiler_options
    -Og
    -Wall
    -Wextra
    -pedantic
    -fcheck=all
    -ffpe-trap=invalid,zero,overflow
    -fbacktrace
    -fmax-errors=0)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel" AND CMAKE_BUILD_TYPE MATCHES "Debug")
  list(
    APPEND
    _fyaml_compiler_options
    -O0
    -warn
    all
    -check
    all
    -implicitnone
    -fpe0
    -traceback
    -error-limit
    0
    -diag-disable
    6379,10448)
endif()

# Set the Fortran standard
set(CMAKE_Fortran_STANDARD 2003)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)

# Add the source files from the src directory
add_subdirectory(src)
add_subdirectory(tests)
# add_subdirectory(example)

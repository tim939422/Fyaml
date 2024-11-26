# Minimum CMake version required
cmake_minimum_required(VERSION 3.10)

# Project name and language
project(Fyaml LANGUAGES Fortran)

# Set the Fortran standard
set(CMAKE_Fortran_STANDARD 2003)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)

# Add the source files from the src directory
file(GLOB SOURCES "src/*.f90")

# Create the library
add_library(fyaml STATIC ${SOURCES})

# Specify the output directory for the library
set_target_properties(fyaml PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
)

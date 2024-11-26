# Minimum CMake version required
cmake_minimum_required(VERSION 3.10)

# Project name and language
project(YAMLParserFortran LANGUAGES Fortran)

# Set the Fortran standard
set(CMAKE_Fortran_STANDARD 2008)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)

# Add the source files from the src directory
file(GLOB SOURCES "src/*.f90")

# Create the executable
add_executable(yaml_parser ${SOURCES})

# Specify the output directory for the executable
set_target_properties(yaml_parser PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

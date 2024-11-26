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

# Add the test files
file(GLOB TESTS "tests/*.f90")

# Create the test executable
add_executable(test_fyaml ${TESTS})

# Link the test executable with the library
target_link_libraries(test_fyaml fyaml)

# Specify the output directory for the test executable
set_target_properties(test_fyaml PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
)

# Enable testing
enable_testing()

# Add a test
add_test(NAME TestFyaml COMMAND ${CMAKE_BINARY_DIR}/bin/test_fyaml)

set(_srcs
    test_utils.F90
    test_fyaml.F90)

add_compile_options(${_fyaml_compiler_options})

enable_testing()

add_executable(test_fyaml test_utils.F90 test_fyaml.F90)
target_link_libraries(test_fyaml PRIVATE fyaml)
set_target_properties(test_fyaml PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

# Copy test data file to build directory
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/test_example.yaml
              ${CMAKE_CURRENT_BINARY_DIR}/test_example.yaml
              COPYONLY)

# Add environment variable for test data location
set(ENV{FYAML_TEST_DIR} ${CMAKE_CURRENT_SOURCE_DIR})

add_test(
  NAME test_fyaml
  COMMAND ${CMAKE_COMMAND} -E env
          FYAML_TEST_DIR=${CMAKE_CURRENT_SOURCE_DIR}
          $<TARGET_FILE:test_fyaml>
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

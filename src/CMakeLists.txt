set(_srcs
    yaml_types.F90
    yaml_parser.F90
    fyaml.F90)

add_library(fyaml ${_srcs})
set_target_properties(fyaml PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/include)

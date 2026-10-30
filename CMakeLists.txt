cmake_minimum_required (VERSION 3.14)

project (v3fit C Fortran)

add_executable (xv3fit)
add_dependencies(xv3fit pull_v3fit)

target_link_libraries (xv3fit PUBLIC siesta vmec)

add_subdirectory (Sources)

install (TARGETS xv3fit)

################################################################################
#  Testing                                                                     #
################################################################################

add_subdirectory (Testing)

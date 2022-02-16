cmake_minimum_required (VERSION 3.14)

project (v3fit C Fortran)

add_executable (xv3fit)
target_link_libraries (xv3fit PUBLIC siesta vmec)

add_subdirectory (Sources)

################################################################################
#  Testing                                                                     #
################################################################################

add_subdirectory (Testing)

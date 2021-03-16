cmake_minimum_required (VERSION 3.14)

project (v3fit C Fortran)
set (CMAKE_CXX_STANDARD 11)

add_executable (xv3fit)
add_dependencies (xv3fit siesta vmec)
target_link_libraries (xv3fit siesta vmec)
target_include_directories (xv3fit PUBLIC
                            $<TARGET_PROPERTY:siesta,BINARY_DIR>
                            $<TARGET_PROPERTY:vmec,BINARY_DIR>)

add_subdirectory (Sources)

################################################################################
#  Testing                                                                     #
################################################################################

add_subdirectory (Testing)

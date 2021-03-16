#  Run the reconstruction task. This just tests if the reconstruction executes correctly. 
#  Subsequent tests will examine the results of the reconstruction for correct 
#  results.

#  Copy input file from the source to the build directory.
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/coils.simple_kp36
                ${CMAKE_CURRENT_BINARY_DIR}/coils.simple_kp36
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.grid.v3fit
                ${CMAKE_CURRENT_BINARY_DIR}/input.grid.v3fit
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.iso.v3fit
                ${CMAKE_CURRENT_BINARY_DIR}/input.iso.v3fit
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/limiter.dat
                ${CMAKE_CURRENT_BINARY_DIR}/limiter.dat
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.vmec
                ${CMAKE_CURRENT_BINARY_DIR}/input.vmec
                COPYONLY)

add_test (NAME    limiter_test_lgrid
          COMMAND $<TARGET_PROPERTY:lgrid,BINARY_DIR>/lgrid limiter.dat 0.7 0.001 600 -0.30 0.001 600)
add_test (NAME    limiter_test_mgrid
          COMMAND $<TARGET_PROPERTY:mgrid,BINARY_DIR>/mgrid coils.simple_kp36)

add_test (NAME    limiter_test_gridtest
          COMMAND $<TARGET_PROPERTY:stell,MPIEXEC_EXECUTABLE> $<TARGET_PROPERTY:stell,MPIEXEC_NUMPROC_FLAG> $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS> $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.grid.v3fit -para=-1)
add_test (NAME    limiter_test_gridtest_model
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.grid.v3fit --s_name=edge_lim_1 --attribute=model --value=0.00000E+00 --range=5.0E-9)

add_test (NAME    limiter_test_isotest
          COMMAND $<TARGET_PROPERTY:stell,MPIEXEC_EXECUTABLE> $<TARGET_PROPERTY:stell,MPIEXEC_NUMPROC_FLAG> $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS> $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.iso.v3fit -para=-1)
add_test (NAME    limiter_test_isotest_model
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.iso.v3fit --s_name=edge_lim_1 --attribute=model --value=0.00000E+00 --range=5.0E-9)


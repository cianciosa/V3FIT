#  Run the reconstruction task. This just tests if the reconstruction executes correctly. 
#  Subsequent tests will examine the results of the reconstruction for correct 
#  results.

#  Copy input file from the source to the build directory.
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.vmec
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.vmec
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.v3fit
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.v3fit
                COPYONLY)

add_test (NAME    equilibrium_reset_test
          COMMAND $<TARGET_PROPERTY:stell,MPIEXEC_EXECUTABLE> $<TARGET_PROPERTY:stell,MPIEXEC_NUMPROC_FLAG> $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS> $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit -para=-1 -serial)
set_tests_properties (equilibrium_reset_test
                      PROPERTIES
                      PROCESSORS $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS>)

#  Since the reconstruction task test was run, Check the output files for correct values.

#  The reconstruction was configured with all the same reconstruction parameters. If the equilibrium was correctky reset, the number of iterations should be the same for all parameters.
add_test (NAME    equilibrium_reset_test_jacobian_iter
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jacobian_iter.py --file=runlog.input.test.v3fit)
set_tests_properties (equilibrium_reset_test_jacobian_iter
                      PROPERTIES
                      DEPENDS equilibrium_reset_test)

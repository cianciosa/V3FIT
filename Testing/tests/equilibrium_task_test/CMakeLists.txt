#  Run the equilibrium task. This just tests if the equilibrium executes
#  correctly. Subsequent tests will examine the results of the equilibrium for
#  correct results.

#  Copy input file from the source to the build directory.
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.vmec
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.vmec
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.v3fit
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.v3fit
                COPYONLY)

add_test (NAME    equilibrium_task_test
          COMMAND $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit)
add_test (NAME    equilibrium_task_test_mpi
          COMMAND $<TARGET_PROPERTY:stell,MPIEXEC_EXECUTABLE> $<TARGET_PROPERTY:stell,MPIEXEC_NUMPROC_FLAG> $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS> $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit)

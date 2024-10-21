#  Run the reconstruction task. This just tests if the reconstruction executes correctly. 
#  Subsequent tests will examine the results of the reconstruction for correct 
#  results.

#  Copy input file from source to the build directory.
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/thomson.dot
                ${CMAKE_CURRENT_BINARY_DIR}/thomson.dot
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.vmec
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.vmec
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/siesta.jcf
                ${CMAKE_CURRENT_BINARY_DIR}/siesta.jcf
                COPYONLY)
SET (VMEC_FILE_NAME input.test.vmec)
SET (SIESTA_FILE_NAME siesta.jcf)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.v3fit
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.v3fit)

add_test (NAME    siesta_recon_task_test
          COMMAND $<TARGET_PROPERTY:stell,MPIEXEC_EXECUTABLE> $<TARGET_PROPERTY:stell,MPIEXEC_NUMPROC_FLAG> $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS> $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit -para=-1)

#  Since the reconstruction task test was run, Check the output files for correct values.

#  Check that the reconstruction parameter reconstructed to the correct values.
add_test (NAME    siesta_recon_task_test_helpert
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=helpert --value=5.0E-4 --range=1.0E-4 --index1=1)
set_tests_properties (siesta_recon_task_test_helpert
                      PROPERTIES
                      DEPENDS siesta_recon_task_test)

#  Check that the signal effectiveness total.
add_test (NAME    siesta_recon_task_test_sem_totals
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_sem_total.py --file=recout.input.test.v3fit)
set_tests_properties (siesta_recon_task_test_sem_totals
                      PROPERTIES
                      DEPENDS siesta_recon_task_test)

#  Check correlation matrix values.
add_test (NAME siesta_recon_task_test_check_reconstruction_correlation_matrix
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Reconstruction)
set_tests_properties (siesta_recon_task_test_check_reconstruction_correlation_matrix
                      PROPERTIES
                      DEPENDS siesta_recon_task_test)

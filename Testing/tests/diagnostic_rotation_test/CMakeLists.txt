#  Run the reconstruction task. This just tests if the reconstruction executes correctly. 
#  Subsequent tests will examine the results of the reconstruction for correct 
#  results. First the mdsig files must be made.

#  Copy input file from the source to the build directory.
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/diagnostic_rot.dot
                ${CMAKE_CURRENT_BINARY_DIR}/diagnostic_rot.dot
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/thomson_rot.dot
                ${CMAKE_CURRENT_BINARY_DIR}/thomson_rot.dot
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/sxrem_rot.dot
                ${CMAKE_CURRENT_BINARY_DIR}/sxrem_rot.dot
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/intpol_rot.dot
                ${CMAKE_CURRENT_BINARY_DIR}/intpol_rot.dot
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/v3rfun.in
                ${CMAKE_CURRENT_BINARY_DIR}/v3rfun.in
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.unrot
                ${CMAKE_CURRENT_BINARY_DIR}/input.unrot
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.v3fit
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.v3fit
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/coils.null
                ${CMAKE_CURRENT_BINARY_DIR}/coils.null
                COPYONLY)

add_test (NAME    diagnostic_rotation_test_mdsig
          COMMAND $<TARGET_PROPERTY:xv3rfun,BINARY_DIR>/xv3rfun v3rfun.in)
add_test (NAME    diagnostic_rotation_test
          COMMAND $<TARGET_PROPERTY:stell,MPIEXEC_EXECUTABLE> $<TARGET_PROPERTY:stell,MPIEXEC_NUMPROC_FLAG> $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS> $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit -para=-1)

#  Since the reconstruction task test was run, Check the output files for correct values.

#  Check that the reconstruction parameter reconstructed to the correct values.
add_test (NAME    diagnostic_rotation_test_phi_offset
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=phi_offset --value=0.157 --range=0.0002)

#  Check that the signal effectiveness total.
add_test (NAME    diagnostic_rotation_test_sem_totals
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_sem_total.py --file=recout.input.test.v3fit)

#  Check correlation matrix values.
add_test (NAME    diagnostic_rotation_test_check_reconstruction_correlation_matrix
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Reconstruction)

#  Check the multiple sxr profile.
add_test (NAME    diagnostic_rotation_test_check_sxrem_model
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr1-005 --attribute=model --value=5.0E-1 --range=1.0E-1)
add_test (NAME    diagnostic_rotation_test_check_sxrem_model_2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr2-006 --attribute=model --value=1.0 --range=1.0E-1)

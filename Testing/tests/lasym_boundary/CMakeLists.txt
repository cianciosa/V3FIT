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

add_test (NAME    lasym_boundary_test
          COMMAND $<TARGET_PROPERTY:stell,MPIEXEC_EXECUTABLE> $<TARGET_PROPERTY:stell,MPIEXEC_NUMPROC_FLAG> $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS> $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit)

#  Since the reconstruction task test was run, Check the output files for correct values.

#  Check that the signal effectiveness total.

#  Check the jacobians for the the correct values.
add_test (NAME    lasym_boundary_test_jac_1-1:1-1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=0 --ihigh=0 --jlow=0 --jhigh=0 --value=100.0)
add_test (NAME    lasym_boundary_test_jac_1-1:2-2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=0 --ihigh=0 --jlow=1 --jhigh=1 --value=0.0)
add_test (NAME    lasym_boundary_test_jac_2-2:1-1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=1 --ihigh=1 --jlow=0 --jhigh=0 --value=0.0)
add_test (NAME    lasym_boundary_test_jac_2-2:2-2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=1 --ihigh=1 --jlow=1 --jhigh=1 --value=100.0)

#  Check that the observed signal was written correctly.
add_test (NAME    lasym_boundary_test_check_observed_prior_1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=observed --value=0.8 --range=1.0E-10)
add_test (NAME    lasym_boundary_test_check_observed_prior_2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=observed --value=0.1 --range=1.0E-10)

#  Check that the observed sigma was written correctly.
add_test (NAME    lasym_boundary_test_check_sigma_prior_1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=sigma --value=1.00000E-04 --range=1.0E-10)
add_test (NAME    lasym_boundary_test_check_sigma_prior_2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=sigma --value=1.00000E-04 --range=1.0E-10)

#  Check correlation matrix values.
add_test (NAME lasym_boundary_test_check_reconstruction_correlation_matrix
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Reconstruction)

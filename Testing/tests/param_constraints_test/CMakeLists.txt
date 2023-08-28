#  Run the reconstruction task. This just tests if the reconstruction executes
#  correctly. Subsequent tests will examine the results of the reconstruction for
#  correct results.

#  Copy input file from the source to the build directory.
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.vmec
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.vmec
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.v3fit
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.v3fit
                COPYONLY)

add_test (NAME    param_constraints_test
          COMMAND $<TARGET_PROPERTY:stell,MPIEXEC_EXECUTABLE> $<TARGET_PROPERTY:stell,MPIEXEC_NUMPROC_FLAG> $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS> $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit -para=-1)
set_tests_properties (param_constraints_test
                      PROPERTIES
                      PROCESSORS $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS>)

#  Since the reconstruction task test was run, Check the output files for correct values.

#  Check that the reconstruction parameter reconstructed to the correct values.
add_test (NAME    param_constraints_test_pp_ne_af
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_ne_af --value=0.0 --index1=2)
set_tests_properties (param_constraints_test_pp_ne_af
                      PROPERTIES
                      DEPENDS param_constraints_test)
add_test (NAME    param_constraints_test_pp_te_as
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_te_as --value=1.0 --index1=2)
set_tests_properties (param_constraints_test_pp_te_as
                      PROPERTIES
                      DEPENDS param_constraints_test)
add_test (NAME    param_constraints_test_pp_sxrem_as_a_1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_sxrem_as_a --value=0.7 --index1=1 --index2=2)
set_tests_properties (param_constraints_test_pp_sxrem_as_a_1
                      PROPERTIES
                      DEPENDS param_constraints_test)
add_test (NAME    param_constraints_test_pp_sxrem_as_a_2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_sxrem_as_a --value=0.7 --index1=1 --index2=3)
set_tests_properties (param_constraints_test_pp_sxrem_as_a_2
                      PROPERTIES
                      DEPENDS param_constraints_test)

#  Check that the signal effectiveness total.
add_test (NAME    param_constraints_test_sem_totals
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_sem_total.py --file=recout.input.test.v3fit)
set_tests_properties (param_constraints_test_sem_totals
                      PROPERTIES
                      DEPENDS param_constraints_test)

#  Check that jacobians for the correct values.
add_test (NAME    param_constraints_test_jac_1-1:2-2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=0 --ihigh=0 --jlow=1 --jhigh=1 --value=0.0)
set_tests_properties (param_constraints_test_jac_1-1:2-2
                      PROPERTIES
                      DEPENDS param_constraints_test)
add_test (NAME    param_constraints_test_jac_2-2:3-4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=1 --ihigh=1 --jlow=2 --jhigh=3 --value=0.0)
set_tests_properties (param_constraints_test_jac_2-2:3-4
                      PROPERTIES
                      DEPENDS param_constraints_test)
add_test (NAME    param_constraints_test_jac_1-2:3-4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=0 --ihigh=1 --jlow=2 --jhigh=3 --value=0.0)
set_tests_properties (param_constraints_test_jac_1-2:3-4
                      PROPERTIES
                      DEPENDS param_constraints_test)
add_test (NAME    param_constraints_test_jac_3-4:1-2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=2 --ihigh=3 --jlow=0 --jhigh=2 --value=0.0)
set_tests_properties (param_constraints_test_jac_3-4:1-2
                      PROPERTIES
                      DEPENDS param_constraints_test)
add_test (NAME    param_constraints_test_jac_3-3:4-4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=2 --ihigh=2 --jlow=3 --jhigh=3 --value=0.0)
set_tests_properties (param_constraints_test_jac_3-3:4-4
                      PROPERTIES
                      DEPENDS param_constraints_test)
add_test (NAME    param_constraints_test_jac_4-4:3-3
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=3 --ihigh=3 --jlow=4 --jhigh=4 --value=0.0)
set_tests_properties (param_constraints_test_jac_4-4:3-3
                      PROPERTIES
                      DEPENDS param_constraints_test)

#  Check parameter constraint types for the correct values.
add_test (NAME    param_constraints_test_type_pp_ne_af_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_ne_af --index1=2 --range_l=value)
set_tests_properties (param_constraints_test_type_pp_ne_af_type
                      PROPERTIES
                      DEPENDS param_constraints_test)
add_test (NAME    param_constraints_test_type_pp_te_as_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_te_as --index1=2 --range_h=pp_te_as --range_l=pp_te_as --indexh1=3 --indexl1=1)
set_tests_properties (param_constraints_test_type_pp_te_as_type
                      PROPERTIES
                      DEPENDS param_constraints_test)
add_test (NAME    param_constraints_test_type_pp_sxrem_as_a_1_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_sxrem_as_a --index1=1 --index2=2 --range_h=pp_sxrem_as_a --range_l=pp_sxrem_as_a --indexh1=1 --indexh2=3 --indexl1=1 --indexl2=1)
set_tests_properties (param_constraints_test_type_pp_sxrem_as_a_1_type
                      PROPERTIES
                      DEPENDS param_constraints_test)
add_test (NAME    param_constraints_test_type_pp_sxrem_as_a_2_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_sxrem_as_a --index1=1 --index2=3 --range_h=pp_sxrem_as_a --range_l=pp_sxrem_as_a --indexh1=1 --indexh2=4 --indexl1=1 --indexl2=2)
set_tests_properties (param_constraints_test_type_pp_sxrem_as_a_2_type
                      PROPERTIES
                      DEPENDS param_constraints_test)

#  Check correlation matrix values.
add_test (NAME param_constraints_test_check_reconstruction_correlation_matrix
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Reconstruction)
set_tests_properties (param_constraints_test_check_reconstruction_correlation_matrix
                      PROPERTIES
                      DEPENDS param_constraints_test)

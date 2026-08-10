#  Run the multigrid equilibrium task. This just tests if the task executes
#  correctly. Subsequent tests will examine the results of the reconstruction
#  for correct results.

configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.vmec
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.vmec
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.v3fit
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.v3fit
                COPYONLY)

add_test (NAME    multigrid_v3post_test
          COMMAND $<TARGET_PROPERTY:stell,MPIEXEC_EXECUTABLE> $<TARGET_PROPERTY:stell,MPIEXEC_NUMPROC_FLAG> $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS> $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit -para=-1)
set_tests_properties (multigrid_v3post_test
                      PROPERTIES
                      PROCESSORS $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS>)

#  Since the reconstruction task test was run, Check the output files for correct values.
add_test (NAME    multigrid_v3post_test_rbc00
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=1.0 --range=0.00001 --index1=0 --index2=0)
set_tests_properties (multigrid_v3post_test_rbc00
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_rbc01
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.4 --range=0.00001 --index1=0 --index2=1)
set_tests_properties (multigrid_v3post_test_rbc01
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_zbs01
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=zbs --value=3.06E-1 --range=0.00001 --index1=0 --index2=1)
set_tests_properties (multigrid_v3post_test_zbs01
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_rbc02
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=5.872E-1 --range=0.00001 --index1=0 --index2=2)
set_tests_properties (multigrid_v3post_test_rbc02
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_zbs02
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=zbs --value=3.4E-2 --range=0.00001 --index1=0 --index2=2)
set_tests_properties (multigrid_v3post_test_zbs02
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_rbc03
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.0 --range=0.00001 --index1=0 --index2=3)
set_tests_properties (multigrid_v3post_test_rbc03
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_zbs03
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=zbs --value=4.4E-2 --range=0.00001 --index1=0 --index2=3)
set_tests_properties (multigrid_v3post_test_zbs03
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)

#  Check that the observed signal was written correctly.
add_test (NAME    multigrid_v3post_test_check_observed_prior_1
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=observed --value=1.0 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_observed_prior_1
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_observed_prior_2
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=observed --value=0.4 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_observed_prior_2
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_observed_prior_3
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=observed --value=3.06E-1 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_observed_prior_3
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_observed_prior_4
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=observed --value=5.872E-1 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_observed_prior_4
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_observed_prior_5
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=observed --value=3.4E-2 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_observed_prior_5
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_observed_prior_6
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=observed --value=0.0 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_observed_prior_6
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_observed_prior_7
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=observed --value=4.4E-2 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_observed_prior_7
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)

#  Check that the model signal was written correctly.
add_test (NAME    multigrid_v3post_test_check_model_prior_1
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=model --value=1.0 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_model_prior_1
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_model_prior_2
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=model --value=0.4 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_model_prior_2
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_model_prior_3
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=model --value=3.06E-1 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_model_prior_3
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_model_prior_4
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=model --value=5.872E-1 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_model_prior_4
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_model_prior_5
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=model --value=3.4E-2 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_model_prior_5
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_model_prior_6
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=model --value=0.0 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_model_prior_6
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_model_prior_7
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=model --value=4.4E-2 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_model_prior_7
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)

#  Check that the sigma signal was written correctly.
add_test (NAME    multigrid_v3post_test_check_sigma_prior_1
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=sigma --value=1.0E-2 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_sigma_prior_1
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_sigma_prior_2
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=sigma --value=1.0E-3 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_sigma_prior_2
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_sigma_prior_3
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=sigma --value=1.0E-3 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_sigma_prior_3
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_sigma_prior_4
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=sigma --value=1.0E-3 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_sigma_prior_4
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_sigma_prior_5
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=sigma --value=1.0E-4 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_sigma_prior_5
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_sigma_prior_6
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=sigma --value=1.0E-5 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_sigma_prior_6
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)
add_test (NAME    multigrid_v3post_test_check_sigma_prior_7
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=sigma --value=1.0E-4 --range=1.0E-10)
set_tests_properties (multigrid_v3post_test_check_sigma_prior_7
                      PROPERTIES
                      DEPENDS multigrid_v3post_test)

#  Run the reconstruction task. This just tests if the reconstruction executes correctly. 
#  Subsequent tests will examine the results of the reconstruction for correct 
#  results.

#  Copy input file from the source to the build directory.
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/thomson.dot
                ${CMAKE_CURRENT_BINARY_DIR}/thomson.dot
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.vmec
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.vmec
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.v3fit
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.v3fit
                COPYONLY)

add_test (NAME    ne_pressure_test
          COMMAND $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit -para=-1 -mpi=auto)

#  Since the reconstruction task test was run, Check the output files for correct values.

#  Check that the observed signal was written correctly.
add_test (NAME    ne_pressure_test_check_observed_thm001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=observed --value=15 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_observed_thm002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=observed --value=15 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_observed_thm003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=observed --value=15 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_observed_thm004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=observed --value=15 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_observed_thm005
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=observed --value=1.24E20 --range=1.0)
add_test (NAME    ne_pressure_test_check_observed_thm006
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=observed --value=1.24E20 --range=1.0)
add_test (NAME    ne_pressure_test_check_observed_thm007
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=observed --value=1.24E20 --range=1.0)
add_test (NAME    ne_pressure_test_check_observed_thm008
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=observed --value=1.24E20 --range=1.0)
add_test (NAME    ne_pressure_test_check_observed_thm009
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=observed --value=400 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_observed_thm010
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=observed --value=400 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_observed_thm011
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=observed --value=400 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_observed_thm012
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=observed --value=400 --range=1.0E-10)

#  Check that the observed sigma was written correctly.
add_test (NAME    ne_pressure_test_check_sigma_thm001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=sigma --value=0.015 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_sigma_thm002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=sigma --value=0.015 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_sigma_thm003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=sigma --value=0.015 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_sigma_thm004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=sigma --value=0.015 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_sigma_thm005
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=sigma --value=1.24E18 --range=1.0)
add_test (NAME    ne_pressure_test_check_sigma_thm006
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=sigma --value=1.24E18 --range=1.0)
add_test (NAME    ne_pressure_test_check_sigma_thm007
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=sigma --value=1.24E18 --range=1.0)
add_test (NAME    ne_pressure_test_check_sigma_thm008
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=sigma --value=1.24E18 --range=1.0)
add_test (NAME    ne_pressure_test_check_sigma_thm009
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=sigma --value=4 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_sigma_thm010
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=sigma --value=4 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_sigma_thm011
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=sigma --value=4 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_sigma_thm012
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=sigma --value=4 --range=1.0E-10)

#  Check that the model signal was computed correctly.
add_test (NAME    ne_pressure_test_check_model_thm001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=model --value=15 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_model_thm002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=model --value=15 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_model_thm003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=model --value=15 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_model_thm004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=model --value=15 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_model_thm005
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=model --value=1.24E20 --range=1.0E18)
add_test (NAME    ne_pressure_test_check_model_thm006
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=model --value=1.24E20 --range=1.0E18)
add_test (NAME    ne_pressure_test_check_model_thm007
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=model --value=1.24E20 --range=1.0E18)
add_test (NAME    ne_pressure_test_check_model_thm008
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=model --value=1.24E20 --range=1.0E18)
add_test (NAME    ne_pressure_test_check_model_thm009
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=model --value=400 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_model_thm010
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=model --value=400 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_model_thm011
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=model --value=400 --range=1.0E-10)
add_test (NAME    ne_pressure_test_check_model_thm012
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=model --value=400 --range=1.0E-10)

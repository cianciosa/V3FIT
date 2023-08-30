#  Run the v3post task. This just tests if the equilibrium executes correctly. 
#  Subsequent tests will examine the results of the v3post for correct 
#  results.

#  Copy input file from the source to the build directory.
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.v3fit
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.v3fit
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.vmec
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.vmec
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/intpol.dot
                ${CMAKE_CURRENT_BINARY_DIR}/intpol.dot
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/sxrem.dot
                ${CMAKE_CURRENT_BINARY_DIR}/sxrem.dot
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/thomson.dot
                ${CMAKE_CURRENT_BINARY_DIR}/thomson.dot
                COPYONLY)

add_test (NAME    v3post_task_test
          COMMAND $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit -para=-1)

#  Since the v3post task test was run, Check the recout file for correct values.
add_test (NAME    v3post_task_check_model_sxr001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr001 --attribute=model --value=0.5 --range=1.0E-10)
set_tests_properties (v3post_task_check_model_sxr001
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_sxr002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr002 --attribute=model --value=0.5 --range=1.0E-10)
set_tests_properties (v3post_task_check_model_sxr002
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_sxr003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr003 --attribute=model --value=0.5 --range=1.0E-10)
set_tests_properties (v3post_task_check_model_sxr003
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_sxr004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr004 --attribute=model --value=0.5 --range=1.0E-10)
set_tests_properties (v3post_task_check_model_sxr004
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_int001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int001 --attribute=model --value=5.0E17 --range=1.0)
set_tests_properties (v3post_task_check_model_int001
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_int002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int002 --attribute=model --value=5.0E17 --range=1.0)
set_tests_properties (v3post_task_check_model_int002
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_int003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int003 --attribute=model --value=5.0E17 --range=1.0)
set_tests_properties (v3post_task_check_model_int003
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_int004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int004 --attribute=model --value=5.0E17 --range=1.0)
set_tests_properties (v3post_task_check_model_int004
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_int005
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int005 --attribute=model --value=0 --range=1.0E-10)
set_tests_properties (v3post_task_check_model_int005
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_int006
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int006 --attribute=model --value=0 --range=1.0E-10)
set_tests_properties (v3post_task_check_model_int006
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_int007
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int007 --attribute=model --value=0 --range=1.0E-10)
set_tests_properties (v3post_task_check_model_int007
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_int008
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int008 --attribute=model --value=0 --range=1.0E-10)
set_tests_properties (v3post_task_check_model_int008
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_int009
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int009 --attribute=model --value=-5.5E-5 --range=5.0E-7)
set_tests_properties (v3post_task_check_model_int009
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_int010
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int010 --attribute=model --value=-5.5E-5 --range=5.0E-7)
set_tests_properties (v3post_task_check_model_int010
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_int011
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int011 --attribute=model --value=-5.5E-5 --range=5.0E-7)
set_tests_properties (v3post_task_check_model_int011
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_int012
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int012 --attribute=model --value=-5.5E-5 --range=5.0E-7)
set_tests_properties (v3post_task_check_model_int012
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_thm001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=model --value=15 --range=1.0E-10)
set_tests_properties (v3post_task_check_model_thm001
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_thm002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=model --value=15 --range=1.0E-10)
set_tests_properties (v3post_task_check_model_thm002
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_thm003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=model --value=15 --range=1.0E-10)
set_tests_properties (v3post_task_check_model_thm003
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_thm004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=model --value=15 --range=1.0E-10)
set_tests_properties (v3post_task_check_model_thm004
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_thm005
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=model --value=1.0E18 --range=1.0)
set_tests_properties (v3post_task_check_model_thm005
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_thm006
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=model --value=1.0E18 --range=1.0)
set_tests_properties (v3post_task_check_model_thm006
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_thm007
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=model --value=1.0E18 --range=1.0)
set_tests_properties (v3post_task_check_model_thm007
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_thm008
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=model --value=1.0E18 --range=1.0)
set_tests_properties (v3post_task_check_model_thm008
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_thm009
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=model --value=400 --range=1.0E-10)
set_tests_properties (v3post_task_check_model_thm009
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_thm010
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=model --value=400 --range=1.0)
set_tests_properties (v3post_task_check_model_thm010
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_thm011
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=model --value=400 --range=1.0)
set_tests_properties (v3post_task_check_model_thm011
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_thm012
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=model --value=400 --range=1.0)
set_tests_properties (v3post_task_check_model_thm012
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_prior_1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=model --value=1 --range=1.0E-10)
set_tests_properties (v3post_task_check_model_prior_1
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_prior_2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=model --value=1 --range=1.0)
set_tests_properties (v3post_task_check_model_prior_2
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_prior_3
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=model --value=15 --range=1.0)
set_tests_properties (v3post_task_check_model_prior_3
                      PROPERTIES
                      DEPENDS v3post_task_test)
add_test (NAME    v3post_task_check_model_prior_4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=model --value=1 --range=1.0)
set_tests_properties (v3post_task_check_model_prior_4
                      PROPERTIES
                      DEPENDS v3post_task_test)

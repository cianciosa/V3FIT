#  Run the reconstruction task. This just tests if the reconstruction executes correctly. 
#  Subsequent tests will examine the results of the reconstruction for correct 
#  results.

#  Copy input file from the source to the build directory.
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/thomson.dot
                ${CMAKE_CURRENT_BINARY_DIR}/thomson.dot
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/sxrem.dot
                ${CMAKE_CURRENT_BINARY_DIR}/sxrem.dot
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/intpol.dot
                ${CMAKE_CURRENT_BINARY_DIR}/intpol.dot
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/magnetic.dot
                ${CMAKE_CURRENT_BINARY_DIR}/magnetic.dot
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/v3rfun.in
                ${CMAKE_CURRENT_BINARY_DIR}/v3rfun.in
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.vmec
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.vmec
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.v3fit
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.v3fit
                COPYONLY)

add_test (NAME    reconstruction_task_test_v3rfun
          COMMAND $<TARGET_PROPERTY:xv3rfun,BINARY_DIR>/xv3rfun v3rfun.in)

add_test (NAME    reconstruction_task_test
          COMMAND $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit -para=-1)
set_tests_properties (reconstruction_task_test
                      PROPERTIES
                      DEPENDS reconstruction_task_test_v3rfun)

#  Since the reconstruction task test was run, Check the output files for correct values.

#  Check that the reconstruction parameter reconstructed to the correct values.
add_test (NAME    reconstruction_task_test_pp_ne_af
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_ne_af --value=1.0 --range=0.1 --index1=2)
set_tests_properties (reconstruction_task_test_pp_ne_af
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_am_aux_f
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=am_aux_f --value=1.0 --range=0.1 --index1=2)
set_tests_properties (reconstruction_task_test_pp_ne_af
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_pp_te_af
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_te_af --value=15.0 --range=0.1 --index1=2)
set_tests_properties (reconstruction_task_test_pp_te_af
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_pp_sxrem_af_a
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_sxrem_af_a --value=1.0 --range=0.1 --index1=1 --index2=1)
set_tests_properties (reconstruction_task_test_pp_sxrem_af_a
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_rbc00
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.75 --range=0.1 --index1=0 --index2=0)
set_tests_properties (reconstruction_task_test_rbc00
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_rbc01
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.25 --range=0.1 --index1=0 --index2=1)
set_tests_properties (reconstruction_task_test_rbc01
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_zbs01
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=zbs --value=0.25 --range=0.1 --index1=0 --index2=1)
set_tests_properties (reconstruction_task_test_zbs01
                      PROPERTIES
                      DEPENDS reconstruction_task_test)

#  Check that the signal effectiveness total.
add_test (NAME    reconstruction_task_test_sem_totals
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_sem_total.py --file=recout.input.test.v3fit)
set_tests_properties (reconstruction_task_test_sem_totals
                      PROPERTIES
                      DEPENDS reconstruction_task_test)

#  Check the jacobians for the the correct values.
add_test (NAME    reconstruction_task_test_jac_1-12:1-8
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=0 --ihigh=11 --jlow=0 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_1-12:1-8
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_13-16:1-1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=12 --ihigh=15 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_13-16:1-1
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_13-16:3-3
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=12 --ihigh=15 --jlow=2 --jhigh=2 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_13-16:3-3
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_13-16:7-7
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=12 --ihigh=15 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_13-16:7-7
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_17-28:3-4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=16 --ihigh=27 --jlow=2 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_17-28:3-4
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_21-28:8-8
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=20 --ihigh=27 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_21-28:8-8
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_29-32:1-1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=28 --ihigh=31 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_29-32:1-1
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_29-32:4-4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=28 --ihigh=31 --jlow=3 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_29-32:4-4
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_29-32:8-8
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=28 --ihigh=31 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_29-32:8-8
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_33-36:3-4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=32 --ihigh=35 --jlow=2 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_33-36:3-4
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_33-36:8-8
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=32 --ihigh=35 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_33-36:8-8
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_37-40:1-1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=36 --ihigh=39 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_37-40:1-1
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_37-40:3-4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=36 --ihigh=39 --jlow=2 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_37-40:3-4
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_37-40:8-8
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=36 --ihigh=39 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_37-40:8-8
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_1:2-8
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=40 --ihigh=40 --jlow=1 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_1:2-8
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_2:1-1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=41 --ihigh=41 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_2:1-1
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_2:3-8
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=41 --ihigh=41 --jlow=2 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_2:3-8
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_3:1-2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=42 --ihigh=42 --jlow=0 --jhigh=1 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_3:1-2
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_3:4-8
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=42 --ihigh=42 --jlow=3 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_3:4-8
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_4:1-3
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=43 --ihigh=43 --jlow=0 --jhigh=2 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_4:1-3
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_4:5-8
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=43 --ihigh=43 --jlow=4 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_4:5-8
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_5:1-4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=44 --ihigh=44 --jlow=0 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_5:1-4
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_5:6-8
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=44 --ihigh=44 --jlow=5 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_5:6-8
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_6:1-5
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=45 --ihigh=45 --jlow=0 --jhigh=4 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_6:1-5
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_6:7-8
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=45 --ihigh=45 --jlow=6 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_6:7-8
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_7:1-6
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=46 --ihigh=46 --jlow=0 --jhigh=5 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_7:1-6
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_7:8-8
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=46 --ihigh=46 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_7:8-8
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_8:1-2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=47 --ihigh=47 --jlow=0 --jhigh=1 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_8:1-2
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_8:4-8
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=47 --ihigh=47 --jlow=3 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_8:4-8
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_jac_prior_9:1-7
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=48 --ihigh=48 --jlow=0 --jhigh=6 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_9:1-7
                      PROPERTIES
                      DEPENDS reconstruction_task_test)

#  Check that the observed signal was written correctly.
add_test (NAME    reconstruction_task_test_check_observed_sxr001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr001 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_sxr001
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_sxr002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr002 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_sxr002
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_sxr003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr003 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_sxr003
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_sxr004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr004 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_sxr004
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_int001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int001 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_int001
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_int002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int002 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_int002
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_int003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int003 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_int003
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_int004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int004 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_int004
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_int005
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int005 --attribute=observed --value=0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int005
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_int006
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int006 --attribute=observed --value=0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int006
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_int007
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int007 --attribute=observed --value=0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int007
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_int008
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int008 --attribute=observed --value=0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int008
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_int009
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int009 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int009
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_int010
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int010 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int010
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_int011
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int011 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int011
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_int012
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int012 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int012
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_thm001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm001
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_thm002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm002
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_thm003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm003
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_thm004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm004
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_thm005
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm005
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_thm006
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm006
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_thm007
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm007
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_thm008
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm008
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_thm009
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=observed --value=400 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm009
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_thm010
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=observed --value=400 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm010
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_thm011
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=observed --value=400 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm011
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_thm012
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=observed --value=400 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm012
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_prior_1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=observed --value=1 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_1
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_prior_2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=observed --value=1 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_2
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_prior_3
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_3
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_prior_4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=observed --value=1 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_4
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_prior_5
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=observed --value=0.75 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_5
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_prior_6
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=observed --value=0.25 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_6
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_observed_prior_7
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=observed --value=0.25 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_7
                      PROPERTIES
                      DEPENDS reconstruction_task_test)

#  Check that the observed sigma was written correctly.
add_test (NAME    reconstruction_task_test_check_sigma_sxr001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr001 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_sxr001
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_sxr002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr002 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_sxr002
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_sxr003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr003 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_sxr003
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_sxr004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr004 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_sxr004
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_int001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int001 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_int001
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_int002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int002 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_int002
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_int003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int003 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_int003
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_int004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int004 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_int004
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_int005
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int005 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int005
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_int006
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int006 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int006
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_int007
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int007 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int007
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_int008
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int008 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int008
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_int009
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int009 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int009
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_int010
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int010 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int010
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_int011
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int011 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int011
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_int012
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int012 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int012
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_thm001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm001
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_thm002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm002
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_thm003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm003
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_thm004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm004
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_thm005
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm005
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_thm006
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm006
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_thm007
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm007
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_thm008
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm008
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_thm009
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=sigma --value=4 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm009
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_thm010
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=sigma --value=4 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm010
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_thm011
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=sigma --value=4 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm011
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_thm012
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=sigma --value=4 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm012
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_prior_1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_1
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_prior_2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_2
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_prior_3
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=sigma --value=1.50000E-01 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_3
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_prior_4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_4
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_prior_5
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=sigma --value=0.0001 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_5
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_prior_6
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=sigma --value=0.0001 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_6
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_sigma_prior_7
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=sigma --value=0.0001 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_7
                      PROPERTIES
                      DEPENDS reconstruction_task_test)

#  Check parameter constraint types for the correct values.
add_test (NAME    reconstruction_task_test_check_pp_ne_af_constraint_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_ne_af --index1=2 --range_l=value)
set_tests_properties (reconstruction_task_test_check_pp_ne_af_constraint_type
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_am_aux_f_constraint_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=am_aux_f --index1=2 --range_l=value)
set_tests_properties (reconstruction_task_test_check_am_aux_f_constraint_type
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_pp_te_af_constraint_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_te_af --index1=2 --range_l=value)
set_tests_properties (reconstruction_task_test_check_pp_te_af_constraint_type
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_pp_sxrem_af_a_constraint_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_sxrem_af_a --index1=1 --index2=1 --range_l=value)
set_tests_properties (reconstruction_task_test_check_pp_sxrem_af_a_constraint_type
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_rbc00_constraint_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=rbc --index1=0 --index2=0 --range_l=value --range_h=value)
set_tests_properties (reconstruction_task_test_check_rbc00_constraint_type
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_rbc01_constraint_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=rbc --index1=0 --index2=1 --range_l=value)
set_tests_properties (reconstruction_task_test_check_rbc01_constraint_type
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_zbs01_constraint_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=zbs --index1=0 --index2=1 --range_l=value)
set_tests_properties (reconstruction_task_test_check_zbs01_constraint_type
                      PROPERTIES
                      DEPENDS reconstruction_task_test)

#  Check correlation matrix values.
add_test (NAME reconstruction_task_test_check_reconstruction_correlation_matrix
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Reconstruction)
set_tests_properties (reconstruction_task_test_check_reconstruction_correlation_matrix
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME reconstruction_task_test_check_derived_correlation_matrix
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Derived)
set_tests_properties (reconstruction_task_test_check_derived_correlation_matrix
                      PROPERTIES
                      DEPENDS reconstruction_task_test)

#  Check point magnetic diagnostics values.
add_test (NAME    reconstruction_task_test_check_point1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=point1 --attribute=model --value=0.0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_point1
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_point2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=point2 --attribute=model --value=0.0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_point2
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_point3
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=point3 --attribute=model --value=0.0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_point3
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_point4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=point4 --attribute=model --value=0.0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_point4
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_point1_probe1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value2.py --file=recout.input.test.v3fit --s_name1=probe1 --s_name2=point1 --attribute=plasma --range=1.0E-5)
set_tests_properties (reconstruction_task_test_check_point1_probe1
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_point2_probe2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value2.py --file=recout.input.test.v3fit --s_name1=probe2 --s_name2=point2 --attribute=plasma --range=1.0E-5)
set_tests_properties (reconstruction_task_test_check_point2_probe2
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_point3_probe3
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value2.py --file=recout.input.test.v3fit --s_name1=probe3 --s_name2=point3 --attribute=plasma --range=1.0E-5)
set_tests_properties (reconstruction_task_test_check_point3_probe3
                      PROPERTIES
                      DEPENDS reconstruction_task_test)
add_test (NAME    reconstruction_task_test_check_point4_probe4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value2.py --file=recout.input.test.v3fit --s_name1=probe4 --s_name2=point4 --attribute=plasma --range=1.0E-5)
set_tests_properties (reconstruction_task_test_check_point4_probe4
                      PROPERTIES
                      DEPENDS reconstruction_task_test)

#  Test central differencing.

add_test (NAME    reconstruction_task_test_c_diff
          COMMAND $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit -para=-1 -c_diff)
set_tests_properties (reconstruction_task_test_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_v3rfun)

#  Since the reconstruction task test was run, Check the output files for correct values.

#  Check that the reconstruction parameter reconstructed to the correct values.
add_test (NAME    reconstruction_task_test_pp_ne_af_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_ne_af --value=1.0 --range=0.1 --index1=2)
set_tests_properties (reconstruction_task_test_pp_ne_af_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_am_aux_f_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=am_aux_f --value=1.0 --range=0.1 --index1=2)
set_tests_properties (reconstruction_task_test_pp_ne_af_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_pp_te_af_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_te_af --value=15.0 --range=0.1 --index1=2)
set_tests_properties (reconstruction_task_test_pp_te_af_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_pp_sxrem_af_a_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_sxrem_af_a --value=1.0 --range=0.1 --index1=1 --index2=1)
set_tests_properties (reconstruction_task_test_pp_sxrem_af_a_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_rbc00_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.75 --range=0.1 --index1=0 --index2=0)
set_tests_properties (reconstruction_task_test_rbc00_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_rbc01_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.25 --range=0.1 --index1=0 --index2=1)
set_tests_properties (reconstruction_task_test_rbc01_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_zbs01_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=zbs --value=0.25 --range=0.1 --index1=0 --index2=1)
set_tests_properties (reconstruction_task_test_zbs01_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)

#  Check that the signal effectiveness total.
add_test (NAME    reconstruction_task_test_sem_totals_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_sem_total.py --file=recout.input.test.v3fit)
set_tests_properties (reconstruction_task_test_sem_totals_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)

#  Check the jacobians for the the correct values.
add_test (NAME    reconstruction_task_test_jac_1-12:1-8_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=0 --ihigh=11 --jlow=0 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_1-12:1-8_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_13-16:1-1_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=12 --ihigh=15 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_13-16:1-1_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_13-16:3-3_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=12 --ihigh=15 --jlow=2 --jhigh=2 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_13-16:3-3_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_13-16:7-7_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=12 --ihigh=15 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_13-16:7-7_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_17-28:3-4_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=16 --ihigh=27 --jlow=2 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_17-28:3-4_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_21-28:8-8_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=20 --ihigh=27 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_21-28:8-8_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_29-32:1-1_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=28 --ihigh=31 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_29-32:1-1_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_29-32:4-4_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=28 --ihigh=31 --jlow=3 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_29-32:4-4_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_29-32:8-8_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=28 --ihigh=31 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_29-32:8-8_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_33-36:3-4_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=32 --ihigh=35 --jlow=2 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_33-36:3-4_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_33-36:8-8_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=32 --ihigh=35 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_33-36:8-8_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_37-40:1-1_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=36 --ihigh=39 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_37-40:1-1_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_37-40:3-4_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=36 --ihigh=39 --jlow=2 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_37-40:3-4_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_37-40:8-8_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=36 --ihigh=39 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_37-40:8-8_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_1:2-8_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=40 --ihigh=40 --jlow=1 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_1:2-8_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_2:1-1_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=41 --ihigh=41 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_2:1-1_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_2:3-8_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=41 --ihigh=41 --jlow=2 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_2:3-8_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_3:1-2_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=42 --ihigh=42 --jlow=0 --jhigh=1 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_3:1-2_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_3:4-8_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=42 --ihigh=42 --jlow=3 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_3:4-8_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_4:1-3_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=43 --ihigh=43 --jlow=0 --jhigh=2 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_4:1-3_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_4:5-8_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=43 --ihigh=43 --jlow=4 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_4:5-8_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_5:1-4_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=44 --ihigh=44 --jlow=0 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_5:1-4_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_5:6-8_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=44 --ihigh=44 --jlow=5 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_5:6-8_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_6:1-5_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=45 --ihigh=45 --jlow=0 --jhigh=4 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_6:1-5_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_6:7-8_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=45 --ihigh=45 --jlow=6 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_6:7-8_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_7:1-6_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=46 --ihigh=46 --jlow=0 --jhigh=5 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_7:1-6_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_7:8-8_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=46 --ihigh=46 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_7:8-8_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_8:1-2_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=47 --ihigh=47 --jlow=0 --jhigh=1 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_8:1-2_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_8:4-8_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=47 --ihigh=47 --jlow=3 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_8:4-8_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_jac_prior_9:1-7_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=48 --ihigh=48 --jlow=0 --jhigh=6 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_9:1-7_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)

#  Check that the observed signal was written correctly.
add_test (NAME    reconstruction_task_test_check_observed_sxr001_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr001 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_sxr001_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_sxr002_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr002 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_sxr002_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_sxr003_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr003 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_sxr003_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_sxr004_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr004 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_sxr004_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_int001_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int001 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_int001_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_int002_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int002 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_int002_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_int003_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int003 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_int003_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_int004_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int004 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_int004_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_int005_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int005 --attribute=observed --value=0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int005_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_int006_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int006 --attribute=observed --value=0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int006_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_int007_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int007 --attribute=observed --value=0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int007_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_int008_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int008 --attribute=observed --value=0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int008_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_int009_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int009 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int009_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_int010_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int010 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int010_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_int011_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int011 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int011_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_int012_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int012 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int012_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_thm001_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm001_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_thm002_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm002_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_thm003_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm003_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_thm004_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm004_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_thm005_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm005_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_thm006_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm006_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_thm007_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm007_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_thm008_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm008_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_thm009_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=observed --value=400 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm009_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_thm010_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=observed --value=400 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm010_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_thm011_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=observed --value=400 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm011_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_thm012_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=observed --value=400 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm012_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_prior_1_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=observed --value=1 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_1_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_prior_2_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=observed --value=1 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_2_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_prior_3_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_3_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_prior_4_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=observed --value=1 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_4_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_prior_5_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=observed --value=0.75 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_5_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_prior_6_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=observed --value=0.25 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_6_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_observed_prior_7_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=observed --value=0.25 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_7_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)

#  Check that the observed sigma was written correctly.
add_test (NAME    reconstruction_task_test_check_sigma_sxr001_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr001 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_sxr001_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_sxr002_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr002 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_sxr002_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_sxr003_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr003 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_sxr003_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_sxr004_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr004 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_sxr004_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_int001_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int001 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_int001_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_int002_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int002 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_int002_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_int003_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int003 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_int003_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_int004_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int004 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_int004_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_int005_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int005 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int005_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_int006_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int006 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int006_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_int007_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int007 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int007_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_int008_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int008 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int008_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_int009_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int009 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int009_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_int010_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int010 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int010_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_int011_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int011 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int011_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_int012_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int012 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int012_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_thm001_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm001_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_thm002_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm002_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_thm003_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm003_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_thm004_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm004_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_thm005_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm005_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_thm006_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm006_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_thm007_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm007_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_thm008_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm008_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_thm009_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=sigma --value=4 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm009_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_thm010_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=sigma --value=4 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm010_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_thm011_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=sigma --value=4 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm011_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_thm012_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=sigma --value=4 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm012_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_prior_1_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_1_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_prior_2_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_2_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_prior_3_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=sigma --value=1.50000E-01 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_3_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_prior_4_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_4_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_prior_5_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=sigma --value=0.0001 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_5_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_prior_6_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=sigma --value=0.0001 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_6_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_sigma_prior_7_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=sigma --value=0.0001 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_7_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)

#  Check parameter constraint types for the correct values.
add_test (NAME    reconstruction_task_test_check_pp_ne_af_constraint_type_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_ne_af --index1=2 --range_l=value)
set_tests_properties (reconstruction_task_test_check_pp_ne_af_constraint_type_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_am_aux_f_constraint_type_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=am_aux_f --index1=2 --range_l=value)
set_tests_properties (reconstruction_task_test_check_am_aux_f_constraint_type_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_pp_te_af_constraint_type_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_te_af --index1=2 --range_l=value)
set_tests_properties (reconstruction_task_test_check_pp_te_af_constraint_type_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_pp_sxrem_af_a_constraint_type_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_sxrem_af_a --index1=1 --index2=1 --range_l=value)
set_tests_properties (reconstruction_task_test_check_pp_sxrem_af_a_constraint_type_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_rbc00_constraint_type_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=rbc --index1=0 --index2=0 --range_l=value --range_h=value)
set_tests_properties (reconstruction_task_test_check_rbc00_constraint_type_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_rbc01_constraint_type_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=rbc --index1=0 --index2=1 --range_l=value)
set_tests_properties (reconstruction_task_test_check_rbc01_constraint_type_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_zbs01_constraint_type_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=zbs --index1=0 --index2=1 --range_l=value)
set_tests_properties (reconstruction_task_test_check_zbs01_constraint_type_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)

#  Check correlation matrix values.
add_test (NAME reconstruction_task_test_check_reconstruction_correlation_matrix_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Reconstruction)
set_tests_properties (reconstruction_task_test_check_reconstruction_correlation_matrix_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME reconstruction_task_test_check_derived_correlation_matrix_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Derived)
set_tests_properties (reconstruction_task_test_check_derived_correlation_matrix_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)

#  Check point magnetic diagnostics values.
add_test (NAME    reconstruction_task_test_check_point1_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=point1 --attribute=model --value=0.0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_point1_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_point2_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=point2 --attribute=model --value=0.0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_point2_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_point3_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=point3 --attribute=model --value=0.0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_point3_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_point4_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=point4 --attribute=model --value=0.0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_point4_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_point1_probe1_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value2.py --file=recout.input.test.v3fit --s_name1=probe1 --s_name2=point1 --attribute=plasma --range=1.0E-5)
set_tests_properties (reconstruction_task_test_check_point1_probe1_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_point2_probe2_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value2.py --file=recout.input.test.v3fit --s_name1=probe2 --s_name2=point2 --attribute=plasma --range=1.0E-5)
set_tests_properties (reconstruction_task_test_check_point2_probe2_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_point3_probe3_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value2.py --file=recout.input.test.v3fit --s_name1=probe3 --s_name2=point3 --attribute=plasma --range=1.0E-5)
set_tests_properties (reconstruction_task_test_check_point3_probe3_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)
add_test (NAME    reconstruction_task_test_check_point4_probe4_c_diff
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value2.py --file=recout.input.test.v3fit --s_name1=probe4 --s_name2=point4 --attribute=plasma --range=1.0E-5)
set_tests_properties (reconstruction_task_test_check_point4_probe4_c_diff
                      PROPERTIES
                      DEPENDS reconstruction_task_test_c_diff)

#  Rerun reconstruction to test that MPI parallel version works.
#  Run the reconstruction task. This just tests if the reconstruction executes correctly.
#  Subsequent tests will examine the results of the reconstruction for correct
#  results.
add_test (NAME    reconstruction_task_test_mpi
          COMMAND $<TARGET_PROPERTY:stell,MPIEXEC_EXECUTABLE> $<TARGET_PROPERTY:stell,MPIEXEC_NUMPROC_FLAG> $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS> $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit -para=-1)
set_tests_properties (reconstruction_task_test_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_v3rfun
                      PROCESSORS $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS>)

#  Since the reconstruction task test was run, Check the output files for correct values.

#  Check that the reconstruction parameter reconstructed to the correct values.
add_test (NAME    reconstruction_task_test_pp_ne_af_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_ne_af --value=1.0 --range=0.1 --index1=2)
set_tests_properties (reconstruction_task_test_pp_ne_af_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_am_aux_f_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=am_aux_f --value=1.0 --range=0.1 --index1=2)
set_tests_properties (reconstruction_task_test_am_aux_f_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_pp_te_af_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_te_af --value=15.0 --range=0.1 --index1=2)
set_tests_properties (reconstruction_task_test_pp_te_af_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_pp_sxrem_af_a_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_sxrem_af_a --value=1.0 --range=0.1 --index1=1 --index2=1)
set_tests_properties (reconstruction_task_test_pp_sxrem_af_a_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_rbc00_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.75 --range=0.1 --index1=0 --index2=0)
set_tests_properties (reconstruction_task_test_rbc00_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_rbc01_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.25 --range=0.1 --index1=0 --index2=1)
set_tests_properties (reconstruction_task_test_rbc01_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_zbs01_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=zbs --value=0.25 --range=0.1 --index1=0 --index2=1)
set_tests_properties (reconstruction_task_test_zbs01_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)

#  Check that the signal effectiveness total.
add_test (NAME    reconstruction_task_test_sem_totals_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_sem_total.py --file=recout.input.test.v3fit)
set_tests_properties (reconstruction_task_test_sem_totals_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)

#  Check the jacobians for the the correct values.
add_test (NAME    reconstruction_task_test_jac_1-12:1-8_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=0 --ihigh=11 --jlow=0 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_1-12:1-8_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_13-16:1-1_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=12 --ihigh=15 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_13-16:1-1_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_13-16:3-3_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=12 --ihigh=15 --jlow=2 --jhigh=2 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_13-16:3-3_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_13-16:7-7_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=12 --ihigh=15 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_13-16:7-7_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_17-28:3-4_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=16 --ihigh=27 --jlow=2 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_17-28:3-4_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_21-28:8-8_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=20 --ihigh=27 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_21-28:8-8_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_29-32:1-1_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=28 --ihigh=31 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_29-32:1-1_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_29-32:4-4_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=28 --ihigh=31 --jlow=3 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_29-32:4-4_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_29-32:8-8_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=28 --ihigh=31 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_29-32:8-8_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_33-36:3-4_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=32 --ihigh=35 --jlow=2 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_33-36:3-4_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_33-36:8-8_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=32 --ihigh=35 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_33-36:8-8_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_37-40:1-1_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=36 --ihigh=39 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_37-40:1-1_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_37-40:3-4_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=36 --ihigh=39 --jlow=2 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_37-40:3-4_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_37-40:8-8_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=36 --ihigh=39 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_37-40:8-8_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_1:2-8_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=40 --ihigh=40 --jlow=1 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_1:2-8_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_2:1-1_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=41 --ihigh=41 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_2:1-1_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_2:3-8_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=41 --ihigh=41 --jlow=2 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_2:3-8_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_3:1-2_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=42 --ihigh=42 --jlow=0 --jhigh=1 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_3:1-2_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_3:4-8_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=42 --ihigh=42 --jlow=3 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_3:4-8_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_4:1-3_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=43 --ihigh=43 --jlow=0 --jhigh=2 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_4:1-3_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_4:5-8_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=43 --ihigh=43 --jlow=4 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_4:5-8_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_5:1-4_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=44 --ihigh=44 --jlow=0 --jhigh=3 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_5:1-4_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_5:6-8_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=44 --ihigh=44 --jlow=5 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_5:6-8_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_6:1-5_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=45 --ihigh=45 --jlow=0 --jhigh=4 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_6:1-5_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_6:7-8_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=45 --ihigh=45 --jlow=6 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_6:7-8_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_7:1-6_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=46 --ihigh=46 --jlow=0 --jhigh=5 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_7:1-6_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_7:8-8_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=46 --ihigh=46 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_7:8-8_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_8:1-2_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=47 --ihigh=47 --jlow=0 --jhigh=1 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_8:1-2_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_8:4-8_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=47 --ihigh=47 --jlow=3 --jhigh=7 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_8:4-8_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_jac_prior_9:1-7_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=48 --ihigh=48 --jlow=0 --jhigh=6 --value=0.0)
set_tests_properties (reconstruction_task_test_jac_prior_9:1-7_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)

#  Check that the observed signal was written correctly.
add_test (NAME    reconstruction_task_test_check_observed_sxr001_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr001 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_sxr001_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_sxr002_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr002 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_sxr002_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_sxr003_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr003 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_sxr003_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_sxr004_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr004 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_sxr004_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_int001_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int001 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_int001_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_int002_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int002 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_int002_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_int003_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int003 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_int003_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_int004_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int004 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_int004_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_int005_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int005 --attribute=observed --value=0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int005_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_int006_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int006 --attribute=observed --value=0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int006_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_int007_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int007 --attribute=observed --value=0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int007_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_int008_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int008 --attribute=observed --value=0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int008_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_int009_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int009 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int009_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_int010_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int010 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int010_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_int011_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int011 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int011_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_int012_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int012 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_int012_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_thm001_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm001_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_thm002_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm002_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_thm003_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm003_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_thm004_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm004_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_thm005_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm005_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_thm006_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm006_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_thm007_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm007_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_thm008_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm008_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_thm009_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=observed --value=400 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_thm009_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_thm010_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=observed --value=400 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm010_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_thm011_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=observed --value=400 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm011_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_thm012_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=observed --value=400 --range=1.0)
set_tests_properties (reconstruction_task_test_check_observed_thm012_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_prior_1_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=observed --value=1 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_1_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_prior_2_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=observed --value=1 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_2_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_prior_3_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_3_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_prior_4_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=observed --value=1 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_4_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_prior_5_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=observed --value=0.75 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_5_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_prior_6_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=observed --value=0.25 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_6_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_observed_prior_7_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=observed --value=0.25 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_observed_prior_7_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)

#  Check that the observed sigma was written correctly.
add_test (NAME    reconstruction_task_test_check_sigma_sxr001_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr001 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_sxr001_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_sxr002_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr002 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_sxr002_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_sxr003_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr003 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_sxr003_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_sxr004_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr004 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_sxr004_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_int001_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int001 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_int001_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_int002_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int002 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_int002_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_int003_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int003 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_int003_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_int004_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int004 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_int004_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_int005_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int005 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int005_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_int006_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int006 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int006_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_int007_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int007 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int007_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_int008_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int008 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int008_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_int009_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int009 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int009_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_int010_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int010 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int010_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_int011_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int011 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int011_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_int012_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int012 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_int012_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_thm001_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm001_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_thm002_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm002_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_thm003_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm003_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_thm004_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm004_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_thm005_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm005_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_thm006_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm006_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_thm007_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm007_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_thm008_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm008_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_thm009_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=sigma --value=4 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_thm009_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_thm010_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=sigma --value=4 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm010_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_thm011_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=sigma --value=4 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm011_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_thm012_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=sigma --value=4 --range=1.0)
set_tests_properties (reconstruction_task_test_check_sigma_thm012_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_prior_1_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_1_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_prior_2_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_2_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_prior_3_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=sigma --value=1.50000E-01 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_3_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_prior_4_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_4_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_prior_5_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=sigma --value=0.0001 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_5_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_prior_6_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=sigma --value=0.0001 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_6_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_sigma_prior_7_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=sigma --value=0.0001 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_sigma_prior_7_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)

#  Check parameter constraint types for the correct values.
add_test (NAME    reconstruction_task_test_check_pp_ne_af_constraint_type_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_ne_af --index1=2 --range_l=value)
set_tests_properties (reconstruction_task_test_check_pp_ne_af_constraint_type_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_am_aux_f_constraint_type_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=am_aux_f --index1=2 --range_l=value)
set_tests_properties (reconstruction_task_test_check_am_aux_f_constraint_type_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_pp_te_af_constraint_type_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_te_af --index1=2 --range_l=value)
set_tests_properties (reconstruction_task_test_check_pp_te_af_constraint_type_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_pp_sxrem_af_a_constraint_type_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_sxrem_af_a --index1=1 --index2=1 --range_l=value)
set_tests_properties (reconstruction_task_test_check_pp_sxrem_af_a_constraint_type_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_rbc00_constraint_type_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=rbc --index1=0 --index2=0 --range_l=value --range_h=value)
set_tests_properties (reconstruction_task_test_check_rbc00_constraint_type_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_rbc01_constraint_type_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=rbc --index1=0 --index2=1 --range_l=value)
set_tests_properties (reconstruction_task_test_check_rbc01_constraint_type_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_zbs01_constraint_type_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=zbs --index1=0 --index2=1 --range_l=value)
set_tests_properties (reconstruction_task_test_check_zbs01_constraint_type_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)

#  Check correlation matrix values.
add_test (NAME reconstruction_task_test_check_reconstruction_correlation_matrix_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Reconstruction)
set_tests_properties (reconstruction_task_test_check_reconstruction_correlation_matrix_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME reconstruction_task_test_check_derived_correlation_matrix_mpi
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Derived)
set_tests_properties (reconstruction_task_test_check_derived_correlation_matrix_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)

#  Check point magnetic diagnostics values.
add_test (NAME    reconstruction_task_test_check_point1_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=point1 --attribute=model --value=0.0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_point1_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_point2_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=point2 --attribute=model --value=0.0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_point2_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_point3_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=point3 --attribute=model --value=0.0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_point3_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_point4_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=point4 --attribute=model --value=0.0 --range=1.0E-10)
set_tests_properties (reconstruction_task_test_check_point4_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_point1_probe1_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value2.py --file=recout.input.test.v3fit --s_name1=probe1 --s_name2=point1 --attribute=plasma --range=1.0E-5)
set_tests_properties (reconstruction_task_test_check_point1_probe1_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_point2_probe2_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value2.py --file=recout.input.test.v3fit --s_name1=probe2 --s_name2=point2 --attribute=plasma --range=1.0E-5)
set_tests_properties (reconstruction_task_test_check_point2_probe2_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_point3_probe3_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value2.py --file=recout.input.test.v3fit --s_name1=probe3 --s_name2=point3 --attribute=plasma --range=1.0E-5)
set_tests_properties (reconstruction_task_test_check_point3_probe3_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)
add_test (NAME    reconstruction_task_test_check_point4_probe4_mpi
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value2.py --file=recout.input.test.v3fit --s_name1=probe4 --s_name2=point4 --attribute=plasma --range=1.0E-5)
set_tests_properties (reconstruction_task_test_check_point4_probe4_mpi
                      PROPERTIES
                      DEPENDS reconstruction_task_test_mpi)

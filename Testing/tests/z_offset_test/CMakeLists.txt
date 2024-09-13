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
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.vmec
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.vmec
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.v3fit
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.v3fit
                COPYONLY)

add_test (NAME    z_offset_test
          COMMAND $<TARGET_PROPERTY:stell,MPIEXEC_EXECUTABLE> $<TARGET_PROPERTY:stell,MPIEXEC_NUMPROC_FLAG> $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS> $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit -para=-1)
set_tests_properties (z_offset_test
                      PROPERTIES
                      PROCESSORS $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS>)

#  Since the reconstruction task test was run, Check the output files for correct values.

#  Check that the reconstruction parameter reconstructed to the correct values.
add_test (NAME    z_offset_test_pp_ne_af
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_ne_af --value=1.0 --range=0.1 --index1=2)
set_tests_properties (z_offset_test_pp_ne_af
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_am_aux_f
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=am_aux_f --value=1.0 --range=0.1 --index1=2)
set_tests_properties (z_offset_test_am_aux_f
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_pp_te_af
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_te_af --value=15.0 --range=0.1 --index1=2)
set_tests_properties (z_offset_test_pp_te_af
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_pp_sxrem_af_a
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_sxrem_af_a --value=1.0 --range=0.1 --index1=1 --index2=1)
set_tests_properties (z_offset_test_pp_sxrem_af_a
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_rbc00
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.75 --range=0.1 --index1=0 --index2=0)
set_tests_properties (z_offset_test_rbc00
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_rbc01
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.25 --range=0.1 --index1=0 --index2=1)
set_tests_properties (z_offset_test_rbc01
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_zbs01
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=zbs --value=0.25 --range=0.1 --index1=0 --index2=1)
set_tests_properties (z_offset_test_zbs01
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_z_offset
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=z_offset --value=0.1 --range=0.001)
set_tests_properties (z_offset_test_z_offset
                      PROPERTIES
                      DEPENDS z_offset_test)

#  Check that the signal effectiveness total.
add_test (NAME    z_offset_test_sem_totals
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_sem_total.py --file=recout.input.test.v3fit)
set_tests_properties (z_offset_test_sem_totals
                      PROPERTIES
                      DEPENDS z_offset_test)

#  Check the jacobians for the the correct values.
add_test (NAME    z_offset_test_jac_1-4:1-1
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=0 --ihigh=3 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (z_offset_test_jac_1-4:1-1
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_1-4:3-3
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=0 --ihigh=3 --jlow=2 --jhigh=2 --value=0.0)
set_tests_properties (z_offset_test_jac_1-4:3-3
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_5-16:3-4
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=4 --ihigh=15 --jlow=2 --jhigh=3 --value=0.0)
set_tests_properties (z_offset_test_jac_5-16:3-4
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_17-20:1-1
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=16 --ihigh=19 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (z_offset_test_jac_17-20:1-1
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_17-20:4-4
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=16 --ihigh=19 --jlow=3 --jhigh=3 --value=0.0)
set_tests_properties (z_offset_test_jac_17-20:4-4
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_21-28:3-4
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=20 --ihigh=27 --jlow=2 --jhigh=3 --value=0.0)
set_tests_properties (z_offset_test_jac_21-28:3-4
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_25-28:1-1
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=24 --ihigh=27 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (z_offset_test_jac_25-28:1-1
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_prior_1
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=28 --ihigh=28 --jlow=1 --jhigh=3 --value=0.0)
set_tests_properties (z_offset_test_jac_prior_1
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_prior_2-1
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=29 --ihigh=29 --jlow=0 --jhigh=0 --value=0.0)
set_tests_properties (z_offset_test_jac_prior_2-1
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_prior_2-2
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=29 --ihigh=29 --jlow=2 --jhigh=3 --value=0.0)
set_tests_properties (z_offset_test_jac_prior_2-2
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_prior_3-1
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=30 --ihigh=30 --jlow=0 --jhigh=1 --value=0.0)
set_tests_properties (z_offset_test_jac_prior_3-1
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_prior_3-2
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=30 --ihigh=30 --jlow=3 --jhigh=3 --value=0.0)
set_tests_properties (z_offset_test_jac_prior_3-2
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_prior_4
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=31 --ihigh=31 --jlow=0 --jhigh=2 --value=0.0)
set_tests_properties (z_offset_test_jac_prior_4
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_prior_5-7:1-4
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=32 --ihigh=34 --jlow=0 --jhigh=3 --value=0.0)
set_tests_properties (z_offset_test_jac_prior_5-7:1-4
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_prior_1-4:5-7
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=28 --ihigh=31 --jlow=4 --jhigh=6 --value=0.0)
set_tests_properties (z_offset_test_jac_prior_1-4:5-7
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_prior_5:6-7
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=32 --ihigh=32 --jlow=5 --jhigh=6 --value=0.0)
set_tests_properties (z_offset_test_jac_prior_5:6-7
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_prior_6:5
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=33 --ihigh=33 --jlow=4 --jhigh=4 --value=0.0)
set_tests_properties (z_offset_test_jac_prior_6:5
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_prior_6:7
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=33 --ihigh=33 --jlow=6 --jhigh=6 --value=0.0)
set_tests_properties (z_offset_test_jac_prior_6:7
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_prior_7:5-6
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=34 --ihigh=34 --jlow=4 --jhigh=5 --value=0.0)
set_tests_properties (z_offset_test_jac_prior_7:5-6
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_prior_7:8
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=34 --ihigh=34 --jlow=7 --jhigh=7 --value=0.0)
set_tests_properties (z_offset_test_jac_prior_7:8
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_jac_prior_8:1-7
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=35 --ihigh=35 --jlow=0 --jhigh=6 --value=0.0)
set_tests_properties (z_offset_test_jac_prior_8:1-7
                      PROPERTIES
                      DEPENDS z_offset_test)

#  Check that the observed signal was written correctly.
add_test (NAME    z_offset_test_check_observed_sxr001
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr001 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_sxr001
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_sxr002
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr002 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_sxr002
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_sxr003
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr003 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_sxr003
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_sxr004
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr004 --attribute=observed --value=0.5 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_sxr004
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_int001
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int001 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (z_offset_test_check_observed_int001
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_int002
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int002 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (z_offset_test_check_observed_int002
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_int003
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int003 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (z_offset_test_check_observed_int003
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_int004
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int004 --attribute=observed --value=5.0E17 --range=1.0)
set_tests_properties (z_offset_test_check_observed_int004
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_int005
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int005 --attribute=observed --value=9.6E-7 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_int005
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_int006
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int006 --attribute=observed --value=9.6E-7 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_int006
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_int007
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int007 --attribute=observed --value=9.6E-7 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_int007
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_int008
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int008 --attribute=observed --value=9.6E-7 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_int008
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_int009
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int009 --attribute=observed --value=0.0 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_int009
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_int010
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int010 --attribute=observed --value=0.0 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_int010
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_int011
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int011 --attribute=observed --value=0.0 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_int011
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_int012
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int012 --attribute=observed --value=0.0 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_int012
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_thm001
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_thm001
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_thm002
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_thm002
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_thm003
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_thm003
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_thm004
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_thm004
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_thm005
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (z_offset_test_check_observed_thm005
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_thm006
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (z_offset_test_check_observed_thm006
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_thm007
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (z_offset_test_check_observed_thm007
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_thm008
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=observed --value=1.0E18 --range=1.0)
set_tests_properties (z_offset_test_check_observed_thm008
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_thm009
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=observed --value=400 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_thm009
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_thm010
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=observed --value=400 --range=1.0)
set_tests_properties (z_offset_test_check_observed_thm010
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_thm011
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=observed --value=400 --range=1.0)
set_tests_properties (z_offset_test_check_observed_thm011
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_thm012
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=observed --value=400 --range=1.0)
set_tests_properties (z_offset_test_check_observed_thm012
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_prior_1
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=observed --value=1 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_prior_1
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_prior_2
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=observed --value=1 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_prior_2
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_prior_3
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=observed --value=15 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_prior_3
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_prior_4
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=observed --value=1 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_prior_4
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_prior_5
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=observed --value=0.75 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_prior_5
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_prior_6
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=observed --value=0.25 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_prior_6
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_prior_7
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=observed --value=0.25 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_prior_7
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_observed_prior_8
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_8 --attribute=observed --value=0.1 --range=1.0E-10)
set_tests_properties (z_offset_test_check_observed_prior_8
                      PROPERTIES
                      DEPENDS z_offset_test)

#  Check that the observed sigma was written correctly.
add_test (NAME    z_offset_test_check_sigma_sxr001
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr001 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_sxr001
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_sxr002
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr002 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_sxr002
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_sxr003
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr003 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_sxr003
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_sxr004
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr004 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_sxr004
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_int001
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int001 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (z_offset_test_check_sigma_int001
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_int002
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int002 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (z_offset_test_check_sigma_int002
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_int003
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int003 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (z_offset_test_check_sigma_int003
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_int004
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int004 --attribute=sigma --value=5.00000E+15 --range=1.0)
set_tests_properties (z_offset_test_check_sigma_int004
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_int005
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int005 --attribute=sigma --value=9.6E-9 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_int005
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_int006
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int006 --attribute=sigma --value=9.6E-9 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_int006
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_int007
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int007 --attribute=sigma --value=9.6E-9 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_int007
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_int008
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int008 --attribute=sigma --value=9.6E-9 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_int008
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_int009
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int009 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_int009
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_int010
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int010 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_int010
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_int011
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int011 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_int011
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_int012
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int012 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_int012
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_thm001
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_thm001
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_thm002
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_thm002
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_thm003
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_thm003
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_thm004
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_thm004
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_thm005
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (z_offset_test_check_sigma_thm005
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_thm006
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (z_offset_test_check_sigma_thm006
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_thm007
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (z_offset_test_check_sigma_thm007
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_thm008
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=sigma --value=1.0E16 --range=1.0)
set_tests_properties (z_offset_test_check_sigma_thm008
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_thm009
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=sigma --value=4 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_thm009
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_thm010
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=sigma --value=4 --range=1.0)
set_tests_properties (z_offset_test_check_sigma_thm010
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_thm011
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=sigma --value=4 --range=1.0)
set_tests_properties (z_offset_test_check_sigma_thm011
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_thm012
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=sigma --value=4 --range=1.0)
set_tests_properties (z_offset_test_check_sigma_thm012
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_prior_1
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_prior_1
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_prior_2
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_prior_2
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_prior_3
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=sigma --value=1.50000E-01 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_prior_3
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_prior_4
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_prior_4
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_prior_5
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=sigma --value=0.0001 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_prior_5
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_prior_6
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=sigma --value=0.0001 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_prior_6
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_prior_7
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=sigma --value=0.0001 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_prior_7
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_sigma_prior_8
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_8 --attribute=sigma --value=0.0001 --range=1.0E-10)
set_tests_properties (z_offset_test_check_sigma_prior_8
                      PROPERTIES
                      DEPENDS z_offset_test)

#  Check parameter constraint types for the correct values.
add_test (NAME    z_offset_test_check_pp_ne_af_constraint_type
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_ne_af --index1=2 --range_l=value)
set_tests_properties (z_offset_test_check_pp_ne_af_constraint_type
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_am_aux_f_constraint_type
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=am_aux_f --index1=2 --range_l=value)
set_tests_properties (z_offset_test_check_am_aux_f_constraint_type
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_pp_te_af_constraint_type
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_te_af --index1=2 --range_l=value)
set_tests_properties (z_offset_test_check_pp_te_af_constraint_type
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_pp_sxrem_af_a_constraint_type
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_sxrem_af_a --index1=1 --index2=1 --range_l=value)
set_tests_properties (z_offset_test_check_pp_sxrem_af_a_constraint_type
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_rbc00_constraint_type
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=rbc --index1=0 --index2=0 --range_l=value --range_h=value)
set_tests_properties (z_offset_test_check_rbc00_constraint_type
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_rbc01_constraint_type
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=rbc --index1=0 --index2=1 --range_l=value)
set_tests_properties (z_offset_test_check_rbc01_constraint_type
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME    z_offset_test_check_zbs01_constraint_type
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=zbs --index1=0 --index2=1 --range_l=value)
set_tests_properties (z_offset_test_check_zbs01_constraint_type
                      PROPERTIES
                      DEPENDS z_offset_test)

#  Check correlation matrix values.
add_test (NAME z_offset_test_check_reconstruction_correlation_matrix
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Reconstruction)
set_tests_properties (z_offset_test_check_reconstruction_correlation_matrix
                      PROPERTIES
                      DEPENDS z_offset_test)
add_test (NAME z_offset_test_check_derived_correlation_matrix
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Derived)
set_tests_properties (z_offset_test_check_derived_correlation_matrix
                      PROPERTIES
                      DEPENDS z_offset_test)

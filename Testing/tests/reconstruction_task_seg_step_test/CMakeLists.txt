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

add_test (NAME    reconstruction_task_seg_step_test
          COMMAND $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit -para=-1)

#  Since the reconstruction task test was run, Check the output files for correct values.

#  Check that the reconstruction parameter reconstructed to the correct values.
add_test (NAME    reconstruction_task_seg_step_test_pp_ne_af
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_ne_af --value=1.0 --range=0.1 --index1=2)
add_test (NAME    reconstruction_task_seg_step_test_am_aux_f
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=am_aux_f --value=1.0 --range=0.1 --index1=2)
add_test (NAME    reconstruction_task_seg_step_test_pp_te_af
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_te_af --value=15.0 --range=0.1 --index1=2)
add_test (NAME    reconstruction_task_seg_step_test_pp_sxrem_af_a
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_sxrem_af_a --value=1.0 --range=0.1 --index1=1 --index2=1)
add_test (NAME    reconstruction_task_seg_step_test_rbc00
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.75 --range=0.1 --index1=0 --index2=0)
add_test (NAME    reconstruction_task_seg_step_test_rbc01
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.25 --range=0.1 --index1=0 --index2=1)
add_test (NAME    reconstruction_task_seg_step_test_zbs01
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=zbs --value=0.25 --range=0.1 --index1=0 --index2=1)

#  Check that the signal effectiveness total.
add_test (NAME    reconstruction_task_seg_step_test_sem_totals
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_sem_total.py --file=recout.input.test.v3fit)

#  Check the jacobians for the the correct values.
add_test (NAME    reconstruction_task_seg_step_test_jac_1-4:1-1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=0 --ihigh=3 --jlow=0 --jhigh=0 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_1-4:3-3
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=0 --ihigh=3 --jlow=2 --jhigh=2 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_5-16:3-4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=4 --ihigh=15 --jlow=2 --jhigh=3 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_17-20:1-1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=16 --ihigh=19 --jlow=0 --jhigh=0 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_17-20:4-4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=16 --ihigh=19 --jlow=3 --jhigh=3 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_21-28:3-4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=20 --ihigh=27 --jlow=2 --jhigh=3 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_25-28:1-1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=24 --ihigh=27 --jlow=0 --jhigh=0 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=28 --ihigh=28 --jlow=1 --jhigh=3 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_2-1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=29 --ihigh=29 --jlow=0 --jhigh=0 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_2-2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=29 --ihigh=29 --jlow=2 --jhigh=3 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_3-1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=30 --ihigh=30 --jlow=0 --jhigh=1 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_3-2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=30 --ihigh=30 --jlow=3 --jhigh=3 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=31 --ihigh=31 --jlow=0 --jhigh=2 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_5:6-7
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=32 --ihigh=32 --jlow=5 --jhigh=6 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_6:5
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=33 --ihigh=33 --jlow=4 --jhigh=4 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_6:7
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=33 --ihigh=33 --jlow=6 --jhigh=6 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_7:5-6
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=34 --ihigh=34 --jlow=4 --jhigh=5 --value=0.0)

#  Check that the observed signal was written correctly.
add_test (NAME    reconstruction_task_seg_step_test_check_observed_sxr001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr001 --attribute=observed --value=0.5 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_sxr002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr002 --attribute=observed --value=0.5 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_sxr003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr003 --attribute=observed --value=0.5 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_sxr004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr004 --attribute=observed --value=0.5 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int001 --attribute=observed --value=5.0E17 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int002 --attribute=observed --value=5.0E17 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int003 --attribute=observed --value=5.0E17 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int004 --attribute=observed --value=5.0E17 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int005
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int005 --attribute=observed --value=0 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int006
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int006 --attribute=observed --value=0 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int007
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int007 --attribute=observed --value=0 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int008
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int008 --attribute=observed --value=0 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int009
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int009 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int010
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int010 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int011
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int011 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int012
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int012 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=observed --value=15 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=observed --value=15 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=observed --value=15 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=observed --value=15 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm005
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=observed --value=1.0E18 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm006
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=observed --value=1.0E18 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm007
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=observed --value=1.0E18 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm008
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=observed --value=1.0E18 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm009
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=observed --value=400 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm010
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=observed --value=400 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm011
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=observed --value=400 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm012
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=observed --value=400 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_prior_1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=observed --value=1 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_prior_2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=observed --value=1 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_prior_3
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=observed --value=15 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_prior_4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=observed --value=1 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_prior_5
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=observed --value=0.75 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_prior_6
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=observed --value=0.25 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_prior_7
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=observed --value=0.25 --range=1.0E-10)

#  Check that the observed sigma was written correctly.
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_sxr001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr001 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_sxr002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr002 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_sxr003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr003 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_sxr004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr004 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int001 --attribute=sigma --value=5.00000E+15 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int002 --attribute=sigma --value=5.00000E+15 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int003 --attribute=sigma --value=5.00000E+15 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int004 --attribute=sigma --value=5.00000E+15 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int005
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int005 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int006
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int006 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int007
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int007 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int008
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int008 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int009
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int009 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int010
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int010 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int011
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int011 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int012
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int012 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm001
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm002
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm003
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm004
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm005
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=sigma --value=1.0E16 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm006
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=sigma --value=1.0E16 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm007
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=sigma --value=1.0E16 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm008
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=sigma --value=1.0E16 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm009
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=sigma --value=4 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm010
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=sigma --value=4 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm011
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=sigma --value=4 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm012
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=sigma --value=4 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_prior_1
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_prior_2
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_prior_3
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=sigma --value=1.50000E-01 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_prior_4
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_prior_5
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=sigma --value=0.0001 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_prior_6
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=sigma --value=0.0001 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_prior_7
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=sigma --value=0.0001 --range=1.0E-10)

#  Check parameter constraint types for the correct values.
add_test (NAME    reconstruction_task_seg_step_test_check_pp_ne_af_constraint_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_ne_af --index1=2 --range_l=value)
add_test (NAME    reconstruction_task_seg_step_test_check_am_aux_f_constraint_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=am_aux_f --index1=2 --range_l=value)
add_test (NAME    reconstruction_task_seg_step_test_check_pp_te_af_constraint_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_te_af --index1=2 --range_l=value)
add_test (NAME    reconstruction_task_seg_step_test_check_pp_sxrem_af_a_constraint_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_sxrem_af_a --index1=1 --index2=1 --range_l=value)
add_test (NAME    reconstruction_task_seg_step_test_check_rbc00_constraint_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=rbc --index1=0 --index2=0 --range_l=value --range_h=value)
add_test (NAME    reconstruction_task_seg_step_test_check_rbc01_constraint_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=rbc --index1=0 --index2=1 --range_l=value)
add_test (NAME    reconstruction_task_seg_step_test_check_zbs01_constraint_type
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=zbs --index1=0 --index2=1 --range_l=value)
          
#  Check correlation matrix values.
add_test (NAME reconstruction_task_seg_step_test_check_reconstruction_correlation_matrix
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Reconstruction)

#  Rerun reconstruction to test that MPI parvmec version works.

#  Run the reconstruction task. This just tests if the reconstruction executes correctly.
#  Subsequent tests will examine the results of the reconstruction for correct
#  results.
    add_test (NAME    reconstruction_task_seg_step_test_parvmec
              COMMAND $<TARGET_PROPERTY:stell,MPIEXEC_EXECUTABLE> $<TARGET_PROPERTY:stell,MPIEXEC_NUMPROC_FLAG> $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS> $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit -para=-1)
#  Since the reconstruction task test was run, Check the output files for correct values.

#  Check that the reconstruction parameter reconstructed to the correct values.
add_test (NAME    reconstruction_task_seg_step_test_pp_ne_af_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_ne_af --value=1.0 --range=0.1 --index1=2)
add_test (NAME    reconstruction_task_seg_step_test_am_aux_f_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=am_aux_f --value=1.0 --range=0.1 --index1=2)
add_test (NAME    reconstruction_task_seg_step_test_pp_te_af_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_te_af --value=15.0 --range=0.1 --index1=2)
add_test (NAME    reconstruction_task_seg_step_test_pp_sxrem_af_a_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=pp_sxrem_af_a --value=1.0 --range=0.1 --index1=1 --index2=1)
add_test (NAME    reconstruction_task_seg_step_test_rbc00_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.75 --range=0.1 --index1=0 --index2=0)
add_test (NAME    reconstruction_task_seg_step_test_rbc01_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.25 --range=0.1 --index1=0 --index2=1)
add_test (NAME    reconstruction_task_seg_step_test_zbs01_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=zbs --value=0.25 --range=0.1 --index1=0 --index2=1)

#  Check that the signal effectiveness total.
add_test (NAME    reconstruction_task_seg_step_test_sem_totals_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_sem_total.py --file=recout.input.test.v3fit)

#  Check the jacobians for the the correct values.
add_test (NAME    reconstruction_task_seg_step_test_jac_1-4:1-1_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=0 --ihigh=3 --jlow=0 --jhigh=0 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_1-4:3-3_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=0 --ihigh=3 --jlow=2 --jhigh=2 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_5-16:3-4_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=4 --ihigh=15 --jlow=2 --jhigh=3 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_17-20:1-1_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=16 --ihigh=19 --jlow=0 --jhigh=0 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_17-20:4-4_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=16 --ihigh=19 --jlow=3 --jhigh=3 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_21-28:3-4_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=20 --ihigh=27 --jlow=2 --jhigh=3 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_25-28:1-1_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=24 --ihigh=27 --jlow=0 --jhigh=0 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_1_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=28 --ihigh=28 --jlow=1 --jhigh=3 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_2-1_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=29 --ihigh=29 --jlow=0 --jhigh=0 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_2-2_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=29 --ihigh=29 --jlow=2 --jhigh=3 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_3-1_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=30 --ihigh=30 --jlow=0 --jhigh=1 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_3-2_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=30 --ihigh=30 --jlow=3 --jhigh=3 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_4_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=31 --ihigh=31 --jlow=0 --jhigh=2 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_5-7:1-4_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=32 --ihigh=34 --jlow=0 --jhigh=3 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_1-4:5-7_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=28 --ihigh=31 --jlow=4 --jhigh=6 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_5:6-7_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=32 --ihigh=32 --jlow=5 --jhigh=6 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_6:5_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=33 --ihigh=33 --jlow=4 --jhigh=4 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_6:7_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=33 --ihigh=33 --jlow=6 --jhigh=6 --value=0.0)
add_test (NAME    reconstruction_task_seg_step_test_jac_prior_7:5-6_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_jac_value.py --file=runlog.input.test.v3fit --ilow=34 --ihigh=34 --jlow=4 --jhigh=5 --value=0.0)

#  Check that the observed signal was written correctly.
add_test (NAME    reconstruction_task_seg_step_test_check_observed_sxr001_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr001 --attribute=observed --value=0.5 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_sxr002_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr002 --attribute=observed --value=0.5 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_sxr003_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr003 --attribute=observed --value=0.5 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_sxr004_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr004 --attribute=observed --value=0.5 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int001_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int001 --attribute=observed --value=5.0E17 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int002_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int002 --attribute=observed --value=5.0E17 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int003_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int003 --attribute=observed --value=5.0E17 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int004_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int004 --attribute=observed --value=5.0E17 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int005_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int005 --attribute=observed --value=0 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int006_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int006 --attribute=observed --value=0 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int007_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int007 --attribute=observed --value=0 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int008_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int008 --attribute=observed --value=0 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int009_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int009 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int010_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int010 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int011_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int011 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_int012_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int012 --attribute=observed --value=-5.52983E-05 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm001_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=observed --value=15 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm002_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=observed --value=15 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm003_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=observed --value=15 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm004_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=observed --value=15 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm005_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=observed --value=1.0E18 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm006_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=observed --value=1.0E18 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm007_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=observed --value=1.0E18 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm008_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=observed --value=1.0E18 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm009_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=observed --value=400 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm010_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=observed --value=400 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm011_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=observed --value=400 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_thm012_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=observed --value=400 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_prior_1_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=observed --value=1 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_prior_2_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=observed --value=1 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_prior_3_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=observed --value=15 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_prior_4_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=observed --value=1 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_prior_5_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=observed --value=0.75 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_prior_6_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=observed --value=0.25 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_observed_prior_7_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=observed --value=0.25 --range=1.0E-10)

#  Check that the observed sigma was written correctly.
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_sxr001_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr001 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_sxr002_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr002 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_sxr003_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr003 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_sxr004_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=sxr004 --attribute=sigma --value=5.00000E-03 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int001_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int001 --attribute=sigma --value=5.00000E+15 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int002_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int002 --attribute=sigma --value=5.00000E+15 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int003_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int003 --attribute=sigma --value=5.00000E+15 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int004_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int004 --attribute=sigma --value=5.00000E+15 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int005_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int005 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int006_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int006 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int007_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int007 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int008_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int008 --attribute=sigma --value=1.00000E-10 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int009_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int009 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int010_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int010 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int011_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int011 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_int012_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=int012 --attribute=sigma --value=5.52983E-07 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm001_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm001 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm002_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm002 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm003_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm003 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm004_parvmec
          COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm004 --attribute=sigma --value=1.50000E-02 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm005_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm005 --attribute=sigma --value=1.0E16 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm006_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm006 --attribute=sigma --value=1.0E16 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm007_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm007 --attribute=sigma --value=1.0E16 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm008_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm008 --attribute=sigma --value=1.0E16 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm009_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm009 --attribute=sigma --value=4 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm010_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm010 --attribute=sigma --value=4 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm011_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm011 --attribute=sigma --value=4 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_thm012_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=thm012 --attribute=sigma --value=4 --range=1.0)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_prior_1_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_1 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_prior_2_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_2 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_prior_3_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_3 --attribute=sigma --value=1.50000E-01 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_prior_4_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_4 --attribute=sigma --value=1.00000E-02 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_prior_5_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_5 --attribute=sigma --value=0.0001 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_prior_6_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_6 --attribute=sigma --value=0.0001 --range=1.0E-10)
add_test (NAME    reconstruction_task_seg_step_test_check_sigma_prior_7_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_signal_value.py --file=recout.input.test.v3fit --s_name=prior_7 --attribute=sigma --value=0.0001 --range=1.0E-10)

#  Check parameter constraint types for the correct values.
add_test (NAME    reconstruction_task_seg_step_test_check_pp_ne_af_constraint_type_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_ne_af --index1=2 --range_l=value)
add_test (NAME    reconstruction_task_seg_step_test_check_am_aux_f_constraint_type_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=am_aux_f --index1=2 --range_l=value)
add_test (NAME    reconstruction_task_seg_step_test_check_pp_te_af_constraint_type_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_te_af --index1=2 --range_l=value)
add_test (NAME    reconstruction_task_seg_step_test_check_pp_sxrem_af_a_constraint_type_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=pp_sxrem_af_a --index1=1 --index2=1 --range_l=value)
add_test (NAME    reconstruction_task_seg_step_test_check_rbc00_constraint_type_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=rbc --index1=0 --index2=0 --range_l=value --range_h=value)
add_test (NAME    reconstruction_task_seg_step_test_check_rbc01_constraint_type_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=rbc --index1=0 --index2=1 --range_l=value)
add_test (NAME    reconstruction_task_seg_step_test_check_zbs01_constraint_type_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_constraint_types.py --file=recout.input.test.v3fit --p_type=zbs --index1=0 --index2=1 --range_l=value)

#  Check correlation matrix values.
add_test (NAME reconstruction_task_seg_step_test_check_reconstruction_correlation_matrix_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Reconstruction)
add_test (NAME reconstruction_task_seg_step_test_check_derived_correlation_matrix_parvmec
       	  COMMAND ${PYTHON_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_correlation_matrix.py --file=recout.input.test.v3fit --param=Derived)

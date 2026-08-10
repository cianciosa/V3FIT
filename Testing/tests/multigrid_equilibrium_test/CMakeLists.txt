#  Run the multigrid equilibrium task. This just tests if the task executes
#  correctly. Subsequent tests will examine the results of the reconstruction
#  for correct results.

configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.vmec
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.vmec
                COPYONLY)
configure_file (${CMAKE_CURRENT_SOURCE_DIR}/input.test.v3fit
                ${CMAKE_CURRENT_BINARY_DIR}/input.test.v3fit
                COPYONLY)

add_test (NAME    multigrid_equilibrium_test
          COMMAND $<TARGET_PROPERTY:stell,MPIEXEC_EXECUTABLE> $<TARGET_PROPERTY:stell,MPIEXEC_NUMPROC_FLAG> $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS> $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit input.test.v3fit)
set_tests_properties (multigrid_equilibrium_test
                      PROPERTIES
                      PROCESSORS $<TARGET_PROPERTY:stell,MPIEXEC_MAX_NUMPROCS>)

#  Since the reconstruction task test was run, Check the output files for correct values.
add_test (NAME    multigrid_equilibirum_test_rbc00
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=1.0 --range=0.00001 --index1=0 --index2=0)
set_tests_properties (multigrid_equilibirum_test_rbc00
                      PROPERTIES
                      DEPENDS multigrid_equilibrium_test)
add_test (NAME    multigrid_equilibirum_test_rbc01
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.4 --range=0.00001 --index1=0 --index2=1)
set_tests_properties (multigrid_equilibirum_test_rbc01
                      PROPERTIES
                      DEPENDS multigrid_equilibrium_test)
add_test (NAME    multigrid_equilibirum_test_zbs01
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=zbs --value=3.06E-1 --range=0.00001 --index1=0 --index2=1)
set_tests_properties (multigrid_equilibirum_test_zbs01
                      PROPERTIES
                      DEPENDS multigrid_equilibrium_test)
add_test (NAME    multigrid_equilibirum_test_rbc02
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=5.872E-1 --range=0.00001 --index1=0 --index2=2)
set_tests_properties (multigrid_equilibirum_test_rbc02
                      PROPERTIES
                      DEPENDS multigrid_equilibrium_test)
add_test (NAME    multigrid_equilibirum_test_zbs02
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=zbs --value=3.4E-2 --range=0.00001 --index1=0 --index2=2)
set_tests_properties (multigrid_equilibirum_test_zbs02
                      PROPERTIES
                      DEPENDS multigrid_equilibrium_test)
add_test (NAME    multigrid_equilibirum_test_rbc03
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=rbc --value=0.0 --range=0.00001 --index1=0 --index2=3)
set_tests_properties (multigrid_equilibirum_test_rbc03
                      PROPERTIES
                      DEPENDS multigrid_equilibrium_test)
add_test (NAME    multigrid_equilibirum_test_zbs03
          COMMAND ${Python3_EXECUTABLE} ${TEST_SCRIPT_DIR}/check_param_value.py --file=recout.input.test.v3fit --p_type=zbs --value=4.4E-2 --range=0.00001 --index1=0 --index2=3)
set_tests_properties (multigrid_equilibirum_test_zbs03
                      PROPERTIES
                      DEPENDS multigrid_equilibrium_test)

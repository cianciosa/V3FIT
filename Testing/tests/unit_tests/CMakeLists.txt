#  Run the built in unit test task of v3fit.
add_test (NAME    unit_tests 
          COMMAND $<TARGET_PROPERTY:xv3fit,BINARY_DIR>/xv3fit -test)

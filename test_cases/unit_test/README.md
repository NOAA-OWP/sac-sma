# Unit testing instructions

1. Follow the instructions in INSTALL.md to install the program.
2. Change directory to the test directory: `cd snow17/test_cases/unit_test`
3. Run `make` to compile the unit testing program and generate the executable, which is `snow17/test_cases/unit_test/snow17_driver_test.exe`
4. Run `make test` to run the unit tests using the example test case in `snow17/test_cases`. 
5. Steps 3 and 4 can be combined into one by issuing the command `make test` under `snow17/test_cases/unit_test`.
6. Check the screen print out to see if the tests pass. 

Note that a `gfortran` compiler is assumed here. To change it to another compiler, modify the `F90` variable in the `Makefile` and corresponding `F90FLAGS` and `CPPFLAGS`.

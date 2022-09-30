# Installation instructions

Detailed instructions on how to install, configure, and get the project running.

### Compilation
 * the model has been tested with the gfortran compiler
 * cd to the build/ directory.  Copy the Makefile to Makefile.local.  Edit Makefile.local to recognize your system libraries and compiler settings.
 * build the model with 'make -f Makefile.local'.  Makefile.local will not be stored in the repo. 
 * this installs a binary executable in the bin/ directory

### Running a test case
 * cd to test_cases/ex1/
 * tar -xzvf ex1.tgz
 * cd further to the run/ directory
 * run the model:  ../../bin/sac.exe namelist.bmi.HHWM8
 * outputs will be in test_cases/ex1/output/ and test_cases/ex1/state/
 * the expanded test case will be ignored by git




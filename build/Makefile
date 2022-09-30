#========================================================================
# Makefile to compile the Sac and driver code for multi-zone model
#========================================================================

#========================================================================
# PART 1: Define directory paths and compiler

# Define main/top directory (parent of /build/)
# Note:  the relative path of "../" drops into sac parent directory
F_MASTER_DIR = ../

# Location of the compiled modules
# MOD_PATH = $(F_MASTER_DIR)/build/ -- not used

# Define directories
driver_dir = $(F_MASTER_DIR)/src/driver/
sac_dir    = $(F_MASTER_DIR)/src/sac/
share_dir  = $(F_MASTER_DIR)/src/share/
bmi_dir    = $(F_MASTER_DIR)/src/bmi/
exe_dir    = $(F_MASTER_DIR)/bin/

# Define the executable and path
EXE = $(exe_dir)/sac.exe

# Define your Fortran compiler
#FC  = pgf90
#FC  = ifort
FC   = gfortran

#========================================================================
# PART 2: Define the libraries and flags (can be an include file)

# --- Define the libraries and path to include files
ifeq "$(FC)" "pgf90"
 LOCL_PATH = /usr/local
 LIB = -L$(LOCL_PATH)/lib 
 INC = -I ./
endif

ifeq "$(FC)" "gfortran"
 LOCL_PATH = /usr/local
 LIB = -L$(LOCL_PATH)/lib 
 INC = -I ../
endif

# --- Define flags
ifeq "$(FC)" "gfortran"
  FLAGS_DEBUG  = -static -Wall -g -ffree-line-length-none -cpp -fcheck=all
  FLAGS_DEBUG77 = -static -std=legacy -Wall -g -ffree-line-length-none -cpp -fcheck=all
  FLAGS = -O3 -fdefault-real-8 -fno-align-commons -ffree-line-length-none -cpp -fcheck=all
  FLAGS77 = -O3 -fdefault-real-8 -fno-align-commons -ffree-line-length-none -cpp -fcheck=all
endif

ifeq "$(FC)" "ifort"
  FLAGS_PROF = -static -debug -warn all -check all -FR -O0 -auto -WB -traceback -g -fltconsistency -fpe0
  FLAGS = -O3 -warn all -check all
  FLAGS77 = -O3 -f77rtl 
endif

ifeq "$(FC)" "pgf90"
  FLAGS_PROF = -Bstatic -Mbackslash -g -Mchkptr -Mchkstk -Mpgicoff -Minform=inform -Ktrap=divz,inv -Mprof=lines,time
  # FLAGS = -Mfreeform -O3 -Mbackslash -g -Mchkptr -Mchkstk -Mpgicoff -Minform=inform -Ktrap=divz,inv
  FLAGS = -O3 -r8 -Kieee
  FLAGS77 = -O3 -r8
endif


#========================================================================
# PART 3: Assemble all of the various subroutines

# --- sac code (fortran 90, different options)
sac = \
		ex_sac1.f \
		sac1.f 
#		duamel.f \
		
model_sac = $(patsubst %, $(sac_dir)/%, $(sac))


# -- share utilities and code used by the driver in running the model
run_util = \
	nrtype.f90 \
	constants.f90 \
	namelistModule.f90 \
        parametersType.f90 \
        forcingType.f90 \
        dateTimeUtilsModule.f90 \
        runInfoType.f90 \
        modelVarType.f90 \
        ioModule.f90 \
        runSac.f90 

model_run = $(patsubst %, $(share_dir)/%, $(run_util))

# --- BMI functions used by the driver to run the model
bmi_functions = bmi.f90 bmi_sac.f90
sac_bmi    = $(patsubst %, $(bmi_dir)/%, $(bmi_functions))

# --- sac driver
driver_sac = driver_bmi.f90
sac_drv    = $(patsubst %, $(driver_dir)/%, $(driver_sac))


# --- prepare to stitch it all together      # check just using the original names ... this does nothing?
run_code = $(model_run)
model    = $(model_sac)
bmi      = $(sac_bmi)


#========================================================================
# PART 4: Compile it 

all: model link clean 
debug: model_debug link
devel: model link 

check:
	echo test
	echo $(FC)

# compile model code
model:
	$(FC) $(FLAGS77) -c $(model_sac) $(INC)
	$(FC) $(FLAGS) -c $(model_run) $(sac_bmi) $(sac_drv) $(INC)

model_debug:
	$(FC) $(FLAGS_DEBUG77) -c $(model_sac) $(INC)
	$(FC) $(FLAGS_DEBUG) -c $(model_run) $(sac_bmi) $(sac_drv) $(INC)

# link routines
link:
#	$(FC) -fPIC -Bstatic_pgi -rpath,/usr/local/netcdf4-pgi/lib *.o -I./ $(LIBNETCDF) -o $(EXE)
	$(FC) -fPIC -I./ $(LIB) -o $(EXE) *.o 

# Remove object files
clean:
	rm -f *.o
	rm -f *.mod

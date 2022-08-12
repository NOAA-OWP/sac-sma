#========================================================================
# Makefile to compile the Snow-17 and driver code for multi-zone model
#========================================================================

#========================================================================
# PART 1: Define directory paths and compiler

# Define main/top directory (parent of /build/)
# Note:  the relative path of "../" drops into snow17 parent directory
F_MASTER_DIR = ../

# Location of the compiled modules
# MOD_PATH = $(F_MASTER_DIR)/build/ -- not used

# Define directories
driver_dir = $(F_MASTER_DIR)/src/driver/
snow19_dir = $(F_MASTER_DIR)/src/snow19/
share_dir  = $(F_MASTER_DIR)/src/share/
bmi_dir    = $(F_MASTER_DIR)/src/bmi/
exe_dir    = $(F_MASTER_DIR)/bin/

# Define the executable and path
EXE = $(exe_dir)/snow17.exe

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
  FLAGS_DEBUG = -static -Wall -g -ffree-line-length-none -cpp -fcheckall
  FLAGS = -O3 -fdefault-real-8 -fno-align-commons -ffree-line-length-none -cpp -fcheckall
  FLAGS2 = -O3 -fdefault-real-8 -fno-align-commons -ffree-line-length-none -cpp -fcheckall
endif

ifeq "$(FC)" "ifort"
  FLAGS_PROF = -static -debug -warn all -check all -FR -O0 -auto -WB -traceback -g -fltconsistency -fpe0
  FLAGS = -O3 -warn all -check all
  FLAGS2 = -O3 
  FLAGS77 = -O3 -f77rtl 
endif

ifeq "$(FC)" "pgf90"
  FLAGS_PROF = -Bstatic -Mbackslash -g -Mchkptr -Mchkstk -Mpgicoff -Minform=inform -Ktrap=divz,inv -Mprof=lines,time
  # FLAGS = -Mfreeform -O3 -Mbackslash -g -Mchkptr -Mchkstk -Mpgicoff -Minform=inform -Ktrap=divz,inv
  FLAGS = -O3 -r8 -Kieee
  FLAGS2 = -O3 -r8
endif


#========================================================================
# PART 3: Assemble all of the various subroutines

# --- snow-19 code (fortran 90, different options)
snow19 = \
		zero19.f \
		rout19.f \
		aesc19.f \
		melt19.f \
		SNEW.f \
		SNOWT.f \
		SNOWPACK.f \
		adjc19.f \
		aeco19.f \
		updt19.f \
		SNDEPTH.f \
		PACK19.f \
		exsnow19.f
model_snow19 = $(patsubst %, $(snow19_dir)/%, $(snow19))


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
        runSnow17.f90 

model_run = $(patsubst %, $(share_dir)/%, $(run_util))

# --- BMI functions used by the driver to run the model
bmi_functions = bmi.f90 bmi_snow17.f90
snow17_bmi    = $(patsubst %, $(bmi_dir)/%, $(bmi_functions))

# --- snow17 driver
driver_snow17 = driver_bmi.f90
snow17_drv    = $(patsubst %, $(driver_dir)/%, $(driver_snow17))


# --- prepare to stitch it all together      # check just using the original names ... this does nothing?
run_code = $(model_run)
model    = $(model_snow19)
bmi      = $(snow17_bmi)


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
	$(FC) $(FLAGS2) -c $(model) $(INC)
	$(FC) $(FLAGS) -c $(run_code) $(bmi) $(snow17_drv) $(INC)

model_debug:
	$(FC) $(FLAGS2) -g -fcheck=all -c $(model) $(INC)
	$(FC) $(FLAGS) -g -fcheck=all -c $(run_code) $(bmi) $(snow17_drv) $(INC)

# link routines
link:
#	$(FC) -fPIC -Bstatic_pgi -rpath,/usr/local/netcdf4-pgi/lib *.o -I./ $(LIBNETCDF) -o $(EXE)
	$(FC) -fPIC -I./ $(LIB) -o $(EXE) *.o 

# Remove object files
clean:
	rm -f *.o
	rm -f *.mod

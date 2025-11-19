#========================================================================
# Makefile to compile the Sac and driver code for multi-zone model
#========================================================================

#========================================================================
# PART 1: Define directory paths and compiler

# Define main/top directory (parent of /build/)
# Note:  the relative path of "../" drops into sac parent directory
F_MASTER_DIR = ../

# Location of the compiled modules
MOD_PATH = $(F_MASTER_DIR)/build/

# Define directories
driver_dir = $(F_MASTER_DIR)/src/driver/
sac_dir    = $(F_MASTER_DIR)/src/sac/
share_dir  = $(F_MASTER_DIR)/src/share/
bmi_dir    = $(F_MASTER_DIR)/src/bmi/
exe_dir    = $(F_MASTER_DIR)/bin/

# Define the executable and path
EXE = $(exe_dir)/sac.exe

# 1. Set FC based on the environment variable, if it exists.
#    If the environment variable $FC is NOT set, use 'mpiifort' as the default.
FC ?= gfortran

# Export the Fortran compiler environmental variable flag
export FC

#========================================================================
# PART 2: Define the libraries and flags (can be an include file)

# --- Define the libraries and path to include files
LIB = -L$./
INC = -I ./


# Check if FC contains the substring "ifort" OR "ifx"
# This covers ifort, mpiifort, ifx, and mpiifx.
INTEL_COMPILER_CHECK = $(findstring ifort,$(FC)) $(findstring ifx,$(FC))

# --- Define flags

# gfortran compiler flags
ifeq "$(FC)" "gfortran"
  FLAGS_DEBUG  = -static -fno-fast-math -Wall -g -ffree-line-length-none -cpp -fcheck=all -UNGEN_ACTIVE -I$(MOD_PATH)
  FLAGS_DEBUG77 = -static -fno-fast-math -std=legacy -Wall -g -ffree-line-length-none -cpp -fcheck=all -UNGEN_ACTIVE -I$(MOD_PATH)
  FLAGS = -O3 -fno-fast-math -fdefault-real-8 -fno-align-commons -ffree-line-length-none -cpp -fcheck=all -UNGEN_ACTIVE -I$(MOD_PATH)
  FLAGS77 = -O3 -fno-fast-math -fdefault-real-8 -fno-align-commons -ffree-line-length-none -cpp -fcheck=all -UNGEN_ACTIVE -I$(MOD_PATH)
endif

# Check if FC contains the substring "ifort" OR "ifx" 
# This covers ifort, mpiifort, ifx, and mpiifx.
INTEL_COMPILER_CHECK = $(findstring ifort,$(FC)) $(findstring ifx,$(FC))

# Intel compiler flags
ifneq (,$(strip $(INTEL_COMPILER_CHECK)))
  FLAGS_PROF = -fpp -fp-model strict -static -debug -warn all -FR -O0 -auto -WB -traceback -g -fltconsistency -fpe0 -UNGEN_ACTIVE -I$(MOD_PATH)
  FLAGS = -fpp -fp-model strict -O3 -warn all -UNGEN_ACTIVE -I$(MOD_PATH)
  FLAGS77 = -fpp -fp-model strict -O3 -f77rtl -UNGEN_ACTIVE -I$(MOD_PATH)
endif

# pgf90 compiler flags
ifeq "$(FC)" "pgf90"
  FLAGS_PROF = -Mpreprocess -mp -Bstatic -Mbackslash -g -Mchkptr -Mchkstk -Mpgicoff -Minform=inform -Ktrap=divz,inv -Mprof=lines,time -UNGEN_ACTIVE -I$(MOD_PATH)
  # FLAGS = -Mfreeform -O3 -Mbackslash -g -Mchkptr -Mchkstk -Mpgicoff -Minform=inform -Ktrap=divz,inv
  FLAGS = -Mpreprocess -O3 -mp -r8 -Kieee -UNGEN_ACTIVE -I$(MOD_PATH)
  FLAGS77 = -Mpreprocess -O3 -mp -r8 -UNGEN_ACTIVE -I$(MOD_PATH)
endif


#========================================================================
# PART 3: Assemble all of the various subroutines

# --- sac code (fortran 90, different options)
sac = \
                duamel.f \
                ex_sac1.f90 \
                sac1.f90 \
                sac_data_mod.f90

model_sac = $(patsubst %, $(sac_dir)/%, $(sac))

# --- Dependency Management for sac_data_mod ---
SAC_DATA_MOD_SRC = $(sac_dir)/sac_data_mod.f90
SAC_DATA_MOD_OBJ = $(patsubst %.f90, %.o, $(notdir $(SAC_DATA_MOD_SRC)))
# Filter out sac_data_mod.f90 from the main list so it's only compiled once
OTHER_SAC_SOURCES = $(filter-out $(SAC_DATA_MOD_SRC), $(model_sac))

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
        derivedType.f90\
        ioModule.f90 \
        sacLogger.f90 \
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
model:  $(SAC_DATA_MOD_OBJ)
	$(FC) $(FLAGS77) -c $(OTHER_SAC_SOURCES) $(INC)
	$(FC) $(FLAGS) -c $(model_run) $(sac_bmi) $(sac_drv) $(INC)

# Dedicated rule to compile sac_data_mod.f90 and create the .mod file FIRST
# This fixes the two-run failure problem.
$(SAC_DATA_MOD_OBJ): $(SAC_DATA_MOD_SRC)
	$(FC) $(FLAGS) -c $< $(INC)

model_debug:
	$(FC) $(FLAGS_DEBUG77) -c $(SAC_DATA_MOD_SRC) $(INC)
	$(FC) $(FLAGS_DEBUG77) -c $(OTHER_SAC_SOURCES) $(INC)
	$(FC) $(FLAGS_DEBUG) -c $(model_run) $(sac_bmi) $(sac_drv) $(INC)

# link routines
link:
	$(FC) -fPIC -I./ $(LIB) -o $(EXE) *.o

# Remove object files
clean:
	rm -f *.o
	rm -f *.mod

# Sac-SMA

**Description**:  A BMI enabled version of the Sacramento Soil Moisture Accounting (Sac-SMA) model.  

This version of Sac-SMA allows for multiple hydrologic response units (HRUs) to be modeled at once.  It was built to run standalone as well as in the [ngen framework](https://github.com/NOAA-OWP/ngen).

Primary Language: Fortran


## Dependencies
Fortran compiler

## Installation and Running in Standalone

The following describes how to install the run the Sac-SMA as a standalone model.

Clone repository and set up directory
```
git clone https://github.com/NOAA-OWP/sac-sma.git
cd sac-sma
mkdir bin
cd build
```

Define your directory paths and fortran compiler in `Makefile.local`. 
Compiler options: pgf90, ifort, gfortran

```
make -f Makefile.local
```

You should now see `sac.exe` in the `bin/` directory. 

To run the example provided:

```
cd ../test_cases/ex1/run/
../../../bin/sac.exe namelist.bmi.HHWM8
```

## Installation and Running in [Ngen](https://github.com/NOAA-OWP/ngen)

The following are instructions for setting up this BMI wrapped Sac-SMA model in the Next Generation Water Resources Modeling Framework (ngen) developed by the NOAA's Office of Water Prediction.

Clone ngen and update submodules.
```
git clone https://github.com/NOAA-OWP/ngen.git
cd ngen
git submodule update --init --recursive
```

Add Sac-SMA as a submodule.
```
git submodule add https://github.com/NOAA-OWP/sac-sma.git ./extern/sac-sma/sac-sma/
```

Copy the necessary files from the `ngen_files` directory.
```
cp ./extern/sac-sma/sac-sma/ngen_files/sacbmi.pc.in ./extern/sac-sma/sac-sma/ngen_files/CMakeLists.txt ./extern/sac-sma
```

Build the model.
```
cmake -B extern/sac-sma/cmake_build -S extern/sac-sma
cmake --build extern/sac-sma/cmake_build --target all
```
This should create a library file (libsacbmi.1.0.0.dylib or libsacbmi.1.0.0.so) under /ngen/extern/sac-sma/cmake_build/

**NOTE: ngen requires boost libraries.  Check that you have these and that ngen is pointing to the right location (e.g. `echo $BOOST_ROOT`).  If you do not have them, download the libraries. If you have issues with the path, explicitly define it by `export BOOST_ROOT=<path>`. For more information on building the nextgen framework, see the [ngen git repo](https://github.com/NOAA-OWP/ngen/blob/master/INSTALL.md).

Below are instructions for running an example simulation using Sac-SMA in ngen:
Copy the necessary files to their respective folders.
```
cp ./extern/sac-sma/sac-sma/ngen_files/example_realization_w_pet_sac.json ./data
cp ./extern/sac-sma/sac-sma/ngen_files/sac-init-HHWM8.namelist.input ./data/bmi/fortran/
cp ./extern/sac-sma/sac-sma/ngen_files/cat-27.csv ./data/forcing/
```

Make and build the example.
```
cmake -B extern/iso_c_fortran_bmi/cmake_build -S extern/iso_c_fortran_bmi
make -C extern/iso_c_fortran_bmi/cmake_build

cmake -B extern/sac-sma/cmake_build -S extern/sac-sma 
make -C extern/sac-sma/cmake_build

cmake -B extern/evapotranspiration/cmake_build -S extern/evapotranspiration/evapotranspiration
make -C extern/evapotranspiration/cmake_build

cmake -DNGEN_WITH_BMI_FORTRAN=ON -DNGEN_WITH_BMI_C=ON -DNGEN_WITH_PYTHON=ON -B cmake_build -S .
cmake --build cmake_build --target ngen
```

Create a new directory in the main ngen folder to run the model and keep results.
``` 
mkdir sac
cd sac
ln -s ../data
ln -s ../extern
```

Run the model.
```
../cmake_build/ngen data/catchment_data.geojson "cat-27" ./data/nexus_data.geojson "nex-26" ./data/example_realization_w_pet_sac.json
```

This should generate the files `cat-27.csv` and `nex-26_output.csv`.

## Parameters

For an [example parameter file](https://github.com/NOAA-OWP/sac-sma/tree/master/test_cases/ex1/input/params), see the test case.

Parameter | Description | Units
----------|-------------|--------
hru_id | Identification string for each hrus | -
hru_area | Area of each HRU | * 
uztwm | Maximum upper zone tension water | mm
uzfwm | Maximum upper zone free water | mm
lztwm | Maximum lower zone tension water | mm
lzfsm | Maximum lower zone free water, secondary (aka supplemental) | mm
lzfpm | Maximum lower zone free water, primary | mm
adimp | Additional "impervious" area due to saturation | decimal percent
uzk | Upper zone recession coefficient | - 
lzpk | Lower zone recession coefficient, primary | - 
lzsk | Lower zone recession coefficient, secondary (aka supplemental) | -
zperc | Minimum percolation rate coefficient | - 
rexp | Percolation equation exponent | - 
pctim | Minimum percent impervious area | decimal percent
pfree | Percent percolating directly to lower zone free water | decimal percent
riva | Percent of the basin that is riparian area | decimal percent
side | Portion of the baseflow which does not go to the stream | decimal percent
rserv | Percent of lower zone free water not transferable to the lower zone tension water | decimal percent

<sup>*</sup> The area is used for areal averaging outputs, so the units of area are not important as long as they are consistent.


## Logger

The Errror Warning and Trapping Systems (EWTS) has been added to this module using a logging schema. All write statements have been converted to `write_log` statements, which saves the ouptut to a log file based on the log level.

When running within the ngen framework, the log file and log level are handled programatically. When running standalone, logging is defaulted to DISABLED. 

**Running Standalone**

In order to generate log messages when running standalone, the `NGEN_EWTS_LOGGING` environment variable must be set to `ENABLED`. This is the only required environment variable . Other optional logger environment variables exist for specifying the log file full pathname and setting the log level. If the user only enables logging, the log level will be set to INFO and the filename will be created based on the user and module names. All logger setup details are written to the console when the module is run. 
```
# Case Sensitive
export NGEN_EWTS_LOGGING=ENABLED
export NGEN_LOG_FILE_PATH=<full pathname for log file>
export SACSMA_LOGLEVEL=<DEBUG, INFO, WARNING, SEVERE, FATAL>
```
**Log Levels**
| Level   | Description                                         | Typical Use                                   |
|---------|-----------------------------------------------------|-----------------------------------------------|
| DEBUG   | Detailed diagnostic info for development/troubleshooting. | Variable values, function entry/exit. |
| FATAL   | Critical failure that aborts or makes app unrecoverable. | Crashes, memory errors, invalid state.        |
| INFO    | General events confirming expected operations.       | Startup/shutdown, configs, task completions.  |
| SEVERE  | Significant problem; app may continue in degraded state. | Failed services, corrupted configs, data loss.|
| WARNING | Potential issue that doesn’t stop execution.         | Deprecated APIs, missing files, repeatable errors. |

Default log level is INFO. The log level is hierarchical. Setting it to INFO, will log INFO, WARNING, SEVERE and FATAL
messages.

## Getting help

If you have questions, concerns, bug reports, etc, please file an issue in this repository's Issue Tracker.

## Open source licensing info
1. [TERMS](TERMS.md)
2. [LICENSE](LICENSE)


----

## References

1. Burnash, R.J.C., R.L. Ferral, R.A. McGuire. (1973). A generalized streamflow simulation system: Conceptual modeling for digital computers. US Department of Commerce, National Weather Service

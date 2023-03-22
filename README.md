# Sac-SMA

**Description**:  A BMI enabled version of the Sacramento Soil Moisture Accounting (Sac-SMA) model.  

This version of Sac-SMA allows for multiple hydrologic response units (HRUs) to be modeled at once.  It was built to run standalone and will be expanded to be incorporated into the [ngen framework](https://github.com/NOAA-OWP/ngen).

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

Create a folder for Sac-SMA and add as a submodule.
```
cd extern
mkdir sac-sma
cd ..
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
This should create the file: /ngen/extern/sac-sma/cmake_build/libsacbmi.so.1.0.0

Below are instructions for running an example simulation using Sac-SMA in ngen:
Copy the necessary files to their respective folders.
```
cp ./extern/sac-sma/sac-sma/ngen_files/example_realization_w_pet_sac.json ./data
cp ./extern/sac-sma/sac-sma/ngen_files/sac-init-HHWM8.namelist.input ./data/bmi/fortran/
cp ./extern/sac-sma/sac-sma/ngen_files/cat-27.csv ./data/forcing/
```

Create a new directory in the main ngen folder to run the model and keep results.
``` 
mkdir sac
cd sac
ln -s ../data
ln -s ../extern
```

Make and build the example.
```
cmake -B extern/iso_c_fortran_bmi/cmake_build -S extern/iso_c_fortran_bmi
make -C extern/iso_c_fortran_bmi/cmake_build
cmake -B cmake_build -S . -DNGEN_ACTIVATE_PYTHON:BOOL=ON -DBMI_C_LIB_ACTIVE:BOOL=ON -DBMI_FORTRAN_ACTIVE:BOOL=ON
make -C cmake_build

cmake -B extern/sac-sma/cmake_build -S extern/sac-sma 
make -C extern/sac-sma/cmake_build

cmake -B extern/evapotranspiration/cmake_build -S extern/evapotranspiration/evapotranspiration
make -C extern/evapotranspiration/cmake_build
```

Run the model.
```
cmake_build/ngen data/catchment_data.geojson "cat-27" ./data/nexus_data.geojson "nex-26" ./data/example_realization_w_pet_sac.json
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


## Getting help

If you have questions, concerns, bug reports, etc, please file an issue in this repository's Issue Tracker.

## Open source licensing info
1. [TERMS](TERMS.md)
2. [LICENSE](LICENSE)


----

## References

1. Burnash, R.J.C., R.L. Ferral, R.A. McGuire. (1973). A generalized streamflow simulation system: Conceptual modeling for digital computers. US Department of Commerce, National Weather Service

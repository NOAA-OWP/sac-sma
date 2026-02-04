# Sac-SMA

**Description**:  A BMI enabled version of the Sacramento Soil Moisture Accounting (Sac-SMA) model.  

This version of Sac-SMA allows for multiple hydrologic response units (HRUs) to be modeled at once.  It was built to run standalone as well as in the [ngen framework](https://github.com/NOAA-OWP/ngen).

Primary Language: Fortran


## Dependencies
- Fortran compiler
- NextGen ISO C Fortran BMI library (optional)

## Running in Standalone

The following describes how to build and run Sac-SMA as a standalone model.

Clone repository and change to project directory
```
git clone https://github.com/NOAA-OWP/sac-sma.git
cd sac-sma

Generate a CMake build system and directory (in the example below and those that follow, the directory is assumed to be `cmake_build` within the repo root):
```bash
# You can also include the '-DCMAKE_BUILD_TYPE=Debug' option if you want to build for debugging
cmake -B cmake_build -S .
```

Note that you may need or want to specify the Fortran compiler, done by supplying a value for the `FC` variable when you generate the build directory.  There are many reasons for that:  maybe you have multiple compilers installed, or maybe your compiler isn't installed to the standard system path or using a standard name CMake will recognize.```
Regardless, if necessary just append `FC=<path_to_compiler>` to the rest of the command:
```bash
# Here we are manually telling CMake to use '/opt/local/bin/gfortran-mp-14' as the Fortran compiler, rather than 
# whatever compiler it would find on its own.
FC=/opt/local/bin/gfortran-mp-14 cmake -B cmake_build -S .
```

With the build directory generated, build the stand-alone executable:
```bash
cmake --build cmake_build --target sac```

You should now see the `cmake_build/sac` stand-alone executable. 

To run the example provided:

```
cd ../test_cases/ex1/run/
../../../cmake_build/sac namelist.bmi.HHWM8
```

## Running in [Ngen](https://github.com/NOAA-OWP/ngen)

The following are instructions for building the Sac-SMA BMI module shared library.  In particular, this is necessary to run the Sac-SMA model in the [Next Generation Water Resources Modeling Framework](https://github.com/NOAA-OWP/ngen).

As described above for stand-alone builds, clone the repo if necessary and change into the repo root directory.
```bash
git clone https://github.com/NOAA-OWP/sac-sma.git
cd sac-sma```
```

Within the repo root, once again generate a CMake build directory.  However, this time, we (probably) need to specify the location of the NextGen ISO C Fortran BMI library.  This is an intermediate library needed for any Fortran BMI module to ensure NextGen compatibility.  This is done with the `ISO_C_FORTRAN_BMI_PATH` option

> [!WARNING]
> If you created stand-alone-only build directory already, remove it first.  Don't worry:  the NextGen-supporting build directory will also support stand-alone builds.
```bash
# The same advice discussed in the stand-alone section about optionally adding 'FC=<path_to_compiler>' and/or 
# '-DCMAKE_BUILD_TYPE=Debug' applies here
cmake -B cmake_build -DISO_C_FORTRAN_BMI_PATH=/Users/rbartel/Developer/noaa/ngen/extern/iso_c_fortran_bmi -S .
```
> [!TIP]
> The directory to use for ISO_C_FORTRAN_BMI_PATH will generally be `<path_to_your_ngen_repo>/extern/iso_c_fortran_bmi`.  It is assumed that you have already cloned the NextGen repo locally.

With this done when the build directory is created, we will also have access to another build target:  `sacbmi`.   This is how we build the NextGen BMI module shared library:
```bash
cmake --build cmake_build --target sacbmi
``` 
> [!NOTE]
> You can still build the stand-alone executable as described above when you generate a build directory for NextGen builds.  You can also omit specify a target when building, in which case, CMake will build all valid build targets.
```bash
cmake --build cmake_build --target sac    # This will build the stand-alone executable, even when generating for NextGen builds
cmake --build cmake_build                 # This will build the stand-alone executable and the NextGen BMI module shared library```
```
Once you build the shared library, you should see the shared library in build directory, named `cmake_build/libsacbmi.<version>.so` on Linux systems (on Mac, you will see `.dylib` rather than `.so`).  There will also be `cmake_build/libsacbmi.so` symlink pointing to the shared library file.

### Example Config
The [ngen_files/](./ngen_files) directory has a few example configurations that you can use to test running the Sac-SMA model in ngen.  Just make sure to update the path of the Sac-SMA shared library in the `library_file` parameter to point to wherever your build shared library is (you may need to do something similar for the configured PET shared library).

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

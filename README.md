# Sac-SMA

**Description**:  A BMI enabled version of the Sacramento Soil Moisture Accounting (Sac-SMA) model.  

This version of Sac-SMA allows for multiple hydrologic response units (HRUs) to be modeled at once.  It was built to run standalone and will be expanded to be incorporated into the [ngen framework](https://github.com/NOAA-OWP/ngen).

Primary Language: Fortran


## Dependencies
Fortran compiler

## Installation and Running the Model

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

1. 

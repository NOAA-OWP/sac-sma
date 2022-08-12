#!/bin/csh
# A. Wood, 2016; updated May 2022 for snow only version using BMI

# Run general distributed snow17 code, which loops over lumped code version for multiple model areas and 
# writes an additional combined (basin mean) output file

# To run the snow17/sac model, just execute this script (or else the command below)

../../../bin/snow17.exe namelist.bmi.HHWM8

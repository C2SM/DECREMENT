#!/bin/bash -l

# Archive CLM output
# Petra Sieber 2024-01-22

#==========================================
# Case settings
#==========================================

#case_source=cclm2_EUR11_hist-spinup          # case name on SCRATCH
case_source=$(basename "$(dirname "${PWD}")") # extract case name

scriptdir=$PWD
casedir=$SCRATCH/${case_source}
sourcedir=$SCRATCH/${case_source}/20_cclm2_c

#==========================================
# Move CLM output files to cesm_output directory
#==========================================

mkdir -p $sourcedir/cesm_output/daily
mkdir -p $sourcedir/cesm_output/daily_min
mkdir -p $sourcedir/cesm_output/daily_max
mkdir -p $sourcedir/cesm_output/monthly

mv $sourcedir/clm5.0_eur0.1.clm2.h0.*.nc $sourcedir/cesm_output/daily/.
mv $sourcedir/clm5.0_eur0.1.clm2.h1.*.nc $sourcedir/cesm_output/daily_min/.
mv $sourcedir/clm5.0_eur0.1.clm2.h2.*.nc $sourcedir/cesm_output/daily_max/.
mv $sourcedir/clm5.0_eur0.1.clm2.h3.*.nc $sourcedir/cesm_output/monthly/.


# Move CLM restart files to directory
clm5.0_eur0.1.clm2.r*.*.nc
clm5.0_eur0.1.cpl.r.*.nc
clm5.0_eur0.1.datm.rs1.*.bin
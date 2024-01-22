#!/bin/bash -l
#
#SBATCH --job-name="postproc"
#SBATCH --account="s1256"
#SBATCH --time=10:00:00
#SBATCH --partition=normal
#SBATCH --constraint=gpu
#SBATCH --hint=nomultithread
#SBATCH --nodes=1
#SBATCH --ntasks-per-core=1
#SBATCH --ntasks-per-node=12
#SBATCH --cpus-per-task=1
#SBATCH --output=log_postproc_CLM.out
#SBATCH --error=log_postproc_CLM.err

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export CRAY_CUDA_MPS=1

# Postprocessing of CCLM2 output from CLM (takes ca 7h)
# Launch from and work on scratch to have writing permission  (project is read-only for slurm jobs)
# Transfer from scratch to project for permanent storage with rsync in the end
# Petra Sieber 2024-01-17

module load cray-hdf5
module load cray-netcdf
module load CDO
module load NCO

#==========================================
# Case settings
#==========================================

#case_source=cclm2_EUR11_hist-spinup          # case name on SCRATCH
case_source=$(basename "$(dirname "${PWD}")") # extract case name
year_ini=2004                                 # initial date of the simulation, incl spin-up
year_start=2006                               # start of the analysis period, excl spin-up or min 10 years
year_end=2015                                 # end of the analysis period
case_dest=cclm2_EUR11_historical              # case name on PROJECT
#case_dest=$case_source

scriptdir=$PWD
casedir=$SCRATCH/${case_source}
sourcedir=$SCRATCH/${case_source}/20_cclm2_c

#==========================================
#          (1) CLM DAILY
#==========================================

echo -e "\n *** (1) CLM DAILY *** \n"

# Track duration
SECONDS=0

destdir=$scriptdir/cclm2_output_processed/${case_dest}/clm/daily
mkdir -p $destdir

# Work locally in the destination directory (affects netcdf history)
cd ${destdir}

#==========================================
# Save time-invariant auxiliary variables
#==========================================

cdo -selname,area,landfrac,landmask,pftmask,nbedrock,ZSOI,DZSOI,WATSAT,SUCSAT,BSW,HKSAT,ZLAKE,DZLAKE ${sourcedir}/clm5.0_eur0.1.clm2.h0.${year_start}-01-01-00000.nc clm5.0_eur0.1.clm2.hx_auxiliaries.nc

#==========================================
# Merge time, merge tapes, fix calendar
#==========================================

# Merge time and select variables per tape
# h0 (mean), creates 250 GB file and takes ca 30 min
cdo -chname,TREFMNAV,TSA_MIN,TREFMXAV,TSA_MAX -mergetime -apply,-selname,FSDS,FSR,FLDS,FIRE,EFLX_LH_TOT,FSH,QFLX_EVAP_TOT,QSOIL,QVEGE,QVEGT,QOVER,QDRAI,TOTSOILLIQ,TOTSOILICE,SOILWATER_10CM,FSNO,SNOW_DEPTH,TSOI_10CM,FPSN,TV,TG,TSKIN,TSA,TREFMNAV,TREFMXAV,TBOT,QIRRIG [ $(ls ${sourcedir}/clm5.0_eur0.1.clm2.h0.*.nc | tail -n 11) ] tmp_h0.nc

# h1 (min)
cdo -chname,TBOT,TBOT_MIN,TSKIN,TSKIN_MIN -mergetime -apply,-selname,TBOT,TSKIN [ $(ls ${sourcedir}/clm5.0_eur0.1.clm2.h1.*.nc | tail -n 11) ] tmp_h1.nc

# h2 (max)
cdo -chname,TBOT,TBOT_MAX,TSKIN,TSKIN_MAX -mergetime -apply,-selname,TBOT,TSKIN [ $(ls ${sourcedir}/clm5.0_eur0.1.clm2.h2.*.nc | tail -n 11) ] tmp_h2.nc

# Merge tapes, creates 285 GB file and takes ca 30 min
# Shift timestamp to the beginning of the accumulation interval to mark the correct day for averaging and splitting
outfile=clm5.0_eur0.1.clm2.hx_2006-2015_daily.nc
cdo -shifttime,-1day -merge [ tmp_h0.nc tmp_h1.nc tmp_h2.nc ] tmp_$outfile

# Set calendar from 365_day (=noleap, goes to 2016-01-04) to proleptic_gregorian (leap, goes to 2016-01-01)
# Change time format to relative for reading with e.g. xarray
# Clip to the analysis period
ncatted -a calendar,time,m,c,proleptic_gregorian tmp_$outfile tmp_calendar.nc
cdo -a copy tmp_calendar.nc tmp_$outfile
cdo -selyear,${year_start}/${year_end} -setreftime,${year_ini}-01-01,00:00:00,days tmp_$outfile $outfile

rm tmp*.nc

#==========================================
# Basic post-processing of CLM output
#==========================================

infile=$outfile

# Modify global file attribute
ncatted -O -h -a source,global,m,c,"Community Land Model CLM5.0" $infile tmp1.nc

# Rearrange longitude variable from 0..360 deg to -180..180 deg
# Use ncap2 arithmetics with rounding to preserve 2 digit precision
ncap2 -O -s 'where(lon>180) lon=round((lon-360)*100)/100' tmp1.nc tmp2.nc

# Cut away sponge zone
# EUR11 lonlat bounds excl. sponge zone for clipping
lonmin=-44.55
lonmax=64.95
latmin=21.95
latmax=72.65 # +0.1 for NCO to clip at 72.55
ncks -O -d lon,${lonmin},${lonmax} -d lat,${latmin},${latmax} tmp2.nc tmp3.nc

mv tmp3.nc $infile
rm tmp*.nc

#==========================================
# Split into yearly files
#==========================================

cdo splityear $outfile clm5.0_eur0.1.clm2.hx_daily_
#rm $outfile

# Finish
cd $scriptdir

# Evaluate duration and print to log file
duration=$SECONDS
echo -e "\n Finished (1) CLM DAILY"
echo "$(($duration / 3600)) hours and $(($duration % 3600 /60)) minutes elapsed."

#==========================================
#          (2) CLM MONTHLY
#==========================================

echo -e "\n *** (2) CLM MONTHLY *** \n"

# Track duration (ca 2 min)
SECONDS=0

destdir=$scriptdir/cclm2_output_processed/${case_dest}/clm/monthly
mkdir -p $destdir

# Work locally in the destination directory (affects netcdf history)
cd ${destdir}

# No additional time-invariant auxiliary variables compared to daily files

#==========================================
# Merge time
#==========================================

# Merge time and select variables per tape
# Shift timestamp to the beginning of the accumulation interval to mark the correct day for averaging and splitting
# h3 (mean), creates 2.5 GB file
outfile=clm5.0_eur0.1.clm2.h3_${year_start}-${year_end}_monthly.nc
cdo -shifttime,-1month -mergetime -apply,-selname,TSKIN,TSA,TREFMNAV,TREFMXAV,TBOT,RAIN,SNOW,Q2M,U10,TLAI [ $(ls ${sourcedir}/clm5.0_eur0.1.clm2.h3.*.nc | tail -n 10) ] tmp1.nc

# Remove the time bounds which are incorrect afer shifting (first variable attribute, then variable)
ncatted -O -a bounds,time,d,, tmp1.nc tmp2.nc
cdo -delname,time_bnds tmp2.nc $outfile

# Set the time bounds to set them correctly at timestamp-timestamp+1
#ncap2 -O -s 'time@bounds="time_bnds";defdim("bnds",2);time_bnds[$time,$bnds]=0.0;*time_dff=(time(1)-time(0));time_bnds(:,0)=time;time_bnds(:,1)=time+time_dff;' tmp.nc $outfile

# Fixing the calendar doesn't help for monthly files

#==========================================
# Basic post-processing of CLM output
#==========================================

infile=$outfile

# Modify global file attribute
ncatted -O -h -a source,global,m,c,"Community Land Model CLM5.0" $infile tmp1.nc

# Rearrange longitude variable from 0..360 deg to -180..180 deg
# Use ncap2 arithmetics with rounding to preserve 2 digit precision
ncap2 -O -s 'where(lon>180) lon=round((lon-360)*100)/100' tmp1.nc tmp2.nc

# Cut away sponge zone
# EUR11 lonlat bounds excl. sponge zone for clipping
lonmin=-44.55
lonmax=64.95
latmin=21.95
latmax=72.65 # +0.1 for NCO to clip at 72.55
ncks -O -h -d lon,${lonmin},${lonmax} -d lat,${latmin},${latmax} tmp2.nc tmp3.nc

mv tmp3.nc $infile
rm tmp*.nc

# No need to split years for monthly output

# Finish
cd $scriptdir

# Evaluate duration and print to log file
duration=$SECONDS
echo -e "\n Finished (2) CLM MONTHLY"
echo "$(($duration / 3600)) hours and $(($duration % 3600 /60)) minutes elapsed."

#==========================================
#          (3) CLM CASE_DOCS
#==========================================

echo -e "\n *** (3) CLM CASE_DOCS *** \n"

# Track duration (ca 2 min)
SECONDS=0

destdir=$scriptdir/cclm2_output_processed/${case_dest}/clm/case_docs
mkdir -p $destdir/indata
mkdir -p $destdir/namelists
mkdir -p $destdir/config

#==========================================
# Collect files
#==========================================

# CESM input
fsurdat=$(ncks -M $sourcedir/clm5.0_eur0.1.clm2.h3.${year_start}-02-01-00000.nc | grep -oP '[^"]*"\K[^"]*' | grep surfdata)
rsync -av $SCRATCH/cclm2_input_decrement/cesm_input/$fsurdat $destdir/indata/
rsync -av $SCRATCH/cclm2_input_decrement/cesm_input/domain_EUR11_lon360_reduced.nc $destdir/indata/

# CESM namelists
rsync -av $sourcedir/*_in $destdir/namelists/

# Config files
fconfig=$(readlink -f $casedir/config)
rsync -av $fconfig $destdir/config/
if [[ $fconfig =~ "ssp1" ]]; then
    rsync -av $casedir/simulation_configs/SIMULATION_COSMO-EUR11_MPI-ESM $destdir/config/
else
    #rsync -av $casedir/simulation_configs/SIMULATION_COSMO-EUR11_ERA5 $destdir/config/
    rsync -av $casedir/simulation_configs/SIMULATION_COSMO-EUR11_COPAT2 $destdir/config/
fi
rsync -av $casedir/user_settings $destdir/config/

# Finish
cd $scriptdir

# Evaluate duration and print to log file
duration=$SECONDS
echo -e "\n Finished (3) CLM CASE_DOCS"
echo "$(($duration / 3600)) hours and $(($duration % 3600 /60)) minutes elapsed."

#==========================================
# Rsync to PROJECT
#==========================================

echo -e "\n *** RSYNC *** \n"
rsync -av --progress $scriptdir/cclm2_output_processed $PROJECT/


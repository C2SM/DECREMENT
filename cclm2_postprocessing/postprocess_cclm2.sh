#!/bin/bash -l
#
#SBATCH --job-name="postproc"
#SBATCH --account="s1256"
#SBATCH --time=06:00:00
#SBATCH --partition=normal
#SBATCH --constraint=gpu
#SBATCH --hint=nomultithread
#SBATCH --nodes=1
#SBATCH --ntasks-per-core=1
#SBATCH --ntasks-per-node=12
#SBATCH --cpus-per-task=1
#SBATCH --output=log_postproc.out
#SBATCH --error=log_postproc.err

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export CRAY_CUDA_MPS=1

# Postprocessing of CCLM2 output (CLM and COSMO)
# Launch from scratch because of writing permission (to log)
# Transfer from scratch to project for permanent storage
# Petra Sieber 2024-01-17

# Track duration
SECONDS=0

module load cray-hdf5
module load cray-netcdf
module load CDO
module load NCO

#==========================================
# Case settings
#==========================================

#case_source=cclm2_EUR11_hist-spinup # case name on SCRATCH
case_source=$(basename "$(dirname "${PWD}")") # extract case name
year_start=2004
year_end=2015
case_dest=cclm2_EUR11_historical # case name on PROJECT
#case_dest=$case_source

scriptdir=$PWD
sourcedir=$SCRATCH/${case_source}/20_cclm2_c

#==========================================
#          (1) CLM DAILY
#==========================================

destdir=$PROJECT/CCLM2_output/processed/${case_dest}/daily/clm
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
# Delete the first timestamp (initialisation for daily and hourly files)
outfile=clm5.0_eur0.1.clm2.hx_2006-2015_daily.nc
cdo delete,timestep=0 -shifttime,-1day -merge [ tmp_h0.nc tmp_h1.nc tmp_h2.nc ] $outfile
rm tmp_*.nc

# Set calendar from 365_day (=noleap, goes to 2016-01-04) to proleptic_gregorian (leap, goes to 2016-01-01)
ncatted -a calendar,time,m,c,proleptic_gregorian $outfile tmp_calendar.nc
cdo -a copy tmp_calendar.nc $outfile

#==========================================
# Basic post-processing of CLM output
#==========================================

infile=$outfile

# Modify global file attribute
ncatted -O -h -a source,global,m,c,"Community Land Model CLM5.0" $infile tmp1.nc

# Rearrange longitude variable from 0..360 deg to -180..180 deg
# cdo -O -sellonlatbox,-180,180,-90,90 $infile $tmpfile
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

#==========================================
# Split into yearly files
#==========================================

cdo splityear $outfile clm5.0_eur0.1.clm2.hx_daily_.nc

#==========================================
# Finish
#==========================================

cd $scriptdir

# Evaluate duration and print to log file
duration=$SECONDS
echo "$(($duration / 3600)) hours and $(($duration % 3660)) minutes elapsed."

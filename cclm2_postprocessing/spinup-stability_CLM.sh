#!/bin/bash -l
#
#SBATCH --job-name="postproc"
#SBATCH --account="s1256"
#SBATCH --time=24:00:00
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

# Evaluate selected variables as to whether they are spunup (i.e. in equilibrium)
# For inspiration, see …/tools/contrib/SpinupStability_SP.ncl on the CTSM dev branch (but this is for global domains!)
# Variables examined are:
#     for BGC: TOTECOSYSC,TOTSOMC,TOTVEGC,TLAI,GPP,TWS,H2OSNO
#     for SP: FSH, EFLX_LH_TOT, GPP, TWS (total water storage), H2OSOI (volumetric soil water in layer 8) and TSOI (soil temperature in layer 10)
# Evaluation based on annual means, here caluculated from daily output (h0)
# The percentage of land area in equilibrium is examined

# Spin-up stability CCLM2 (notbook)
#ds['TOTSOILWATER'] = ds['TOTSOILICE'] + ds['TOTSOILLIQ']
#variables = ['FSH', 'EFLX_LH_TOT', 'FSDS', 'FPSN', 'TSOI_10CM', 'TOTSOILWATER', 'SOILWATER_10CM', 'FSNO', 'SNOW_DEPTH', 'TLAI']
#var_long = ['Sensible heat flux', 'Latent heat flux', 'Shortwave down', 'GPP', 'Soil temperature top 10cm', 'Total soil moisture', 'Soil moisture top 10cm', 'Snow fraction', 'Snow depth', 'LAI']

# Create yearly means of all CLM monthly (h3) and daily (h0) output variables
# Launch from scratch because of writing permission (to log)
# Petra Sieber 2024-01-16

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
destdir=$PROJECT/CCLM2_output/processed/${case_dest}/yearly/clm
mkdir -p $destdir

# Work locally in the destination directory (affects netcdf history)
cd ${destdir}

#==========================================
# Merge time and create yearly means
#==========================================
# Shift time by 1h to get periods in means correct because CLM marks interval ends (e.g. daily output for 31-12-2000 is stamped 01-01-2001-00:00 and CDO does not condsider time bounds)
# Keep only unique variables

# For h3 monthly (yearmonmean for correct weighting)
file_h3=clm5.0_eur0.1.clm2.h3_${year_start}-${year_end}_yearmean.nc
cdo --timestat_date last -yearmonmean -shifttime,-1hours -mergetime -apply,-selname,TSKIN,TSA,TREFMNAV,TREFMXAV,TBOT,RAIN,SNOW,Q2M,U10,TLAI [ ${sourcedir}/clm5.0_eur0.1.clm2.h3.*.nc ] ${file_h3}

# For h0, h1, h2 daily (keep only variables that are not on h3, remove single timestamp in 2003 and excess in 2016)
file_h0=clm5.0_eur0.1.clm2.h0_${year_start}-${year_end}_yearmean.nc
cdo --timestat_date last -yearmean -shifttime,-1hours -mergetime -apply,-selname,FSDS,FSR,FLDS,FIRE,EFLX_LH_TOT,FSH,QFLX_EVAP_TOT,QSOIL,QVEGE,QVEGT,QOVER,QDRAI,TOTSOILLIQ,TOTSOILICE,SOILWATER_10CM,FSNO,SNOW_DEPTH,TSOI_10CM,FPSN,TV,TG,QIRRIG [ ${sourcedir}/clm5.0_eur0.1.clm2.h0.*.nc ] ${file_h0}
cdo -selyear,${year_start}/${year_end} ${file_h0} tmp.nc
mv tmp.nc ${file_h0}

# Combine h3 and h0, and remove individual files
# Set time axis to create identical timestamps (at the beginning, e.g. 2004-01-01-00:00)
outfile=clm5.0_eur0.1.clm2.hx_${year_start}-${year_end}_yearmean.nc
cdo -merge -apply,"-setmon,1 -setday,1 -settime,00:00:00" [ ${file_h3} ${file_h0} ] $outfile

rm ${file_h3} ${file_h0}


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

cd $scriptdir

# Evaluate duration and print to log file
duration=$SECONDS
echo "$(($duration / 3600)) hours and $(($duration % 3660)) minutes elapsed."

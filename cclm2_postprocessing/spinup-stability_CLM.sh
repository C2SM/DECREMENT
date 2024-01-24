#!/bin/bash -l
#
#SBATCH --job-name="postproc"
#SBATCH --account="s1256"
#SBATCH --time=00:30:00
#SBATCH --partition=normal
#SBATCH --constraint=gpu
#SBATCH --hint=nomultithread
#SBATCH --nodes=1
#SBATCH --ntasks-per-core=1
#SBATCH --ntasks-per-node=12
#SBATCH --cpus-per-task=1
#SBATCH --output=log_spinup.out
#SBATCH --error=log_spinup.err

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
# Launch from and work on scratch to have writing permission  (project is read-only for slurm jobs)
# Transfer from scratch to project for permanent storage with rsync in the end
# Petra Sieber 2024-01-16

# Track duration
SECONDS=0

module load CDO
module load NCO

set -e # failing commands will cause the shell script to exit

source functions.sh # load functions and general case settings

#==========================================
# Merge time and create yearly means
#==========================================

sourcedir=$SCRATCH/${case_source}/20_cclm2_c
destdir=$scriptdir/cclm2_output_processed/${case_dest}/clm/yearly
mkdir -p $destdir

cd ${sourcedir}

# Shift time by 1h to get periods in means correct because CLM marks interval ends (e.g. daily output for 31-12-2000 is stamped 01-01-2001-00:00 and CDO does not condsider time bounds)
# Keep only unique variables

# For h3 monthly (yearmonmean for correct weighting)
file_h3=clm5.0_eur0.1.clm2.h3_${year_start}-${year_end}_yearmean.nc
cdo --timestat_date first -yearmonmean -shifttime,-1hours -mergetime -apply,-selname,TSKIN,TSA,TREFMNAV,TREFMXAV,TBOT,RAIN,SNOW,Q2M,U10,TLAI [ clm5.0_eur0.1.clm2.h3.*.nc ] ${destdir}/${file_h3}

# For h0, h1, h2 daily (keep only variables that are not on h3, remove single timestamp in 2003 and excess in 2016)
file_h0=clm5.0_eur0.1.clm2.h0_${year_start}-${year_end}_yearmean.nc
cdo --timestat_date first -yearmean -shifttime,-1hours -mergetime -apply,-selname,FSDS,FSR,FLDS,FIRE,EFLX_LH_TOT,FSH,QFLX_EVAP_TOT,QSOIL,QVEGE,QVEGT,QOVER,QDRAI,TOTSOILLIQ,TOTSOILICE,SOILWATER_10CM,FSNO,SNOW_DEPTH,TSOI_10CM,FPSN,TV,TG,QIRRIG [ clm5.0_eur0.1.clm2.h0.*.nc ] ${destdir}/${file_h0}
cd ${destdir}
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

postproc_clm $outfile

# Evaluate duration and print to log file
duration=$SECONDS
echo "$(($duration / 3600)) hours and $(($duration % 3600 /60)) minutes elapsed."

# Finish
cd $scriptdir

#==========================================
# Rsync to PROJECT
#==========================================

# Do this in a separate xfer job!

#echo -e "\n *** RSYNC *** \n"
#rsync -av --progress $scriptdir/cclm2_output_processed $PROJECT/

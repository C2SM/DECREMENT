#!/bin/bash -l
#
#SBATCH --job-name="post_cosmo"
#SBATCH --account="s1256"
#SBATCH --time=16:00:00
#SBATCH --partition=normal
#SBATCH --constraint=gpu
#SBATCH --hint=nomultithread
#SBATCH --nodes=1
#SBATCH --ntasks-per-core=1
#SBATCH --ntasks-per-node=12
#SBATCH --cpus-per-task=1
#SBATCH --output=log_postproc_COSMO.out
#SBATCH --error=log_postproc_COSMO.err

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export CRAY_CUDA_MPS=1

# Postprocessing of CCLM2 output from COSMO (takes ca 7h)
# Launch from and work on scratch to have writing permission  (project is read-only on compute nodes)
# Transfer from scratch to project for permanent storage with rsync in a separate xfer job
# Petra Sieber 2024-01-22

module load CDO
module load NCO

set -e # failing commands will cause the shell script to exit

source functions.sh # load functions and general case settings

#==========================================
#          (1) COSMO DAILY ACCUM 2D
#==========================================

echo -e "\n *** (1) COSMO DAILY *** \n"

# Track duration
SECONDS=0

sourcedir=$SCRATCH/${case_source}/20_cclm2_c/cosmo_output/daily_2D
destdir=$scriptdir/cclm2_output_processed/${case_dest}/cosmo/daily
mkdir -p $destdir

#==========================================
# Save time-invariant auxiliary variables
#==========================================

cd ${sourcedir}
cdo -selname,rotated_pole,slonu,slatu,slonv,slatv,vcoord lffd${year_start}0101*.nc ${destdir}/cosmo_auxiliaries.nc

#==========================================
# Merge time per year
#==========================================

for year in $years; do
    # Merge time and select variables, creates 5 GB files and takes ca 2 min
    # Shift timestamp to the beginning of the accumulation interval to mark the correct day for averaging and splitting
    cd ${sourcedir}
    let year_after=${year}+1
    cdo -selyear,${year} -shifttime,-1day -mergetime -apply,-selname,ASWDIR_S,ASWDIFD_S,ASWDIFU_S,ATHD_S,ATHU_S,ALHFL_S,ASHFL_S,AEVAP_S,ATHB_T,ASOD_T,ASOB_T,TOT_PREC,RAIN_CON,SNOW_CON,T_2M_AV,TMIN_2M,TMAX_2M,VMAX_10M,VABSMX_10M,U_10M_AV,V_10M_AV [ $(ls lffd${year}*.nc) $(ls lffd${year_after}0101*.nc) ] ${destdir}/tmp1_${year}.nc
    
    # Remove the time bounds which are incorrect afer shifting (first variable attribute, then variable)
    cd ${destdir}
    ncatted -O -a bounds,time,d,, tmp1_${year}.nc tmp2_${year}.nc
    cdo -delname,time_bnds tmp2_${year}.nc tmp3_${year}.nc
done

#==========================================
# Basic post-processing of COSMO output
#==========================================

cd ${destdir}
for year in $years; do
    postproc_cosmo tmp3_${year}.nc lffd_${year}_daily.nc
done

rm tmp*.nc

# Evaluate duration and print to log file
duration=$SECONDS
echo -e "Finished (1) COSMO DAILY"
echo "$(($duration / 3600)) hours and $(($duration % 3600 /60)) minutes elapsed."

#==========================================
#          (2) COSMO 3h INST 2D
#==========================================

echo -e "\n *** (2) COSMO 3h 2D *** \n"

# Track duration
SECONDS=0

sourcedir=$SCRATCH/${case_source}/20_cclm2_c/cosmo_output/3h_2D
destdir=$scriptdir/cclm2_output_processed/${case_dest}/cosmo/daily_avg
mkdir -p $destdir

#==========================================
# Merge time per year
#==========================================

for year in $years; do
    # Merge time and select variables, creates 3 GB files and takes ca 6 min
    # Shift timestamp to the beginning of the accumulation interval to mark the correct day for averaging and splitting
    # Calculate daily mean
    cd ${sourcedir}
    let year_after=${year}+1
    cdo --timestat_date first -daymean -selyear,${year} -shifttime,-3 -mergetime -apply,-selname,T_2M,T_G,QV_2M,RELHUM_2M,TQV,TQI,TQC,CLCT,PS,HPBL [ $(ls lffd${year}*.nc) $(ls lffd${year_after}0101*.nc) ] ${destdir}/tmp1_${year}.nc
    
    # Remove the time bounds which are incorrect afer shifting (first variable attribute, then variable)
    cd ${destdir}
    ncatted -O -a bounds,time,d,, tmp1_${year}.nc tmp2_${year}.nc
    cdo -delname,time_bnds tmp2_${year}.nc tmp3_${year}.nc
done

#==========================================
# Basic post-processing of COSMO output
#==========================================

cd ${destdir}
for year in $years; do
    postproc_cosmo tmp3_${year}.nc lffd_${year}_daymean.nc
done

rm tmp*.nc

# Evaluate duration and print to log file
duration=$SECONDS
echo -e "Finished (2) COSMO 3h 2D"
echo "$(($duration / 3600)) hours and $(($duration % 3600 /60)) minutes elapsed."

#==========================================
#          (3) COSMO 3h INST PLEV
#==========================================

echo -e "\n *** (3) COSMO 3h PLEV *** \n"

# Track duration
SECONDS=0

sourcedir=$SCRATCH/${case_source}/20_cclm2_c/cosmo_output/3h_plev
destdir=$scriptdir/cclm2_output_processed/${case_dest}/cosmo/daily_avg_plev
mkdir -p $destdir

#==========================================
# Merge time per year
#==========================================

for year in $years; do
    # Merge time and select variables, creates 3 GB files and takes ca 6 min
    # Shift timestamp to the beginning of the accumulation interval to mark the correct day for averaging and splitting
    # Calculate daily mean
    cd ${sourcedir}
    let year_after=${year}+1
    cdo --timestat_date first -daymean -selyear,${year} -shifttime,-3 -mergetime -apply,-selname,T,QV,FI [ $(ls lffd${year}*.nc) $(ls lffd${year_after}0101*.nc) ] ${destdir}/tmp1_${year}.nc
    
    # Remove the time bounds which are incorrect afer shifting (first variable attribute, then variable)
    cd ${destdir}
    ncatted -O -a bounds,time,d,, tmp1_${year}.nc tmp2_${year}.nc
    cdo -delname,time_bnds tmp2_${year}.nc tmp3_${year}.nc
done

#==========================================
# Basic post-processing of COSMO output
#==========================================

cd ${destdir}
for year in $years; do
    postproc_cosmo tmp3_${year}.nc lffd_plev_${year}_daymean.nc
done

rm tmp*.nc

# Evaluate duration and print to log file
duration=$SECONDS
echo -e "Finished (3) COSMO 3h PLEV"
echo "$(($duration / 3600)) hours and $(($duration % 3600 /60)) minutes elapsed."

#==========================================
#          (4) COSMO MONTHLY
#==========================================

echo -e "\n *** (4) COSMO MONTHLY *** \n"

# Track duration
SECONDS=0

destdir=$scriptdir/cclm2_output_processed/${case_dest}/cosmo/monthly
mkdir -p $destdir

#==========================================
# Merge time and tapes
#==========================================

# Daily and daily_avg (not plev), average the already processed data
# Merge time per tape and average
cd $scriptdir/cclm2_output_processed/${case_dest}/cosmo/daily
cdo -L --timestat_date first -monmean -mergetime [ lffd* ] ${destdir}/tmp_daily.nc
cd $scriptdir/cclm2_output_processed/${case_dest}/cosmo/daily_avg
cdo -L --timestat_date first -monmean -mergetime [ lffd* ] ${destdir}/tmp_daily_avg.nc

# Merge tapes
outfile=lffd_${year_start}-${year_end}_monmean.nc
cdo -merge [ tmp_daily.nc tmp_daily_avg.nc ] $outfile

rm tmp*.nc

# Evaluate duration and print to log file
duration=$SECONDS
echo -e "Finished (4) COSMO MONTHLY"
echo "$(($duration / 3600)) hours and $(($duration % 3600 /60)) minutes elapsed."

#==========================================
#          (5) COSMO CASE_DOCS
#==========================================

echo -e "\n *** (5) COSMO CASE_DOCS *** \n"

# Track duration (ca 2 min)
SECONDS=0

destdir=$scriptdir/cclm2_output_processed/${case_dest}/cosmo/case_docs
mkdir -p $destdir/indata
mkdir -p $destdir/namelists
mkdir -p $destdir/config

#==========================================
# Collect files
#==========================================

# COSMO input
rsync -av $casedir/bin/extpar_EUR11_COPAT2.nc $destdir/indata/

# COSMO namelists
rsync -av $casedir/20_cclm2_c/INPUT_* $destdir/namelists/
rsync -av $casedir/20_cclm2_c/YU* $destdir/namelists/

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

# Evaluate duration and print to log file
duration=$SECONDS
echo -e "Finished (4) COSMO CASE_DOCS"
echo "$(($duration / 3600)) hours and $(($duration % 3600 /60)) minutes elapsed."

# Finish
cd $scriptdir

#==========================================
# Rsync to PROJECT
#==========================================

# Do this in a separate xfer job!

echo -e "\n *** RSYNC *** \n"
rsync -av --progress $scriptdir/cclm2_output_processed $PROJECT/


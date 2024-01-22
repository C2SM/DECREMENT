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

# Postprocessing of CCLM2 output from COSMO (takes ca 7h)
# Launch from and work on scratch to have writing permission  (project is read-only for slurm jobs)
# Transfer from scratch to project for permanent storage with rsync in the end
# Petra Sieber 2024-01-22

module load cray-hdf5
module load cray-netcdf
module load CDO
module load NCO

#==========================================
# Case settings
#==========================================

case_source=cclm2_EUR11_hist-spinup          # case name on SCRATCH
#case_source=$(basename "$(dirname "${PWD}")") # extract case name
year_ini=2004                                 # initial date of the simulation, incl spin-up
year_start=2006                               # start of the analysis period, excl spin-up or min 10 years
year_end=2015                                 # end of the analysis period
case_dest=cclm2_EUR11_historical              # case name on PROJECT
#case_dest=$case_source

scriptdir=$PWD
casedir=$SCRATCH/${case_source}

years="$(seq $year_start $year_end)"

#==========================================
#          (1) COSMO DAILY ACCUM 2D
#==========================================

echo -e "\n *** (1) COSMO DAILY *** \n"

# Track duration
SECONDS=0

sourcedir=$SCRATCH/${case_source}/20_cclm2_c/cosmo_output/daily_2D
destdir=$scriptdir/cclm2_output_processed/${case_dest}/cosmo/daily
mkdir -p $destdir

# Work locally in the destination directory (affects netcdf history)
cd ${destdir}

#==========================================
# Save time-invariant auxiliary variables
#==========================================

cdo -selname,rotated_pole,slonu,slatu,slonv,slatv,vcoord ${sourcedir}/lffd${year_start}0101*.nc lffd_auxiliaries.nc

#==========================================
# Merge time per year
#==========================================

for year in $years; do
    # Merge time and select variables, creates 5 GB files and takes ca 2 min
    # Shift timestamp to the beginning of the accumulation interval to mark the correct day for averaging and splitting
    let year_after=${year}+1
    cdo -selyear,${year} -shifttime,-1day -mergetime -apply,-selname,ASWDIR_S,ASWDIFD_S,ASWDIFU_S,ATHD_S,ATHU_S,ALHFL_S,ASHFL_S,AEVAP_S,ATHB_T,ASOD_T,ASOB_T,TOT_PREC,RAIN_CON,SNOW_CON,T_2M_AV,TMIN_2M,TMAX_2M,VMAX_10M,VABSMX_10M,U_10M_AV,V_10M_AV [ $(ls ${sourcedir}/lffd${year}*.nc) ${sourcedir}/lffd${year_after}0101000000.nc ] tmp1_${year}.nc
    
    # Remove the time bounds which are incorrect afer shifting (first variable attribute, then variable)
    ncatted -O -a bounds,time,d,, tmp1_${year}.nc tmp2_${year}.nc
    cdo -delname,time_bnds tmp2_${year}.nc tmp3_${year}.nc
done

#==========================================
# Basic post-processing of COSMO output
#==========================================

for year in $years; do
    # Cut away sponge zone (13 cells per side)
    ncks -O -d rlon,14,-13 -d rlat,14,-13 tmp3_${year}.nc tmp4_${year}.nc
    
    # Remap to EUR11 0.1° lonlat grid (15 GB)
    cdo -remapbil,grid_EUR11_lonlat.txt tmp4_${year}.nc tmp5_${year}.nc

    # Compress (6 GB)
    ncks -7 -L 1 tmp5_${year}.nc lffd_${year}_daily.nc
done

rm tmp*.nc

# Finish
cd $scriptdir

# Evaluate duration and print to log file
duration=$SECONDS
echo -e "\n Finished (1) COSMO DAILY"
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

# Work locally in the destination directory (affects netcdf history)
cd ${destdir}

#==========================================
# Merge time per year
#==========================================

for year in $years; do
    # Merge time and select variables, creates 3 GB files and takes ca 6 min
    # Shift timestamp to the beginning of the accumulation interval to mark the correct day for averaging and splitting
    # Calculate daily mean
    let year_after=${year}+1
    cdo --timestat_date first -daymean -selyear,${year} -shifttime,-3 -mergetime -apply,-selname,T_2M,T_G,QV_2M,RELHUM_2M,TQV,TQI,TQC,CLCT,PS,HPBL [ $(ls ${sourcedir}/lffd${year}*.nc) ${sourcedir}/lffd${year_after}0101*.nc ] tmp1_${year}.nc
    
    # Remove the time bounds which are incorrect afer shifting (first variable attribute, then variable)
    ncatted -O -a bounds,time,d,, tmp1_${year}.nc tmp2_${year}.nc
    cdo -delname,time_bnds tmp2_${year}.nc tmp3_${year}.nc
done

#==========================================
# Basic post-processing of COSMO output
#==========================================

for year in $years; do
    # Cut away sponge zone (13 cells per side)
    ncks -O -d rlon,14,-13 -d rlat,14,-13 tmp3_${year}.nc tmp4_${year}.nc

    # Remap to EUR11 0.1° lonlat grid
    cdo -remapbil,grid_EUR11_lonlat.txt tmp4_${year}.nc tmp5_${year}.nc

    # Compress
    ncks -7 -L 1 tmp5_${year}.nc lffd_${year}_daymean.nc

done

rm tmp*.nc

# Finish
cd $scriptdir

# Evaluate duration and print to log file
duration=$SECONDS
echo -e "\n Finished (2) COSMO 3h 2D"
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

# Work locally in the destination directory (affects netcdf history)
cd ${destdir}

#==========================================
# Merge time per year
#==========================================

for year in $years; do
    # Merge time and select variables, creates 3 GB files and takes ca 6 min
    # Shift timestamp to the beginning of the accumulation interval to mark the correct day for averaging and splitting
    # Calculate daily mean
    let year_after=${year}+1
    cdo --timestat_date first -daymean -selyear,${year} -shifttime,-3 -mergetime -apply,-selname,T,QV,FI [ $(ls ${sourcedir}/lffd${year}*.nc) ${sourcedir}/lffd${year_after}0101*.nc ] tmp1_${year}.nc
    
    # Remove the time bounds which are incorrect afer shifting (first variable attribute, then variable)
    ncatted -O -a bounds,time,d,, tmp1_${year}.nc tmp2_${year}.nc
    cdo -delname,time_bnds tmp2_${year}.nc tmp3_${year}.nc
done

#==========================================
# Basic post-processing of COSMO output
#==========================================

for year in $years; do
    # Cut away sponge zone (13 cells per side)
    ncks -O -d rlon,14,-13 -d rlat,14,-13 tmp3_${year}.nc tmp4_${year}.nc

    # Remap to EUR11 0.1° lonlat grid
    cdo -remapbil,grid_EUR11_lonlat.txt tmp4_${year}.nc tmp5_${year}.nc

    # Compress
    ncks -7 -L 1 tmp5_${year}.nc lffd_plev_${year}_daymean.nc

done

rm tmp*.nc

# Finish
cd $scriptdir

# Evaluate duration and print to log file
duration=$SECONDS
echo -e "\n Finished (3) COSMO 3h PLEV"
echo "$(($duration / 3600)) hours and $(($duration % 3600 /60)) minutes elapsed."

#==========================================
#          (4) COSMO MONTHLY
#==========================================

echo -e "\n *** (4) COSMO MONTHLY *** \n"

# Track duration
SECONDS=0

destdir=$scriptdir/cclm2_output_processed/${case_dest}/cosmo/monthly
mkdir -p $destdir

# Work locally in the destination directory (affects netcdf history)
cd ${destdir}

#==========================================
# Merge time and tapes
#==========================================

# Daily and daily_avg (not plev), average the already processed data

# Merge time per tape and average
cdo --timestat_date first -monmean -mergetime [ $scriptdir/cclm2_output_processed/${case_dest}/cosmo/daily/* ] tmp_daily.nc
cdo --timestat_date first -monmean -mergetime [ $scriptdir/cclm2_output_processed/${case_dest}/cosmo/daily_avg/* ] tmp_daily_avg.nc

# Merge tapes
outfile=lffd_${year_start}-${year_end}_monmean.nc
cdo -merge [ tmp_daily.nc tmp_daily_avg.nc ] $outfile

rm tmp*.nc

# Finish
cd $scriptdir

# Evaluate duration and print to log file
duration=$SECONDS
echo -e "\n Finished (4) COSMO MONTHLY"
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
rsync -av $sourcedir/INPUT_* $destdir/namelists/
rsync -av $sourcedir/YU* $destdir/namelists/

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
echo -e "\n Finished (4) COSMO CASE_DOCS"
echo "$(($duration / 3600)) hours and $(($duration % 3600 /60)) minutes elapsed."

#==========================================
# Rsync to PROJECT
#==========================================

echo -e "\n *** RSYNC *** \n"
rsync -av --progress $scriptdir/cclm2_output_processed $PROJECT/


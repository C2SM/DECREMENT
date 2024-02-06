#!/bin/bash -l

#SBATCH --job-name="postproc"
#SBATCH --account="s1256"
#SBATCH --time=00:30:00
#SBATCH --output=log_postproc.out
#SBATCH --error=log_postproc.err
#SBATCH --partition=prepost
#SBATCH --constraint=mc
#SBATCH --hint=nomultithread

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

# Minimal post-processing of CLM output for Dirk
# Petra Sieber 01/2024

module load cray-hdf5
module load cray-netcdf
module load CDO
module load NCO

scriptdir=$PWD

cases=("cclm2_EUR11_FB_ssp1" "cclm2_EUR11_FB_nfn" "cclm2_EUR11_FB_nfs" "cclm2_EUR11_FB_nac")
#case=cclm2_EUR11_FB_nfs
year_start=2041
year_end=2043
years="$(seq $year_start $year_end)"

# For h3 monthly

for case in ${cases[@]}; do
    destdir=$SCRATCH/for_dirk/${case}
    mkdir -p $destdir
    cd $destdir

    for year in $years; do
        # Create a copy and work locally in the destination directory (affects netcdf history)
        rsync -av $SCRATCH/${case}/20_cclm2_c/clm5.0_eur0.1.clm2.h3.${year}-02-01-00000.nc .
        infile=clm5.0_eur0.1.clm2.h3.${year}-02-01-00000.nc
        
        # Modify global file attribute
        ncatted -O -h -a source,global,m,c,"Community Land Model CLM5.0" $infile tmp1_${infile}
        
        # Delete unneeded variables/coordinates
        # h0: levdcmp,levlak,ZSOI,DZSOI,WATSAT,SUCSAT,BSW,HKSAT,ZLAKE,DZLAKE
        # h3: levdcmp,levlak
        cdo -O --no_history -delname,levdcmp,levlak tmp1_${infile} tmp2_${infile}
    
        # Rearrange longitude variable from 0..360 deg to -180..180 deg
        # cdo -O -sellonlatbox,-180,180,-90,90 $infile $tmpfile
        # Use ncap2 arithmetics with rounding to preserve 2 digit precision
        ncap2 -O -s 'where(lon>180) lon=round((lon-360)*100)/100' tmp2_${infile} tmp3_${infile}
        
        # Cut away sponge zone
        # EUR11 lonlat bounds excl. sponge zone for clipping
        lonmin=-44.55
        lonmax=64.95
        latmin=21.95
        latmax=72.65 # +0.1 for NCO to clip at 72.55
        ncks -O -h -d lon,${lonmin},${lonmax} -d lat,${latmin},${latmax} tmp3_${infile} tmp4_${infile}

        mv tmp4_${infile} $infile
        rm tmp*_${infile}
    
    done
    cd $scriptdir

done

#for case in ${cases[@]}; do
#    destdir=$SCRATCH/for_dirk/${case} 
#    rm $destdir/tmp_*.nc
#done




#!/bin/bash -l
#
#SBATCH --job-name="post_pack"
#SBATCH --account="s1256"
#SBATCH --time=24:00:00
#SBATCH --partition=normal
#SBATCH --constraint=gpu
#SBATCH --hint=nomultithread
#SBATCH --nodes=1
#SBATCH --ntasks-per-core=1
#SBATCH --ntasks-per-node=12
#SBATCH --cpus-per-task=1
#SBATCH --output=log_pack.out
#SBATCH --error=log_pack.err

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export CRAY_CUDA_MPS=1

# Compress and archive (tar) CCLM2 output for permanent storage on STORE
# Launch from and work on SCRATCH to have writing permission (store is read-only for slurm jobs)
# Transfer from SCRATCH to STORE for permanent storage with rsync in the end
# Petra Sieber 2024-01-24

module load CDO
module load NCO

set -e # failing commands will cause the shell script to exit

#==========================================
# Case settings
#==========================================

case_source=cclm2_EUR11_hist-spinup           # case name on SCRATCH
#case_source=$(basename "$(dirname "${PWD}")") # extract case name
year_ini=2004                                 # initial date of the simulation, incl spin-up
year_start=2006                               # start of the analysis period, excl spin-up or min 10 years
year_end=2015                                 # end of the analysis period
case_dest=cclm2_EUR11_historical              # case name on PROJECT
#case_dest=$case_source

scriptdir=$PWD
casedir=$SCRATCH/${case_source}
sourcedir=$SCRATCH/${case_source}/20_cclm2_c

years="$(seq $year_ini $year_end)"

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


##########
export PACK_COMPRESS_LEVEL=1
export PACK_CREATE_ARCHIVE=true
export PACK_REMOVE_ORIGINAL=true

# Load general defaults
source tools.sh

# Check if output folder exists
mkdir -p $(readlink output)

# Go to input dir
cd input

let year_fin=${year_end}+1

LM_COMPRESS_DIRS="daily_2D 3h_2D 3h_plev"
LM_BEGIN_DATE="${year_ini}-01-01T00:00"
LM_END_DATE="${year_fin}-01-01T00:00"

# Compress, archive and remove originals per year

for year in $years; do

for direc in ${LM_COMPRESS_DIRS}; do
    echo ${direc}
    nc_list=$(cosmo_file_list "${direc}" ">=" "${LM_BEGIN_DATE}" "<" "${LM_END_DATE}")
    if [[ -n ${nc_list} ]]; then
        # compress files
        echo "Compressing files"
        nfiles=0
        nerr=0
        nz_list=""
        for file in ${nc_list}; do
            ((nfiles++))
            echo "  $file"
            ofile=${file%.nc}.nz
            ncks -4 -L ${PACK_COMPRESS_LEVEL} --no_abc -O ${file} ${ofile}
            status=$?
            [[ -e ${ofile} && $status == 0 ]] && nz_list+=" ${ofile}" || ((nerr++))
        done
        if (( nerr > 0 )); then
            echo "something went wrong when compressing ${nerr}/${nfiles} files, exiting"
            exit 1
        fi

        if [[ ${PACK_CREATE_ARCHIVE} == true ]]; then
            # create archive
            part=$(dirname ${direc})
            stream=$(basename ${direc})
            tar_file="../output/${part}__${stream}__${LM_BEGIN_DATE_DG}-${LM_END_DATE_DG}.tar"
            echo "building archive file ${tar_file}"
            printf '%s\n' ${nz_list} > .nz_list.txt
            tar --transform='s/.nz/.nc/' -cvf ${tar_file} --files-from .nz_list.txt
            status=$?
            if [[ $status != 0 ]]; then
                echo "something went wrong when creating archive, exiting"
                exit $status
            fi

            # remove compressed and original files
            if [[ ${PACK_REMOVE_ORIGINAL} == true ]]; then
                echo "removing compressed and original files"
                nfiles=0
                nerr=0
                for file in ${nc_list} ${nz_list}; do
                    ((nfiles++))
                    rm -f ${file}
                    [[ $? != 0 ]] && ((nerr++))
                done
                if (( nerr > 0 )); then
                    echo "something went wrong when removing ${nerr}/${nfiles} compressed and original files, exiting"
                    exit 1
                fi
            fi
        else
            # Replace original nc files by compressed files
            echo "replacing original nc files by compressed files"
            nfiles=0
            nerr=0
            for file in ${nz_list}; do
                ((nfiles++))
                mv ${file} ${file%.nz}.nc
                [[ $? != 0 ]] && ((nerr++))
            done
            if (( nerr > 0 )); then
                echo "something went wrong when replacing ${nerr}/${nfiles} original nc files by compressed files, exiting"
                exit 1
            fi
        fi
    else
        echo "no nc files found in ${direc} between ${LM_BEGIN_DATE_FR} and ${LM_END_DATE_FR}"
    fi
done

# Update job status
update_status "done"

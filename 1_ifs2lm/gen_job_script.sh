#!/bin/bash

cat > job <<EOF_job
#!/bin/bash
#SBATCH --job-name=ifs2lm
#SBATCH --output=job_${LM_BEGIN_DATE_FR}_${LM_END_DATE_FR}.out
#SBATCH --nodes=${LM_IFS2LM_NODES}
#SBATCH --ntasks-per-node=${LM_NTASKS_PER_NODE_INT2LM}
#SBATCH --partition=${QUEUE}
#SBATCH --time=${NQS_ELAPSED_IFS2LM}
#SBATCH --account=${ACCOUNT}

# Initialization
set verbose
set echo

# clean
./clean

# set environmental parameters
export OMP_NUM_THREADS=1
export MALLOC_MMAP_MAX_=0
export MALLOC_TRIM_THRESHOLD_=536870912
export MV2_ENABLE_AFFINITY=0

# Set this to avoid segmentation faults
ulimit -s unlimited
ulimit -a

unset G2G
export MV2_USE_CUDA=0
export MV2_USE_GPUDIRECT=0
export MV2_USE_GPUDIRECT_GDRCOPY=0

# load spack environment
if [[ -n "\${LM_INT2LM_ENV}" ]]; then
    source ../\${LM_INT2LM_ENV}
elif [[ -n "\${LM_INT2LM_SPEC}" ]]; then
    spack load \${LM_INT2LM_SPEC}
fi

# echo date
date

# write namelists
ifs2lm_INPUT

# Check if output folder exists
mkdir -p $(readlink output)

# Run LM in case directory
${RUNCMD} -u ${EXE_INT2LM}

# Add S_ORO_MAX if missing
module load NCO
cd output
for laf_file in laf*.nc ; do
    ncdump -h \${laf_file} | grep -q S_ORO_MAX
    if [[ \$? != 0 ]]; then
        echo "S_ORO_MAX not in \${laf_file}, copying S_ORO as S_ORO_MAX"
        ncks -O -v S_ORO \${laf_file} tmp.nc
        ncrename -v S_ORO,S_ORO_MAX tmp.nc
        ncks -A -v S_ORO_MAX tmp.nc \${laf_file}
    fi
done
rm tmp.nc
cd -

# echo date
date

sacct -j \$SLURM_JOB_ID --format=User,JobID,Jobname,partition,state,time,start,end,elapsed,MaxRss,MaxVMSize,nnodes,ncpus,nodelist,AveCPUFreq
EOF_job

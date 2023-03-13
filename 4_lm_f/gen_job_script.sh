#!/bin/bash

cat > job <<EOF_job
#!/bin/bash
#SBATCH --job-name=lm_f
#SBATCH --output=job_${LM_BEGIN_DATE_FR}_${LM_END_DATE_FR}.out
#SBATCH --nodes=${LM_COSMO_NODES_F}
#SBATCH --ntasks-per-node=${LM_NTASKS_PER_NODE_COSMO}
#SBATCH --partition=${QUEUE}
#SBATCH --time=${NQS_ELAPSED_LM_F}
#SBATCH --account=${ACCOUNT}

# Initialization
set verbose
set echo

# clean
./clean

# Set this to avoid segmentation faults
ulimit -s unlimited
ulimit -a

# set environmental parameters
export MALLOC_MMAP_MAX_=0
export MALLOC_TRIM_THRESHOLD_=536870912
export OMP_NUM_THREADS=1
EOF_job

if [[ $COSMO_TARGET == "gpu" ]]; then
    cat >> job <<EOF_job
export COSMO_NPROC_NODEVICE=${NQS_NIOLM_F}
export MPICH_RDMA_ENABLED_CUDA=1
export MPICH_G2G_PIPELINE=256
# KO: Need to set path to cuda library explicitly or GPU runs fail on cuInit
# export LD_LIBRARY_PATH="\$LD_LIBRARY_PATH\:/opt/cray/nvidia/default/lib64"
EOF_job
fi

cat >> job <<EOF_job
# load spack environment
if [[ -n "\${LM_COSMO_ENV}" ]]; then
    source ../\${LM_COSMO_ENV}
elif [[ -n "\${LM_COSMO_SPEC}" ]]; then
    spack load \${LM_COSMO_SPEC}
fi
# echo date
date

# write namelists
lm_f_INPUT_ORG
lm_f_INPUT_DYN
lm_f_INPUT_PHY
lm_f_INPUT_IO
lm_f_INPUT_DIA
lm_f_INPUT_INI
lm_f_INPUT_SAT
lm_f_INPUT_ASS

# create ydir directories
mkydirs INPUT_IO

# Run LM in case directory
${RUNCMD} -u ${EXE_COSMO}

# echo date
date

#Print accounting file
sacct -j \$SLURM_JOB_ID --format=User,JobID,Jobname,partition,state,time,start,end,elapsed,MaxRss,MaxVMSize,nnodes,ncpus,nodelist,AveCPUFreq
EOF_job

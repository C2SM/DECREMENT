#!/bin/bash

cat > job <<EOF_job
#!/bin/bash
#SBATCH --job-name=compress
#SBATCH --output=job.out
#SBATCH --nodes=1
#SBATCH --cpus-per-task=12
#SBATCH --ntasks=1
#SBATCH --partition=${QUEUE}
#SBATCH --time=${NQS_ELAPSED_COMPRESS}
#SBATCH --account=${ACCOUNT}

# Load modules
export OMP_NUM_THREADS=$CORES_PER_NODE
export CRAY_CUDA_MPS=1
module load daint-gpu
module load NCO

# Compress, archive and remove originals
./pack_data.sh ${LM_BEGIN_DATE} ${LM_END_DATE} "${LM_COMPRESS_DIRS}"

EOF_job

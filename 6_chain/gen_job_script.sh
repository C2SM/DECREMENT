#!/bin/bash

cat > job <<EOF_job
#!/bin/bash -l
#
#SBATCH --job-name=chain
#SBATCH --output=job.out
#SBATCH --time=00:05:00
#SBATCH --nodes=1
#SBATCH --partition=normal
#SBATCH --account=${ACCOUNT}

cd ..
./run_daint.sh
EOF_job

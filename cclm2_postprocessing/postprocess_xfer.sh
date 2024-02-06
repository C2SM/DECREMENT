#!/bin/bash -l
#
#SBATCH --job-name="post_xfer"
#SBATCH --time=01:00:00
#SBATCH --partition=xfer
#SBATCH --hint=nomultithread
#SBATCH --nodes=1
#SBATCH --output=log_postproc_xfer.out
#SBATCH --error=log_postproc_xfer.err

# Track duration
SECONDS=0

#---------------------------------------------------------------
# Transfer from scratch to project for permanent storage
#---------------------------------------------------------------

rsync -av cclm2_output_processed $PROJECT/


# Evaluate duration and print to log file
duration=$SECONDS
echo "$(($duration / 3600)) hours and $(($duration % 3600 /60)) minutes elapsed."

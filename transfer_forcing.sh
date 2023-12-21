#!/bin/bash -l
#SBATCH --job-name="transfer"
#SBATCH --time=03:00:00
#SBATCH --partition=xfer
#SBATCH --hint=nomultithread
#SBATCH --nodes=1
#SBATCH --output=log_transfer_forcing.out
#SBATCH --error=log_transfer_forcing.err

# Track duration
SECONDS=0

# Transfer from project to scratch for runtime access
# cas input files to INT2LM (output of 00_get_data)
rsync -av --progress /project/s1256/shared/gcm_forcing $SCRATCH/

# laf/lbfd input files to COSMO (output of INT2LM)
#rsync -av --progress /project/s1256/shared/cosmo_bc_ssp1 $SCRATCH/

# Evaluation duration and print to log file
duration=$SECONDS
echo "$(($duration / 60)) minutes and $(($duration % 60)) seconds elapsed."

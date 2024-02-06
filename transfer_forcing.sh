#!/bin/bash -l
#
#SBATCH --job-name="transfer_ssp1"
#SBATCH --time=24:00:00
#SBATCH --partition=xfer
#SBATCH --hint=nomultithread
#SBATCH --nodes=1
#SBATCH --output=log_transfer_ssp1.out
#SBATCH --error=log_transfer_ssp1.err

# Track duration
SECONDS=0

#---------------------------------------------------------------
# Transfer from scratch to project for permanent storage
#---------------------------------------------------------------
# laf/lbfd input files for COSMO (output of INT2LM)
# Historical 2004-2015 (ca 6h)
#rsync -av --progress $SCRATCH/cclm2_EUR11_hist-spinup/10_ifs2lm/output/* /project/s1256/psieber/CCLM2_preprocessing/COSMO_boundary/INT2LM_output_ERA5

# Future SSP1 2034-2050 (ca 6h)
rsync -av --progress $SCRATCH/cclm2_EUR11_future_BC/10_ifs2lm/output/* /project/s1256/psieber/CCLM2_preprocessing/COSMO_boundary/INT2LM_output_MPI-ESM-HR_ssp126

#---------------------------------------------------------------
# Transfer from project to scratch for runtime access
#---------------------------------------------------------------
# cas input files for INT2LM
# Historical: output of 00_get_data, not transferred

# Future SSP1
#rsync -av --progress /project/s1256/psieber/CCLM2_preprocessing/COSMO_boundary/gcm_forcing $SCRATCH/COSMO_boundary/

# laf/lbfd input files for COSMO (output of INT2LM)
#rsync -av --progress /project/s1256/psieber/CCLM2_preprocessing/COSMO_boundary/INT2LM_output_ERA5 $SCRATCH/COSMO_boundary/
#rsync -av --progress /project/s1256/psieber/CCLM2_preprocessing/COSMO_boundary/INT2LM_output_MPI-ESM-HR_ssp126 $SCRATCH/COSMO_boundary/

# Evaluate duration and print to log file
duration=$SECONDS
echo "$(($duration / 3600)) hours and $(($duration % 3600 /60)) minutes elapsed."

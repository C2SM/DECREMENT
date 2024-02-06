#!/bin/bash -l
#
#SBATCH --job-name="arch_xfer"
#SBATCH --time=01:00:00
#SBATCH --partition=xfer
#SBATCH --hint=nomultithread
#SBATCH --nodes=1
#SBATCH --output=log_archive_xfer.out
#SBATCH --error=log_archive_xfer.err

# Track duration
SECONDS=0

#---------------------------------------------------------------
# Transfer from SCRATCH to STORE for permanent storage
#---------------------------------------------------------------

case_source=$(basename "$(dirname "${PWD}")")   # extract case name
case_dest=${case_source}                        # case name on STORE

destdir=/store/c2sm/landclim/psieber/cclm2_output_raw/${case_dest}
mkdir -p ${destdir}

# Compressed and packed nc files
rsync -av ../50_pack_data/output/* ${destdir}/

# Namelists
mkdir -p ${destdir}/cclm2_clm/clm_namelists
mkdir -p ${destdir}/cclm2_cosmo/cosmo_namelists
rsync -v cclm2_output_processed/${case_dest}/clm/case_docs/namelists/* ${destdir}/cclm2_clm/clm_namelists/
rsync -v cclm2_output_processed/${case_dest}/cosmo/case_docs/namelists/* ${destdir}/cclm2_cosmo/cosmo_namelists/


# Evaluate duration and print to log file
duration=$SECONDS
echo "$(($duration / 3600)) hours and $(($duration % 3600 /60)) minutes elapsed."

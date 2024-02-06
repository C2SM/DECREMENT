#!/bin/bash

# Transfer decrement and indata to SCRATCH, link the CCLM2 config

# USAGE
# bash transfer_decrement.sh cclm2_EUR11_hist-spinup
# bash transfer_decrement.sh cclm2_EUR11_FB_ssp1

NAME=$1     # name of the decrement directory on scratch, e.g. decrement_EUR11

# ---------------------------------------------------------------------------
# Sync decrement to scratch
# ---------------------------------------------------------------------------
mkdir -p $SCRATCH/$NAME
rsync -av $PROJECT/decrement/* $SCRATCH/$NAME/.

# Link the appropriate simulation_config; make sure that this one links the desired COSMO config (ERA5 vs. MPI-ESM-HR)
rm -f $SCRATCH/$NAME/config
ln -s ./simulation_configs/CCLM2/CCLM2_EUR11_ssp1 $SCRATCH/$NAME/config

# COSMO standalone
#rm -f $SCRATCH/$NAME/config
#ln -s ./simulation_configs/SIMULATION_EU_12km_COSMO_COPAT2 $SCRATCH/$NAME/config


# ---------------------------------------------------------------------------
# Sync cesm and oasis input data
# ---------------------------------------------------------------------------

# Sync all input data to scratch
rsync -av $PROJECT/cclm2_input_decrement $SCRATCH/

# Linking cesm_input files and oasis_input files (masks, areas, grids; rmp once generated) is done in the CCLM2 config


# ---------------------------------------------------------------------------
# For bypassing in2lm (testing)
# ---------------------------------------------------------------------------

# Sync COSMO indata from sm61 to scratch
# 50km (3 days, 2011-01-01 to 2011-01-03)
#rsync -av /project/sm61/leclairm/CCLM2_sandbox_inputdata/cosmo_input_044/* $SCRATCH/COSMO_inputdata/bc_50km/

# 12km (1 month, 2011-01-01 to 2011-02-01)
#rsync -av /project/sm61/leclairm/CCLM2_sandbox_inputdata/cosmo_input_011/* $SCRATCH/COSMO_inputdata/bc_12km/

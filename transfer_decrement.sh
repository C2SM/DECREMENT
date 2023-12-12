#!/bin/bash

# Transfer decrement and indata to SCRATCH, link the CCLM2 config

# USAGE
# bash transfer_decrement.sh cclm2_EUR11_hist-spinup

NAME=$1     # name of the decrement directory on scratch, e.g. decrement_EUR11
PROJ=/project/sm61/psieber

# ---------------------------------------------------------------------------
# Sync decrement to scratch for desired resolution
# ---------------------------------------------------------------------------
mkdir -p $SCRATCH/$NAME
rsync -av $PROJ/decrement/* $SCRATCH/$NAME/.

# Link the appropriate simulation_config
rm -f $SCRATCH/$NAME/config
ln -s ./simulation_configs/CCLM2/CCLM2_EUR11 $SCRATCH/$NAME/config

# COSMO standalone
#rm -f $SCRATCH/$NAME/config
#ln -s ./simulation_configs/SIMULATION_EU_12km_COSMO_COPAT2 $SCRATCH/$NAME/config


# ---------------------------------------------------------------------------
# Sync cesm and oasis input data
# ---------------------------------------------------------------------------

# Sync all input data to scratch
rsync -av $PROJ/cclm2_input_decrement $SCRATCH/

# Linking cesm_input files and oasis_input files (masks, areas, grids; rmp once generated) is done in the CCLM2 config


# ---------------------------------------------------------------------------
# For bypassing in2lm (testing)
# ---------------------------------------------------------------------------

# Sync COSMO indata from sm61 to scratch
# 50km (3 days, 2011-01-01 to 2011-01-03)
#rsync -av /project/sm61/leclairm/CCLM2_sandbox_inputdata/cosmo_input_044/* $SCRATCH/COSMO_inputdata/bc_50km/

# 12km (1 month, 2011-01-01 to 2011-02-01)
#rsync -av /project/sm61/leclairm/CCLM2_sandbox_inputdata/cosmo_input_011/* $SCRATCH/COSMO_inputdata/bc_12km/
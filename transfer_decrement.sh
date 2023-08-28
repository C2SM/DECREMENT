#!/bin/bash

RES=$1

# Sync to scratch
rsync -av $PROJECT/decrement/* $SCRATCH/decrement_${RES}/.

# Link the appropriate simulation_config
rm $SCRATCH/decrement_${RES}/config
ln -s $SCRATCH/decrement_${RES}/simulation_configs/CCLM2_EU-CORDEX/SIMULATION_CCLM2_EU_CORDEX_${RES} $SCRATCH/decrement_${RES}/config


# ---------------------------------------------------------------------------
# For bypassing in2lm (testing)
# ---------------------------------------------------------------------------

# Sync COSMO indata from sm61 to scratch
# 50km (3 days, 2011-01-01 to 2011-01-03)
rsync -av /project/sm61/leclairm/CCLM2_sandbox_inputdata/cosmo_input_044/* $SCRATCH/COSMO_inputdata/bc_50km/

# 12km (1 month, 2011-01-01 to 2011-02-01)
rsync -av /project/sm61/leclairm/CCLM2_sandbox_inputdata/cosmo_input_011/* $SCRATCH/COSMO_inputdata/bc_12km/

# Link the appropriate COSMO indata
cd $SCRATCH/decrement_${RES}/10_ifs2lm/output
rm *
ln -s $SCRATCH/COSMO_inputdata/bc_${RES}/* .
cd $PROJECT/decrement
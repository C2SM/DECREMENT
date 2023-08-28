#!/bin/bash

# Link the appropriate simulation_config
rm config
ln -s simulation_configs/CCLM2_EU-CORDEX/SIMULATION_CCLM2_EU_CORDEX_50km config

# Sync to scratch
rsync -av $PROJECT/decrement $SCRATCH/
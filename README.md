# COSMO sandbox
A set of scripts used to run weather and climate simulations with COSMO. It is is intended to be simple and easy to extend. For more comprehensive solution consider using the LM Package.

## Basic usage

1. Copy an `int2lm` executaeble and a `cosmo` executable to ./bin. If you want to run a stock simulation, you can get the external parameter data by runnng `./get_extpar_data.sh`. Otherwise you need to copy them to `/bin` as well an adapt the corresponding `LM_NL_EXTPAR_?` env. variable.
2. Link the corresponding configuration file like this: `ln -s simulation_configs/SIMULATION_S_ATL config` 
4. Open `./run_daint.sh`, adapt startdate and enddate of your simualtion, check that all the simulation setps you want to run are in the `parts` string.
5. Adapt the output variables in GRIBOUT, you might want to adapt the respektive `mkdir` in `./clean` 
6. Run the simulation: `./run_daint.sh`
7. Copy your data from `output/*` to `/project`

## Simualtion configuration
Basic configuration is done by environmental variables in `config`. They are collected in the folder `simulation_configs/`. Consider sharing your configs with the group. If you need to introduce new environmental variables `export ...`, remember to adapt the existing configutations accordingly so they keep running.

## Chain
If a simulation does not complete within 24h, you can split it up into smaller chucks and submit them one after the other. To this end, set the `*_INI` and `*_FINISHED` variables to the values of the entire simulation period. The corresponding `*_BEGIN`and `_END` variables are used for the current step and changed by the chain functionality.

## Retry
There is a mechanism that resubmits the entire simulation step, if int2lm or cosmo have failed., i.e. `./run_daint.sh` is called again. In that case an empty file `.retry` is written in the directory of the part that failed. 

## Cleanup
Each directory contains a cleaning script `./clean`. To clean the entire package  run `./distclean.sh`

## Asynchronous execution
The individual parts are chained together by using `--dependency=afterok:JOBID` feature of [SLURM][https://slurm.schedmd.com/sbatch.html]. The option `afterok` in each `./run` means that a dependent job is started after the parent job completed successfully. For jobs to execute in paralell this can be changed to `after`. Then the dependent job is started after the parted has started running. 

For certain scenarios it may make sense to move `0_get_data` and `1_ifs2lm` after `6_chain`. Then the infut for the next step is already computed while the current simulation still runs.


## Branch
The current branch is inteneded for COSMO-ORG.

## Modify the parts
The number of parts (`0_get_data`, `1_ifs2lm`, etc.) or their order can be changed quite easily. They are executed in ordered fashion based on the folder name. So lets say you want to modify the boundary conditions after `1_ifs2lm` (e.g, for PGW). Just make a new directory called `2_PGW` contaning the scripts. Rename the remaining directories (`2_lm_c` becomes `3_lm_c`) and then adjust the `SB_PARTS` env. variable in `config`. 

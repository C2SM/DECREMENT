# COSMO sandbox
A set of scripts used to run weather and climate simulations with COSMO. It is is intended to be simple and easy to extend. For more comprehensive solution consider using the LM Package.

## How settings are prescribed
The scripts are loading settings from 2 different files. The first one contains everything related to the simulation configuration (domain, physical parametrizations, etc). It must be the `config` file in the root directory. Either link a stock configuration, e.g. `ln -sf simulation_configs/SIMULATION_EU_CORDEX_50km config`, or create one for your own case. The second one is the `user_settings` file. It contains some dedicated settings but you can also overwrite there any setting specified in the config file (like dates, chaining interval, nodes distribution, wall time, ...). Anything in `user_config` will take precedence over the config file. The idea being to leave the config file untouched for cleaner workflow and easier git tracking.

## Basic usage
1. Copy an `int2lm` executaeble and a `cosmo` executable to ./bin or specify the `LM_INT2LM_SPEC` and `LM_COSMO_SPEC` in the `user_settings` file if you want the scripts to get the executables for you. If you want to run a stock simulation, you can get the external parameter data by runnng `./get_extpar_data.sh`. Otherwise you need to copy them to `/bin` as well and adapt the corresponding `LM_NL_EXTPAR_?` env. variable.
2. Link the corresponding configuration file like this: `ln -sf simulation_configs/SIMULATION_EU_CORDEX_50km config` 
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

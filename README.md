# COSMO sandbox
A set of scripts used to run weather and climate simulations with COSMO. It is is intended to be simple. For more comprehensive solution consider using the LM Package.

## Basic usage

1. Copy an `int2lm` executaeble and a `cosmo` executable to ./bin
2. Link the corresponding configuration file like this: `ln -s simulation_configs/SIMULATION_S_ATL config` 
3. run `./get_extpar_data.sh` to download all the external parameters  
4. Open `./run_daint.sh`, adapt startdate and enddate of your simualtion, check that all the simulation setps you want to run are in the `parts` string.
5. `./run_daint.sh`

## Simualtion configuration
Basic configuration is done by environmental variables in CONFIG. Consider sharing your configs with the group. If you need to introduce new environmental variables `export ...`, remember to adapt the existing configutations accordingly.

## Cleanup
Each directory contains a cleaning script `./clean`. To clean the entire package  run `./distclean.sh`

## Todos
- [ ] Improve Readme
- [ ] Add job for compressing output
- [ ] Add mechanism for  retrys
- [ ] Add mechanism that allows doing year-long simualtions in multiple steps (similat to Jesus solution, but less complicated. I. e. steal what works :-) )

## Branch
The current branch is inteneded for COSMO-ORG.

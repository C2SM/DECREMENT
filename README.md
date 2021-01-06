# COSMO sandbox
A set of scripts used to run weather and climate simulations with COSMO. It is is intended to be simple. For more comprehensive solution consider using the LM Package.

## Basic usage

1. Copy an `int2lm` executaeble and a `cosmo` executable to ./bin. If you want to run a stock simulation, you can get the external parameter files by runnng `./get_extpar_data.sh`. Otherwise you need to copy them to `/bin` as well an adapt the corresponding `LM_NL_EXTPAR_?` env. variable.
2. Link the corresponding configuration file like this: `ln -s simulation_configs/SIMULATION_S_ATL config` 
4. Open `./run_daint.sh`, adapt startdate and enddate of your simualtion, check that all the simulation setps you want to run are in the `parts` string.
5. Adapt the output variables in GRIBOUT, you might want to adapt the respektive `mkdir` in `./clean` 
6. Run the simulation: `./run_daint.sh`
7. Copy your data from `output/*` to `/project`

## Simualtion configuration
Basic configuration is done by environmental variables in CONFIG. Consider sharing your configs with the group. If you need to introduce new environmental variables `export ...`, remember to adapt the existing configutations accordingly.

## Cleanup
Each directory contains a cleaning script `./clean`. To clean the entire package  run `./distclean.sh`

## Todos
- [ ] Improve Readme
- [x] Add job for compressing output
- [ ] Add mechanism for  retrys
- [x] Add mechanism that allows doing year-long simualtions in multiple steps

## Branch
The current branch is inteneded for COSMO-ORG.

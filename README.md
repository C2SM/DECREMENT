# DECREMENT, The DirEctory COSMO Runtime EnvironMENT

A set of scripts used to run weather and climate simulations with COSMO. It is is intended to be simple and easy to extend. For more comprehensive solution consider using the LM Package.


## The default chain of tasks

By default, DECREMENT is intended to run a so-called *coarse* resolution domain (just refering to the outer domain, doesn't have to actaully be coarse) and potentially a *fine* nested one. Concretely the following sequence of tasks - potentially a subset of it - is executed, each in a separate folder under the root directory:
`0_get_data`transfers input data (mostly reanalysis data) for the INT2LM preprocessing software
`1_ifs2lm`produces input data for COSMO at coarse resolution.
`2_lm_c`runs COSMO on the coarse resolution domain.
`3_lm2lm`runs INT2LM with output from the coarse reolution cosmo as input data and produces input data for the fine resolution one.
`4_lm_f`runs cosmo in the fine resolution nested domain
`5_pack_data` compresses the output netcdf files and gathers them into tar balls.
`6_chain` manages the chaining of the simulation chuncks 


## How settings are prescribed

The scripts are loading settings from different locations with the following priority: default < config < user settings. The defaults can come from `defaults.sh` or `Default_namelists/*`. The idea is that the `config` file hosts settings related to the simulation configuration (domain, physical parametrizations, etc) and must be located in the rot directory. It can either be a link to a stock configuration, e.g. `ln -sf simulation_configs/SIMULATION_EU_CORDEX_50km config`, or a personal configuration written from scratch. Finally the optional but almost always needed `user_settings` file, which must also be located in the root directory, hosts settings one wants to modify or even create and takes precedence over others.


## Changing arbitrary namelist parameters

A design idea of DECREMENT is that parameters that users change most often have a corresponding environment variable defined either in `defaults.sh` or in the `config` file. For instance, `LM_NL_DT_C` is the time step for the coarse reslution domain. In order to be able to modify any parameter, the namelsts are generated in exported functions (see files in the `Default_namelists` directory). These can be redefined either in the `config`or `user_settings` file and exported from there in order to take precedence over the default ones. This is tipically the way to go to modify the output streams. If this generates too much cluttering of these files, one can always source a custom file from inside them to keep a good level of readability.


## Basic usage

1. Get executable related files:
    * Copy an `int2lm` executable and a `cosmo` executable to `./bin`. When using an executable built with spack, it's recommanded to specify the corresponding spack environment . To this end, generate the environment file with, e.g. for COSMO `spack load --sh cosmo@c2sm-features %nvhpc cosmo_target=gpu +cppdycore ^mpich%nvhpc > spack_env_cosmo.sh` and place the `spack_env_cosmo.sh`file in both directories running cosmo, i.e. `2_lm_c` and `4_lm_f`. In order to keep things tidy, one can also put this file in the `bin` directory with a more descriptive name, like `spack_env_cosmo_gpu.sh`, and link it as `spack_env_cosmo.sh` in the right directories. Same thing applies to INT2LM: if a `spack_env_int2lm.sh` file is found in `1_ifs2lm` or `3_lm2lm`, it will be sourced before submitting the corresponding job.
    * In order to run INT2LM, copy the necessary extpar file in `bin` as well. Commands to get the extpar files for the stock configurations are listed in `./get_extpar_data.sh` (no need to execute the whole file). If necessary, adapt the `LM_NL_EXTPAR_?`environment variables accordingly.
2. Link the simulation configuration file like `ln -s simulation_configs/SIMULATION_EU_CORDEX_50km config`.
3. Copy the user settings example (`cp user_settings_example user_settings`) where some common settings are commented out with minimal documentation. There you can set anything:
    * startdate end enddate
    * scheduler resources like node numbers or walltime (see settings in `config`)
    * parts of the chain to be ran
    * chaining interval
    * output variables defined in the `&GRIBOUT` namelists by proceeding like described for [arbitrary namelist parameters](#changing-arbitrary-namelist-parameters).
    * anything else defined in `defaults.sh`, `Default_namelists/*` or `config`
4. Launch the simulation with `./run_daint.sh`.
5. Transfer your output data to a safe place!


## Asynchronous run

The current version of DECREMENT enables asynchronous execution of tasks, i.e. the current COSMO chunk runs simultaneously with the postprocessing of the previous one and the preprocessing of the next one. The dependencies between the tasks are described in `defaults.sh`


## Ensembles

It is possible to run the `2_lm_c` step in perturbed-intial-conditions ensemble mode. To this end, uncomment the corresponding block of env. vars in `user_settings`. Doing so will create sub-directories in in `2_lm_c`and and output. Note that in COSMO6 the random seed cannot be set explicitly and is based on machine time [in ms].


## Integrating a custom task in the chain

DECREMENT is generic enough to allow integration of custom parts in the chain which can be very helpful for *online* post-processing, archiving or custom pre-processing (like the PGW method). To do so, follow these steps:
1. Create a corresponding directory, like `25_my_post_proc`. It can contain digits at the start to indicate the position in the chain but doesn't have to.
2. In that directory, put a `run` file which will be executed at runtime
3. In `user_settings`, define environment variables that control the scheduling resources for nodes, walltime and partition. Their name must follow a certain pattern containing the uppercase name of the part, in our case "MY\_POST\_PROC" (without the preprending digits): `NQS_NODES_MY_POST_PROC`, `NQS_ELAPSED_MY_POST_PROC` and `NQS_QUEUE_MY_POST_PROC`. IF sepcial calculations are needed to infer these environmental variables or if others are needed at runtime, one can use an optional `env.sh` file inside the part directory. If found, it will be sourced just before submitting the job.

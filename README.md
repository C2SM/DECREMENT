# DECREMENT

***The DirEctory COSMO Runtime EnvironMENT***

A set of scripts used to run weather and climate simulations with COSMO. It is is intended to be simple and easy to extend. It was originally a stripped down version of the LM_package (hence all the variable names starting with `LM_`) and evolved later into a more generic workflow tool with async capabilities.

[[_TOC_]]


## The default chain of tasks

By default, DECREMENT is intended to run a so-called *coarse* resolution domain (just refering to the outer domain, doesn't have to actaully be coarse) and potentially a *fine* nested one. Concretely the following sequence of tasks (or a subset of it) is executed, each in a separate folder under the root directory:
* `00_get_data` transfers input data (mostly reanalysis data) for the INT2LM preprocessing software.
* `10_ifs2lm` produces input data for COSMO at coarse resolution.
* `20_lm_c` runs COSMO on the coarse resolution domain.
* `30_lm2lm` runs INT2LM with output from the coarse reolution COSMO as input data and produces input data for the fine resolution one.
* `40_lm_f` runs COSMO in the fine resolution nested domain.
* `50_pack_data` compresses the output netcdf files and gathers them into tar balls.
* `60_chain` manages the chaining of the simulation chuncks.


## How settings are prescribed

The scripts are loading settings from different locations with the following priority: **defaults < config < user settings**. The defaults can come from the `defaults.sh` file or anyting under `kk_part_name/Defaults`. The idea is that the `config` file hosts settings related to the simulation configuration (domain, physical parametrizations, domain decomposition, etc...) and must be located in the root directory. It can either be a link to a stock configuration, e.g. `ln -sf simulation_configs/SIMULATION_EU_CORDEX_50km config`, or a personal configuration written from scratch. Finally the optional but almost always needed `user_settings` file, which must also be located in the root directory, hosts settings one wants to modify (or even create) and takes precedence over others.

**Note:** `config` and `user_settings` are sourced from `run_daint.sh` in the root directory, so if one wants to source another file inside of them, the path has to be relative to the root directory. This also applies to nested sourcing.


## Changing arbitrary namelist parameters

A design idea of DECREMENT is that parameters that users change most often have a corresponding environment variable defined either in `part_name/Defaults/*` or in the `config` file. For instance, `LM_NL_DT_C` is the time step for the coarse reslution domain corresponding to the `dt` namelist parameter in the `20_lm_c` part. In order to be able to modify any parameter, the namelsts are generated in exported functions (see files in the `kk_part_name/Defaults` directory). These can be redefined either in the `config`or `user_settings` file and re-exported from there in order to take precedence over the default ones. This is tipically the way to go to modify the output streams. If this generates too much clutter in `config` or `user_settings`, one can always source a custom file from inside them to keep a good level of readability.


## Basic usage

1. Get executable related files:
    * Copy an `int2lm` executable and a `cosmo` executable to `./bin`. When using an executable built with spack, it's recommanded to specify the corresponding spack environment . To this end, generate the environment file with, e.g. for COSMO
        ```bash
        spack load --sh cosmo@c2sm-features %nvhpc cosmo_target=gpu +cppdycore ^mpich%nvhpc > spack_env_cosmo.sh
        ```
        and place the `spack_env_cosmo.sh` file in both directories running cosmo, i.e. `20_lm_c` and `40_lm_f`. In order to keep things tidy, one can also put this file in the `bin` directory with a more descriptive name, like `spack_env_cosmo_gpu.sh`, and link it as `spack_env_cosmo.sh` in the right directories. Same thing applies to INT2LM: if a `spack_env_int2lm.sh` file is found in `10_ifs2lm` or `30_lm2lm`, it will be sourced before submitting the corresponding job.
    * In order to run INT2LM, copy the necessary extpar file in `bin` as well. Commands to get the extpar files for the stock configurations are listed in `./get_extpar_data.sh` (no need to execute the whole file). If necessary, adapt the `LM_NL_EXTPAR_?` environment variables accordingly.

2. Link the simulation configuration file like
    ```bash
    ln -s simulation_configs/SIMULATION_EU_CORDEX_50km config
    ```

3. Copy the user settings example (`cp user_settings_example user_settings`) where some common settings are commented out with minimal documentation. There you can set anything:
    * startdate end enddate
    * scheduler resources like node numbers or walltime (see settings in `config`)
    * parts of the chain to be ran
    * chaining interval
    * output variables defined in the `&GRIBOUT` namelists by proceeding like described for [arbitrary namelist parameters](#changing-arbitrary-namelist-parameters).
    * anything else defined in `defaults.sh`, `kk_part_name/Defaults/*` or `config`

4. Launch the simulation with
    ```bash
    ./run_daint.sh
    ```
You can monitor the overall status in the `status.log` file.

5. Transfer your output data to a safe place!


## Asynchronous run

The current version of DECREMENT enables asynchronous execution of tasks, i.e. the current COSMO chunk runs simultaneously with the postprocessing of the previous one and the preprocessing of the next one. The dependencies between the tasks are described in `defaults.sh`


## Ensembles

It is possible to run the `20_lm_c` step in perturbed-intial-conditions ensemble mode. To this end, uncomment the corresponding block of env. vars in `user_settings`. Doing so will create member sub-directories in in `20_lm_c`. Note that in COSMO6 the random seed cannot be set explicitly and is based on machine time [in ms].


## Integrating a custom part in the chain

DECREMENT is generic enough to allow integration of custom parts in the chain which can be very helpful for *online* post-processing, archiving or custom pre-processing (like the PGW method).

A part is a directory directly placed in the root dir (like the stock parts for INT2LM and COSMO). It's name can contain leading digits to indicate the position in the chain for readability but doesn't have to. from now on, let's assume we want to insert the part `45_my_post_proc`. It is expected to contain the following elements, some of them being optional:
* a `Defaults` directory. Any file in that directory will be sourced and `config` and `user_settings` can overwrite the settings. Like for stock parts, it can for instance contain the definition of default parameters or the default functions to generate namelists.
* a `submit.sh` script. It's optional. When present, it is executed (as opposed to sourced) to submit the part and the automated submit mechanism is skipped. The only requirement is that it returns (`echo`) the jobid(s) of the submitted part. This feature is used in `20_lm_c` to handle ensemble runs and a similar procedure could be used for sensitivity runs.
* a `run` file. If `submit.sh` is not present, it has to be there. It will be the file submitted with environment variables controling the resources used. The later must have a name that follows a certain pattern containing the uppercase name of the part without optional leading digits. In our case, they would be the following:
    * `NQS_NODES_MY_POST_PROC` : nodes number (defaults to 1)
    * `NQS_NTPN_MY_POST_PROC` (optional) : number of tasks per nodes (defaults to the number of cores per node, so 12 on Piz'Daint)
    * `NQS_ELAPSED_MY_POST_PROC` : wall time (defaults to 5 min)
    * `NQS_PARTITION_MY_POST_PROC` : machine partition (defaults to `"normal"`)
* an `env.sh` file. It's optional. If present and `submit.sh` is not present, it will be sourced before submitting the job. It can for instance contain operations to determine the `NQS_XXX` env vars if they need be calculated rather than prescribed in `user_settings`or source a spack environement (see examples in stock parts).

**Note:** The submission mechanism is ran as a subprocess inside the part directory so any files sourced or executed inside `env.sh` or `submit.sh` must have a path relative to that directory.

The last required setting relates to the job dependencies. You can check in `defaults.sh` how it's set for the stock parts. The format is
``` bash
export xxx_deps="current_yyy previous_zzz ..."
```
where `xxx`, `yyy` and `zzz` are valid *short names* of parts to be ran, i.e without the leading digits "kk_". In our case, in order to insert the new part between `40_lm_f` and `50_pack_data`, we would add the following lines either to any file under `Defaults` or in `user_settings`:
```bash
export my_post_proc_deps="current_lm_f"
export pack_data_deps="current_my_post_proc"
```

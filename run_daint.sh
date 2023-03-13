#!/bin/bash

# Load defaults
# =============
source tools.sh
source defaults.sh
source Default_namelists/ifs2lm_defaults.sh
source Default_namelists/lm_c_defaults.sh 
source Default_namelists/lm2lm_defaults.sh 
source Default_namelists/lm_f_defaults.sh

# Load user defined parameters
# ============================
# Load once here, just to have access to COSMO_TARGET in config
if [ ! -f user_settings ]; then 
    echo "user_settings doesn't exist yet. Copying default file user_settings_example"
    cp user_settings_example user_settings
fi

if [ ! -f config ]; then
    echo "Abort. No config specifed. Copy one from simulation_configs, e.g., cp simulation_configs/SIMULATION_EU_CORDEX_50km config"
    exit 1
fi

source user_settings


# Load simualtion config
# ======================
source config


# Reload user settings
# ====================
# Source user settings again so that they take precedence over anything else
# user_settings > config > Default_namelists/*
source user_settings


# Daint specific settings
# =======================
export QUEUE="normal"
export RUNCMD="srun"
export CORES_PER_NODE=24


# Architecture specific settings
# ==============================
if [[ $COSMO_TARGET == "cpu" ]]; then
    export LM_NL_LCPP_DYCORE=.False.
else
    export LM_NL_LCPP_DYCORE=.True.
fi

# Async IO settings
# =================
if (( ${NQS_NIOLM_C} > 0 )); then
    export LM_NL_LASYNC_IO_C=.TRUE.
    export LM_NL_NUM_IOPE_PERCOMM_C=1
else
    export LM_NL_LASYNC_IO_C=.FALSE.
    export LM_NL_NUM_IOPE_PERCOMM_C=0
    export LM_NL_LPREFETCH_C=.FALSE.
fi

if (( ${NQS_NIOLM_F} > 0 )); then
    export LM_NL_LASYNC_IO_F=.TRUE.
    export LM_NL_NUM_IOPE_PERCOMM_F=1
else
    export LM_NL_LASYNC_IO_F=.FALSE.
    export LM_NL_NUM_IOPE_PERCOMM_F=0
    export LM_NL_LPREFETCH_F=.FALSE.
fi


# Handle dates
# ============
# Convert dates to formats capable of handling addition of arbitrary time intervals
export LM_INI_DATE=$(date -d "${LM_INI_DATE}" +%c)
export LM_FIN_DATE=$(date -d "${LM_FIN_DATE}" +%c)

# Start date of the simulation (not the chunk)
# If not set by the user, set it to the LM_INI_DATE
if [[ -z "${LM_START_DATE}" ]]; then
    export LM_START_DATE=${LM_INI_DATE}
else
    export LM_START_DATE=$(date -d "${LM_START_DATE}" +%c)
fi

# Begin date of current step
# If set in 6_chain, keep the value, otherwise, use LM_START_DATE
if [[ -z "${LM_BEGIN_DATE}" ]]; then
    export LM_BEGIN_DATE=$(date -d "${LM_START_DATE}" +%c)
fi

# End date of current step
if [[ -z "${LM_CHAIN_INTERVAL}" ]]; then
    export LM_END_DATE=${LM_FIN_DATE}
else
    END_DATE=$(date -d "${LM_BEGIN_DATE}+${LM_CHAIN_INTERVAL}" +%c)
    if (( $(date -d "$END_DATE" +%s) > $(date -d "$LM_FIN_DATE" +%s) )); then
        export LM_END_DATE=${LM_FIN_DATE}
    else
        export LM_END_DATE=${END_DATE}
    fi
fi

# Store dates components
export LM_YYYY_INI=$(date -d "${LM_INI_DATE}" +%Y)
export LM_MM_INI=$(date -d "${LM_INI_DATE}" +%m)
export LM_DD_INI=$(date -d "${LM_INI_DATE}" +%d)
export LM_ZZ_INI=$(date -d "${LM_INI_DATE}" +%H)

export LM_YYYY_BEGIN=$(date -d "${LM_BEGIN_DATE}" +%Y)
export LM_MM_BEGIN=$(date -d "${LM_BEGIN_DATE}" +%m)
export LM_DD_BEGIN=$(date -d "${LM_BEGIN_DATE}" +%d)   # - ML - unused
export LM_ZZ_BEGIN=$(date -d "${LM_BEGIN_DATE}" +%H)   # - ML - unused

export LM_YYYY_END=$(date -d "${LM_END_DATE}" +%Y)
export LM_MM_END=$(date -d "${LM_END_DATE}" +%m)
export LM_DD_END=$(date -d "${LM_END_DATE}" +%d)
export LM_ZZ_END=$(date -d "${LM_END_DATE}" +%H)

# Compute HSTART, HSTOP and corresponding NSTART, NSTOP
diff_hours() {
    d1=$(date -d "$1" +%s)
    d2=$(date -d "$2" +%s)
    echo $(( ($d2 - $d1) / 3600 ))
}
export LM_NL_HSTART=$(diff_hours "${LM_INI_DATE}" "${LM_BEGIN_DATE}")
export LM_NL_HSTOP=$(diff_hours "${LM_INI_DATE}" "${LM_END_DATE}")

export LM_NL_NSTART_C=$((LM_NL_HSTART*3600/LM_NL_DT_C))
export LM_NL_NSTOP_C=$((LM_NL_HSTOP*3600/LM_NL_DT_C))

export LM_NL_NSTART_F=$((LM_NL_HSTART*3600/LM_NL_DT_F))
export LM_NL_NSTOP_F=$((LM_NL_HSTOP*3600/LM_NL_DT_F))

# Dates for filenames
export LM_BEGIN_DATE_FR=$(date -d "${LM_BEGIN_DATE}" +%FT%R)
export LM_END_DATE_FR=$(date -d "${LM_END_DATE}" +%FT%R)


# Submit jobs
# ===========
echo "submitting jobs for the priod from ${LM_BEGIN_DATE_FR} to ${LM_END_DATE_FR}"

for part in ${SB_PARTS} ; do
    short=${part#[0-9]*_}
    SHORT=$(echo ${short} | tr '[:lower:]' '[:upper:]')
    mkdir -p output/${short}

    cd ${part}

    # Build sbatch options
    # --------------------
    # Common options
    sbatch_opts="--parsable -C gpu --output job_${LM_BEGIN_DATE_FR}_${LM_END_DATE_FR}.out"
    sbatch_opts+=" --job-name ${short}"
    sbatch_opts+=" --account=${ACCOUNT}"

    # number of nodes and potentitally number of tasks per node
    ntpn=12
    case ${short} in
        ifs2lm)
            nodes=$(compute_nodes ${NQS_NXIFS2LM} ${NQS_NYIFS2LM} ${NQS_NIOIFS2LM} ${ntpn});;
        lm2lm)
            nodes=$(compute_nodes ${NQS_NXLM2LM} ${NQS_NYLM2LM} ${NQS_NIOLM2LM} ${ntpn});;
        lm_c)
            if [[ $COSMO_TARGET == "gpu" ]]; then
                ntpn=1
                sbatch_opts+=" --ntasks-per-node=1"
            fi
            nodes=$(compute_nodes ${NQS_NXLM_C} ${NQS_NYLM_C} ${NQS_NIOLM_C} ${ntpn});;
        lm_f)
            if [[ $COSMO_TARGET == "gpu" ]]; then
                ntpn=1
                sbatch_opts+=" --ntasks-per-node=1"
            fi
            nodes=$(compute_nodes ${NQS_NXLM_C} ${NQS_NYLM_C} ${NQS_NIOLM_C} ${ntpn});;
        *)
            eval nodes=\${NQS_NODES_${SHORT}}
            [[ -z "${nodes}" ]] && nodes=1;;
    esac
    sbatch_opts+=" --nodes=${nodes}"

    # dependencies
    dep_ids=$(get_dep_ids ${short})
    [[ -n "${dep_ids}" ]] && sbatch_opts+=" --dependency=afterok:${dep_ids}"

    # wall time
    eval time=\${NQS_ELAPSED_${SHORT}}
    [[ -z "${time}" ]] && time="00:05:00"
    sbatch_opts+=" --time=${time}"
                 
    # partition
    eval partition=\${NQS_QUEUE_${SHORT}}
    [[ -z "${partition}" ]] && partition=${QUEUE}
    sbatch_opts+=" --partition=${partition}"
    
    # log message
    # -----------
    message="launching ${short}"
    [[ -n "${dep_ids}" ]] && message+=" with the following dependencies: ${dep_ids}"
    echo ${message}
    
    if [[ "${short}" == "lm_c" ]] && (( LM_NL_ENS_NUMBER_C > 1 )); then
        
        # Ensemble execution
        # ------------------
        echo "running lm_c in ensemble mode with ${LM_NL_ENS_NUMBER_C} realizations"

        # Loop over ensemble members
        (( max_k=${LM_NL_ENS_NUMBER_C}-1 ))
        for (( k=0; k<=$max_k; k++ )); do
            # create 0-padded member number directory
            member=$(printf "%0${#max_k}d" $k)
            mkdir -p $member
            cd $member

            # cleanup
            ln -sf ../clean
            ./clean

            # link necessary paths and exe
            ln -sf ../gen_job_script.sh
            ln -sf ../output/$member output
            ln -sf ../input
            ln -sf ../cosmo

            # generate job script
            [[ ${LM_BEGIN_DATE} == ${LM_START_DATE} ]] && ./gen_job_script.sh

            # Submit job and store job id
            jobid=$(sbatch ${sbatch_opts} --wrap="./run")
            jobids="${jobids} ${jobid}"
            
            cd ..
        done
        
        # gather all member ids in a :-separated list and export
        jobids=$(id_list ${jobids})
        eval export current_${short}_id=\${jobids}
        
    else
        
        # Normal execution
        # ----------------
        # generate job script
        [[ ${LM_BEGIN_DATE} == ${LM_START_DATE} ]] && ./gen_job_script.sh
        
        # Submit job and get job id
        jobid=$(sbatch ${sbatch_opts} --wrap="./run")
        
        # Store job id
        eval export current_${short}_id=\${jobid}
    fi
    
    cd - 1>/dev/null 2>/dev/null
done

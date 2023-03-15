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
# Load once here, just to have access to COSMO_TARGET when sourcing config

if [[ ! -f config ]]; then
    echo "ERROR : No config file found. Copy or link one from simulation_configs, e.g., ln -s simulation_configs/SIMULATION_EU_CORDEX_50km config"
    exit 1
fi
if [[ ! -f user_settings ]]; then 
    echo "WARNING : No user_setting file found. You could start from the user_settings_example file if needed."
else
    source user_settings    
fi


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

export LM_NL_HSTART=$(diff_hours "${LM_INI_DATE}" "${LM_BEGIN_DATE}")
export LM_NL_HSTOP=$(diff_hours "${LM_INI_DATE}" "${LM_END_DATE}")

export LM_NL_NSTART_C=$((LM_NL_HSTART*3600/LM_NL_DT_C))
export LM_NL_NSTOP_C=$((LM_NL_HSTOP*3600/LM_NL_DT_C))

export LM_NL_NSTART_F=$((LM_NL_HSTART*3600/LM_NL_DT_F))
export LM_NL_NSTOP_F=$((LM_NL_HSTOP*3600/LM_NL_DT_F))

# Dates for filenames or log messages
export LM_BEGIN_DATE_FR=$(date -d "${LM_BEGIN_DATE}" +%FT%R)
export LM_END_DATE_FR=$(date -d "${LM_END_DATE}" +%FT%R)
export LM_BEGIN_DATE_DG=$(date -d "${LM_BEGIN_DATE}" +%Y%m%d%H%M%S)
export LM_END_DATE_DG=$(date -d "${LM_END_DATE}" +%Y%m%d%H%M%S)

# Init matter
# ===========
if [[ "${LM_BEGIN_DATE}" == "${LM_START_DATE}" ]]; then
    export status_file=$(realpath "./status.log")
    # do some cleaning
    ./clean.sh
    rm -f ${status_file}
fi

# Submit jobs
# ===========
echo "submitting jobs for the priod from ${LM_BEGIN_DATE_FR} to ${LM_END_DATE_FR}" >> ${status_file}

for part in ${SB_PARTS} ; do
    short=${part#[0-9]*_}
    SHORT=$(echo ${short} | tr '[:lower:]' '[:upper:]')
    mkdir -p output/${short}

    cd ${part}

    # Build sbatch command
    # --------------------
    cmd="sbatch --parsable -C gpu"
    
    # Common options
    export LM_RUN_OUTPUT="run_${LM_BEGIN_DATE_DG}_${LM_END_DATE_DG}.out"
    cmd+=" --output ${LM_RUN_OUTPUT}"
    cmd+=" --job-name ${short}"
    cmd+=" --account=${ACCOUNT}"

    # number of nodes and potentitally number of tasks per node
    ntpn=12
    case ${short} in
        ifs2lm)
            nodes=$(compute_nodes ${NQS_NXIFS2LM} ${NQS_NYIFS2LM} ${NQS_NIOIFS2LM} ${ntpn});;
        lm2lm)
            nodes=$(compute_nodes ${NQS_NXLM2LM} ${NQS_NYLM2LM} ${NQS_NIOLM2LM} ${ntpn});;
        lm_c)
            [[ $COSMO_TARGET == "gpu" ]] && ntpn=1
            nodes=$(compute_nodes ${NQS_NXLM_C} ${NQS_NYLM_C} ${NQS_NIOLM_C} ${ntpn});;
        lm_f)
            [[ $COSMO_TARGET == "gpu" ]] && ntpn=1
            nodes=$(compute_nodes ${NQS_NXLM_F} ${NQS_NYLM_F} ${NQS_NIOLM_F} ${ntpn});;
        *)
            eval nodes=\${NQS_NODES_${SHORT}}
            [[ -z "${nodes}" ]] && nodes=1;;
    esac
    cmd+=" --nodes=${nodes} --ntasks-per-node=${ntpn}"

    # dependencies
    dep_ids=$(get_dep_ids ${short})
    [[ -n "${dep_ids}" ]] && cmd+=" --dependency=afterok:${dep_ids}"

    # wall time
    eval time=\${NQS_ELAPSED_${SHORT}}
    [[ -z "${time}" ]] && time="00:05:00"
    cmd+=" --time=${time}"
                 
    # partition
    eval partition=\${NQS_QUEUE_${SHORT}}
    [[ -z "${partition}" ]] && partition=${QUEUE}
    cmd+=" --partition=${partition}"

    # job file
    cmd+=" ./run"

    if [[ "${short}" == "lm_c" ]] && (( LM_NL_ENS_NUMBER_C > 1 )); then
        
        # Ensemble execution
        # ------------------
        echo "running lm_c in ensemble mode with ${LM_NL_ENS_NUMBER_C} realizations" >> ${status_file}

        # Loop over ensemble members
        (( max_k=${LM_NL_ENS_NUMBER_C}-1 ))
        for (( k=0; k<=$max_k; k++ )); do
            # create 0-padded member directory
            member=$(printf "%0${#max_k}d" $k)
            mkdir -p $member
            cd $member

            # link necessary paths and exe
            ln -sf ../clean
            ln -sf ../run
            ln -sf ../output/$member output
            ln -sf ../input
            ln -sf ../cosmo

            # Submit job and store job id
            jobid=$(${cmd})
            jobids+=" ${jobid}"
            
            cd ..
        done
        
        # gather all member ids in a :-separated list and export
        jobid=$(id_list ${jobids})
        eval export current_${short}_id=\${jobid}
        
    else
        
        # Normal execution
        # ----------------
        # Submit job and get job id
        jobid=$(${cmd})
        
        # Store job id
        eval export current_${short}_id=\${jobid}
        
    fi
    
    # Status log
    # ----------
    GREEN="\033[32m"
    NORMAL="\033[0;39m"
    echo -e "${GREEN}[[ ${part} ]]${NORMAL}" >> ${status_file}
    echo "[ nodes        ] ${nodes}" >> ${status_file}
    echo "[ time         ] ${time}" >> ${status_file}
    echo "[ queue        ] ${partition}" >> ${status_file}
    echo "[ dependencies ] ${dep_ids}" >> ${status_file}
    echo "[ job id       ] ${jobid}" >> ${status_file}
    echo "[ status       ] submitted" >> ${status_file}
    
    cd - 1>/dev/null 2>/dev/null
    
done


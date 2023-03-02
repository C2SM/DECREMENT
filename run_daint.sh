#!/bin/bash


# Load default namelist definitions
# =================================

source Default_namelists/ifs2lm_defaults.sh
source Default_namelists/lm_c_defaults.sh 
source Default_namelists/lm2lm_defaults.sh 
source Default_namelists/lm_f_defaults.sh


# Load user defined parameters
# ============================
# Load once here, just to have access to COSMO_TARGET

if [ ! -f user_settings ]; then 
    echo "user_settings doesn't exist yet. Copying default file user_settings_example"
    cp user_settings_example user_settings
fi

if [ ! -f config ]; then
    echo "Abort. No config specifed. Copy one from simulation_configs, e.g., cp simulation_configs/SIMULATION_EU_CORDEX_50km config"
    exit 1
fi

source user_settings


# Daint specific settings
# =======================

export QUEUE="normal"
export RUNCMD="srun"
export CORES_PER_NODE=24
export GPUS_PER_NODE=1
[[ $COSMO_TARGET == "cpu" ]] && export LM_NTASKS_PER_NODE_COSMO=12 || export LM_NTASKS_PER_NODE_COSMO=1
export LM_NTASKS_PER_NODE_INT2LM=12


# Load simualtion config
# ======================

source config


# Reload user settings
# ====================

# Source user settings again so that they take precedence over anything else
# user_settings > config > Default_namelists/*
source user_settings

# Compute requested node numbers
# ==============================
nodes(){
    echo $(( ($1 * $2 + $3 + $4 - 1) / $4 ))
}
export LM_COSMO_NODES_C=$(nodes ${NQS_NXLM_C} ${NQS_NYLM_C} ${NQS_NIOLM_C} ${LM_NTASKS_PER_NODE_COSMO})
export LM_COSMO_NODES_F=$(nodes ${NQS_NXLM_F} ${NQS_NYLM_F} ${NQS_NIOLM_F} ${LM_NTASKS_PER_NODE_COSMO})
export LM_IFS2LM_NODES=$(nodes ${NQS_NXIFS2LM} ${NQS_NYIFS2LM} ${NQS_NIOIFS2LM} ${LM_NTASKS_PER_NODE_INT2LM})
export LM_LM2LM_NODES=$(nodes ${NQS_NXLM2LM} ${NQS_NYLM2LM} ${NQS_NIOLM2LM} ${LM_NTASKS_PER_NODE_INT2LM})


# Handle dates
# ============
# Convert dates to formats capable of handling addition of arbitrary time intervals
export LM_INI_DATE=$(date -d "${LM_INI_DATE}" +%c)
export LM_FIN_DATE=$(date -d "${LM_INI_DATE}" +%c)

# Startdate of current step
# If set in 6_chain/run, keep the value
# otherwise if LM_START_DATE is set in user_settings, use that date
# else start from absolute initial date LM_INI_DATE
if [[ -z ${LM_BEGIN_DATE} ]]; then
    if [[ -n ${LM_START_DATE} ]]; then
        export LM_BEGIN_DATE=$(date -d "${LM_START_DATE}" +%c)
    else
        export LM_BEGIN_DATE=${LM_INI_DATE}
    fi
fi

# Enddate of current step
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
export LM_NL_HSTART=$(diff_hours ${LM_INI_DATE} ${LM_BEGIN_DATE})
export LM_NL_HSTOP=$(diff_hours ${LM_INI_DATE} ${LM_END_DATE})

export LM_NL_NSTART_C=$((LM_NL_HSTART*3600/LM_NL_DT_C))
export LM_NL_NSTOP_C=$((LM_NL_HSTOP*3600/LM_NL_DT_C))

export LM_NL_NSTART_F=$((LM_NL_HSTART*3600/LM_NL_DT_F))
export LM_NL_NSTOP_F=$((LM_NL_HSTOP*3600/LM_NL_DT_F))


# Submit jobs
# ===========

for part in ${SB_PARTS} ; do
    short=$(echo "${part}" | sed 's/^[0-9]*_//g')
    echo "launching ${short}"
    mkdir -p output/${short}

    cd ${part}
    
    # Ensemble execution
    if [ "${part}" = "2_lm_c" ] && [ ! -z $LM_NL_ENS_NUMBER_C ]; then
        echo "running ${short} in ensemble mode with ${LM_NL_ENS_NUMBER_C} realizations"
        jobid=$(./run_ensemble ${jobid})
        cd - 1>/dev/null 2>/dev/null
        continue
    fi

    # Normal execution
    jobid=$(./run ${jobid})
    
    cd - 1>/dev/null 2>/dev/null
done

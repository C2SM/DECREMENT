#!/bin/bash

# Load settings from defaults, config and user
# ============================================

# Load general defaults
source tools.sh
source defaults.sh

# Load config and user settings once here, just to have access to SB_PARTS
if [[ ! -f config ]]; then
    echo "ERROR : No config file found. Copy or link one from simulation_configs, e.g., ln -s simulation_configs/SIMULATION_EU_CORDEX_50km config"
    exit 1
fi
if [[ ! -f user_settings ]]; then 
    echo "WARNING : No user_setting file found. You could start from the user_settings_example file if needed."
else
    source user_settings    
fi

# Load Defaults from parts if a non empty "Defaults" directory is found 
for part in ${SB_PARTS}; do
    if [[ -d ${part}/Defaults ]]; then
        if [[ "$(ls -A ${part}/Defaults)" ]]; then
            for f in ${part}/Defaults/*; do
                source $f
            done
        fi
    fi
done

# Reload config and user settings to ensure user_settings > config > defaults
source config
[[ -f user_settings ]] && source user_settings


# Daint specific settings
# =======================

export PARTITION="normal"
export RUNCMD="srun"
export CORES_PER_NODE=12


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
# If set in 60_chain, keep the value, otherwise, use LM_START_DATE
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

# Compute HSTART and HSTOP
export LM_NL_HSTART=$(diff_hours "${LM_INI_DATE}" "${LM_BEGIN_DATE}")
export LM_NL_HSTOP=$(diff_hours "${LM_INI_DATE}" "${LM_END_DATE}")

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
    rm -f ${status_file}
fi


# Submit jobs
# ===========

if [[ "${LM_BEGIN_DATE}" != "${LM_START_DATE}" ]]; then
    echo "" >> ${status_file}
fi
echo "====>  jobs for the priod ${LM_BEGIN_DATE_FR} -- ${LM_END_DATE_FR}  <===="  >> ${status_file}
echo "" >> ${status_file}

export LM_RUN_OUTPUT="run_${LM_BEGIN_DATE_DG}_${LM_END_DATE_DG}.out"

for part in ${SB_PARTS} ; do
    export short=${part#[0-9]*_}
    export SHORT=$(echo ${short} | tr '[:lower:]' '[:upper:]')

    # Enter part directory
    # --------------------
    pushd ${part} >&/dev/null

    # Submit job
    # ----------
    if [[ -e submit.sh ]]; then
        jobid=$(./submit.sh)
    else
        jobid=$(submit)
    fi

    # Store job id
    # ------------
    eval export current_${short}_id=\${jobid}

    # Exit part directory
    # -------------------
    popd >&/dev/null
    
done


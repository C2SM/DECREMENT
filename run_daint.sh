#!/bin/bash

# User defined parameters
# =======================

source user_settings

# Daint specific settings
# =======================

export QUEUE="normal"
export RUNCMD="srun"
export CORES_PER_NODE=24
export GPUS_PER_NODE=1
[[ $COSMO_TARGET == "cpu" ]] && export LM_NTASKS_PER_NODE_COSMO=12 || export LM_NTASKS_PER_NODE_COSMO=1
export LM_NTASKS_PER_NODE_INT2LM=12


# Read simualtion config
# ======================

source config

# Source user settings again so that they take precedence over the default simulation config
source user_settings


# Set Start and end
# =================

# Global init date components
export LM_YYYY_INI=$(date -d "${LM_INI_DATE}" +%Y)
export LM_MM_INI=$(date -d "${LM_INI_DATE}" +%m)
export LM_DD_INI=$(date -d "${LM_INI_DATE}" +%d)
export LM_ZZ_INI=$(date -d "${LM_INI_DATE}" +%H)

# Startdate of current step
# If set in 6_chain/run, keep the value otherwise initialize with absolute start date
export LM_BEGIN_DATE=${LM_BEGIN_DATE:-${LM_INI_DATE}}
export LM_YYYY_BEGIN=$(date -d "${LM_BEGIN_DATE}" +%Y)
export LM_MM_BEGIN=$(date -d "${LM_BEGIN_DATE}" +%m)
export LM_DD_BEGIN=$(date -d "${LM_BEGIN_DATE}" +%d)   # - ML - unused
export LM_ZZ_BEGIN=$(date -d "${LM_BEGIN_DATE}" +%H)   # - ML - unused

# Enddate of current step
if [[ -z "${LM_CHAIN_INTERVAL}" ]]; then
  export LM_END_DATE=${LM_FIN_DATE}
else
  END_DATE=$(date -d "${LM_BEGIN_DATE}+${LM_CHAIN_INTERVAL}" +%Y-%m-%dT%H:00)
  if (( $(date -d "$END_DATE" +%s) > $(date -d "$LM_FIN_DATE" +%s) )); then
    export LM_END_DATE=${LM_FIN_DATE}
  else
    export LM_END_DATE=${END_DATE}
  fi
fi
export LM_YYYY_END=$(date -d "${LM_END_DATE}" +%Y)
export LM_MM_END=$(date -d "${LM_END_DATE}" +%m)
export LM_DD_END=$(date -d "${LM_END_DATE}" +%d)
export LM_ZZ_END=$(date -d "${LM_END_DATE}" +%H)

#Compute HSTART and HEND of the simulation
diff_hours() {
  d1=$(date -d "$1" +%s)
  d2=$(date -d "$2" +%s)
  echo $(( ($d2 - $d1) / 3600 ))
}
export LM_NL_HSTART=$(diff_hours ${LM_INI_DATE} ${LM_BEGIN_DATE})
export LM_NL_HSTOP=$(diff_hours ${LM_INI_DATE} ${LM_END_DATE})

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

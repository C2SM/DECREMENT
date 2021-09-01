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
if [[ $COSMO_TARGET == "cpu" ]]; then
    export TASKS_PER_NODE=12
else
    export TASKS_PER_NODE=1
fi


# Spack Executables
# =================

[[-z ${LM_INT2LM_SPEC} ]] || rsync -av $(spack location -i ${INT2LM_SPEC})/bin ./bin/$(basename ${EXE_INT2LM})
[[-z ${LM_COSMO_SPEC} ]] || rsync -av $(spack location -i ${COSMO_SPEC})/bin/cosmo_${COSMO_TARGET} ./bin/$(basename ${EXE_COSMO})


# Read simualtion config
# ======================

source config


# Set Start and end
# =================

# Global init date components
export LM_YYYY_INI=$(date -d "${LM_INI_DATE}" +%Y)
export LM_MM_INI=$(date -d "${LM_INI_DATE}" +%m)
export LM_DD_INI=$(date -d "${LM_INI_DATE}" +%d)
export LM_ZZ_INI=$(date -d "${LM_INI_DATE}" +%H)

# Startdate of current step
export LM_BEGIN_DATE=${LM_END_DATE:-${LM_INI_DATE}}
export LM_YYYY_BEGIN=$(date -d "${LM_BEGIN_DATE}" +%Y)
export LM_MM_BEGIN=$(date -d "${LM_BEGIN_DATE}" +%m)
export LM_DD_BEGIN=$(date -d "${LM_BEGIN_DATE}" +%d)   # - ML - unused
export LM_ZZ_BEGIN=$(date -d "${LM_BEGIN_DATE}" +%H)   # - ML - unused

# Enddate of current step
export LM_END_DATE=$(date -d "${LM_BEGIN_DATE}+${LM_CHAIN_INTERVAL}" +%Y-%m-%dT%H:00)
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
  jobid=$(./run ${jobid})
  
  cd - 1>/dev/null 2>/dev/null
done



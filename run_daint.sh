#!/bin/bash

diff_hours() {
  #Diff
  d1=$(date -d "$1" +%s)
  d2=$(date -d "$2" +%s)
  echo $(( ($d2 - $d1) / 3600 ))
}

export COSMO_TARGET="gpu"

#Read grids and simualtion config
source config

# Set Start and end
# ================= 

# Date of model initialization
export LM_YYYY_INI=2011
export LM_MM_INI=01
export LM_DD_INI=01
export LM_ZZ_INI=00

#Overall 
export LM_YYYY_FINISHED=2015
export LM_MM_FINISHED=01
export LM_DD_FINISHED=01
export LM_ZZ_FINISHED=00

# Startdate of current step
export LM_YYYY_BEGIN=2011
export LM_MM_BEGIN=01
export LM_DD_BEGIN=01
export LM_ZZ_BEGIN=00

# Enddate of current step
export LM_YYYY_END=2012
export LM_MM_END=01
export LM_DD_END=01
export LM_ZZ_END=00

#Set if cpus or gpus will be used
if [[ $COSMO_TARGET == "cpu" ]]; then
    export TASKS_PER_NODE=12
else
    export TASKS_PER_NODE=1
fi

#Compute HSTART and HEND of the simulation
export LM_NL_HSTART=$( diff_hours ${LM_YYYY_INI}-${LM_MM_INI}-${LM_DD_INI}T${LM_ZZ_INI}:00 ${LM_YYYY_BEGIN}-${LM_MM_BEGIN}-${LM_DD_BEGIN}T${LM_ZZ_BEGIN}:00 )
export LM_NL_HSTOP=$( diff_hours ${LM_YYYY_INI}-${LM_MM_INI}-${LM_DD_INI}T${LM_ZZ_INI}:00 ${LM_YYYY_END}-${LM_MM_END}-${LM_DD_END}T${LM_ZZ_END}:00  )

for part in ${SB_PARTS} ; do
  short=`echo "${part}" | sed 's/^[0-9]*_//g'`
  echo "launching ${short}"
  mkdir -p output/${short}

  cd ${part}
  jobid=`./run ${jobid}`
  
  cd - 1>/dev/null 2>/dev/null
done



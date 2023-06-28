#!/bin/bash

# COSMO settings
# ==============
if [[ $COSMO_TARGET == "cpu" ]]; then
    export LM_NL_LCPP_DYCORE=.False.
else
    export LM_NL_LCPP_DYCORE=.True.
    export COSMO_NPROC_NODEVICE=${NQS_NIOLM_C}
fi

if (( ${NQS_NIOLM_C} > 0 )); then
    export LM_NL_LASYNC_IO_C=.TRUE.
    export LM_NL_NUM_IOPE_PERCOMM_C=1
else
    export LM_NL_LASYNC_IO_C=.FALSE.
    export LM_NL_NUM_IOPE_PERCOMM_C=0
    export LM_NL_LPREFETCH_C=.FALSE.
fi
export LM_NL_NSTART_C=$((LM_NL_HSTART*3600/LM_NL_DT_C))
export LM_NL_NSTOP_C=$((LM_NL_HSTOP*3600/LM_NL_DT_C))

# Overall Number of nodes
# =======================
if [[ $COSMO_TARGET == "gpu" ]]; then
    export NQS_NODES_CCLM2_C=$(compute_cosmo_nodes ${NQS_NXLM_C} ${NQS_NYLM_C} ${NQS_NIOLM_C} 1)
    export CLM_C_NTASKS=$(( NQS_NODES_CCLM2_C * (CORES_PER_NODE - 1) ))
else
    ((ADD_TASKS = NQS_NIOLM_C + CLM_C_NTASKS))
    export NQS_NODES_CCLM2_C=$(compute_cosmo_nodes ${NQS_NXLM_C} ${NQS_NYLM_C} ${ADD_TASKS} ${CORES_PER_NODE})
fi

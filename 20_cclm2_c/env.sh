#!/bin/bash

# COSMO settings
# ==============
# DYN and PHY settings for CPU/GPU, mostly related to performance (faster stuff needed on cpu, not implemented for gpu)
# CPU settings correspond to COPAT2
if [[ $COSMO_TARGET == "cpu" ]]; then
    export LM_NL_LCPP_DYCORE=.FALSE.
    export LM_NL_Y_SCALAR_ADVECT='"BOTTDC2"'
    export LM_NL_L_3D_DIV_DAMPING=.TRUE.
    export LM_NL_LGSP_FIRST=.FALSE. 
else
    export LM_NL_LCPP_DYCORE=.TRUE.
    export LM_NL_Y_SCALAR_ADVECT='"BOTT2_STRANG"'
    export LM_NL_L_3D_DIV_DAMPING=.FALSE.
    export LM_NL_LGSP_FIRST=.TRUE.
    # Only for CCLM2
    export COSMO_NPROC_NODEVICE=${NQS_NIOLM_C}
fi

# IO settings
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
    
    # PS: Test limiting the number of tasks for CLM (relevant for >4x4), not working because OASIS distributes the MPI communicators
    # Maximum number of tasks using all idling cpus
    #CLM_C_NTASKS_MAX=$(( NQS_NODES_CCLM2_C * (CORES_PER_NODE - 1) ))
    # Limit actual number to 187 because CLM fails with too many
    #export CLM_C_NTASKS=$(( CLM_C_NTASKS_MAX < 187 ? CLM_C_NTASKS_MAX : 187 ))
    
else
    ((ADD_TASKS = NQS_NIOLM_C + CLM_C_NTASKS))
    export NQS_NODES_CCLM2_C=$(compute_cosmo_nodes ${NQS_NXLM_C} ${NQS_NYLM_C} ${ADD_TASKS} ${CORES_PER_NODE})
fi

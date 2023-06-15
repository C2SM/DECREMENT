#!/bin/bash

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

# Nodes
# =====
[[ $COSMO_TARGET == "gpu" ]] && ntpn=1 || ntpn=12
export NQS_NODES_LM_C=$(compute_cosmo_nodes ${NQS_NXLM_C} ${NQS_NYLM_C} ${NQS_NIOLM_C} ${ntpn})
export NQS_NTPN_LM_C=ntpn

# Spack
# =====
[[ -e spack_env_cosmo.sh ]] && source spack_env_cosmo.sh

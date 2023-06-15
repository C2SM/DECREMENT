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
if (( ${NQS_NIOLM_F} > 0 )); then
    export LM_NL_LASYNC_IO_F=.TRUE.
    export LM_NL_NUM_IOPE_PERCOMM_F=1
else
    export LM_NL_LASYNC_IO_F=.FALSE.
    export LM_NL_NUM_IOPE_PERCOMM_F=0
    export LM_NL_LPREFETCH_F=.FALSE.
fi

# Nodes
# =====
[[ $COSMO_TARGET == "gpu" ]] && ntpn=1 || ntpn=12
export NQS_NODES_LM_F=$(compute_cosmo_nodes ${NQS_NXLM_F} ${NQS_NYLM_F} ${NQS_NIOLM_F} ${ntpn})
export NQS_NTPN_LM_F=${ntpn}

# Spack
# =====
[[ -e spack_env_cosmo.sh ]] && source spack_env_cosmo.sh

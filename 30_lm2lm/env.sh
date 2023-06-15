#!/bin/bash

# Nodes
# =====
export NQS_NODES_LM2LM=$(compute_cosmo_nodes ${NQS_NXLM2LM} ${NQS_NYLM2LM} ${NQS_NIOLM2LM} ${CORES_PER_NODE})

# Spack
# =====
[[ -e spack_env_int2lm.sh ]] && source spack_env_int2lm.sh

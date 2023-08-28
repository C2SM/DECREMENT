#!/bin/bash

# Nodes
# =====
export NQS_NODES_IFS2LM=$(compute_cosmo_nodes ${NQS_NXIFS2LM} ${NQS_NYIFS2LM} ${NQS_NIOIFS2LM} ${CORES_PER_NODE})

# Spack
# =====
[[ -e spack_env_int2lm.sh ]] && source spack_env_int2lm.sh

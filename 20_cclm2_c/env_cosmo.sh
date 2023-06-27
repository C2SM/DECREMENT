#!/bin/bash

# General Runtime settings
# ========================
export MALLOC_MMAP_MAX_=0
export MALLOC_TRIM_THRESHOLD_=536870912
export OMP_NUM_THREADS=1
if [[ $COSMO_TARGET == "gpu" ]]; then
    export MPICH_RDMA_ENABLED_CUDA=1
    export MPICH_G2G_PIPELINE=256
fi

# Spack
# =====
[[ -e spack_env_cosmo.sh ]] && source spack_env_cosmo.sh


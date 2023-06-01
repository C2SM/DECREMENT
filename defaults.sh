#!/bin/bash

# COSMO target architecture
export COSMO_TARGET="gpu"

# Scheduler account
export ACCOUNT="pr133"

# Name of the executables
export EXE_INT2LM="./int2lm"
export EXE_COSMO="./cosmo"

# Load environemt using "spack load" commands.
export LM_INT2LM_SPEC="int2lm@c2sm-master%nvhpc"
export LM_COSMO_SPEC="cosmo@c2sm-features%nvhpc cosmo_target=gpu real_type=double +cppdycore"

# Define default dependencies
# The format is xxx_deps="current_yyy previous_zzz ..."
# where xxx, yyy and zzz are valid "short names" of parts to be ran,
# i.e without the "k_" with k an integer in front of the name.
export get_data_deps="previous_get_data"
export ifs2lm_deps="previous_ifs2lm current_get_data"
export lm_c_deps="previous_lm_c current_ifs2lm"
export lm2lm_deps="previous_lm2lm current_lm_c"
export lm_f_deps="previous_lm_f current_lm2lm"
export pack_data_deps="current_lm_c current_lm_f"
export chain_deps="previous_lm_c previous_lm_f"
# Note : setting chain_deps="" will submit all chunks at once,
# which could be a desired behavior

#Async NetCDF I/O
export LM_NL_ASYNIO_BLOCK_SIZE_C=10
export LM_NL_ASYNIO_BLOCK_SIZE_F=10

# Get data
export NQS_ELAPSED_GET_DATA="01:00:00"

# Pack data
export COMPRESS_LEVEL=1
export PACK_DATA=true

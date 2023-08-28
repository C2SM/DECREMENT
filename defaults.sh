#!/bin/bash

# COSMO target architecture
export COSMO_TARGET="gpu"

# Scheduler account
export ACCOUNT="pr133"

# Name of the executables
export EXE_INT2LM="../bin/int2lm"
export EXE_COSMO="../bin/cosmo"

# Define default dependencies
# The format is xxx_deps="current_yyy previous_zzz ..."
# where xxx, yyy and zzz are valid "short names" of parts to be ran,
# i.e without the leading digits "kk_" in front of the name.
export get_data_deps="previous_get_data"
export ifs2lm_deps="previous_ifs2lm current_get_data"
export lm_c_deps="previous_lm_c current_ifs2lm"
export lm2lm_deps="previous_lm2lm current_lm_c"
export lm_f_deps="previous_lm_f current_lm2lm"
export pack_data_deps="current_lm_c current_lm_f"
export chain_deps="previous_lm_c previous_lm_f"
# Note : setting chain_deps="" will submit all chunks at once,
# which could be a desired behavior

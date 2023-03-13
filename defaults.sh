#!/bin/bash

# Define default dependencies
# The format is xxx_deps="current_yyy previous_zzz ..."
# where xxx, yyy and zzz are valid "short names" of parts to be ran,
# i.e without the "k_" with k an integer in front of the name.
export get_data_deps=""
export ifs2lm_deps="previous_ifs2lm current_get_data"
export lm_c_deps="previous_lm_c current_ifs2lm"
export lm2lm_deps="previous_lm2lm current_lm_c"
export lm_f_deps="previous_lm_f current_lm2lm"
export pack_data_deps="current_lm_c current_lm_f"
export chain_deps="previous_lm_c previous_lm_f"

#Async NetCDF I/O
export LM_NL_ASYNIO_BLOCK_SIZE_C=10
export LM_NL_ASYNIO_BLOCK_SIZE_F=10

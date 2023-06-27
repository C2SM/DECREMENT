#!/bin/bash

module load daint-gpu
module load cray-mpich
if [[ ${CLM_COMPILER} == gcc ]]; then
    module switch PrgEnv-cray PrgEnv-gnu
    module switch gcc gcc/9.3.0
elif [[ ${CLM_COMPILER} == nvhpc ]]; then
    module switch PrgEnv-cray PrgEnv-nvidia
    module load nvhpc/21.3
fi
module load cray-netcdf-hdf5parallel
module load cray-hdf5-parallel
module load cray-parallel-netcdf

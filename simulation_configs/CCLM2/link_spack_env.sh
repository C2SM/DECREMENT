#!/bin/bash

# Link the spack environment files for COSMO and INT2LM

# For CCLM2 COSMO (cosmo@c2sm-features%nvhpc)
cd 20_cclm2_c
rm -f spack_env_cosmo.sh
ln -s ../bin/spack_env_cosmo_${COSMO_TARGET}.sh spack_env_cosmo.sh
cd ..

# For COSMO only, just in case
cd 20_lm_c
rm -f spack_env_cosmo.sh
ln -s ../bin/spack_env_cosmo_${COSMO_TARGET}.sh spack_env_cosmo.sh
cd ..

cd 40_lm_f
rm -f spack_env_cosmo.sh
ln -s ../bin/spack_env_cosmo_${COSMO_TARGET}.sh spack_env_cosmo.sh
cd ..

# For INT2LM (int2lm@c2sm-features%nvhpc)
cd 10_ifs2lm
rm -f spack_env_int2lm.sh
ln -s ../bin/spack_env_int2lm.sh spack_env_int2lm.sh
cd ..
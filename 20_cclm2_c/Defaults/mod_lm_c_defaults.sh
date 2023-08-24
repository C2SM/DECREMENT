#!/bin/bash

# ---------------------------------------------------------------
#                       lm_c defaults
# ---------------------------------------------------------------

# Source all default settings from lm_c
for f in 20_lm_c/Defaults/*; do
    source $f
done

# ---------------------------------------------------------------
#                       Modify lm_c defaults
# ---------------------------------------------------------------

lm_c_mod_INPUT_ORG(){
    # Replace hstop setting by nstop in INPUT_ORG
    sed -i 's/\(^\s*\)hstop\(\s*\)=\(\s*\).*$/\1nstop\2=\3'"$((LM_NL_NSTOP_C-1))"'/' INPUT_ORG
}
export -f lm_c_mod_INPUT_ORG

lm_c_mod_INPUT_IO(){
    # Replace input and output by cesm_input and cesm_output
    sed -i 's/input/cosmo_input/g' INPUT_IO
    sed -i 's/output/cosmo_output/g' INPUT_IO
}
export -f lm_c_mod_INPUT_IO

# ---------------------------------------------------------------
#                            INPUT_OAS
# ---------------------------------------------------------------

lm_c_INPUT_OAS(){
  # - ML TODO - parametrize this
  cat > INPUT_OAS << EONL
&oasisctl
IOASISDEBUGLVL = 200
lconcurrent_cpl = .FALSE.
lcoup_oas_clm = .TRUE.
/

EONL
}
export -f lm_c_INPUT_OAS
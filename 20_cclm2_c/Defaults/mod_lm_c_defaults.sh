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

# input and output directory names
export LM_NL_INPUT_DIR_C=cosmo_input
export LM_NL_OUTPUT_DIR_C=cosmo_output


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

#!/bin/bash

if (( LM_NL_ENS_NUMBER_C > 1 )); then
    
    # Ensemble execution
    # ------------------
    echo "running lm_c in ensemble mode with ${LM_NL_ENS_NUMBER_C} realizations" >> ${status_file}

    # Loop over ensemble members
    (( max_k = ${LM_NL_ENS_NUMBER_C} - 1 ))
    for (( k=0; k<=$max_k; k++ )); do
        # create 0-padded member directory
        member=$(printf "%0${#max_k}d" $k)
        mkdir -p $member

        # Enter member directory
        pushd $member >&/dev/null

        # link necessary paths and exe
        ln -sf ../clean
        ln -sf ../run
        mkdir output
        ln -sf ../input
        ln -sf ../cosmo
        ln -sf ../env.sh

        # Submit job and store job id
        jobid=$(submit)
        jobids+=" ${jobid}"

        # Exit member directory
        popd >&/dev/null
    done
    
    # gather all member ids in a :-separated list and export
    jobid=$(id_list ${jobids})
    
else

    # Normal execution
    # ----------------
    jobid=$(submit)
    
fi

# Return job id(s)
# ----------------
echo $jobid

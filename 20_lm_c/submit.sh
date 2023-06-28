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

    # Status log
    GREEN="\033[32m"
    NORMAL="\033[0;39m"
    echo -e "${GREEN}[[ ${part} ]]${NORMAL}" >> ${status_file}
    echo "[ nodes        ] ${NQS_NODES_LM_C}" >> ${status_file}
    echo "[ time         ] ${NQS_ELAPSED_LM_C}" >> ${status_file}
    echo "[ queue        ] ${NQS_PARTITION_LM_C}" >> ${status_file}
    echo "[ dependencies ] $(get_dep_ids lm_c)" >> ${status_file}
    echo "[ job id       ] ${jobid}" >> ${status_file}
    echo "[ status       ] submitted" >> ${status_file}
        
else

    # Normal execution
    # ----------------
    jobid=$(submit)
    
fi

# Return job id(s)
# ----------------
echo $jobid

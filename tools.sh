#!/bin/bash

mkydirs(){
    # search for lines matching "ydirxxx = 'yyy'" in file given by first
    # argument and create the corresponding directory
    for ydir in $(sed -n 's/^\s*ydir.*=\s*["'\'']\(.*\)["'\'']\s*/\1/p' $1); do
        mkdir -p ${ydir}
    done
}
export -f mkydirs


add_s_oro_max(){
    # Add the S_ORO_MAX variable to $1 if missing by copying the S_ORO variable
    ncdump -h ${1} | grep -q S_ORO_MAX
    if [[ $? != 0 ]]; then
        echo "S_ORO_MAX not in ${1}, copying S_ORO as S_ORO_MAX"
        ncks -O -v S_ORO ${1} tmp.nc
        ncrename -v S_ORO,S_ORO_MAX tmp.nc
        ncks -A -v S_ORO_MAX tmp.nc ${1}
        rm -f tmp.nc
    fi
}
export -f add_s_oro_max


id_list(){
    # Convert a space-separated list in a :-separated one,
    # i.e. list ids in the slurm format
    local IFS=":"
    echo "$*"
}


gen_slurm_dependencies(){
    # Check the dependencies of argument $1 and output
    # the corresponding sbatch option
    eval dep_names=\$${1}_deps
    for dep in ${dep_names}; do
        eval job_id=\$${dep}_id
        [[ -n ${job_id} ]] && dep_ids="${dep_ids} ${job_id}"
    done
    [[ -n ${dep_ids} ]] && echo "--dependency=afterok:$(id_list ${dep_ids})"
}


get_dep_ids(){
    # Check the dependencies of argument $1 and output
    # the corresponding job ids
    eval dep_names=\$${1}_deps
    for dep in ${dep_names}; do
        eval job_id=\$${dep}_id
        [[ -n ${job_id} ]] && dep_ids="${dep_ids} ${job_id}"
    done
    echo $(id_list ${dep_ids})
}
export -f get_dep_ids


compute_nodes(){
    echo $(( ($1 * $2 + $3 + $4 - 1) / $4 ))
}
export -f compute_nodes


submit(){
    # Submit part $1 of the chain
    part="${1}"
    short=${part#[0-9]*_}
    mkdir -p output/${short}

    cd ${part}
    
    # Get dependencies ids
    dep_str=$(gen_slurm_dependencies ${short})
    echo "launching ${short} with the following dependencies: ${dep_str}"
    
    if [[ "${short}" == "lm_c" ]] && (( LM_NL_ENS_NUMBER_C > 1 )); then
        
        # Ensemble execution
        # ------------------
        echo "running lm_c in ensemble mode with ${LM_NL_ENS_NUMBER_C} realizations"

        # Loop over ensemble members
        (( max_k=${LM_NL_ENS_NUMBER_C}-1 ))
        for (( k=0; k<=$max_k; k++ )); do
            # create 0-padded member number directory
            member=$(printf "%0${#max_k}d" $k)
            mkdir -p $member
            cd $member

            # cleanup
            ln -sf ../clean
            ./clean

            # link necessary paths and exe
            ln -sf ../gen_job_script.sh
            ln -sf ../output/$member output
            ln -sf ../input
            ln -sf ../cosmo

            # generate job script
            [[ ${LM_BEGIN_DATE} == ${LM_START_DATE} ]] && ./gen_job_script.sh

            # Submit job and store job id
            output=job_${LM_BEGIN_DATE_FR}_${LM_END_DATE_FR}.out
            jobid=$(sbatch --parsable --output=${output} -C gpu ${dep_str} job)
            jobids="${jobids} ${jobid}"
            
            cd ..
        done
        
        # gather all member ids in a :-separated list and export
        jobids=$(id_list ${jobids})
        eval export current_${short}_id=\${jobids}
        
    else
        
        # Normal execution
        # ----------------
        # generate job script
        [[ ${LM_BEGIN_DATE} == ${LM_START_DATE} ]] && ./gen_job_script.sh
        
        # Submit job and get job id
        output=job_${LM_BEGIN_DATE_FR}_${LM_END_DATE_FR}.out
        jobid=$(sbatch --parsable --output=${output} -C gpu ${dep_str} job)
        
        # Store job id
        eval export current_${short}_id=\${jobid}
    fi
    
    cd - 1>/dev/null 2>/dev/null
}
export -f submit

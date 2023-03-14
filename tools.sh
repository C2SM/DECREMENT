#!/bin/bash

diff_hours(){
    # get difference between 2 dates in hours
    d1=$(date -d "$1" +%s)
    d2=$(date -d "$2" +%s)
    echo $(( ($d2 - $d1) / 3600 ))
}
export -f diff_hours


mkydirs(){
    # search for lines matching "ydirxxx = 'yyy'" in file given by first
    # argument and create the corresponding directory
    for ydir in $(sed -n 's/^\s*ydir.*=\s*["'\'']\(.*\)["'\'']\s*/\1/p' $1); do
        mkdir -p ${ydir}
    done
}
export -f mkydirs


file_list(){
    # get list of COSMO files between 2 dates assuming the "d format"

    # Arguments
    direc="$1"
    date_1="$2"
    date_2="$3"

    # Compute date bounds to seconds since some absolute date
    begin_sec=$(date -d "$date_1" +%s)
    end_sec=$(date -d "$date_2" +%s)

    # Get list
    list=""
    for f in $(find ${direc} -name *.nc); do
        # Capture digits reprensting the date in the file name
        digits=$(sed 's/l.\{1,2\}fd\([[:digit:]]*\).*\.nc/\1/' <<< $(basename ${f}))
        # Form date string
        this_date=${digits:0:4}-${digits:4:2}-${digits:6:2}T${digits:8:2}:${digits:10:2}
        # Convert date to seconds
        this_sec=$(date -d "${this_date}" +%s)
        # Check if it belongs to the interval
        if (( this_sec >= begin_sec && this_sec <= end_sec )); then
            list+=" ${f}"
        fi
    done
    echo ${list}
}
export -f file_list


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
export -f id_list


get_dep_ids(){
    # Check the dependencies of argument $1 and output
    # the corresponding job ids
    eval dep_names=\$${1}_deps
    unset dep_ids
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

#!/bin/bash

# store arguments
date_1="$1"
date_2="$2"
target_dirs="$3"

# Compute date bounds to seconds since some absolute date
begin_sec=$(date -d "$date_1" +%s)
end_sec=$(date -d "$date_2" +%s)

for d in ${target_dirs}; do
    
    resolution=$(dirname $d)
    stream=$(basename $d)
    echo ""
    echo "$resolution/$stream"
    
    # get file list
    k=0
    for f in $(find input/${resolution}/${stream} -name *.nc); do
        # Capture digits reprensting the date in the file name
        digits=$(sed 's/l.\{1,2\}fd\([[:digit:]]*\).*\.nc/\1/' <<< $(basename $f))
        # Convert date to seconds
        this_sec=$(date -d "${digits:0:4}-${digits:4:2}-${digits:6:2}T${digits:8:2}:${digits:10:2}" +%s)
        # Check if it belongs to the interval
        if (( this_sec >= begin_sec && this_sec <= end_sec )); then
            ((k++))
            file_list[$k]=$f
        fi
    done

    if ((${#file_list[@]} > 0)); then        
        # compress files
        ./nczip -kv "${file_list[@]}"

        # archive files
        if [[ $? == 0 ]]; then
            cd input 
            nz_files=$(find ${resolution}/${stream} -name *.nz)
            echo "archiving compressed files and renaming them as xxx.nc in the archive"
            d1=$(date -d "$date_1" +%Y%m%d%H%M)
            d2=$(date -d "$date_2" +%Y%m%d%H%M)
            tar --transform='s/.nz/.nc/' -cvf ../output/${resolution}__${stream}__${d1}-${d2}.tar ${nz_files[@]}
        fi
        
        # remove compressed files
        if [[ $? == 0 ]]; then        
            echo "removing compressed files"
            rm ${nz_files[@]}
        fi

        # remove original files
        if [[ $? == 0 ]]; then
            cd ..
            echo "removing original files"
            echo "rm ${file_list[@]}"
        fi
    else
        echo "no files found between ${date_1} and ${date_2}"
    fi

    unset file_list
    
done

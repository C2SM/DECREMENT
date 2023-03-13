#!/bin/bash

cat > job <<EOF_job
#!/bin/bash -l
#
#SBATCH --job-name=chain
#SBATCH --output=job_${LM_BEGIN_DATE_FR}_${LM_END_DATE_FR}.out
#SBATCH --time=00:05:00
#SBATCH --nodes=1
#SBATCH --partition=normal
#SBATCH --account=${ACCOUNT}

# Check if we are done
if (( \$(date -d "\$LM_END_DATE" +%s) >= \$(date -d "\$LM_FIN_DATE" +%s) )); then
    echo "End of the simulation"
    exit 0
fi

# Swap dates
export LM_BEGIN_DATE=\${LM_END_DATE}

# submit next chunk
cd ..
./run_daint.sh
EOF_job

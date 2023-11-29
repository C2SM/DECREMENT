#!/bin/bash

cclm2_task_dispatch(){
  ((ntasks_cosmo = $1 * $2 + $3))
  ((ntasks_cesm = $4))

  if [[ $COSMO_TARGET == "cpu" ]]; then
    cosmo_tasks="0-$((ntasks_cosmo-1))"
    cesm_tasks="${ntasks_cosmo}-$((ntasks_cosmo+ntasks_cesm-1))"
  else
    cosmo_tasks=""
    cesm_tasks=""
    for ((k=0; k<ntasks_cosmo; k++)); do
      ((n0 = k*12))
      ((n1 = n0+1))
      ((n2 = n0+11))
      cosmo_tasks+="${n0},"
      cesm_tasks+="${n1}-${n2},"
      
      # PS: Test limiting the number of tasks for CLM (relevant for >4x4), not working because OASIS distributes the MPI communicators
      # Test also included writing to ./dummy.sh and using tasks to run it
      # For cesm, add until max 17 nodes = 187 idling cpus
      #if (( k<17 )); then
      #  cesm_tasks+="${n1}-${n2},"
      #else
      #  dummy_tasks+="${n1}-${n2},"
      #fi
    done
    # remove trailing comma
    cosmo_tasks=${cosmo_tasks:0:-1}
    cesm_tasks=${cesm_tasks:0:-1}
  fi
  
  # write task distribution file
  cat > $5 << EOF
${cosmo_tasks} ./cosmo.sh
${cesm_tasks} ./cesm.sh
EOF
}
export -f cclm2_task_dispatch


cclm2_exe_files(){
  # Put code related environmental settings in dedicated
  # executable files before the execution statement (./xxx)

  # Task number suffix
  if [[ ${LM_PER_TASK_LOG} == true ]]; then
    TASK_SUFFIX='_$(printf %0${#SLURM_NTASKS}d ${SLURM_PROCID})'
  else
    TASK_SUFFIX=""
  fi
  
  # COSMO
  cat > cosmo.sh << EOF
#!/bin/bash
source env_cosmo.sh
${EXE_COSMO} 2>&1 > cosmo_${LM_BEGIN_DATE_DG}_${LM_END_DATE_DG}${TASK_SUFFIX}.out
EOF
  chmod 755 cosmo.sh

  # CESM
  cat > cesm.sh << EOF
#!/bin/bash
source env_cesm.sh
${EXE_CESM} 2>&1 > cesm_${LM_BEGIN_DATE_DG}_${LM_END_DATE_DG}${TASK_SUFFIX}.out 
EOF
  chmod 755 cesm.sh
}
export -f cclm2_exe_files


mkcesmdirs(){
  # search for lines matching "timing_dir = 'yyy'" and "tchkpt_dir = 'yyy'"in drv_in
  # and create the corresponding directories
  for param in timing_dir tchkpt_dir; do
    path=$(sed -n 's/^\s*'$param'.*=\s*["'\'']\(.*\)["'\'']\s*/\1/p' drv_in)
    [[ -n $path ]] && mkdir -p $path
  done
}
export -f mkcesmdirs

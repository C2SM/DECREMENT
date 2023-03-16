#!/bin/bash

# clean jobs
for part in [0-9]*_* ; do
  echo "cleaning ${part}"
  cd ${part}
  ./clean
  \rm core *.out INPUT INPUT_* *.swp 2>/dev/null
  cd - 1>/dev/null 2>/dev/null
done

# clean data
\rm -rf input output


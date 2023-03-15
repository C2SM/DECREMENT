#!/bin/bash

for d in [0-9]*_*; do
  cd $d
  ./clean
  rm -f run_*.out
  cd - 1>/dev/null 2>/dev/null
done

#!/bin/bash

for d in [0-9]*_*; do
  cd $d
  ./clean
  rm -f .retry 1>/dev/null 2>/dev/null
  cd - 1>/dev/null 2>/dev/null
done

#!/bin/bash

for d in [0-9]*_*; do
  cd $d
  ./clean
  rm -f .retry > /dev/null 2>&
  cd - > /dev/null
done

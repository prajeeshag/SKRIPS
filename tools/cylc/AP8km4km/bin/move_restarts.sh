#!/bin/bash
set -eu

echo "Move restart files"
mkdir -p $RESTART_DIR
nIter=$(echo "$startTimeNext/$deltaT" | bc)
nIter=$(printf "%010d\n" $nIter)

rsync -a  ./wrfrst_* $RESTART_DIR/
rsync -a ./pickup.$nIter.data ./pickup.$nIter.meta $RESTART_DIR/
rm ./wrfrst_*
rm ./pickup.$nIter.*

#!/bin/bash

set -eu

echo "Link restart files"

# Link restart files
[[ "$CYLC_TASK_CYCLE_POINT" == "$CYLC_WORKFLOW_INITIAL_CYCLE_POINT" ]] && \
echo "Cold start: No restart to link" && exit 0

nIter=$(echo "$startTime/$deltaT" | bc)
nIter=$(printf "%010d\n" $nIter)
dateStamp=$( isodatetime ${CYLC_TASK_CYCLE_POINT} --format CCYY-MM-DDThh:mm:ss | sed 's/T/_/g')

source_file=$RESTART_DIR/pickup.$nIter.meta
[ -e "$source_file" ] && ln -sf "$source_file" . || { echo "Error while linking $source_file "; exit 1;}

source_file=$RESTART_DIR/pickup.$nIter.data
[ -e "$source_file" ] && ln -sf "$source_file" . || { echo "Error while linking $source_file "; exit 1;}

source_file=$RESTART_DIR/wrfrst_d01_$dateStamp
[ -e "$source_file" ] && ln -sf "$source_file" . || { echo "Error while linking $source_file "; exit 1;}
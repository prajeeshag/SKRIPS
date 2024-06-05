#!/bin/bash

for i in $etcdir/wrf/*; do 
    basei=$( basename $i )
    envsubst < $i > $basei
done
srun -n $cpuPerNode $REAL_EXE
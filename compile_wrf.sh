#!/bin/bash
set -x

export SKRIPS_DIR=${SKRIPS_DIR:-$(pwd)}
. $SKRIPS_DIR/etc/env.shaheen_intel
cd $WRF_DIR
printf '50\n1\n' | ./configure &> $SKRIPS_DIR/wrf.configure.log
cp configure.wrf configure.wrf_org
cp $SKRIPS_DIR/etc/configure.wrf.shaheen_intel configure.wrf
./compile $@ em_real 2>&1 | tee $SKRIPS_DIR/wrf.compile.log
linenumber=$(grep -n "bundled:" configure.wrf | cut -d : -f 1)
head -n $((linenumber-1)) configure.wrf > configure.wrf_cpl
 
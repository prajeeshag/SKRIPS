#!/bin/bash
set -x

export SKRIPS_DIR=${SKRIPS_DIR:-$(pwd)}
. $SKRIPS_DIR/etc/env.shaheen_intel
cd $WRF_DIR
printf '50\n1\n' | ./configure &> $SKRIPS_DIR/wrf.configure.log
cp $SKRIPS_DIR/etc/configure.wrf.shaheen_intel configure.wrf
./compile $@ em_real 2>&1 | tee $SKRIPS_DIR/wrf.compile.log
 
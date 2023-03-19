#!/bin/bash
set -x

export SKRIPS_DIR=${SKRIPS_DIR:-$(pwd)}
. etc/env.shaheen_intel
cd $ESMF_DIR
make $@
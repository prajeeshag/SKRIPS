#!/bin/bash
set -x

export SKRIPS_DIR=${SKRIPS_DIR:-$(pwd)}
. $SKRIPS_DIR/etc/env.${MACH}
cd $ESMF_DIR
make $@
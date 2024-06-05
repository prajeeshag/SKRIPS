#!/bin/bash
set -eu

rm -rf *

ln -s ../ungrib/* .

envsubst < $etcdir/wps/namelist.wps > namelist.wps

ln -sf $WPS_DIR/metgrid .
ln -sf $GEO_EM_FILE geo_em.d01.nc

./metgrid/metgrid.exe
grep "Successful completion" metgrid.log

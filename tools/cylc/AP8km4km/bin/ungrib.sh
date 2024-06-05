#!/bin/bash 

set -eu

# start with a clean directory
rm -rf *

envsubst < $etcdir/wps/namelist.wps > namelist.wps

ln -sf $WPS_DIR/ungrib .
cp -f $etcdir/wps/Vtable .
link_grib=$WPS_DIR/link_grib.csh

if [[ $start_year != $end_year ]] || [[ $start_month != $end_month ]]; then 
    $link_grib $ERA_DATA/${start_year}/${start_year}_${start_month}*.grib \
                    $ERA_DATA/${end_year}/${end_year}_${end_month}*.grib
else
    $link_grib $ERA_DATA/${start_year}/${start_year}_${start_month}*.grib
fi

./ungrib/ungrib.exe && grep "Successful completion" ungrib.log

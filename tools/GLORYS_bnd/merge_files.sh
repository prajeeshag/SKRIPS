#!/bin/bash 


CDO=/project/k1028/pag/mambaforge/envs/skrips/bin/cdo
for bnd in E W S; do 
  for varnm in vo uo thetao so; do 
    ifile="${varnm}_${bnd}_????_??.nc"
    ofile="${varnm}_${bnd}.nc"
    $CDO -mergetime ${ifile} $ofile
  done
done


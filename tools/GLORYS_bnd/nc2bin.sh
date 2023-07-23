#!/bin/bash 

for bnd in E W S; do 
  for varnm in vo uo thetao so; do 
    ifile="${varnm}_${bnd}.nc"
    skup mitgcm bndnc2bin $varnm $ifile
  done
done


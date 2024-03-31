#!/bin/bash 


IDIR=/lustre2/project/k1028/pag/DATA/GLORYS
for yy in {2000..2020}; do 
  for mm in {01..12}; do 
for bnd in E W S; do 
  for varnm in vo uo thetao so; do 
    ifile="${IDIR}/${yy}/${varnm}_glorys_daily_${yy}${mm}.nc"
    ofile="${varnm}_${bnd}_${yy}_${mm}.nc"
    ls $ofile &> /dev/null 
    if [[ $? -ne "0" ]]; then 
      echo $yy $mm $bnd $varnm 
      ls -l $ifile
      #rm -f $ifile
    fi
  done
done
done 
done


#!/bin/bash 

IDIR=/lustre2/project/k1028/pag/DATA/GLORYS
for yy in {2000..2020}; do 
  for mm in {01..12}; do 

jobname=${yy}_${mm}_bnd

cat <<EOF > $jobname.sh
#!/bin/bash 
#SBATCH -N 1
#SBATCH --partition=workq
#SBATCH --ntasks=1
#SBATCH -o $jobname.out
#SBATCH -t 23:00:00

LEVELS="0.5,1.5,2.5,3.5,5.,7.,9.5,12.5,16.,20.,24.5,29.5,34.5,40.,46.,52.,58.5,65.5,73.,81.,89.5,98.5,108.,118.5,130.,142.5,156.,170.5,186.,203.,222.,242.5,264.5,288.5,315.,344.,375.,408.5,445.,485.,528.5,575.5,626.5,682.,742.5,808.,879.,956.5,1041.,1132.5,1231.5,1339.,1456.,1587.,1737.,1909.5,2107.,2332.,2587.,2872.,3192.,3552.,3967.,4442."
CDO=/project/k1028/pag/mambaforge/envs/skrips/bin/cdo
for bnd in E W S; do 
  for varnm in vo uo thetao so; do 
    ifile="${IDIR}/${yy}/\${varnm}_glorys_daily_${yy}${mm}.nc"
    ofile="\${varnm}_\${bnd}_${yy}_${mm}.nc"
    gridFile="bndGrid\${bnd}.nc"
    echo -setmisstonn -intlevel,\${LEVELS} -remapbil,\${gridFile} -selvar,\${varnm} \${ifile} \$ofile
    \$CDO -setmisstonn -intlevel,\${LEVELS} -remapbil,\${gridFile} -selvar,\${varnm} \${ifile} \$ofile
  done
done
EOF
sbatch $jobname.sh
done 
done


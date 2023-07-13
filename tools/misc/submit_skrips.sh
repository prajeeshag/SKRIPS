#!/bin/bash
set -xe

. _utils.sh

EXPNAME=cpld1600_438-exch2

WORKDIRPATH=/lustre/scratch/athippp/AP4km2km_test
SETUPDIR=/lustre2/project/k1028/pag/SKRIPS_SETUPS/AP4km2km

cpuOCN=438
cpuATM=1600
EXE=/project/k1028/pag/s2s/SKRIPS/build/AP4km2km/$cpuOCN/main/skrips.exe

if [ ! -f "$EXE" ]; then
  echo "Error: $EXE does not exist...!!!"
  exit 1
fi

atmTimeStep=20
ocnTimeStep=60
esmTimeStep=60


syy=2000
smm=03
sdd=01
eyy=2000
emm=03
edd=03

coupleMode=2

endTime=$(date_diff_seconds "${syy}-${smm}-${sdd} 00:00:00" "${eyy}-${emm}-${edd} 00:00:00")

RUNDIR=$SETUPDIR/RUN
INPUT=$SETUPDIR/INPUTS

WORKDIR=$WORKDIRPATH/$EXPNAME

if [ -d "$WORKDIR" ]; then
  echo $WORKDIR exist...
  exit 1
fi

mkdir -p $WORKDIR

cd $WORKDIR
ntasks_per_node=32
cpuALL=$(( cpuATM + cpuOCN ))
n_nodes=$(( (cpuALL + ntasks_per_node - 1) / ntasks_per_node ))

submitScript=${EXPNAME}.sh

cat << EOF > $submitScript
#!/bin/bash
#SBATCH -N $n_nodes
#SBATCH --partition=workq
#SBATCH --ntasks-per-node=$ntasks_per_node
#SBATCH -o std_out
#SBATCH -t 24:00:00
#SBATCH --mail-user=prajeesh.athippattagopinathan@kaust.edu.sa
#SBATCH --mail-type=ALL

. env.shaheen_intel
time srun --ntasks $cpuALL $EXE

EOF

cp $RUNDIR/* .
ln -sf $INPUT/*  .


sed -i "s/_syy_/$syy/g" namelist.input
sed -i "s/_eyy_/$eyy/g" namelist.input
sed -i "s/_smm_/$smm/g" namelist.input
sed -i "s/_emm_/$emm/g" namelist.input
sed -i "s/_sdd_/$sdd/g" namelist.input
sed -i "s/_edd_/$edd/g" namelist.input

sed -i "s/_atmTimeStep_/$atmTimeStep/g" namelist.input

sed -i "/nio_/d" namelist.input  # remove quilting

sed -i "s/_ocnTimeStep_/$ocnTimeStep/g" data 
sed -i "s/_endTime_/$endTime/g" data 

sed -i "s/_ocnTimeStep_/$ocnTimeStep/g" namelist.rc
sed -i "s/_esmTimeStep_/$esmTimeStep/g" namelist.rc
sed -i "s/_atmTimeStep_/$atmTimeStep/g" namelist.rc
sed -i "s/_cpuATM_/$cpuATM/g" namelist.rc
sed -i "s/_cpuOCN_/$cpuOCN/g" namelist.rc

sed -i "s/_syy_/$syy/g" namelist.rc
sed -i "s/_eyy_/$eyy/g" namelist.rc
sed -i "s/_smm_/$smm/g" namelist.rc
sed -i "s/_emm_/$emm/g" namelist.rc
sed -i "s/_sdd_/$sdd/g" namelist.rc
sed -i "s/_edd_/$edd/g" namelist.rc
sed -i "s/_coupleMode_/$coupleMode/g" namelist.rc

#sbatch $submitScript



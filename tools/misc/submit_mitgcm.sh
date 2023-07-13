#!/bin/bash
set -xe

. _utils.sh

EXPNAME=mitgcm826

WORKDIRPATH=/lustre/scratch/athippp/AP4km2km_test
SETUPDIR=/lustre2/project/k1028/pag/SKRIPS_SETUPS/AP4km2km

atmTimeStep=20
ocnTimeStep=60
esmTimeStep=60

cpuOCN=826
EXEDIR=/project/k1028/pag/s2s/SKRIPS/build/AP4km2km
EXE=${EXEDIR}/${cpuOCN}/mitgcm/mitgcmuv

if [ ! -f "$EXE" ]; then
  echo "Error: $EXE does not exist...!!!"
  exit 1
fi

syy=2000
smm=03
sdd=01
eyy=2000
emm=03
edd=03


endTime=$(date_diff_seconds "${syy}-${smm}-${sdd} 00:00:00" "${eyy}-${emm}-${edd} 00:00:00")


RUNDIR=$SETUPDIR/RUN
INPUT=$SETUPDIR/INPUTS

WORKDIR=$WORKDIRPATH/$EXPNAME

mkdir -p $WORKDIR

cd $WORKDIR
ntasks_per_node=32
n_nodes=$(( (cpuOCN + ntasks_per_node - 1) / ntasks_per_node ))

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
time srun --ntasks $cpuOCN $EXE

EOF

cp $RUNDIR/* .
ln -sf $INPUT/*  .


sed -i "s/_ocnTimeStep_/$ocnTimeStep/g" data 
sed -i "s/_endTime_/$endTime/g" data 

#sbatch $submitScript

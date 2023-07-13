#!/bin/bash
set -xe

EXPNAME=wrf2400

WORKDIRPATH=/lustre/scratch/athippp/AP4km2km_test
SETUPDIR=/lustre2/project/k1028/pag/SKRIPS_SETUPS/AP4km2km

atmTimeStep=20
ocnTimeStep=60
esmTimeStep=60

cpuATM=2400

syy=2000
smm=03
sdd=01
eyy=2000
emm=03
edd=03

EXE=/lustre2/project/k1028/pag/s2s/build_WRF/WRF/main/wrf.exe

RUNDIR=$SETUPDIR/RUN
INPUT=$SETUPDIR/INPUTS

WORKDIR=$WORKDIRPATH/$EXPNAME

mkdir -p $WORKDIR

cd $WORKDIR
ntasks_per_node=32
n_nodes=$(( (cpuATM + ntasks_per_node - 1) / ntasks_per_node ))

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
time srun --ntasks $cpuATM $EXE

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

 sed -i "/auxinput5/d" namelist.input 
 sed -i "/auxhist5/d" namelist.input 

sbatch $submitScript

#!/bin/bash
set -e

exename=ap4km
infile=ap4km_decomp.txt
codeDir=examples/ap4km/code/
SIZEH=tools/misc/SIZE.h
NAMELISTRC=tools/misc/namelist.rc
baseRunDir=/scratch/athippp/ap4km
workDir=/scratch/athippp/ap4km_scale_test/
rootDir=$(pwd)

nx=1000
ny=900


rm -rf tmp && mkdir tmp


get_line() {
    node=$(echo $xline | awk '{print $1}')
    nP=$(echo $xline | awk '{print $2}')
    nPx=$(echo $xline | awk '{print $3}')
    nPy=$(echo $xline | awk '{print $4}')
    sNx=$(echo $xline | awk '{print $5}')
    sNy=$(echo $xline | awk '{print $6}')
    echo "$node $nP $nPx $nPy $sNx $sNy"
}

ready_compile(){
mkdir tmp/code_$node
cp $codeDir/* tmp/code_$node/
cp $SIZEH tmp/code_$node/
sed -i \
        -e "s/_nPy_/$nPy/g" \
        -e "s/_nPx_/$nPx/g" \
        -e "s/_sNx_/$sNx/g" \
        -e "s/_sNy_/$sNy/g" \
        tmp/code_$node/SIZE.h
    execNm=${exename}_${node}nodes

cat << EOF >> build_mitgcm_lib_jobs.list 
./compile.sh build_mitgcm_lib -c tmp/code_$node -e $execNm &> build_mitgcm_lib_${node}.log
EOF

cat << EOF >> build_skrips_jobs.list 
./compile.sh build_skrips -e $execNm &> build_skrips_lib_${node}.log
EOF
}

__ready_build() {
    rm -f  build_mitgcm_lib_jobs.list build_skrips_jobs.list 
    while read line
    do
        xline=$(echo $line | xargs | sed 's/,/ /g' ) #trim whitespaces
        if [ ${xline:0:1} == "#" ]; then
            continue
        fi
        get_line $xline
        ready_compile
    done < $infile
}

__ready_rundir(){

runDir=$workDir/run_$node
mkdir -p $runDir
ln -sf $baseRunDir/* $runDir/
rm -f $runDir/namelist.rc
cp $baseRunDir/namelist.rc $runDir/

sed -i \
        -e "s/_cpuOCN_/$nP/g" \
        -e "s/_cpuATM_/$nP/g" \
        $runDir/namelist.rc

execNm=${exename}_${node}nodes

cat << EOF >> $runDir/_submit.sh 
#!/bin/bash

#SBATCH -N $node
#SBATCH --ntasks=$nP
#SBATCH -t 6:00:00
#SBATCH --partition=workq
#SBATCH --account=k1028
#SBATCH --job-name=$execNm
#SBATCH --output=output.txt
#SBATCH --error=error.txt

. $rootDir/env.mach

srun $rootDir/build/$execNm/main/skrips.exe

EOF

}

__ready_run() {
    while read line
    do
        xline=$(echo $line | xargs | sed 's/,/ /g' ) #trim whitespaces
        if [ ${xline:0:1} == "#" ]; then
            continue
        fi
        get_line $xline
        __ready_rundir
    done < $infile
}

__launch_run() {
    echo "Launching runs...."
    while read line
    do
        xline=$(echo $line | xargs | sed 's/,/ /g' ) #trim whitespaces
        if [ ${xline:0:1} == "#" ]; then
            continue
        fi
        get_line $xline
        runDir=$workDir/run_$node
        echo "launching run in $runDir"
        cd $runDir
        sbatch _submit.sh 
    done < $infile
}

#__ready_run
__launch_run
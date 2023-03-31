#!/bin/bash
set -e

source ./tools/koi/koi
koiname=$0
koidescription="Compile SKRIPS model"
koicolors=0

function __get_absolute_path {
  echo "$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
}

function __source_env {
    if [ -f "env.mach" ]; then
        export SKRIPS_DIR=$(pwd)
        source env.mach
    else
        echo "No env file found"
        echo "NOTE: try again after running: $0 set_machine MACHINENAME "
        exit 1
    fi
}

function __available_machines {
    for i in $(ls etc/env.* 2> /dev/null); do
        echo ${i:8}
    done
}

function available_machines {
    __addarg "-h" "--help" "help" "optional" "" "List available machines"
    __parseargs "$@"
    __available_machines
}

function __set_machine {
    machinename = "$1"
    if [ -f "etc/env.$machinename" ]; then
        ln -sf etc/env.$machinename env.mach
    else
        echo "etc/env.$machinename doesn't exist!!"
        echo "Available machines are:"
        __list_machines
        exit 1
    fi
}

function set_machine {
    __addarg "-h" "--help" "help" "optional" "" "Set a machine"
    __addarg "" "machinename" "positionalvalue" "required" "" "Machine name"
    __parseargs "$@"

    __set_machine $machinename
}

function __build_esmf_lib {

    jobs=$1
    echo "Building ESMF library...."
    cd $ESMF_DIR
    make -j $jobs 2>&1 | tee $SKRIPS_DIR/esmf.compile.log
}

function build_esmf_lib {
    __addarg "-h" "--help" "help" "optional" "" "Build the ESMF library"
    __addarg "-j" "--jobs" "storevalue" "optional" "4" "Allow N parallel jobs at once"
    __parseargs "$@"

    __source_env
    __build_esmf_lib $jobs
}

function __build_wrf_lib {
    jobs=$1
    cd $WRF_DIR
    printf $WRF_CONFIG_OPT | ./configure 2>&1 | tee $SKRIPS_DIR/wrf.configure.log
    cp configure.wrf configure.wrf_org
    cp $SKRIPS_DIR/etc/$WRFCONFIGURE_FILE configure.wrf
    ./compile -j $jobs em_real 2>&1 | tee $SKRIPS_DIR/wrf.compile.log
    linenumber=$(grep -n "bundled:" configure.wrf | cut -d : -f 1)
    head -n $((linenumber-1)) configure.wrf > configure.wrf_cpl
}

function build_wrf_lib {
    __addarg "-h" "--help" "help" "optional" "" "Build the WRF as library"
    __addarg "-j" "--jobs" "storevalue" "optional" "4" "Allow N parallel jobs at once"
    __parseargs "$@"

    __source_env
    __build_wrf_lib $jobs
}

function __build_mitgcm_lib {
    code=$1
    exe=$2
    jobs=$3
    codeAbs=$(__get_absolute_path $code)
    mitgcm_optfile=$SKRIPS_DIR/etc/$MITGCM_OPTFILE
    mitgcm_cplsrc=$SKRIPS_DIR/src/MITgcm
    echo "MITgcm user code directory: $codeAbs"
    echo "MITgcm optfile: $mitgcm_optfile"
    echo 
    mkdir -p build/$exe/mitgcm 
    cd build/$exe/mitgcm
    ${MITGCM_DIR}/tools/genmake2 "-rootdir" "${MITGCM_DIR}" "-mpi" "-mods" "$codeAbs $mitgcm_cplsrc" "-optfile" "$mitgcm_optfile"
    make -j $jobs depend
    make -j $jobs lib
}

function build_mitgcm_lib {
    __addarg "-h" "--help" "help" "optional" "" "Build the MITgcm as library"
    __addarg "-c" "--code" "storevalue" "required" "" "Path to the MITgcm user code directory"
    __addarg "-e" "--exe" "storevalue" "required" "" "Name of the executable"
    __addarg "-j" "--jobs" "storevalue" "optional" "4" "Allow N parallel jobs at once"
    __parseargs "$@"

    __source_env

    __build_mitgcm_lib $code $exe $jobs
}

function build_skrips {
    __addarg "-h" "--help" "help" "optional" "" "Build the SKRIPS coupled model"
    __addarg "-e" "--exe" "storevalue" "required" "" "Name of the executable (Should be same given in build_mitgcm_lib command)"
    __addarg "-j" "--jobs" "storevalue" "optional" "4" "Allow N parallel jobs at once"
    __parseargs "$@"

    __source_env

    # cd to the $exe, it must have created by the earlier commands by this time
    cd build/$exe
    mkdir -p main && cd main
    cp $SKRIPS_DIR/src/coupler/* .
    make -j $jobs
}

__koirun "$@"
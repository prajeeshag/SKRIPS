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
        source env.mach
        export SKRIPS_DIR=$(pwd)
        export MITGCM_DIR=$SKRIPS_DIR/external/MITgcm/
        export WRF_DIR=$SKRIPS_DIR/external/WRF/
        export JASPER_DIR=$SKRIPS_DIR/external/jasper/
        export WPS_DIR=$SKRIPS_DIR/external/WPS/
        export ESMF_DIR=$SKRIPS_DIR/external/esmf/
        export ESMF_LIB=$ESMF_DIR/lib/lib$ESMF_BOPT/$ESMF_OS.$ESMF_COMPILER.$ESMF_ABI.$ESMF_COMM.$ESMF_SITE/
        export ESMF_MOD=$ESMF_DIR/mod/mod$ESMF_BOPT/$ESMF_OS.$ESMF_COMPILER.$ESMF_ABI.$ESMF_COMM.$ESMF_SITE/ 
        export ESMFMKFILE=$ESMF_LIB/esmf.mk
        export JASPERLIB=$SKRIPS_DIR/libs/jasper/lib64
        export JASPERINC=$SKRIPS_DIR/libs/jasper/include
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
    machinename="$1"
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
    clean=$2
    echo "Building ESMF library...."
    cd $ESMF_DIR
    if [[ $clean -eq 1 ]]; then 
      make -j $jobs clean
      return 
    fi
    time make -j $jobs 2>&1 | tee $SKRIPS_DIR/esmf.compile.log
}

function __build_jasper {

    jobs=$1
    echo "Building JASPER...."
    cd $JASPER_DIR
    buildDir=$SKRIPS_DIR/build/jasper
    mkdir -p $buildDir
    prefix=$SKRIPS_DIR/libs

    cmake -G "Unix Makefiles" \
      -H$JASPER_DIR -B$buildDir \
      -DCMAKE_INSTALL_PREFIX=$prefix \
      -DCMAKE_BUILD_TYPE=RELEASE \
      -DJAS_ENABLE_DOC=FALSE \
      -DCMAKE_POLICY_DEFAULT_CMP0074=NEW \

    cd $buildDir && make -j $jobs && make install
}

function __build_wps {
    jobs=$1
    clean=$2
    cd $WPS_DIR

    if [[ $clean -eq 1 ]]; then 
      ./clean -a 
      return 
    fi
   
    printf $WPS_CONFIG_OPT 
    printf $WPS_CONFIG_OPT | ./configure 2>&1 | tee $SKRIPS_DIR/wps.configure.log
    wpsconfig=$SKRIPS_DIR/etc/$WPSCONFIGURE_FILE

    if [ -f $wpsconfig ]; then
      cp $wpsconfig configure.wps
    fi

    time ./compile 2>&1 | tee $SKRIPS_DIR/wps.compile.log
}

function build_jasper {
    __addarg "-h" "--help" "help" "optional" "" "Build JASPER"
    __addarg "-j" "--jobs" "storevalue" "optional" "4" "Allow N parallel jobs at once"
    __parseargs "$@"

    __source_env
    __build_jasper $jobs 
}

function build_wps {
    __addarg "-h" "--help" "help" "optional" "" "Build WPS"
    __addarg "-j" "--jobs" "storevalue" "optional" "4" "Allow N parallel jobs at once"
    __addarg "" "--clean" "flag" "optional" "" "make clean"
    __parseargs "$@"

    __source_env
    __build_wps $jobs $clean
}


function build_esmf_lib {
    __addarg "-h" "--help" "help" "optional" "" "Build the ESMF library"
    __addarg "-j" "--jobs" "storevalue" "optional" "4" "Allow N parallel jobs at once"
    __addarg "" "--clean" "flag" "optional" "" "make clean"
    __parseargs "$@"

    __source_env
    __build_esmf_lib $jobs $clean
}

function __build_wrf_lib {
    jobs=$1
    clean=$2
    debug=$3
    cd $WRF_DIR

    if [[ $clean -eq 1 ]]; then 
      ./clean -a 
      return 
    fi
    
    printf $WRF_CONFIG_OPT | ./configure 2>&1 | tee $SKRIPS_DIR/wrf.configure.log
    
    wrfconfig=$SKRIPS_DIR/etc/$WRFCONFIGURE_FILE

    if [ -f $wrfconfig ]; then
      cp $wrfconfig configure.wrf
    fi
    rm -rf external/esmf_time_f90 && ln -sf $SKRIPS_DIR/external/cesmf_time_f90 external/esmf_time_f90

    if [[ $debug -eq 1 ]]; then
      sed -i '/^FCDEBUG/s/.*/FCDEBUG=-g -traceback/' configure.wrf
    fi
    
    time ./compile -j $jobs em_real 2>&1 | tee $SKRIPS_DIR/wrf.compile.log
    linenumber=$(grep -n "bundled:" configure.wrf | cut -d : -f 1)
    head -n $((linenumber-1)) configure.wrf > configure.wrf_cpl
}

function build_wrf {
    __addarg "-h" "--help" "help" "optional" "" "Build the WRF as library"
    __addarg "-j" "--jobs" "storevalue" "optional" "16" "Allow N parallel jobs at once"
    __addarg "" "--clean" "flag" "optional" "" "clean the build"
    __addarg "" "--debug" "flag" "optional" "" "Compile with debug symbols"
    __parseargs "$@"

    __source_env
    __build_wrf_lib $jobs $clean $debug
}


function __get_mitgcm_domain_parm {
    code=$1
    codeAbs=$code/SIZE.h
    mitgcm_cplsrc=$SKRIPS_DIR/src/MITgcm
    sNx=$(grep -E 'sNx\s*=\s*[0-9]+' $codeAbs | awk -F '=' '{print $2}' | awk '{print $1}' | sed 's/,//g')
    sNy=$(grep -E 'sNy\s*=\s*[0-9]+' $codeAbs | awk -F '=' '{print $2}' | awk '{print $1}' | sed 's/,//g')
    nPx=$(grep -E 'nPx\s*=\s*[0-9]+' $codeAbs | awk -F '=' '{print $2}' | awk '{print $1}' | sed 's/,//g')
    nPy=$(grep -E 'nPy\s*=\s*[0-9]+' $codeAbs | awk -F '=' '{print $2}' | awk '{print $1}' | sed 's/,//g')
    Nx=$(( sNx * nPx ))
    Ny=$(( sNy * nPy ))
    nP=$(( nPx * nPy ))
    echo "${nP}"
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
    domain=$(__get_mitgcm_domain_parm $codeAbs)
    mkdir -p build/$exe/$domain/mitgcm 
    builddir=$(__get_absolute_path build/$exe/$domain)
    cd build/$exe/$domain/mitgcm
    ${MITGCM_DIR}/tools/genmake2 "-rootdir" "${MITGCM_DIR}" "-mpi" "-mods" "$codeAbs $mitgcm_cplsrc" "-optfile" "$mitgcm_optfile"
    make -j $jobs depend
    make -j $jobs lib
    make -j $jobs
    echo "$builddir"
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
    __addarg "-e" "--exe" "storevalue" "required" "" "Path were mitgcm build directory (output of build_mitgcm_lib command)"
    __addarg "-j" "--jobs" "storevalue" "optional" "4" "Allow N parallel jobs at once"
    __addarg "" "--debug" "flag" "optional" "" "Add debug symbols"

    __parseargs "$@"

    __source_env

    if [[ $debug -eq 1 ]]; then
      echo "compiling with debug "
      export DEBUG_OPTS="-g -traceback"
    fi

    # cd to the $exe, it must have created by the earlier commands by this time
    cd $exe
    mkdir -p main && cd main
    cp $SKRIPS_DIR/src/coupler/* .
    make -j $jobs
}

__koirun "$@"

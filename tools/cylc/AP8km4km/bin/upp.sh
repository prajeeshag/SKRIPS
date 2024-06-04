#/bin/bash

set -eu

mydir=$( dirname $0 )
. $mydir/koi

koiname=$0

function __koimain {
    __addarg "-h" "--help" "help" "optional" "" "Run UPP"
    __addarg "-u" "--uppdir" "storevalue" "required" "" "Path to UPP root directory"
    __addarg "-c" "--ctlfile" "storevalue" "required" "" "UPP Post cntrl text file"
    __addarg "-p" "--prefix" "storevalue" "required" "" "WRF output file prefix, e.g. 'wrfout_d01_'"
    __addarg "-r" "--runcmd" "storevalue" "required" "" "run command e.g. 'srun -n 16' "
    __parseargs "$@"

    CRTMDIR=$uppdir/src/lib/crtm2/src/fix

    ln -fs $uppdir/parm/post_avblflds.xml post_avblflds.xml
    ln -fs $uppdir/src/lib/g2tmpl/params_grib2_tbl_new params_grib2_tbl_new
    ln -fs $uppdir/parm/nam_micro_lookup.dat .
    ln -fs $uppdir/parm/hires_micro_lookup.dat .

    ln -fs $CRTMDIR/EmisCoeff/IR_Water/Big_Endian/* .

    ln -fs $CRTMDIR/EmisCoeff/IR_Land/SEcategory/Big_Endian/* .

    ln -fs $CRTMDIR/AerosolCoeff/Big_Endian/AerosolCoeff.bin     ./
    ln -fs $CRTMDIR/CloudCoeff/Big_Endian/CloudCoeff.bin         ./

    ln -fs $CRTMDIR/SpcCoeff/Big_Endian/* .
    ln -fs $CRTMDIR/TauCoeff/ODPS/Big_Endian/* .

    ln -fs ${ctlfile} postxconfig-NT.txt


    for inFile in ${prefix}*; do
        sc=$(echo "$ifile" | rev | cut -c1-2 | rev)
        mn=$(echo "$ifile" | rev | cut -c4-5 | rev)
        hr=$(echo "$ifile" | rev | cut -c7-8 | rev)
        dd=$(echo "$ifile" | rev | cut -c10-11 | rev)
        mm=$(echo "$ifile" | rev | cut -c13-14 | rev)
        yyyy=$(echo "$ifile" | rev | cut -c16-19 | rev)
        dateStamp=${yyyy}-${mm}-${dd}T${hr}:${mn}:${sc}
        dateStamp1=${yyyy}_${mm}_${dd}_${hr}_${mn}_${sc}

        cat <<EOF >itag
        $inFile
        netcdf
        grib2
        $dateStamp
        NCAR
EOF

        oprefix=$(head -n 3 $ctlfile | tail -n 1)

        eval "$runcmd $uppdir/bin/unipost.exe > upp.log"
        rm upp.log #because these files too large
        ofile=${oprefix}_${dateStamp1}.grb2
        mv $oprefix* $ofile
        echo "Wrote file $ofile"
    done
}

__koirun "$@"
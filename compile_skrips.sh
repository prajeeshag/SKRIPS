#!/bin/sh
set -ex

export SKRIPS_DIR=${SKRIPS_DIR:-$(pwd)}
. $SKRIPS_DIR/etc/env.${MACH}


echo "The option file is: $SKRIPS_DIR/etc/$MITGCM_OPTFILE"

# build the MITGCM as a library
mkdir -p build/ 
cp utils/* build/ # copy the scripts to install MITGCM
cp mitCode/* code/ # copy the scripts to install MITGCM
cp mitSettingCA/* code/ # copy the scripts to install MITGCM
cd build
./makescript_fwd.sio.shaheen ${MITGCM_DIR} # install MITGCM, generate *.f files
cd ..


# build the test coupler
cd coupledCode
ln -sf ../build/libmitgcmuv.a
make clean
make
cd ..

if [ -f ./coupledCode/esmf_application ]; then
  echo "Installation is successful!"
  echo The coupled model is installed as ./coupledCode/esmf_application
else 
  echo ERROR! Installation is NOT successful!
fi

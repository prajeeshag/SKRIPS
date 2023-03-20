#!/bin/sh

. ../../etc/env.shaheen_intel

echo "ESMF location? : " ${ESMF_DIR}
echo "WRF413 (with OA coupling) location? : " ${WRF_DIR}
echo "MITgcm (source code) location? : " ${MITGCM_DIR}

export MITGCM_OPT=mitgcm_optfile.ifort
echo "The option file is: $MITGCM_OPT"

# build the MITGCM as a library
mkdir build code
cp utils/* build/ # copy the scripts to install MITGCM
cp mitCode/* code/ # copy the scripts to install MITGCM
cp mitSettingCA/* code/ # copy the scripts to install MITGCM
cd build
./makescript_fwd.sio.shaheen ${MITGCM_DIR} # install MITGCM, generate *.f files
cd ..


# build the test coupler
cd coupledCode
ln -sf ../build/libmitgcmuv.a
make
cd ..

if [ -f ./coupledCode/esmf_application ]; then
  echo "Installation is successful!"
  echo The coupled model is installed as ./coupledCode/esmf_application
else 
  echo ERROR! Installation is NOT successful!
fi

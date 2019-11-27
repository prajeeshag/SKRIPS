#!/bin/sh

WRF_DIR=/home/x_sunr/scripps_kaust_model_github/WRFV412_AO_01/
ESMF_DIR=/home/x_sunr/scripps_kaust_model_github/esmf/
COUPLER_DIR=/home/x_sunr/scripps_kaust_model_github/coupler/

export ESMFMKFILE=$ESMF_DIR/lib/libg/Unicos.intel.64.mpi.default/esmf.mk

make distclean

ln -s $COUPLER_DIR/L3.C1.coupled_RS2012_ring/coupledCode/mod_* .
ln -s $COUPLER_DIR/L3.C1.coupled_RS2012_ring/coupledCode/mitgcm_wrf* .

sed -i s/#include/include/g mod_esmf_atm.F90

ln -s ../build/*.mod .
ln -s ../build/mmout/*.a .
ln -s ../build/setrlstk.o .
ln -s ../build/sigreg.o .

ln -s $WRF_DIR/main/wrf_ESMFMod.o .
ln -s $WRF_DIR/main/module_wrf_top.o .
ln -s $WRF_DIR/main/libwrflib.a .

make

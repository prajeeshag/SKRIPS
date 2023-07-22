# GNU Makefile template for user ESMF application

################################################################################
################################################################################
## This Makefile must be able to find the "esmf.mk" Makefile fragment in the  ##
## 'include' line below. Following the ESMF User's Guide, a complete ESMF     ##
## installation should ensure that a single environment variable "ESMFMKFILE" ##
## is made available on the system. This variable should point to the         ##
## "esmf.mk" file.                                                            ##
##                                                                            ##
## This example Makefile uses the "ESMFMKFILE" environment variable.          ##
##                                                                            ##
## If you notice that this Makefile cannot find variable ESMFMKFILE then      ##
## please contact the person responsible for the ESMF installation on your    ##
## system.                                                                    ##
## As a work-around you can simply hardcode the path to "esmf.mk" in the      ##
## include line below. However, doing so will render this Makefile a lot less ##
## flexible and non-portable.                                                 ##
################################################################################

EXE=skrips.exe
F90_FILES := $(wildcard *.F90)

F90_INTERMEDIATE_FILES := $(patsubst %.F90, %.f90, $(F90_FILES))

OBJECT_FILES := $(patsubst %.F90, %.o, $(F90_FILES))

.PRECIOUS: $(F90_INTERMEDIATE_FILES)

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)
include ./wrflib.mk

FC=$(ESMF_F90COMPILER)

%.o : %.f90
	$(FC) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(WRF_INC) $(ESMF_F90COMPILEFREENOCPP) $(DEBUG_OPTS) $< -I${PWD} 

%.f90 : %.F90
	#cpp $< | sed 's/_nl_/\n/g' > $@
	$(FC) -cpp -E $< | sed 's/_nl_/\n/g' > $@

% : %.c
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<

% : %.C
	$(ESMF_CXXCOMPILER) -c $(ESMF_CXXCOMPILEOPTS) $(ESMF_CXXCOMPILEPATHSLOCAL) $(ESMF_CXXCOMPILEPATHS) $(ESMF_CXXCOMPILECPPFLAGS) $<



# -----------------------------------------------------------------------------
$(EXE): $(OBJECT_FILES) 
	$(FC) $(ESMF_F90LINKOPTS) $(DEBUG_OPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o $@ $^ $(ESMF_F90ESMFLINKLIBS) $(WRF_LIB) -L../mitgcm -lmitgcmuv 

# module dependencies:
mitgcm_wrf.o: mod_esmf_esm.o
mod_esmf_esm.o: mod_esmf_ocn.o mod_esmf_atm.o mod_types.o 
mod_esmf_cpl.o: mod_types.o 
mod_esmf_ocn.o: mod_types.o
mod_esmf_atm.o: mod_types.o
mod_config.o: mod_types.o 
mod_types.o:

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
.PHONY: clean distclean
clean:
	rm -f $(EXE) *.o *.mod 
distclean: clean
	rm -f  *.mod *.a mit*.mod mitgcm_wrf* mod* namelist*

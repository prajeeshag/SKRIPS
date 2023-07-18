# To build this by itself, use the make target esmf_time_f90_only 
# from the top-level WRF Makefile.  
# > cd ../..
# configure
# make esmf_time_f90_only

.SUFFIXES: .F90 .o .f

# get rid of single quotes after comments
# WARNING:  This will break if a quoted string is followed by a comment that has
#           a single quote.
SED_FTN = sed -e "/\!.*'/s/'//g"

RM = /bin/rm -f
AR = ar
#RANLIB	= ranlib
RANLIB	= echo
FGREP = fgrep -iq

OBJS = module_symbols_util.o \
			 module_esmf_extensions.o \
				module_utility.o 

include $(ESMF_LIB)/esmf.mk

default: libesmf_time.a

libesmf_time.a : $(OBJS)
	$(RM) libesmf_time.a
	if [ "$(AR)" != "lib.exe" ] ; then \
          $(AR) ru libesmf_time.a $(OBJS) ; \
        else \
          $(AR) /out:libesmf_time.a $(OBJS) ; \
        fi
	$(RANLIB) libesmf_time.a

.F90.o :
	$(RM) $@
	$(SED_FTN) $*.F90 > $*.b
	$(CPP) -I. $*.b > $*.f
	$(RM) $*.b
	@ if echo $(CPP) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F90 for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f > $*.f.tmp ; \
          mv $*.f.tmp $*.f ; \
        fi
	$(FC) $(ESMF_F90COMPILEPATHS) -o $@ -c $*.f

.F90.f :
	$(RM) $@
	$(SED_FTN) $*.F90 > $*.b
	$(CPP) -I. $*.b > $*.f
	$(RM) $*.b
	@ if echo $(CPP) | $(FGREP) 'DVAR4D'; then \
          echo COMPILING $*.F90 for 4DVAR ; \
          $(WRF_SRC_ROOT_DIR)/var/build/da_name_space.pl $*.f > $*.f.tmp ; \
          mv $*.f.tmp $*.f ; \
        fi

.f.o :
	$(RM) $@
	$(RM) $*.b
	$(FC) -c $*.f

clean : testclean

testclean: 
	@$(RM) *.b *.f *.o *.obj *.i libesmf_time.a *.mod Test1*.exe

superclean: testclean
	@$(RM) Test1*.out make_tests.out

# DEPENDENCIES : only dependencies after this line 

#$$$ update dependencies!  

module_utility.o :  module_symbols_util.o module_esmf_extensions.o


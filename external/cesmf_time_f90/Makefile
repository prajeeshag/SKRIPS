# To build this by itself, use the make target cesmf_time_f90_only 
# from the top-level WRF Makefile.  
# > cd ../..
# configure
# make cesmf_time_f90_only

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

OBJS = CESMF_Alarm.o CESMF_BaseTime.o CESMF_Clock.o CESMF_Time.o \
        Meat.o CESMF_Base.o CESMF_Calendar.o CESMF_Fraction.o   \
        CESMF_TimeInterval.o CESMF_Stubs.o CESMF_Mod.o \
        module_symbols_util.o \
	module_utility.o CESMF_AlarmClock.o

default: libesmf_time.a

tests: Test1_CESMF.exe Test1_WRFU.exe

libesmf_time.a : $(OBJS)
	$(RM) libesmf_time.a
	if [ "$(AR)" != "lib.exe" ] ; then \
          $(AR) ru libesmf_time.a $(OBJS) ; \
        else \
          $(AR) /out:libesmf_time.a $(OBJS) ; \
        fi
	$(RANLIB) libesmf_time.a

Test1_CESMF.f : Test1.F90
	$(RM) Test1_CESMF.b Test1_CESMF.f
	cp Test1.F90 Test1_CESMF.b
	$(CPP) -I. Test1_CESMF.b > Test1_CESMF.f

Test1_CESMF.exe : libesmf_time.a Test1_CESMF.o
	$(FC) -o Test1_CESMF.exe Test1_CESMF.o libesmf_time.a

Test1_WRFU.f : Test1.F90
	$(RM) Test1_WRFU.b Test1_WRFU.f
	sed -e "s/CESMF_Mod/module_utility/g" -e "s/CESMF_/WRFU_/g" Test1.F90 > Test1_WRFU.b
	$(CPP) -I. Test1_WRFU.b > Test1_WRFU.f

Test1_WRFU.exe : libesmf_time.a Test1_WRFU.o
	$(FC) -o Test1_WRFU.exe Test1_WRFU.o libesmf_time.a

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
	$(FC) -o $@ -c $*.f

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

CESMF_Alarm.o : CESMF_Base.o CESMF_Time.o CESMF_TimeInterval.o
CESMF_BaseTime.o : CESMF_Base.o
CESMF_Clock.o : CESMF_Base.o CESMF_Time.o CESMF_TimeInterval.o CESMF_Alarm.o
CESMF_AlarmClock.o : CESMF_Alarm.o CESMF_Clock.o CESMF_Time.o CESMF_TimeInterval.o
CESMF_Time.o : CESMF_Base.o CESMF_BaseTime.o CESMF_TimeInterval.o CESMF_Calendar.o \
              CESMF_Stubs.o
CESMF_Base.o :
CESMF_Calendar.o : CESMF_Base.o CESMF_BaseTime.o
CESMF_Fraction.o : 
CESMF_TimeInterval.o : CESMF_Base.o CESMF_BaseTime.o CESMF_Calendar.o CESMF_Fraction.o
CESMF_Mod.o : CESMF_Alarm.o CESMF_BaseTime.o CESMF_Clock.o CESMF_Time.o \
        CESMF_Base.o CESMF_Calendar.o CESMF_Fraction.o    \
        CESMF_TimeInterval.o CESMF_Stubs.o CESMF_AlarmClock.o
Meat.o : CESMF_Alarm.o CESMF_BaseTime.o CESMF_Clock.o CESMF_Time.o \
        CESMF_Base.o CESMF_Calendar.o CESMF_Fraction.o    \
        CESMF_TimeInterval.o
CESMF_Stubs.o : CESMF_Base.o CESMF_Calendar.o
module_utility.o :  module_symbols_util.o
module_symbols_util.o :  CESMF_Mod.o
Test1.o :  module_utility.o


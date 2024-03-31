! TBH:  This version is for use with the CESMF library embedded in the WRF 
! TBH:  distribution.  
MODULE CESMF_Mod
   USE cesmf_alarmmod
   USE cesmf_basemod
   USE cesmf_basetimemod
   USE cesmf_calendarmod
   USE cesmf_clockmod
   USE cesmf_fractionmod
   USE cesmf_timeintervalmod
   USE cesmf_timemod
   USE cesmf_alarmclockmod
   USE cesmf_stubs   ! add new dummy interfaces and typedefs here as needed
#include <CESMF_TimeMgr.inc>
   INTEGER, PARAMETER :: CESMF_MAX_ALARMS=MAX_ALARMS
!
END MODULE CESMF_Mod

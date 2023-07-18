! TBH:  This version is for use with the CESMF library embedded in the WRF 
! TBH:  distribution.  
MODULE CESMF_Mod
   USE esmf_alarmmod
   USE esmf_basemod
   USE esmf_basetimemod
   USE esmf_calendarmod
   USE esmf_clockmod
   USE esmf_fractionmod
   USE esmf_timeintervalmod
   USE esmf_timemod
   USE esmf_alarmclockmod
   USE esmf_stubs   ! add new dummy interfaces and typedefs here as needed
#include <CESMF_TimeMgr.inc>
   INTEGER, PARAMETER :: CESMF_MAX_ALARMS=MAX_ALARMS
!
END MODULE CESMF_Mod

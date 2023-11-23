!
!==============================================================================
!
!     CESMF Alarm-Clock Module
      module CESMF_AlarmClockMod
!
!==============================================================================
!
! This file contains the AlarmCreate method.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <CESMF_TimeMgr.inc>

!===============================================================================
!BOPI
!
! !MODULE: CESMF_AlarmClockMod
!
! !DESCRIPTION:
! Separate module that uses both CESMF_AlarmMod and CESMF_ClockMod.  
! Separation is needed to avoid cyclic dependence.  
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Alarm}
!
! See {\tt ../include/ESMC\_Alarm.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit CESMF_Alarm and CESMF_Clock
      use CESMF_AlarmMod, only : CESMF_Alarm, CESMF_AlarmSet
      use CESMF_ClockMod, only : CESMF_Clock, CESMF_ClockAddAlarm

      ! associated derived types
      use CESMF_TimeIntervalMod, only : CESMF_TimeInterval
      use CESMF_TimeMod,         only : CESMF_Time

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
     private
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public CESMF_AlarmCreate

!==============================================================================

      contains

!==============================================================================


! Create CESMF_Alarm using CESMF 2.1.0+ semantics
      FUNCTION CESMF_AlarmCreate( clock, RingTime, RingInterval, &
                                 StopTime, Enabled, rc )

        ! return value
        type(CESMF_Alarm) :: CESMF_AlarmCreate
        ! !ARGUMENTS:
        type(CESMF_Clock), intent(inout), optional :: clock
        type(CESMF_Time), intent(in), optional :: RingTime
        type(CESMF_TimeInterval), intent(in), optional :: RingInterval
        type(CESMF_Time), intent(in), optional :: StopTime
        logical, intent(in), optional :: Enabled
        integer, intent(out), optional :: rc
        ! locals
        type(CESMF_Alarm) :: alarmtmp
         ! TBH:  ignore allocate errors, for now
        ALLOCATE( alarmtmp%alarmint )
        CALL CESMF_AlarmSet( alarmtmp,                  &
                            RingTime=RingTime,         &
                            RingInterval=RingInterval, &
                            StopTime=StopTime,         &
                            Enabled=Enabled,           &
                            rc=rc )
        IF ( PRESENT ( clock ) ) THEN
          CALL CESMF_ClockAddAlarm( clock, alarmtmp, rc )
        ENDIF
        CESMF_AlarmCreate = alarmtmp
      END FUNCTION CESMF_AlarmCreate


!------------------------------------------------------------------------------

      end module CESMF_AlarmClockMod

!
!==============================================================================
!
!     CESMF Alarm Module
      module CESMF_AlarmMod
!
!==============================================================================
!
! This file contains the Alarm class definition and all Alarm class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <CESMF_TimeMgr.inc>

!===============================================================================
!BOPI
!
! !MODULE: CESMF_AlarmMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Alarm}
!
! See {\tt ../include/ESMC\_Alarm.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from CESMF base class
      use CESMF_BaseMod

      ! associated derived types
      use CESMF_TimeIntervalMod, only : CESMF_TimeInterval, &
                                       CESMF_TimeIntervalAbsValue
      use CESMF_TimeMod,         only : CESMF_Time

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
     private
!------------------------------------------------------------------------------
!     ! CESMF_Alarm
!
!     ! F90 class type to match C++ Alarm class in size only;
!     !  all dereferencing within class is performed by C++ implementation

! internals for CESMF_Alarm
      type CESMF_AlarmInt
        type(CESMF_TimeInterval) :: RingInterval
        type(CESMF_Time)  :: RingTime
        type(CESMF_Time)  :: PrevRingTime
        type(CESMF_Time)  :: StopTime
        integer :: ID
        integer :: AlarmMutex
        logical :: Ringing
        logical :: Enabled
        logical :: RingTimeSet
        logical :: RingIntervalSet
        logical :: StopTimeSet
      end type

! Actual public type:  this bit allows easy mimic of "deep" CESMF_AlarmCreate
! in CESMF 2.1.0+.  Note that CESMF_AlarmCreate is in a separate module to avoid 
! cyclic dependence.  
! NOTE:  DO NOT ADD NON-POINTER STATE TO THIS DATA TYPE.  It emulates CESMF 
!        shallow-copy-masquerading-as-reference-copy insanity.  
      type CESMF_Alarm
        type(CESMF_AlarmInt), pointer :: alarmint
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public CESMF_Alarm
      public CESMF_AlarmInt   ! needed on AIX but not PGI
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:
      public CESMF_AlarmDestroy
      public CESMF_AlarmSet
      public CESMF_AlarmGet
!      public CESMF_AlarmGetRingInterval
!      public CESMF_AlarmSetRingInterval
!      public CESMF_AlarmGetRingTime
!      public CESMF_AlarmSetRingTime
!      public CESMF_AlarmGetPrevRingTime
!      public CESMF_AlarmSetPrevRingTime
!      public CESMF_AlarmGetStopTime
!      public CESMF_AlarmSetStopTime
      public CESMF_AlarmEnable
      public CESMF_AlarmDisable
      public CESMF_AlarmRingerOn
      public CESMF_AlarmRingerOff
      public CESMF_AlarmIsRinging
!      public CESMF_AlarmCheckRingTime
      public operator(==)
 
! Required inherited and overridden CESMF_Base class methods

!      public CESMF_AlarmRead
!      public CESMF_AlarmWrite
      public CESMF_AlarmValidate
      public CESMF_AlarmPrint

! !PRIVATE MEMBER FUNCTIONS:
      private CESMF_AlarmEQ
!EOPI

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface operator(==)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure CESMF_AlarmEQ

! !DESCRIPTION:
!     This interface overloads the == operator for the {\tt CESMF\_Alarm} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_AlarmSet - Initializes an alarm

! !INTERFACE:
      subroutine CESMF_AlarmSet(alarm, RingTime, RingInterval, PrevRingTime, &
                               StopTime, Enabled, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      type(CESMF_Time), intent(in), optional :: RingTime, PrevRingTime
      type(CESMF_TimeInterval), intent(in), optional :: RingInterval
      type(CESMF_Time), intent(in), optional :: StopTime
      logical, intent(in), optional :: Enabled
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Initializes an {\tt CESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to initialize
!     \item[{[RingTime]}]
!          Optional ring time for one-shot or first repeating alarm
!     \item[{[RingInterval]}]
!          Optional ring interval for repeating alarms
!     \item[{[StopTime]}]
!          Optional stop time for repeating alarms
!     \item[Enabled]
!          Alarm enabled/disabled
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.1, TMG4.7
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%RingTimeSet = .FALSE.
        alarm%alarmint%RingIntervalSet = .FALSE.
        alarm%alarmint%StopTimeSet = .FALSE.
        IF ( PRESENT( RingInterval ) ) THEN
          ! force RingInterval to be positive
          alarm%alarmint%RingInterval = &
            CESMF_TimeIntervalAbsValue( RingInterval )
          alarm%alarmint%RingIntervalSet = .TRUE.
        ENDIF
        IF ( PRESENT( PrevRingTime ) ) THEN
          alarm%alarmint%PrevRingTime = PrevRingTime
        ENDIF
        IF ( PRESENT( RingTime ) ) THEN
          alarm%alarmint%RingTime = RingTime
          alarm%alarmint%RingTimeSet = .TRUE.
        ENDIF
        IF ( PRESENT( StopTime ) ) THEN
          alarm%alarmint%StopTime = StopTime
          alarm%alarmint%StopTimeSet = .TRUE.
        ENDIF
        alarm%alarmint%Enabled = .TRUE.
        IF ( PRESENT( Enabled ) ) THEN
          alarm%alarmint%Enabled = Enabled
        ENDIF
        IF ( PRESENT( rc ) ) THEN
          rc = CESMF_SUCCESS
        ENDIF
        alarm%alarmint%Ringing = .FALSE.
        alarm%alarmint%Enabled = .TRUE.
      ELSE
        IF ( PRESENT( rc ) ) rc = CESMF_FAILURE
      ENDIF

      end subroutine CESMF_AlarmSet



! Deallocate memory for CESMF_Alarm
      SUBROUTINE CESMF_AlarmDestroy( alarm, rc )
         TYPE(CESMF_Alarm), INTENT(INOUT) :: alarm
         INTEGER,          INTENT(  OUT), OPTIONAL :: rc
         IF ( ASSOCIATED( alarm%alarmint ) ) THEN
           DEALLOCATE( alarm%alarmint )
         ENDIF
         ! TBH:  ignore deallocate errors, for now
         IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
      END SUBROUTINE CESMF_AlarmDestroy



!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_AlarmGetRingInterval - Get an alarm's ring interval
!
! !INTERFACE:
      subroutine CESMF_AlarmGetRingInterval(alarm, RingInterval, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(in) :: alarm
      type(CESMF_TimeInterval), intent(out) :: RingInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt CESMF\_Alarm}'s ring interval
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring interval
!     \item[RingInterval]
!          The {\tt Alarm}'s ring interval
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.7
!EOP
      RingInterval = alarm%alarmint%RingInterval

      end subroutine CESMF_AlarmGetRingInterval
 
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_AlarmSetRingInterval - Set an alarm's ring interval
!
! !INTERFACE:
      subroutine CESMF_AlarmSetRingInterval(alarm, RingInterval, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(out) :: alarm
      type(CESMF_TimeInterval), intent(in) :: RingInterval
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt CESMF\_Alarm}'s ring interval
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring interval
!     \item[RingInterval]
!          The {\tt Alarm}'s ring interval
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'CESMF_AlarmSetRingInterval not supported' )
      end subroutine CESMF_AlarmSetRingInterval

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AlarmGetRingTime - Get an alarm's time to ring
!
! !INTERFACE:
      subroutine CESMF_AlarmGetRingTime(alarm, RingTime, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(in) :: alarm
      type(CESMF_Time), intent(out) :: RingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt CESMF\_Alarm}'s time to ring
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the ring time
!     \item[RingTime]
!          The {\tt CESMF\_Alarm}'s ring time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      CALL wrf_error_fatal( 'CESMF_AlarmGetRingTime not supported' )
      end subroutine CESMF_AlarmGetRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AlarmSetRingTime - Set an alarm's time to ring
!
! !INTERFACE:
      subroutine CESMF_AlarmSetRingTime(alarm, RingTime, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(out) :: alarm
      type(CESMF_Time), intent(in) :: RingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt CESMF\_Alarm}'s time to ring
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the ring time
!     \item[RingTime]
!          The {\tt CESMF\_Alarm}'s ring time to set
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.1, TMG4.7, TMG4.8
!EOP
      CALL wrf_error_fatal( 'CESMF_AlarmSetRingTime not supported' )
      end subroutine CESMF_AlarmSetRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AlarmGet - Get an alarm's parameters -- compatibility with CESMF 2.0.1
!
! !INTERFACE:
      subroutine CESMF_AlarmGet(alarm, PrevRingTime, RingInterval, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(in) :: alarm
      type(CESMF_Time), intent(out), optional :: PrevRingTime
      type(CESMF_TimeInterval), intent(out), optional :: RingInterval
      integer, intent(out), optional :: rc
      integer :: ierr

! !DESCRIPTION:
!     Get an {\tt CESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the previous ring time
!     \item[PrevRingTime]
!          The {\tt CESMF\_Alarm}'s previous ring time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP

      ierr = CESMF_SUCCESS

      IF ( PRESENT(PrevRingTime) ) THEN
        CALL CESMF_AlarmGetPrevRingTime(alarm, PrevRingTime, rc=ierr)
      ENDIF
      IF ( PRESENT(RingInterval) ) THEN
        CALL CESMF_AlarmGetRingInterval(alarm, RingInterval, rc=ierr)
      ENDIF

      IF ( PRESENT(rc) ) THEN
        rc = ierr
      ENDIF

      end subroutine CESMF_AlarmGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AlarmGetPrevRingTime - Get an alarm's previous ring time
!
! !INTERFACE:
      subroutine CESMF_AlarmGetPrevRingTime(alarm, PrevRingTime, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(in) :: alarm
      type(CESMF_Time), intent(out) :: PrevRingTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt CESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the previous ring time
!     \item[PrevRingTime]
!          The {\tt CESMF\_Alarm}'s previous ring time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        PrevRingTime = alarm%alarmint%PrevRingTime
        IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = CESMF_FAILURE
      ENDIF
      end subroutine CESMF_AlarmGetPrevRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AlarmSetPrevRingTime - Set an alarm's previous ring time
!
! !INTERFACE:
      subroutine CESMF_AlarmSetPrevRingTime(alarm, PrevRingTime, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(out) :: alarm
      type(CESMF_Time), intent(in) :: PrevRingTime
      integer, intent(out), optional :: rc
   
! !DESCRIPTION:
!     Set an {\tt CESMF\_Alarm}'s previous ring time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the previous ring time
!     \item[PrevRingTime]
!          The {\tt CESMF\_Alarm}'s previous ring time to set
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.7, TMG4.8
!EOP
      CALL wrf_error_fatal( 'CESMF_AlarmSetPrevRingTime not supported' )
      end subroutine CESMF_AlarmSetPrevRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AlarmGetStopTime - Get an alarm's stop time
!
! !INTERFACE:
      subroutine CESMF_AlarmGetStopTime(alarm, StopTime, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(in) :: alarm
      type(CESMF_Time), intent(out) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt CESMF\_Alarm}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to get the stop time
!     \item[StopTime]
!          The {\tt CESMF\_Alarm}'s stop time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'CESMF_AlarmGetStopTime not supported' )
      end subroutine CESMF_AlarmGetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AlarmSetStopTime - Set an alarm's stop time
!
! !INTERFACE:
      subroutine CESMF_AlarmSetStopTime(alarm, StopTime, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(out) :: alarm
      type(CESMF_Time), intent(in) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt CESMF\_Alarm}'s stop time
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to set the stop time
!     \item[StopTime]
!          The {\tt CESMF\_Alarm}'s stop time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.5.2, TMG4.7
!EOP
      CALL wrf_error_fatal( 'CESMF_AlarmSetStopTime not supported' )
      end subroutine CESMF_AlarmSetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_AlarmEnable - Enables an alarm

! !INTERFACE:
      subroutine CESMF_AlarmEnable(alarm, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Enables an {\tt CESMF\_Alarm} to function
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to enable
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.5.3
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Enabled = .TRUE.
        IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = CESMF_FAILURE
      ENDIF
      end subroutine CESMF_AlarmEnable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_AlarmDisable - Disables an alarm

! !INTERFACE:
      subroutine CESMF_AlarmDisable(alarm, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Disables an {\tt CESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to disable
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.5.3
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Enabled = .FALSE.
        IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
      ELSE
        IF ( PRESENT( rc ) ) rc = CESMF_FAILURE
      ENDIF
      end subroutine CESMF_AlarmDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AlarmRingerOn - Turn on an alarm


! !INTERFACE:
      subroutine CESMF_AlarmRingerOn(alarm, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn on an {\tt CESMF\_Alarm}; sets ringing state
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn on
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG4.6
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        IF ( alarm%alarmint%Enabled ) THEN
          alarm%alarmint%Ringing = .TRUE.
          IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
        ELSE
          alarm%alarmint%Ringing = .FALSE.
          IF ( PRESENT( rc ) ) rc = CESMF_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = CESMF_FAILURE
      ENDIF

      end subroutine CESMF_AlarmRingerOn

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AlarmRingerOff - Turn off an alarm

! !INTERFACE:
      subroutine CESMF_AlarmRingerOff(alarm, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(inout) :: alarm  ! really INTENT(OUT)
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Turn off an {\tt CESMF\_Alarm}; unsets ringing state
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to turn off   
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.6
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        alarm%alarmint%Ringing = .FALSE.
        IF ( alarm%alarmint%Enabled ) THEN
          IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
        ELSE
          IF ( PRESENT( rc ) ) rc = CESMF_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = CESMF_FAILURE
      ENDIF
      end subroutine CESMF_AlarmRingerOff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AlarmIsRinging - Check if alarm is ringing

! !INTERFACE:
      function CESMF_AlarmIsRinging(alarm, rc)
!
! !RETURN VALUE:
      logical :: CESMF_AlarmIsRinging

! !ARGUMENTS:
      type(CESMF_Alarm), intent(in) :: alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Check if {\tt CESMF\_Alarm} is ringing.
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check for ringing state  
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.4
!EOP
      IF ( ASSOCIATED( alarm%alarmint ) ) THEN
        IF ( alarm%alarmint%Enabled ) THEN
          CESMF_AlarmIsRinging = alarm%alarmint%Ringing
          IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
        ELSE
          CESMF_AlarmIsRinging = .FALSE.
          IF ( PRESENT( rc ) ) rc = CESMF_FAILURE
        ENDIF
      ELSE
        IF ( PRESENT( rc ) ) rc = CESMF_FAILURE
      ENDIF
      end function CESMF_AlarmIsRinging

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_AlarmCheckRingTime - Method used by a clock to check whether to trigger an alarm
!
! !INTERFACE:
      function CESMF_AlarmCheckRingTime(alarm, ClockCurrTime, positive, rc)
!
! !RETURN VALUE:
      logical :: CESMF_AlarmCheckRingTime
!
! !ARGUMENTS:
      type(CESMF_Alarm), intent(inout) :: alarm
      type(CESMF_Time), intent(in) :: ClockCurrTime
      integer, intent(in) :: positive
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Main method used by a {\tt CESMF\_Clock} to check whether to trigger
!     the {\tt CESMF\_Alarm} 
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to check if time to ring   
!     \item[ClockCurrTime]
!          The {\tt CESMF\_Clock}'s current time
!     \item[positive]
!          Whether to check ring time in the positive or negative direction
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG4.4, TMG4.6
!EOP
      CALL wrf_error_fatal( 'CESMF_AlarmCheckRingTime not supported' )
      CESMF_AlarmCheckRingTime = .FALSE.  ! keep compilers happy
      end function CESMF_AlarmCheckRingTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AlarmEQ - Compare two alarms for equality
!
! !INTERFACE:
      function CESMF_AlarmEQ(alarm1, alarm2)
!
! !RETURN VALUE:
      logical :: CESMF_AlarmEQ

! !ARGUMENTS:
      type(CESMF_Alarm), intent(in) :: alarm1
      type(CESMF_Alarm), intent(in) :: alarm2

! !DESCRIPTION:
!     Compare two alarms for equality; return true if equal, false otherwise
!     Maps to overloaded (==) operator interface function
!
!     The arguments are:
!     \begin{description}
!     \item[alarm1]
!          The first {\tt CESMF\_Alarm} to compare
!     \item[alarm2]
!          The second {\tt CESMF\_Alarm} to compare
!     \end{description}
!
! !REQUIREMENTS:  
!EOP
      CALL wrf_error_fatal( 'CESMF_AlarmEQ not supported ' )
      CESMF_AlarmEQ = .FALSE.       ! keep compilers happy
      end function CESMF_AlarmEQ

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the CESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_AlarmRead - restores an alarm

! !INTERFACE:
      subroutine CESMF_AlarmRead(alarm, RingInterval, RingTime, &
                           PrevRingTime, StopTime, Ringing, &
                           Enabled, ID, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(out) :: alarm
      type(CESMF_TimeInterval), intent(in) :: RingInterval
      type(CESMF_Time), intent(in) :: RingTime
      type(CESMF_Time), intent(in) :: PrevRingTime
      type(CESMF_Time), intent(in) :: StopTime
      logical, intent(in) :: Ringing
      logical, intent(in) :: Enabled
      integer, intent(in) :: ID
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Restores an {\tt CESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to restore
!     \item[RingInterval]
!          The ring interval for repeating alarms
!     \item[RingTime]
!          Ring time for one-shot or first repeating alarm
!     \item[PrevRingTime]
!          The {\tt CESMF\_Alarm}'s previous ring time
!     \item[StopTime]
!          Stop time for repeating alarms
!     \item[Ringing]
!          The {\tt CESMF\_Alarm}'s ringing state
!     \item[Enabled]
!          {\tt CESMF\_Alarm} enabled/disabled
!     \item[ID]
!          The {\tt CESMF\_Alarm}'s ID
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'CESMF_AlarmRead not supported' )
      end subroutine CESMF_AlarmRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_AlarmWrite - saves an alarm

! !INTERFACE:
      subroutine CESMF_AlarmWrite(alarm, RingInterval, RingTime, &
                            PrevRingTime, StopTime, Ringing, &
                            Enabled, ID, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(in) :: alarm
      type(CESMF_TimeInterval), intent(out) :: RingInterval
      type(CESMF_Time), intent(out) :: RingTime
      type(CESMF_Time), intent(out) :: PrevRingTime
      type(CESMF_Time), intent(out) :: StopTime
      logical, intent(out) :: Ringing
      logical, intent(out) :: Enabled
      integer, intent(out) :: ID
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Saves an {\tt CESMF\_Alarm}
!
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          The object instance to save
!     \item[RingInterval]
!          Ring interval for repeating alarms
!     \item[RingTime]
!          Ring time for one-shot or first repeating alarm
!     \item[PrevRingTime]
!          The {\tt CESMF\_Alarm}'s previous ring time
!     \item[StopTime]
!          Stop time for repeating alarms
!     \item[Ringing]
!          The {\tt CESMF\_Alarm}'s ringing state
!     \item[Enabled]
!          {\tt CESMF\_Alarm} enabled/disabled
!     \item[ID]
!          The {\tt CESMF\_Alarm}'s ID
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'CESMF_AlarmWrite not supported' )
      end subroutine CESMF_AlarmWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AlarmValidate - Validate an Alarm's properties

! !INTERFACE:
      subroutine CESMF_AlarmValidate(alarm, opts, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(in) :: alarm
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on a {\tt CESMF\_Alarm}'s properties
!
!     The arguments are:  
!     \begin{description}
!     \item[alarm]
!          {\tt CESMF\_Alarm} to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description} 
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'CESMF_AlarmValidate not supported' )
      end subroutine CESMF_AlarmValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AlarmPrint - Print out an Alarm's properties

! !INTERFACE:
      subroutine CESMF_AlarmPrint(alarm, opts, rc)

! !ARGUMENTS:
      type(CESMF_Alarm), intent(in) :: alarm
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out a {\tt CESMF\_Alarm}'s
!     properties.
! 
!     The arguments are:
!     \begin{description}
!     \item[alarm]
!          {\tt CESMF\_Alarm} to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'CESMF_AlarmPrint not supported' )
      end subroutine CESMF_AlarmPrint

!------------------------------------------------------------------------------

      end module CESMF_AlarmMod

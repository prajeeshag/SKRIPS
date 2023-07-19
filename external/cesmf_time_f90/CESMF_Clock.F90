!
!==============================================================================
!
!     CESMF Clock Module
      module CESMF_ClockMod
!     
!==============================================================================
!     
! This file contains the Clock class definition and all Clock class methods.
!     
!------------------------------------------------------------------------------
! INCLUDES
#include <CESMF_TimeMgr.inc> 

!==============================================================================
!BOPI
! !MODULE: CESMF_ClockMod
!     
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Time} implementation
!     
! See {\tt ../include/ESMC\_Clock.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from CESMF base class
      use CESMF_BaseMod

      ! associated derived types
      use CESMF_TimeIntervalMod   ! , only : CESMF_TimeInterval, &
                                 !          CESMF_TimeIntervalIsPositive
      use CESMF_TimeMod           ! , only : CESMF_Time
      use CESMF_AlarmMod,        only : CESMF_Alarm

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! CESMF_Clock
!     
!     ! F90 class type to match C++ Clock class in size only;
!     !  all dereferencing within class is performed by C++ implementation

! internals for CESMF_Clock
      type CESMF_ClockInt
        type(CESMF_TimeInterval) :: TimeStep
        type(CESMF_Time)  :: StartTime
        type(CESMF_Time)  :: StopTime
        type(CESMF_Time)  :: RefTime
        type(CESMF_Time)  :: CurrTime
        type(CESMF_Time)  :: PrevTime
        integer(CESMF_KIND_I8) :: AdvanceCount
        integer :: ClockMutex
        integer :: NumAlarms
        ! Note:  to mimic CESMF 2.1.0+, AlarmList is maintained 
        ! within CESMF_Clock even though copies of each alarm are 
        ! returned from CESMF_AlarmCreate() at the same time they 
        ! are copied into the AlarmList!  This duplication is not 
        ! as hideous as it might be because the CESMF_Alarm type 
        ! has data members that are all POINTERs (thus the horrible 
        ! shallow-copy-masquerading-as-reference-copy hack works).  
        type(CESMF_Alarm), pointer, dimension(:) :: AlarmList
      end type

! Actual public type:  this bit allows easy mimic of "deep" CESMF_ClockCreate 
! in CESMF 2.1.0+
! NOTE:  DO NOT ADD NON-POINTER STATE TO THIS DATA TYPE.  It emulates CESMF 
!        shallow-copy-masquerading-as-reference-copy.  
      type CESMF_Clock
        type(CESMF_ClockInt), pointer  :: clockint
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public CESMF_Clock
      public CESMF_ClockInt   ! needed on AIX but not PGI
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public CESMF_ClockCreate
      public CESMF_ClockDestroy
      public CESMF_ClockSet
!      public CESMF_ClockSetOLD
      public CESMF_ClockGet
!      public CESMF_ClockGetAdvanceCount
!      public CESMF_ClockGetTimeStep
!      public CESMF_ClockSetTimeStep
!      public CESMF_ClockGetCurrTime
!      public CESMF_ClockSetCurrTime
!      public CESMF_ClockGetStartTime
!      public CESMF_ClockGetStopTime
!      public CESMF_ClockGetRefTime
!      public CESMF_ClockGetPrevTime
!      public CESMF_ClockGetCurrSimTime
!      public CESMF_ClockGetPrevSimTime
! This must be public for CESMF_AlarmClockMod...  
      public CESMF_ClockAddAlarm
      public CESMF_ClockGetAlarmList
!      public CESMF_ClockGetNumAlarms
!      public CESMF_ClockSyncToWallClock
      public CESMF_ClockAdvance
      public CESMF_ClockIsStopTime
      public CESMF_ClockStopTimeDisable

! Required inherited and overridden CESMF_Base class methods

!      public CESMF_ClockRead
!      public CESMF_ClockWrite
      public CESMF_ClockValidate
      public CESMF_ClockPrint
!EOPI

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Set methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockSetOLD - Initialize a clockint

! !INTERFACE:
      subroutine CESMF_ClockSetOLD(clockint, TimeStep, StartTime, &
                                  StopTime, RefTime, rc)

! !ARGUMENTS:
      type(CESMF_ClockInt), intent(out) :: clockint
      type(CESMF_TimeInterval), intent(in), optional :: TimeStep
      type(CESMF_Time), intent(in) :: StartTime
      type(CESMF_Time), intent(in) :: StopTime
      type(CESMF_Time), intent(in), optional :: RefTime
      integer, intent(out), optional :: rc
! Local
      integer i
    
! !DESCRIPTION:
!     Initialize an {\tt CESMF\_Clock}
!     
!     The arguments are:
!     \begin{description}
!     \item[clockint]
!          The object instance to initialize
!     \item[{[TimeStep]}]
!          The {\tt CESMF\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt CESMF\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt CESMF\_Clock}'s stopping time
!     \item[{[RefTime]}]
!          The {\tt CESMF\_Clock}'s reference time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4
!EOP
      IF ( PRESENT(TimeStep) ) clockint%TimeStep = TimeStep
      IF ( PRESENT(RefTime) )THEN
         clockint%RefTime = RefTime
      ELSE
         clockint%RefTime = StartTime
      END IF
      clockint%CurrTime = StartTime
      clockint%StartTime = StartTime
      clockint%StopTime = StopTime
      clockint%NumAlarms = 0
      clockint%AdvanceCount = 0
      ALLOCATE(clockint%AlarmList(MAX_ALARMS))
      ! TBH:  This incredible hack can be removed once CESMF_*Validate() 
      ! TBH:  can tell if a deep CESMF_* was created or not.  
      DO i = 1, MAX_ALARMS
        NULLIFY( clockint%AlarmList( i )%alarmint )
      ENDDO
      IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
    
      end subroutine CESMF_ClockSetOLD


! !IROUTINE: CESMF_ClockSet - Set clock properties -- for compatibility with CESMF 2.0.1

! !INTERFACE:
      subroutine CESMF_ClockSet(clock, TimeStep, StartTime, StopTime, &
                               RefTime, CurrTime, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(inout) :: clock
      type(CESMF_TimeInterval), intent(in), optional :: TimeStep
      type(CESMF_Time), intent(in), optional :: StartTime
      type(CESMF_Time), intent(in), optional :: StopTime
      type(CESMF_Time), intent(in), optional :: RefTime
      type(CESMF_Time), intent(in), optional :: CurrTime
      integer, intent(out), optional :: rc
! Local
      integer ierr
    
! !DESCRIPTION:
!     Initialize an {\tt CESMF\_Clock}
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to initialize
!     \item[{[TimeStep]}]
!          The {\tt CESMF\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt CESMF\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt CESMF\_Clock}'s stopping time
!     \item[{[RefTime]}]
!          The {\tt CESMF\_Clock}'s reference time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!     TMG3.1, TMG3.4.4
!EOP
      ierr = CESMF_SUCCESS
      IF ( PRESENT(TimeStep) ) THEN
        CALL CESMF_ClockSetTimeStep ( clock, TimeStep, rc=ierr )
      ENDIF
      IF ( PRESENT(RefTime) ) clock%clockint%RefTime = RefTime
      IF ( PRESENT(StartTime) ) clock%clockint%StartTime = StartTime
      IF ( PRESENT(StopTime) ) clock%clockint%StopTime = StopTime
      IF ( PRESENT(CurrTime) ) THEN
        CALL CESMF_ClockSetCurrTime(clock, CurrTime, rc=ierr)
      ENDIF
      IF ( PRESENT(rc) ) rc = ierr

      end subroutine CESMF_ClockSet


! Create CESMF_Clock using CESMF 2.1.0+ semantics
      FUNCTION CESMF_ClockCreate( name, TimeStep, StartTime, StopTime, &
                                 RefTime, rc )
        ! return value
        type(CESMF_Clock) :: CESMF_ClockCreate
        ! !ARGUMENTS:
        character (len=*),       intent(in),  optional :: name
        type(CESMF_TimeInterval), intent(in), optional :: TimeStep
        type(CESMF_Time), intent(in) :: StartTime
        type(CESMF_Time), intent(in) :: StopTime
        type(CESMF_Time), intent(in), optional :: RefTime
        integer, intent(out), optional :: rc
        ! locals
        type(CESMF_Clock) :: clocktmp
         ! TBH:  ignore allocate errors, for now
        ALLOCATE( clocktmp%clockint )
        CALL CESMF_ClockSetOLD( clocktmp%clockint,   &
                               TimeStep= TimeStep,  &
                               StartTime=StartTime, &
                               StopTime= StopTime,  &
                               RefTime=RefTime, rc=rc )
        CESMF_ClockCreate = clocktmp
      END FUNCTION CESMF_ClockCreate


! Deallocate memory for CESMF_Clock
      SUBROUTINE CESMF_ClockDestroy( clock, rc )
         TYPE(CESMF_Clock), INTENT(INOUT) :: clock
         INTEGER,          INTENT(  OUT), OPTIONAL :: rc
         ! TBH:  ignore deallocate errors, for now
         DEALLOCATE( clock%clockint%AlarmList )
         DEALLOCATE( clock%clockint )
         IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
      END SUBROUTINE CESMF_ClockDestroy


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockGet - Get clock properties -- for compatibility with CESMF 2.0.1 

! !INTERFACE:
      subroutine CESMF_ClockGet(clock, StartTime, CurrTime,       &
                               AdvanceCount, StopTime, TimeStep, &
                               PrevTime, RefTime, &
                               rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      type(CESMF_Time), intent(out), optional :: StartTime
      type(CESMF_Time), intent(out), optional :: CurrTime
      type(CESMF_Time), intent(out), optional :: StopTime
      type(CESMF_Time), intent(out), optional :: PrevTime
      type(CESMF_Time), intent(out), optional :: RefTime
      integer(CESMF_KIND_I8), intent(out), optional :: AdvanceCount
      type(CESMF_TimeInterval), intent(out), optional :: TimeStep
      integer, intent(out), optional :: rc
      integer :: ierr

! !DESCRIPTION:
!     Returns the number of times the {\tt CESMF\_Clock} has been advanced
!     (time stepped)
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the advance count from
!     \item[StartTime]
!          The start time
!     \item[CurrTime]
!          The current time
!     \item[AdvanceCount]
!          The number of times the {\tt CESMF\_Clock} has been advanced
!     \item[StopTime]
!          The {\tt CESMF\_Clock}'s stopping time
!     \item[{[TimeStep]}]
!          The {\tt CESMF\_Clock}'s time step interval
!     \item[{[PrevTime]}]
!          The {\tt CESMF\_Clock}'s previous current time
!     \item[{[PrevTime]}]
!          The {\tt CESMF\_Clock}'s reference time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG3.5.1
!EOP
      ierr = CESMF_SUCCESS

      IF ( PRESENT (StartTime) ) THEN
        CALL CESMF_ClockGetStartTime( clock, StartTime=StartTime, rc=ierr )
      ENDIF
      IF ( PRESENT (CurrTime) ) THEN
        CALL CESMF_ClockGetCurrTime( clock , CurrTime, ierr )
      ENDIF
      IF ( PRESENT (StopTime) ) THEN
        CALL CESMF_ClockGetStopTime( clock , StopTime, ierr )
      ENDIF
      IF ( PRESENT (AdvanceCount) ) THEN
        CALL CESMF_ClockGetAdvanceCount(clock, AdvanceCount, ierr)
      ENDIF
      IF ( PRESENT (TimeStep) ) THEN
        CALL CESMF_ClockGetTimeStep(clock, TimeStep, ierr)
      ENDIF
      IF ( PRESENT (PrevTime) ) THEN
        CALL CESMF_ClockGetPrevTime(clock, PrevTime, ierr)
      ENDIF
      IF ( PRESENT (RefTime) ) THEN
        CALL CESMF_ClockGetRefTime(clock, RefTime, ierr)
      ENDIF

      IF ( PRESENT (rc) ) THEN
        rc = ierr
      ENDIF
    
      end subroutine CESMF_ClockGet


! !IROUTINE: CESMF_ClockGetAdvanceCount - Get the clock's advance count

! !INTERFACE:
      subroutine CESMF_ClockGetAdvanceCount(clock, AdvanceCount, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      integer(CESMF_KIND_I8), intent(out) :: AdvanceCount
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Returns the number of times the {\tt CESMF\_Clock} has been advanced
!     (time stepped)
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the advance count from
!     \item[AdvanceCount]
!          The number of times the {\tt CESMF\_Clock} has been advanced
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG3.5.1
!EOP

      AdvanceCount = clock%clockint%AdvanceCount

      IF ( PRESENT(rc) ) rc = CESMF_SUCCESS
    
      end subroutine CESMF_ClockGetAdvanceCount

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockGetTimeStep - Get a clock's timestep interval

! !INTERFACE:
      subroutine CESMF_ClockGetTimeStep(clock, TimeStep, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      type(CESMF_TimeInterval), intent(out) :: TimeStep
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt CESMF\_Clock}'s timestep interval
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the time step from
!     \item[TimeStep]
!          The time step
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.2
!EOP

      TimeStep = clock%clockint%TimeStep
      IF ( PRESENT(rc) ) rc = CESMF_SUCCESS
    
      end subroutine CESMF_ClockGetTimeStep

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockSetTimeStep - Set a clock's timestep interval

! !INTERFACE:
      subroutine CESMF_ClockSetTimeStep(clock, TimeStep, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(inout) :: clock  ! really INTENT(OUT)
      type(CESMF_TimeInterval), intent(in) :: TimeStep
      integer, intent(out), optional      :: rc

! !DESCRIPTION:
!     Set an {\tt CESMF\_Clock}'s timestep interval
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set the time step
!     \item[TimeStep]
!          The time step
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.4.2
!EOP

      clock%clockint%TimeStep = TimeStep
      IF ( PRESENT(rc) ) rc = CESMF_SUCCESS

      end subroutine CESMF_ClockSetTimeStep

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockGetCurrTime - Get a clock's current time

! !INTERFACE:
      subroutine CESMF_ClockGetCurrTime(clock, CurrTime, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      type(CESMF_Time), intent(out) :: CurrTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt CESMF\_Clock}'s current time     
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the current time from
!     \item[CurrTime]
!          The current time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.4
!EOP

      CurrTime = clock%clockint%CurrTime
      IF ( PRESENT(rc) ) rc = CESMF_SUCCESS
      end subroutine CESMF_ClockGetCurrTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockSetCurrTime - Set a clock's current time

! !INTERFACE:
      subroutine CESMF_ClockSetCurrTime(clock, CurrTime, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(inout) :: clock  ! really INTENT(OUT)
      type(CESMF_Time), intent(in) :: CurrTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Set an {\tt CESMF\_Clock}'s current time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to set the current time from
!     \item[CurrTime]
!          The current time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.4.3
!EOP

      clock%clockint%CurrTime = CurrTime
      IF ( PRESENT(rc) ) rc = CESMF_SUCCESS
    
      end subroutine CESMF_ClockSetCurrTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockGetStartTime - Get a clock's start time

! !INTERFACE:
      subroutine CESMF_ClockGetStartTime(clock, StartTime, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      type(CESMF_Time), intent(out) :: StartTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt CESMF\_Clock}'s start time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the start time from
!     \item[StartTime]
!          The start time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP

      StartTime = clock%clockint%StartTime
      IF ( PRESENT(rc) ) rc = CESMF_SUCCESS
    
      end subroutine CESMF_ClockGetStartTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockGetStopTime - Get a clock's stop time

! !INTERFACE:
      subroutine CESMF_ClockGetStopTime(clock, StopTime, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      type(CESMF_Time), intent(out) :: StopTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt CESMF\_Clock}'s stop time
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the stop time from
!     \item[StopTime]
!          The stop time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP

      StopTime = clock%clockint%StopTime
      IF ( PRESENT(rc) ) rc = CESMF_SUCCESS
    
      end subroutine CESMF_ClockGetStopTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockGetRefTime - Get a clock's reference time

! !INTERFACE:
      subroutine CESMF_ClockGetRefTime(clock, RefTime, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      type(CESMF_Time), intent(out) :: RefTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt CESMF\_Clock}'s reference time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the reference time from
!     \item[RefTime]
!          The reference time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.3
!EOP
      refTime = clock%clockint%RefTime
      IF ( PRESENT(rc) ) rc = CESMF_SUCCESS
      end subroutine CESMF_ClockGetRefTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockGetPrevTime - Get a clock's previous current time

! !INTERFACE:
      subroutine CESMF_ClockGetPrevTime(clock, PrevTime, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      type(CESMF_Time), intent(out) :: PrevTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt CESMF\_Clock}'s previous current time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the previous current time from
!     \item[PrevTime]
!          The previous current time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.4
!EOP

! hack for bug in PGI 5.1-x
!      prevTime = Clock%clockint%CurrTime - Clock%clockint%TimeStep
      prevTime = CESMF_TimeDec( Clock%clockint%CurrTime, &
                               Clock%clockint%TimeStep )

      IF ( PRESENT(rc) ) rc = CESMF_SUCCESS
      end subroutine CESMF_ClockGetPrevTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockGetCurrSimTime - Get a clock's current simulation time

! !INTERFACE:
      subroutine CESMF_ClockGetCurrSimTime(clock, CurrSimTime, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      type(CESMF_TimeInterval), intent(out) :: CurrSimTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt CESMF\_Clock}'s current simulation time
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the current simulation time from
!     \item[CurrSimTime]
!          The current simulation time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.5
!EOP
      CALL wrf_error_fatal( 'CESMF_ClockGetCurrSimTime not supported' )
      end subroutine CESMF_ClockGetCurrSimTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockGetPrevSimTime - Get a clock's previous simulation time

! !INTERFACE:
      subroutine CESMF_ClockGetPrevSimTime(clock, PrevSimTime, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      type(CESMF_TimeInterval), intent(out) :: PrevSimTime
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt CESMF\_Clock}'s previous simulation time
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the previous simulation time from
!     \item[PrevSimTime]
!          The previous simulation time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG3.5.5
!EOP
      CALL wrf_error_fatal( 'CESMF_ClockGetPrevSimTime not supported' )
      end subroutine CESMF_ClockGetPrevSimTime

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockAddAlarm - Add an alarm to a clock's alarm list

! !INTERFACE:
      subroutine CESMF_ClockAddAlarm(clock, Alarm, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(inout) :: clock
      type(CESMF_Alarm), intent(inout) :: Alarm
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Add an {\tt CESMF\_Alarm} to an {\tt CESMF\_Clock}'s {\tt CESMF\_Alarm} list
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to add an {\tt CESMF\_Alarm} to
!     \item[Alarm]
!          The {\tt CESMF\_Alarm} to add to the {\tt CESMF\_Clock}'s
!          {\tt CESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG4.1, TMG4.2
!EOP
    
      IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
      clock%clockint%NumAlarms = clock%clockint%NumAlarms + 1
      IF ( clock%clockint%NumAlarms > SIZE (clock%clockint%AlarmList) ) THEN
        CALL wrf_error_fatal ( 'CESMF_ClockAddAlarm:  too many alarms' )
      ELSE IF ( .NOT. ASSOCIATED( Alarm%alarmint ) ) THEN
        CALL wrf_error_fatal ( &
               'CESMF_ClockAddAlarm:  alarm not created' )
      ELSE
        IF ( Alarm%alarmint%RingTimeSet ) THEN
           Alarm%alarmint%PrevRingTime = Alarm%alarmint%RingTime
        ELSE
!TBH:  This has the nasty side-effect of forcing us to explicitly turn on 
!TBH:  alarms that are created with RingInterval only, if we want them to start 
!TBH:  ringing right away.  And this is done (see 
!TBH:  COMPUTE_VORTEX_CENTER_ALARM).  Straighten this out...  
           Alarm%alarmint%PrevRingTime = clock%clockint%CurrTime
        ENDIF
        Alarm%alarmint%Ringing = .FALSE.

        ! finally, load the alarm into the list
! write(0,*)'CESMF_ClockAddAlarm ',clock%clockint%NumAlarms
        clock%clockint%AlarmList(clock%clockint%NumAlarms) = Alarm
      ENDIF
    
      end subroutine CESMF_ClockAddAlarm

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockGetAlarmList - Get a clock's alarm list

! !INTERFACE:
      subroutine CESMF_ClockGetAlarmList(clock, AlarmList, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      type(CESMF_Alarm), pointer :: AlarmList(:)
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get an {\tt CESMF\_Clock}'s {\tt CESMF\_Alarm} list     
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the {\tt CESMF\_Alarm} list from
!     \item[AlarmList]
!          The {\tt CESMF\_Clock}'s {\tt CESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG4.3
!EOP

      AlarmList => clock%clockint%AlarmList
      IF ( PRESENT(rc) ) rc = CESMF_SUCCESS

      end subroutine CESMF_ClockGetAlarmList

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockGetNumAlarms - Get the number of alarms in a clock's alarm list

! !INTERFACE:
      subroutine CESMF_ClockGetNumAlarms(clock, NumAlarms, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      integer, intent(out) :: NumAlarms
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get the number of {\tt CESMF\_Alarm}s in an {\tt CESMF\_Clock}'s
!       {\tt CESMF\_Alarm} list     
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to get the number of {\tt CESMF\_Alarm}s from
!     \item[NumAlarms]
!          The number of {\tt CESMF\_Alarm}s in the {\tt CESMF\_Clock}'s
!            {\tt CESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG4.3
!EOP

      NumAlarms = clock%clockint%NumAlarms
      IF ( PRESENT(rc) ) rc = CESMF_SUCCESS
    
      end subroutine CESMF_ClockGetNumAlarms

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockSyncToWallClock - Set clock's current time to wall clock time

! !INTERFACE:
      subroutine CESMF_ClockSyncToWallClock(clock, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(inout) :: clock
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Set an {\tt CESMF\_Clock}'s current time to wall clock time     
!   
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to synchronize to wall clock time
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!   
! !REQUIREMENTS:
!     TMG3.4.5
!EOP
      CALL wrf_error_fatal( 'CESMF_ClockSyncToWallClock not supported' )
      end subroutine CESMF_ClockSyncToWallClock

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockAdvance - Advance a clock's current time by one time step

! !INTERFACE:
      subroutine CESMF_ClockAdvance(clock, RingingAlarmList, &
                                   NumRingingAlarms, rc)

use cesmf_timemod

! !ARGUMENTS:
      type(CESMF_Clock), intent(inout) :: clock
      type(CESMF_Alarm), dimension(MAX_ALARMS), intent(out), optional :: &
                                        RingingAlarmList
      integer, intent(out), optional :: NumRingingAlarms
      integer, intent(out), optional :: rc
! Local
      logical pred1, pred2, pred3
      integer i, n
      type(CESMF_Alarm) :: alarm
      logical :: positive_timestep
!   
! !DESCRIPTION:
!     Advance an {\tt CESMF\_Clock}'s current time by one time step
!  
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to advance
!     \item[{[RingingAlarmList]}]
!          Return a list of any ringing alarms after the time step
!     \item[{[NumRingingAlarms]}]
!          The number of ringing alarms returned
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!  
! !REQUIREMENTS:
!     TMG3.4.1
!EOP
! hack for bug in PGI 5.1-x
!      clock%clockint%CurrTime = clock%clockint%CurrTime + &
!                                clock%clockint%TimeStep
      clock%clockint%CurrTime = CESMF_TimeInc( clock%clockint%CurrTime, &
                                              clock%clockint%TimeStep )
      positive_timestep = CESMF_TimeIntervalIsPositive( clock%clockint%TimeStep )

      IF ( Present(NumRingingAlarms) ) NumRingingAlarms = 0
      clock%clockint%AdvanceCount = clock%clockint%AdvanceCount + 1
      DO i = 1, MAX_ALARMS
        alarm = clock%clockint%AlarmList(i)
        ! TBH:  This is really dangerous.  We need to be able to NULLIFY 
        ! TBH:  alarmint at compile-time (F95 synax) to make this safe.  
!$$$TBH:  see if F95 compile-time pointer-nullification is supported by all 
!$$$TBH:  compilers we support
        IF ( ASSOCIATED( alarm%alarmint ) ) THEN
          IF ( alarm%alarmint%Enabled ) THEN
            IF ( alarm%alarmint%RingIntervalSet ) THEN
              pred1 = .FALSE. ; pred2 = .FALSE. ; pred3 = .FALSE.
              ! alarm cannot ring if clock has passed the alarms stop time
              IF ( alarm%alarmint%StopTimeSet ) THEN
                IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                  PRED1 = clock%clockint%CurrTime > alarm%alarmint%StopTime
                  PRED1 = CESMF_TimeGT( clock%clockint%CurrTime, &
                                       alarm%alarmint%StopTime )
                ELSE
                  ! in this case time step is negative and stop time is 
                  ! less than start time
!                  PRED1 = clock%clockint%CurrTime < alarm%alarmint%StopTime
                  PRED1 = CESMF_TimeLT( clock%clockint%CurrTime, &
                                       alarm%alarmint%StopTime )
                ENDIF
              ENDIF
              ! one-shot alarm:  check for ring time 
! TBH:  Need to remove duplicated code.  Need to enforce only one of 
! TBH:  alarm%alarmint%RingTimeSet or alarm%alarmint%RingIntervalSet ever 
! TBH:  being .TRUE. and simplify the logic.  Also, the simpler 
! TBH:  implementation in the duplicated code below should be sufficient.  
              IF ( alarm%alarmint%RingTimeSet ) THEN
                IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                   PRED2 = ( alarm%alarmint%RingTime <= clock%clockint%CurrTime     &
!                          .AND. clock%clockint%CurrTime < alarm%alarmint%RingTime + &
!                                clock%clockint%TimeStep )
                   PRED2 = ( CESMF_TimeLE( alarm%alarmint%RingTime,       &
                                          clock%clockint%CurrTime )      &
                             .AND. CESMF_TimeLT( clock%clockint%CurrTime, &
                               CESMF_TimeInc( alarm%alarmint%RingTime,    &
                                             clock%clockint%TimeStep ) ) )
                ELSE
                  ! in this case time step is negative and stop time is 
                  ! less than start time
! hack for bug in PGI 5.1-x
!                   PRED2 = ( alarm%alarmint%RingTime >= clock%clockint%CurrTime     &
!                          .AND. clock%clockint%CurrTime > alarm%alarmint%RingTime + &
!                                clock%clockint%TimeStep )
                   PRED2 = ( CESMF_TimeGE( alarm%alarmint%RingTime,       &
                                          clock%clockint%CurrTime )      &
                             .AND. CESMF_TimeGT( clock%clockint%CurrTime, &
                               CESMF_TimeInc( alarm%alarmint%RingTime,    &
                                             clock%clockint%TimeStep ) ) )
                ENDIF
              ENDIF
              ! repeating alarm:  check for ring interval
              IF ( alarm%alarmint%RingIntervalSet ) THEN
                IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                   PRED3 = ( alarm%alarmint%PrevRingTime + alarm%alarmint%RingInterval <= &
!                             clock%clockint%CurrTime )

                   PRED3 = ( CESMF_TimeLE( CESMF_TimeInc(                  &
                                          alarm%alarmint%PrevRingTime,   &
                                          alarm%alarmint%RingInterval ), &
                             clock%clockint%CurrTime ) )
                ELSE
                  ! in this case time step is negative and stop time is 
                  ! less than start time
                  ! ring interval must always be positive
! hack for bug in PGI 5.1-x
!                   PRED3 = ( alarm%alarmint%PrevRingTime - alarm%alarmint%RingInterval >= &
!                             clock%clockint%CurrTime )

                   PRED3 = ( CESMF_TimeGE( CESMF_TimeDec(                  &
                                          alarm%alarmint%PrevRingTime,   &
                                          alarm%alarmint%RingInterval ), &
                             clock%clockint%CurrTime ) )
                ENDIF
              ENDIF
              IF ( (.NOT. pred1) .AND. pred2 ) THEN
                 alarm%alarmint%Ringing = .TRUE.
                 alarm%alarmint%PrevRingTime = clock%clockint%CurrTime
                 alarm%alarmint%RingTimeSet = .FALSE.  !it is a one time alarm, it rang, now let it resort to interval
                 IF ( PRESENT( RingingAlarmList ) .AND. &
                      PRESENT ( NumRingingAlarms ) ) THEN
                   NumRingingAlarms = NumRingingAlarms + 1
                   RingingAlarmList( NumRingingAlarms ) = alarm
                 ENDIF
              ELSE IF ( (.NOT. pred1) .AND. pred3 ) THEN
                 alarm%alarmint%Ringing = .TRUE.
                 IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                   IF ( PRED3) alarm%alarmint%PrevRingTime = alarm%alarmint%PrevRingTime + &
!                                                    alarm%alarmint%RingInterval
                   IF ( PRED3 )                                   &
                     alarm%alarmint%PrevRingTime =                &
                       CESMF_TimeInc( alarm%alarmint%PrevRingTime, &
                                     alarm%alarmint%RingInterval )
                 ELSE
                   ! in this case time step is negative and stop time is
                   ! less than start time
                   ! ring interval must always be positive
! hack for bug in PGI 5.1-x
!                   IF ( PRED3) alarm%alarmint%PrevRingTime = alarm%alarmint%PrevRingTime - &
!                                                    alarm%alarmint%RingInterval
                   IF ( PRED3 )                                   &
                     alarm%alarmint%PrevRingTime =                &
                       CESMF_TimeDec( alarm%alarmint%PrevRingTime, &
                                     alarm%alarmint%RingInterval )
                 ENDIF
                 IF ( PRESENT( RingingAlarmList ) .AND. &
                      PRESENT ( NumRingingAlarms ) ) THEN
                   NumRingingAlarms = NumRingingAlarms + 1
                   RingingAlarmList( NumRingingAlarms ) = alarm
                 ENDIF
              ENDIF
            ELSE IF ( alarm%alarmint%RingTimeSet ) THEN
! TBH:  Need to remove duplicated code.  Need to enforce only one of 
! TBH:  alarm%alarmint%RingTimeSet or alarm%alarmint%RingIntervalSet ever 
! TBH:  being .TRUE. and simplify the logic.  Also, the simpler 
! TBH:  implementation in here should be sufficient.  
              IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!                IF ( alarm%alarmint%RingTime <= clock%clockint%CurrTime ) THEN
                IF ( CESMF_TimeLE( alarm%alarmint%RingTime, &
                                  clock%clockint%CurrTime ) ) THEN
                   alarm%alarmint%RingTimeSet = .FALSE.  !it is a one time alarm, it rang, now let it resort to interval
                   alarm%alarmint%Ringing = .TRUE.
                   alarm%alarmint%PrevRingTime = clock%clockint%CurrTime
                   IF ( PRESENT( RingingAlarmList ) .AND. &
                        PRESENT ( NumRingingAlarms ) ) THEN
                     NumRingingAlarms = NumRingingAlarms + 1
                     RingingAlarmList( NumRingingAlarms ) = alarm
                   ENDIF
                ENDIF
              ELSE
                ! in this case time step is negative and stop time is 
                ! less than start time
! hack for bug in PGI 5.1-x
!                IF ( alarm%alarmint%RingTime >= clock%clockint%CurrTime ) THEN
                IF ( CESMF_TimeGE( alarm%alarmint%RingTime, &
                                  clock%clockint%CurrTime ) ) THEN
                   alarm%alarmint%RingTimeSet = .FALSE.  !it is a one time alarm, it rang, now let it resort to interval
                   alarm%alarmint%Ringing = .TRUE.
                   alarm%alarmint%PrevRingTime = clock%clockint%CurrTime
                   IF ( PRESENT( RingingAlarmList ) .AND. &
                        PRESENT ( NumRingingAlarms ) ) THEN
                     NumRingingAlarms = NumRingingAlarms + 1
                     RingingAlarmList( NumRingingAlarms ) = alarm
                   ENDIF
                ENDIF
              ENDIF
            ENDIF
            IF ( alarm%alarmint%StopTimeSet ) THEN
! TBH:  what is this for???  
            ENDIF
          ENDIF
        ENDIF
        clock%clockint%AlarmList(i) = alarm
      ENDDO
      IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
    
      end subroutine CESMF_ClockAdvance

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockStopTimeDisable - NOOP for compatibility with CESMF 2.1.0+

! !INTERFACE:
      subroutine CESMF_ClockStopTimeDisable(clock, rc)
!
! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      integer, intent(out), optional :: rc

      rc = CESMF_SUCCESS

      end subroutine CESMF_ClockStopTimeDisable

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockIsStopTime - Has the clock reached its stop time ?

! !INTERFACE:
      function CESMF_ClockIsStopTime(clock, rc)
!
! !RETURN VALUE:
      logical :: CESMF_ClockIsStopTime

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      integer, intent(out), optional :: rc
      logical :: positive_timestep

! !DESCRIPTION:
!     Return true if {\tt CESMF\_Clock} has reached its stop time, false 
!     otherwise     
!
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to check
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}

! !REQUIREMENTS:
!     TMG3.5.6
!EOP

      positive_timestep = CESMF_TimeIntervalIsPositive( clock%clockint%TimeStep )
      IF ( positive_timestep ) THEN
! hack for bug in PGI 5.1-x
!        if ( clock%clockint%CurrTime .GE. clock%clockint%StopTime ) THEN
        if ( CESMF_TimeGE( clock%clockint%CurrTime, &
                          clock%clockint%StopTime ) ) THEN
          CESMF_ClockIsStopTime = .TRUE.
        else
          CESMF_ClockIsStopTime = .FALSE.
        endif
      ELSE
! hack for bug in PGI 5.1-x
!        if ( clock%clockint%CurrTime .LE. clock%clockint%StopTime ) THEN
        if ( CESMF_TimeLE( clock%clockint%CurrTime, &
                          clock%clockint%StopTime ) ) THEN
          CESMF_ClockIsStopTime = .TRUE.
        else
          CESMF_ClockIsStopTime = .FALSE.
        endif
      ENDIF
      IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
    
      end function CESMF_ClockIsStopTime

!------------------------------------------------------------------------------
!
! This section defines the overridden Read, Write, Validate and Print methods
! from the CESMF_Base class
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockRead - Restores a clock

! !INTERFACE:
      subroutine CESMF_ClockRead(clock, TimeStep, StartTime, StopTime, &
                                RefTime, CurrTime, PrevTime, AdvanceCount, &
                                AlarmList, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(out) :: clock
      type(CESMF_TimeInterval), intent(in) :: TimeStep
      type(CESMF_Time), intent(in) :: StartTime
      type(CESMF_Time), intent(in) :: StopTime
      type(CESMF_Time), intent(in) :: RefTime
      type(CESMF_Time), intent(in) :: CurrTime
      type(CESMF_Time), intent(in) :: PrevTime
      integer(CESMF_KIND_I8), intent(in) :: AdvanceCount
      type(CESMF_Alarm), dimension(MAX_ALARMS), intent(in) :: AlarmList
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Restore an {\tt CESMF\_Clock}
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to restore
!     \item[TimeStep]
!          The {\tt CESMF\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt CESMF\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt CESMF\_Clock}'s stopping time
!     \item[RefTime]
!          The {\tt CESMF\_Clock}'s reference time
!     \item[CurrTime]
!          The {\tt CESMF\_Clock}'s current time
!     \item[PrevTime]
!          The {\tt CESMF\_Clock}'s previous time
!     \item[AdvanceCount]
!          The number of times the {\tt CESMF\_Clock} has been advanced
!     \item[AlarmList]
!          The {\tt CESMF\_Clock}'s {\tt CESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'CESMF_ClockRead not supported' )
      end subroutine CESMF_ClockRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_ClockWrite - Saves a clock

! !INTERFACE:
      subroutine CESMF_ClockWrite(clock, TimeStep, StartTime, StopTime, &
                            RefTime, CurrTime, PrevTime, AdvanceCount, &
                            AlarmList, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      type(CESMF_TimeInterval), intent(out) :: TimeStep
      type(CESMF_Time), intent(out) :: StartTime
      type(CESMF_Time), intent(out) :: StopTime
      type(CESMF_Time), intent(out) :: RefTime
      type(CESMF_Time), intent(out) :: CurrTime
      type(CESMF_Time), intent(out) :: PrevTime
      integer(CESMF_KIND_I8), intent(out) :: AdvanceCount
      type(CESMF_Alarm), dimension(MAX_ALARMS), intent(out) :: AlarmList
      integer, intent(out), optional :: rc
    
! !DESCRIPTION:
!     Save an {\tt CESMF\_Clock}
!     
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          The object instance to save
!     \item[TimeStep]
!          The {\tt CESMF\_Clock}'s time step interval
!     \item[StartTime]
!          The {\tt CESMF\_Clock}'s starting time
!     \item[StopTime]
!          The {\tt CESMF\_Clock}'s stopping time
!     \item[RefTime]
!          The {\tt CESMF\_Clock}'s reference time
!     \item[CurrTime]
!          The {\tt CESMF\_Clock}'s current time
!     \item[PrevTime]
!          The {\tt CESMF\_Clock}'s previous time
!     \item[AdvanceCount]
!          The number of times the {\tt CESMF\_Clock} has been advanced
!     \item[AlarmList]
!          The {\tt CESMF\_Clock}'s {\tt CESMF\_Alarm} list
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!     
! !REQUIREMENTS:
!EOP
      CALL wrf_error_fatal( 'CESMF_ClockWrite not supported' )
      end subroutine CESMF_ClockWrite

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_ClockValidate - Validate a Clock's properties

! !INTERFACE:
      subroutine CESMF_ClockValidate(clock, opts, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Perform a validation check on an {\tt CESMF\_Clock}'s properties
!
!     The arguments are:  
!     \begin{description}
!     \item[clock]
!          {\tt CESMF\_Clock} to validate
!     \item[{[opts]}]
!          Validate options
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description} 
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'CESMF_ClockValidate not supported' )
      end subroutine CESMF_ClockValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_ClockPrint - Print out a Clock's properties

! !INTERFACE:
      subroutine CESMF_ClockPrint(clock, opts, rc)

! !ARGUMENTS:
      type(CESMF_Clock), intent(in) :: clock
      character (len=*), intent(in), optional :: opts
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     To support testing/debugging, print out an {\tt CESMF\_Clock}'s
!     properties.
! 
!     The arguments are:
!     \begin{description}
!     \item[clock]
!          {\tt CESMF\_Clock} to print out
!     \item[{[opts]}]
!          Print options
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
      CALL wrf_error_fatal( 'CESMF_ClockPrint not supported' )
      end subroutine CESMF_ClockPrint

!------------------------------------------------------------------------------

      end module CESMF_ClockMod

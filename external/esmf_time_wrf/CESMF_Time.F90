!
!==============================================================================
!
!     CESMF Time Module
      module CESMF_TimeMod
!
!==============================================================================
!
! This file contains the Time class definition and all Time class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <CESMF_TimeMgr.inc>

!==============================================================================
!BOPI
! !MODULE: CESMF_TimeMod
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! Defines F90 wrapper entry points for corresponding
! C++ class {\tt ESMC\_Time} implementation
!
! See {\tt ../include/ESMC\_Time.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      ! inherit from CESMF base class
      use CESMF_BaseMod

      ! inherit from base time class
      use CESMF_BaseTimeMod

      ! associated derived types
      use CESMF_TimeIntervalMod
      use CESMF_CalendarMod
      use CESMF_Stubs

      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! CESMF_Time
!
!     ! F90 class type to match C++ Time class in size only;
!     !  all dereferencing within class is performed by C++ implementation

     type CESMF_Time
       type(CESMF_BaseTime) :: basetime           ! inherit base class
       ! time instant is expressed as year + basetime
       integer :: YR
       type(CESMF_Calendar), pointer :: calendar  ! associated calendar
     end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public CESMF_Time
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
      public CESMF_TimeGet
      public CESMF_TimeSet

! Required inherited and overridden CESMF_Base class methods

      public CESMF_TimeCopy

! !PRIVATE MEMBER FUNCTIONS:

      private CESMF_TimeGetDayOfYear
      private CESMF_TimeGetDayOfYearInteger

! Inherited and overloaded from CESMF_BaseTime

      ! NOTE:  CESMF_TimeInc, CESMF_TimeDec, CESMF_TimeDiff, CESMF_TimeEQ, 
      !        CESMF_TimeNE, CESMF_TimeLT, CESMF_TimeGT, CESMF_TimeLE, and 
      !        CESMF_TimeGE are PUBLIC only to work around bugs in the 
      !        PGI 5.1-x compilers.  They should all be PRIVATE.  

      public operator(+)
      public CESMF_TimeInc

      public operator(-)
      public CESMF_TimeDec
      public CESMF_TimeDec2
      public CESMF_TimeDiff

      public operator(.EQ.)
      public CESMF_TimeEQ

      public operator(.NE.)
      public CESMF_TimeNE

      public operator(.LT.)
      public CESMF_TimeLT

      public operator(.GT.)
      public CESMF_TimeGT

      public operator(.LE.)
      public CESMF_TimeLE

      public operator(.GE.)
      public CESMF_TimeGE

!EOPI

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface CESMF_TimeGetDayOfYear

! !PRIVATE MEMBER FUNCTIONS:
      module procedure CESMF_TimeGetDayOfYearInteger

! !DESCRIPTION:
!     This interface overloads the {\tt CESMF\_GetDayOfYear} method
!     for the {\tt CESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(+)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure CESMF_TimeInc, CESMF_TimeInc2

! !DESCRIPTION:
!     This interface overloads the + operator for the {\tt CESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface assignment (=)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure CESMF_TimeCopy

! !DESCRIPTION:
!     This interface overloads the = operator for the {\tt CESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(-)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure CESMF_TimeDec, CESMF_TimeDec2

! !DESCRIPTION:
!     This interface overloads the - operator for the {\tt CESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(-)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure CESMF_TimeDiff

! !DESCRIPTION:
!     This interface overloads the - operator for the {\tt CESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.EQ.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure CESMF_TimeEQ

! !DESCRIPTION:
!     This interface overloads the .EQ. operator for the {\tt CESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.NE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure CESMF_TimeNE

! !DESCRIPTION:
!     This interface overloads the .NE. operator for the {\tt CESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.LT.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure CESMF_TimeLT

! !DESCRIPTION:
!     This interface overloads the .LT. operator for the {\tt CESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.GT.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure CESMF_TimeGT

! !DESCRIPTION:
!     This interface overloads the .GT. operator for the {\tt CESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.LE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure CESMF_TimeLE

! !DESCRIPTION:
!     This interface overloads the .LE. operator for the {\tt CESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface operator(.GE.)

! !PRIVATE MEMBER FUNCTIONS:
      module procedure CESMF_TimeGE

! !DESCRIPTION:
!     This interface overloads the .GE. operator for the {\tt CESMF\_Time} class
!
!EOP
      end interface
!
!------------------------------------------------------------------------------

!==============================================================================

      contains

!==============================================================================
!
! Generic Get/Set routines which use F90 optional arguments
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_TimeGet - Get value in user-specified units

! !INTERFACE:
      subroutine CESMF_TimeGet(time, YY, YRl, MM, DD, D, Dl, H, M, S, Sl, MS, &
                              US, NS, d_, h_, m_, s_, ms_, us_, ns_, Sn, Sd, &
                              dayOfYear, dayOfYear_r8, dayOfYear_intvl,      &
                              timeString, rc)

! !ARGUMENTS:
      type(CESMF_Time), intent(in) :: time
      integer, intent(out), optional :: YY
      integer(CESMF_KIND_I8), intent(out), optional :: YRl
      integer, intent(out), optional :: MM
      integer, intent(out), optional :: DD
      integer, intent(out), optional :: D
      integer(CESMF_KIND_I8), intent(out), optional :: Dl
      integer, intent(out), optional :: H
      integer, intent(out), optional :: M
      integer, intent(out), optional :: S
      integer(CESMF_KIND_I8), intent(out), optional :: Sl
      integer, intent(out), optional :: MS
      integer, intent(out), optional :: US
      integer, intent(out), optional :: NS
      double precision, intent(out), optional :: d_
      double precision, intent(out), optional :: h_
      double precision, intent(out), optional :: m_
      double precision, intent(out), optional :: s_
      double precision, intent(out), optional :: ms_
      double precision, intent(out), optional :: us_
      double precision, intent(out), optional :: ns_
      integer, intent(out), optional :: Sn
      integer, intent(out), optional :: Sd
      integer, intent(out), optional :: dayOfYear
      ! dayOfYear_r8 = 1.0 at 0Z on 1 January, 1.5 at 12Z on
      ! 1 January, etc.
      real(CESMF_KIND_R8), intent(out), optional :: dayOfYear_r8
      character (len=*), intent(out), optional :: timeString
      type(CESMF_TimeInterval), intent(out), optional :: dayOfYear_intvl
      integer, intent(out), optional :: rc

      type(CESMF_TimeInterval) :: day_step
      integer :: ierr

! !DESCRIPTION:
!     Get the value of the {\tt CESMF\_Time} in units specified by the user
!     via F90 optional arguments.
!
!     Time manager represents and manipulates time internally with integers
!     to maintain precision. Hence, user-specified floating point values are
!     converted internally from integers.
!
!     See {\tt ../include/ESMC\_BaseTime.h and ../include/ESMC\_Time.h} for
!     complete description.
!     
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[{[YY]}]
!          Integer year CCYR (>= 32-bit)
!     \item[{[YRl]}]
!          Integer year CCYR (large, >= 64-bit)
!     \item[{[MM]}]
!          Integer month 1-12
!     \item[{[DD]}]
!          Integer day of the month 1-31
!     \item[{[D]}]
!          Integer Julian days (>= 32-bit)
!     \item[{[Dl]}]
!          Integer Julian days (large, >= 64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds (>= 32-bit)
!     \item[{[Sl]}]
!          Integer seconds (large, >= 64-bit)
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.1, TMG2.5.1, TMG2.5.6
!EOP
      TYPE(CESMF_Time) :: begofyear
      INTEGER :: year, month, dayofmonth, hour, minute, second
      REAL(CESMF_KIND_R8) :: rsec

      ierr = CESMF_SUCCESS

      IF ( PRESENT( YY ) ) THEN
        YY = time%YR
      ENDIF
      IF ( PRESENT( MM ) ) THEN
        CALL timegetmonth( time, MM )
      ENDIF
      IF ( PRESENT( DD ) ) THEN
        CALL timegetdayofmonth( time, DD )
      ENDIF
!
!$$$ Push HMS down into CESMF_BaseTime from EVERYWHERE
!$$$ and THEN add CESMF scaling behavior when other args are present...  
      IF ( PRESENT( H ) ) THEN
        H = mod( time%basetime%S, SECONDS_PER_DAY ) / SECONDS_PER_HOUR
      ENDIF
      IF ( PRESENT( M ) ) THEN
        M = mod( time%basetime%S, SECONDS_PER_HOUR) / SECONDS_PER_MINUTE
      ENDIF
      IF ( PRESENT( S ) ) THEN
        S = mod( time%basetime%S, SECONDS_PER_MINUTE )
      ENDIF
      ! TBH:  HACK to allow DD and S to behave as in CESMF 2.1.0+ when 
      ! TBH:  both are present and H and M are not.  
      IF ( PRESENT( S ) .AND. PRESENT( DD ) ) THEN
        IF ( ( .NOT. PRESENT( H ) ) .AND. ( .NOT. PRESENT( M ) ) ) THEN
          S = mod( time%basetime%S, SECONDS_PER_DAY )
        ENDIF
      ENDIF
      IF ( PRESENT( MS ) ) THEN
        IF ( time%basetime%Sd /= 0 ) THEN
          MS = NINT( ( time%basetime%Sn*1.0D0 / time%basetime%Sd*1.0D0 ) * 1000.0D0 )
        ELSE
          MS = 0
        ENDIF
      ENDIF
      IF ( PRESENT( Sd ) .AND. PRESENT( Sn ) ) THEN
        Sd = time%basetime%Sd
        Sn = time%basetime%Sn
      ENDIF
      IF ( PRESENT( dayOfYear ) ) THEN
        CALL CESMF_TimeGetDayOfYear( time, dayOfYear, rc=ierr )
      ENDIF
      IF ( PRESENT( dayOfYear_r8 ) ) THEN
        ! 64-bit IEEE 754 has 52-bit mantisssa -- only need 25 bits to hold 
        ! number of seconds in a year...  
        rsec = REAL( time%basetime%S, CESMF_KIND_R8 )
        IF ( time%basetime%Sd /= 0 ) THEN
          rsec = rsec + ( REAL( time%basetime%Sn, CESMF_KIND_R8 ) / &
                          REAL( time%basetime%Sd, CESMF_KIND_R8 ) )
        ENDIF
        dayOfYear_r8 = rsec / REAL( SECONDS_PER_DAY, CESMF_KIND_R8 )
        ! start at 1
        dayOfYear_r8 = dayOfYear_r8 + 1.0_CESMF_KIND_R8
      ENDIF
      IF ( PRESENT( timeString ) ) THEN
        ! This duplication for YMD is an optimization that avoids calling 
        ! timegetmonth() and timegetdayofmonth() when it is not needed.  
        year = time%YR
        CALL timegetmonth( time, month )
        CALL timegetdayofmonth( time, dayofmonth )
!$$$ push HMS down into CESMF_BaseTime
        hour = mod( time%basetime%S, SECONDS_PER_DAY ) / SECONDS_PER_HOUR
        minute = mod( time%basetime%S, SECONDS_PER_HOUR) / SECONDS_PER_MINUTE
        second = mod( time%basetime%S, SECONDS_PER_MINUTE )
        CALL CESMFold_TimeGetString( year, month, dayofmonth, &
                                    hour, minute, second, timeString )
      ENDIF
      IF ( PRESENT( dayOfYear_intvl ) ) THEN
        year = time%YR
        CALL CESMF_TimeSet( begofyear, yy=year, mm=1, dd=1, s=0, &
                           calendar=time%calendar, rc=ierr )
        IF ( ierr == CESMF_FAILURE)THEN
           rc = ierr
           RETURN
        END IF
        CALL CESMF_TimeIntervalSet( day_step, d=1, s=0, rc=ierr )
        dayOfYear_intvl = time - begofyear + day_step
      ENDIF

      IF ( PRESENT( rc ) ) THEN
        rc = ierr
      ENDIF

      end subroutine CESMF_TimeGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_TimeSet - Initialize via user-specified unit set

! !INTERFACE:
      subroutine CESMF_TimeSet(time, YY, YRl, MM, DD, D, Dl, H, M, S, Sl, &
                              MS, US, NS, d_, h_, m_, s_, ms_, us_, ns_, &
                              Sn, Sd, calendar, rc)

! !ARGUMENTS:
      type(CESMF_Time), intent(inout) :: time
      integer, intent(in), optional :: YY
      integer(CESMF_KIND_I8), intent(in), optional :: YRl
      integer, intent(in), optional :: MM
      integer, intent(in), optional :: DD
      integer, intent(in), optional :: D
      integer(CESMF_KIND_I8), intent(in), optional :: Dl
      integer, intent(in), optional :: H
      integer, intent(in), optional :: M
      integer, intent(in), optional :: S
      integer(CESMF_KIND_I8), intent(in), optional :: Sl
      integer, intent(in), optional :: MS
      integer, intent(in), optional :: US
      integer, intent(in), optional :: NS
      double precision, intent(in), optional :: d_
      double precision, intent(in), optional :: h_
      double precision, intent(in), optional :: m_
      double precision, intent(in), optional :: s_
      double precision, intent(in), optional :: ms_
      double precision, intent(in), optional :: us_
      double precision, intent(in), optional :: ns_
      integer, intent(in), optional :: Sn
      integer, intent(in), optional :: Sd
      type(CESMF_Calendar), intent(in), target, optional :: calendar
      integer, intent(out), optional :: rc
      ! locals
      INTEGER :: ierr

! !DESCRIPTION:
!     Initializes a {\tt CESMF\_Time} with a set of user-specified units
!     via F90 optional arguments.
!
!     Time manager represents and manipulates time internally with integers
!     to maintain precision. Hence, user-specified floating point values are
!     converted internally to integers.
!
!     See {\tt ../include/ESMC\_BaseTime.h and ../include/ESMC\_Time.h} for
!     complete description.
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to initialize
!     \item[{[YY]}]
!          Integer year CCYR (>= 32-bit)
!     \item[{[YRl]}]
!          Integer year CCYR (large, >= 64-bit)
!     \item[{[MM]}]
!          Integer month 1-12
!     \item[{[DD]}]
!          Integer day of the month 1-31
!     \item[{[D]}]
!          Integer Julian days (>= 32-bit)
!     \item[{[Dl]}]
!          Integer Julian days (large, >= 64-bit)
!     \item[{[H]}]
!          Integer hours
!     \item[{[M]}]
!          Integer minutes
!     \item[{[S]}]
!          Integer seconds (>= 32-bit)
!     \item[{[Sl]}]
!          Integer seconds (large, >= 64-bit)
!     \item[{[MS]}]
!          Integer milliseconds
!     \item[{[US]}]
!          Integer microseconds
!     \item[{[NS]}]
!          Integer nanoseconds
!     \item[{[d\_]}]
!          Double precision days
!     \item[{[h\_]}]
!          Double precision hours
!     \item[{[m\_]}]
!          Double precision minutes
!     \item[{[s\_]}]
!          Double precision seconds
!     \item[{[ms\_]}]
!          Double precision milliseconds
!     \item[{[us\_]}]
!          Double precision microseconds
!     \item[{[ns\_]}]
!          Double precision nanoseconds
!     \item[{[Sn]}]
!          Integer fractional seconds - numerator
!     \item[{[Sd]}]
!          Integer fractional seconds - denominator
!     \item[{[cal]}]
!          Associated {\tt Calendar}
!     \item[{[tz]}]
!          Associated timezone (hours offset from GMT, e.g. EST = -5)
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
!  PRINT *,'DEBUG:  BEGIN CESMF_TimeSet()'
!$$$ push this down into CESMF_BaseTime constructor
      time%basetime%S  = 0
      time%basetime%Sn = 0
      time%basetime%Sd = 0

      IF ( PRESENT( rc ) ) rc = CESMF_FAILURE
      time%YR = 0
      IF ( PRESENT( YY ) ) THEN
!  PRINT *,'DEBUG:  CESMF_TimeSet():  YY = ',YY
        time%YR = YY
      ENDIF
      IF ( PRESENT( MM ) ) THEN
!  PRINT *,'DEBUG:  CESMF_TimeSet():  MM = ',MM
        CALL timeaddmonths( time, MM, ierr )
        IF ( ierr == CESMF_FAILURE ) THEN
          IF ( PRESENT( rc ) ) THEN
            rc = CESMF_FAILURE
            RETURN
          ENDIF
        ENDIF
!  PRINT *,'DEBUG:  CESMF_TimeSet():  back from timeaddmonths'
      ENDIF
      IF ( PRESENT( DD ) ) THEN
!$$$ no check for DD in range of days of month MM yet
!$$$ Must separate D and DD for correct interface!
!  PRINT *,'DEBUG:  CESMF_TimeSet():  DD = ',DD
        time%basetime%S = time%basetime%S + &
          ( SECONDS_PER_DAY * INT( (DD-1), CESMF_KIND_I8 ) )
      ENDIF
!$$$ push H,M,S,Sn,Sd,MS down into CESMF_BaseTime constructor
      IF ( PRESENT( H ) ) THEN
!  PRINT *,'DEBUG:  CESMF_TimeSet():  H = ',H
        time%basetime%S = time%basetime%S + &
          ( SECONDS_PER_HOUR * INT( H, CESMF_KIND_I8 ) )
      ENDIF
      IF ( PRESENT( M ) ) THEN
!  PRINT *,'DEBUG:  CESMF_TimeSet():  M = ',M
        time%basetime%S = time%basetime%S + &
          ( SECONDS_PER_MINUTE * INT( M, CESMF_KIND_I8 ) )
      ENDIF
      IF ( PRESENT( S ) ) THEN
!  PRINT *,'DEBUG:  CESMF_TimeSet():  S = ',S
        time%basetime%S = time%basetime%S + &
          INT( S, CESMF_KIND_I8 )
      ENDIF
      IF ( PRESENT( Sn ) .AND. ( .NOT. PRESENT( Sd ) ) ) THEN
        CALL wrf_error_fatal( &
          "CESMF_TimeSet:  Must specify Sd if Sn is specified")
      ENDIF
      IF ( PRESENT( Sd ) .AND. PRESENT( MS ) ) THEN
        CALL wrf_error_fatal( &
          "CESMF_TimeSet:  Must not specify both Sd and MS")
      ENDIF
      time%basetime%Sn = 0
      time%basetime%Sd = 0
      IF ( PRESENT( MS ) ) THEN
!  PRINT *,'DEBUG:  CESMF_TimeSet():  MS = ',MS
        time%basetime%Sn = MS
        time%basetime%Sd = 1000_CESMF_KIND_I8
      ELSE IF ( PRESENT( Sd ) ) THEN
!  PRINT *,'DEBUG:  CESMF_TimeSet():  Sd = ',Sd
        time%basetime%Sd = Sd
        IF ( PRESENT( Sn ) ) THEN
!  PRINT *,'DEBUG:  CESMF_TimeSet():  Sn = ',Sn
          time%basetime%Sn = Sn
        ENDIF
      ENDIF
      IF ( PRESENT(calendar) )THEN
!  PRINT *,'DEBUG:  CESMF_TimeSet():  using passed-in calendar'
! Note that the ugly hack of wrapping the call to CESMF_CalendarInitialized() 
! inside this #ifdef is due to lack of support for compile-time initialization 
! of components of Fortran derived types.  Some older compilers like PGI 5.1-x 
! do not support this F95 feature.  In this case we only lose a safety check.  
#ifndef NO_DT_COMPONENT_INIT
        IF ( .not. CESMF_CalendarInitialized( calendar ) )THEN
           call wrf_error_fatal( "Error:: CESMF_CalendarCreate not "// &
                                 "called on input Calendar")
        END IF
#endif
        time%Calendar => calendar
      ELSE
!  PRINT *,'DEBUG:  CESMF_TimeSet():  using default calendar'
        IF ( .not. CESMF_IsInitialized() )THEN
           call wrf_error_fatal( "Error:: CESMF_Initialize not called")
        END IF
        time%Calendar => defaultCal
      END IF

!  PRINT *,'DEBUG:  CESMF_TimeSet():  calling normalize_time()'
!$$$DEBUG
!IF ( time%basetime%Sd > 0 ) THEN
!  PRINT *,'DEBUG CESMF_TimeSet() before normalize:  S,Sn,Sd = ', &
!    time%basetime%S, time%basetime%Sn, time%basetime%Sd
!ENDIF
!$$$END DEBUG
      CALL normalize_time( time )
!$$$DEBUG
!IF ( time%basetime%Sd > 0 ) THEN
!  PRINT *,'DEBUG CESMF_TimeSet() after normalize:  S,Sn,Sd = ', &
!    time%basetime%S, time%basetime%Sn, time%basetime%Sd
!ENDIF
!$$$END DEBUG

!  PRINT *,'DEBUG:  CESMF_TimeSet():  back from normalize_time()'
      IF ( PRESENT( rc ) ) THEN
        rc = CESMF_SUCCESS
      ENDIF

      end subroutine CESMF_TimeSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMFold_TimeGetString - Get time instant value in string format

! !INTERFACE:
      subroutine CESMFold_TimeGetString( year, month, dayofmonth, &
                                        hour, minute, second, TimeString )

! !ARGUMENTS:
      integer, intent(in) :: year
      integer, intent(in) :: month
      integer, intent(in) :: dayofmonth
      integer, intent(in) :: hour
      integer, intent(in) :: minute
      integer, intent(in) :: second
      character*(*), intent(out) :: TimeString
! !DESCRIPTION:
!     Convert {\tt CESMF\_Time}'s value into ISO 8601 format YYYY-MM-DDThh:mm:ss
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to convert
!     \item[TimeString]
!          The string to return
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMG2.4.7
!EOP

!PRINT *,'DEBUG:  CESMF_TimePrint():  YR,S,Sn,Sd = ',time%YR,time%basetime%S,time%basetime%Sn,time%basetime%Sd
!PRINT *,'DEBUG:  CESMF_TimePrint():  year = ',year
!PRINT *,'DEBUG:  CESMF_TimePrint():  month, dayofmonth = ',month,dayofmonth
!PRINT *,'DEBUG:  CESMF_TimePrint():  hour = ',hour
!PRINT *,'DEBUG:  CESMF_TimePrint():  minute = ',minute
!PRINT *,'DEBUG:  CESMF_TimePrint():  second = ',second

!$$$here...  add negative sign for YR<0
!$$$here...  add Sn, Sd ??
#ifdef PLANET
      write(TimeString,FMT="(I4.4,'-',I5.5,'_',I2.2,':',I2.2,':',I2.2)") &
             year,dayofmonth,hour,minute,second
#else
      write(TimeString,FMT="(I4.4,'-',I2.2,'-',I2.2,'_',I2.2,':',I2.2,':',I2.2)") &
             year,month,dayofmonth,hour,minute,second
#endif

      end subroutine CESMFold_TimeGetString

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_TimeGetDayOfYearInteger - Get time instant's day of the year as an integer value
!
! !INTERFACE:
      subroutine CESMF_TimeGetDayOfYearInteger(time, DayOfYear, rc)
!
! !ARGUMENTS:
      type(CESMF_Time), intent(in) :: time
      integer, intent(out) :: DayOfYear
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Get the day of the year the given {\tt CESMF\_Time} instant falls on
!     (1-365).  Returned as an integer value
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The object instance to query
!     \item[DayOfYear]
!          The {\tt CESMF\_Time} instant's day of the year (1-365)
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      ! requires that time be normalized
!$$$ bug when Sn>0?  test
!$$$ add tests
      DayOfYear = ( time%basetime%S / SECONDS_PER_DAY ) + 1
      IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
      end subroutine CESMF_TimeGetDayOfYearInteger

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_TimeInc - Increment time instant with a time interval
!
! !INTERFACE:
      function CESMF_TimeInc(time, timeinterval)
!
! !RETURN VALUE:
      type(CESMF_Time) :: CESMF_TimeInc
!
! !ARGUMENTS:
      type(CESMF_Time), intent(in) :: time
      type(CESMF_TimeInterval), intent(in) :: timeinterval
! !LOCAL:
      integer   :: rc
!
! !DESCRIPTION:
!     Increment {\tt CESMF\_Time} instant with a {\tt CESMF\_TimeInterval},
!     return resulting {\tt CESMF\_Time} instant
!
!     Maps overloaded (+) operator interface function to
!     {\tt CESMF\_BaseTime} base class
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The given {\tt CESMF\_Time} to increment
!     \item[timeinterval]
!          The {\tt CESMF\_TimeInterval} to add to the given {\tt CESMF\_Time}
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP

      ! copy CESMF_Time specific properties (e.g. calendar, timezone) 
      CESMF_TimeInc = time

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeSum(time, timeinterval, CESMF_TimeInc)

      end function CESMF_TimeInc
!
! this is added for certain compilers that don't deal with commutativity
!
      function CESMF_TimeInc2(timeinterval, time)
      type(CESMF_Time) :: CESMF_TimeInc2
      type(CESMF_Time), intent(in) :: time
      type(CESMF_TimeInterval), intent(in) :: timeinterval
      CESMF_TimeInc2 = CESMF_TimeInc( time, timeinterval )
      end function CESMF_TimeInc2
!

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_TimeDec - Decrement time instant with a time interval
!
! !INTERFACE:
      function CESMF_TimeDec(time, timeinterval)
!
! !RETURN VALUE:
      type(CESMF_Time) :: CESMF_TimeDec
!
! !ARGUMENTS:
      type(CESMF_Time), intent(in) :: time
      type(CESMF_TimeInterval), intent(in) :: timeinterval
! !LOCAL:
      integer   :: rc
!
! !DESCRIPTION:
!     Decrement {\tt CESMF\_Time} instant with a {\tt CESMF\_TimeInterval},
!     return resulting {\tt CESMF\_Time} instant
!
!     Maps overloaded (-) operator interface function to
!     {\tt CESMF\_BaseTime} base class
!
!     The arguments are:
!     \begin{description}
!     \item[time]
!          The given {\tt CESMF\_Time} to decrement
!     \item[timeinterval]
!          The {\tt CESMF\_TimeInterval} to subtract from the given
!          {\tt CESMF\_Time}
!     \end{description}
!     
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP

      ! copy CESMF_Time specific properties (e.g. calendar, timezone) 
      CESMF_TimeDec = time

      ! call ESMC_BaseTime base class function
       call c_ESMC_BaseTimeDec(time, timeinterval, CESMF_TimeDec)

      end function CESMF_TimeDec

!
! this is added for certain compilers that don't deal with commutativity
!
      function CESMF_TimeDec2(timeinterval, time)
      type(CESMF_Time) :: CESMF_TimeDec2
      type(CESMF_Time), intent(in) :: time
      type(CESMF_TimeInterval), intent(in) :: timeinterval
      CESMF_TimeDec2 = CESMF_TimeDec( time, timeinterval )
      end function CESMF_TimeDec2
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_TimeDiff - Return the difference between two time instants
!
! !INTERFACE:
      function CESMF_TimeDiff(time1, time2)
!
! !RETURN VALUE:
      type(CESMF_TimeInterval) :: CESMF_TimeDiff
!
! !ARGUMENTS:
      type(CESMF_Time), intent(in) :: time1
      type(CESMF_Time), intent(in) :: time2
! !LOCAL:
      integer :: rc

! !DESCRIPTION:
!     Return the {\tt CESMF\_TimeInterval} difference between two
!     {\tt CESMF\_Time} instants
!
!     Maps overloaded (-) operator interface function to
!     {\tt CESMF\_BaseTime} base class
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          The first {\tt CESMF\_Time} instant
!     \item[time2]
!          The second {\tt CESMF\_Time} instant
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.4, TMG2.4.4, TMG2.4.5, TMG2.4.6, TMG5.1, TMG5.2, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      CALL CESMF_TimeIntervalSet( CESMF_TimeDiff, rc=rc )
      call c_ESMC_BaseTimeDiff(time1, time2, CESMF_TimeDiff)

      end function CESMF_TimeDiff

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_TimeEQ - Compare two times for equality
!
! !INTERFACE:
      function CESMF_TimeEQ(time1, time2)
!
! !RETURN VALUE:
      logical :: CESMF_TimeEQ
!
! !ARGUMENTS:
      type(CESMF_Time), intent(in) :: time1
      type(CESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if both given {\tt CESMF\_Time} instants are equal, false
!     otherwise.  Maps overloaded (==) operator interface function to
!     {\tt CESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! invoke C to C++ entry point for CESMF_BaseTime base class function
      call c_ESMC_BaseTimeEQ(time1, time2, CESMF_TimeEQ)

      end function CESMF_TimeEQ

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_TimeNE - Compare two times for non-equality
!
! !INTERFACE:
      function CESMF_TimeNE(time1, time2)
!
! !RETURN VALUE:
      logical :: CESMF_TimeNE
!
! !ARGUMENTS:
      type(CESMF_Time), intent(in) :: time1
      type(CESMF_Time), intent(in) :: time2

! !DESCRIPTION:
!     Return true if both given {\tt CESMF\_Time} instants are not equal, false
!     otherwise.  Maps overloaded (/=) operator interface function to
!     {\tt CESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeNE(time1, time2, CESMF_TimeNE)

      end function CESMF_TimeNE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_TimeLT - Time instant 1 less than time instant 2 ?
!
! !INTERFACE:
      function CESMF_TimeLT(time1, time2)
!
! !RETURN VALUE:
      logical :: CESMF_TimeLT
!
! !ARGUMENTS:
      type(CESMF_Time), intent(in) :: time1
      type(CESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if first {\tt CESMF\_Time} instant is less than second
!     {\tt CESMF\_Time} instant, false otherwise.  Maps overloaded (<)
!     operator interface function to {\tt CESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLT(time1, time2, CESMF_TimeLT)

      end function CESMF_TimeLT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_TimeGT - Time instant 1 greater than time instant 2 ?
!
! !INTERFACE:
      function CESMF_TimeGT(time1, time2)
!
! !RETURN VALUE:
      logical :: CESMF_TimeGT
!
! !ARGUMENTS:
      type(CESMF_Time), intent(in) :: time1
      type(CESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if first {\tt CESMF\_Time} instant is greater than second
!     {\tt CESMF\_Time} instant, false otherwise.  Maps overloaded (>) operator
!     interface function to {\tt CESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGT(time1, time2, CESMF_TimeGT)

      end function CESMF_TimeGT

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_TimeLE - Time instant 1 less than or equal to time instant 2 ?
!
! !INTERFACE:
      function CESMF_TimeLE(time1, time2)
!
! !RETURN VALUE:
      logical :: CESMF_TimeLE
!
! !ARGUMENTS:
      type(CESMF_Time), intent(in) :: time1
      type(CESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if first {\tt CESMF\_Time} instant is less than or equal to
!     second {\tt CESMF\_Time} instant, false otherwise.  Maps overloaded (<=)
!     operator interface function to {\tt CESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeLE(time1, time2, CESMF_TimeLE)

      end function CESMF_TimeLE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: CESMF_TimeGE - Time instant 1 greater than or equal to time instant 2 ?
!
! !INTERFACE:
      function CESMF_TimeGE(time1, time2)
!
! !RETURN VALUE:
      logical :: CESMF_TimeGE
!
! !ARGUMENTS:
      type(CESMF_Time), intent(in) :: time1
      type(CESMF_Time), intent(in) :: time2
!
! !DESCRIPTION:
!     Return true if first {\tt CESMF\_Time} instant is greater than or equal to
!     second {\tt CESMF\_Time} instant, false otherwise.  Maps overloaded (>=)
!     operator interface function to {\tt CESMF\_BaseTime} base class.
!
!     The arguments are:
!     \begin{description}
!     \item[time1]
!          First time instant to compare
!     \item[time2]
!          Second time instant to compare
!     \end{description}
!
! !REQUIREMENTS:
!     TMG1.5.3, TMG2.4.3, TMG7.2
!EOP

      ! call ESMC_BaseTime base class function
      call c_ESMC_BaseTimeGE(time1, time2, CESMF_TimeGE)

      end function CESMF_TimeGE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_TimeCopy - Copy a time-instance

! !INTERFACE:
      subroutine CESMF_TimeCopy(timeout, timein)

! !ARGUMENTS:
      type(CESMF_Time), intent(out) :: timeout
      type(CESMF_Time), intent(in) :: timein

! !DESCRIPTION:
!     Copy a time-instance to a new instance.
!
!     \item[{[rc]}]
!          Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!     TMGn.n.n
!EOP
   
      timeout%basetime = timein%basetime
      timeout%YR       = timein%YR
      timeout%Calendar => timein%Calendar

      end subroutine CESMF_TimeCopy

      end module CESMF_TimeMod

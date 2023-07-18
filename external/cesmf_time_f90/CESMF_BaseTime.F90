!
!==============================================================================
!
!     CESMF BaseTime Module
      module CESMF_BaseTimeMod
!
!==============================================================================
!
! This file contains the BaseTime class definition and all BaseTime class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES

#include <CESMF_TimeMgr.inc>
!
!===============================================================================
!BOPI
! !MODULE: CESMF_BaseTimeMod - Base CESMF time definition 
!
! !DESCRIPTION:
! Part of Time Manager F90 API wrapper of C++ implemenation
!
! This module serves only as the common Time definition inherited
! by {\tt CESMF\_TimeInterval} and {\tt CESMF\_Time}
!
! See {\tt ../include/ESMC\_BaseTime.h} for complete description
!
!------------------------------------------------------------------------------
! !USES:
      use CESMF_BaseMod    ! CESMF Base class
      implicit none
!
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! CESMF_BaseTime
!
!     ! Base class type to match C++ BaseTime class in size only;
!     !  all dereferencing within class is performed by C++ implementation

      type CESMF_BaseTime
        integer(CESMF_KIND_I8) :: S   ! whole seconds
        integer(CESMF_KIND_I8) :: Sn  ! fractional seconds, numerator
        integer(CESMF_KIND_I8) :: Sd  ! fractional seconds, denominator
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public CESMF_BaseTime
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
! overloaded operators
      public operator(+)
      private CESMF_BaseTimeSum
      public operator(-)
      private CESMF_BaseTimeDifference
      public operator(/)
      private CESMF_BaseTimeQuotI
      private CESMF_BaseTimeQuotI8
      public operator(.EQ.)
      private CESMF_BaseTimeEQ
      public operator(.NE.)
      private CESMF_BaseTimeNE
      public operator(.LT.)
      private CESMF_BaseTimeLT
      public operator(.GT.)
      private CESMF_BaseTimeGT
      public operator(.LE.)
      private CESMF_BaseTimeLE
      public operator(.GE.)
      private CESMF_BaseTimeGE

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
      interface operator(+)
        module procedure CESMF_BaseTimeSum
      end interface
      interface operator(-)
        module procedure CESMF_BaseTimeDifference
      end interface
      interface operator(/)
        module procedure CESMF_BaseTimeQuotI,CESMF_BaseTimeQuotI8
      end interface
      interface operator(.EQ.)
        module procedure CESMF_BaseTimeEQ
      end interface
      interface operator(.NE.)
        module procedure CESMF_BaseTimeNE
      end interface
      interface operator(.LT.)
        module procedure CESMF_BaseTimeLT
      end interface
      interface operator(.GT.)
        module procedure CESMF_BaseTimeGT
      end interface
      interface operator(.LE.)
        module procedure CESMF_BaseTimeLE
      end interface
      interface operator(.GE.)
        module procedure CESMF_BaseTimeGE
      end interface


!==============================================================================

      contains

!==============================================================================


! Add two basetimes
      FUNCTION CESMF_BaseTimeSum( basetime1, basetime2 )
        TYPE(CESMF_BaseTime) :: CESMF_BaseTimeSum
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime2
        ! locals
        INTEGER (CESMF_KIND_I8) :: Sn1, Sd1, Sn2, Sd2, lcd
!  PRINT *,'DEBUG:  BEGIN CESMF_BaseTimeSum()'
!  PRINT *,'DEBUG:  CESMF_BaseTimeSum():  basetime1%S = ',basetime1%S
!  PRINT *,'DEBUG:  CESMF_BaseTimeSum():  basetime1%Sn = ',basetime1%Sn
!  PRINT *,'DEBUG:  CESMF_BaseTimeSum():  basetime1%Sd = ',basetime1%Sd
!  PRINT *,'DEBUG:  CESMF_BaseTimeSum():  basetime2%S = ',basetime2%S
!  PRINT *,'DEBUG:  CESMF_BaseTimeSum():  basetime2%Sn = ',basetime2%Sn
!  PRINT *,'DEBUG:  CESMF_BaseTimeSum():  basetime2%Sd = ',basetime2%Sd
        CESMF_BaseTimeSum   = basetime1
        CESMF_BaseTimeSum%S = CESMF_BaseTimeSum%S + basetime2%S
        Sn1 = basetime1%Sn
        Sd1 = basetime1%Sd
        Sn2 = basetime2%Sn
        Sd2 = basetime2%Sd
!  PRINT *,'DEBUG:  CESMF_BaseTimeSum():  Sn1 = ',Sn1
!  PRINT *,'DEBUG:  CESMF_BaseTimeSum():  Sd1 = ',Sd1
!  PRINT *,'DEBUG:  CESMF_BaseTimeSum():  Sn2 = ',Sn2
!  PRINT *,'DEBUG:  CESMF_BaseTimeSum():  Sd2 = ',Sd2
        IF      ( ( Sd1 .EQ. 0 ) .AND. ( Sd2 .EQ. 0 ) ) THEN
!  PRINT *,'DEBUG:  CESMF_BaseTimeSum():  no fractions'
          CESMF_BaseTimeSum%Sn = 0
          CESMF_BaseTimeSum%Sd = 0
        ELSE IF ( ( Sd1 .NE. 0 ) .AND. ( Sd2 .EQ. 0 ) ) THEN
          CESMF_BaseTimeSum%Sn = Sn1
          CESMF_BaseTimeSum%Sd = Sd1
        ELSE IF ( ( Sd1 .EQ. 0 ) .AND. ( Sd2 .NE. 0 ) ) THEN
          CESMF_BaseTimeSum%Sn = Sn2
          CESMF_BaseTimeSum%Sd = Sd2
        ELSE IF ( ( Sd1 .NE. 0 ) .AND. ( Sd2 .NE. 0 ) ) THEN
          CALL compute_lcd( Sd1 , Sd2 , lcd )
          CESMF_BaseTimeSum%Sd = lcd
          CESMF_BaseTimeSum%Sn = (Sn1 * lcd / Sd1) + (Sn2 * lcd / Sd2)
        ENDIF
!  PRINT *,'DEBUG:  CESMF_BaseTimeSum():  CESMF_BaseTimeSum%S = ',CESMF_BaseTimeSum%S
!  PRINT *,'DEBUG:  CESMF_BaseTimeSum():  CESMF_BaseTimeSum%Sn = ',CESMF_BaseTimeSum%Sn
!  PRINT *,'DEBUG:  CESMF_BaseTimeSum():  CESMF_BaseTimeSum%Sd = ',CESMF_BaseTimeSum%Sd
        CALL normalize_basetime( CESMF_BaseTimeSum )
!  PRINT *,'DEBUG:  END CESMF_BaseTimeSum()'
      END FUNCTION CESMF_BaseTimeSum


! Subtract two basetimes
      FUNCTION CESMF_BaseTimeDifference( basetime1, basetime2 )
        TYPE(CESMF_BaseTime) :: CESMF_BaseTimeDifference
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime2
        ! locals
        TYPE(CESMF_BaseTime) :: neg2

        neg2%S  = -basetime2%S
        neg2%Sn = -basetime2%Sn
        neg2%Sd =  basetime2%Sd

        CESMF_BaseTimeDifference = basetime1 + neg2

      END FUNCTION CESMF_BaseTimeDifference


! Divide basetime by 8-byte integer
      FUNCTION CESMF_BaseTimeQuotI8( basetime, divisor )
        TYPE(CESMF_BaseTime) :: CESMF_BaseTimeQuotI8
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime
        INTEGER(CESMF_KIND_I8), INTENT(IN) :: divisor
        ! locals
        INTEGER(CESMF_KIND_I8) :: d, n, dinit

!PRINT *,'DEBUG CESMF_BaseTimeQuotI8() A:  S,Sn,Sd = ', &
!  basetime%S,basetime%Sn,basetime%Sd
!PRINT *,'DEBUG CESMF_BaseTimeQuotI8() A:  divisor = ', divisor
        IF ( divisor == 0_CESMF_KIND_I8 ) THEN
          CALL wrf_error_fatal( 'CESMF_BaseTimeQuotI8:  divide by zero' )
        ENDIF

!$$$ move to default constructor
        CESMF_BaseTimeQuotI8%S  = 0
        CESMF_BaseTimeQuotI8%Sn = 0
        CESMF_BaseTimeQuotI8%Sd = 0

        ! convert to a fraction and divide by multipling the denonminator by 
        ! the divisor
        IF ( basetime%Sd == 0 ) THEN
          dinit = 1_CESMF_KIND_I8
        ELSE
          dinit = basetime%Sd
        ENDIF
        n = basetime%S * dinit + basetime%Sn
        d = dinit * divisor
!PRINT *,'DEBUG CESMF_BaseTimeQuotI8() B:  n,d = ',n,d
        CALL simplify( n, d, CESMF_BaseTimeQuotI8%Sn, CESMF_BaseTimeQuotI8%Sd )
!PRINT *,'DEBUG CESMF_BaseTimeQuotI8() C:  S,Sn,Sd = ', &
!  CESMF_BaseTimeQuotI8%S,CESMF_BaseTimeQuotI8%Sn,CESMF_BaseTimeQuotI8%Sd
        CALL normalize_basetime( CESMF_BaseTimeQuotI8 )
!PRINT *,'DEBUG CESMF_BaseTimeQuotI8() D:  S,Sn,Sd = ', &
!  CESMF_BaseTimeQuotI8%S,CESMF_BaseTimeQuotI8%Sn,CESMF_BaseTimeQuotI8%Sd
      END FUNCTION CESMF_BaseTimeQuotI8

! Divide basetime by integer
      FUNCTION CESMF_BaseTimeQuotI( basetime, divisor )
        TYPE(CESMF_BaseTime) :: CESMF_BaseTimeQuotI
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime
        INTEGER, INTENT(IN) :: divisor
        IF ( divisor == 0 ) THEN
          CALL wrf_error_fatal( 'CESMF_BaseTimeQuotI:  divide by zero' )
        ENDIF
        CESMF_BaseTimeQuotI = basetime / INT( divisor, CESMF_KIND_I8 )
      END FUNCTION CESMF_BaseTimeQuotI


! .EQ. for two basetimes
      FUNCTION CESMF_BaseTimeEQ( basetime1, basetime2 )
        LOGICAL :: CESMF_BaseTimeEQ
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        CESMF_BaseTimeEQ = ( retval .EQ. 0 )
      END FUNCTION CESMF_BaseTimeEQ


! .NE. for two basetimes
      FUNCTION CESMF_BaseTimeNE( basetime1, basetime2 )
        LOGICAL :: CESMF_BaseTimeNE
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        CESMF_BaseTimeNE = ( retval .NE. 0 )
      END FUNCTION CESMF_BaseTimeNE


! .LT. for two basetimes
      FUNCTION CESMF_BaseTimeLT( basetime1, basetime2 )
        LOGICAL :: CESMF_BaseTimeLT
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        CESMF_BaseTimeLT = ( retval .LT. 0 )
      END FUNCTION CESMF_BaseTimeLT


! .GT. for two basetimes
      FUNCTION CESMF_BaseTimeGT( basetime1, basetime2 )
        LOGICAL :: CESMF_BaseTimeGT
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        CESMF_BaseTimeGT = ( retval .GT. 0 )
      END FUNCTION CESMF_BaseTimeGT


! .LE. for two basetimes
      FUNCTION CESMF_BaseTimeLE( basetime1, basetime2 )
        LOGICAL :: CESMF_BaseTimeLE
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        CESMF_BaseTimeLE = ( retval .LE. 0 )
      END FUNCTION CESMF_BaseTimeLE


! .GE. for two basetimes
      FUNCTION CESMF_BaseTimeGE( basetime1, basetime2 )
        LOGICAL :: CESMF_BaseTimeGE
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime1
        TYPE(CESMF_BaseTime), INTENT(IN) :: basetime2
        INTEGER :: retval
        CALL seccmp( basetime1%S, basetime1%Sn, basetime1%Sd, &
                     basetime2%S, basetime2%Sn, basetime2%Sd, &
                     retval )
        CESMF_BaseTimeGE = ( retval .GE. 0 )
      END FUNCTION CESMF_BaseTimeGE


      end module CESMF_BaseTimeMod

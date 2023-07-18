! Various dummy type definitions and routines for the sole purpose of 
! mimicking newer CESMF interface features without necessarily implementing 
! them.  

MODULE CESMF_Stubs

   IMPLICIT NONE

   PRIVATE

! Bogus typedefs
   TYPE CESMF_Grid
      INTEGER :: dummy
   END TYPE

   TYPE CESMF_GridComp
      INTEGER :: dummy
   END TYPE

   TYPE CESMF_State
      INTEGER :: dummy
   END TYPE

   TYPE CESMF_VM
      INTEGER :: dummy
   END TYPE

   TYPE CESMF_MsgType
      INTEGER :: mtype
   END TYPE
   TYPE(CESMF_MsgType), PARAMETER  ::      &
      CESMF_LOG_INFO  =   CESMF_MsgType(1), &
      CESMF_LOG_WARNING = CESMF_MsgType(2), &
      CESMF_LOG_ERROR =   CESMF_MsgType(3)

   TYPE CESMF_LOG
      INTEGER :: dummy
   END TYPE

   LOGICAL, private, save :: initialized = .false.

   PUBLIC CESMF_Grid, CESMF_GridComp, CESMF_State, CESMF_VM
   PUBLIC CESMF_Initialize, CESMF_Finalize, CESMF_IsInitialized
   PUBLIC CESMF_LogWrite, CESMF_LOG, CESMF_MsgType
   PUBLIC CESMF_LOG_INFO, CESMF_LOG_WARNING, CESMF_LOG_ERROR

CONTAINS


! NOOP
   SUBROUTINE CESMF_Initialize( vm, defaultcalkind, rc )
      USE esmf_basemod
      USE esmf_calendarmod
      TYPE(CESMF_VM),           INTENT(IN   ), OPTIONAL :: vm
      TYPE(CESMF_CalendarType), INTENT(IN   ), OPTIONAL :: defaultcalkind
      INTEGER,                 INTENT(  OUT), OPTIONAL :: rc

      TYPE(CESMF_CalendarType) :: defaultCalType
      INTEGER :: status

      IF ( PRESENT( rc ) ) rc = CESMF_FAILURE
      ! Initialize the default time manager calendar
      IF ( PRESENT(defaultcalkind) )THEN
         defaultCalType = defaultcalkind
      ELSE
         defaultCalType = CESMF_CAL_NOLEAP
      END IF
      allocate( defaultCal )
      defaultCal = CESMF_CalendarCreate( calendarType=defaultCalType, &
                        rc=status)

      ! initialize tables in time manager
      CALL initdaym

      IF (status .ne. CESMF_SUCCESS) THEN
          PRINT *, "Error initializing the default time manager calendar"
          RETURN
      END IF
      initialized = .true.

      IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
   END SUBROUTINE CESMF_Initialize


   FUNCTION CESMF_IsInitialized()
      LOGICAL CESMF_IsInitialized
      CESMF_IsInitialized = initialized
   END FUNCTION CESMF_IsInitialized


! NOOP
   SUBROUTINE CESMF_Finalize( rc )
      USE esmf_basemod
      INTEGER, INTENT(  OUT), OPTIONAL :: rc
#if (defined SPMD) || (defined COUP_CSM)
#include <mpif.h>
#endif
      LOGICAL :: flag
      INTEGER :: ier

      IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
#if (defined SPMD) || (defined COUP_CSM)
      CALL MPI_Finalized( flag, ier )
      IF ( ier .ne. mpi_success )THEN
        IF ( PRESENT( rc ) ) rc = CESMF_FAILURE
      END IF
      IF ( .NOT. flag ) THEN
        CALL MPI_Finalize( ier ) 
        IF ( ier .ne. mpi_success )THEN
          IF ( PRESENT( rc ) ) rc = CESMF_FAILURE
        END IF
      END IF
#endif
   END SUBROUTINE CESMF_Finalize

! NOOP
   SUBROUTINE CESMF_LogWrite( msg, MsgType, line, file, method, log, rc )
      USE esmf_basemod
      CHARACTER(LEN=*), INTENT(IN) :: msg
      TYPE(CESMF_MsgType), INTENT(IN) :: msgtype
      INTEGER, INTENT(IN), OPTIONAL :: line
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: file
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: method
      TYPE(CESMF_LOG),TARGET,OPTIONAL :: log
      INTEGER, INTENT(OUT),OPTIONAL :: rc
      IF ( PRESENT( rc ) ) rc = CESMF_SUCCESS
   END SUBROUTINE CESMF_LogWrite


END MODULE CESMF_Stubs



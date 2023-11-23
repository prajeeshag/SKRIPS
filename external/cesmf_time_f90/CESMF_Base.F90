!
! CESMF Base Module
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! module definition

      module CESMF_BaseMod
 
!BOP
! !MODULE: CESMF_BaseMod - Base class for all CESMF classes
!
! !DESCRIPTION:
!
! The code in this file implements the Base defined type
!  and functions which operate on all types.  This is an
!  interface to the actual C++ base class implementation in the ../src dir.
!
! See the CESMF Developers Guide document for more details.
!
!------------------------------------------------------------------------------

! !USES:
      implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
!    Global integer parameters, used frequently

      integer, parameter :: CESMF_SUCCESS = 0, CESMF_FAILURE = -1
      integer, parameter :: CESMF_MAXSTR = 128
      integer, parameter :: CESMF_MAXDIM = 7, &
                            CESMF_MAXDECOMPDIM=3, &
                            CESMF_MAXGRIDDIM=2
     
      integer, parameter :: CESMF_MAJOR_VERSION = 2
      integer, parameter :: CESMF_MINOR_VERSION = 1
      integer, parameter :: CESMF_REVISION      = 1
      integer, parameter :: CESMF_PATCHLEVEL    = 0
      character(32), parameter :: CESMF_VERSION_STRING = "2.1.1"

!------------------------------------------------------------------------------
!
      type CESMF_Status
      private
          integer :: status
      end type

      type(CESMF_Status), parameter :: CESMF_STATE_UNINIT = CESMF_Status(1), &
                                      CESMF_STATE_READY = CESMF_Status(2), &
                                      CESMF_STATE_UNALLOCATED = CESMF_Status(3), &
                                      CESMF_STATE_ALLOCATED = CESMF_Status(4), &
                                      CESMF_STATE_BUSY = CESMF_Status(5), &
                                      CESMF_STATE_INVALID = CESMF_Status(6)
 
!------------------------------------------------------------------------------
!
      type CESMF_Pointer
      private
          integer*8 :: ptr
      end type

      type(CESMF_Pointer), parameter :: CESMF_NULL_POINTER = CESMF_Pointer(0), &
                                       CESMF_BAD_POINTER = CESMF_Pointer(-1)


!------------------------------------------------------------------------------
!
      !! TODO: I believe if we define an assignment(=) operator to convert
      !!   a datatype into integer, then we could use the type and kind as
      !!   targets in a select case() statement and make the contents private.
      !!   (see pg 248 of the "big book")
      type CESMF_DataType
      !!private
          integer :: dtype
      end type

      type(CESMF_DataType), parameter :: CESMF_DATA_INTEGER = CESMF_DataType(1), &
                                        CESMF_DATA_REAL = CESMF_DataType(2), &
                                        CESMF_DATA_LOGICAL = CESMF_DataType(3), &
                                        CESMF_DATA_CHARACTER = CESMF_DataType(4)

!------------------------------------------------------------------------------

      integer, parameter :: &
                   CESMF_KIND_I1 = selected_int_kind(2), &
                   CESMF_KIND_I2 = selected_int_kind(4), &
                   CESMF_KIND_I4 = selected_int_kind(9), &
                   CESMF_KIND_I8 = selected_int_kind(14), &
                   CESMF_KIND_R4 = selected_real_kind(3,25), &
                   CESMF_KIND_R8 = selected_real_kind(6,45), &
                   CESMF_KIND_C8 = selected_real_kind(3,25), &
                   CESMF_KIND_C16 = selected_real_kind(6,45)

!------------------------------------------------------------------------------

      type CESMF_DataValue
      private
          type(CESMF_DataType) :: dt
          integer :: rank
          ! how do you do values of all types here ? TODO
          ! in C++ i'd do a union w/ overloaded access funcs
          integer :: vi
          !integer, dimension (:), pointer :: vip
          !real :: vr
          !real, dimension (:), pointer :: vrp
          !logical :: vl
          !logical, pointer :: vlp
          !character (len=CESMF_MAXSTR) :: vc
          !character, pointer :: vcp
      end type

!------------------------------------------------------------------------------
!
      type CESMF_Attribute
      private
          character (len=CESMF_MAXSTR) :: attr_name
          type (CESMF_DataType) :: attr_type
          type (CESMF_DataValue) :: attr_value
      end type

!------------------------------------------------------------------------------
!
      !! TODO: this should be a shallow object, with a simple init() and
      !!  get() function, and the contents should go back to being private.
      type CESMF_AxisIndex
!     !!private
          integer :: l
          integer :: r
          integer :: max
          integer :: decomp
          integer :: gstart
      end type

      !! TODO: same comment as above.
      type CESMF_MemIndex
!     !!private
          integer :: l
          integer :: r
          integer :: str
          integer :: num
      end type

!------------------------------------------------------------------------------
!
      type CESMF_BasePointer
      private
          integer*8 :: base_ptr
      end type

      integer :: global_count = 0

!------------------------------------------------------------------------------
!
!     ! WARNING: must match corresponding values in ../include/ESMC_Base.h
      type CESMF_Logical
      private
          integer :: value
      end type

      type(CESMF_Logical), parameter :: CESMF_TF_UNKNOWN  = CESMF_Logical(1), &
                                       CESMF_TF_TRUE     = CESMF_Logical(2), &
                                       CESMF_TF_FALSE    = CESMF_Logical(3)

!------------------------------------------------------------------------------
!
      type CESMF_Base
      private
         integer :: ID
         integer :: ref_count
         type (CESMF_Status) :: base_status
         character (len=CESMF_MAXSTR) :: name
     end type

! !PUBLIC TYPES:

      public CESMF_STATE_INVALID
!      public CESMF_STATE_UNINIT, CESMF_STATE_READY, &
!             CESMF_STATE_UNALLOCATED, CESMF_STATE_ALLOCATED, &
!             CESMF_STATE_BUSY

      public CESMF_DATA_INTEGER, CESMF_DATA_REAL, &
             CESMF_DATA_LOGICAL, CESMF_DATA_CHARACTER

      public CESMF_KIND_I1, CESMF_KIND_I2, CESMF_KIND_I4, CESMF_KIND_I8, & 
             CESMF_KIND_R4, CESMF_KIND_R8, CESMF_KIND_C8, CESMF_KIND_C16

      public CESMF_NULL_POINTER, CESMF_BAD_POINTER


      public CESMF_FAILURE, CESMF_SUCCESS
      public CESMF_MAXSTR
      public CESMF_MAXDIM, CESMF_MAXDECOMPDIM, CESMF_MAXGRIDDIM
     
      public CESMF_MAJOR_VERSION, CESMF_MINOR_VERSION, CESMF_REVISION
      public CESMF_VERSION_STRING 

      public CESMF_Status, CESMF_Pointer, CESMF_DataType
      public CESMF_DataValue, CESMF_Attribute
!      public CESMF_MemIndex
!      public CESMF_BasePointer
      public CESMF_Base

      public CESMF_AxisIndex, CESMF_AxisIndexGet
!      public CESMF_AxisIndexInit
      public CESMF_Logical
!      public CESMF_TF_TRUE, CESMF_TF_FALSE

! !PUBLIC MEMBER FUNCTIONS:
!
! !DESCRIPTION:
!     The following routines apply to any type in the system.  
!     The attribute routines can be inherited as-is.  The other
!     routines need to be specialized by the higher level objects.
!
!   Base class methods
!      public CESMF_BaseInit
   
!      public CESMF_BaseGetConfig
!      public CESMF_BaseSetConfig

!      public CESMF_BaseGetInstCount

!      public CESMF_BaseSetID
!      public CESMF_BaseGetID

!      public CESMF_BaseSetRefCount
!      public CESMF_BaseGetRefCount

!      public CESMF_BaseSetStatus
!      public CESMF_BaseGetStatus

!   Virtual methods to be defined by derived classes
!      public CESMF_Read
!      public CESMF_Write
!      public CESMF_Validate
!      public CESMF_Print

!  Attribute methods
      public CESMF_AttributeSet
      public CESMF_AttributeGet
      public CESMF_AttributeGetCount
      public CESMF_AttributeGetbyNumber
      public CESMF_AttributeGetNameList
      public CESMF_AttributeSetList
      public CESMF_AttributeGetList
      public CESMF_AttributeSetObjectList
      public CESMF_AttributeGetObjectList
      public CESMF_AttributeCopy
      public CESMF_AttributeCopyAll
 
!  Misc methods
      public CESMF_SetName
      public CESMF_GetName
      public CESMF_SetPointer
      public CESMF_SetNullPointer
      public CESMF_GetPointer

!  Print methods for calling by higher level print functions
!  (they have little formatting other than the actual values)
      public CESMF_StatusString, CESMF_DataTypeString

!  Overloaded = operator functions
      public operator(.eq.), operator(.ne.), assignment(=)
!
!
!EOP

!------------------------------------------------------------------------------

! overload .eq. & .ne. with additional derived types so you can compare 
!  them as if they were simple integers.
 

interface operator (.eq.)
 module procedure CESMF_sfeq
 module procedure CESMF_dteq
 module procedure CESMF_pteq
 module procedure CESMF_tfeq
 module procedure CESMF_aieq
end interface

interface operator (.ne.)
 module procedure CESMF_sfne
 module procedure CESMF_dtne
 module procedure CESMF_ptne
 module procedure CESMF_tfne
 module procedure CESMF_aine
end interface

interface assignment (=)
 module procedure CESMF_dtas
 module procedure CESMF_ptas
end interface

!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
! function to compare two CESMF_Status flags to see if they're the same or not

function CESMF_sfeq(sf1, sf2)
 logical CESMF_sfeq
 type(CESMF_Status), intent(in) :: sf1, sf2

 CESMF_sfeq = (sf1%status .eq. sf2%status)
end function

function CESMF_sfne(sf1, sf2)
 logical CESMF_sfne
 type(CESMF_Status), intent(in) :: sf1, sf2

 CESMF_sfne = (sf1%status .ne. sf2%status)
end function

!------------------------------------------------------------------------------
! function to compare two CESMF_DataTypes to see if they're the same or not

function CESMF_dteq(dt1, dt2)
 logical CESMF_dteq
 type(CESMF_DataType), intent(in) :: dt1, dt2

 CESMF_dteq = (dt1%dtype .eq. dt2%dtype)
end function

function CESMF_dtne(dt1, dt2)
 logical CESMF_dtne
 type(CESMF_DataType), intent(in) :: dt1, dt2

 CESMF_dtne = (dt1%dtype .ne. dt2%dtype)
end function

subroutine CESMF_dtas(intval, dtval)
 integer, intent(out) :: intval
 type(CESMF_DataType), intent(in) :: dtval

 intval = dtval%dtype
end subroutine


!------------------------------------------------------------------------------
! function to compare two CESMF_Pointers to see if they're the same or not

function CESMF_pteq(pt1, pt2)
 logical CESMF_pteq
 type(CESMF_Pointer), intent(in) :: pt1, pt2

 CESMF_pteq = (pt1%ptr .eq. pt2%ptr)
end function

function CESMF_ptne(pt1, pt2)
 logical CESMF_ptne
 type(CESMF_Pointer), intent(in) :: pt1, pt2

 CESMF_ptne = (pt1%ptr .ne. pt2%ptr)
end function

subroutine CESMF_ptas(ptval, intval)
 type(CESMF_Pointer), intent(out) :: ptval
 integer, intent(in) :: intval

 ptval%ptr = intval
end subroutine

!------------------------------------------------------------------------------
! function to compare two CESMF_Logicals to see if they're the same or not
! also need assignment to real f90 logical?

function CESMF_tfeq(tf1, tf2)
 logical CESMF_tfeq
 type(CESMF_Logical), intent(in) :: tf1, tf2

 CESMF_tfeq = (tf1%value .eq. tf2%value)
end function

function CESMF_tfne(tf1, tf2)
 logical CESMF_tfne
 type(CESMF_Logical), intent(in) :: tf1, tf2

 CESMF_tfne = (tf1%value .ne. tf2%value)
end function

!------------------------------------------------------------------------------
! function to compare two CESMF_AxisIndex to see if they're the same or not

function CESMF_aieq(ai1, ai2)
 logical CESMF_aieq
 type(CESMF_AxisIndex), intent(in) :: ai1, ai2

 CESMF_aieq = ((ai1%l .eq. ai2%l) .and. &
              (ai1%r .eq. ai2%r) .and. &
              (ai1%max .eq. ai2%max) .and. &
              (ai1%decomp .eq. ai2%decomp) .and. &
              (ai1%gstart .eq. ai2%gstart))

end function

function CESMF_aine(ai1, ai2)
 logical CESMF_aine
 type(CESMF_AxisIndex), intent(in) :: ai1, ai2

 CESMF_aine = ((ai1%l .ne. ai2%l) .or. &
              (ai1%r .ne. ai2%r) .or. &
              (ai1%max .ne. ai2%max) .or. &
              (ai1%decomp .ne. ai2%decomp) .or. &
              (ai1%gstart .ne. ai2%gstart))

end function

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Base methods
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_BaseInit - initialize a Base object
!
! !INTERFACE:
      subroutine CESMF_BaseInit(base, rc)
!
! !ARGUMENTS:
      type(CESMF_Base) :: base                 
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Set initial state on a Base object.
!
!     \begin{description}
!     \item [base]
!           In the Fortran interface, this must in fact be a {\tt Base}
!           derived type object.  It is expected that all specialized 
!           derived types will include a {\tt Base} object as the first
!           entry.
!     \item [{[rc]}]
!           Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP

      logical :: rcpresent                          ! Return code present   

!     !Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = CESMF_FAILURE
      endif

      global_count = global_count + 1
      base%ID = global_count
      base%ref_count = 1
      base%base_status = CESMF_STATE_READY
      base%name = "undefined"

      if (rcpresent) rc = CESMF_SUCCESS

      end subroutine CESMF_BaseInit

!------------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_SetName - set the name of this object
!
! !INTERFACE:
      subroutine CESMF_SetName(anytype, name, namespace, rc)
!
! !ARGUMENTS:
      type(CESMF_Base) :: anytype                 
      character (len = *), intent(in), optional :: name   
      character (len = *), intent(in), optional :: namespace
      integer, intent(out), optional :: rc     

!
! !DESCRIPTION:
!     Associate a name with any object in the system.
!
!     \begin{description}
!     \item [anytype]
!           In the Fortran interface, this must in fact be a {\tt Base}
!           derived type object.  It is expected that all specialized 
!           derived types will include a {\tt Base} object as the first
!           entry.
!     \item [[name]]
!           Object name.  An error will be returned if a duplicate name 
!           is specified.  If a name is not given a unique name will be
!           generated and can be queried by the {\tt CESMF_GetName} routine.
!     \item [[namespace]]
!           Object namespace (e.g. "Application", "Component", "Grid", etc).
!           If given, the name will be checked that it is unique within
!           this namespace.  If not given, the generated name will be 
!           unique within this namespace.  If namespace is not specified,
!           a default "global" namespace will be assumed and the same rules
!           for names will be followed.
!     \item [[rc]]
!           Return code; equals {\tt CESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
! 

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3
      logical :: rcpresent                          ! Return code present   
      character (len = CESMF_MAXSTR) :: ournamespace ! Namespace if not given
      character (len = CESMF_MAXSTR) :: defaultname  ! Name if not given
      integer, save :: seqnum = 0       ! HACK - generate uniq names
                                        ! but not coordinated across procs

!     !Initialize return code
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = CESMF_FAILURE
      endif

!     ! TODO: this code should generate a unique name if a name
!     !   is not given.  If a namespace is given, the name has to
!     !   be unique within that namespace.  Example namespaces could
!     !   be: Applications, Components, Fields/Bundles, Grids.
!      
!     ! Construct a default namespace if one is not given
      if( present(namespace) ) then
          if( namespace .eq. "" ) then
              ournamespace = "global"
          else
              ournamespace = namespace
          endif
      else
              ournamespace = "global"
      endif

!     ! Construct a default name if one is not given
      if( present(name) ) then
         if( name .eq. "" ) then
            write(defaultname, 20) trim(ournamespace), seqnum
20          format(A,I3.3)
            seqnum = seqnum + 1
            anytype%name = defaultname
         else
            anytype%name = name
         endif
      else
         write(defaultname, 20) trim(ournamespace), seqnum
         seqnum = seqnum + 1
         anytype%name = defaultname
      endif

      if (rcpresent) rc = CESMF_SUCCESS

      end subroutine CESMF_SetName

!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_GetName - get the name of this object
!
! !INTERFACE:
      subroutine CESMF_GetName(anytype, name, rc)
!
! !ARGUMENTS:
      type(CESMF_Base), intent(in) :: anytype             ! any CESMF object/type
      character (len = *), intent(out) :: name           ! object/type name
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Return the name of any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3

      name = anytype%name
      if (present(rc)) rc = CESMF_SUCCESS

      end subroutine CESMF_GetName


!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AttributeSet - set attribute on an CESMF type
!
! !INTERFACE:
      subroutine CESMF_AttributeSet(anytype, name, value, rc)
!
! !ARGUMENTS:
      type(CESMF_Base), intent(in) :: anytype             ! any CESMF type
      character (len = *), intent(in) :: name            ! attribute name
      type(CESMF_DataValue), intent(in) :: value              ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
!     Associate a (name,value) pair with any type in the system.

!
!EOP
! !REQUIREMENTS:  FLD1.5, FLD1.5.3

      end subroutine CESMF_AttributeSet


!-------------------------------------------------------------------------
!BOP
! !IROUTINE:  CESMF_AttributeGet - get attribute from an CESMF type
!
! !INTERFACE:
      subroutine CESMF_AttributeGet(anytype, name, type, value, rc)
!
! !ARGUMENTS:
      type(CESMF_Base), intent(in) :: anytype           ! any CESMF type
      character (len = *), intent(in) :: name          ! attribute name
      type(CESMF_DataType), intent(out) :: type             ! all possible data types
      type(CESMF_DataValue), intent(out) :: value           ! attribute value
      integer, intent(out), optional :: rc             ! return code

!
! !DESCRIPTION:

!
!EOP
! !REQUIREMENTS:  FLD1.5.1, FLD1.5.3

      end subroutine CESMF_AttributeGet


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CESMF_AttributeGetCount - get an CESMF object's number of attributes
!
! !INTERFACE:
      subroutine CESMF_AttributeGetCount(anytype, count, rc)
!
! !ARGUMENTS:
      type(CESMF_Base), intent(in) :: anytype             ! any CESMF type
      integer, intent(out) :: count                      ! attribute count
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Returns number of attributes present.

!
!EOP
! !REQUIREMENTS:  FLD1.7.5

      end subroutine CESMF_AttributeGetCount


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CESMF_AttributeGetbyNumber - get an CESMF object's attribute by num ber
!
! !INTERFACE:
      subroutine CESMF_AttributeGetbyNumber(anytype, number, name, type, value, rc)
!
! !ARGUMENTS:
      type(CESMF_Base), intent(in) :: anytype             ! any CESMF type
      integer, intent(in) :: number                      ! attribute number
      character (len = *), intent(in) :: name            ! attribute name
      type(CESMF_DataType), intent(out) :: type               ! all possible data types
      type(CESMF_DataValue), intent(out) :: value             ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Allows the caller to get attributes by number instead of by name.
! This can be useful in iterating through all attributes in a loop.
!
!EOP
! !REQUIREMENTS: 

      end subroutine CESMF_AttributeGetbyNumber


!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  CESMF_AttributeGetNameList - get an CESMF object's attribute name list
!
! !INTERFACE:
      subroutine CESMF_AttributeGetNameList(anytype, count, namelist, rc)
!
! !ARGUMENTS:
      type(CESMF_Base), intent(in) :: anytype             ! any CESMF type
      integer, intent(out) :: count                      ! attribute count
      character (len = *), dimension (:), intent(out) :: namelist   ! attribute names
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Return a list of all attribute names without returning the values.

!
!EOP
! !REQUIREMENTS:  FLD1.7.3

      end subroutine CESMF_AttributeGetNameList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CESMF_AttributeSetList - set an CESMF object's attributes 
!
! !INTERFACE:
      subroutine CESMF_AttributeSetList(anytype, namelist, valuelist, rc)

!
! !ARGUMENTS:
      type(CESMF_Base), intent(in) :: anytype             ! any CESMF type
      character (len = *), dimension (:), intent(in) :: namelist    ! attribute names
      type(CESMF_DataValue), dimension (:), intent(in) :: valuelist      ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Set multiple attributes on an object in one call.  Depending on what is
! allowed by the interface, all attributes may have to have the same type.
!
!EOP
! !REQUIREMENTS:  (none.  added for completeness)

      end subroutine CESMF_AttributeSetList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CESMF_AttributeGetList - get an CESMF object's attributes
!
! !INTERFACE:
      subroutine CESMF_AttributeGetList(anytype, namelist, typelist, valuelist, rc)
!
! !ARGUMENTS:
      type(CESMF_Base), intent(in) :: anytype             ! any CESMF type
      character (len = *), dimension (:), intent(in) :: namelist    ! attribute names
      type(CESMF_DataType), dimension (:), intent(out) :: typelist       ! all possible data types
      type(CESMF_DataValue), dimension (:), intent(out) :: valuelist     ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Get multiple attributes from an object in a single call.

!
!EOP
! !REQUIREMENTS:  FLD1.7.4

      end subroutine CESMF_AttributeGetList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CESMF_AttributeSetObjectList - set an attribute on multiple CESMF objects 
!
! !INTERFACE:
      subroutine CESMF_AttributeSetObjectList(anytypelist, name, value, rc)
!
! !ARGUMENTS:
      type(CESMF_Base), dimension (:), intent(in) :: anytypelist     ! list of any CESMF types
      character (len = *), intent(in) :: name            ! attribute name
      type(CESMF_DataValue), dimension (:), intent(in) :: value          ! attribute value
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Set the same attribute on multiple objects in one call.

!
!EOP
! !REQUIREMENTS:  FLD1.5.5 (pri 2)

      end subroutine CESMF_AttributeSetObjectList


!-------------------------------------------------------------------------
!BOP
!
!
! !IROUTINE:  CESMF_AttributeGetObjectList - get an attribute from multiple CESMF objects 
!
! !INTERFACE:
      subroutine CESMF_AttributeGetObjectList(anytypelist, name, typelist, valuelist, rc)
!
! !ARGUMENTS:
      type(CESMF_Base), dimension (:), intent(in) :: anytypelist     ! list of any CESMF types
      character (len = *), intent(in) :: name            ! attribute name
      type(CESMF_DataType), dimension (:), intent(out) :: typelist       ! all possible data types
      type(CESMF_DataValue), dimension (:), intent(out) :: valuelist     ! attribute values
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! Get the same attribute name from multiple objects in one call.

!
!EOP
! !REQUIREMENTS:  FLD1.5.5 (pri 2)

      end subroutine CESMF_AttributeGetObjectList


!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  CESMF_AttributeCopy - copy an attribute between two objects
!
! !INTERFACE:
      subroutine CESMF_AttributeCopy(name, source, destination, rc)
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name            ! attribute name
      type(CESMF_Base), intent(in) :: source              ! any CESMF type
      type(CESMF_Base), intent(in) :: destination         ! any CESMF type
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! The specified attribute associated with the source object is
! copied to the destination object.  << does this assume overwriting the
! attribute if it already exists in the output or does this require yet
! another arg to say what to do with collisions? >>


!
!EOP
! !REQUIREMENTS:  FLD1.5.4

      end subroutine CESMF_AttributeCopy


!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMC_AttributeCopyAll - copy attributes between two objects

!
! !INTERFACE:
      subroutine CESMF_AttributeCopyAll(source, destination, rc)
!
! !ARGUMENTS:
      type(CESMF_Base), intent(in) :: source              ! any CESMF type
      type(CESMF_Base), intent(in) :: destination         ! any CESMF type
      integer, intent(out), optional :: rc               ! return code

!
! !DESCRIPTION:
! All attributes associated with the source object are copied to the
! destination object.  Some attributes will have to be considered
! {\tt read only} and won't be updated by this call.  (e.g. an attribute
! like {\tt name} must be unique and therefore can't be duplicated.)

!
!EOP
! !REQUIREMENTS:  FLD1.5.4

      end subroutine CESMF_AttributeCopyAll

!=========================================================================
! Misc utility routines, perhaps belongs in a utility file?
!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  ESMC_AxisIndexInit - initialize an AxisIndex object

!
! !INTERFACE:
      subroutine CESMF_AxisIndexInit(ai, l, r, max, decomp, gstart, rc)
!
! !ARGUMENTS:
      type(CESMF_AxisIndex), intent(inout) :: ai
      integer, intent(in) :: l, r, max, decomp, gstart
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Set the contents of an AxisIndex type.

!
!EOP
! !REQUIREMENTS:

      ai%l = l
      ai%r = r
      ai%max = max
      ai%decomp = decomp
      ai%gstart = gstart

      if (present(rc)) rc = CESMF_SUCCESS

      end subroutine CESMF_AxisIndexInit

!BOP
!
!IROUTINE:  ESMC_AxisIndexInit - initialize an AxisIndex object

!
! !INTERFACE:
      subroutine CESMF_AxisIndexGet(ai, l, r, max, decomp, gstart, rc)
!
! !ARGUMENTS:
      type(CESMF_AxisIndex), intent(inout) :: ai
      integer, intent(out), optional :: l, r, max, decomp, gstart
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
!   Get the contents of an AxisIndex type.

!
!EOP
! !REQUIREMENTS:

      if (present(l)) l = ai%l
      if (present(r)) r = ai%r
      if (present(max)) max = ai%max
      if (present(decomp)) decomp = ai%decomp
      if (present(gstart)) gstart = ai%gstart

      if (present(rc)) rc = CESMF_SUCCESS

      end subroutine CESMF_AxisIndexGet

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  CESMF_SetPointer - set an opaque value

!
! !INTERFACE:
      subroutine CESMF_SetPointer(ptype, contents, rc)
!
! !ARGUMENTS:
      type(CESMF_Pointer) :: ptype 
      integer*8, intent(in) :: contents
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      ptype%ptr = contents
      if (present(rc)) rc = CESMF_SUCCESS

      end subroutine CESMF_SetPointer

!-------------------------------------------------------------------------
!BOP
!
!IROUTINE:  CESMF_SetNullPointer - set an opaque value

!
! !INTERFACE:
      subroutine CESMF_SetNullPointer(ptype, rc)
!
! !ARGUMENTS:
      type(CESMF_Pointer) :: ptype 
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Set the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      integer*8, parameter :: nullp = 0

      ptype%ptr = nullp
      if (present(rc)) rc = CESMF_SUCCESS

      end subroutine CESMF_SetNullPointer
!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  CESMF_GetPointer - get an opaque value 
!  
! !INTERFACE: 
      function CESMF_GetPointer(ptype, rc) 
!
! !RETURN VALUE:
      integer*8 :: CESMF_GetPointer

! !ARGUMENTS:
      type(CESMF_Pointer), intent(in) :: ptype 
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Get the contents of an opaque pointer type.

!
!EOP
! !REQUIREMENTS:
      CESMF_GetPointer = ptype%ptr
      if (present(rc)) rc = CESMF_SUCCESS

      end function CESMF_GetPointer

!------------------------------------------------------------------------- 
! misc print routines
!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  CESMF_StatusString - Return status as a string
!  
! !INTERFACE: 
      subroutine CESMF_StatusString(status, string, rc)
!
! !ARGUMENTS:
      type(CESMF_Status), intent(in) :: status
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a status variable as a string.

!
!EOP
! !REQUIREMENTS:

      if (status .eq. CESMF_STATE_UNINIT) string = "Uninitialized"
      if (status .eq. CESMF_STATE_READY) string = "Ready"
      if (status .eq. CESMF_STATE_UNALLOCATED) string = "Unallocated"
      if (status .eq. CESMF_STATE_ALLOCATED) string = "Allocated"
      if (status .eq. CESMF_STATE_BUSY) string = "Busy"
      if (status .eq. CESMF_STATE_INVALID) string = "Invalid"
 
      if (present(rc)) rc = CESMF_SUCCESS

      end subroutine CESMF_StatusString

!------------------------------------------------------------------------- 
!BOP 
!  !IROUTINE:  CESMF_DataTypeString - Return DataType as a string
!  
! !INTERFACE: 
      subroutine CESMF_DataTypeString(datatype, string, rc)
!
! !ARGUMENTS:
      type(CESMF_DataType), intent(in) :: datatype
      character(len=*), intent(out) :: string
      integer, intent(out), optional :: rc  

!
! !DESCRIPTION:
!   Return a datatype variable as a string.

!
!EOP
! !REQUIREMENTS:

      if (datatype .eq. CESMF_DATA_INTEGER) string = "Integer"
      if (datatype .eq. CESMF_DATA_REAL) string = "Real"
      if (datatype .eq. CESMF_DATA_LOGICAL) string = "Logical"
      if (datatype .eq. CESMF_DATA_CHARACTER) string = "Character"
 
      if (present(rc)) rc = CESMF_SUCCESS

      end subroutine CESMF_DataTypeString

!-------------------------------------------------------------------------
!
!-------------------------------------------------------------------------
! put Print and Validate skeletons here - but they should be
!  overridden by higher level more specialized functions.
!-------------------------------------------------------------------------

      end module CESMF_BaseMod

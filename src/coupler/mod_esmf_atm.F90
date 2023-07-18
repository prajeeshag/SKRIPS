!-----------------------------------------------------------------------
! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!-----------------------------------------------------------------------
!     ATM gridded component code
!-----------------------------------------------------------------------
!
#include "esmf_macros.inc"
module mod_esmf_atm
!
!-----------------------------------------------------------------------
!     Used module declarations
!-----------------------------------------------------------------------
!
  use ESMF
  use NUOPC
  use NUOPC_Model, &
    NUOPC_SetServices => SetServices, &
    NUOPC_Label_SetClock => label_SetClock, &
    NUOPC_Label_Advance => label_Advance, &
    NUOPC_Label_DataInitialize => label_DataInitialize
!
  use ieee_arithmetic, only: ieee_is_nan
!
  use mod_types
  use module_wrf_top, only: wrf_init, wrf_run, wrf_finalize
  use module_domain, only: head_grid, get_ijk_from_grid
  use module_utility, only: WRFU_TIME, WRFU_TIMESET, WRFU_TIMEGET

  implicit none
  private

  integer :: ims, ime, jms, jme, kms, kme
  integer :: ids, ide, jds, jde, kds, kde
  integer :: ips, ipe, jps, jpe, kps, kpe

  integer :: NX, NY  ! Global NX, NY

  integer, allocatable :: ims_global(:)
  integer, allocatable :: jms_global(:)
  integer, allocatable :: ime_global(:)
  integer, allocatable :: jme_global(:)

  logical :: atm_get_first_call = .true.
  logical :: atm_put_first_call = .true.

  public :: ATM_SetServices

  
  interface assignment (=)
    module procedure ESMF_Time_to_WRF_Time
  end interface


contains

  subroutine ATM_SetServices(gcomp, rc)
    !
    !-------------------------------------------------------------------
    ! Imported variable declarations
    !-------------------------------------------------------------------
    !
    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    call NUOPC_CompDerive(gcomp, NUOPC_SetServices, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
                                 phaseLabelList=(/"IPDv00p1"/), &
                                 userRoutine=ATM_Init1, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
                                 phaseLabelList=(/"IPDv00p2"/), &
                                 userRoutine=ATM_Init2, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_SetClock, &
                              specRoutine=ATM_SetClock, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    call NUOPC_CompSpecialize(gcomp, specLabel=NUOPC_Label_Advance, &
                              specRoutine=ATM_Run, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    call ESMF_GridCompSetEntryPoint(gcomp, &
                                    methodflag=ESMF_METHOD_FINALIZE, &
                                    userRoutine=ATM_Final, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

  end subroutine ATM_SetServices


  subroutine ATM_Init1(gcomp, importState, exportState, clock, rc)
    !
    !-----------------------------------------------------------------------
    !     Initialization phase 1, set import/export fields
    !-----------------------------------------------------------------------
    !

    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    character(ESMF_MAXSTR) :: entryNameNUOPC, entryNameWRF
    integer :: iEntry
    logical :: exportEntry

    rc = ESMF_SUCCESS

    ! register the MITgcm entries in ESMF
    do iEntry = 1, nList
      entryNameNUOPC = trim(nuopc_entryNameList(iEntry)); 
      entryNameWRF = trim(wrf_nameList(iEntry)); 
      exportEntry = ATMtoOCN(iEntry); 
      if (exportEntry == .True.) then
        Call NUOPC_Advertise(exportState, &
                             StandardName=entryNameNUOPC, name=entryNameWRF, rc=rc)
        _ERR_CHK(__FILE__,__LINE__)
      else
        Call NUOPC_Advertise(importState, &
                             StandardName=entryNameNUOPC, name=entryNameWRF, rc=rc)
        _ERR_CHK(__FILE__,__LINE__)
      end if
    end do

  end subroutine ATM_Init1
!
!-----------------------------------------------------------------------
!     Initialization phase 2
!-----------------------------------------------------------------------
!
  subroutine ATM_Init2(gcomp, importState, exportState, clock, rc)

    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    integer :: myThid = 1
    integer :: comm, localPet, petCount
    character(ESMF_MAXSTR) :: gname
    type(ESMF_Field) :: field
    type(ESMF_Grid) :: atmGridIn
    type(ESMF_Grid) :: atmGridOut
    type(ESMF_VM) :: vm
    type(ESMF_Time) :: currTime
    type(ESMF_Time) :: nextTime
    integer :: yy, mm, dd, h, m, s

    rc = ESMF_SUCCESS
    !
    !-------------------------------------------------------------------
    ! Get gridded component
    !-------------------------------------------------------------------
    !
    call ESMF_GridCompGet(gcomp, name=gname, vm=vm, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)


    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, &
                    mpiCommunicator=comm, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)
    !
    !-------------------------------------------------------------------
    ! Initialize the gridded component
    !-------------------------------------------------------------------
    !
    call wrf_set_dm_communicator(comm)
    call wrf_init(no_init1=.true.)
    call get_ijk_from_grid( &
      head_grid ,                   &
      ids, ide, jds, jde, kds, kde,    &
      ims, ime, jms, jme, kms, kme,    &
      ips, ipe, jps, jpe, kps, kpe &
    )
    
    NX = head_grid%e_we - head_grid%s_we + 1
    NY = head_grid%e_sn - head_grid%s_sn + 1
    !
    !-------------------------------------------------------------------
    ! Set-up grid and load coordinate data
    !-------------------------------------------------------------------
    !
    call ATM_SetGridArrays(gcomp, petCount, localPet, atmGridIn, rc)
    _ERR_CHK(__FILE__,__LINE__)
    atmGridOut = atmGridIn

    call ATM_SetStates(gcomp, atmGridIn, atmGridOut, rc)

    !! run an empty step to fill the data
    CALL ESMF_ClockGet( clock, currTime=currTime, rc=rc )
    _ERR_CHK(__FILE__,__LINE__)

    head_grid%start_subtime = currTime
    head_grid%stop_subtime = currTime
  
    ! run an empty step (for initialization)
    call wrf_run();

  end subroutine ATM_Init2


  subroutine ESMF_Time_to_WRF_Time(esmfTime, wrfTime)
    type(ESMF_Time), intent(out) :: esmfTime 
    type(WRFU_Time), intent(in) :: wrfTime
    
    integer :: yy, mm, dd, h, m, s
    integer :: rc

    call ESMF_TimeGet(currTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)
    call WRFU_TimeSet(wrfTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
  end subroutine ESMF_Time_to_WRF_Time 


  subroutine ATM_SetClock(gcomp, rc)
    !
    !-----------------------------------------------------------------------
    !     Atm Set Clock
    !-----------------------------------------------------------------------
    !

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    type(ESMF_Clock)     :: clock
    type(ESMF_VM) :: vm

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(gcomp, modelClock=clock, rc=rc)

    call NUOPC_CompSetClock(gcomp, clock, atmTimeStep, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    ! Run 0 time step to initialize data (maybe not useful)
    call ATM_Put(gcomp, 0, rc)
    _ERR_CHK(__FILE__,__LINE__)

  end subroutine
!
!-----------------------------------------------------------------------
!     Run
!-----------------------------------------------------------------------
!
  subroutine ATM_Run(gcomp, rc)

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    type(ESMF_Clock)     :: clock
    type(ESMF_State)     :: importState, exportState

    integer :: i, j, maxdiv, runid, localPet, petCount
    integer :: myThid
    integer :: myIter = 0
    real*8 :: myTime = 0d0
    integer :: iLoop_atm = 1
    integer :: nTimeStepsIn = 1
    character(ESMF_MAXSTR) :: cname

    type(ESMF_VM) :: vm
    type(ESMF_Time) :: nextTime
    type(ESMF_Time) :: currTime
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Clock) :: internalClock
    real(ESMF_KIND_R8) :: wTimeStart, wTimeEnd

    call ESMF_VMWtime(wTimeStart)

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(gcomp, modelClock=clock, &
                        importState=importState, &
                        exportState=exportState, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    call ATM_Get(gcomp, iLoop_atm, rc)

    CALL ESMF_ClockGet( clock, currTime=currTime, &
                        timeStep=timeStep, rc=rc )
    nextTime = currTime + timeStep
    head_grid%start_subtime = currTime
    head_grid%stop_subtime = nextTime
  
    call wrf_run();

    call ATM_Put(gcomp, iLoop_atm, rc)

    call ESMF_ClockPrint(clock, options="currTime", &
                         preString="------>Advancing ATM from: ", rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, &
                       rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    call ESMF_TimePrint(currTime + timeStep, &
                        preString="--------------------------------> to: ", rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

  end subroutine ATM_Run

  

  subroutine ATM_Final(gcomp, importState, exportState, clock, rc)

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    type(ESMF_Clock)     :: clock
    type(ESMF_State)     :: importState, exportState

    rc = ESMF_SUCCESS

  end subroutine ATM_Final


  subroutine ATM_SetGridArrays(gcomp, petCount, localPet, gridIn, rc)
    !
    !-------------------------------------------------------------------
    ! Used module declarations
    !-------------------------------------------------------------------
    !  Sets the grid informations 
    !
    !
    implicit none

    type(ESMF_GridComp), intent(inout) :: gcomp
    integer, intent(in) :: localPet
    integer, intent(in) :: petCount
    type(ESMF_Grid) :: gridIn
    integer, intent(inout) :: rc
    type(ESMF_VM) :: vm
    character(ESMF_MAXSTR) :: cname

    integer :: k, m, n, p, iG, jG
    integer :: localDECount, tile
    character(ESMF_MAXSTR) :: name
    integer(ESMF_KIND_I4), pointer :: ptrM(:, :)
    real(ESMF_KIND_R8), pointer :: ptrX(:, :), ptrY(:, :), ptrA(:, :)
    type(ESMF_Array) :: arrX, arrY, arrM, arrA
    type(ESMF_StaggerLoc) :: staggerLoc
    type(ESMF_DistGrid) :: distGrid
    integer, allocatable :: deBlockList(:, :, :)
    integer, allocatable :: meshType(:)
    character(ESMF_MAXSTR), allocatable :: meshTypeName(:)

    ! Local variable declarations
    rc = ESMF_SUCCESS

    ! Get gridded component
    call ESMF_GridCompGet(gcomp, vm=vm, name=cname, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)
  
    if (allocated(ims_global)) deallocate(ims_global)
    if (allocated(ime_global)) deallocate(ime_global)
    if (allocated(jms_global)) deallocate(jms_global)
    if (allocated(jme_global)) deallocate(jme_global)
    
    allocate(ims_global(petCount))
    allocate(jms_global(petCount))
    allocate(ime_global(petCount))
    allocate(ime_global(petCount))

    call ESMF_VMAllGatherV(&
      vm, sendData=[ims], &
      sendCount=1, &
      recvData=ims_global, &
      recvCounts=[(1, k=0, petCount - 1)], &
      recvOffsets=[(k, k=0, petCount - 1)], &
      rc=rc &
    )
    _ERR_CHK(__FILE__,__LINE__)

    call ESMF_VMAllGatherV(&
      vm, sendData=[jms], &
      sendCount=1, &
      recvData=jms_global, &
      recvCounts=[(1, k=0, petCount - 1)], &
      recvOffsets=[(k, k=0, petCount - 1)], &
      rc=rc &
    )
    _ERR_CHK(__FILE__,__LINE__)

    call ESMF_VMAllGatherV(&
      vm, sendData=[ime], &
      sendCount=1, &
      recvData=ime_global, &
      recvCounts=[(1, k=0, petCount - 1)], &
      recvOffsets=[(k, k=0, petCount - 1)], &
      rc=rc &
    )
    _ERR_CHK(__FILE__,__LINE__)

    call ESMF_VMAllGatherV(&
      vm, sendData=[jme], &
      sendCount=1, &
      recvData=jme_global, &
      recvCounts=[(1, k=0, petCount - 1)], &
      recvOffsets=[(k, k=0, petCount - 1)], &
      rc=rc &
    )
    _ERR_CHK(__FILE__,__LINE__)
  
    
    ! Get limits of the grid arrays (based on PET and nest level)
    if (allocated(deBlockList)) deallocate(deBlockList)
    allocate (deBlockList(2, 2, 1:petCount))

    do tile = 1, petCount
      deBlockList(1, 1, tile) = ims_global(tile)
      deBlockList(1, 2, tile) = ime_global(tile) 
      deBlockList(2, 1, tile) = jms_global(tile) 
      deBlockList(2, 2, tile) = jme_global(tile) 
    end do

    ! Create ESMF DistGrid based on model domain decomposition
    distGrid = ESMF_DistGridCreate( &
      minIndex=(/1, 1/), &
      maxIndex=(/NX, NY/), &
      deBlockList=deBlockList, &
      rc=rc &
    )
    _ERR_CHK(__FILE__,__LINE__)

    ! Set staggering type
    staggerLoc = ESMF_STAGGERLOC_CENTER ! Icross

    ! Create ESMF Grid
    gridIn = ESMF_GridCreate(distgrid=distGrid, &
                             indexflag=ESMF_INDEX_GLOBAL, &
                             name="atm_grid", rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    ! Allocate coordinates
    call ESMF_GridAddCoord(gridIn, staggerLoc=staggerLoc, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    call ESMF_GridGetCoord(&
      gridIn, localDE=0, &
      staggerLoc=staggerLoc, &
      coordDim=1, &
      farrayPtr=ptrX, &
      rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    call ESMF_GridGetCoord(&
      gridIn, localDE=0, &
      staggerLoc=staggerLoc, &
      coordDim=2, &
      farrayPtr=ptrY, &
      rc=rc)
    _ERR_CHK(__FILE__,__LINE__)
 
    do j = jms, jme 
      do i = ims, ime
        ptrY(i,j) = head_grid%xlat(i,j)
        if (head_grid%xlong(i, j) < 0.) then
          ptrX(i, j) = head_grid%xlong(i, j) + 360d0
        else
          ptrX(i, j) = head_grid%xlong(i, j)
        end if
      end do
    end do

    ! Nullify pointers
    if (associated(ptrY)) nullify (ptrY)
    if (associated(ptrX)) nullify (ptrX)
    if (associated(ptrM)) nullify (ptrM)
    if (associated(ptrA)) nullify (ptrA)
  
    ! Write the grid to debug
    if (debugLevel >= 1) then
      call ESMF_GridGetCoord(gridIn, &
                             staggerLoc=ESMF_STAGGERLOC_CENTER, &
                             coordDim=1, array=arrX, rc=rc)
      _ERR_CHK(__FILE__,__LINE__)
      call ESMF_GridGetCoord(gridIn, &
                             staggerLoc=ESMF_STAGGERLOC_CENTER, &
                             coordDim=2, array=arrY, rc=rc)
      _ERR_CHK(__FILE__,__LINE__)
      call ESMF_ArrayWrite(arrX, filename="ocean_xa_1.nc", &
                           status=ESMF_FILESTATUS_NEW, rc=rc)
      _ERR_CHK(__FILE__,__LINE__)
      call ESMF_ArrayWrite(arrY, filename="ocean_ya_1.nc", &
                           status=ESMF_FILESTATUS_NEW, rc=rc)
      _ERR_CHK(__FILE__,__LINE__)
    end if
    
    ! Assign grid to gridded component
    call ESMF_GridCompSet(gcomp, grid=gridIn, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

  end subroutine ATM_SetGridArrays



  subroutine ATM_SetStates(gcomp, gridIn, gridOut, rc)
    implicit none
    !     Imported variable declarations
    type(ESMF_GridComp), intent(in) :: gcomp
    type(ESMF_Grid) :: gridIn
    type(ESMF_Grid) :: gridOut
    integer, intent(out) :: rc

    ! Local variable declarations
    integer :: i, j, k, itemCount, localDECount, localPet, petCount
    character(ESMF_MAXSTR), allocatable :: itemNameList(:)
    real*8, dimension(:, :), pointer :: ptr2d_tmp
    INTEGER sNx, sNy, OLx, OLy, nSx, nSy, nPx, nPy, Nx, Ny, Nr
    INTEGER myXGlobalLo, myYGlobalLo
!
    type(ESMF_VM) :: vm
    type(ESMF_Field) :: field_tmp
    type(ESMF_ArraySpec) :: arraySpec
    type(ESMF_StaggerLoc) :: staggerLoc
    type(ESMF_State) :: importState, exportState

    character(ESMF_MAXSTR) :: entryNameNUOPC, entryNameWRF
    integer :: iEntry
    logical :: exportEntry

    rc = ESMF_SUCCESS

    ! Get information about gridded component
    call ESMF_GridCompGet(gcomp, importState=importState, &
                          exportState=exportState, vm=vm, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    ! Set array descriptor
    call ESMF_ArraySpecSet(arraySpec, typekind=ESMF_TYPEKIND_R8, &
                           rank=2, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    ! Get number of local DEs
    call ESMF_GridGet(gridIn, localDECount=localDECount, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    !     Set staggering type
    staggerLoc = ESMF_STAGGERLOC_CENTER

    ! Create export field
    do iEntry = 1, nList
      entryNameNUOPC = trim(nuopc_entryNameList(iEntry)); 
      entryNameWRF = trim(wrf_nameList(iEntry)); 
      field_tmp = ESMF_FieldCreate(&
        gridOut, arraySpec, &
        staggerloc=staggerLoc, &
        indexflag=ESMF_INDEX_GLOBAL, &
        name=entryNameWRF, rc=rc)


      !Put initial data into state
      call ESMF_FieldGet(field_tmp, localDe=0, farrayPtr=ptr2d_tmp, rc=rc)
      _ERR_CHK(__FILE__,__LINE__)

      ! Initialize pointer
      ptr2d_tmp = 0.d0
      ! Nullify the pointer
      if (associated(ptr2d_tmp)) then
        nullify (ptr2d_tmp)
      end if

      if (exportEntry == .True.) then
        call NUOPC_Realize(exportState, field=field_tmp, rc=rc)
        _ERR_CHK(__FILE__,__LINE__)
      else
        call NUOPC_Realize(importState, field=field_tmp, rc=rc)
        _ERR_CHK(__FILE__,__LINE__)
      end if
    end do
!
  end subroutine ATM_SetStates


  subroutine ATM_put(gcomp, iLoop, rc)
    !-----------------------------------------------------------------------
    !     ATM model put data to external
    !-----------------------------------------------------------------------
    implicit none

    type(ESMF_GridComp) :: gcomp
    integer :: iLoop
    integer, intent(out) :: rc

    ! Local variable declarations
    integer :: localPet, petCount, itemCount, localDECount
    character(ESMF_MAXSTR) :: cname, ofile
    real(ESMF_KIND_R8), dimension(:,:), pointer :: farrayPtr
!
    type(ESMF_VM) :: vm
    type(ESMF_Clock) :: clock
    type(ESMF_Grid) :: gridIn
    type(ESMF_Field) :: field
    type(ESMF_State) :: exportState

    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(&
      gcomp, name=cname, &
      clock=clock, &
      grid=gridIn, &
      exportState=exportState, &
      vm=vm, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    ! Put field 1
    _ATM_PUT(LANDMASK)

    ! Put field 2
    _ATM_PUT(LH)

    ! Put field 3
    _ATM_PUT(HFX)

    ! Put field 4
    _ATM_PUT(SWUPB)

    ! Put field 5
    _ATM_PUT(SWDNB)

    ! Put field 6
    _ATM_PUT(LWUPB)

    ! Put field 7
    _ATM_PUT(LWDNB)

    ! Put field 8
    _ATM_PUT(U10)

    ! Put field 9
    _ATM_PUT(V10)

    ! Put field 10
    _ATM_PUT(T2)

    ! Put field 11
    _ATM_PUT(Q2)

    ! Put field 12
    _ATM_PUT(QFX)

    ! Put field 13
    _ATM_PUT(RAINCV)

    ! Put field 14
    _ATM_PUT(RAINSHV)

    ! Put field 15
    _ATM_PUT(RAINNCV)

    ! Put field 16
    _ATM_PUT(SST_INPUT)

  end subroutine ATM_put


  subroutine ATM_get(gcomp, iLoop, rc)
    !     Atm model get data from external
    implicit none


    ! Imported variable declarations
    type(ESMF_GridComp) :: gcomp
    type(ESMF_Grid) :: gridIn
    integer, intent(out) :: rc
    integer :: iLoop


    character(ESMF_MAXSTR) :: cname, ofile
    real(ESMF_KIND_R8), pointer :: farrayPtr(:, :)
!
    type(ESMF_VM) :: vm
    type(ESMF_Clock) :: clock
    type(ESMF_Field) :: esmfField
    type(ESMF_State) :: esmfState

    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(&
      gcomp, &
      name=cname, &
      clock=clock, &
      grid=gridIn, & 
      importState=importState, &
      vm=vm, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    _ATM_GET(SST)
    _ATM_GET(UOCE)
    _ATM_GET(VOCE)

!   ! Write field to debug
    if (debugLevel >= 1) then
      write (ofile, "(A9,I6.6,A3)") "sstATMput", iLoop, ".nc"
      call ESMF_FieldWrite(field_sst, trim(ofile), rc=rc)
    end if

  end subroutine ATM_Put

end module mod_esmf_atm


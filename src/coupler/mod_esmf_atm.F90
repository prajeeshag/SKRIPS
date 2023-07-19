#include "esmf_macros.inc"
module mod_esmf_atm
  !-----------------------------------------------------------------------
  !     ATM gridded component code
  !-----------------------------------------------------------------------
  !
  use ESMF
  use NUOPC
  use NUOPC_Model, &
    NUOPC_SetServices => SetServices, &
    NUOPC_Label_SetClock => label_SetClock, &
    NUOPC_Label_Advance => label_Advance, &
    NUOPC_Label_DataInitialize => label_DataInitialize

  use ieee_arithmetic, only: ieee_is_nan

  use mod_types
  use module_wrf_top, only: wrf_init, wrf_run, wrf_finalize
  use module_domain, only: head_grid, get_ijk_from_grid
  use module_utility, only: WRFU_TIME, WRFU_TIMESET, WRFU_TIMEGET
  use module_wrf_quilt, only: compute_node

  implicit none
  private


  integer :: NX, NY  ! Global NX, NY
  integer :: isc=0, iec=-1, jsc=0, jec=-1 ! Local bounds

  integer :: localPet, petCount
  character(ESMF_MAXSTR) :: component_name

  integer :: iLoop = 1

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
    type(ESMF_VM) :: vm

    rc = ESMF_SUCCESS

    ! Get gridded component
    call ESMF_GridCompGet(gcomp, vm=vm, name=component_name, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

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

    do iEntry = 1, nList
      entryNameNUOPC = trim(nuopc_entryNameList(iEntry)); 
      entryNameWRF = trim(wrf_nameList(iEntry)); 
      exportEntry = ATMtoOCN(iEntry); 
      if (exportEntry) then
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
    integer :: comm
    character(ESMF_MAXSTR) :: gname
    type(ESMF_Field) :: field
    type(ESMF_Grid) :: atmGridIn
    type(ESMF_Grid) :: atmGridOut
    type(ESMF_VM) :: vm
    type(ESMF_Time) :: currTime
    type(ESMF_Time) :: nextTime
    integer :: yy, mm, dd, h, m, s
    integer :: ims, ime, jms, jme, kms, kme
    integer :: ids, ide, jds, jde, kds, kde
    integer :: ips, ipe, jps, jpe, kps, kpe

    rc = ESMF_SUCCESS
    !
    !-------------------------------------------------------------------
    ! Get gridded component
    !-------------------------------------------------------------------
    !
    call ESMF_GridCompGet(gcomp, name=gname, vm=vm, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)


    call ESMF_VMGet(vm, mpiCommunicator=comm, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)
    !
    !-------------------------------------------------------------------
    ! Initialize the gridded component
    !-------------------------------------------------------------------
    !

    call ESMF_VMBarrier(vm,rc=rc)
    print *, "BEFORE WRF_INIT in __FILE__ on line __LINE__"

    call wrf_set_dm_communicator(comm)
    call wrf_init()
   
    call ESMF_VMBarrier(vm,rc=rc)
    print *, "After WRF_INIT in __FILE__ on line __LINE__"

    if (compute_node) then
      call get_ijk_from_grid( &
        head_grid ,                   &
        ids, ide, jds, jde, kds, kde,    &
        ims, ime, jms, jme, kms, kme,    &
        ips, ipe, jps, jpe, kps, kpe  &
      )
      
      NX = ide - ids  ! in other words, the number of points from ids to ide-1 inclusive
      NY = jde - jds  ! in other words, the number of points from jds to jde-1 inclusive
      
      isc = ips
      jsc = jps
      iec = MIN(ipe, ide - 1)
      jec = MIN(jpe, jde - 1)
    endif


    call ATM_SetGridArrays(gcomp, atmGridIn, rc)
    _ERR_CHK(__FILE__,__LINE__)
    atmGridOut = atmGridIn

    call ATM_SetStates(gcomp, atmGridIn, atmGridOut, rc)
    _ERR_CHK(__FILE__,__LINE__)

    !! run an empty step to fill the data
    CALL ESMF_ClockGet(clock, currTime=currTime, rc=rc )
    _ERR_CHK(__FILE__,__LINE__)

    head_grid%start_subtime = currTime
    head_grid%stop_subtime = currTime
  
    ! run an empty step (for initialization)
    call wrf_run();

  end subroutine ATM_Init2


  subroutine ESMF_Time_to_WRF_Time(wrfTime, esmfTime)
    type(WRFU_Time), intent(out) :: wrfTime
    type(ESMF_Time), intent(in) :: esmfTime 
    
    integer :: yy, mm, dd, h, m, s
    integer :: rc

    call ESMF_TimeGet(esmfTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, rc=rc)
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
    call ATM_Put(gcomp, rc)
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

    integer :: i, j, maxdiv, runid 
    integer :: myThid
    integer :: myIter = 0
    real*8 :: myTime = 0d0
    integer :: nTimeStepsIn = 1

    type(ESMF_VM) :: vm
    type(ESMF_Time) :: nextTime
    type(ESMF_Time) :: currTime
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_Clock) :: internalClock
    real(ESMF_KIND_R8) :: wTimeStart, wTimeEnd

    call ESMF_VMWtime(wTimeStart)

    rc = ESMF_SUCCESS

    call NUOPC_ModelGet(&
      gcomp, modelClock=clock, &
      importState=importState, &
      exportState=exportState, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    call ATM_Get(gcomp, rc)

    CALL ESMF_ClockGet( clock, currTime=currTime, &
                        timeStep=timeStep, rc=rc )
    nextTime = currTime + timeStep
    head_grid%start_subtime = currTime
    head_grid%stop_subtime = nextTime
  
    call wrf_run()

    call ATM_Put(gcomp, rc)

    if (localPet==0) then
      call ESMF_ClockPrint(clock, options="currTime", &
                           preString="------>Advancing ATM from: ", rc=rc)
      _ERR_CHK(__FILE__,__LINE__)

      call ESMF_TimePrint(currTime + timeStep, &
                          preString="--------------------------------> to: ", rc=rc)
      _ERR_CHK(__FILE__,__LINE__)
    endif 

  end subroutine ATM_Run

  

  subroutine ATM_Final(gcomp, importState, exportState, clock, rc)

    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    type(ESMF_Clock)     :: clock
    type(ESMF_State)     :: importState, exportState

    rc = ESMF_SUCCESS

  end subroutine ATM_Final


  subroutine ATM_SetGridArrays(gcomp, gridIn, rc)
    !
    !-------------------------------------------------------------------
    ! Used module declarations
    !-------------------------------------------------------------------
    !  Sets the grid informations 
    !
    !
    implicit none

    type(ESMF_GridComp), intent(inout) :: gcomp
    type(ESMF_Grid) :: gridIn
    integer, intent(inout) :: rc

    type(ESMF_VM) :: vm
    integer ::  tile
    real(ESMF_KIND_R8), pointer :: ptrX(:, :), ptrY(:, :)
    type(ESMF_Array) :: arrX, arrY
    type(ESMF_StaggerLoc) :: staggerLoc
    type(ESMF_DistGrid) :: distGrid
    type(ESMF_DELayout) :: deLayout
    integer, allocatable :: deBlockList(:, :, :)
    integer :: j, i, k, de, deCount
    integer, allocatable :: isc_global(:)
    integer, allocatable :: jsc_global(:)
    integer, allocatable :: iec_global(:)
    integer, allocatable :: jec_global(:)
    integer, allocatable :: petMap(:)


    ! Local variable declarations
    rc = ESMF_SUCCESS

    ! Get gridded component
    call ESMF_GridCompGet(gcomp, vm=vm, name=component_name, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    if (allocated(isc_global)) deallocate(isc_global); allocate(isc_global(petCount))
    if (allocated(iec_global)) deallocate(iec_global); allocate(iec_global(petCount))
    if (allocated(jsc_global)) deallocate(jsc_global); allocate(jsc_global(petCount))
    if (allocated(jec_global)) deallocate(jec_global); allocate(jec_global(petCount))

    call ESMF_VMAllGatherV(&
      vm, sendData=[isc], &
      sendCount=1, &
      recvData=isc_global, &
      recvCounts=[(1, k=0, petCount - 1)], &
      recvOffsets=[(k, k=0, petCount - 1)], &
      rc=rc &
    )
    _ERR_CHK(__FILE__,__LINE__)

    call ESMF_VMAllGatherV(&
      vm, sendData=[jsc], &
      sendCount=1, &
      recvData=jsc_global, &
      recvCounts=[(1, k=0, petCount - 1)], &
      recvOffsets=[(k, k=0, petCount - 1)], &
      rc=rc &
    )
    _ERR_CHK(__FILE__,__LINE__)

    call ESMF_VMAllGatherV(&
      vm, sendData=[iec], &
      sendCount=1, &
      recvData=iec_global, &
      recvCounts=[(1, k=0, petCount - 1)], &
      recvOffsets=[(k, k=0, petCount - 1)], &
      rc=rc &
    )
    _ERR_CHK(__FILE__,__LINE__)

    call ESMF_VMAllGatherV(&
      vm, sendData=[jec], &
      sendCount=1, &
      recvData=jec_global, &
      recvCounts=[(1, k=0, petCount - 1)], &
      recvOffsets=[(k, k=0, petCount - 1)], &
      rc=rc &
    )
    _ERR_CHK(__FILE__,__LINE__)
    
    
    deCount = count(isc_global > 0) ! isc is 0 for ioservers
    allocate (deBlockList(2, 2, 1:deCount))
    allocate (petMap(1:deCount))
    
    de = 0 
    do tile = 1, petCount
      if (isc_global(tile) < 1) continue
      de = de + 1
      deBlockList(1, 1, de) = isc_global(tile)
      deBlockList(1, 2, de) = iec_global(tile) 
      deBlockList(2, 1, de) = jsc_global(tile) 
      deBlockList(2, 2, de) = jec_global(tile)
      petMap(de) = tile-1
    end do
    
    deLayout = ESMF_DELayoutCreate(petMap, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)
    
    ! Create ESMF DistGrid based on model domain decomposition
    distGrid = ESMF_DistGridCreate( &
      minIndex=(/1, 1/), &
      maxIndex=(/NX, NY/), &
      deBlockList=deBlockList, &
      deLayout=deLayout, &
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

    deallocate(isc_global)
    deallocate(iec_global)
    deallocate(jsc_global)
    deallocate(jec_global)
    deallocate(deBlockList)
    deallocate(petMap)

    ! Allocate coordinates
    call ESMF_GridAddCoord(gridIn, staggerLoc=staggerLoc, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)
    
    if (compute_node) then
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
 
      do j = jsc, jec
        do i = isc, iec
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
    end if ! if (compute_node) 
  
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
    integer :: i, j, k, itemCount, localDECount
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

      if (compute_node) then
        !Put initial data into state
        call ESMF_FieldGet(field_tmp, localDe=0, farrayPtr=ptr2d_tmp, rc=rc)
        _ERR_CHK(__FILE__,__LINE__)

        ! Initialize pointer
        ptr2d_tmp = 0.d0
        ! Nullify the pointer
        if (associated(ptr2d_tmp)) then
          nullify (ptr2d_tmp)
        end if
      end if

      if (ATMtoOCN(iEntry)) then
        call NUOPC_Realize(exportState, field=field_tmp, rc=rc)
        _ERR_CHK(__FILE__,__LINE__)
      else
        call NUOPC_Realize(importState, field=field_tmp, rc=rc)
        _ERR_CHK(__FILE__,__LINE__)
      end if
    end do
!
  end subroutine ATM_SetStates


  subroutine ATM_put(gcomp, rc)
    !-----------------------------------------------------------------------
    !     ATM model put data to external
    !-----------------------------------------------------------------------
    implicit none

    type(ESMF_GridComp) :: gcomp
    integer, intent(out) :: rc

    ! Local variable declarations
    integer :: itemCount, localDECount
    character(ESMF_MAXSTR) :: ofile
    real(ESMF_KIND_R8), dimension(:,:), pointer :: farrayPtr
!
    type(ESMF_VM) :: vm
    type(ESMF_Clock) :: clock
    type(ESMF_Grid) :: gridIn
    type(ESMF_Field) :: esmfField
    type(ESMF_State) :: esmfState

    rc = ESMF_SUCCESS
    
    call ESMF_GridCompGet(&
      gcomp, name=component_name, &
      clock=clock, &
      grid=gridIn, &
      exportState=esmfState, &
      vm=vm, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)

    if (.not. compute_node) return

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


  subroutine ATM_get(gcomp, rc)
    !     Atm model get data from external
    implicit none


    ! Imported variable declarations
    type(ESMF_GridComp) :: gcomp
    type(ESMF_Grid) :: gridIn
    integer, intent(out) :: rc


    character(ESMF_MAXSTR) :: ofile
    real(ESMF_KIND_R8), pointer :: farrayPtr(:, :)
!
    type(ESMF_VM) :: vm
    type(ESMF_Clock) :: clock
    type(ESMF_Field) :: esmfField
    type(ESMF_State) :: esmfState

    rc = ESMF_SUCCESS


    call ESMF_GridCompGet(&
      gcomp, &
      name=component_name, &
      clock=clock, &
      grid=gridIn, & 
      importState=esmfState, &
      vm=vm, rc=rc)
    _ERR_CHK(__FILE__,__LINE__)
    if (.not. compute_node) return

    _ATM_GET(SST)
    _ATM_GET(UOCE)
    _ATM_GET(VOCE)

  end subroutine ATM_get

end module mod_esmf_atm


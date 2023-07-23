! Earth System Modeling Framework
! Copyright 2002-2017, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!-----------------------------------------------------------------------
!     Main function
!-----------------------------------------------------------------------
!
    program esmf_application
    
      ! modules
      use ESMF
      use NUOPC
      use mod_esmf_esm, only : ESM_SetServices 
      use mod_config, only : read_config
      use mod_types, only: cpuOcn, cpuAtm, coupleMode, debuglevel


      use module_wrf_quilt, only: compute_node, quilt
      use module_wrf_top, only: wrf_init
      ! use mod_config, only : set_field_dir
       
      implicit none
      include 'mpif.h'

      ! local variables
      integer :: rc, urc
      type(ESMF_GridComp) :: esmComp
      type(ESMF_VM) :: vm

      integer :: ierror 
      integer :: iounit=10
      integer :: nprocs, pe, no_io_nprocs
      integer :: icolor
      integer :: local_comm, no_io_comm
      logical :: lexist, is_wrf, is_wrf_io

      character(len=256) :: namelist_file="namelist.esmf"
     

      namelist/coupler_nml/cpuOcn, cpuAtm, coupleMode, debuglevel

!-----------------------------------------------------------------------
!     Initialize ESMF framework
!-----------------------------------------------------------------------
!
      call MPI_INIT(ierror)

      inquire(file=trim(namelist_file), exist=lexist)

      if (.not.lexist) then 
        print *, "Error: "//trim(namelist_file)//" not found"
        call MPI_ABORT(MPI_COMM_WORLD, ierror)
      endif

      open(iounit, file=namelist_file)

      read(unit=iounit, nml=coupler_nml, iostat=ierror) 
      if (ierror /= 0) then 
        print *, "Error: Error while reading coupler_nml from "//trim(namelist_file)
        call MPI_ABORT(MPI_COMM_WORLD, ierror)
      endif

      call MPI_COMM_SIZE(MPI_COMM_WORLD, nprocs, ierror) 
      if (coupleMode /= 1 .and. nprocs /= cpuAtm+cpuOcn) then 
        print *, "cpuOcn + cpuAtm /= nprocs"
        call MPI_ABORT(MPI_COMM_WORLD, ierror)
      end if

      call MPI_COMM_RANK(MPI_COMM_WORLD, pe, ierror) 
      
      icolor = 0
      is_wrf = .false.
      is_wrf_io = .false.
      if (pe < cpuAtm) then
        icolor = 1
        is_wrf = .true.
      endif
      
      call MPI_Comm_split(MPI_COMM_WORLD, icolor, pe, local_comm, ierror)


      if (is_wrf) then 
        call wrf_set_dm_communicator(local_comm)
        call wrf_init()
        is_wrf_io = .not. compute_node
      endif
      
      icolor = 0
      if (is_wrf_io) icolor = 1
      
      call MPI_Comm_split(MPI_COMM_WORLD, icolor, pe, no_io_comm, ierror)

      if (is_wrf_io) call quilt ! wrf io pes won't return

      call MPI_Comm_size(no_io_comm, no_io_nprocs, ierror)
      ! Adjust cpuAtm
      cpuAtm = cpuAtm - (nprocs-no_io_nprocs)

      if (debuglevel > 0) then
        call ESMF_Initialize(&
          logkindflag=ESMF_LOGKIND_Multi,    &
          defaultCalkind=ESMF_CALKIND_GREGORIAN,       &
          mpiCommunicator=no_io_comm, &
          vm=vm, rc=rc)
      else
        call ESMF_Initialize(&
          logkindflag=ESMF_LOGKIND_Multi_On_Error,    &
          defaultCalkind=ESMF_CALKIND_GREGORIAN,       &
          mpiCommunicator=no_io_comm, &
          vm=vm, rc=rc)
      end if


      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Create component 
!-----------------------------------------------------------------------
!
      esmComp = ESMF_GridCompCreate(name="NEW_GRID", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Read main configuration file 
!-----------------------------------------------------------------------
!
      call read_config(vm, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Register component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompSetServices(esmComp, ESM_SetServices,           &
                                    userRc=urc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU,   &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Wait for finishing initialize phase
!-----------------------------------------------------------------------
!
      call ESMF_VMBarrier(vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Initialize component
!-----------------------------------------------------------------------
!
      call ESMF_GridCompInitialize(esmComp, userRc=urc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU,   &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Run component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompRun(esmComp, userRc=urc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU,   &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
!
!-----------------------------------------------------------------------
!     Finalize component 
!-----------------------------------------------------------------------
!
      call ESMF_GridCompFinalize(esmComp, userRc=urc, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
      if (ESMF_LogFoundError(rcToCheck=urc, msg=ESMF_LOGERR_PASSTHRU,   &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
! 
!-----------------------------------------------------------------------
!     Destroy the earth system Component
!-----------------------------------------------------------------------
! 
      call ESMF_GridCompDestroy(esmComp, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU,    &
          line=__LINE__, file=__FILE__))                                &
          call ESMF_Finalize(endflag=ESMF_END_ABORT)
! 
!-----------------------------------------------------------------------
!     Finalize ESMF framework 
!-----------------------------------------------------------------------
!
      call ESMF_Finalize(rc=rc)
      
    end program esmf_application

!*******************************************************************************
!>  @file vmec_context.f
!>  @brief Contains module @ref vmec_context.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref vmec_context_class. This contains
!>  the state variables needed by VMEC.
!*******************************************************************************
      MODULE vmec_context
      USE stel_kinds
      USE data_parameters
      USE xstuff
      USE vmec_input, Only: raxis_cc, raxis_cs, zaxis_cc, zaxis_cs,            &
     &                      rbc, rbs, zbc, zbs
      USE v3f_vmec_comm
      USE profiler
      USE mpi_inc

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) vmec_context_class class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a vmec_context. This contains a copy of every
!>  variable that is needed to define the VMEC state.
!-------------------------------------------------------------------------------
      TYPE vmec_context_class
!>  Cache of the vmec internal state xc array.
         REAL (rprec), DIMENSION(:), POINTER :: xc => null()

!>  Cache of the magnetic axis R cosine coefficents.
         REAL (rprec), DIMENSION(:), POINTER   :: raxis_cc => null()
!>  Cache of the magnetic axis R sine coefficents.
         REAL (rprec), DIMENSION(:), POINTER   :: raxis_cs => null()
!>  Cache of the magnetic axis Z cosine coefficents.
         REAL (rprec), DIMENSION(:), POINTER   :: zaxis_cc => null()
!>  Cache of the magnetic axis Z sine coefficents.
         REAL (rprec), DIMENSION(:), POINTER   :: zaxis_cs => null()

!>  Cache of the equilibrium boundary R cosine coefficents.
         REAL (rprec), DIMENSION(:,:), POINTER :: rbc => null()
!>  Cache of the equilibrium boundary R sine coefficents.
         REAL (rprec), DIMENSION(:,:), POINTER :: rbs => null()
!>  Cache of the equilibrium boundary Z cosine coefficents.
         REAL (rprec), DIMENSION(:,:), POINTER :: zbc => null()
!>  Cache of the equilibrium boundary Z sine coefficents.
         REAL (rprec), DIMENSION(:,:), POINTER :: zbs => null()

!  threed1 file variables.
!>  Internal inductance.
         REAL (rprec)                          :: vvc_smaleli
!>  Mean elongation.
         REAL (rprec)                          :: vvc_kappa_p
      END TYPE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a new @ref vmec_context_class object.
!>
!>  Allocates memory and initializes a @ref vmec_context_class object.
!>
!>  @returns A pointer to a constructed @ref vmec_context_class object.
!-------------------------------------------------------------------------------
      FUNCTION vmec_context_construct()

      IMPLICIT NONE

!  Declare Arguments
      TYPE (vmec_context_class), POINTER :: vmec_context_construct

!  local variables
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(vmec_context_construct)

      ALLOCATE(vmec_context_construct%xc(SIZE(xc)))
      ALLOCATE(vmec_context_construct%raxis_cc(SIZE(raxis_cc)))
      ALLOCATE(vmec_context_construct%raxis_cs(SIZE(raxis_cs)))
      ALLOCATE(vmec_context_construct%zaxis_cc(SIZE(zaxis_cc)))
      ALLOCATE(vmec_context_construct%zaxis_cs(SIZE(zaxis_cs)))

      ALLOCATE(vmec_context_construct%rbc(SIZE(rbc,1),SIZE(rbc,2)))
      ALLOCATE(vmec_context_construct%rbs(SIZE(rbs,1),SIZE(rbs,2)))
      ALLOCATE(vmec_context_construct%zbc(SIZE(zbc,1),SIZE(zbc,2)))
      ALLOCATE(vmec_context_construct%zbs(SIZE(zbs,1),SIZE(zbs,2)))

      CALL vmec_context_get_context(vmec_context_construct)

      CALL profiler_set_stop_time('vmec_context_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref vmec_context_class object.
!>
!>  Deallocates memory and uninitializes a @ref vmec_context_class object.
!>
!>  @param[inout] this A @ref vmec_context_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE vmec_context_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (vmec_context_class), POINTER :: this

!  Start of executable code
      IF (ASSOCIATED(this%xc)) THEN
         DEALLOCATE(this%xc)
         this%xc => null()
      END IF

      IF (ASSOCIATED(this%raxis_cc)) THEN
         DEALLOCATE(this%raxis_cc)
         this%raxis_cc => null()
      END IF

      IF (ASSOCIATED(this%raxis_cs)) THEN
         DEALLOCATE(this%raxis_cs)
         this%raxis_cs => null()
      END IF

      IF (ASSOCIATED(this%zaxis_cc)) THEN
         DEALLOCATE(this%zaxis_cc)
         this%zaxis_cc => null()
      END IF

      IF (ASSOCIATED(this%zaxis_cs)) THEN
         DEALLOCATE(this%zaxis_cs)
         this%zaxis_cs => null()
      END IF

      IF (ASSOCIATED(this%rbc)) THEN
         DEALLOCATE(this%rbc)
         this%rbc => null()
      END IF

      IF (ASSOCIATED(this%rbs)) THEN
         DEALLOCATE(this%rbs)
         this%rbs => null()
      END IF

      IF (ASSOCIATED(this%zbc)) THEN
         DEALLOCATE(this%zbc)
         this%zbc => null()
      END IF

      IF (ASSOCIATED(this%zbs)) THEN
         DEALLOCATE(this%zbs)
         this%zbs => null()
      END IF

      DEALLOCATE(this)

      END SUBROUTINE

!*******************************************************************************
!  SETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Sets the current context to the @ref vmec_context_class object.
!>
!>  This sets this context to be the VMEC state.
!>
!>  @param[in] this A @ref vmec_context_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE vmec_context_set_context(this)
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      TYPE (vmec_context_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL assert_eq(SIZE(this%xc), SIZE(xc), "xc size change. " //            &
     &               "Cannot set context.")
      xc = this%xc

      raxis_cc = this%raxis_cc
      raxis_cs = this%raxis_cs
      zaxis_cc = this%zaxis_cc
      zaxis_cs = this%zaxis_cs

      rbc = this%rbc
      rbs = this%rbs
      zbc = this%zbc
      zbs = this%zbs

      vvc_smaleli = this%vvc_smaleli
      vvc_kappa_p = this%vvc_kappa_p

      CALL profiler_set_stop_time('vmec_context_set_context',                  &
     &                            start_time)

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Gets the current context from the VMEC internal state.
!>
!>  Copy the internal vmec state to the context object.
!>
!>  @param[inout] this A @ref vmec_context_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE vmec_context_get_context(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (vmec_context_class), INTENT(inout) :: this

!  local variables
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF(SIZE(this%xc) .ne. SIZE(xc)) THEN
         DEALLOCATE(this%xc)
         ALLOCATE(this%xc(SIZE(xc)))
      END IF
      this%xc = xc

      this%raxis_cc = raxis_cc
      this%raxis_cs = raxis_cs
      this%zaxis_cc = zaxis_cc
      this%zaxis_cs = zaxis_cs

      this%rbc = rbc
      this%rbs = rbs
      this%zbc = zbc
      this%zbs = zbs

      this%vvc_smaleli = vvc_smaleli
      this%vvc_kappa_p = vvc_kappa_p

      CALL profiler_set_stop_time('vmec_context_get_context',                  &
     &                            start_time)

      END SUBROUTINE

!*******************************************************************************
!  MPI SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Syncronize the vmec_context to children.
!>
!>  Syncs data between the parent and child processes. If MPI support is not
!>  compiled in this subroutine reduces to a no op. This syncs the VMEC
!>  variables directly.
!>
!>  @param[inout] this       A @ref vmec_context_class instance.
!>  @param[in]    recon_comm MPI communicator for the reconstruction processes.
!-------------------------------------------------------------------------------
      SUBROUTINE vmec_context_sync_state(this, recon_comm)
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      TYPE (vmec_context_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                      :: recon_comm

#if defined(MPI_OPT)
!  local variables
      INTEGER                                  :: error
      INTEGER                                  :: temp_size
      INTEGER                                  :: mpi_rank

!  Start of executable code
      CALL MPI_COMM_RANK(recon_comm, mpi_rank, error)

!  If the size of the xc array changed size we cannot sync.
      temp_size = SIZE(xc)
      CALL MPI_BCAST(temp_size, 1, MPI_INTEGER, 0, recon_comm, error)

      IF (mpi_rank .gt. 0) THEN
         CALL assert_eq(temp_size, SIZE(xc),                                   &
     &                  'Cannot sync xc arrays. Arrays changed size.')
      END IF
      CALL MPI_BCAST(xc, temp_size, MPI_REAL8, 0, recon_comm, error)

      CALL MPI_BCAST(raxis_cc, SIZE(raxis_cc), MPI_REAL8, 0, recon_comm,       &
     &               error)
      CALL MPI_BCAST(raxis_cs, SIZE(raxis_cs), MPI_REAL8, 0, recon_comm,       &
     &               error)
      CALL MPI_BCAST(zaxis_cc, SIZE(zaxis_cc), MPI_REAL8, 0, recon_comm,       &
     &               error)
      CALL MPI_BCAST(zaxis_cs, SIZE(zaxis_cs), MPI_REAL8, 0, recon_comm,       &
     &               error)

      CALL MPI_BCAST(rbc, SIZE(rbc,1)*SIZE(rbc,2), MPI_REAL8, 0,               &
     &               recon_comm, error)
      CALL MPI_BCAST(rbs, SIZE(rbs,1)*SIZE(rbs,2), MPI_REAL8, 0,               &
     &               recon_comm, error)
      CALL MPI_BCAST(zbc, SIZE(zbc,1)*SIZE(zbc,2), MPI_REAL8, 0,               &
     &               recon_comm, error)
      CALL MPI_BCAST(zbs, SIZE(zbs,1)*SIZE(zbs,2), MPI_REAL8, 0,               &
     &               recon_comm, error)

      CALL MPI_BCAST(vvc_smaleli, 1, MPI_REAL8, 0, recon_comm, error)
      CALL MPI_BCAST(vvc_kappa_p, 1, MPI_REAL8, 0, recon_comm, error)

#endif
      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Syncronize a child vmec_context to the parent.
!>
!>  Syncs data between a child and the parent process. If MPI support is not
!>  compiled in this subroutine reduces to a no op. This syncs the VMEC
!>  variables directly.
!>
!>  @param[inout] this       A @ref vmec_context_class instance.
!>  @param[in]    index      Reconstruction rank to sync.
!>  @param[in]    recon_comm MPI communicator for the reconstruction processes.
!-------------------------------------------------------------------------------
      SUBROUTINE vmec_context_sync_child(this, index, recon_comm)
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      TYPE (vmec_context_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                      :: index
      INTEGER, INTENT(in)                      :: recon_comm

#if defined(MPI_OPT)
!  local variables
      INTEGER                                  :: error
      INTEGER                                  :: temp_size
      INTEGER                                  :: mpi_rank

!  Start of executable code
      CALL MPI_COMM_RANK(recon_comm, mpi_rank, error)

!  If the size of the xc array changed size we cannot sync.
      temp_size = SIZE(xc)
      IF (mpi_rank .eq. index) THEN
         CALL MPI_SSEND(temp_size, 1, MPI_INTEGER, 0, mpi_rank,                &
     &                  recon_comm, error)

         CALL MPI_SSEND(xc, temp_size, MPI_REAL8, 0, mpi_rank,                 &
     &                  recon_comm, error)

         CALL MPI_SSEND(raxis_cc, SIZE(raxis_cc), MPI_REAL8, 0,                &
     &                  mpi_rank, recon_comm, error)
         CALL MPI_SSEND(raxis_cs, SIZE(raxis_cs), MPI_REAL8, 0,                &
     &                  mpi_rank, recon_comm, error)
         CALL MPI_SSEND(zaxis_cc, SIZE(zaxis_cc), MPI_REAL8, 0,                &
     &                  mpi_rank, recon_comm, error)
         CALL MPI_SSEND(zaxis_cs, SIZE(zaxis_cs), MPI_REAL8, 0,                &
     &                  mpi_rank, recon_comm, error)

         CALL MPI_SSEND(rbc, SIZE(rbc,1)*SIZE(rbc,2), MPI_REAL8, 0,            &
     &                  mpi_rank, recon_comm, error)
         CALL MPI_SSEND(rbs, SIZE(rbs,1)*SIZE(rbs,2), MPI_REAL8, 0,            &
     &                  mpi_rank, recon_comm, error)
         CALL MPI_SSEND(zbc, SIZE(zbc,1)*SIZE(zbc,2), MPI_REAL8, 0,            &
     &                  mpi_rank, recon_comm, error)
         CALL MPI_SSEND(zbs, SIZE(zbs,1)*SIZE(zbs,2), MPI_REAL8, 0,            &
     &                  mpi_rank, recon_comm, error)

         CALL MPI_SSEND(vvc_smaleli, 1, MPI_REAL8, 0, mpi_rank,                &
     &                  recon_comm, error)
         CALL MPI_SSEND(vvc_kappa_p, 1, MPI_REAL8, 0, mpi_rank,                &
     &                  recon_comm, error)

      ELSE IF (mpi_rank .eq. 0) THEN
         CALL MPI_RECV(temp_size, 1, MPI_INTEGER, index, index,                &
     &                 recon_comm, MPI_STATUS_IGNORE, error)
         CALL assert_eq(temp_size, SIZE(xc),                                   &
     &                  'Cannot sync xc arrays. Arrays changed size.')

         CALL MPI_RECV(xc, temp_size, MPI_REAL8, index, index,                 &
     &                 recon_comm, MPI_STATUS_IGNORE, error)

         CALL MPI_RECV(raxis_cc, SIZE(raxis_cc), MPI_REAL8, index,             &
     &                 index, recon_comm, MPI_STATUS_IGNORE, error)
         CALL MPI_RECV(raxis_cs, SIZE(raxis_cs), MPI_REAL8, index,             &
     &                 index, recon_comm, MPI_STATUS_IGNORE, error)
         CALL MPI_RECV(zaxis_cc, SIZE(zaxis_cc), MPI_REAL8, index,             &
     &                 index, recon_comm, MPI_STATUS_IGNORE, error)
         CALL MPI_RECV(zaxis_cs, SIZE(zaxis_cs), MPI_REAL8, index,             &
     &                 index, recon_comm, MPI_STATUS_IGNORE, error)

         CALL MPI_RECV(rbc, SIZE(rbc,1)*SIZE(rbc,2), MPI_REAL8, index,        &
     &                 index, recon_comm, MPI_STATUS_IGNORE, error)
         CALL MPI_RECV(rbs, SIZE(rbs,1)*SIZE(rbs,2), MPI_REAL8, index,        &
     &                 index, recon_comm, MPI_STATUS_IGNORE, error)
         CALL MPI_RECV(zbc, SIZE(zbc,1)*SIZE(zbc,2), MPI_REAL8, index,        &
     &                 index, recon_comm, MPI_STATUS_IGNORE, error)
         CALL MPI_RECV(zbs, SIZE(zbs,1)*SIZE(zbs,2), MPI_REAL8, index,        &
     &                 index, recon_comm, MPI_STATUS_IGNORE, error)

         CALL MPI_RECV(vvc_smaleli, 1, MPI_REAL8, index, index,                &
     &                 recon_comm, MPI_STATUS_IGNORE, error)
         CALL MPI_RECV(vvc_kappa_p, 1, MPI_REAL8, index, index,                &
     &                 recon_comm, MPI_STATUS_IGNORE, error)

      END IF

#endif
      END SUBROUTINE

      END MODULE

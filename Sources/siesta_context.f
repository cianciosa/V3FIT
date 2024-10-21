!*******************************************************************************
!>  @file siesta_context.f
!>  @brief Contains module @ref siesta_context.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref siesta_context_class. This contains
!>  the state variables needed by SIESTA.
!*******************************************************************************
      MODULE siesta_context
      USE stel_kinds
      USE profiler
      USE restart_mod

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) siesta_context_class class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a siesta_context. This contains a copy of every
!>  variable that is needed to define the SIESTA state.
!-------------------------------------------------------------------------------
      TYPE :: siesta_context_class
!>  Flag indicating that stellarator asymmetric terms are used.
         LOGICAL                                 :: l_asym

!>  Number of radial grid points.
         INTEGER                                 :: ns
!>  Number of poloidal modes.
         INTEGER                                 :: mpol
!>  Number of toroidal modes.
         INTEGER                                 :: ntor
!>  Toroidal modes.
         INTEGER, DIMENSION(:), POINTER          :: tor_modes => null()
!>  Number of field periods.
         INTEGER                                 :: nfp

!>  Minimum pressure.
         REAL (rprec)                            :: p_min
!>  Maximum pressure.
         REAL (rprec)                            :: p_max
!>  Cosine components of the pressure on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: pmnch => null()
!>  Sine components of the pressure on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: pmnsh => null()

!>  Normalization factor for magnetic field.
         REAL (rprec)                            :: b_factor

!>  Cosine components of B^s on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: bsupsmnch => null()
!>  Sine components of B^s on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: bsupsmnsh => null()
!>  Cosine components of B^u on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: bsupumnch => null()
!>  Sine components of B^u on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: bsupumnsh => null()
!>  Cosine components of B^v on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: bsupvmnch => null()
!>  Sine components of B^v on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: bsupvmnsh => null()

!>  Cosine components of B_s on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: bsubsmnch => null()
!>  Sine components of B_s on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: bsubsmnsh => null()
!>  Cosine components of B_u on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: bsubumnch => null()
!>  Sine components of B_u on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: bsubumnsh => null()
!>  Cosine components of B_v on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: bsubvmnch => null()
!>  Sine components of B_v on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: bsubvmnsh => null()

!>  Cosine components of jK^s on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jksupsmncf => null()
!>  Sine components of jK^s on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jksupsmnsf => null()
!>  Cosine components of jK^u on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jksupumncf => null()
!>  Sine components of jK^u on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jksupumnsf => null()
!>  Cosine components of jK^v on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jksupvmncf => null()
!>  Sine components of jK^v on the half mesh.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jksupvmnsf => null()
      CONTAINS
         FINAL     :: siesta_context_destruct
         PROCEDURE :: read => siesta_context_read
      END TYPE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref siesta_context_class object.
!>
!>  Allocates memory and initializes a @ref siesta_context_class object.
!>
!>  @param[in] restart_file_name Filename of the SIESTA restart file.
!>  @returns A pointer to a constructed @ref siesta_context_class object.
!-------------------------------------------------------------------------------
      FUNCTION siesta_context_construct(restart_file_name)
      USE ezcdf
      USE file_opts, only: path_length

      IMPLICIT NONE

!  Declare Arguments
      CLASS (siesta_context_class), POINTER :: siesta_context_construct
      CHARACTER (len=*), INTENT(in)         :: restart_file_name

!  local variables
      INTEGER                               :: ncid
      INTEGER                               :: ns
      INTEGER                               :: mpol
      INTEGER                               :: ntor
      INTEGER                               :: status
      INTEGER                               :: varid
      INTEGER                               :: i
      INTEGER                               :: flags
      REAL (rprec)                          :: start_time

!  local parameters
      INTEGER, PARAMETER                    :: l_asym_flag = 31

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(siesta_context_construct)

      status = nf90_open(TRIM(restart_file_name), NF90_NOWRITE, ncid)
      IF (status .ne. 0) THEN
         STOP 'Failed to open SIESTA Restrat file.'
      END IF

      status = nf90_inq_varid(ncid, vn_nsin, varid)
      status = nf90_get_var(ncid, varid, ns)
      status = nf90_inq_varid(ncid, vn_mpolin, varid)
      status = nf90_get_var(ncid, varid, mpol)
      status = nf90_inq_varid(ncid, vn_ntorin, varid)
      status = nf90_get_var(ncid, varid, ntor)

      siesta_context_construct%ns = ns
      siesta_context_construct%mpol = mpol
      siesta_context_construct%ntor = ntor

      ALLOCATE(siesta_context_construct%tor_modes(-ntor:ntor))
      status = nf90_inq_varid(ncid, vn_tor_modes, varid)
      status = nf90_get_var(ncid, varid,                                       &
     &                      siesta_context_construct%tor_modes)

      status = nf90_inq_varid(ncid, vn_nfpin, varid)
      status = nf90_get_var(ncid, varid, siesta_context_construct%nfp)

      status = nf90_inq_varid(ncid, vn_flags, varid)
      status = nf90_get_var(ncid, varid, flags)
      siesta_context_construct%l_asym = BTEST(flags, l_asym_flag)

!  Pressure
      status = nf90_inq_varid(ncid, vn_p_min, varid)
      status = nf90_get_var(ncid, varid, siesta_context_construct%p_min)
      status = nf90_inq_varid(ncid, vn_p_max, varid)
      status = nf90_get_var(ncid, varid, siesta_context_construct%p_max)

      ALLOCATE(siesta_context_construct%pmnch(0:mpol,-ntor:ntor,ns))
      status = nf90_inq_varid(ncid, vn_pmnc, varid)
      status = nf90_get_var(ncid, varid, siesta_context_construct%pmnch)

!  Magnetic field
      status = nf90_inq_varid(ncid, vn_b_factor, varid)
      status = nf90_get_var(ncid, varid,                                       &
     &                      siesta_context_construct%b_factor)

!  Bsup*
      ALLOCATE(siesta_context_construct%bsupsmnsh(0:mpol,-ntor:ntor,ns))
      status = nf90_inq_varid(ncid, vn_bsupsmns, varid)
      status = nf90_get_var(ncid, varid,                                       &
     &                      siesta_context_construct%bsupsmnsh)
      ALLOCATE(siesta_context_construct%bsupumnch(0:mpol,-ntor:ntor,ns))
      status = nf90_inq_varid(ncid, vn_bsupumnc, varid)
      status = nf90_get_var(ncid, varid,                                       &
     &                      siesta_context_construct%bsupumnch)
      ALLOCATE(siesta_context_construct%bsupvmnch(0:mpol,-ntor:ntor,ns))
      status = nf90_inq_varid(ncid, vn_bsupvmnc, varid)
      status = nf90_get_var(ncid, varid,                                       &
     &                      siesta_context_construct%bsupvmnch)

!  Bsub*
      ALLOCATE(siesta_context_construct%bsubsmnsh(0:mpol,-ntor:ntor,ns))
      status = nf90_inq_varid(ncid, vn_bsubsmns, varid)
      status = nf90_get_var(ncid, varid,                                       &
     &                      siesta_context_construct%bsubsmnsh)
      ALLOCATE(siesta_context_construct%bsubumnch(0:mpol,-ntor:ntor,ns))
      status = nf90_inq_varid(ncid, vn_bsubumnc, varid)
      status = nf90_get_var(ncid, varid,                                       &
     &                      siesta_context_construct%bsubumnch)
      ALLOCATE(siesta_context_construct%bsubvmnch(0:mpol,-ntor:ntor,ns))
      status = nf90_inq_varid(ncid, vn_bsubvmnc, varid)
      status = nf90_get_var(ncid, varid,                                       &
     &                      siesta_context_construct%bsubvmnch)

!  JKsup*
      ALLOCATE(siesta_context_construct%jksupsmnsf(0:mpol,-ntor:ntor,          &
     &                                             ns))
      status = nf90_inq_varid(ncid, vn_jksupsmns, varid)
      status = nf90_get_var(ncid, varid,                                       &
     &                      siesta_context_construct%jksupsmnsf)
      ALLOCATE(siesta_context_construct%jksupumncf(0:mpol,-ntor:ntor,          &
     &                                             ns))
      status = nf90_inq_varid(ncid, vn_jksupumnc, varid)
      status = nf90_get_var(ncid, varid,                                       &
     &                      siesta_context_construct%jksupumncf)
      ALLOCATE(siesta_context_construct%jksupvmncf(0:mpol,-ntor:ntor,          &
     &                                             ns))
      status = nf90_inq_varid(ncid, vn_jksupvmnc, varid)
      status = nf90_get_var(ncid, varid,                                       &
     &                      siesta_context_construct%jksupvmncf)

      IF (siesta_context_construct%l_asym) THEN
!  Pressure
         ALLOCATE(siesta_context_construct%pmnsh(0:mpol,-ntor:ntor,ns))
         status = nf90_inq_varid(ncid, vn_pmns, varid)
         status = nf90_get_var(ncid, varid,                                    &
     &                         siesta_context_construct%pmnsh)

!  Bsup*
         ALLOCATE(siesta_context_construct%bsupsmnch(0:mpol,-ntor:ntor,        &
     &                                               ns))
         status = nf90_inq_varid(ncid, vn_bsupsmnc, varid)
         status = nf90_get_var(ncid, varid,                                    &
     &                         siesta_context_construct%bsupsmnch)
         ALLOCATE(siesta_context_construct%bsupumnsh(0:mpol,-ntor:ntor,        &
     &                                               ns))
         status = nf90_inq_varid(ncid, vn_bsupumns, varid)
         status = nf90_get_var(ncid, varid,                                    &
     &                         siesta_context_construct%bsupumnsh)
         ALLOCATE(siesta_context_construct%bsupvmnsh(0:mpol,-ntor:ntor,        &
     &                                               ns))
         status = nf90_inq_varid(ncid, vn_bsupvmns, varid)
         status = nf90_get_var(ncid, varid,                                    &
     &                         siesta_context_construct%bsupvmnsh)

!  Bsub*
         ALLOCATE(siesta_context_construct%bsubsmnch(0:mpol,-ntor:ntor,        &
     &                                               ns))
         status = nf90_inq_varid(ncid, vn_bsupsmnc, varid)
         status = nf90_get_var(ncid, varid,                                    &
     &                         siesta_context_construct%bsupsmnch)
         ALLOCATE(siesta_context_construct%bsubumnsh(0:mpol,-ntor:ntor,        &
     &                                               ns))
         status = nf90_inq_varid(ncid, vn_bsubumns, varid)
         status = nf90_get_var(ncid, varid,                                    &
     &                         siesta_context_construct%bsubumnsh)
         ALLOCATE(siesta_context_construct%bsubvmnsh(0:mpol,-ntor:ntor,        &
     &                                               ns))
         status = nf90_inq_varid(ncid, vn_bsubvmns, varid)
         status = nf90_get_var(ncid, varid,                                    &
     &                         siesta_context_construct%bsubvmnsh)

!  JKsup*
         ALLOCATE(siesta_context_construct%jksupsmncf(0:mpol,-ntor:ntor,       &
     &                                                ns))
         status = nf90_inq_varid(ncid, vn_jksupsmnc, varid)
         status = nf90_get_var(ncid, varid,                                    &
     &                         siesta_context_construct%jksupsmncf)
         ALLOCATE(siesta_context_construct%jksupumnsf(0:mpol,-ntor:ntor,       &
     &                                                ns))
         status = nf90_inq_varid(ncid, vn_jksupumns, varid)
         status = nf90_get_var(ncid, varid,                                    &
     &                         siesta_context_construct%jksupumnsf)
         ALLOCATE(siesta_context_construct%jksupvmnsf(0:mpol,-ntor:ntor,       &
     &                                                ns))
         status = nf90_inq_varid(ncid, vn_jksupvmns, varid)
         status = nf90_get_var(ncid, varid,                                    &
     &                         siesta_context_construct%jksupvmnsf)
      END IF

      status = nf90_close(ncid)

      CALL profiler_set_stop_time('siesta_context_construct',                  &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref siesta_context_class object.
!>
!>  Deallocates memory and uninitializes a @ref siesta_context_class object.
!>
!>  @param[inout] this A @ref siesta_context_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_context_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_context_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%tor_modes)) THEN
         DEALLOCATE(this%tor_modes)
         this%tor_modes => null()
      END IF

      IF (ASSOCIATED(this%pmnch)) THEN
         DEALLOCATE(this%pmnch)
         this%pmnch => null()
      END IF

      IF (ASSOCIATED(this%pmnsh)) THEN
         DEALLOCATE(this%pmnsh)
         this%pmnsh => null()
      END IF

      IF (ASSOCIATED(this%bsupsmnsh)) THEN
         DEALLOCATE(this%bsupsmnsh)
         this%bsupsmnsh => null()
      END IF

      IF (ASSOCIATED(this%bsupsmnch)) THEN
         DEALLOCATE(this%bsupsmnch)
         this%bsupsmnch => null()
      END IF

      IF (ASSOCIATED(this%bsupumnsh)) THEN
         DEALLOCATE(this%bsupumnsh)
         this%bsupumnsh => null()
      END IF

      IF (ASSOCIATED(this%bsupumnch)) THEN
         DEALLOCATE(this%bsupumnch)
         this%bsupumnch => null()
      END IF

      IF (ASSOCIATED(this%bsupvmnsh)) THEN
         DEALLOCATE(this%bsupvmnsh)
         this%bsupvmnsh => null()
      END IF

      IF (ASSOCIATED(this%bsupvmnch)) THEN
         DEALLOCATE(this%bsupvmnch)
         this%bsupvmnch => null()
      END IF

      IF (ASSOCIATED(this%bsubsmnsh)) THEN
         DEALLOCATE(this%bsubsmnsh)
         this%bsubsmnsh => null()
      END IF

      IF (ASSOCIATED(this%bsubsmnch)) THEN
         DEALLOCATE(this%bsubsmnch)
         this%bsubsmnch => null()
      END IF

      IF (ASSOCIATED(this%bsubumnsh)) THEN
         DEALLOCATE(this%bsubumnsh)
         this%bsubumnsh => null()
      END IF

      IF (ASSOCIATED(this%bsubumnch)) THEN
         DEALLOCATE(this%bsubumnch)
         this%bsubumnch => null()
      END IF

      IF (ASSOCIATED(this%bsubvmnsh)) THEN
         DEALLOCATE(this%bsubvmnsh)
         this%bsubvmnsh => null()
      END IF

      IF (ASSOCIATED(this%bsubvmnch)) THEN
         DEALLOCATE(this%bsubvmnch)
         this%bsubvmnch => null()
      END IF

      IF (ASSOCIATED(this%jksupsmnsf)) THEN
         DEALLOCATE(this%jksupsmnsf)
         this%jksupsmnsf => null()
      END IF

      IF (ASSOCIATED(this%jksupsmncf)) THEN
         DEALLOCATE(this%jksupsmncf)
         this%jksupsmncf => null()
      END IF

      IF (ASSOCIATED(this%jksupumnsf)) THEN
         DEALLOCATE(this%jksupumnsf)
         this%jksupumnsf => null()
      END IF

      IF (ASSOCIATED(this%jksupumncf)) THEN
         DEALLOCATE(this%jksupumncf)
         this%jksupumncf => null()
      END IF

      IF (ASSOCIATED(this%jksupvmnsf)) THEN
         DEALLOCATE(this%jksupvmnsf)
         this%jksupvmnsf => null()
      END IF

      IF (ASSOCIATED(this%jksupvmncf)) THEN
         DEALLOCATE(this%jksupvmncf)
         this%jksupvmncf => null()
      END IF

      END SUBROUTINE

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Read a restart file.
!>
!>  Loads values from a new restart file.
!>
!>  @param[inout] this              A @ref siesta_context_class instance.
!>  @param[in]    restart_file_name Filename of the SIESTA restart file.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_context_read(this, restart_file_name)
      USE ezcdf
      USE file_opts, only: path_length

      IMPLICIT NONE

!  Declare Arguments
      CLASS (siesta_context_class), INTENT(inout) :: this
      CHARACTER (len=*), INTENT(in)               :: restart_file_name

!  local variables
      INTEGER                                     :: ncid
      INTEGER                                     :: status
      INTEGER                                     :: varid
      INTEGER                                     :: i
      INTEGER                                     :: flags
      CHARACTER (len=path_length)                 :: wout_file_name
      REAL (rprec)                                :: start_time

!  local parameters
      INTEGER, PARAMETER                          :: l_asym_flag = 31

!  Start of executable code
      start_time = profiler_get_start_time()

      status = nf90_open(TRIM(restart_file_name), NF90_NOWRITE, ncid)
      IF (status .ne. 0) THEN
         STOP 'Failed to open SIESTA Restrat file.'
      END IF

      status = nf90_inq_varid(ncid, vn_nsin, varid)
      status = nf90_get_var(ncid, varid, this%ns)
      status = nf90_inq_varid(ncid, vn_mpolin, varid)
      status = nf90_get_var(ncid, varid, this%mpol)
      status = nf90_inq_varid(ncid, vn_ntorin, varid)
      status = nf90_get_var(ncid, varid, this%ntor)

      status = nf90_inq_varid(ncid, vn_tor_modes, varid)
      status = nf90_get_var(ncid, varid, this%tor_modes)

      status = nf90_inq_varid(ncid, vn_nfpin, varid)
      status = nf90_get_var(ncid, varid, this%nfp)

      status = nf90_inq_varid(ncid, vn_flags, varid)
      status = nf90_get_var(ncid, varid, flags)
      IF (this%l_asym .neqv. BTEST(flags, l_asym_flag)) THEN
         STOP 'Restart file changed parity.'
      END IF

!  Pressure
      status = nf90_inq_varid(ncid, vn_p_min, varid)
      status = nf90_get_var(ncid, varid, this%p_min)
      status = nf90_inq_varid(ncid, vn_p_max, varid)
      status = nf90_get_var(ncid, varid, this%p_max)

      status = nf90_inq_varid(ncid, vn_pmnc, varid)
      status = nf90_get_var(ncid, varid, this%pmnch)

!  Magnetic field
      status = nf90_inq_varid(ncid, vn_b_factor, varid)
      status = nf90_get_var(ncid, varid, this%b_factor)

!  Bsup*
      status = nf90_inq_varid(ncid, vn_bsupsmns, varid)
      status = nf90_get_var(ncid, varid, this%bsupsmnsh)
      status = nf90_inq_varid(ncid, vn_bsupumnc, varid)
      status = nf90_get_var(ncid, varid, this%bsupumnch)
      status = nf90_inq_varid(ncid, vn_bsupvmnc, varid)
      status = nf90_get_var(ncid, varid, this%bsupvmnch)

!  Bsub*
      status = nf90_inq_varid(ncid, vn_bsubsmns, varid)
      status = nf90_get_var(ncid, varid, this%bsubsmnsh)
      status = nf90_inq_varid(ncid, vn_bsubsmns, varid)
      status = nf90_get_var(ncid, varid, this%bsubumnch)
      status = nf90_inq_varid(ncid, vn_bsubsmns, varid)
      status = nf90_get_var(ncid, varid, this%bsubvmnch)

!  JKsup*
      status = nf90_inq_varid(ncid, vn_jksupsmns, varid)
      status = nf90_get_var(ncid, varid, this%jksupsmnsf)
      status = nf90_inq_varid(ncid, vn_jksupsmns, varid)
      status = nf90_get_var(ncid, varid, this%jksupumncf)
      status = nf90_inq_varid(ncid, vn_jksupsmns, varid)
      status = nf90_get_var(ncid, varid, this%jksupvmncf)

      IF (this%l_asym) THEN
!  Pressure
         status = nf90_inq_varid(ncid, vn_pmns, varid)
         status = nf90_get_var(ncid, varid, this%pmnsh)

!  Bsup*
         status = nf90_inq_varid(ncid, vn_bsupsmnc, varid)
         status = nf90_get_var(ncid, varid, this%bsupsmnch)
         status = nf90_inq_varid(ncid, vn_bsupumns, varid)
         status = nf90_get_var(ncid, varid, this%bsupumnsh)
         status = nf90_inq_varid(ncid, vn_bsupvmns, varid)
         status = nf90_get_var(ncid, varid, this%bsupvmnsh)

!  Bsub*
         status = nf90_inq_varid(ncid, vn_bsupsmnc, varid)
         status = nf90_get_var(ncid, varid, this%bsubsmnch)
         status = nf90_inq_varid(ncid, vn_bsubumns, varid)
         status = nf90_get_var(ncid, varid, this%bsubumnsh)
         status = nf90_inq_varid(ncid, vn_bsubvmns, varid)
         status = nf90_get_var(ncid, varid, this%bsubvmnsh)

!  JKsup*
         status = nf90_inq_varid(ncid, vn_jksupsmnc, varid)
         status = nf90_get_var(ncid, varid, this%jksupsmncf)
         status = nf90_inq_varid(ncid, vn_jksupumns, varid)
         status = nf90_get_var(ncid, varid, this%jksupumnsf)
         status = nf90_inq_varid(ncid, vn_jksupvmns, varid)
         status = nf90_get_var(ncid, varid, this%jksupvmnsf)
      END IF

      status = nf90_close(ncid)

      CALL profiler_set_stop_time('siesta_context_read', start_time)

      END SUBROUTINE

      END MODULE

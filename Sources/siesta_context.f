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
      TYPE siesta_context_class
!>  Flag indicating that stellarator asymmetric terms are used.
         LOGICAL                                 :: l_asym

!>  Number of radial grid points.
         INTEGER                                 :: ns
!>  Number of poloidal modes.
         INTEGER                                 :: mpol
!>  Number of toroidal modes.
         INTEGER                                 :: ntor
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
      TYPE (siesta_context_class), POINTER :: siesta_context_construct
      CHARACTER (len=*), INTENT(in)        :: restart_file_name

!  local variables
      INTEGER                              :: ncid
      INTEGER                              :: ns
      INTEGER                              :: mpol
      INTEGER                              :: ntor
      INTEGER                              :: status
      INTEGER                              :: i
      INTEGER                              :: flags
      REAL (rprec)                         :: start_time

!  local parameters
      INTEGER, PARAMETER                   :: l_asym_flag = 31

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(siesta_context_construct)

      CALL cdf_open(ncid, TRIM(restart_file_name), 'r', status)

      CALL cdf_read(ncid, vn_nsin, ns)
      CALL cdf_read(ncid, vn_mpolin, mpol)
      CALL cdf_read(ncid, vn_ntorin, ntor)

      siesta_context_construct%ns = ns
      siesta_context_construct%mpol = mpol
      siesta_context_construct%ntor = ntor

      CALL cdf_read(ncid, vn_nfpin, siesta_context_construct%nfp)

      CALL cdf_read(ncid, vn_flags, flags)
      siesta_context_construct%l_asym = BTEST(flags, l_asym_flag)

!  Pressure
      CALL cdf_read(ncid, vn_p_min, siesta_context_construct%p_min)
      CALL cdf_read(ncid, vn_p_max, siesta_context_construct%p_max)

      ALLOCATE(siesta_context_construct%pmnch(0:mpol,-ntor:ntor,ns))
      CALL cdf_read(ncid, vn_pmnc, siesta_context_construct%pmnch)

!  Magnetic field
      CALL cdf_read(ncid, vn_b_factor,                                         &
     &              siesta_context_construct%b_factor)

!  Bsup*
      ALLOCATE(siesta_context_construct%bsupsmnsh(0:mpol,-ntor:ntor,ns))
      CALL cdf_read(ncid, vn_bsupsmns,                                         &
     &              siesta_context_construct%bsupsmnsh)
      ALLOCATE(siesta_context_construct%bsupumnch(0:mpol,-ntor:ntor,ns))
      CALL cdf_read(ncid, vn_bsupumnc,                                         &
     &              siesta_context_construct%bsupumnch)
      ALLOCATE(siesta_context_construct%bsupvmnch(0:mpol,-ntor:ntor,ns))
      CALL cdf_read(ncid, vn_bsupvmnc,                                         &
     &              siesta_context_construct%bsupvmnch)

!  Bsub*
      ALLOCATE(siesta_context_construct%bsubsmnsh(0:mpol,-ntor:ntor,ns))
      CALL cdf_read(ncid, vn_bsubsmns,                                         &
     &              siesta_context_construct%bsubsmnsh)
      ALLOCATE(siesta_context_construct%bsubumnch(0:mpol,-ntor:ntor,ns))
      CALL cdf_read(ncid, vn_bsubumnc,                                         &
     &              siesta_context_construct%bsubumnch)
      ALLOCATE(siesta_context_construct%bsubvmnch(0:mpol,-ntor:ntor,ns))
      CALL cdf_read(ncid, vn_bsubvmnc,                                         &
     &              siesta_context_construct%bsubvmnch)

!  JKsup*
      ALLOCATE(siesta_context_construct%jksupsmnsf(0:mpol,-ntor:ntor,          &
     &                                             ns))
      CALL cdf_read(ncid, vn_jksupsmns,                                        &
     &              siesta_context_construct%jksupsmnsf)
      ALLOCATE(siesta_context_construct%jksupumncf(0:mpol,-ntor:ntor,          &
     &                                             ns))
      CALL cdf_read(ncid, vn_jksupumnc,                                        &
     &              siesta_context_construct%jksupumncf)
      ALLOCATE(siesta_context_construct%jksupvmncf(0:mpol,-ntor:ntor,          &
     &                                             ns))
      CALL cdf_read(ncid, vn_jksupvmnc,                                        &
     &              siesta_context_construct%jksupvmncf)

      IF (siesta_context_construct%l_asym) THEN
!  Pressure
         ALLOCATE(siesta_context_construct%pmnsh(0:mpol,-ntor:ntor,ns))
         CALL cdf_read(ncid, vn_pmns, siesta_context_construct%pmnsh)

!  Bsup*
         ALLOCATE(siesta_context_construct%bsupsmnch(0:mpol,-ntor:ntor,        &
     &                                               ns))
         CALL cdf_read(ncid, vn_bsupsmnc,                                      &
     &                 siesta_context_construct%bsupsmnch)
         ALLOCATE(siesta_context_construct%bsupumnsh(0:mpol,-ntor:ntor,        &
     &                                               ns))
         CALL cdf_read(ncid, vn_bsupumns,                                      &
     &                 siesta_context_construct%bsupumnsh)
         ALLOCATE(siesta_context_construct%bsupvmnsh(0:mpol,-ntor:ntor,        &
     &                                               ns))
         CALL cdf_read(ncid, vn_bsupvmns,                                      &
     &                 siesta_context_construct%bsupvmnsh)

!  Bsub*
         ALLOCATE(siesta_context_construct%bsubsmnch(0:mpol,-ntor:ntor,        &
     &                                               ns))
         CALL cdf_read(ncid, vn_bsubsmnc,                                      &
     &                 siesta_context_construct%bsubsmnch)
         ALLOCATE(siesta_context_construct%bsubumnsh(0:mpol,-ntor:ntor,        &
     &                                               ns))
         CALL cdf_read(ncid, vn_bsubumns,                                      &
     &                 siesta_context_construct%bsubumnsh)
         ALLOCATE(siesta_context_construct%bsubvmnsh(0:mpol,-ntor:ntor,        &
     &                                               ns))
         CALL cdf_read(ncid, vn_bsubvmns,                                      &
     &                 siesta_context_construct%bsubvmnsh)

!  JKsup*
         ALLOCATE(siesta_context_construct%jksupsmncf(0:mpol,-ntor:ntor,       &
     &                                                ns))
         CALL cdf_read(ncid, vn_jksupsmnc,                                     &
     &                 siesta_context_construct%jksupsmncf)
         ALLOCATE(siesta_context_construct%jksupumnsf(0:mpol,-ntor:ntor,       &
     &                                                ns))
         CALL cdf_read(ncid, vn_jksupumns,                                     &
     &                 siesta_context_construct%jksupumnsf)
         ALLOCATE(siesta_context_construct%jksupvmnsf(0:mpol,-ntor:ntor,       &
     &                                                ns))
         CALL cdf_read(ncid, vn_jksupvmns,                                     &
     &                 siesta_context_construct%jksupvmnsf)
      END IF

      CALL cdf_close(ncid)

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
      TYPE (siesta_context_class), POINTER :: this

!  Start of executable code
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

      DEALLOCATE(this)

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
      TYPE (siesta_context_class), POINTER :: this
      CHARACTER (len=*), INTENT(in)        :: restart_file_name

!  local variables
      INTEGER                              :: ncid
      INTEGER                              :: status
      INTEGER                              :: i
      INTEGER                              :: flags
      CHARACTER (len=path_length)          :: wout_file_name
      REAL (rprec)                         :: start_time

!  local parameters
      INTEGER, PARAMETER                   :: l_asym_flag = 31

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL cdf_open(ncid, TRIM(restart_file_name), 'r', status)
      IF (status .ne. 0) STOP 'Failed'

      CALL cdf_read(ncid, vn_nsin, this%ns)
      CALL cdf_read(ncid, vn_mpolin, this%mpol)
      CALL cdf_read(ncid, vn_ntorin, this%ntor)

      CALL cdf_read(ncid, vn_nfpin, this%nfp)

      CALL cdf_read(ncid, vn_flags, flags)
      IF (this%l_asym .neqv. BTEST(flags, l_asym_flag)) THEN
         STOP 'Restart file changed parity.'
      END IF

!  Pressure
      CALL cdf_read(ncid, vn_p_min, this%p_min)
      CALL cdf_read(ncid, vn_p_max, this%p_max)

      CALL cdf_read(ncid, vn_pmnc, this%pmnch)

!  Magnetic field
      CALL cdf_read(ncid, vn_b_factor, this%b_factor)

!  Bsup*
      CALL cdf_read(ncid, vn_bsupsmns, this%bsupsmnsh)
      CALL cdf_read(ncid, vn_bsupumnc, this%bsupumnch)
      CALL cdf_read(ncid, vn_bsupvmnc, this%bsupvmnch)

!  Bsub*
      CALL cdf_read(ncid, vn_bsubsmns, this%bsubsmnsh)
      CALL cdf_read(ncid, vn_bsubumnc, this%bsubumnch)
      CALL cdf_read(ncid, vn_bsubvmnc, this%bsubvmnch)

!  JKsup*
      CALL cdf_read(ncid, vn_jksupsmns, this%jksupsmnsf)
      CALL cdf_read(ncid, vn_jksupumnc, this%jksupumncf)
      CALL cdf_read(ncid, vn_jksupvmnc, this%jksupvmncf)

      IF (this%l_asym) THEN
!  Pressure
         CALL cdf_read(ncid, vn_pmns, this%pmnsh)

!  Bsup*
         CALL cdf_read(ncid, vn_bsupsmnc, this%bsupsmnch)
         CALL cdf_read(ncid, vn_bsupumns, this%bsupumnsh)
         CALL cdf_read(ncid, vn_bsupvmns, this%bsupvmnsh)

!  Bsub*
         CALL cdf_read(ncid, vn_bsubsmnc, this%bsubsmnch)
         CALL cdf_read(ncid, vn_bsubumns, this%bsubumnsh)
         CALL cdf_read(ncid, vn_bsubvmns, this%bsubvmnsh)

!  JKsup*
         CALL cdf_read(ncid, vn_jksupsmnc, this%jksupsmncf)
         CALL cdf_read(ncid, vn_jksupumns, this%jksupumnsf)
         CALL cdf_read(ncid, vn_jksupvmns, this%jksupvmnsf)
      END IF

      CALL cdf_close(ncid)

      CALL profiler_set_stop_time('siesta_context_read', start_time)

      END SUBROUTINE

      END MODULE

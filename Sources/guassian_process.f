!*******************************************************************************
!>  @file guassian_process.f
!>  @brief Contains module @ref guassian_process.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref guassian_process_class. The
!>  guassian_process contains code to compuet guassian process profiles.
!*******************************************************************************
      MODULE guassian_process
      USE signal
      USE pprofile_T
      USE v3fit_params

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) gaussian process base class
!  2) gaussian process pointer type
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a gaussian process.
!-------------------------------------------------------------------------------
      TYPE gaussp_class
!>  Type descriptor of the gaussian procees type.
!>  @par Possible values are:
!>  * @ref gaussp_no_type
!>  * @ref gaussp_sxrem_type
!>  * @ref gaussp_te_type
!>  * @ref gaussp_ne_type
!  Keep for now, but this might not be needed later.
         INTEGER :: flags = model_state_all_off

!>  Array of @ref signal_pointer
         TYPE (signal_pointer), DIMENSION(:), POINTER ::                       &
     &      signals => null()

!>  Gradient ascent convergence stop.
         REAL (rprec)                          :: tolerance

!>  Matrix of signal profile.
         REAL (rprec), DIMENSION(:,:), POINTER :: kls => null()
!>  Matrix of signal signal.
         REAL (rprec), DIMENSION(:,:), POINTER :: kll => null()
!>  Work matrix for the cholesky factorization.
         REAL (rprec), DIMENSION(:,:), POINTER :: work => null()
!>  Positive factor to add to the diagonal entries of the matrix before
!>  factoring.
         REAL (rprec)                          :: cholesky_fac

!>  Profile index for sxrem profiles.
         INTEGER  :: profile_index = -1

!>  Pointer to the profile af array.
         REAL (rprec), DIMENSION(:), POINTER   :: fpoints => null()

!>  Array of hyper parameters.
         TYPE (param_pointer), DIMENSION(:), POINTER ::                        &
     &      params => null()

!>  Log file input output unit.
         INTEGER                               :: iou
      END TYPE

!-------------------------------------------------------------------------------
!>  Pointer to a gaussian process object. Used for creating arrays of gaussian
!>  process pointers. This is needed because fortran does not allow arrays of
!>  pointers directly.
!-------------------------------------------------------------------------------
      TYPE gaussp_class_pointer
!>  Pointer to a @ref gaussp_class. Used for building arrays of
!>  @ref gaussp_class objects.
         TYPE (gaussp_class), POINTER :: p => null()
      END TYPE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref gaussp_class.
!>
!>  Allocates memory and initializes a @ref gaussp_class object. This just
!>  allocates the @ref gaussp_class::signals, @ref gaussp_class::a
!>  array, and matrixices for the GP computataion.  The indices of these arrays
!>  are set by @ref gaussp_set_signal, and the matrices are calculated in
!>  @ref gaussp_get_modeled_signal
!>
!>  @param[in] a_model       A @ref model::model_class instance.
!>  @param[in] n_signals     Number of signals in this gaussian process.
!>  @param[in] gaussp_type   The type of profile modeled by the gaussian
!>                           process.
!>  @param[in] profile_index Index of the profile of the sxrem profile.
!>  @param[in] vrnc          Variance values for the hyper parameters.
!>  @param[in] tolerance     Convergence value to maximize the log evidence.
!>  @param[in] choleskey_fac Value added to diagnoal element to ensure K is PSD.
!>  @returns A pointer to a constructed @ref gaussp_class object.
!-------------------------------------------------------------------------------
      FUNCTION gaussp_construct(a_model, n_signals, gaussp_type,               &
     &                          profile_index, vrnc, tolerance,                &
     &                          cholesky_fac)
      USE data_parameters
      USE safe_open_mod

      IMPLICIT NONE

!  Declare Arguments
      TYPE (gaussp_class), POINTER         :: gaussp_construct
      CLASS (model_class), INTENT(in)      :: a_model
      INTEGER, INTENT(in)                  :: n_signals
      INTEGER, INTENT(in)                  :: profile_index
      CHARACTER(len=*), INTENT(in)         :: gaussp_type
      REAL (rprec), DIMENSION(:)           :: vrnc
      REAL (rprec)                         :: tolerance
      REAL (rprec)                         :: cholesky_fac

!  Local Variables
      INTEGER                              :: i
      INTEGER                              :: n_points
      INTEGER                              :: n_params
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: indices
      CHARACTER (len=data_name_length)     :: param_name
      INTEGER                              :: status
      CHARACTER (len=15)                   :: log_file
      REAL (rprec)                         :: start_time

!  Local Parameters
      CHARACTER (len=*), DIMENSION(2), PARAMETER :: range_type =               &
     &   'infinity'
      INTEGER, DIMENSION(2,2), PARAMETER    :: range_indices = 0
      REAL (rprec), DIMENSION(2), PARAMETER :: range_value = 0.0

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(gaussp_construct)

      SELECT CASE (TRIM(gaussp_type))

         CASE ('sxrem')
            n_params =                                                         &
     &         a_model%get_gp_sxrem_num_hyper_param(profile_index)
            gaussp_construct%flags = IBSET(gaussp_construct%flags,             &
     &                                     model_state_sxrem_flag +            &
     &                                     profile_index - 1)
            gaussp_construct%profile_index = profile_index
            gaussp_construct%fpoints =>                                        &
     &         a_model%get_sxrem_af(profile_index)
            param_name = 'pp_sxrem_b_a'
            ALLOCATE(indices(data_max_indices,n_params))
            indices = 0
            DO i = 1, n_params
               indices(1,i) = profile_index
               indices(2,i) = i - 1
            END DO
            gaussp_construct%iou = 0
            WRITE (log_file,1001) 'sxrem', profile_index

         CASE ('te')
            n_params = a_model%get_gp_te_num_hyper_param()
            gaussp_construct%flags = IBSET(gaussp_construct%flags,             &
     &                                     model_state_te_flag)
            gaussp_construct%fpoints => a_model%get_te_af()
            param_name = 'pp_te_b'
            ALLOCATE(indices(data_max_indices,n_params))
            indices = 0
            DO i = 1, n_params
               indices(1,i) = i - 1
            END DO
            gaussp_construct%iou = 0
            WRITE (log_file,1000) 'te'

         CASE ('ti')
            n_params = a_model%get_gp_te_num_hyper_param()
            gaussp_construct%flags = IBSET(gaussp_construct%flags,             &
     &                                     model_state_ti_flag)
            gaussp_construct%fpoints => a_model%get_te_af()
            param_name = 'pp_te_b'
            ALLOCATE(indices(data_max_indices,n_params))
            indices = 0
            DO i = 1, n_params
               indices(1,i) = i - 1
            END DO
            gaussp_construct%iou = 0
            WRITE (log_file,1000) 'ti'

         CASE ('ne')
            n_params = a_model%get_gp_ne_num_hyper_param()
            gaussp_construct%flags = IBSET(gaussp_construct%flags,             &
     &                                     model_state_ne_flag)
            gaussp_construct%fpoints => a_model%get_ne_af()
            param_name = 'pp_ne_b'
            ALLOCATE(indices(data_max_indices,n_params))
            indices = 0
            DO i = 1, n_params
               indices(1,i) = i - 1
            END DO
            gaussp_construct%iou = 0
            WRITE (log_file,1000) 'ne'

      END SELECT

      CALL safe_open(gaussp_construct%iou, status, TRIM(log_file),             &
     &               'replace', 'formatted', delim_in='none')

      n_points = SIZE(gaussp_construct%fpoints)

      ALLOCATE(gaussp_construct%signals(n_signals))

      ALLOCATE(gaussp_construct%kls(n_signals,n_points))
      ALLOCATE(gaussp_construct%kll(n_signals,n_signals))

      ALLOCATE(gaussp_construct%work(n_signals,n_points + 1))

      gaussp_construct%tolerance = tolerance
      gaussp_construct%cholesky_fac = cholesky_fac

      ALLOCATE(gaussp_construct%params(n_params))
      DO i = 1, n_params
!>  @todo FIXME: The guassian process kernels should know the correct ranges.
!>               Query those instead. For now set all to infinity.
         gaussp_construct%params(i)%p =>                                       &
     &      param_construct(a_model, param_name, indices(:,i), vrnc(i),        &
     &                      range_type, range_indices, range_value,            &
     &                      n_signals, n_params)
      END DO

      DEALLOCATE(indices)

      CALL profiler_set_stop_time('gaussp_construct', start_time)

1000  FORMAT(a,'_gp.log')
1001  FORMAT(a,'_',i0.2,'_gp.log')

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref gaussp_class object.
!>
!>  Deallocates memory and uninitializes a @ref gaussp_class object.
!>
!>  @param[inout] this A @ref gaussp_class instance.
!>  @note The destructors of the signals in the signal array are not called her
!>  because the gaussian process signals do not own the memory for those signals.
!-------------------------------------------------------------------------------
      SUBROUTINE gaussp_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (gaussp_class), POINTER :: this

!  Local Variables
      INTEGER                       :: i

!  Start of executable code

!  Null all pointers in the signals array. Do not deallocate the signals in the
!  array since this object is not the owner.
      DO i = 1, SIZE(this%signals)
         this%signals(i)%p => null()
      END DO

      IF (ASSOCIATED(this%signals)) THEN
         DEALLOCATE(this%signals)
         this%signals => null()
      END IF

      IF (ASSOCIATED(this%kls)) THEN
         DEALLOCATE(this%kls)
         this%kls => null()
      END IF

      IF (ASSOCIATED(this%kll)) THEN
         DEALLOCATE(this%kll)
         this%kll => null()
      END IF

      IF (ASSOCIATED(this%work)) THEN
         DEALLOCATE(this%work)
         this%work => null()
      END IF

      this%fpoints => null()

!  Deconstruct and deallocate all the hyper parameters.
      IF (ASSOCIATED(this%params)) THEN
         DO i = 1, SIZE(this%params)
            IF (ASSOCIATED(this%params(i)%p)) THEN
               CALL param_destruct(this%params(i)%p)
               this%params(i)%p => null()
            END IF
         END DO
         DEALLOCATE(this%params)
         this%params => null()
      END IF

      CLOSE (this%iou)

      DEALLOCATE(this)

      END SUBROUTINE

!*******************************************************************************
!  SETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Set the object and coefficient for an index.
!>
!>  @param[inout] this   A @ref gaussp_class instance.
!>  @param[in]    signal A @ref signal_class instance.
!>  @param[in]    a      A weight for the signal
!>  @param[in]    index  The index to place the gaussp @ref signal.
!>  @note Only signals that were created before this gaussian process
!>  can be added. Otherwise it will creat an infinitely recursive loop in
!>  @ref gaussp_get_modeled_signal
!-------------------------------------------------------------------------------
      SUBROUTINE gaussp_set_signal(this, signal, index)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (gaussp_class), INTENT(inout) :: this
      CLASS (signal_class), POINTER      :: signal
      INTEGER, INTENT(in)                :: index

!  local Variables
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      this%signals(index)%p => signal

      CALL profiler_set_stop_time('gaussp_set_signal', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Set the object and coefficient for an index.
!>
!>  This method uses a gaussian process to set a model using the signals pointed
!>  in the  @ref gaussp_class::signals array. Hyper parameters are determined
!>  by maximizing the evidence.
!>
!>  @param[in]    this    A @ref gaussp_class instance.
!>  @param[inout] a_model A @ref model instance.
!-------------------------------------------------------------------------------
      SUBROUTINE gaussp_set_profile(this, a_model)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (gaussp_class), INTENT(inout)        :: this
      CLASS (model_class), POINTER              :: a_model

!  Local Variables
      INTEGER                                   :: i
      INTEGER                                   :: j
      REAL (rprec)                              :: start_time
      REAL (rprec), DIMENSION(:), ALLOCATABLE   :: cached_value
      REAL (rprec), DIMENSION(:), ALLOCATABLE   :: gradient
      REAL (rprec)                              :: evidence
      REAL (rprec)                              :: new_evidence
      REAL (rprec)                              :: gamma
      REAL (rprec)                              :: temp_param
      INTEGER                                   :: status

!  local parameters
      REAL (rprec), PARAMETER                   :: gamma_init = 0.01

!  Start of executable code
      start_time = profiler_get_start_time()

!  Flags will have a single bit position of a corresponding profile set.
!
!    this%flags .and. a_model%state_flags
!
!  will evaluate to true if the coresponding flag is set in the model state
!  flags.
      IF (BTEST(a_model%state_flags, model_state_vmec_flag)   .or.             &
     &    BTEST(a_model%state_flags, model_state_siesta_flag) .or.             &
     &    BTEST(a_model%state_flags, model_state_shift_flag)  .or.             &
     &    BTEST(a_model%state_flags, model_state_signal_flag) .or.             &
     &    IAND(this%flags, a_model%state_flags) .ne. 0) THEN

!  Use gradient ascent to fine the optimal hyper parameters by maximizing the
!  the evidence.
         new_evidence = gaussp_get_evidence(this, a_model)

         ALLOCATE(cached_value(SIZE(this%params)))
         ALLOCATE(gradient(SIZE(this%params)))

         DO i = 1, SIZE(this%params)
            cached_value(i) = param_get_value(this%params(i)%p, a_model)
         END DO

         WRITE (this%iou,1000)
         WRITE (this%iou,1001) new_evidence

         DO
            evidence = new_evidence
            gamma = gamma_init

!  Compute gradient.
            DO i = 1, SIZE(this%params)
               CALL param_increment(this%params(i)%p, a_model,                 &
     &                              MPI_COMM_NULL, .false.)
               gradient(i) = (gaussp_get_evidence(this, a_model) -             &
     &                        evidence)/this%params(i)%p%recon%delta
               CALL param_decrement(this%params(i)%p, a_model,                 &
     &                              MPI_COMM_NULL)
            END DO

!  Step towards the maximum.
            cached_value = cached_value + gamma*gradient

            DO i = 1, SIZE(this%params)
               CALL param_set_value(this%params(i)%p, a_model,                 &
     &                              cached_value(i), MPI_COMM_NULL,            &
     &                              .false.)
            END DO
            new_evidence = gaussp_get_evidence(this, a_model)

            DO WHILE (new_evidence - evidence .lt. 0.0)
               gamma = 0.5*gamma
               cached_value = cached_value - gamma*gradient

               DO i = 1, SIZE(this%params)
                  CALL param_set_value(this%params(i)%p, a_model,              &
     &                                 cached_value(i), MPI_COMM_NULL,         &
     &                                 .false.)
               END DO
               new_evidence = gaussp_get_evidence(this, a_model)
            END DO

            WRITE (this%iou,1002) new_evidence,                                &
     &                            new_evidence - evidence, gamma
            FLUSH(this%iou)

            IF (ABS(new_evidence - evidence) .lt. this%tolerance) THEN
               EXIT
            END IF
         END DO

         WRITE (this%iou,*) 'Final Parameters', cached_value
         FLUSH(this%iou)

         DEALLOCATE(cached_value)
         DEALLOCATE(gradient)
      END IF

!  Found the maximum now set the profile. The profile is determined by sampling
!  the distribution.
!
!    af = K_SL * A^-1                                                        (1)
!$OMP PARALLEL
!$OMP& DEFAULT(SHARED)
!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = 1, SIZE(this%fpoints)
         this%fpoints(i) = DOT_PRODUCT(this%kls(:,i), this%work(:,1))
      END DO
!$OMP END DO
!$OMP END PARALLEL

      CALL profiler_set_stop_time('gaussp_set_profile', start_time)

1000  FORMAT('New Process')
1001  FORMAT('Log Evidence : ',es12.5)
1002  FORMAT('Log Evidence : ',es12.5,' Change : ',es12.5,                     &
     &       ' Gamma : ',es12.5)

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the evidence.
!>
!>  This method finds a gaussian process evidence. The matrices computed can be
!>  later used to set the profile.
!>
!>  @param[in]    this    A @ref gaussp_class instance.
!>  @param[inout] a_model A @ref model instance.
!>  @returns The log evidence for the current hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION gaussp_get_evidence(this, a_model)
      USE stel_constants, ONLY: twopi

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                       :: gaussp_get_evidence
      TYPE (gaussp_class), INTENT(inout) :: this
      CLASS (model_class), POINTER       :: a_model

!  Local Variables
      CLASS (signal_class), POINTER      :: signal_obj
      INTEGER                            :: i
      INTEGER                            :: j
      REAL (rprec)                       :: start_time
      INTEGER                            :: ierr

!  Local Parameters
      REAL (rprec), PARAMETER            :: log2pi = LOG(twopi)

!  Start of executable code
      start_time = profiler_get_start_time()

!$OMP PARALLEL
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i,j,signal_obj)

!$OMP DO
!$OMP& SCHEDULE(DYNAMIC)
      DO j = 1, SIZE(this%fpoints)
         DO i = 1, SIZE(this%signals)
            signal_obj => this%signals(i)%p
            this%kls(i,j) = signal_obj%get_gp(a_model, j, this%flags)
         END DO
         this%work(:,j + 1) = this%kls(:,j)
      END DO
!$OMP END DO

!$OMP DO
!$OMP& SCHEDULE(DYNAMIC)
      DO j = 1, SIZE(this%signals)
         DO i = j + 1, SIZE(this%signals)
            signal_obj => this%signals(i)%p
            this%kll(i,j) = signal_obj%get_gp(a_model,                         &
     &                                        this%signals(j)%p,               &
     &                                        this%flags)
            this%kll(j,i) = this%kll(i,j)
         END DO
         signal_obj => this%signals(j)%p
         this%kll(j,j) = signal_obj%get_gp(a_model, signal_obj,                &
     &                                     this%flags)                         &
     &                 + signal_obj%get_sigma2()                               &
     &                 + this%cholesky_fac
         this%work(j,1) = signal_obj%get_observed_signal(a_model)
      END DO
!$OMP END DO
!$OMP END PARALLEL

!  CALL LAPACK to do Cholesky factorization of the A=(K_LL + Sigma_y)
      CALL DPOTRF('L', SIZE(this%signals), this%kll,                           &
     &            SIZE(this%signals), ierr)

!  The elements in the cholesky matrix for j > i are meaning less.
      IF (ierr .lt. 0) THEN
         CALL err_fatal('gaussp_get_evidence: DROTRF cannot ' //               &
     &                  'factor matrix. ierr is negative')
      ELSE IF (ierr .gt. 0) THEN
         CALL err_fatal('gaussp_get_evidence: DROTRF cannot ' //               &
     &                  'factor matrix. ierr is positive. ' //                 &
     &                  'Try increasing gp_cholesky_fact')
      END IF

!  IF statement correctly throws an error if I cannot factor the matrix!

!  CALL LAPACK to solve A^-1y and A^-1 K_LS simultaneously. The work matrix
!  stores the RHS on input, and returns the solution matrix.
      CALL DPOTRS('L', SIZE(this%signals), SIZE(this%fpoints) + 1,             &
     &            this%kll, SIZE(this%signals), this%work,                     &
     &            SIZE(this%signals), ierr)
      IF (ierr .lt. 0) THEN
         CALL err_fatal('gaussp_get_modeled_signal: DROTRS ' //                &
     &                  'failed to solve the equation')
      END IF

!  Calculate log evidence.
!
!    P(y|sigma,x) = 1/Sqrt((2Pi)^N*|A|)Exp(-y^T.A^-1.y/2)                    (1)
!
!  Taking the ln of the evidence results in.
!
!    ln(P) = -N/2*ln(2Pi) - ln(|A|)/2 - y^T.A^-1.y/2                         (2)
!
!  The determinant of A is found by the Cholesky factorization and equals the
!  product of the squares of the diagonal.
!
!    |A| = Prod (A_ii)^2                                                     (3)
!
!  Taking the ln of this results in.
!
!    ln(|A|) = Sum 2*ln(A_ii)                                                (4)
!

      gaussp_get_evidence = -0.5*SIZE(this%signals)*log2pi
!$OMP PARALLEL
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(j)
!$OMP DO
!$OMP& REDUCTION(-:gaussp_get_evidence)
      DO j = 1, SIZE(this%signals)
         gaussp_get_evidence = gaussp_get_evidence                             &
     &                       - LOG(this%kll(j,j))                              &
     &                       - 0.5*signal_get_observed_signal(                 &
     &                                this%signals(j)%p, a_model)*             &
     &                         this%work(j,1)
      END DO
!$OMP END DO
!$OMP END PARALLEL

      CALL profiler_set_stop_time('gaussp_get_evidence', start_time)

      END FUNCTION

      END MODULE

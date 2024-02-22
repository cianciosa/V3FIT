!*******************************************************************************
!>  @file reconstruction.f
!>  @brief Contains module @ref reconstruction.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref reconstruction_class. This class
!>  contains the minimization algorithm.
!*******************************************************************************

      MODULE reconstruction
      USE stel_kinds
      USE guassian_process
      USE profiler

      IMPLICIT NONE

!*******************************************************************************
!  reconstruction module parameters
!*******************************************************************************
!>  No reconstruction step type.
      INTEGER, PARAMETER :: reconstruction_no_step_type  = -1
!>  Straight line reconstruction step type.
      INTEGER, PARAMETER :: reconstruction_sl_step_type  = 0
!>  Levenberg-Marquardt reconstruction step type.
      INTEGER, PARAMETER :: reconstruction_lm_step_type  = 1
!>  Segmented path reconstruction step type.
      INTEGER, PARAMETER :: reconstruction_seg_step_type = 2

!>  Maximum number of reconstruction step attemps.
      INTEGER, PARAMETER :: reconstruction_max_step_try = 5

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) reconstruction base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class containing all the data needed to reconstruct a model.
!-------------------------------------------------------------------------------
      TYPE :: reconstruction_class
!>  Type descriptor of the boundry type for the lower(1) and upper(2) ranges.
!>  @par Possible values are:
!>  * @ref reconstruction_no_step_type
!>  * @ref reconstruction_sl_step_type
!>  * @ref reconstruction_lm_step_type
!>  * @ref reconstruction_seg_step_type
         INTEGER :: step_type = reconstruction_no_step_type
!>  Maximum number of reconstruction steps.
!>  @see v3fit_input::nrstep
         REAL (rprec)                          :: step_max
!>  Controls if central differencing is used.
         LOGICAL                               :: use_central = .false.
!>  Counter to track the step number.
         INTEGER                               :: current_step = 0
!>  Index of the last parallelable signal.
         INTEGER                               :: last_para_signal = 0

!>  Array of all e vectors for each reconstruction step.
!>  @see signal::signal_get_e
         REAL (rprec), DIMENSION(:,:), POINTER :: e => null()
!>  Array of all f vectors for each reconstruction step. The f vector contains
!>  the initial value of the derived parameter.
         REAL (rprec), DIMENSION(:,:), POINTER :: f => null()
!>  The normalized jacobian for the current step.
         REAL (rprec), DIMENSION(:,:), POINTER :: jacobian => null()
!>  The normalized derived jacobian for the current step.
         REAL (rprec), DIMENSION(:,:), POINTER ::                              &
     &      derived_jacobian => null()
!>  The normalized hessian for the current step.
         REAL (rprec), DIMENSION(:,:), POINTER :: hessian => null()
!>  The normalized gradient for the current step.
         REAL (rprec), DIMENSION(:), POINTER   :: gradient => null()
!>  The normalized parameter step for each sigular value.
         REAL (rprec), DIMENSION(:,:), POINTER :: delta_a => null()
!>  The matrix of singular value decomposition singular values.
         REAL (rprec), DIMENSION(:), POINTER   :: j_svd_w => null()
!>  The U matrix of singular value decomposition.
         REAL (rprec), DIMENSION(:,:), POINTER :: j_svd_u => null()
!>  The tansposed V matrix of singular value decomposition.
         REAL (rprec), DIMENSION(:,:), POINTER :: j_svd_vt => null()
!>  The normalized step length for each sigular value.
         REAL (rprec), DIMENSION(:), POINTER   :: delta_a_len => null()
!>  The singular values used in inverting parameter covariance matrix.
         REAL (rprec), DIMENSION(:), POINTER   :: svd_w => null()

!>  The expected g^2 for each reconstruction step.
         REAL (rprec), DIMENSION(:), POINTER   :: exp_g2 => null()
!>  The normalized step size for each reconstruction step.
         REAL (rprec), DIMENSION(:), POINTER   :: step_size => null()
!>  The number of sigular values used for each reconstruction step.
         INTEGER, DIMENSION(:), POINTER        :: num_sv => null()

!  Cutoff value controls.
!>  Cutoff value for relative singular values.
!>  @see v3fit_input::cut_svd
         REAL (rprec) :: cut_svd
!>  Cutoff value for expected step efficiency.
!>  @see v3fit_input::cut_eff
         REAL (rprec) :: cut_eff
!>  Cutoff value for expected marginal step efficiency.
!>  @see v3fit_input::cut_marg_eff
         REAL (rprec) :: cut_marg_eff
!>  Cutoff value for expected step size.
!>  @see v3fit_input::cut_delta_a
         REAL (rprec) :: cut_delta_a
!>  Cutoff value for expected change in g^2.
!>  @see v3fit_input::cut_dg2
         REAL (rprec) :: cut_dg2
!>  Cutoff value for pseudo inverse singular values
!>  @see v3fit_input::cut_inv_svd
         REAL (rprec) :: cut_inv_svd

!>  Ratio of singular values to the Levenberg-Marquardt lambda.
         REAL (rprec), DIMENSION(:,:), POINTER :: lm_ratio => null()

!>  Last signals.
         REAL (rprec), DIMENSION(:,:), POINTER :: last_values => null()
      CONTAINS
         FINAL     :: reconstruction_destruct
         PROCEDURE :: get_k_use => reconstruction_get_k_use
         PROCEDURE :: get_exp_dg2 => reconstruction_get_exp_dg2
         PROCEDURE :: get_exp_g2 => reconstruction_get_exp_g2
         PROCEDURE :: get_g2 => reconstruction_get_g2
         PROCEDURE :: get_lastg2 => reconstruction_get_lastg2
         PROCEDURE :: get_dg2 => reconstruction_get_dg2
         PROCEDURE :: eval_e => reconstruction_eval_e
         PROCEDURE :: eval_f => reconstruction_eval_f
         PROCEDURE :: eval_jacobians => reconstruction_eval_jacobians
         PROCEDURE :: eval_step => reconstruction_eval_step
         PROCEDURE :: sl_step => reconstruction_sl_step
         PROCEDURE :: seg_step => reconstruction_seg_step
         PROCEDURE :: lm_step => reconstruction_lm_step
         PROCEDURE :: lm_rootfind => reconstruction_lm_rootfind
         PROCEDURE :: lm_function => reconstruction_lm_function
         PROCEDURE :: step => reconstruction_step
         PROCEDURE :: try_step => reconstruction_try_step
         PROCEDURE :: eval_sem => reconstruction_eval_sem
         PROCEDURE :: invert_matrix => reconstruction_invert_matrix
         PROCEDURE :: write => reconstruction_write
         PROCEDURE :: write_step1 => reconstruction_write_step1
         PROCEDURE :: write_step2 => reconstruction_write_step2
         GENERIC   :: write_step => write_step1, write_step2
         PROCEDURE :: restart => reconstruction_restart
         PROCEDURE :: sync_state => reconstruction_sync_state
         PROCEDURE :: sync_svd => reconstruction_sync_svd
         PROCEDURE :: sync_parent => reconstruction_sync_parent
      END TYPE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref reconstruction_class object.
!>
!>  Allocates memory and initializes a @ref reconstruction_class object.
!>
!>  @param[in] num_steps              Number of reconstruction steps.
!>                                    @ref v3fit_input::nrstep
!>  @param[in] num_signals            Number of signals.
!>  @param[in] num_derived_parameters Number of derived parameters.
!>  @param[in] num_parameters         Number of reconstruction parameters.
!>  @param[in] step_type              Reconstruction step type.
!>                                    @ref v3fit_input::step_type
!>  @param[in] step_max               Reconstruction step size.
!>                                    @ref v3fit_input::astep_max
!>  @param[in] cut_svd                Cutoff value for relative singular values.
!>                                    @ref v3fit_input::cut_svd
!>  @param[in] cut_eff                Cutoff value for expected step efficiency.
!>                                    @ref v3fit_input::cut_eff
!>  @param[in] cut_marg_eff           Cutoff value for expected marginal step
!>                                    efficiency. @ref v3fit_input::cut_marg_eff
!>  @param[in] cut_delta_a            Cutoff value for expected step size.
!>                                    @ref v3fit_input::cut_delta_a
!>  @param[in] cut_dg2                Cutoff value for expected change in g^2.
!>                                    @ref v3fit_input::cut_dg2
!>  @param[in] last_para_signal       Index of the last signal that can be
!>                                    parallelized.
!>  @param[in] cut_inv_svd            Cutoff value for the pseudo inverse
!>                                    singular values.
!>  @returns A pointer to a constructed @ref reconstruction_class object.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_construct(num_steps, num_signals,                &
     &                                  num_derived_parameters,                &
     &                                  num_parameters, step_type,             &
     &                                  step_max, cut_svd, cut_eff,            &
     &                                  cut_marg_eff, cut_delta_a,             &
     &                                  cut_dg2, last_para_signal,             &
     &                                  cut_inv_svd, use_central)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (reconstruction_class), POINTER :: reconstruction_construct
      INTEGER, INTENT(in)                   :: num_steps
      INTEGER, INTENT(in)                   :: num_signals
      INTEGER, INTENT(in)                   :: num_derived_parameters
      INTEGER, INTENT(in)                   :: num_parameters
      CHARACTER (len=*), INTENT(in)         :: step_type
      REAL (rprec), INTENT(in)              :: step_max
      REAL (rprec), INTENT(in)              :: cut_svd
      REAL (rprec), INTENT(in)              :: cut_eff
      REAL (rprec), INTENT(in)              :: cut_marg_eff
      REAL (rprec), INTENT(in)              :: cut_delta_a
      REAL (rprec), INTENT(in)              :: cut_dg2
      INTEGER, INTENT(in)                   :: last_para_signal
      REAL (rprec), INTENT(in)              :: cut_inv_svd
      LOGICAL, INTENT(in)                   :: use_central

!  local variables
      INTEGER                               :: num_svd
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(reconstruction_construct)

      num_svd = MIN(num_signals, num_parameters)

      SELECT CASE (TRIM(step_type))

         CASE ('lm')
            reconstruction_construct%step_type =                               &
     &         reconstruction_lm_step_type
            ALLOCATE(reconstruction_construct%lm_ratio(num_svd,3))

         CASE ('seg')
            reconstruction_construct%step_type =                               &
     &         reconstruction_seg_step_type

         CASE DEFAULT
            reconstruction_construct%step_type =                               &
     &         reconstruction_sl_step_type

      END SELECT

      reconstruction_construct%step_max = step_max

      reconstruction_construct%cut_svd = cut_svd
      reconstruction_construct%cut_eff = cut_eff
      reconstruction_construct%cut_marg_eff = cut_marg_eff
      reconstruction_construct%cut_delta_a = cut_delta_a
      reconstruction_construct%cut_dg2 = cut_dg2

      reconstruction_construct%last_para_signal = last_para_signal

      reconstruction_construct%cut_inv_svd = cut_inv_svd

      reconstruction_construct%use_central = use_central

      ALLOCATE(reconstruction_construct%e(num_signals,0:num_steps))
      ALLOCATE(reconstruction_construct%f(num_derived_parameters,              &
     &                                    0:num_steps))
      ALLOCATE(reconstruction_construct%jacobian(num_signals,                  &
     &                                           num_parameters))
      ALLOCATE(reconstruction_construct%derived_jacobian(                      &
     &            num_derived_parameters,num_parameters))
      ALLOCATE(reconstruction_construct%hessian(num_parameters,                &
     &                                          num_parameters))
      ALLOCATE(reconstruction_construct%gradient(num_parameters))
      ALLOCATE(reconstruction_construct%delta_a(num_parameters,                &
     &                                          0:num_svd))
      ALLOCATE(reconstruction_construct%j_svd_w(num_svd))
      ALLOCATE(reconstruction_construct%j_svd_u(num_signals,                   &
     &                                          num_signals))
      ALLOCATE(reconstruction_construct%j_svd_vt(num_parameters,               &
     &                                           num_parameters))
      ALLOCATE(reconstruction_construct%delta_a_len(0:num_svd))
      ALLOCATE(reconstruction_construct%svd_w(num_parameters))
      reconstruction_construct%e = 0.0

      ALLOCATE(reconstruction_construct%exp_g2(num_steps))
      ALLOCATE(reconstruction_construct%step_size(num_steps))
      ALLOCATE(reconstruction_construct%num_sv(num_steps))

      ALLOCATE(reconstruction_construct%last_values(4,num_signals))
      reconstruction_construct%last_values = 0.0

      CALL profiler_set_stop_time('reconstruction_construct',                  &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref reconstruction_class object.
!>
!>  Deallocates memory and uninitializes a @ref reconstruction_class object.
!>
!>  @param[inout] this A @ref reconstruction_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE reconstruction_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (reconstruction_class), INTENT(inout) :: this

!  Start of executable code
      this%step_type = reconstruction_no_step_type

      this%cut_svd = 0.0
      this%cut_eff = 0.0
      this%cut_marg_eff = 0.0
      this%cut_delta_a = 0.0
      this%cut_dg2 = 0.0

      this%current_step = 0

      IF (ASSOCIATED(this%e)) THEN
         DEALLOCATE(this%e)
         this%e => null()
      END IF

      IF (ASSOCIATED(this%f)) THEN
         DEALLOCATE(this%f)
         this%f => null()
      END IF

      IF (ASSOCIATED(this%jacobian)) THEN
         DEALLOCATE(this%jacobian)
         this%jacobian => null()
      END IF

      IF (ASSOCIATED(this%derived_jacobian)) THEN
         DEALLOCATE(this%derived_jacobian)
         this%derived_jacobian => null()
      END IF

      IF (ASSOCIATED(this%hessian)) THEN
         DEALLOCATE(this%hessian)
         this%hessian => null()
      END IF

      IF (ASSOCIATED(this%gradient)) THEN
         DEALLOCATE(this%gradient)
         this%gradient => null()
      END IF

      IF (ASSOCIATED(this%delta_a)) THEN
         DEALLOCATE(this%delta_a)
         this%delta_a => null()
      END IF

      IF (ASSOCIATED(this%j_svd_w)) THEN
         DEALLOCATE(this%j_svd_w)
         this%j_svd_w => null()
      END IF

      IF (ASSOCIATED(this%j_svd_u)) THEN
         DEALLOCATE(this%j_svd_u)
         this%j_svd_u => null()
      END IF

      IF (ASSOCIATED(this%j_svd_vt)) THEN
         DEALLOCATE(this%j_svd_vt)
         this%j_svd_vt => null()
      END IF

      IF (ASSOCIATED(this%delta_a_len)) THEN
         DEALLOCATE(this%delta_a_len)
         this%delta_a_len => null()
      END IF

      IF (ASSOCIATED(this%svd_w)) THEN
         DEALLOCATE(this%svd_w)
         this%svd_w => null()
      END IF

      IF (ASSOCIATED(this%exp_g2)) THEN
         DEALLOCATE(this%exp_g2)
         this%exp_g2 => null()
      END IF

      IF (ASSOCIATED(this%step_size)) THEN
         DEALLOCATE(this%step_size)
         this%step_size => null()
      END IF

      IF (ASSOCIATED(this%num_sv)) THEN
         DEALLOCATE(this%num_sv)
         this%num_sv => null()
      END IF

      IF (ASSOCIATED(this%lm_ratio)) THEN
         DEALLOCATE(this%lm_ratio)
         this%lm_ratio => null()
      END IF

      IF (ASSOCIATED(this%last_values)) THEN
         DEALLOCATE(this%last_values)
         this%last_values => null()
      END IF

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Get the number of singular values to use.
!>
!>  Number of sigular values to uses is determined by the various cutoffs.
!>
!>  @param[inout] this   A @ref reconstruction_class instance.
!>  @param[in]    num_sv Total number of sigular values.
!>  @returns The number of sigular values to use.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_get_k_use(this, num_sv)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: reconstruction_get_k_use
      CLASS (reconstruction_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                         :: num_sv

!  local variables
      REAL (rprec), DIMENSION(:), ALLOCATABLE     :: dg2exp
      REAL (rprec), DIMENSION(:), ALLOCATABLE     :: exp_eff
      REAL (rprec), DIMENSION(:), ALLOCATABLE     :: marg_exp_eff
      INTEGER                                     :: k
      REAL (rprec)                                :: denom
      REAL (rprec)                                :: largest_w
      REAL (rprec)                                :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(dg2exp(0:num_sv))
      ALLOCATE(exp_eff(0:num_sv))
      ALLOCATE(marg_exp_eff(0:num_sv))

!  Estimate the expected changes in g^2. Equation 22 in Hanson et. al.
!  doi: 10.1088/0029-5515/49/7/075031
      DO k = 0, num_sv
         dg2exp(k) = this%get_exp_dg2(this%delta_a(:,k))

         this%delta_a_len(k) = SQRT(DOT_PRODUCT(this%delta_a(:,k),             &
     &                                          this%delta_a(:,k)))

!  Equation 23 in Hanson et. al. doi: 10.1088/0029-5515/49/7/075031
         exp_eff(k) = ABS(dg2exp(k))/this%delta_a_len(k)
      END DO

!  Although marginal efficiencies are not strictly defined for the Steepest
!  Descent (index 0) and just one singular value cases, for convenience define
!  them here.
      marg_exp_eff(0:1) = exp_eff(0:1)

      DO k = 2, num_sv
         denom = this%delta_a_len(k) - this%delta_a_len(k - 1)
         IF (denom .eq. 0.0) THEN
            IF (this%delta_a_len(k) .eq. 0.0) THEN
               denom = 1.0E-10_rprec
            ELSE
               denom = 1.0E-10_rprec*this%delta_a_len(k)
            END IF
         END IF
         marg_exp_eff(k) = ABS(dg2exp(k)) - ABS(dg2exp(k - 1))/denom
      END DO

!  Check for cutoffs.
      IF (this%j_svd_w(1) .le. 0.0) THEN
         largest_w = 1.0
      ELSE
         largest_w = this%j_svd_w(1)
      END IF

!  Find the largest number of singular values to use. All the selection criteria
!  could fail so initalize it to use no sigular values.
      reconstruction_get_k_use = 0
      DO k = num_sv, 1, -1
         IF ((this%j_svd_w(k)/largest_w .ge. this%cut_svd)      .and.          &
     &       (exp_eff(k)                .ge. this%cut_eff)      .and.          &
     &       (marg_exp_eff(k)           .ge. this%cut_marg_eff) .and.          &
     &       (this%delta_a_len(k)       .ge. this%cut_delta_a)  .and.          &
     &       (ABS(dg2exp(k))            .ge. this%cut_dg2)) THEN
            reconstruction_get_k_use = k
            EXIT
         END IF
      END DO

      DEALLOCATE(dg2exp)
      DEALLOCATE(exp_eff)
      DEALLOCATE(marg_exp_eff)

      CALL profiler_set_stop_time('reconstruction_get_k_use',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the expected dg^2 size
!>
!>  Estimates the change in g^2.
!>  Equation 22 in Hanson et. al. doi:10.1088/0029-5515/49/7/075031
!>  Note: there is a typo in equation 22. The - sign in the last term should be
!>  a plus sign.
!>
!>  @param[in] this    A @ref reconstruction_class instance.
!>  @param[in] delta_a The normalized parameter step.
!>  @returns Change expected change in g^2.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_get_exp_dg2(this, delta_a)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: reconstruction_get_exp_dg2
      CLASS (reconstruction_class), INTENT(in) :: this
      REAL (rprec), DIMENSION(:), INTENT(in)   :: delta_a

!  local variables
      REAL (rprec)                             :: dg2exp_lin
      REAL (rprec)                             :: dg2exp_quad
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  2e * A * da  
      dg2exp_lin =                                                             &
     &   2.0*DOT_PRODUCT(this%e(:,this%current_step),                          &
     &                   MATMUL(this%jacobian, delta_a))

!  da * A^T * A * da = da * alpha * da where alpha is the hessian matrix.
!  Equation 14 in Hanson et. al. doi: 10.1088/0029-5515/49/7/075031
      dg2exp_quad = DOT_PRODUCT(delta_a, MATMUL(this%hessian, delta_a))
      reconstruction_get_exp_dg2 = dg2exp_lin - dg2exp_quad

      CALL profiler_set_stop_time('reconstruction_get_exp_dg2',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the expected g^2 size
!>
!>  Estimates the g^2.
!>  Equation 22 in Hanson et. al. doi:10.1088/0029-5515/49/7/075031
!>
!>  @param[in] this    A @ref reconstruction_class instance.
!>  @param[in] delta_a The normalized parameter step.
!>  @returns Expected g^2.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_get_exp_g2(this, delta_a)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: reconstruction_get_exp_g2
      CLASS (reconstruction_class), INTENT(in) :: this
      REAL (rprec), DIMENSION(:), INTENT(in)   :: delta_a

!  local variables
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      reconstruction_get_exp_g2 = this%get_g2()                                &
     &                          - this%get_exp_dg2(delta_a)

      CALL profiler_set_stop_time('reconstruction_get_exp_g2',                 &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the current g^2
!>
!>  Gets the g^2 value for the current reconstruction step.
!>
!>  @param[in] this A @ref reconstruction_class instance.
!>  @returns Current g^2.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_get_g2(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: reconstruction_get_g2
      CLASS (reconstruction_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      reconstruction_get_g2 =                                                  &
     &   DOT_PRODUCT(this%e(:,this%current_step),                              &
     &               this%e(:,this%current_step))

      CALL profiler_set_stop_time('reconstruction_get_g2', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the last g^2
!>
!>  Gets the g^2 value for the last reconstruction step.
!>
!>  @param[in] this A @ref reconstruction_class instance.
!>  @returns The last g^2.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_get_lastg2(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: reconstruction_get_lastg2
      CLASS (reconstruction_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      reconstruction_get_lastg2 =                                              &
     &   DOT_PRODUCT(this%e(:,this%current_step - 1),                          &
     &               this%e(:,this%current_step - 1))

      CALL profiler_set_stop_time('reconstruction_get_lastg2',                 &
     &                            start_time)
      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the change in g^2
!>
!>  Gets the change in g^2 from the last reconstruction step to the current
!>  step.
!>
!>  @param[in] this A @ref reconstruction_class instance.
!>  @returns The last g^2.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_get_dg2(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: reconstruction_get_dg2
      CLASS (reconstruction_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      reconstruction_get_dg2 = this%get_lastg2() - this%get_g2()

      CALL profiler_set_stop_time('reconstruction_get_dg2', start_time)
      END FUNCTION

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Evaluate the e vector for a reconstruction step.
!>
!>  Evaluates the e vector after soving the equilibrium.
!>  @see signal::signal_get_e
!>
!>  @param[inout] this     A @ref reconstruction_class instance.
!>  @param[inout] signals  Array of @ref signal objects.
!>  @param[inout] a_model  A @ref model instance.
!>  @param[inout] gaussp   Array of @ref guassian_process::gaussp_class objects.
!>  @param[inout] eq_steps Number of steps for the equilibrium to take.
!>  @param[in]    iou      Input/output file to write log messages to.
!>  @param[in]    eq_comm  MPI communicator for the child equilibrium processes.
!>  @returns True if the equilibrium converged and false if otherwise.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_eval_e(this, signals, a_model, gaussp,           &
     &                               eq_steps, iou, eq_comm)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL :: reconstruction_eval_e
      CLASS (reconstruction_class), INTENT(inout)        :: this
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      CLASS (model_class), POINTER                       :: a_model
      TYPE (gaussp_class_pointer), DIMENSION(:), INTENT(inout) ::              &
     &   gaussp
      INTEGER, INTENT(inout)                             :: eq_steps
      INTEGER, INTENT(in)                                :: iou
      INTEGER, INTENT(in)                                :: eq_comm

!  local variables
      INTEGER                                            :: i
      REAL (rprec)                                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Converge the equilibrium. this%e is initialized to zero in the constructor.
      IF (a_model%converge(eq_steps, iou, eq_comm, 'All')) THEN
         DO i = 1, SIZE(gaussp)
            CALL gaussp_set_profile(gaussp(i)%p, a_model)
         END DO

!$OMP PARALLEL DO
!$OMP& SCHEDULE(DYNAMIC)
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i)
         DO i = 1, this%last_para_signal
            this%e(i,this%current_step) =                                      &
     &         signals(i)%p%get_e(a_model, .false.,                            &
     &                            this%last_values(:,i))
         END DO
!$OMP END PARALLEL DO

         DO i = this%last_para_signal + 1, SIZE(signals)
            this%e(i,this%current_step) =                                      &
     &         signals(i)%p%get_e(a_model, .false.,                            &
     &                            this%last_values(:,i))
         END DO

!  All signals are computed reset the model state.
         a_model%state_flags = model_state_all_off

         !  Write out the step information.
         reconstruction_eval_e = .true.
      ELSE
         reconstruction_eval_e = .false.
      END IF

      CALL profiler_set_stop_time('reconstruction_eval_e', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Evaluate the f vector for a reconstruction step.
!>
!>  Evaluates the f vector after soving the equilibrium.
!>  @see param::param_get_value
!>
!>  @param[inout] this           A @ref reconstruction_class instance.
!>  @param[in]    derived_params Array of @ref param objects.
!>  @param[inout] a_model        A @ref model instance.
!-------------------------------------------------------------------------------
      SUBROUTINE reconstruction_eval_f(this, derived_params, a_model)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (reconstruction_class), INTENT(inout)    :: this
      TYPE (param_pointer), DIMENSION(:), INTENT(in) :: derived_params
      CLASS (model_class), INTENT(inout)             :: a_model

!  local variables
      INTEGER                                        :: i
      REAL (rprec)                                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      DO i = 1, SIZE(derived_params)
         this%f(i,this%current_step) =                                         &
     &      param_get_value(derived_params(i)%p, a_model)
      END DO

      CALL profiler_set_stop_time('reconstruction_eval_f', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Evaluate the jacobian.
!>
!>  Evaluates the jacobian. When using MPI, reach row of the jacobian is
!>  evaluated in parallel.
!>
!>  @param[inout] this           A @ref reconstruction_class instance.
!>  @param[inout] signals        Array of @ref signal objects.
!>  @param[in]    derived_params Array of derived @ref param objects.
!>  @param[inout] a_model        A @ref model instance.
!>  @param[inout] params         Array of @ref parameter objects.
!>  @param[inout] eq_steps       Number of steps for the equilibrium to take.
!>  @param[in]    iou            Input/output file to write log messages to.
!>  @param[in]    recon_comm     MPI communicator for the child jacobian
!>                               processes.
!>  @param[in]    eq_comm        MPI communicator for the child equilibrium
!>                               processes.
!-------------------------------------------------------------------------------
      SUBROUTINE reconstruction_eval_jacobians(this, signals,                  &
     &                                         derived_params, locks,          &
     &                                         a_model, gaussp, params,        &
     &                                         eq_steps, iou,                  &
     &                                         recon_comm, eq_comm)
      USE xstuff

      IMPLICIT NONE

!  Declare Arguments
      CLASS (reconstruction_class), INTENT(inout)        :: this
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      TYPE (param_pointer), DIMENSION(:), INTENT(in)     ::                    &
     &   derived_params
      TYPE (param_pointer), DIMENSION(:), INTENT(in)     :: locks
      CLASS (model_class), POINTER                       :: a_model
      TYPE (gaussp_class_pointer), DIMENSION(:), INTENT(inout) ::              &
     &   gaussp
      TYPE (param_pointer), DIMENSION(:), INTENT(inout)  :: params
      INTEGER, INTENT(inout)                             :: eq_steps
      INTEGER, INTENT(in)                                :: iou
      INTEGER, INTENT(in)                                :: recon_comm
      INTEGER, INTENT(in)                                :: eq_comm

!  local variables
      INTEGER                                            :: i, j
      REAL (rprec)                                       :: param_value
      REAL (rprec)                                       :: start_time
      INTEGER                                            :: mpi_size
      INTEGER                                            :: mpi_rank
#if defined(MPI_OPT)
      INTEGER                                            :: error
#endif
      INTEGER, DIMENSION(:), ALLOCATABLE                 :: stride
      INTEGER, DIMENSION(:), ALLOCATABLE                 :: offset
      INTEGER                                            :: minsize
      INTEGER                                            :: extra

!  Start of
      start_time = profiler_get_start_time()

#if defined(MPI_OPT)
!  Set the params first since they can change the state of the converged
!  variable. Syning the model afterwards will bring the internal state up to
!  sync.
      DO i = 1, SIZE(params)
         CALL param_sync_value(params(i)%p, a_model, recon_comm,               &
     &                         eq_comm, this%use_central)
      END DO
      CALL a_model%sync_state(recon_comm)
      CALL this%sync_state(recon_comm)

!  Set the locked values.
      DO i = 1, SIZE(locks)
         CALL param_set_lock_value(locks(i)%p, a_model, eq_comm)
      END DO

      CALL MPI_COMM_RANK(recon_comm, mpi_rank, error)
      CALL MPI_COMM_SIZE(recon_comm, mpi_size, error)

#else
      mpi_rank = 0
      mpi_size = 1
#endif

      ALLOCATE(stride(mpi_size))
      ALLOCATE(offset(mpi_size))

!  Determine the offsets and strides that each uses. This determined the array
!  indices that each process computes the jacobian for. The number of parameters
!  might not be evenly dividable by the number of process. As a result. need to
!  determine the minimum size and add extra work for some of the processes.
      minsize = SIZE(params)/mpi_size
      extra = MOD(SIZE(params), mpi_size)

      stride = minsize
      stride(1:extra) = stride(1:extra) + 1
      offset = 0
      DO i = 2, mpi_size
         offset(i) = offset(i - 1) + stride(i - 1)
      END DO

      CALL a_model%save_state

!  Loop over the parameters, but only do iterations for the rank as determined
!  by the stride and the offset. Strides and offsets are determined using C
!  array notation since the MPI libraries use C conventions. Make sure the
!  offset is incremented by one for the initial index.
      DO j = offset(mpi_rank + 1) + 1, offset(mpi_rank + 1) +                  &
     &                                 stride(mpi_rank + 1)

!  Save the initial parameter value and the equilirbium state.
         param_value = param_get_value(params(j)%p, a_model)

!  Increment the parameter. param_increment checks the bounds and adjust the
!  step size to keep the parameter in range.
         CALL param_increment(params(j)%p, a_model, eq_comm,                   &
     &                        this%use_central)
!  Set the locked values.
         DO i = 1, SIZE(locks)
            CALL param_set_lock_value(locks(i)%p, a_model, eq_comm)
         END DO

!  Check that model was able to converge. If the model converged proceed as
!  normal otherwise set the jth index of the jacobian to zero to avoid a step
!  in that parameter direction.
         IF (a_model%converge(eq_steps, iou, eq_comm,                          &
     &                        param_get_name(params(j)%p,                      &
     &                                       a_model))) THEN
            DO i = 1, SIZE(gaussp)
               CALL gaussp_set_profile(gaussp(i)%p, a_model)
            END DO

!  Compute the normalized jacobian. d e_i/d a_j . Equation 12 in Hanson et. al.
!  doi:10.1088/0029-5515/49/7/075031
!
!    a_j = p_j/|pi_j|                                                        (1)
!
!  Where p is the jth parameter, and pi is the jth parameter vrnc. For the
!  finite difference, the partial derivative becomes
!
!    d e_i/d a_j = (e'_i - e_i)/(a'_j - a_j)                                 (2)
!
!  Where the primed components are values for the incremented value.
!
!    a'_j = (p_j + pi_j)/|pi_j|                                              (3)
!
!  Subsituting equation 2 into 3, reduces to
!
!    d e_i/d a_j = (e'_i - e_i)*|pi_j|/pi_j                                  (4)
!
!  In this coding e_i = Sqrt(signal_get_g2)
!  and |pi_j|/pi_j = SIGN(1,delta_param)
!
!  For the derived parameter jacobian. d f_i/d a_j . This will parallel the
!  signal jacobian with the exception that the parameters aren't normalized.
!  Using equation (3) above the derived jacobian becomes
!
!    d f_i/d a_j = (f'_i - f_i)*|pi_j|/pi_j                                  (5)
!
!  Again in this coding |pi_j|/pi_j = SIGN(1,delta_param)

            IF (this%use_central) THEN
!  Certian signals cannot be parallelized. Compute all the signals up to that
!  point in parallel then continue in serial. Store half the finite difference
!  in the jacobian. Central difference only takes steps in a single direction so
!  there is no need to normaize the direction.
!$OMP PARALLEL DO
!$OMP& SCHEDULE(DYNAMIC)
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i)
               DO i = 1, this%last_para_signal
                  this%jacobian(i,j) =                                         &
     &               signals(i)%p%get_e(a_model, .false.,                      &
     &                                  this%last_values(:,i))
               END DO
!$OMP END PARALLEL DO
               DO i = this%last_para_signal + 1, SIZE(signals)
                  this%jacobian(i,j) =                                         &
     &               signals(i)%p%get_e(a_model, .false.,                      &
     &                                  this%last_values(:,i))
               END DO

!  Derived Jacobian.
               DO i = 1, SIZE(derived_params)
                  this%derived_jacobian(i,j) =                                 &
     &               param_get_value(derived_params(i)%p, a_model)
               END DO

!  Reset the parameters and equilibrium to prepare for the backward step.
               CALL param_set_value(params(j)%p, a_model, param_value,         &
     &                              eq_comm, this%use_central)
               CALL a_model%reset_state
!  Set the locked values.
               DO i = 1, SIZE(locks)
                  CALL param_set_lock_value(locks(i)%p, a_model,               &
     &                                      eq_comm)
               END DO

!  Decrement the parameter.
               CALL param_decrement(params(j)%p, a_model, eq_comm)

               IF (a_model%converge(eq_steps, iou, eq_comm,                    &
     &                              param_get_name(params(j)%p,                &
     &                                             a_model))) THEN
                  DO i = 1, SIZE(gaussp)
                     CALL gaussp_set_profile(gaussp(i)%p, a_model)
                  END DO

!  Find other half of the finite difference. Negate the jacobian to obtain a
!  direction down hill.
!$OMP PARALLEL DO
!$OMP& SCHEDULE(DYNAMIC)
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i)
                  DO i = 1, this%last_para_signal
                     this%jacobian(i,j) =                                      &
     &                  -this%jacobian(i,j) +                                  &
     &                   signals(i)%p%get_e(a_model, .false.,                  &
     &                                      this%last_values(:,i))
                  END DO
!$OMP END PARALLEL DO
                  DO i = this%last_para_signal + 1, SIZE(signals)
                     this%jacobian(i,j) =                                      &
     &                  -this%jacobian(i,j) +                                  &
     &                   signals(i)%p%get_e(a_model, .false.,                  &
     &                                      this%last_values(:,i))
                  END DO

!  Derived Jacobian.
                  DO i = 1, SIZE(derived_params)
                     this%derived_jacobian(i,j) =                              &
     &                  -this%derived_jacobian(i,j) +                          &
     &                   param_get_value(derived_params(i)%p, a_model)
                  END DO

               ELSE
!  Warn the user that the model failed to converge on the jth parameter.
                  WRITE (*,1000) j

                  this%jacobian(:,j) = 0.0
                  this%derived_jacobian(:,j) = 0.0
               END IF
            ELSE

!  Certian signals cannot be parallelized. Compute all the signals up to that
!  point in parallel then continue in serial. When using a single sided
!  difference, the step may be taken in either direction. Normalize by the sign
!  of the parameter delta.
!$OMP PARALLEL DO
!$OMP& SCHEDULE(DYNAMIC)
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i)
               DO i = 1, this%last_para_signal
                  this%jacobian(i,j) =                                         &
     &               -(signals(i)%p%get_e(a_model, .false.,                    &
     &                                    this%last_values(:,i)) -             &
     &                this%e(i,this%current_step))
               END DO
!$OMP END PARALLEL DO
               DO i = this%last_para_signal + 1, SIZE(signals)
                  this%jacobian(i,j) =                                         &
     &               -(signals(i)%p%get_e(a_model, .false.,                    &
     &                                    this%last_values(:,i)) -             &
     &                this%e(i,this%current_step))
               END DO
               this%jacobian(:,j) = this%jacobian(:,j)                         &
     &                            * SIGN(1.0_rprec,                            &
     &                                   params(j)%p%recon%delta)

!  Derived Jacobian.
               DO i = 1, SIZE(derived_params)
                  this%derived_jacobian(i,j) =                                 &
     &               -(param_get_value(derived_params(i)%p, a_model)           &
     &               - this%f(i,this%current_step))
               END DO
               this%derived_jacobian(:,j) =                                    &
     &            this%derived_jacobian(:,j) *                                 &
     &            SIGN(1.0_rprec, params(j)%p%recon%delta)
            END IF
         ELSE
!  Warn the user that the model failed to converge on the jth parameter.
            WRITE (*,1000) j

            this%jacobian(:,j) = 0.0
            this%derived_jacobian(:,j) = 0.0
         END IF

!  Restore the parameter and the equilibrium to it's initial value. Need to sync
!  the value of param delta back to the parent process.
         CALL param_set_value(params(j)%p, a_model, param_value,               &
     &                        eq_comm, this%use_central)
         CALL a_model%reset_state
!  Set the locked values.
         DO i = 1, SIZE(locks)
            CALL param_set_lock_value(locks(i)%p, a_model, eq_comm)
         END DO
      END DO
!  At this point, the computed jacobian rows should be returned to the parent
!  process.
#if defined(MPI_OPT)
      CALL this%sync_parent(params, SIZE(signals),                             &
     &                      SIZE(derived_params), stride,                      &
     &                      offset, recon_comm)
#endif

      DEALLOCATE(stride)
      DEALLOCATE(offset)

      CALL profiler_set_stop_time('reconstruction_eval_jacobians',             &
     &                            start_time)

1000  FORMAT('Warning: Model failed to converge in Jacobian for ',             &
     &       'index ',i3,'.')

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Evaluate the proper quasi-Newton step.
!>
!>  Evaluates the jacobian, performs the SVD and computes the quasi-Newton step.
!>
!>  @param[inout] this           A @ref reconstruction_class instance.
!>  @param[inout] signals        Array of @ref signal objects.
!>  @param[in]    derived_params Array of derived @ref param objects.
!>  @param[inout] a_model        A @ref model instance.
!>  @param[inout] params         Array of @ref parameter objects.
!>  @param[inout] eq_steps       Number of steps for the equilibrium to take.
!>  @param[in]    iou            Input/output file to write log messages to.
!>  @param[in]    recon_comm     MPI communicator for the child jacobian
!>                               processes.
!>  @param[in]    eq_comm        MPI communicator for the child equilibrium
!>                               processes.
!>  @returns The number of singular values to use.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_eval_step(this, signals, derived_params,         &
     &                                  locks, a_model, gaussp, params,        &
     &                                  eq_steps, iou, recon_comm,             &
     &                                  eq_comm)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: reconstruction_eval_step
      CLASS (reconstruction_class), INTENT(inout)        :: this
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      TYPE (param_pointer), DIMENSION(:), INTENT(in)     ::                    &
     &   derived_params
      TYPE (param_pointer), DIMENSION(:), INTENT(in)     :: locks
      CLASS (model_class), POINTER                       :: a_model
      TYPE (gaussp_class_pointer), DIMENSION(:), INTENT(inout) ::              &
     &   gaussp
      TYPE (param_pointer), DIMENSION(:), INTENT(inout)  :: params
      INTEGER, INTENT(inout)                             :: eq_steps
      INTEGER, INTENT(in)                                :: iou
      INTEGER, INTENT(in)                                :: recon_comm
      INTEGER, INTENT(in)                                :: eq_comm

!  local variables
      INTEGER                                    :: i
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE  :: temp_jacobian
      REAL (rprec), DIMENSION(:), ALLOCATABLE    :: temp_work
      REAL (rprec), DIMENSION(:), ALLOCATABLE    :: temp_work2
      REAL (rprec), DIMENSION(:), ALLOCATABLE    :: temp_work3
      INTEGER                                    :: svd_status
      REAL (rprec)                               :: start_time
#if defined(MPI_OPT)
      INTEGER                                    :: error
#endif

!  Start of executable code
      start_time = profiler_get_start_time()

!  Tell the child processes to begin the jacobian task.
#if defined(MPI_OPT)
      CALL MPI_BCAST(mpi_jacobian_task, 1, MPI_INTEGER, 0, recon_comm,         &
     &               error)
      CALL MPI_BCAST(eq_steps, 1, MPI_INTEGER, 0, recon_comm, error)
#endif

      CALL this%eval_jacobians(signals, derived_params, locks, a_model,        &
     &                         gaussp, params, eq_steps, iou,                  &
     &                         recon_comm, eq_comm)

!  Compute the positive definite Hessian, matrix. Equation 14 in Hanson et. al.
!  doi:10.1088/0029-5515/49/7/075031
!
!    alpha = A^T * A                                                         (1)
!
!  Numerical Recipes Third edition Equation 15.5.7 . In here, the second
!  derivatives are assumed to be small and ignored.
      this%hessian = MATMUL(TRANSPOSE(this%jacobian), this%jacobian)

!  Compute the gradient vector in parameter space. Equation 13 in Hanson et. al.
!  doi:10.1088/0029-5515/49/7/075031
!
!    beta = A^T * e                                                          (2)
      this%gradient = MATMUL(TRANSPOSE(this%jacobian),                         &
     &                       this%e(:,this%current_step))

!  Compute the steepest descent step size in normalized parameter space.
!
!    da = beta * beta * beta / (beta * alpha * beta)                         (3)
      this%delta_a(:,0) = this%gradient*DOT_PRODUCT(this%gradient,             &
     &                                              this%gradient)             &
     &                  / DOT_PRODUCT(this%gradient,                           &
     &                                MATMUL(this%hessian,                     &
     &                                       this%gradient))

!  Singular value decomposition of the jacobian. Need to allocate a new jacobian
!  matrix because dgesvd will destroy the one passed to it. dgesvd is from
!  LAPACK
      ALLOCATE(temp_jacobian(SIZE(signals),SIZE(params)))
      temp_jacobian = this%jacobian

      ALLOCATE(temp_work(5*MAX(SIZE(signals), SIZE(params))))
      temp_work = 0.0

      CALL dgesvd('All', 'All', SIZE(signals), SIZE(params),                   &
     &            temp_jacobian, SIZE(signals), this%j_svd_w,                  &
     &            this%j_svd_u, SIZE(signals), this%j_svd_vt,                  &
     &            SIZE(params), temp_work, SIZE(temp_work), svd_status)
      CALL assert_eq(0, svd_status, 'reconstruction_eval_step: ' //            &
     &               'dgesvd problem')

      DEALLOCATE(temp_work)
      DEALLOCATE(temp_jacobian)

!  Reuse the temp work array.
      ALLOCATE(temp_work(SIZE(this%j_svd_w)))
      ALLOCATE(temp_work2(SIZE(signals)))
      ALLOCATE(temp_work3(SIZE(params)))

!  Define the inverse singular values.
      WHERE (this%j_svd_w .gt. 0.0)
         temp_work = 1.0/this%j_svd_w
      ELSE WHERE
         temp_work = 0.0
      END WHERE

!  U^T * e
      temp_work2 = MATMUL(TRANSPOSE(this%j_svd_u),                             &
     &                    this%e(:,this%current_step))

!  Perform pseudo-inversion for successive numbers of singular values retained.
      temp_work3 = 0.0
      DO i = 1, SIZE(this%j_svd_w)
         temp_work3(i) = temp_work(i)*temp_work2(i)
         this%delta_a(:,i) = MATMUL(TRANSPOSE(this%j_svd_vt),                  &
     &                              temp_work3)
      END DO

      DEALLOCATE(temp_work)
      DEALLOCATE(temp_work2)
      DEALLOCATE(temp_work3)

      reconstruction_eval_step = this%get_k_use(SIZE(this%j_svd_w))

      CALL profiler_set_stop_time('reconstruction_eval_step',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Take a straight line step.
!>
!>  Determines the normalized change in parameters for a straight line step.
!>
!>  @param[inout] this      A @ref reconstruction_class instance.
!>  @param[in]    k_use     Number of singular values to use.
!>  @param[in]    step_size Current step size limit.
!>  @param[out]   delta_a   Normalized change in parameter.
!>  @returns The step size used.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_sl_step(this, k_use, step_size, delta_a)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: reconstruction_sl_step
      CLASS (reconstruction_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                         :: k_use
      REAL (rprec), INTENT(in)                    :: step_size
      REAL (rprec), DIMENSION(:), INTENT(out)     :: delta_a

!  local variables
      REAL (rprec)                                :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF ((step_size .ge. 0.0) .and.                                           &
     &    (this%delta_a_len(k_use) .gt. step_size)) THEN
         delta_a = this%step_max/this%delta_a_len(k_use)                       &
     &           * this%delta_a(:,k_use)
         reconstruction_sl_step = SQRT(DOT_PRODUCT(delta_a, delta_a))
      ELSE
         delta_a = this%delta_a(:,k_use)
         reconstruction_sl_step = this%delta_a_len(k_use)
      END IF

      CALL profiler_set_stop_time('reconstruction_sl_step', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Take a segmented path step.
!>
!>  Determines the normalized change in parameters for a segmented path step.
!>
!>  @param[inout] this      A @ref reconstruction_class instance.
!>  @param[in]    k_use     Number of singular values to use.
!>  @param[in]    step_size Current step size limit.
!>  @param[out]   delta_a   Normalized change in parameter.
!>  @returns The step size used.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_seg_step(this, k_use, step_size, delta_a)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: reconstruction_seg_step
      CLASS (reconstruction_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                         :: k_use
      REAL (rprec), INTENT(in)                    :: step_size
      REAL (rprec), DIMENSION(:), INTENT(out)     :: delta_a

!  local variables
      INTEGER                                     :: k, k_min
      REAL (rprec)                                :: ysq, zsq, ydz
      REAL (rprec)                                :: aa, bb, cc, xx
      REAL (rprec)                                :: discriminant
      REAL (rprec)                                :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF ((step_size .ge. 0.0) .and.                                           &
     &    (this%delta_a_len(k_use) .gt. step_size)) THEN
!  First, determine k_min - the largest k value where the step is smaller than
!  step_size.
         k_min = 0
         DO k = k_use - 1, 1, -1
            IF (this%delta_a_len(k) .le. step_size) THEN
               k_min = k
               EXIT
            END IF
         END DO

!  Set up the quadratic
         ysq = DOT_PRODUCT(this%delta_a(:,k_min), this%delta_a(:,k_min))
         zsq = DOT_PRODUCT(this%delta_a(:,k_min + 1),                          &
     &                     this%delta_a(:,k_min + 1))
         ydz = DOT_PRODUCT(this%delta_a(:,k_min),                              &
     &                     this%delta_a(:,k_min + 1))

!  Special case when (k_min .eq. 0) - reconstruction arrays contain SD step, but
!  use the zero step for (kmin .eq. 0)
         IF (k_min .eq. 0) THEN
            ysq = 0.0
            ydz = 0.0
         END IF

         aa = ysq + zsq - 2.0*ydz
         bb = -2.0*ysq + 2.0*ydz
         cc = ysq - step_size**2.0

         discriminant = bb**2.0 - 4.0*aa*cc

         IF (discriminant .le. 0.0) THEN
            xx = 0.0
         ELSE
            xx = (-bb + SQRT(discriminant))/(2.0*aa)
         END IF

         IF ((xx .lt. 0.0) .or. (xx .gt. 1.0)) THEN
            xx = 0.0
         END IF

         delta_a = xx*this%delta_a(:,k_min + 1)
         IF (k_min .ne. 0) THEN
            delta_a = this%delta_a(:,k_min)*(1.0 - xx) + delta_a
         END IF

         reconstruction_seg_step = SQRT(DOT_PRODUCT(delta_a, delta_a))
      ELSE
         delta_a = this%delta_a(:,k_use)
         reconstruction_seg_step = this%delta_a_len(k_use)
      END IF

      CALL profiler_set_stop_time('reconstruction_seg_step', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Take a Levenberg-Marquardt step.
!>
!>  Determines the normalized change in parameters for a Levenberg-Marquardt
!>  step.
!>
!>  @param[inout] this      A @ref reconstruction_class instance.
!>  @param[in]    k_use     Number of singular values to use.
!>  @param[in]    step_size Current step size limit.
!>  @param[out]   delta_a   Normalized change in parameter.
!>  @returns The step size used.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_lm_step(this, k_use, step_size, delta_a)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: reconstruction_lm_step
      CLASS (reconstruction_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                         :: k_use
      REAL (rprec), INTENT(in)                    :: step_size
      REAL (rprec), DIMENSION(:), INTENT(out)     :: delta_a

!  local variables
      REAL (rprec)                                :: lambda
      REAL (rprec), DIMENSION(:), ALLOCATABLE     :: utdote
      REAL (rprec)                                :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      this%lm_ratio = 0.0

      IF ((step_size .ge. 0.0) .and.                                           &
     &    (this%delta_a_len(k_use) .gt. step_size)) THEN

!  Define a default value of lambda in case the root find does not work well.
         lambda =                                                              &
     &      (this%j_svd_w(1)*this%delta_a_len(k_use)/step_size)**2.0

         ALLOCATE(utdote(SIZE(this%e, 1)))
         utdote = MATMUL(this%e(:,this%current_step), this%j_svd_u)

!  Find the L-M parameter lambda that corresponds to a step length of step_size.
         lambda = this%lm_rootfind(k_use, lambda, step_size, utdote)

!  Find the step
         utdote(1:k_use) = utdote(1:k_use)*this%j_svd_w(1:k_use)               &
     &                   / (this%j_svd_w(1:k_use)**2.0 + lambda)

         delta_a = MATMUL(utdote(1:k_use), this%j_svd_vt(1:k_use,:))
         reconstruction_lm_step = SQRT(DOT_PRODUCT(delta_a, delta_a))

         this%lm_ratio(1:k_use,1) = lambda
         this%lm_ratio(1:k_use,2) = this%j_svd_w(1:k_use)**2.0
         this%lm_ratio(1:k_use,3) = this%lm_ratio(1:k_use,1)                   &
     &                            / this%lm_ratio(1:k_use,2)

         DEALLOCATE(utdote)
      ELSE
         delta_a = this%delta_a(:,k_use)
         reconstruction_lm_step = this%delta_a_len(k_use)
      END IF

      CALL profiler_set_stop_time('reconstruction_lm_step', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Root find for Levenberg-Marquardt step.
!>
!>  Find the root of the @ref reconstruction_lm_function for a
!>  Levenberg-Marquardt step.
!>
!>  @param[inout] this           A @ref reconstruction_class instance.
!>  @param[in]    k_use          Number of singular values to use.
!>  @param[in]    lambda_default The default lambda.
!>  @param[in]    step_size      Current step size limit.
!>  @param[in]    utdote         Vector of U^T * e
!>  @returns The root lambda.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_lm_rootfind(this, k_use, lambda_default,         &
     &                                    step_size, utdote)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: reconstruction_lm_rootfind
      CLASS (reconstruction_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                         :: k_use
      REAL (rprec), INTENT(in)                    :: lambda_default
      REAL (rprec), INTENT(in)                    :: step_size
      REAL (rprec), DIMENSION(:), INTENT(in)      :: utdote

!  local variables
      INTEGER                                     :: i
      REAL (rprec), DIMENSION(:), ALLOCATABLE     :: f_sqrd
      REAL (rprec)                                :: step_size_sqrd
      REAL (rprec)                                :: f_0
      REAL (rprec)                                :: f_2
      REAL (rprec)                                :: lambda_0
      REAL (rprec)                                :: lambda_2
      REAL (rprec)                                :: lambda_try
      REAL (rprec)                                :: lambda_new
      REAL (rprec)                                :: f_try, f_new
      REAL (rprec)                                :: s
      REAL (rprec)                                :: start_time

!  local parameters
      REAL (rprec), PARAMETER :: d_lambda_min = 1.0E-6_rprec

!  Start of executable code
      start_time = profiler_get_start_time()

!  Set up stuff for the function reconstruction_lm_function for root find.
      ALLOCATE(f_sqrd(k_use))

!  Put (U^t*e)^2 in the function.
      f_sqrd = utdote(1:k_use)**2.0
      step_size_sqrd = step_size**2.0

      reconstruction_lm_rootfind = lambda_default

!  Find the bracketing values of lambda. Look for a small value of lambda, to
!  give a positive function value. f_0 should be greater than zero, as the step
!  size at k_use is larger than step_size.
      lambda_0 = 0.0
      f_0 = this%lm_function(f_sqrd, step_size_sqrd, lambda_0)
      IF (f_0 .lt. 0.0) THEN
         CALL err_warn('reconstruction_lm_rootfind: f_0 < 0.0')
         DEALLOCATE(f_sqrd)
         CALL profiler_set_stop_time('reconstruction_lm_rootfind',             &
     &                               start_time)
         RETURN
      END IF

!  Look for a large value of lambda, to give a negative function value.
      lambda_2 = this%j_svd_w(1)**2.0
      f_2 = this%lm_function(f_sqrd, step_size_sqrd, lambda_2)
      DO i = 1, 20
         IF (f_2 .le. 0.0) THEN
            EXIT
         ELSE
            lambda_2 = 4.0*lambda_2
            f_2 = this%lm_function(f_sqrd, step_size_sqrd, lambda_2)
         END IF
      END DO
      IF (f_2 .gt. 0.0) THEN
         CALL err_warn('reconstruction_lm_rootfind: f_2 > 0.0')
         DEALLOCATE(f_sqrd)
         CALL profiler_set_stop_time('reconstruction_lm_rootfind',             &
     &                               start_time)
         RETURN
      END IF

!  Check that f_0 and f_2 actually bracket.
      IF (f_0*f_2 .gt. 0.0) THEN
         CALL err_warn('reconstruction_lm_rootfind: fail to bracket')
         DEALLOCATE(f_sqrd)

         CALL profiler_set_stop_time('reconstruction_lm_rootfind',             &
     &                               start_time)
         RETURN
      END IF

!  Find the root using the Ridder method. Numerical Recipes.
      reconstruction_lm_rootfind = -9.99E99_rprec
      DO i = 1, 99
         lambda_try = 0.5*(lambda_0 + lambda_2)
         f_try = this%lm_function(f_sqrd, step_size_sqrd, lambda_try)
         s = SQRT(f_try**2.0 - f_0*f_2)
         IF (s .eq. 0.0) THEN
            EXIT
         END IF

         lambda_new = lambda_try + (lambda_try - lambda_0)*                    &
     &                             (SIGN(1.0_rprec, f_0 - f_2)*f_try/s)

         IF (ABS(lambda_new - reconstruction_lm_rootfind) .le.                 &
     &       d_lambda_min) THEN
            EXIT
         END IF

         reconstruction_lm_rootfind = lambda_new
         f_new = this%lm_function(f_sqrd, step_size_sqrd, lambda_new)
         IF (f_new .eq. 0.0) THEN
            EXIT
         ELSE IF (SIGN(f_try, f_new) .ne. f_try) THEN
            lambda_0 = lambda_try
            f_0 = f_try
            lambda_2 = lambda_new
            f_2 = f_new
         ELSE IF (SIGN(f_0, f_new) .ne. f_0) THEN
            lambda_2 = lambda_new
            f_2 = f_new
         ELSE IF (SIGN(f_2, f_new) .ne. f_2) THEN
            lambda_0 = lambda_new
            f_0 = f_new
         ELSE
            STOP 'reconstruction_lm_rootfind: should never get here'
         END IF

         IF (ABS(lambda_0 - lambda_2) .le. d_lambda_min) THEN
            EXIT
         END IF
      END DO

      DEALLOCATE(f_sqrd)

      CALL profiler_set_stop_time('reconstruction_lm_rootfind',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Levenberg-Marquardt function.
!>
!>  Function used by @ref reconstruction_lm_rootfind for a
!>  Levenberg-Marquardt step.
!>
!>  @param[inout] this           A @ref reconstruction_class instance.
!>  @param[in]    f_sqrd         f^2
!>  @param[in]    step_size_sqrd Square of the current step size.
!>  @param[in]    lambda         Lambda value.
!>  @returns F(lambda)
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_lm_function(this, f_sqrd, step_size_sqrd,        &
     &                                    lambda)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: reconstruction_lm_function
      CLASS (reconstruction_class), INTENT(inout) :: this
      REAL (rprec), DIMENSION(:), INTENT(in)      :: f_sqrd
      REAL (rprec), INTENT(in)                    :: step_size_sqrd
      REAL (rprec), INTENT(in)                    :: lambda

!  local variables
      INTEGER                                     :: i
      REAL (rprec)                                :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      reconstruction_lm_function = 0.0
      DO i = 1, SIZE(f_sqrd)
         reconstruction_lm_function = reconstruction_lm_function               &
     &      + f_sqrd(i)*(this%j_svd_w(i)/(this%j_svd_w(i)**2.0 +               &
     &                                    lambda))**2.0
      END DO

      reconstruction_lm_function = reconstruction_lm_function                  &
     &                           - step_size_sqrd

      CALL profiler_set_stop_time('reconstruction_lm_function',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Step the reconstruction.
!>
!>  Advance the reconstruction by a single step. This is the main V3FIT
!>  algorithm.
!>
!>  @param[inout] this       A @ref reconstruction_class instance.
!>  @param[inout] signals    Array of @ref signal objects.
!>  @param[inout] locks      Array of @ref lock objects.
!>  @param[inout] a_model    A @ref model instance.
!>  @param[inout] gaussp     Array of @ref guassian_process::gaussp_class
!>                           objects.
!>  @param[inout] params     Array of @ref v3fit_params::param_class objects.
!>  @param[inout] eq_steps   Number of steps the equilibrium took.
!>  @param[in]    iou        Input/output file to write log messages to.
!>  @param[in]    recon_comm MPI communicator for the child jacobian processes.
!>  @param[in]    eq_comm    MPI communicator for the child equilibrium
!>                           processes.
!>  @returns True if the step was successfull and false otherwise.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_step(this, signals, derived_params,              &
     &                             locks, a_model, gaussp, params,             &
     &                             eq_steps, iou, recon_comm, eq_comm)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL :: reconstruction_step
      CLASS (reconstruction_class), INTENT(inout)        :: this
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      TYPE (param_pointer), DIMENSION(:), INTENT(in)     ::                    &
     &   derived_params
      TYPE (param_pointer), DIMENSION(:), INTENT(in)     :: locks
      CLASS (model_class), POINTER                       :: a_model
      TYPE (gaussp_class_pointer), DIMENSION(:), INTENT(inout) ::              &
     &   gaussp
      TYPE (param_pointer), DIMENSION(:), INTENT(inout)  :: params
      INTEGER, INTENT(inout)                             :: eq_steps
      INTEGER, INTENT(in)                                :: iou
      INTEGER, INTENT(in)                                :: recon_comm
      INTEGER, INTENT(in)                                :: eq_comm

!  local variables
      INTEGER                                            :: k_use
      INTEGER                                            :: i
      INTEGER                                            :: j
      REAL (rprec)                                       :: step_use
      REAL (rprec)                                       :: start_time
      LOGICAL                                            :: temp
      REAL (rprec), DIMENSION(4)                         :: temp_array
      INTEGER                                            :: error

!  Start of executable code
      start_time = profiler_get_start_time()
      reconstruction_step = .false.

!  Evaluate the quasi-Newton step.
      k_use = this%eval_step(signals, derived_params, locks, a_model,          &
     &                       gaussp, params, eq_steps, iou, recon_comm,        &
     &                       eq_comm)
      this%num_sv(this%current_step + 1) = k_use

!  Sync the results of the jacobian and singular value decomposition to the
!  child processes.
#if defined(MPI_OPT)
      CALL MPI_BCAST(mpi_sync_task, 1, MPI_INTEGER, 0, recon_comm,             &
     &               error)
      CALL this%sync_svd(recon_comm)
      DO i = 1, SIZE(params)
         CALL param_sync_delta(params(i)%p, recon_comm)
      END DO
#endif

!  Set the initial step size.
      step_use = this%step_max

      DO i = 1, reconstruction_max_step_try

         IF (this%try_step(signals, derived_params, locks, a_model,            &
     &                     gaussp, params, eq_steps, step_use, iou,            &
     &                     recon_comm, eq_comm)) THEN

!  Equilibrium converged

            IF (this%get_dg2() .ge. 0.0) THEN
               CALL this%eval_f(derived_params, a_model)

!  The signal cache holds the next set of good signal values.
               DO j = 1, SIZE(signals)
                  this%last_values(:,j) = signals(j)%p%modeled
               END DO

               reconstruction_step = .true.
               WRITE (*,*)
               WRITE (*,*) ' *** Step succeeded'
               WRITE (*,*) ' Result of step'
               WRITE (*,1000)
               WRITE (*,1001) this%get_lastg2(), this%get_g2(),                &
     &                        this%get_dg2()

               WRITE (iou,*)
               WRITE (iou,*) ' *** Step succeeded'
               WRITE (iou,*) ' Result of step'
               WRITE (iou,1000)
               WRITE (iou,1001) this%get_lastg2(), this%get_g2(),              &
     &                          this%get_dg2()
               EXIT
            ELSE
!  Equilibrium converged but g^2 increased
               WRITE (*,*) 'Equilibrium converged but g2 increased'
               WRITE (*,1000)
               WRITE (*,1001) this%get_lastg2(), this%get_g2(),                &
     &                        this%get_dg2()
!               WRITE (*,1002) MAXLOC(ABS(delta_a))
               WRITE (iou,*) 'Equilibrium converged but g2 increased'
               WRITE (iou,1000)
               WRITE (iou,1001) this%get_lastg2(), this%get_g2(),              &
     &                          this%get_dg2()
!               WRITE (iou,1002) MAXLOC(ABS(delta_a))
            END IF

         ELSE
!  Equilibrium did not converge
            WRITE (*,*) 'Equilibrium did not converge'
            WRITE (iou,*) 'Equilibrium did not converge'
         END IF

!  If this section was reached, the reconstruction step failed. Reset the
!  current step to the last good step.
         this%current_step = this%current_step - 1

         IF (i .eq. reconstruction_max_step_try) THEN
!  The signals contain cached values. If the step fails recompute the modeled
!  signals so that values written to the recout match the result file.
            temp = this%eval_e(signals, a_model, gaussp, eq_steps, iou,        &
     &                         eq_comm)

            WRITE (*,*) 'Reconstruction step failed.'
            WRITE (iou,*) 'Reconstruction step failed.'
            EXIT
         END IF

!  Retry step with a smaller stepsize.
         step_use = step_use/2.0
         WRITE (*,*) 'Changing max step size to ', step_use
         WRITE (iou,*) 'Changing max step size to ', step_use
      END DO

      CALL profiler_set_stop_time('reconstruction_step', start_time)

1000  FORMAT('g^2 old',7x,'g^2 new',7x,'g^2 diff')
1001  FORMAT(es12.5,2x,es12.5,2x,es12.5)
1002  FORMAT('Change vrnc value for rp number ',i4)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Try step size.
!>
!>  Sample reconstruction by trying to take a specified step size.
!>
!>  @param[inout] this        A @ref reconstruction_class instance.
!>  @param[inout] signals     Array of @ref signal objects.
!>  @param[inout] locks       Array of @ref lock objects.
!>  @param[inout] a_model     A @ref model instance.
!>  @param[inout] params      Array of @ref parameter objects.
!>  @param[inout] eq_steps    Number of steps for the equilibrium to take.
!>  @param[in]    max_step    Upper limit of the step size to attemp.
!>  @param[in]    iou         Input/output file to write log messages to.
!>  @param[in]    recon_comm  MPI communicator for the child jacobian processes.
!>  @param[in]    eq_comm     MPI communicator for the child equilibrium
!>                            processes.
!>  @param[out]   param_value Parameter corresponding to the smallest g^2
!>                             value.
!>  @returns The lowest q^2 value.
!-------------------------------------------------------------------------------
      FUNCTION reconstruction_try_step(this, signals, derived_params,          &
     &                                 locks, a_model, gaussp, params,         &
     &                                 eq_steps, max_step, iou,                &
     &                                 recon_comm, eq_comm)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL :: reconstruction_try_step
      CLASS (reconstruction_class), INTENT(inout)        :: this
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      TYPE (param_pointer), DIMENSION(:), INTENT(in)     ::                    &
     &   derived_params
      TYPE (param_pointer), DIMENSION(:), INTENT(in)     :: locks
      CLASS (model_class), POINTER                       :: a_model
      TYPE (gaussp_class_pointer), DIMENSION(:), INTENT(inout) ::              &
     &   gaussp
      TYPE (param_pointer), DIMENSION(:), INTENT(inout)  :: params
      INTEGER, INTENT(inout)                             :: eq_steps
      REAL (rprec), INTENT(inout)                        :: max_step
      INTEGER, INTENT(in)                                :: iou
      INTEGER, INTENT(in)                                :: recon_comm
      INTEGER, INTENT(in)                                :: eq_comm

!  local variables
      INTEGER                                            :: i
      INTEGER                                            :: mpi_rank
      INTEGER                                            :: mpi_size
      INTEGER                                            :: error
      LOGICAL                                            :: converged
      INTEGER                                            :: k_use
      REAL (rprec)                                       :: step_use
      REAL (rprec)                                       :: delta_norm
      REAL (rprec), DIMENSION(:), ALLOCATABLE            :: delta_a
      REAL (rprec)                                       :: best_g2
      REAL (rprec)                                       :: temp_g2
      INTEGER                                            :: best_index
      REAL (rprec), DIMENSION(:), ALLOCATABLE            :: param_value
      REAL (rprec)                                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (this%use_central) THEN
         delta_norm = 2.0
      ELSE
         delta_norm = 1.0
      END IF

#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(recon_comm, mpi_rank, error)
      CALL MPI_COMM_SIZE(recon_comm, mpi_size, error)
#else
      mpi_rank = 0
      mpi_size = 1
#endif

      k_use = this%num_sv(this%current_step + 1)

!  Change the step to use based on the mpi_rank. This heuristic sub divids the
!  max step size to one step just under half the size. For instance if the max
!  step size is 100 and their are two processes, each process will use the
!  following step sizes.
!
!    Process   Step Size
!          0       100.0
!          1        75.0
!
!  If the are 4 processes, the step sizes will become.
!
!    Process   Step Size
!          0       100.0
!          1        87.5
!          2        75.0
!          3        62.5
!
!  If none of the processes manages to lower g^2, the max step size is halved in
!  reconstruction and the process repeats on the trucated step size. Step size
!  cannot be increased beyond the max step size due to the check in the step
!  types. See the reconstruction_**_step functions.
      step_use = max_step - mpi_rank*max_step/(2*mpi_size)

      ALLOCATE(param_value(SIZE(params)))
      ALLOCATE(delta_a(SIZE(params)))

!  Save the parameter values.
      DO i = 1, SIZE(params)
         param_value(i) = param_get_value(params(i)%p, a_model)
      END DO

!  Step the parameters using the specified step method.
      SELECT CASE (this%step_type)

         CASE (reconstruction_sl_step_type)
            step_use = this%sl_step(k_use, step_use, delta_a)
            this%exp_g2(this%current_step + 1) =                               &
     &         this%get_exp_g2(delta_a)
            this%step_size(this%current_step + 1) = step_use
            CALL this%write_step('sl', iou)

         CASE (reconstruction_lm_step_type)
            step_use = this%lm_step(k_use, step_use, delta_a)
            this%exp_g2(this%current_step + 1) =                               &
     &         this%get_exp_g2(delta_a)
            this%step_size(this%current_step + 1) = step_use
            CALL this%write_step('lm', iou)

         CASE (reconstruction_seg_step_type)
            step_use = this%seg_step(k_use, step_use, delta_a)
            this%exp_g2(this%current_step + 1) =                               &
     &         this%get_exp_g2(delta_a)
            this%step_size(this%current_step + 1) = step_use
            CALL this%write_step('seg', iou)

      END SELECT

!  Rank zero will attempt to take the largest step. Once the largest step is
!  determined, broadcast to children. This avoids the children taking redundant
!  steps.
      IF (mpi_rank .eq. 0) THEN
#if defined(MPI_OPT)
         CALL MPI_BCAST(mpi_step_task, 1, MPI_INTEGER, 0, recon_comm,          &
     &                  error)
         CALL MPI_BCAST(step_use, 1, MPI_REAL8, 0, recon_comm, error)
         CALL MPI_BCAST(eq_steps, 1, MPI_INTEGER, 0, recon_comm, error)
#endif
         max_step = step_use
      END IF

!  Set the new parameter values. ABS(params(j)%p%delta) is the normalization
!  factor. This is not vrnc because the increment function can take a non vrnc
!  step size.
      WRITE (iou,1000)
      DO i = 1, SIZE(params)
         CALL param_set_value(params(i)%p, a_model, param_value(i) +           &
     &           delta_a(i)*delta_norm*ABS(params(i)%p%recon%delta),           &
     &           eq_comm, this%use_central)
         WRITE (iou,1001) delta_a(i), param_get_name(params(i)%p,              &
     &                                               a_model)
      END DO
      DO i = 1, SIZE(locks)
         CALL param_set_lock_value(locks(i)%p, a_model, eq_comm)
      END DO

!  Reconverge the equilibrium. Increment the current step so
!  reconstruction_eval_e writes the e array to the correct index.
      this%current_step = this%current_step + 1
      converged = this%eval_e(signals, a_model, gaussp, eq_steps, iou,         &
     &                        eq_comm)

!  Search all the processes for the lowest value of g^2.
      best_index = -1
#if defined(MPI_OPT)
      IF (mpi_rank .gt. 0) THEN
         CALL MPI_SSEND(converged, 1, MPI_LOGICAL, 0, mpi_rank,                &
     &                 recon_comm, error)

!  If the equilibrium converged send the value of g^2.
         IF (converged) THEN
            best_g2 = this%get_g2()
            WRITE (*,1002) mpi_rank, best_g2, step_use
            CALL MPI_SSEND(best_g2, 1, MPI_REAL8, 0, mpi_rank,                 &
     &                    recon_comm, error)
         END IF

      ELSE
#endif
         IF (converged) THEN
            best_index = 0
            best_g2 = this%get_g2()
            WRITE (*,1002) mpi_rank, best_g2, step_use
         END IF

#if defined(MPI_OPT)
         DO i = 1, mpi_size - 1
            CALL MPI_RECV(converged, 1, MPI_LOGICAL, i, i, recon_comm,         &
     &                    MPI_STATUS_IGNORE, error)

!  If the equilibrium on that processor converged, receive the value of g^2 and
!  reset the best index if g^2 is lower.
            IF (converged) THEN
               CALL MPI_RECV(temp_g2, 1, MPI_REAL8, i, i, recon_comm,          &
     &                       MPI_STATUS_IGNORE, error)
               IF (temp_g2 .lt. best_g2) THEN
                  best_index = i
                  best_g2 = temp_g2
               END IF
            END IF
         END DO
      END IF

!  Now that we identified the best index, broadcast that values to all the
!  processes.
      CALL MPI_BCAST(best_index, 1, MPI_INTEGER, 0, recon_comm, error)

      IF (mpi_rank .eq. 0) THEN
         WRITE (*,1003) best_index
         IF (best_index .gt. 0) THEN
!  Recieve the best parameters from the best process and sync the model. Store
!  these into param_value. For the zeroth rank, the old parameters were saved in
!  reconstruction_step.
            DO i = 1, SIZE(params)
               CALL param_sync_child(params(i)%p, a_model, best_index,         &
     &                               recon_comm, eq_comm,                      &
     &                               this%use_central)
            END DO
            CALL MPI_RECV(this%e(:,this%current_step), SIZE(this%e, 1),        &
     &                    MPI_REAL8, best_index, best_index, recon_comm,       &
     &                    MPI_STATUS_IGNORE, error)
            CALL MPI_RECV(this%exp_g2(this%current_step), 1, MPI_REAL8,        &
     &                    best_index, best_index, recon_comm,                  &
     &                    MPI_STATUS_IGNORE, error)

!  For some reason, this needs to be before the model is synced or it can dead
!  lock.
            DO i = 1, SIZE(signals)
               CALL signals(i)%p%sync_child(best_index, recon_comm)
            END DO

!  FIXME: Not sure if the model needs to be updated before the lock values are
!         are set.
            DO i = 1, SIZE(locks)
               CALL param_set_lock_value(locks(i)%p, a_model, eq_comm)
            END DO

!  Sync the model from the best index.
            CALL a_model%sync_child(best_index, recon_comm)
            a_model%state_flags = model_state_all_off
         END IF

!  Ensure that parent had a chance sync the child equilibrium before it gets
!  reset. See below.
         CALL MPI_BARRIER(recon_comm, error)

!  If the equilibrium failed to converge reset the parameters and equilibrium.
         IF ((best_index     .lt. 0) .or.                                      &
     &       (this%get_dg2() .lt. 0.0)) THEN
!  The step failed, reset the param values so that they remain in sync with the
!  step.
             DO i = 1, SIZE(params)
                CALL param_set_value(params(i)%p, a_model,                     &
     &                               param_value(i), eq_comm,                  &
     &                               this%use_central)
             END DO
             DO i = 1, SIZE(locks)
                CALL param_set_lock_value(locks(i)%p, a_model, eq_comm)
             END DO

!  Reset the model state back to the last step. The state of the last step was
!  saved before the jacobian was computed. This needs to be called after the
!  parameters are reset so that the model is flagged to the converged state.
!  Otherwise the model will try to converge again producing an incorrect wout
!  file.
             CALL a_model%reset_state
         END IF
      ELSE

         IF (mpi_rank .eq. best_index) THEN
!  Send the best result back to the parent process.
            DO i = 1, SIZE(params)
               CALL param_sync_child(params(i)%p, a_model, best_index,         &
     &                               recon_comm, eq_comm,                      &
     &                               this%use_central)
            END DO
            CALL MPI_SSEND(this%e(:,this%current_step), SIZE(this%e, 1),       &
     &                     MPI_REAL8, 0, mpi_rank, recon_comm, error)
            CALL MPI_SSEND(this%exp_g2(this%current_step), 1, MPI_REAL8,       &
     &                     0, mpi_rank, recon_comm, error)

!  For some reason, this needs to be before the model is synced or it can dead
!  lock.
            DO i = 1, SIZE(signals)
               CALL signals(i)%p%sync_child(best_index, recon_comm)
            END DO

!  Sync the model from the best index. The barrier ensures that child's wout
!  file was copied before the child resets it. See model sync above.
            CALL a_model%sync_child(mpi_rank, recon_comm)
         END IF

!  Reset everything in case this step failed. When the next jacobian is
!  computed, this will be resynced.
         DO i = 1, SIZE(params)
            CALL param_set_value(params(i)%p, a_model, param_value(i),         &
     &                           eq_comm, this%use_central)
         END DO
         DO i = 1, SIZE(locks)
            CALL param_set_lock_value(locks(i)%p, a_model, eq_comm)
         END DO

!  Ensure that parent had a chance to sync the child equilibrium before it gets
!  reset. See above.
         CALL MPI_BARRIER(recon_comm, error)
         CALL a_model%reset_state
         this%current_step = this%current_step - 1

      END IF
#endif

      reconstruction_try_step = best_index .ge. 0

      DEALLOCATE(param_value)
      DEALLOCATE(delta_a)

      CALL profiler_set_stop_time('reconstruction_try_step', start_time)

1000  FORMAT('Normalized Step',2x,'param_name')
1001  FORMAT(es12.5,5x,a)
1002  FORMAT('  Trying rank: ',i3,' g^2: ',es12.5,' step size: ',es12.5)
1003  FORMAT('  Best index ',i3)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Evaluate the parameter covariance and signal effectiveness.
!>
!>  Calculates the signal effectiveness matrix. This value is stored in
!>  @ref parameter::param_class::sem. To find the signal effectiveness, an
!>  intermediate step computes the parameter covariance. Computes the derived
!>  parameter covariance matrix also.
!>
!>  @param[inout] this           A @ref reconstruction_class instance.
!>  @param[inout] params         Array of @ref parameter objects.
!>  @param[inout] signals        Array of @ref signal objects.
!>  @param[inout] derived_params Array of @ref parameter objects.
!-------------------------------------------------------------------------------
      SUBROUTINE reconstruction_eval_sem(this, params, signals,                &
     &                                   derived_params)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (reconstruction_class), INTENT(inout)        :: this
      TYPE (param_pointer), DIMENSION(:), INTENT(inout)  :: params
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      TYPE (param_pointer), DIMENSION(:), INTENT(inout)  ::                    &
     &   derived_params

!  local variables
      INTEGER                                            :: j
      INTEGER                                            :: i
      REAL (rprec), DIMENSION(:,:), POINTER              :: cp, cd
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE          :: j_dot_cp
      REAL (rprec)                                       :: delta_norm
      REAL (rprec)                                       :: start_time

!  local parameters
      CHARACTER (len=*), PARAMETER ::                                          &
     &   sub_name = 'reconstruction_eval_sem'

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (this%use_central) THEN
         delta_norm = 2.0
      ELSE
         delta_norm = 1.0
      END IF

!  Since the jacobian that will be used is normalized, the signal covaiance
!  must be normalized as well. This results in an idenity matrix of
!  SIZE(signals) X SIZE(signals).
!
!    C_p^-1 = J^T * C_s^-1 * J -> A^T * I * A = A^T * A                      (1)
!
!  Equation 3 in Hanson et. al. doi:10.1088/0029-5515/49/7/075031
!  Note: This is the same as the hessian that was computed for the
!  reconstruction. Use that value to avoid a matrix multiplication.
      ALLOCATE(cp(SIZE(params),SIZE(params)))
      cp = this%hessian

!  Invert C_p^-1 to get the parameter covariance matrix.
      CALL this%invert_matrix(cp, sub_name, 'C_p^-1')

!  From the parameter covariance matrix, map the reconstructed uncertainty to
!  the derived parameters by the derived jacobian.
!
!    C_d = J' * C_p * J'^T                                                   (2)
      IF (SIZE(derived_params) .gt. 0) THEN
         ALLOCATE(cd(SIZE(derived_params), SIZE(derived_params)))
         cd = MATMUL(this%derived_jacobian,                                    &
     &               MATMUL(cp, TRANSPOSE(this%derived_jacobian)))

         DO j = 1, SIZE(derived_params)
            derived_params(j)%p%sigma = SQRT(cd(j,j))
            derived_params(j)%p%correlation = cd(j,:)
         END DO

         DEALLOCATE(cd)
      END IF

!  Normalize derived parameter correlation matrix.
      CALL reconstruction_normalize_correlations(derived_params)

!  Store the parameter covariance. Since the normalized signal covariance matrix
!  was used, the parameter covariance needs to be denormalized.
      DO j = 1, SIZE(params)
         params(j)%p%sigma =                                                   &
     &      SQRT(cp(j,j)*(delta_norm*params(j)%p%recon%delta)**2.0)
         DO i = 1, SIZE(params)
            params(j)%p%correlation(i) =                                       &
     &         cp(j,i)*delta_norm*params(j)%p%recon%delta *                    &
     &                 delta_norm*params(i)%p%recon%delta
         END DO
      END DO

!  Normalize reconstruction parameter correlation matrix.
      CALL reconstruction_normalize_correlations(params)

!  Compute signal effectiveness.
      ALLOCATE(j_dot_cp(SIZE(signals),SIZE(params)))
      j_dot_cp = MATMUL(this%jacobian, cp)

!  The signal effectiveness for each parameter is stored in the parameter
!  itself. Equation 6 in Hanson et. al. doi:10.1002/ctpp.200900059
!
!    (J * cp)^2 / (sigma_p^2 * sigma_s^2)                                    (3)
!
!  Due to use of the normalized jacobian and normalized parameter covariance
!  matrix, the 1/sigma_s^2 term is found in the J^2 term. An extra Pi term must
!  be multiplied out. In terms of the normalized quantities, becomes
!
!    (A * cp_n)^2 * pi^2 / sigma_p^2                                         (4)
      DO j = 1, SIZE(params)
         params(j)%p%recon%sem = (j_dot_cp(:,j)**2.0) *                        &
     &      (delta_norm*params(j)%p%recon%delta)**2.0 /                        &
     &      params(j)%p%sigma**2.0
      END DO

      DEALLOCATE(j_dot_cp, cp)

      CALL profiler_set_stop_time(sub_name, start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Invert an M x N matrix.
!>
!>  Perform a pseudo-inversion using a singular value decomposition. This
!>  subroutine destroys the original matrix.
!>
!>  @param[inout] this   A @ref reconstruction_class instance.
!>  @param[inout] matrix The matrix to invert.
!>  @param[in]    sub    The subroutine of the matrix this was called from.
!>  @param[in]    name   The name of the matrix to be inverted.
!-------------------------------------------------------------------------------
      SUBROUTINE reconstruction_invert_matrix(this, matrix, sub, name)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (reconstruction_class), INTENT(inout) :: this
      REAL (rprec), DIMENSION(:,:), POINTER      :: matrix
      CHARACTER (len=*), INTENT(in)              :: sub
      CHARACTER (len=*), INTENT(in)              :: name

!  local variables
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE  :: svd_vt
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE  :: svd_u
      REAL (rprec), DIMENSION(:), ALLOCATABLE    :: svd_work
      INTEGER                                    :: num_svd
      INTEGER                                    :: work_size
      INTEGER                                    :: i, status
      INTEGER                                    :: m, n
      REAL (rprec)                               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      m = SIZE(matrix, 1)
      n = SIZE(matrix, 2)

      ALLOCATE(svd_vt(n,n))
      ALLOCATE(svd_u(m,m))
      ALLOCATE(svd_work(1))

      svd_vt = 0.0
      svd_u = 0.0
      this%svd_w = 0.0
      svd_work = 0.0

!  Find the optimal work size.
      CALL dgesvd('All', 'All', m, n, matrix, m, this%svd_w, svd_u, m,         &
     &            svd_vt, n, svd_work, -1, status)
      work_size = INT(svd_work(1))
      DEALLOCATE(svd_work)
      ALLOCATE(svd_work(work_size))

!  Factor the matrix to M = U * W * V^T
      CALL dgesvd('All', 'All', m, n, matrix, m, this%svd_w, svd_u, m,         &
     &            svd_vt, n, svd_work, work_size, status)
      CALL assert_eq(0, status, sub // ': dgesvd problem when ' //             &
     &               'inverting ' // name)

!  If the matrix is rectangular the inverse matrix will be transposed.
      DEALLOCATE(matrix)
      ALLOCATE(matrix(n,m))

!  Invert the nonzero elements of the W matrix.
      matrix = 0
      DO i = 1, SIZE(this%svd_w)
         IF (this%svd_w(i) .gt. 0.0 .and.                                      &
     &       this%svd_w(i)/this%svd_w(1) .ge. this%cut_inv_svd) THEN
            matrix(i,i) = 1.0/this%svd_w(i)
         END IF
      END DO

!  Invert the matric M^-1 = V * W^-1 * U^T
      matrix = MATMUL(TRANSPOSE(svd_vt), MATMUL(matrix,                        &
     &                                          TRANSPOSE(svd_u)))
      DEALLOCATE(svd_vt, svd_u, svd_work)

      CALL profiler_set_stop_time('reconstruction_invert_matrix',              &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Normalize a correlation matrix.
!>
!>  Normalizes a correlation matrix by dividing by the sigmas for that element.
!>  This method is static so it doesn't require a this parameter.
!>
!>  @param[inout] params Array of parameter to normalize the correlations of.
!-------------------------------------------------------------------------------
      SUBROUTINE reconstruction_normalize_correlations(params)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_pointer), DIMENSION(:), INTENT(inout) :: params

!  local variables
      INTEGER                                           :: j, i
      REAL (rprec)                                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      DO j = 1, SIZE(params)
         params(j)%p%correlation = params(j)%p%correlation                     &
     &                           / params(j)%p%sigma
         DO i = 1, SIZE(params)
            params(j)%p%correlation(i)                                         &
     &         = params(j)%p%correlation(i)/params(i)%p%sigma
         END DO
      END DO

      CALL profiler_set_stop_time(                                             &
     &        'reconstruction_normalize_correlations', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write out a reconstruction.
!>
!>  Write out the final information about a reconstruction. This writes out a
!>  table of the g^2, expected g^2, number of sigular values used and the
!>  normalized step size used.
!>
!>  @param[in] this A @ref reconstruction_class instance.
!>  @param[in] iou  Input/output file to write final result to.
!-------------------------------------------------------------------------------
      SUBROUTINE reconstruction_write(this, iou)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (reconstruction_class), INTENT(in) :: this
      INTEGER, INTENT(in)                      :: iou

!  local variables
      INTEGER                                  :: i
      REAL (rprec)                             :: temp_g2
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (*,*)
      WRITE (iou,*)
      WRITE (*,*) ' *** Reconstruction Steps'
      WRITE (iou,*) ' *** Reconstruction Steps'
      WRITE (*,1000)
      WRITE (iou,1000)

      temp_g2 = DOT_PRODUCT(this%e(:,0), this%e(:,0))
      WRITE (*,1001) 0, temp_g2
      WRITE (iou,1001) 0, temp_g2

      DO i = 1, this%current_step
         temp_g2 = DOT_PRODUCT(this%e(:,i), this%e(:,i))
         WRITE (*,1001) i, temp_g2, this%exp_g2(i), this%num_sv(i),            &
     &                  this%step_size(i)
         WRITE (iou,1001) i, temp_g2, this%exp_g2(i), this%num_sv(i),          &
     &                    this%step_size(i)
      END DO

      CALL profiler_set_stop_time('reconstruction_write', start_time)

1000  FORMAT('irstep',2x,'g^2',11x,'Expected g^2',4x,'sv ',2x,                 &
     &       'normalized step size')
1001  FORMAT(i6,2(2x,es12.5),2x,i3,2x,es12.5)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write out a reconstruction step attempt.
!>
!>  Write out information about a reconstruction step. This writes the number of
!>  singular values use, the step type, the normalized step size and the
!>  expected g^2.
!>
!>  @param[in] this      A @ref reconstruction_class instance.
!>  @param[in] step_type The step type used.
!>  @param[in] iou       Input/output file to write final result to.
!-------------------------------------------------------------------------------
      SUBROUTINE reconstruction_write_step1(this, step_type, iou)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (reconstruction_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)            :: step_type
      INTEGER, INTENT(in)                      :: iou

!  local variables
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (*,*)
      WRITE (*,1000) this%num_sv(this%current_step + 1)
      WRITE (*,1001) step_type, this%step_size(this%current_step + 1)
      WRITE (*,1002) this%exp_g2(this%current_step + 1)

      WRITE (iou,*)
      WRITE (iou,1000) this%num_sv(this%current_step + 1)
      WRITE (iou,1001) step_type, this%step_size(this%current_step + 1)
      WRITE (iou,1002) this%exp_g2(this%current_step + 1)

      CALL profiler_set_stop_time('reconstruction_write_step1',                &
     &                            start_time)

1000  FORMAT('Number of singular values used: ',i3)
1001  FORMAT(a4,': Step size limited to ',es12.5)
1002  FORMAT('  Expected g^2 = ',es12.5)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write out a final reconstruction step.
!>
!>  Write out information about a reconstruction step. This writes the number of
!>  singular values use, the step type, the normalized step size and the
!>  expected g^2.
!>
!>  @param[in] this      A @ref reconstruction_class instance.
!>  @param[in] iou       Input/output file to write final result to.
!-------------------------------------------------------------------------------
      SUBROUTINE reconstruction_write_step2(this, iou)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (reconstruction_class), INTENT(in) :: this
      INTEGER, INTENT(in)                      :: iou

!  local variables
      CHARACTER (len=21)                       :: jac_format
      INTEGER                                  :: i
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (iou,*)
      WRITE (iou,*) ' *** Jacobian'
      WRITE (jac_format,1000) SIZE(this%jacobian, 2)
      DO i = 1, SIZE(this%jacobian, 1)
         WRITE (iou,jac_format) i, this%jacobian(i,:)
      END DO

      IF (this%step_type .eq. reconstruction_lm_step_type) THEN
         WRITE (iou,*)
         WRITE (iou,*) ' *** Levenberg-Marquardt Parameters'
         DO i = 1, this%num_sv(this%current_step)
            WRITE (iou,1003) i, this%lm_ratio(i,:)
         END DO
      END IF

      WRITE (iou,*)
      WRITE (iou,*) ' *** Jacobian Singular Values'
      WRITE (iou,1001)
      DO i = 1, SIZE(this%j_svd_w)
         WRITE (iou,1002) this%j_svd_w(i), i .le.                              &
     &                    this%num_sv(this%current_step)
      END DO
      WRITE (iou,*)

      WRITE (iou,*)
      WRITE (iou,*) ' *** Derived Jacobian'
      WRITE (jac_format,1000) SIZE(this%derived_jacobian, 2)
      DO i = 1, SIZE(this%derived_jacobian, 1)
         WRITE (iou,jac_format) i, this%derived_jacobian(i,:)
      END DO

      WRITE (iou,*)
      WRITE (iou,*) ' *** Parameter Covarance Singular Values'
      WRITE (iou,1001)
      DO i = 1, SIZE(this%svd_w)
         WRITE (iou,1002) this%svd_w(i),                                       &
     &      this%svd_w(i)/this%svd_w(1) .ge. this%cut_inv_svd
      END DO
      WRITE (iou,*)

      CALL profiler_set_stop_time('reconstruction_write_step2',                &
     &                            start_time)

1000  FORMAT('(i4,',i3,'(2x,es12.5))')
1001  FORMAT('svd value',5x,'used')
1002  FORMAT(es12.5,2x,l)
1003  FORMAT(i3,' lambda = ',es12.5,' W_i^2 = ',es12.5,' ratio = ',            &
     &       es12.5)

      END SUBROUTINE

!*******************************************************************************
!  NETCDF SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Restart from result file.
!>
!>  Restarts the reconstruction from the result file. Note, that there is an
!>  external step counter and an internal step counter. Internally we will
!>  restart current step at zero.
!>
!>  @param[inout] this         A @ref reconstruction_class instance.
!>  @param[in]    result_ncid  NetCDF file id of the result file.
!>  @param[in]    current_step Step index to read variables from.
!>  @param[inout] a_model      A @ref model instance.
!-------------------------------------------------------------------------------
      SUBROUTINE reconstruction_restart(this, result_ncid, current_step,       &
     &                                  signals, derived_params,               &
     &                                  a_model)
      USE ezcdf

      IMPLICIT NONE

!  Declare Arguments
      CLASS (reconstruction_class), INTENT(inout)     :: this
      INTEGER, INTENT(in)                             :: result_ncid
      INTEGER, INTENT(in)                             :: current_step
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      TYPE (param_pointer), DIMENSION(:), INTENT(in)  :: derived_params
      CLASS (model_class), POINTER                    :: a_model

!  Local variables
      REAL (rprec), DIMENSION(:), ALLOCATABLE         :: temp_model
      INTEGER                                         :: i
      INTEGER                                         :: temp_var_id
      INTEGER                                         :: status
      REAL (rprec)                                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      status = nf90_inq_varid(result_ncid, 'signal_model_value',               &
     &                        temp_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_get_var(result_ncid, temp_var_id, this%last_values,        &
     &                      start=(/ 1, 1, current_step /),                    &
     &                      count=(/ 4, SIZE(signals), 1 /))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

!$OMP PARALLEL DO
!$OMP& SCHEDULE(DYNAMIC)
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i)
      DO i = 1, this%last_para_signal
         this%e(i,this%current_step) =                                         &
     &      signals(i)%p%get_e(a_model, .false., this%last_values(:,i))
      END DO
!$OMP END PARALLEL DO
      DO i = this%last_para_signal + 1, SIZE(signals)
         this%e(i,this%current_step) =                                         &
     &      signals(i)%p%get_e(a_model, .false., this%last_values(:,i))
      END DO

      CALL this%eval_f(derived_params, a_model)

      CALL profiler_set_stop_time('reconstruction_restart', start_time)

      END SUBROUTINE

!*******************************************************************************
!  MPI SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Syncronize state to children.
!>
!>  Syncs data between the parent and child processes. If MPI support is not
!>  compiled in this subroutine reduces to a no op.
!>
!>  @param[inout] this       A @ref reconstruction_class instance.
!>  @param[in]    recon_comm A MPI recon_comm handle.
!-------------------------------------------------------------------------------
      SUBROUTINE reconstruction_sync_state(this, recon_comm)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (reconstruction_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                         :: recon_comm

#if defined(MPI_OPT)
!  local variables
      REAL (rprec)                                :: start_time
      INTEGER                                     :: error

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL MPI_BCAST(this%current_step, 1, MPI_INTEGER, 0, recon_comm,         &
     &               error)
      CALL MPI_BCAST(this%e(:,this%current_step), SIZE(this%e, 1),             &
     &               MPI_REAL8, 0, recon_comm, error)
      CALL MPI_BCAST(this%f(:,this%current_step), SIZE(this%f, 1),             &
     &               MPI_REAL8, 0, recon_comm, error)
      CALL MPI_BCAST(this%last_values, 4*SIZE(this%e, 1), MPI_REAL8, 0,        &
     &               recon_comm, error)

      CALL profiler_set_stop_time('reconstruction_sync_state',                 &
     &                            start_time)

#endif

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Syncronize svd.
!>
!>  Syncs the svd information to the child processes so these processes can
!>  sample parameter space for the step sizes.
!>
!>  @param[inout] this       A @ref reconstruction_class instance.
!>  @param[in]    recon_comm A MPI recon_comm handle.
!-------------------------------------------------------------------------------
      SUBROUTINE reconstruction_sync_svd(this, recon_comm)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (reconstruction_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                         :: recon_comm

#if defined(MPI_OPT)
!  local variables
      REAL (rprec)                                :: start_time
      INTEGER                                     :: error

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL MPI_BCAST(this%j_svd_w, SIZE(this%j_svd_w), MPI_REAL8, 0,           &
     &               recon_comm, error)
      CALL MPI_BCAST(this%j_svd_u, SIZE(this%j_svd_u), MPI_REAL8, 0,           &
     &               recon_comm, error)
      CALL MPI_BCAST(this%j_svd_vt, SIZE(this%j_svd_vt), MPI_REAL8, 0,         &
     &               recon_comm, error)

      CALL MPI_BCAST(this%delta_a_len, SIZE(this%delta_a_len),                 &
     &               MPI_REAL8, 0, recon_comm, error)
      CALL MPI_BCAST(this%delta_a, SIZE(this%delta_a), MPI_REAL8, 0,           &
     &               recon_comm, error)

      CALL MPI_BCAST(this%jacobian, SIZE(this%jacobian), MPI_REAL8, 0,         &
     &               recon_comm, error)
      CALL MPI_BCAST(this%hessian, SIZE(this%hessian), MPI_REAL8, 0,           &
     &               recon_comm, error)

!  Number of signular values has been stored in the next step but the
!  step hasn't been updated yet.
      CALL MPI_BCAST(this%num_sv(this%current_step + 1), 1, MPI_INTEGER,       &
     &               0, recon_comm, error)

      CALL profiler_set_stop_time('reconstruction_sync_svd', start_time)

#endif

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Syncronize child state back to parent.
!>
!>  Syncs data between the parent and child processes. If MPI support is not
!>  compiled in this subroutine reduces to a no op.
!>
!>  @param[inout] this               A @ref reconstruction_class instance.
!>  @param[in]    num_params         Number of reconstruction parameters.
!>  @param[in]    num_signal         Number of signals.
!>  @param[in]    num_derived_params Number of derived parameters.
!>  @param[in]    stride             Length work size for each process.
!>  @param[in]    offset             Start offset for each process.
!>  @param[in]    recon_comm         A MPI recon_comm handle.
!-------------------------------------------------------------------------------
      SUBROUTINE reconstruction_sync_parent(this, params, num_signal,          &
     &                                      num_derived_params, stride,        &
     &                                      offset, recon_comm)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (reconstruction_class), INTENT(inout) :: this
      TYPE (param_pointer), DIMENSION(:), INTENT(inout) :: params
      INTEGER, INTENT(in)                         :: num_signal
      INTEGER, INTENT(in)                         :: num_derived_params
      INTEGER, DIMENSION(:), INTENT(in)           :: stride
      INTEGER, DIMENSION(:), INTENT(in)           :: offset
      INTEGER, INTENT(in)                         :: recon_comm

#if defined(MPI_OPT)
!  local variables
      REAL (rprec)                                :: start_time
      INTEGER                                     :: error
      INTEGER                                     :: mpi_rank
      INTEGER                                     :: i
      INTEGER                                     :: iLow, iHigh

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL MPI_COMM_RANK(recon_comm, mpi_rank, error)

!  Need to be careful here with strides and offsets. The mpi uses C style array
!  notation but the data is laid out in memory using fortran array notation.
!  This means that all offsets need to be zero referenced. For the jacobians,
!  the arrays are 2D so all strides and offsets need to be multiplied by the
!  relevant column size.
      iLow = offset(mpi_rank + 1) + 1
      iHigh = offset(mpi_rank + 1) + stride(mpi_rank + 1)

!  MPI cannot send and recieve to the same buffer. For the root buffer, use the
!  in place specifer. This makes the recieve buffer act as double duty acts as
!  the send and recieve buffer. Otherwise on the root process, the pointers for
!  the buffers would alias.
      IF (mpi_rank .eq. 0) THEN
         CALL MPI_GATHERV(MPI_IN_PLACE,                                        &
     &                    num_signal*stride(mpi_rank + 1), MPI_REAL8,          &
     &                    this%jacobian, num_signal*stride,                    &
     &                    num_signal*offset, MPI_REAL8, 0, recon_comm,         &
     &                    error)

         CALL MPI_GATHERV(MPI_IN_PLACE,                                        &
     &                    num_derived_params*stride(mpi_rank + 1),             &
     &                    MPI_REAL8, this%derived_jacobian,                    &
     &                    num_derived_params*stride,                           &
     &                    num_derived_params*offset, MPI_REAL8, 0,             &
     &                    recon_comm, error)
      ELSE
         CALL MPI_GATHERV(this%jacobian(:,iLow:iHigh),                         &
     &                    num_signal*stride(mpi_rank + 1), MPI_REAL8,          &
     &                    this%jacobian, num_signal*stride,                    &
     &                    num_signal*offset, MPI_REAL8, 0, recon_comm,         &
     &                    error)

         CALL MPI_GATHERV(this%derived_jacobian(:,iLow:iHigh),                 &
     &                    num_derived_params*stride(mpi_rank + 1),             &
     &                    MPI_REAL8, this%derived_jacobian,                    &
     &                    num_derived_params*stride,                           &
     &                    num_derived_params*offset, MPI_REAL8, 0,             &
     &                    recon_comm, error)
      END IF

!  Sync the delta used to find the jacobians.
      IF (mpi_rank .ne. 0) THEN
         DO i = offset(mpi_rank + 1) + 1, offset(mpi_rank + 1) +               &
     &                                    stride(mpi_rank + 1)
            CALL param_send_delta(params(i)%p, i, recon_comm)
         END DO
      END IF

      IF (mpi_rank .eq. 0) THEN
         DO i = offset(1) + stride(1) + 1, SIZE(params)
            CALL param_recv_delta(params(i)%p, i, recon_comm)
         END DO
      END IF

      CALL profiler_set_stop_time('reconstruction_sync_parent',                &
     &                            start_time)

#endif

      END SUBROUTINE

      END MODULE

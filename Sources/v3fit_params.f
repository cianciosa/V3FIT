!*******************************************************************************
!>  @file v3fit_params.f
!>  @brief Contains module @ref v3fit_params.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref param_class
!*******************************************************************************

      MODULE v3fit_params

      USE stel_kinds, only: rprec
      USE data_parameters
      USE model

      IMPLICIT NONE

!*******************************************************************************
!  param module parameters
!*******************************************************************************
!>  No bounding type specified.
      INTEGER, PARAMETER :: param_range_no_type        = -1
!>  Parameter value is bounded by a value.
      INTEGER, PARAMETER :: param_range_value_type     = 0
!>  Parameter value is unbounded.
      INTEGER, PARAMETER :: param_range_infinity_type  = 1
!>  Parameter value is bounded by another parameter.
      INTEGER, PARAMETER :: param_range_parameter_type = 2

!>  Maximum number of attemps to change the parameter increment size.
      INTEGER, PARAMETER :: param_max_increment_steps = 5
!>  Division factor to decrease the step size.
      INTEGER, PARAMETER :: param_div_factor = 10.0

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) parameter base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Class to hold variables needed when a parameter is a reconstruction
!>  parameter.
!-------------------------------------------------------------------------------
      TYPE param_recon_class
!>  The maximum increment size of the parameter for calculating the jacobian.
!>  @see param_increment
         REAL (rprec)                          :: vrnc = 0.0

!  Parameter ranges
!>  Type descriptor of the boundry type for the lower(1) and upper(2) ranges.
!>  @par Possible values are:
!>  * @ref param_range_no_type
!>  * @ref param_range_value_type
!>  * @ref param_range_infinity_type
!>  * @ref param_range_parameter_type
         INTEGER, DIMENSION(2) :: range_type = param_range_no_type
!>  If the @ref param_recon_class::range_type is
!>  @ref param_range_parameter_type, this holds the parameter id's of the
!>  lower(1) and upper(2) boundary. Id's are provided by either the @ref model
!>  or the @ref equilibrium.
         INTEGER, DIMENSION(2) :: range_id = data_no_id
!>  If the @ref param_recon_class::range_type is
!>  @ref param_range_parameter_type, this holds the parameter indices of the
!>  lower(1,:) and upper(2,:) boundary.
         INTEGER, DIMENSION(2,data_max_indices) :: range_indices = 0
!>  If the @ref param_recon_class::range_type is @ref param_range_value_type,
!>  this holds the value of the lower(1) and upper(2) boundary.
         REAL (rprec), DIMENSION(2) :: range_value = 0.0

!>  The contraints can change the normalization size of the patarameter when
!>  incrementing it. Store the actual change in parameter here for
!>  normalization. This value is set by @ref param_increment.
         REAL (rprec)                          :: delta

!>  Stored row of the signal effectiveness matrix.
         REAL (rprec), DIMENSION(:), POINTER   :: sem => null()
      END TYPE

!-------------------------------------------------------------------------------
!>  Class to hold variables needed when a parameter is a locking parameter. The
!>  locking parameter coefficents are stored in the correlation array of the
!>  parent type.
!-------------------------------------------------------------------------------
      TYPE param_locking_class
!>  Parameter id's of the parameter to lock to.
         INTEGER, DIMENSION(:), POINTER   :: ids => null()
!>  The i and j indices of the parameters to lock to.
         INTEGER, DIMENSION(:,:), POINTER :: indices => null()
      END TYPE

!-------------------------------------------------------------------------------
!>  Base class representing a reconstructed parameter. An upper and lower bound
!>  may be set for the parameter value.
!-------------------------------------------------------------------------------
      TYPE param_class
!>  Id number of the parameter. Id's are provided by either the @ref model or
!>  the @ref equilibrium.
         INTEGER                              :: param_id = data_no_id
!>  The i and j indices of the parameter. These are only used if the parameter
!>  is the element of an array.
         INTEGER, DIMENSION(data_max_indices) :: indices = 0

!>  Pointer to the extra variables needed for reconstruction parameters.
         TYPE (param_recon_class), POINTER    :: recon => null()
!>  Pointer to the extra variables needed for locking parameters.
         TYPE (param_locking_class), POINTER  :: locks => null()

!>  Stored value of the parameter uncertainty for the current reconstruction
!>  step. This value is calculated by
!>  @ref reconstruction::reconstruction_eval_sem.
         REAL (rprec)                         :: sigma = 0.0
!>  Stored row of the correlation matrix. When the parameter is interpreted as a
!>  locking parameter, this array contains the parameter coefficents.
         REAL (rprec), DIMENSION(:), POINTER  :: correlation => null()
      END TYPE

!-------------------------------------------------------------------------------
!>  Pointer to a parameter object. Used for creating arrays of signal pointers.
!>  This is needed because fortran does not allow arrays of pointers directly.
!-------------------------------------------------------------------------------
      TYPE param_pointer
!>  Pointer to a @ref param_class. Used for building arrays of @ref param_class
!>  objects.
         TYPE (param_class), POINTER :: p => null()
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the construction of @ref param_class types using
!>  @ref param_construct_basic or @ref param_construct_recon
!-------------------------------------------------------------------------------
      INTERFACE param_construct
         MODULE PROCEDURE param_construct_basic,                               &
     &                    param_construct_recon,                               &
     &                    param_construct_locking
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the writting of @ref param_class data to the result file using
!>  @ref param_write_step_data_1 or @ref param_write_step_data_2
!-------------------------------------------------------------------------------
      INTERFACE param_write_step_data
         MODULE PROCEDURE param_write_step_data_1,                             &
     &                    param_write_step_data_2
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref param_class object.
!>
!>  Allocates memory and initializes a @ref param_class object. Parameters are
!>  converted from strings to an internal id representation by either the
!>  @ref model or the @ref equilibrium. This version is used to construct
!>  basic parameters.
!>
!>  @param[in] a_model    An instance of a @ref model object.
!>  @param[in] param_name The name of the parameter.
!>  @param[in] indices   The array indices of the parameter.
!>  @param[in] num_params Number of parameters. USed to determine the size of
!>                        the @ref param_class::correlation arrays.
!>  @returns A pointer to a constructed @ref param_class object.
!-------------------------------------------------------------------------------
      FUNCTION param_construct_basic(a_model, param_name, indices,             &
     &                               num_params)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), POINTER       :: param_construct_basic
      CLASS (model_class), INTENT(in)   :: a_model
      CHARACTER (len=*), INTENT(in)     :: param_name
      INTEGER, DIMENSION(2), INTENT(in) :: indices
      INTEGER, INTENT(in)               :: num_params

!  local variables
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(param_construct_basic)

      param_construct_basic%param_id =                                         &
     &   a_model%get_param_id(TRIM(param_name))

      ALLOCATE(param_construct_basic%correlation(num_params))
      param_construct_basic%correlation = 0.0

      param_construct_basic%indices = indices

      CALL profiler_set_stop_time('param_construct_basic', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref param_class object.
!>
!>  Allocates memory and initializes a @ref param_class object. Parameters are
!>  converted from strings to an internal id representation by either the
!>  @ref model or the @ref equilibrium. This version is used to construct
!>  reconstruction parameters.
!>
!>  Parameter boundaries are specified by a range type. This range type may be
!>  one of three types. 'infinity' represents an unbounded case. 'value'
!>  represents a bounds by a fixed value. By specifying another parameter name,
!>  the boundary will be bounded by a fixed or varying parameter. The bounding
!>  parameter string is converted into an internal id representation.
!>
!>  @param[in] a_model        An instance of a @ref model object.
!>  @param[in] param_name     The name of the parameter.
!>  @param[in] indices        The array indices of the parameter.
!>  @param[in] vrnc           The maximum incremental step size.
!>  @param[in] range_type     Range type of the bounds. Values can be
!>                            'infinity', 'value' or the name of a parameter.
!>  @param[in] range_indices  The array indices of the boundary parameter.
!>  @param[in] range_value    Value of a boundary.
!>  @param[in] num_signals    Number of signals. Used to determine the size of
!>                            the @ref param_recon_class::sem arrays.
!>  @param[in] num_params     Number of parameters. Used to determine the size
!>                            of the @ref param_class::correlation arrays.
!>  @returns A pointer to a constructed @ref param_class object.
!-------------------------------------------------------------------------------
      FUNCTION param_construct_recon(a_model, param_name, indices,             &
     &                               vrnc, range_type, range_indices,          &
     &                               range_value, num_signals,                 &
     &                               num_params)
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), POINTER :: param_construct_recon
      CLASS (model_class), INTENT(in)             :: a_model
      CHARACTER (len=*), INTENT(in)               :: param_name
      INTEGER, DIMENSION(2), INTENT(in)           :: indices
      REAL (rprec), INTENT(in)                    :: vrnc
      CHARACTER (len=*), DIMENSION(2), INTENT(in) :: range_type
      INTEGER, DIMENSION(2,2), INTENT(in)         :: range_indices
      REAL (rprec), DIMENSION(2), INTENT(in)      :: range_value
      INTEGER, INTENT(in)                         :: num_signals
      INTEGER, INTENT(in)                         :: num_params

!  local variables
      REAL (rprec)                                :: value
      REAL (rprec)                                :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      param_construct_recon => param_construct(a_model, param_name,            &
     &                                         indices, num_params)

      IF (.not.a_model%is_recon_param(                                         &
     &            param_construct_recon%param_id)) THEN
         CALL err_fatal('param_construct: ' // TRIM(param_name) //             &
     &                  ' is an invalid reconstruction ' //                    &
     &                  'parameter')
      END IF

      ALLOCATE(param_construct_recon%recon)
      param_construct_recon%recon%vrnc = vrnc

      SELECT CASE (TRIM(range_type(1)))

         CASE ('value')
            param_construct_recon%recon%range_type(1) =                        &
     &         param_range_value_type
            param_construct_recon%recon%range_value(1) =                       &
     &         range_value(1)
        
         CASE ('infinity')
            param_construct_recon%recon%range_type(1) =                        &
     &         param_range_infinity_type

         CASE DEFAULT
            param_construct_recon%recon%range_type(1) =                        &
     &         param_range_parameter_type
            param_construct_recon%recon%range_id(1) =                          &
     &         a_model%get_param_id(TRIM(range_type(1)))
            param_construct_recon%recon%range_indices(1,:) =                   &
     &         range_indices(1,:)

      END SELECT

      SELECT CASE (TRIM(range_type(2)))

         CASE ('value')
            param_construct_recon%recon%range_type(2) =                        &
     &         param_range_value_type
            param_construct_recon%recon%range_value(2) =                       &
     &         range_value(2)

         CASE ('infinity')
            param_construct_recon%recon%range_type(2) =                        &
     &         param_range_infinity_type

         CASE DEFAULT
            param_construct_recon%recon%range_type(2) =                        &
     &         param_range_parameter_type
            param_construct_recon%recon%range_id(2) =                          &
     &         a_model%get_param_id(TRIM(range_type(2)))
            param_construct_recon%recon%range_indices(2,:) =                   &
     &         range_indices(2,:)

      END SELECT

      ALLOCATE(param_construct_recon%recon%sem(num_signals))
      param_construct_recon%recon%sem = 0.0

!  Check if the inital parameter value is in range.
      value = param_get_value(param_construct_recon, a_model)
      CALL assert(param_is_in_lower_range(param_construct_recon,               &
     &                                    a_model, value),                     &
     &            TRIM(param_get_name(param_construct_recon, a_model))         &
     &            // ' inital value is outside reconstruction parameter'       &
     &            //' lower bound.')

      CALL assert(param_is_in_upper_range(param_construct_recon,               &
     &                                    a_model, value),                     &
     &            TRIM(param_get_name(param_construct_recon, a_model))         &
     &            // ' inital value is outside reconstruction parameter'       &
     &            // ' upper bound.')

      CALL profiler_set_stop_time('param_construct_recon', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref param_class object.
!>
!>  Allocates memory and initializes a @ref param_class object. Parameters are
!>  converted from strings to an internal id representation by either the
!>  @ref model or the @ref equilibrium. This version is used to construct
!>  locking parameters.
!>
!>  @param[in] a_model     An instance of a @ref model object.
!>  @param[in] param_name  The name of the parameter.
!>  @param[in] indices     The array indices of the parameter.
!>  @param[in] set         Array of parameters to lock to.
!>  @param[in] set_indices Array of parameters indices for parameters to lock
!>                         to.
!>  @param[in] set_coeff   Coefficients for the parameters to lock to.
!>  @param[in] eq_comm MPI communicator for the child equilibrium processes.
!>  @returns A pointer to a constructed @ref param_class object.
!-------------------------------------------------------------------------------
      FUNCTION param_construct_locking(a_model, param_name, indices,           &
     &                                 set, set_indices, set_coeff,            &
     &                                 eq_comm)
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), POINTER            :: param_construct_locking
      CLASS (model_class), INTENT(inout)     :: a_model
      CHARACTER (len=*), INTENT(in)          :: param_name
      INTEGER, DIMENSION(2), INTENT(in)      :: indices
      CHARACTER (len=*), DIMENSION(:), INTENT(in) :: set
      INTEGER, DIMENSION(:,:), INTENT(in)    :: set_indices
      REAL (rprec), DIMENSION(:), INTENT(in) :: set_coeff
      INTEGER, INTENT(in)                    :: eq_comm

!  local variables
      INTEGER                                :: i
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      param_construct_locking => param_construct(a_model, param_name,          &
     &                                           indices, SIZE(set))

      IF (.not.a_model%is_recon_param(                                         &
     &            param_construct_locking%param_id)) THEN
         CALL err_fatal('param_construct: ' // TRIM(param_name) //             &
     &                  ' is an invalid locking parameter')
      END IF

      ALLOCATE(param_construct_locking%locks)

      ALLOCATE(param_construct_locking%locks%ids(SIZE(set)))
      DO i = 1, SIZE(set)
         param_construct_locking%locks%ids(i) =                                &
     &      a_model%get_param_id(TRIM(set(i)))
      END DO

      ALLOCATE(param_construct_locking%locks%indices(SIZE(set),                &
     &                                               data_max_indices))
      param_construct_locking%locks%indices = set_indices

!  Use the correlation array as the array to hold the set coefficients. This
!  array was allocated by param_construct.
      param_construct_locking%correlation = set_coeff

!  Set the inital value of the locked parameter.
      CALL param_set_lock_value(param_construct_locking, a_model,              &
     &                          eq_comm)

      CALL profiler_set_stop_time('param_construct_locking', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref param_class object.
!>
!>  Deallocates memory and uninitializes a @ref param_class object.
!>
!>  @param[inout] this A @ref param_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE param_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), POINTER :: this

!  Start of executable code
      this%param_id = data_no_id
      this%indices = 0

      IF (ASSOCIATED(this%recon)) THEN

         IF (ASSOCIATED(this%recon%sem)) THEN
            DEALLOCATE(this%recon%sem)
            this%recon%sem => null()
         END IF

         DEALLOCATE(this%recon)
         this%recon => null()
      END IF

      IF (ASSOCIATED(this%locks)) THEN

         IF (ASSOCIATED(this%locks%ids)) THEN
            DEALLOCATE(this%locks%ids)
            this%locks%ids => null()
         END IF

         IF (ASSOCIATED(this%locks%indices)) THEN
            DEALLOCATE(this%locks%indices)
            this%locks%indices => null()
         END IF

         DEALLOCATE(this%locks)
         this%locks => null()
      END IF

      IF (ASSOCIATED(this%correlation)) THEN
         DEALLOCATE(this%correlation)
         this%correlation => null()
      END IF

      DEALLOCATE(this)

      END SUBROUTINE

!*******************************************************************************
!  SETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Sets the parameter value.
!>
!>  Sets the value of a reconstruction parameter. If the parameter is outside
!>  one of the bounding parameters, the value is clamped to the bounding value.
!>  The resulting bounded parameter value is set by ethier the @ref model or
!>  the @ref equilibrium.
!>
!>  If central differencing is used, both the upper and lower bounds must be
!>  checked. In this case both the upper and lower bounds need to be with in
!>  half the vrnc values. This ensures there is enough room to increment and
!>  decrement the values. If the bounds are so narrow such that the set value
!>  plus/minus half the vrnc is out of range of both bounds, then split the
!>  difference.
!>
!>  @param[in]    this       A @ref param_class instance.
!>  @param[inout] a_model    A @ref model instance.
!>  @param[in]    value      The value to set the parameter to.
!>  @param[in]    eq_comm    MPI communicator for the child equilibrium
!>                           processes.
!>  @param[in]    is_central Central differencing is being used.
!-------------------------------------------------------------------------------
      SUBROUTINE param_set_value(this, a_model, value, eq_comm,                &
     &                           is_central)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(in)     :: this
      CLASS (model_class), INTENT(inout) :: a_model
      REAL (rprec), INTENT(in)           :: value
      INTEGER, INTENT(in)                :: eq_comm
      LOGICAL, INTENT(in)                :: is_central

!  Local variables
      REAL (rprec)                       :: set_value
      REAL (rprec)                       :: off_set_value
      REAL (rprec)                       :: upper_value
      REAL (rprec)                       :: lower_value
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      set_value = value

      IF (is_central) THEN
         off_set_value = this%recon%vrnc/2.0
      ELSE
         off_set_value = 0.0
      END IF

!  Check if the value is within the lower range. If the value is outside,
!  truncate the value to the lower range.
      IF (this%recon%range_type(1) .ne. param_range_infinity_type) THEN
         lower_value = param_get_lower_range_value(this, a_model)

         IF (set_value - off_set_value .lt. lower_value) THEN

!  The set value must be at least the lower value plus the offset value. When
!  not using central differencing, the the offset is zero.
            set_value = lower_value + off_set_value

!  Need to check the upper range if using central differencing.
            IF (is_central .and. (this%recon%range_type(2) .ne.                &
     &                            param_range_infinity_type)) THEN

               upper_value = param_get_upper_range_value(this, a_model)

               IF (set_value + off_set_value .gt. upper_value) THEN
!  Both bounds are two narrow to fit the new value. Split the difference.
                  set_value = (upper_value + lower_value)/2.0
               END IF
            END IF

            CALL a_model%set_param(this%param_id, this%indices(1),             &
     &                             this%indices(2), set_value,                 &
     &                             eq_comm)

            CALL profiler_set_stop_time('profiler_get_start_time',             &
     &                                  start_time)
            RETURN
         END IF
      END IF

!  The value is in the lower range. Check upper range. If the value is outside,
!  truncate the value to the upper range.
      IF (this%recon%range_type(2) .ne. param_range_infinity_type) THEN
         upper_value = param_get_upper_range_value(this, a_model)

         IF (set_value + off_set_value .gt. upper_value) THEN

!  The set value must be no more than the upper value minus the offset value.
!  When not using central differencing, the the offset is zero. There is no need
!  to check the lower range because it has already been confirmed that the lower
!  range is negative infinity.
            set_value = upper_value - off_set_value

            CALL a_model%set_param(this%param_id, this%indices(1),             &
     &                             this%indices(2), set_value,                 &
     &                             eq_comm)

            CALL profiler_set_stop_time('profiler_get_start_time',             &
     &                                  start_time)
            RETURN
         END IF
      END IF

!  The value is in range. Set the value to this.
      CALL a_model%set_param(this%param_id, this%indices(1),                   &
     &                       this%indices(2), set_value, eq_comm)

      CALL profiler_set_stop_time('profiler_get_start_time', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Sets the locking parameter value.
!>
!>  Sets the value of a locking parameter. These parameters are locked to a
!>  linear combination of other set parameters.
!>
!>  @param[in]    this    A @ref param_class instance.
!>  @param[inout] a_model A @ref model instance.
!>  @param[in]    eq_comm MPI communicator for the child equilibrium processes.
!-------------------------------------------------------------------------------
      SUBROUTINE param_set_lock_value(this, a_model, eq_comm)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(in)     :: this
      CLASS (model_class), INTENT(inout) :: a_model
      INTEGER, INTENT(in)                :: eq_comm

!  Local variables
      INTEGER                            :: i
      REAL (rprec)                       :: temp
      REAL (rprec)                       :: inital_value
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      inital_value = a_model%get_param_value(this%param_id,                    &
     &                                       this%indices(1),                  &
     &                                       this%indices(2))

      temp = 0.0
      DO i = 0, SIZE(this%locks%ids)
         temp = temp + this%correlation(i)                                     &
     &               * a_model%get_param_value(this%locks%ids(i),              &
     &                                         this%locks%indices(i,1),        &
     &                                         this%locks%indices(i,2))
      END DO

!  Only update the lock value if that value changed.
      IF (inital_value .ne. temp) THEN
         CALL a_model%set_param(this%param_id,  this%indices(1),               &
     &                          this%indices(2), temp, eq_comm)
      END iF

      CALL profiler_set_stop_time('param_set_lock_value', start_time)

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Gets the parameter value.
!>
!>  Gets the current parameter value from either the @ref model or the
!>  @ref equilibrium.
!>
!>  @param[in] this    A @ref param_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @returns The parameter value.
!-------------------------------------------------------------------------------
      FUNCTION param_get_value(this, a_model)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: param_get_value
      TYPE (param_class), INTENT(in)  :: this
      CLASS (model_class), INTENT(in) :: a_model

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      param_get_value = a_model%get_param_value(this%param_id,                 &
     &                                          this%indices(1),               &
     &                                          this%indices(2))

      CALL profiler_set_stop_time('param_get_value', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the parameter name.
!>
!>  Converts the parameter id from either the @ref model or the
!>  @ref equilibrium.
!>
!>  @param[in] this    A @ref param_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @returns The parameter name.
!-------------------------------------------------------------------------------
      FUNCTION param_get_name(this, a_model)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: param_get_name
      TYPE (param_class), INTENT(in)   :: this
      CLASS (model_class), INTENT(in)  :: a_model

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      param_get_name = a_model%get_param_name(this%param_id)

      CALL profiler_set_stop_time('param_get_name', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the lower boundary value.
!>
!>  Gets lower boundary value. The results of this function are invalid if the
!>  bounding type is @ref param_range_infinity_type or @ref param_range_no_type.
!>
!>  @param[in] this    A @ref param_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @returns The lower bounding value.
!-------------------------------------------------------------------------------
      FUNCTION param_get_lower_range_value(this, a_model)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: param_get_lower_range_value
      TYPE (param_class), INTENT(in)  :: this
      CLASS (model_class), INTENT(in) :: a_model

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%recon%range_type(1))

         CASE DEFAULT
            param_get_lower_range_value = this%recon%range_value(1)

         CASE (param_range_parameter_type)
            param_get_lower_range_value =                                      &
     &         a_model%get_param_value(this%recon%range_id(1),                 &
     &                                 this%recon%range_indices(1,1),          &
     &                                 this%recon%range_indices(1,2))

      END SELECT

      CALL profiler_set_stop_time('param_get_lower_range_value',               &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the upper boundary value.
!>
!>  Gets upper boundary value. The results of this function are invalid if the
!>  bounding type is @ref param_range_infinity_type or @ref param_range_no_type.
!>
!>  @param[in] this    A @ref param_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @returns The upper bounding value.
!-------------------------------------------------------------------------------
      FUNCTION param_get_upper_range_value(this, a_model)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: param_get_upper_range_value
      TYPE (param_class), INTENT(in)  :: this
      CLASS (model_class), INTENT(in) :: a_model

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%recon%range_type(2))

         CASE DEFAULT
            param_get_upper_range_value = this%recon%range_value(2)

         CASE (param_range_parameter_type)
            param_get_upper_range_value =                                      &
     &         a_model%get_param_value(this%recon%range_id(2),                 &
     &                                 this%recon%range_indices(2,1),          &
     &                                 this%recon%range_indices(2,2))

      END SELECT

      CALL profiler_set_stop_time('param_get_upper_range_value',               &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the lower boundary type description.
!>
!>  Gets lower boundary type description.
!>
!>  @param[in] this    A @ref param_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @returns A string description of the lower boundary type.
!-------------------------------------------------------------------------------
      FUNCTION param_get_lower_range_type(this, a_model)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: param_get_lower_range_type
      TYPE (param_class), INTENT(in)   :: this
      CLASS (model_class), INTENT(in)  :: a_model

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%recon%range_type(1))

         CASE DEFAULT
            param_get_lower_range_type = 'No Type'

         CASE (param_range_value_type)
            param_get_lower_range_type = 'value'

         CASE (param_range_infinity_type)
            param_get_lower_range_type = 'infinity'

         CASE (param_range_parameter_type)
            param_get_lower_range_type =                                       &
     &         a_model%get_param_name(this%recon%range_id(1))

      END SELECT

      CALL profiler_set_stop_time('param_get_lower_range_type',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the upper boundary type description.
!>
!>  Gets upper boundary type description.
!>
!>  @param[in] this    A @ref param_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @returns A string description of the upper boundary type.
!-------------------------------------------------------------------------------
      FUNCTION param_get_upper_range_type(this, a_model)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: param_get_upper_range_type
      TYPE (param_class), INTENT(in)   :: this
      CLASS (model_class), INTENT(in)  :: a_model

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%recon%range_type(2))

         CASE DEFAULT
            param_get_upper_range_type = 'No Type'

         CASE (param_range_value_type)
            param_get_upper_range_type = 'value'

         CASE (param_range_infinity_type)
            param_get_upper_range_type = 'infinity'

         CASE (param_range_parameter_type)
            param_get_upper_range_type =                                       &
     &         a_model%get_param_name(this%recon%range_id(2))

      END SELECT

      CALL profiler_set_stop_time('param_get_upper_range_type',                &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  QUERY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Checks if the value is in the lower range.
!>
!>  If the value is less than the bounding value, the parameter value is out of
!>  range.
!>
!>  @param[in] this    A @ref param_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] value   A parameter value to check.
!>  @returns True for in range and false for out of range.
!-------------------------------------------------------------------------------
      FUNCTION param_is_in_lower_range(this, a_model, value)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL                         :: param_is_in_lower_range
      TYPE (param_class), INTENT(in)  :: this
      CLASS (model_class), INTENT(in) :: a_model
      REAL (rprec), INTENT(in)        :: value

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      param_is_in_lower_range = .true.
      IF (this%recon%range_type(1) .ne. param_range_infinity_type) THEN
         param_is_in_lower_range =                                             &
     &      value .ge. param_get_lower_range_value(this, a_model)
      END IF

      CALL profiler_set_stop_time('param_is_in_lower_range', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Checks if the value is in the upper range.
!>
!>  If the value is greater than the bounding value, the parameter value is out
!>  of range.
!>
!>  @param[in] this    A @ref param_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] value   A parameter value to check.
!>  @returns True for in range and false for out of range.
!-------------------------------------------------------------------------------
      FUNCTION param_is_in_upper_range(this, a_model, value)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL                         :: param_is_in_upper_range
      TYPE (param_class), INTENT(in)  :: this
      CLASS (model_class), INTENT(in) :: a_model
      REAL (rprec), INTENT(in)        :: value

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      param_is_in_upper_range = .true.
      IF (this%recon%range_type(2) .ne. param_range_infinity_type) THEN
         param_is_in_upper_range =                                             &
     &      value .le. param_get_upper_range_value(this, a_model)
      END IF

      CALL profiler_set_stop_time('param_is_in_upper_range', start_time)

      END FUNCTION

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Increments the parameter value.
!>
!>  The value is incremented by the vrnc. The incremented value is then checked
!>  if the new value is in range. If the value is out of range, the value is
!>  incremented in the opposite direction. If the incremented value is outside
!>  of both ranges, the step size value is halved and the procedure is repeated.
!>  Five attempts are made until the function quits with an error. If
!>  successful, the actual step size used is cached in
!>  @ref param_recon_class::delta for normalization purposes.
!>
!>  If central differencing is used, both the upper and lower bounds must be
!>  checked. Starting with half the parameter vrnc, the change in parameter is
!>  divided until the change in parameter fits into both the upper and lower
!>  bounds.
!>
!>  @param[in] this       A @ref param_class instance.
!>  @param[in] a_model    A @ref model instance.
!>  @param[in] eq_comm    MPI communicator for the child equilibrium processes.
!>  @param[in] is_central Central differencing is being used.
!-------------------------------------------------------------------------------
      SUBROUTINE param_increment(this, a_model, eq_comm, is_central)
      USE v3_utilities, only: err_fatal

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(inout)  :: this
      CLASS (model_class), INTENT(inout) :: a_model
      INTEGER, INTENT(in)                :: eq_comm
      LOGICAL, INTENT(in)                :: is_central

!  local variables
      REAL (rprec)                       :: new_value
      REAL (rprec)                       :: value
      REAL (rprec)                       :: step
      INTEGER                            :: itry
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      value = param_get_value(this, a_model)

!  Take the absolute value here to control the direction of the step.
      IF (is_central) THEN
         step = ABS(this%recon%vrnc)/2.0
      ELSE
         step = ABS(this%recon%vrnc)
      END IF

      DO itry = 1, param_max_increment_steps

!  Test to make sure there is a step
         IF (step .eq. 0.0) THEN
            CALL err_fatal('param_increment: tried to take a ' //              &
     &                     'step size of 0')
         END IF

         IF (is_central) THEN
!  Test both upper and lower range.
            IF (param_is_in_upper_range(this, a_model,                         &
     &                                  value + step) .and.                    &
     &          param_is_in_lower_range(this, a_model,                         &
     &                                  value - step)) THEN
               new_value = value + step

!  Store the actual change in value.
               this%recon%delta = step
               EXIT
            END IF

         ELSE

!  Try a step in the positive direction. If the parameter is still in range set
!  otherwise.
            new_value = value + step
            IF (param_is_in_upper_range(this, a_model, new_value)) THEN
!  Store the actual change in value.
               this%recon%delta = step
               EXIT
            END IF

!  Try a step in the negative direction.
            new_value = value - step
            IF (param_is_in_lower_range(this, a_model, new_value)) THEN
!  Store the actual change in value.
               this%recon%delta = -step
               EXIT
            END IF
         END IF

!  Decrease the step and try again.
         step = step/param_div_factor
      END DO

      CALL a_model%set_param(this%param_id, this%indices(1),                   &
     &                       this%indices(2), new_value, eq_comm)

      IF (itry .gt. param_max_increment_steps) THEN
         CALL err_fatal('param_increment: failed to change ' //                &
     &                  'reconstruction. Try decreaing rp_vrnc ' //            &
     &                  'or expanding the range.')
      END IF

      CALL profiler_set_stop_time('param_increment', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Decrements the parameter value.
!>
!>  The value is decremented by the delta. The incrementing function has already
!>  performed the bounds checking so there is no need to do that here.
!>
!>  @param[in] this       A @ref param_class instance.
!>  @param[in] a_model    A @ref model instance.
!>  @param[in] eq_comm    MPI communicator for the child equilibrium processes.
!-------------------------------------------------------------------------------
      SUBROUTINE param_decrement(this, a_model, eq_comm)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(inout)  :: this
      CLASS (model_class), INTENT(inout) :: a_model
      INTEGER, INTENT(in)                :: eq_comm

!  local variables
      REAL (rprec)                       :: value
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      value = param_get_value(this, a_model)
      value = value - this%recon%delta

      CALL a_model%set_param(this%param_id, this%indices(1),                   &
     &                       this%indices(2), value, eq_comm)

      CALL profiler_set_stop_time('param_decrement', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Writes out a parameter to an output file.
!>
!>  Parameter information is formated as the index, name, index 1, index2,
!>  value, sigma, vrnc, upper and lower range types, values, and range indices.
!>
!>  @param[in] this    A @ref param_class instance.
!>  @param[in] iou     Input/output unit representing the file to write to.
!>  @param[in] index   The index of a parameter.
!>  @param[in] a_model A @ref model instance.
!>  @note Eventually parameter range informarion will be added as well.
!-------------------------------------------------------------------------------
      SUBROUTINE param_write(this, iou, index, a_model)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(in)  :: this
      INTEGER, INTENT(in)             :: iou
      INTEGER, INTENT(in)             :: index
      CLASS (model_class), INTENT(in) :: a_model

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (iou,1000) index,                                                  &
     &                 param_get_name(this, a_model),                          &
     &                 this%indices(1), this%indices(2),                       &
     &                 param_get_value(this, a_model),                         &
     &                 this%sigma, this%recon%vrnc,                            &
     &                 param_get_upper_range_type(this, a_model),              &
     &                 param_get_lower_range_type(this, a_model),              &
     &                 param_get_upper_range_value(this, a_model),             &
     &                 param_get_lower_range_value(this, a_model),             &
     &                 this%recon%range_indices(2,:),                          &
     &                 this%recon%range_indices(1,:)
1000  FORMAT(i3,2x,a18,2(1x,i4),3(2x,es12.5),2(2x,a13),2(2x,es12.5),           &
     &       2(2x,i8,1x,i8))

      CALL profiler_set_stop_time('param_write', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Writes out a parameter to an output file.
!>
!>  Parameter information is formated as the index, name, index 1, index2, value
!>  and sigma.
!>
!>  @param[in] this    A @ref param_class instance.
!>  @param[in] iou     Input/output unit representing the file to write to.
!>  @param[in] index   The index of a parameter.
!>  @param[in] a_model A @ref model instance.
!-------------------------------------------------------------------------------
      SUBROUTINE param_write_short(this, iou, index, a_model)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(in)  :: this
      INTEGER, INTENT(in)             :: iou
      INTEGER, INTENT(in)             :: index
      CLASS (model_class), INTENT(in) :: a_model

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (iou,1000) index,                                                  &
     &                 param_get_name(this, a_model),                          &
     &                 this%indices(1), this%indices(2),                       &
     &                 param_get_value(this, a_model), this%sigma
1000  FORMAT(i3,2x,a18,2(1x,i4),2(2x,es12.5))

      CALL profiler_set_stop_time('param_write_short', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Writes out parameter header information to an output file.
!>
!>  Parameter information is formated as the index, name, index 1, index2,
!>  value, sigma, vrnc, upper and lower range types, values, and range indices.
!>  This is implmented as a static method and does not require a parameter
!>  instance. This should only be called once to produce a single header.
!>
!>  @param[in] iou A input/output representing the file to write to.
!-------------------------------------------------------------------------------
      SUBROUTINE param_write_header(iou)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER, INTENT(in) :: iou

!  local variables
      REAL (rprec)        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (iou,*)
      WRITE (iou,*) ' *** Reconstruction parameters'
      WRITE (iou,1000)

      CALL profiler_set_stop_time('param_write_header', start_time)

1000  FORMAT ('irp',2x,'p_type',13x,'inx1',1x,'inx2',2x,'value',9x,            &
     &        'sigma',9x,'vrnc',10x,'range_type_h',3x,'range_type_l',3x,       &
     &        'r_value_h',5x,'r_value_l',5x,                                   &
     &        'r_inx1_h',1x,'r_inx2_h',2x,'r_inx1_l',1x,'r_inx2_l')

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Writes out parameter header information to an output file.
!>
!>  Parameter information is formated as the index, name, index 1, index2, value
!>  and sigma. This is implmented as a static method and does not require a
!>  parameter instance. This should only be called once to produce a single
!>  header.
!>
!>  @param[in] iou A input/output representing the file to write to.
!-------------------------------------------------------------------------------
      SUBROUTINE param_write_header_short(iou)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER, INTENT(in) :: iou

!  local variables
      REAL (rprec)        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (iou,*)
      WRITE (iou,*) ' *** Derived parameters'
      WRITE (iou,1000)

      CALL profiler_set_stop_time('param_write_header_short',                  &
     &                            start_time)

1000  FORMAT ('irp',2x,'p_type',13x,'inx1',1x,'inx2',2x,'value',9x,            &
     &        'sigma')

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Writes out a parameter covariance matrix row.
!>
!>  Each parameter contains the row for it's covariance matrix. This write it
!>  out formatted as the name of the parameter up to 12 characters. This
!>  followed by each value spearated by 2 spaces and a 12 character number.
!>
!>  @param[in] this    A @ref param_class instance.
!>  @param[in] iou     Input/output unit representing the file to write to.
!>  @param[in] a_model A @ref model instance.
!-------------------------------------------------------------------------------
      SUBROUTINE param_write_correlation(this, iou, a_model)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(in)  :: this
      INTEGER, INTENT(in)             :: iou
      CLASS (model_class), INTENT(in) :: a_model

!  local variables
      CHARACTER (len=20)              :: row_format
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (row_format,1000) SIZE(this%correlation)
      WRITE (iou,row_format) param_get_name(this, a_model),                    &
     &                       this%correlation

      CALL profiler_set_stop_time('param_write_correlation', start_time)

1000  FORMAT ('(a12,',i3,'(2x,es12.5))')

      END SUBROUTINE

!*******************************************************************************
!  NETCDF SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Write out the parameter data for a step to the result netcdf file.
!>
!>  Writes out the parameter value, sigma and signal effectiveness matrix to the
!>  result file.
!>
!>  @param[in] this           A @ref param_class instance.
!>  @param[in] a_model        The equilibrium model.
!>  @param[in] result_ncid    A NetCDF id of the result file.
!>  @param[in] current_step   The surrent reconstruction step.
!>  @param[in] index          A index of a parameter.
!>  @param[in] param_value_id NetCDF variable id of the parameter value.
!>                            @see v3fit_context
!>  @param[in] param_sigma_id NetCDF variable id of the parameter sigma.
!>                            @see v3fit_context
!>  @param[in] param_corr_id  NetCDF variable id of the parameter correlation.
!>                            @see v3fit_context
!>  @param[in] param_sem_id   NetCDF variable id of the parameter signal
!>                            effectiveness.
!>  @note This assumes that the parameter sigma has already been calculated.
!>  Name and index information is written when the result file is first created
!>  by @ref v3fit_context::v3fit_context_write_step_data.
!-------------------------------------------------------------------------------
      SUBROUTINE param_write_step_data_1(this, a_model, result_ncid,           &
     &                                   current_step, index,                  &
     &                                   param_value_id, param_sigma_id,       &
     &                                   param_corr_id, param_sem_id)
      USE ezcdf

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(in)  :: this
      CLASS (model_class), INTENT(in) :: a_model
      INTEGER, INTENT(in)             :: result_ncid
      INTEGER, INTENT(in)             :: current_step
      INTEGER, INTENT(in)             :: index
      INTEGER, INTENT(in)             :: param_value_id
      INTEGER, INTENT(in)             :: param_sigma_id
      INTEGER, INTENT(in)             :: param_corr_id
      INTEGER, INTENT(in)             :: param_sem_id

!  local variables
      INTEGER                         :: status
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL param_write_step_data(this, a_model, result_ncid,                   &
     &                           current_step, index,                          &
     &                           param_value_id, param_sigma_id,               &
     &                           param_corr_id)

      status = nf90_put_var(result_ncid, param_sem_id,                         &
     &                      this%recon%sem,                                    &
     &                      start=(/ 1, index, current_step /),                &
     &                      count=(/ SIZE(this%recon%sem), 1, 1 /))

      CALL profiler_set_stop_time('param_write_step_data_1', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write out the parameter data for a step to the result netcdf file.
!>
!>  Writes out the parameter value and sigma to the result file.
!>
!>  @param[in] this           A @ref param_class instance.
!>  @param[in] a_model        The equilibrium model.
!>  @param[in] result_ncid    A NetCDF id of the result file.
!>  @param[in] current_step   The surrent reconstruction step.
!>  @param[in] index          A index of a parameter.
!>  @param[in] param_value_id NetCDF variable id of the parameter value.
!>                            @see v3fit_context
!>  @param[in] param_sigma_id NetCDF variable id of the parameter sigma.
!>                            @see v3fit_context
!>  @param[in] param_corr_id  NetCDF variable id of the parameter correlation.
!>                            @see v3fit_context
!>  @note This assumes that the parameter sigma has already been calculated.
!>  Name and index information is written when the result file is first created
!>  by @ref v3fit_context::v3fit_context_write_step_data.
!-------------------------------------------------------------------------------
      SUBROUTINE param_write_step_data_2(this, a_model, result_ncid,           &
     &                                   current_step, index,                  &
     &                                   param_value_id, param_sigma_id,       &
     &                                   param_corr_id)
      USE ezcdf

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(in)  :: this
      CLASS (model_class), INTENT(in) :: a_model
      INTEGER, INTENT(in)             :: result_ncid
      INTEGER, INTENT(in)             :: current_step
      INTEGER, INTENT(in)             :: index
      INTEGER, INTENT(in)             :: param_value_id
      INTEGER, INTENT(in)             :: param_sigma_id
      INTEGER, INTENT(in)             :: param_corr_id

!  local variables
      INTEGER                         :: status
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      status = nf90_put_var(result_ncid, param_value_id,                       &
     &                      param_get_value(this, a_model),                    &
     &                      start=(/ index, current_step /))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_put_var(result_ncid, param_sigma_id, this%sigma,           &
     &                      start=(/ index, current_step /))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_put_var(result_ncid, param_corr_id,                        &
     &                      this%correlation,                                  &
     &                      start=(/ 1, index, current_step /),                &
     &                      count=(/ SIZE(this%correlation), 1, 1 /))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      CALL profiler_set_stop_time('param_write_step_data_2', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Restart the parameter.
!>
!>  Restarts a parameter from the result file. This reloads and sets the value,
!>  sigma and correlation. All other values are set when the namelist input file
!>  was read.
!>
!>  @param[inout] this           A @ref param_class instance.
!>  @param[in]    a_model        The equilibrium model.
!>  @param[in]    result_ncid    Netcdf if for the result file.
!>  @param[in]    current_step   Step number to restart from.
!>  @param[in]    index          A index of a parameter.
!>  @param[in]    param_value_id NetCDF variable id of the parameter value.
!>                               @see v3fit_context
!>  @param[in]    param_sigma_id NetCDF variable id of the parameter sigma.
!>                               @see v3fit_context
!>  @param[in]    param_corr_id  NetCDF variable id of the parameter
!>                               correlation. @see v3fit_context
!>  @param[in]    eq_comm        MPI communicator for the child equilibrium
!>                               processes.
!>  @param[in]    is_central     Central differencing is being used.
!-------------------------------------------------------------------------------
      SUBROUTINE param_restart(this, a_model, result_ncid, current_step,       &
     &                         index, param_value_id, param_sigma_id,          &
     &                         param_corr_id, eq_comm, is_central)
      USE ezcdf

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(inout) :: this
      CLASS (model_class), INTENT(inout) :: a_model
      INTEGER, INTENT(in)                :: result_ncid
      INTEGER, INTENT(in)                :: current_step
      INTEGER, INTENT(in)                :: index
      INTEGER, INTENT(in)                :: param_value_id
      INTEGER, INTENT(in)                :: param_sigma_id
      INTEGER, INTENT(in)                :: param_corr_id
      INTEGER, INTENT(in)                :: eq_comm
      LOGICAL, INTENT(in)                :: is_central

!  local variables
      INTEGER                            :: status
      REAL (rprec)                       :: temp_value
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      status = nf90_get_var(result_ncid, param_value_id, temp_value,           &
     &                      start=(/ index, current_step /))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      CALL param_set_value(this, a_model, temp_value, eq_comm,                 &
     &                     is_central)

      status = nf90_put_var(result_ncid, param_sigma_id, this%sigma,           &
     &                      start=(/ index, current_step /))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_put_var(result_ncid, param_corr_id,                        &
     &                      this%correlation,                                  &
     &                      start=(/ 1, index, current_step /),                &
     &                      count=(/ SIZE(this%correlation), 1, 1 /))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      CALL profiler_set_stop_time('param_restart', start_time)

      END SUBROUTINE

!*******************************************************************************
!  MPI SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Syncronize the param value to children.
!>
!>  Syncs data between the parent and child processes. If MPI support is not
!>  compiled in this subroutine reduces to a no op.
!>
!>  @param[inout] this       A @ref param_class instance.
!>  @param[inout] a_model    A @ref model instance.
!>  @param[in]    recon_comm A MPI intra_comm handle.
!>  @param[in]    eq_comm    MPI communicator for the child equilibrium
!>                           processes.
!>  @param[in]    is_central Central differencing is being used.
!-------------------------------------------------------------------------------
      SUBROUTINE param_sync_value(this, a_model, recon_comm, eq_comm,          &
     &                            is_central)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(inout)  :: this
      CLASS (model_class), INTENT(inout) :: a_model
      INTEGER, INTENT(in)                :: recon_comm
      INTEGER, INTENT(in)                :: eq_comm
      LOGICAL, INTENT(in)                :: is_central

#if defined(MPI_OPT)
!  local variables
      INTEGER                            :: error
      REAL (rprec)                       :: value
      INTEGER                            :: mpi_rank
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL MPI_COMM_RANK(recon_comm, mpi_rank, error)

      IF (mpi_rank .eq. 0) THEN
         value = param_get_value(this, a_model)
      END IF

      CALL MPI_BCAST(value, 1, MPI_REAL8, 0, recon_comm, error)

      IF (mpi_rank .gt. 0) THEN
         CALL param_set_value(this, a_model, value, eq_comm, is_central)
      END IF

      CALL profiler_set_stop_time('param_sync_value', start_time)

#endif

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Sends the delta used.
!>
!>  Sends the value of delta used in the child process to the parent process. If
!>  MPI support is not compiled in this subroutine reduces to a no op.
!>
!>  @param[inout] this       A @ref param_class instance.
!>  @param[in]    index      Index of the reconstruction parameter.
!>  @param[in]    recon_comm A MPI intra_comm handle.
!-------------------------------------------------------------------------------
      SUBROUTINE param_send_delta(this, index, recon_comm)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(inout) :: this
      INTEGER, INTENT(in)               :: index
      INTEGER, INTENT(in)               :: recon_comm

#if defined(MPI_OPT)
!  local variables
      INTEGER                           :: error
      INTEGER                           :: mpi_size
      INTEGER                           :: mpi_request
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL MPI_COMM_SIZE(recon_comm, mpi_size, error)

!  Must use a non blocking send here because we are sending messages from the
!  main process to itself or need to send multiple sends before the first
!  recieve. Other wise it could block until a recieve request is called.
      CALL MPI_ISEND(this%recon%delta, 1, MPI_REAL8, 0, index,                 &
     &               recon_comm, mpi_request, error)

      CALL profiler_set_stop_time('param_send_delta', start_time)
#endif

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Receives the delta used.
!>
!>  Receives the value of delta used in the child process. If MPI support is not
!>  compiled in this subroutine reduces to a no op.
!>
!>  @param[inout] this       A @ref param_class instance.
!>  @param[in]    index      Index of the reconstruction parameter.
!>  @param[in]    recon_comm A MPI recon_comm handle.
!-------------------------------------------------------------------------------
      SUBROUTINE param_recv_delta(this, index, recon_comm)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(inout) :: this
      INTEGER, INTENT(in)               :: index
      INTEGER, INTENT(in)               :: recon_comm

#if defined(MPI_OPT)
!  local variables
      INTEGER                           :: error
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL MPI_RECV(this%recon%delta, 1, MPI_REAL8, MPI_ANY_SOURCE,            &
     &              index, recon_comm, MPI_STATUS_IGNORE, error)

      CALL profiler_set_stop_time('param_recv_delta', start_time)

#endif

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Sync the delta used.
!>
!>  Syncs the value of delta from the parent process to the child. If MPI
!>  support is not compiled in this subroutine reduces to a no op.
!>
!>  @param[inout] this       A @ref param_class instance.
!>  @param[in]    recon_comm A MPI recon_comm handle.
!-------------------------------------------------------------------------------
      SUBROUTINE param_sync_delta(this, recon_comm)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(inout) :: this
      INTEGER, INTENT(in)               :: recon_comm

#if defined(MPI_OPT)
!  local variables
      INTEGER                           :: error
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL MPI_BCAST(this%recon%delta, 1, MPI_REAL8, 0, recon_comm,            &
     &               error)

      CALL profiler_set_stop_time('param_sync_delta', start_time)

#endif

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Sync the parameter value from a child to the parent.
!>
!>  Syncs the value of delta from a child process to the parent. If MPI
!>  support is not compiled in this subroutine reduces to a no op.
!>
!>  @param[inout] this       A @ref param_class instance.
!>  @param[inout] a_model    A @ref model instance.
!>  @param[in]    index      Index of the reconstruction parameter.
!>  @param[in]    recon_comm A MPI intra_comm handle.
!>  @param[in]    eq_comm    MPI communicator for the child equilibrium
!>                           processes.
!>  @param[in]    is_central Central differencing is being used.
!-------------------------------------------------------------------------------
      SUBROUTINE param_sync_child(this, a_model, index, recon_comm,            &
     &                            eq_comm, is_central)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (param_class), INTENT(inout)  :: this
      CLASS (model_class), INTENT(inout) :: a_model
      INTEGER, INTENT(in)                :: index
      INTEGER, INTENT(in)                :: recon_comm
      INTEGER, INTENT(in)                :: eq_comm
      LOGICAL, INTENT(in)                :: is_central

#if defined(MPI_OPT)
!  local variables
      INTEGER                            :: error
      REAL (rprec)                       :: value
      INTEGER                            :: mpi_rank
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL MPI_COMM_RANK(recon_comm, mpi_rank, error)

      IF (mpi_rank .eq. index) THEN
         value = param_get_value(this, a_model)
         CALL MPI_SSEND(value, 1, MPI_REAL8, 0, mpi_rank, recon_comm,          &
     &                  error)
      ELSE IF (mpi_rank .eq. 0) THEN
         CALL MPI_RECV(value, 1, MPI_REAL8, index, index, recon_comm,          &
     &                 MPI_STATUS_IGNORE, error)
         CALL param_set_value(this, a_model, value, eq_comm, is_central)
      END IF

      CALL profiler_set_stop_time('param_sync_child', start_time)

#endif

      END SUBROUTINE

      END MODULE

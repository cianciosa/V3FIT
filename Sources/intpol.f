!*******************************************************************************
!>  @file intpol.f
!>  @brief Contains module @ref intpol.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Implements interferometry/polarimetry diagnostic. Defines the base class of
!>  the type @ref intpol_class.
!>  @par Super Class:
!>  @ref diagnostic
!*******************************************************************************

      MODULE intpol

      USE stel_kinds, only: rprec, dp
      USE integration_path
      USE coordinate_utilities
      USE profiler
      USE model
      USE signal

      IMPLICIT NONE
!*******************************************************************************
!  intpol module parameters
!*******************************************************************************
!>  Constant term for the polarimety.
      REAL(rprec), PARAMETER :: intpol_polar_constant = 2.62E-13_dp

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) intpol base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a interferometer signal.
!>  @par Super Class:
!>  @ref signal
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(signal_class) :: intpol_class
!>  The complete path of the chord.
         TYPE (vertex), POINTER :: chord_path => null()
      CONTAINS
         PROCEDURE              ::                                             &
     &      get_modeled_signal_last => intpol_get_modeled_signal
         PROCEDURE              ::                                             &
     &      get_type=> intpol_get_type
         PROCEDURE              :: get_gp_i => intpol_get_gp_i
         PROCEDURE              :: get_gp_s => intpol_get_gp_s
         PROCEDURE              :: get_gp_x => intpol_get_gp_x
         FINAL                  :: intpol_destruct
      END TYPE

!-------------------------------------------------------------------------------
!>  Base class representing a polarimetry signal.
!>  @par Super Class:
!>  @ref intpol_class
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(intpol_class) :: intpol_pol_class
!>  Wavelength if the beam.
         REAL (rprec) :: wavelength
!>  Controls if the results is computed in degrees or radians.
         LOGICAL      :: in_degrees
      CONTAINS
         PROCEDURE    ::                                                       &
     &      get_modeled_signal_last => intpol_pol_get_modeled_signal
         PROCEDURE    :: get_gp_i => intpol_pol_get_gp_i
         PROCEDURE    :: get_gp_s => intpol_pol_get_gp_s
         PROCEDURE    :: get_gp_x => intpol_pol_get_gp_x
         FINAL        :: intpol_pol_destruct
      END TYPE

!-------------------------------------------------------------------------------
!>  Structure to hold all memory needed to be sent to the guassian process
!>  callback function of a point.
!-------------------------------------------------------------------------------
      TYPE intpol_gp_context_i
!>  Reference to a @ref model::model_class object.
         TYPE (model_class), POINTER :: model => null()
!>  Position index.
         INTEGER                     :: i
!>  Gaussian process kernel flags.
         INTEGER                     :: flags = model_state_all_off
      END TYPE

!-------------------------------------------------------------------------------
!>  Structure to hold all memory needed to be sent to the guassian process
!>  callback function for signal.
!-------------------------------------------------------------------------------
      TYPE intpol_gp_context_s
!>  Reference to a @ref model::model_class object.
         TYPE (model_class), POINTER   :: model => null()
!>  First position.
         CLASS (signal_class), POINTER :: signal => null()
!>  Gaussian process kernel flags.
         INTEGER                       :: flags = model_state_all_off
      END TYPE

!-------------------------------------------------------------------------------
!>  Structure to hold all memory needed to be sent to the guassian process
!>  callback function for position.
!-------------------------------------------------------------------------------
      TYPE intpol_gp_context_x
!>  Reference to a @ref model::model_class object.
         TYPE (model_class), POINTER :: model => null()
!>  First position.
         REAL (rprec), DIMENSION(3)  :: xcart
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for interferometry constructor.
!-------------------------------------------------------------------------------
      INTERFACE intpol_class
         MODULE PROCEDURE intpol_construct
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for polarimetry constructor.
!-------------------------------------------------------------------------------
      INTERFACE intpol_pol_class
         MODULE PROCEDURE intpol_pol_construct
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface to get the guassian process kernel values.
!-------------------------------------------------------------------------------

      PRIVATE :: int_function, pol_function, gp_function_i,                    &
     &           gp_function_x

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref intpol_class object representing a interferometer
!>  diagnostic.
!>
!>  Allocates memory and initializes a @ref intpol_class object.
!>
!>  @param[in] chord_paths The nodes of a multi point segment integration path.
!>  @returns A pointer to a constructed @ref intpol_class object.
!-------------------------------------------------------------------------------
      FUNCTION intpol_construct(chord_paths)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (intpol_class), POINTER :: intpol_construct
      REAL (rprec), DIMENSION(:,:)  :: chord_paths

!  local variables
      INTEGER                       :: i
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(intpol_construct)

      DO i = 1, SIZE(chord_paths, 1)
         CALL path_append_vertex(intpol_construct%chord_path,                  &
     &                           chord_paths(i,:))
      END DO

      CALL profiler_set_stop_time('intpol_construct', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref intpol_class object representing a polarimetry
!>  diagnostic.
!>
!>  Allocates memory and initializes a @ref intpol_class object.
!>
!>  @param[in] wavelength  Wavelength of the polarimetry beam.
!>  @param[in] in_degrees  Specifies if the sigals is degrees or radians. True
!>                         specifies degree while false specifies radians.
!>  @param[in] chord_paths The nodes of a multi point segment integration path.
!>  @returns A pointer to a constructed @ref intpol_class object.
!-------------------------------------------------------------------------------
      FUNCTION intpol_pol_construct(wavelength, in_degrees, chord_paths)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (intpol_pol_class), POINTER :: intpol_pol_construct
      REAL (rprec), INTENT(in)          :: wavelength
      LOGICAL, INTENT(in)               :: in_degrees
      REAL (rprec), DIMENSION(:,:)      :: chord_paths

!  local variables
      INTEGER                           :: i
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(intpol_pol_construct)

      DO i = 1, SIZE(chord_paths, 1)
         CALL path_append_vertex(intpol_pol_construct%chord_path,              &
     &                           chord_paths(i,:))
      END DO

      intpol_pol_construct%wavelength = wavelength
      intpol_pol_construct%in_degrees = in_degrees

      CALL profiler_set_stop_time('intpol_pol_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref intpol_class object.
!>
!>  Deallocates memory and uninitializes a @ref intpol_class object.
!>
!>  @param[inout] this A @ref intpol_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE intpol_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (intpol_class), INTENT(inout) :: this

!  Start of executable code

      IF (ASSOCIATED(this%chord_path)) THEN
         CALL path_destruct(this%chord_path)
         this%chord_path => null()
      END IF

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref intpol_class object.
!>
!>  Deallocates memory and uninitializes a @ref intpol_class object.
!>
!>  @param[inout] this A @ref intpol_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE intpol_pol_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (intpol_pol_class), INTENT(inout) :: this

!  Start of executable code
      this%wavelength = 0
      this%in_degrees = .false.

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the interferometry signal.
!>
!>  Calculates the modeled signal by integrating along the chord path. The
!>  integration function is provided by @ref int_function.
!>
!>  @param[inout] this       A @ref intpol_class instance.
!>  @param[in]    a_model    A @ref model instance.
!>  @param[out]   sigma      The modeled sigma.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION intpol_get_modeled_signal(this, a_model, sigma,                 &
     &                                   last_value)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: intpol_get_modeled_signal
      CLASS (intpol_class), INTENT(inout)     :: this
      TYPE (model_class), POINTER             :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out) :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)  :: last_value

!  local variables
      CHARACTER(len=1), ALLOCATABLE           :: context(:)
      INTEGER                                 :: context_length
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sigma = 0.0

      IF (BTEST(a_model%state_flags, model_state_vmec_flag)   .or.             &
     &    BTEST(a_model%state_flags, model_state_siesta_flag) .or.             &
     &    BTEST(a_model%state_flags, model_state_ne_flag)     .or.             &
     &    BTEST(a_model%state_flags, model_state_shift_flag)  .or.             &
     &    BTEST(a_model%state_flags, model_state_signal_flag)) THEN

!  Cast model into a data to a context. This is the equivalent to casting to a
!  void pointer in C.
         context_length = SIZE(TRANSFER(a_model, context))
         ALLOCATE(context(context_length))
         context = TRANSFER(a_model, context)

         intpol_get_modeled_signal = 0.0
         intpol_get_modeled_signal(1) =                                        &
     &      path_integrate(a_model%int_params, this%chord_path,                &
     &                     int_function, context)

         DEALLOCATE(context)

         CALL this%scale_and_offset(a_model,                                   &
     &                              intpol_get_modeled_signal(1))
      ELSE
         intpol_get_modeled_signal = last_value
      END IF

      CALL profiler_set_stop_time('intpol_get_modeled_signal',                 &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Calculates the polarimetry signal.
!>
!>  Calculates the modeled signal by integrating along the chord path. The
!>  integration function is provided by @ ref pol_function.
!>
!>  @param[inout] this       A @ref intpol_pol_class instance.
!>  @param[in]    a_model    A @ref model instance.
!>  @param[out]   sigma      The modeled sigma.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION intpol_pol_get_modeled_signal(this, a_model, sigma,             &
     &                                       last_value)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: intpol_pol_get_modeled_signal
      CLASS (intpol_pol_class), INTENT(inout) :: this
      TYPE (model_class), POINTER             :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out) :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)  :: last_value

!  local variables
      CHARACTER(len=1), ALLOCATABLE           :: context(:)
      INTEGER                                 :: context_length
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sigma = 0.0

      IF (BTEST(a_model%state_flags, model_state_vmec_flag)   .or.             &
     &    BTEST(a_model%state_flags, model_state_siesta_flag) .or.             &
     &    BTEST(a_model%state_flags, model_state_ne_flag)     .or.             &
     &    BTEST(a_model%state_flags, model_state_shift_flag)  .or.             &
     &    BTEST(a_model%state_flags, model_state_signal_flag)) THEN

!  Cast model into a data to a context. This is the equivalent to casting to a
!  void pointer in C.
         context_length = SIZE(TRANSFER(a_model, context))
         ALLOCATE(context(context_length))
         context = TRANSFER(a_model, context)

         intpol_pol_get_modeled_signal = 0.0
         intpol_pol_get_modeled_signal(1) =                                    &
     &      intpol_polar_constant*(this%wavelength**2.0_dp) *                  &
     &      path_integrate(a_model%int_params, this%chord_path,                &
     &                     pol_function, context)
         IF (this%in_degrees) THEN
            intpol_pol_get_modeled_signal(1) =                                 &
     &                intpol_pol_get_modeled_signal(1)/degree
         END IF

         DEALLOCATE(context)

         CALL this%scale_and_offset(a_model,                                   &
     &                              intpol_pol_get_modeled_signal(1))
      ELSE
         intpol_pol_get_modeled_signal = last_value
      END IF

      CALL profiler_set_stop_time('intpol_pol_get_modeled_signal',             &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the intpol type.
!>
!>  Returns a description of the intpol type for use when writting output files.
!>
!>  @param[in] this A @ref intpol_class instance.
!>  @returns A string describing the intpol type.
!-------------------------------------------------------------------------------
      FUNCTION intpol_get_type(this)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: intpol_get_type
      CLASS (intpol_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      intpol_get_type = 'ipch'

      CALL profiler_set_stop_time('intpol_get_type', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for an inteferometry signal and a
!>  position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  Density kernels are provided by @ref model::model_get_gp_ne.
!>
!>  @param[in] this    A @ref intpol_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] i       Index of the position for the kernel.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the position and the signal.
!-------------------------------------------------------------------------------
      FUNCTION intpol_get_gp_i(this, a_model, i, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                     :: intpol_get_gp_i
      CLASS (intpol_class), INTENT(in) :: this
      TYPE (model_class), POINTER      :: a_model
      INTEGER, INTENT(in)              :: i
      INTEGER, INTENT(in)              :: flags

!  local variables
      CHARACTER(len=1), ALLOCATABLE    :: context(:)
      INTEGER                          :: context_length
      TYPE (intpol_gp_context_i)       :: gp_context
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  The relevant data for the guassian process context.
      gp_context%model => a_model
      gp_context%i = i
      gp_context%flags = flags

!  Cast model into a data to a context. This is the equivalent to casting to a
!  void pointer in C.
      context_length = SIZE(TRANSFER(gp_context, context))
      ALLOCATE(context(context_length))
      context = TRANSFER(gp_context, context)

      intpol_get_gp_i = path_integrate(a_model%int_params,                     &
     &                                 this%chord_path, gp_function_i,         &
     &                                 context)

      DEALLOCATE(context)

      CALL this%scale_and_offset(a_model, intpol_get_gp_i)

      CALL profiler_set_stop_time('intpol_get_gp_i', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for an inteferometry signal and a
!>  signal.
!>
!>  Calculates the guassian process kernel between the signal and a signal.
!>  Calls back to the @ref signal module to call the other signal. This is the
!>  first signal.
!>
!>  @param[in] this    A @ref intpol_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] signal  A @ref signal_class instance.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION intpol_get_gp_s(this, a_model, signal, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                     :: intpol_get_gp_s
      CLASS (intpol_class), INTENT(in) :: this
      TYPE (model_class), POINTER      :: a_model
      CLASS (signal_class), POINTER    :: signal
      INTEGER, INTENT(in)              :: flags

!  local variables
      CHARACTER(len=1), ALLOCATABLE    :: context(:)
      INTEGER                          :: context_length
      TYPE (intpol_gp_context_s)       :: gp_context
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context%model => a_model
      gp_context%signal => signal
      gp_context%flags = flags

!  Cast model into a data to a context. This is the equivalent to casting to a
!  void pointer in C.
      context_length = SIZE(TRANSFER(gp_context, context))
      ALLOCATE(context(context_length))
      context = TRANSFER(gp_context, context)

      intpol_get_gp_s = path_integrate(a_model%int_params,                     &
     &                                 this%chord_path, gp_function_s,         &
     &                                 context)

      DEALLOCATE(context)

      CALL this%scale_and_offset(a_model, intpol_get_gp_s)

      CALL profiler_set_stop_time('intpol_get_gp_s', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a inteferometry signal and a
!>  cartesian position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  This is the second signal.
!>
!>  @param[in] this    A @ref intpol_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] x_cart  The cartesian position of to get the kernel at.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION intpol_get_gp_x(this, a_model, x_cart, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: intpol_get_gp_x
      CLASS (intpol_class), INTENT(in)       :: this
      TYPE (model_class), POINTER            :: a_model
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: flags

!  local variables
      REAL (rprec)                           :: start_time
      CHARACTER(len=1), ALLOCATABLE          :: context(:)
      INTEGER                                :: context_length
      TYPE (intpol_gp_context_x)             :: gp_context

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context%model => a_model
      gp_context%xcart = x_cart

!  Cast model into a data context. This is the equivalent to casting to a void
!  pointer in C.
      context_length = SIZE(TRANSFER(gp_context, context))
      ALLOCATE(context(context_length))
      context = TRANSFER(gp_context, context)

      intpol_get_gp_x = path_integrate(a_model%int_params,                     &
     &                                 this%chord_path, gp_function_x,
     &                                 context)

      DEALLOCATE(context)

      CALL this%scale_and_offset(a_model, intpol_get_gp_x)

      CALL profiler_set_stop_time('intpol_get_gp_x', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for an polarimetry signal and a
!>  position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  Density kernels are provided by @ref model::model_get_gp_ne.
!>
!>  @param[in] this    A @ref intpol_pol_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] i       Index of the position for the kernel.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the position and the signal.
!-------------------------------------------------------------------------------
      FUNCTION intpol_pol_get_gp_i(this, a_model, i, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                         :: intpol_pol_get_gp_i
      CLASS (intpol_pol_class), INTENT(in) :: this
      TYPE (model_class), POINTER          :: a_model
      INTEGER, INTENT(in)                  :: i
      INTEGER, INTENT(in)                  :: flags

!  local variables
      CHARACTER(len=1), ALLOCATABLE        :: context(:)
      INTEGER                              :: context_length
      TYPE (intpol_gp_context_i)           :: gp_context
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  The relevant data for the guassian process context.
      gp_context%model => a_model
      gp_context%i = i
      gp_context%flags = flags

!  Cast model into a data to a context. This is the equivalent to casting to a
!  void pointer in C.
      context_length = SIZE(TRANSFER(gp_context, context))
      ALLOCATE(context(context_length))
      context = TRANSFER(gp_context, context)

      intpol_pol_get_gp_i = intpol_polar_constant                              &
     &                    * (this%wavelength**2.0_dp)                          &
     &                    * path_integrate(a_model%int_params,                 &
     &                                     this%chord_path,                    &
     &                                     gp_pol_function_i,                  &
     &                                     context)

      DEALLOCATE(context)

      IF (this%in_degrees) THEN
         intpol_pol_get_gp_i = intpol_pol_get_gp_i/degree
      END IF
      CALL this%scale_and_offset(a_model, intpol_pol_get_gp_i)

      CALL profiler_set_stop_time('intpol_pol_get_gp_i', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for an polarimetry signal and a
!>  signal.
!>
!>  Calculates the guassian process kernel between the signal and a signal.
!>  Calls back to the @ref signal module to call the other signal. This is the
!>  first signal.
!>
!>  @param[in] this    A @ref intpol_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] signal  A @ref signal_class instance.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION intpol_pol_get_gp_s(this, a_model, signal, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                         :: intpol_pol_get_gp_s
      CLASS (intpol_pol_class), INTENT(in) :: this
      TYPE (model_class), POINTER          :: a_model
      CLASS (signal_class), POINTER        :: signal
      INTEGER, INTENT(in)                  :: flags

!  local variables
      CHARACTER(len=1), ALLOCATABLE        :: context(:)
      INTEGER                              :: context_length
      TYPE (intpol_gp_context_s)           :: gp_context
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  The relevant data for the guassian process context.
      gp_context%model => a_model
      gp_context%signal => signal
      gp_context%flags = flags

!  Cast model into a data to a context. This is the equivalent to casting to a
!  void pointer in C.
      context_length = SIZE(TRANSFER(gp_context, context))
      ALLOCATE(context(context_length))
      context = TRANSFER(gp_context, context)

      intpol_pol_get_gp_s = intpol_polar_constant                              &
     &                    * (this%wavelength**2.0_dp)                          &
     &                    * path_integrate(a_model%int_params,                 &
     &                                     this%chord_path,                    &
     &                                     gp_pol_function_s,                  &
     &                                     context)

      DEALLOCATE(context)

      IF (this%in_degrees) THEN
         intpol_pol_get_gp_s = intpol_pol_get_gp_s/degree
      END IF
      CALL this%scale_and_offset(a_model, intpol_pol_get_gp_s)

      CALL profiler_set_stop_time('intpol_pol_get_gp_s', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a polarimetry signal and a
!>  cartesian position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  This is the second signal.
!>
!>  @param[in] this    A @ref intpol_pol_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] x_cart  The cartesian position of to get the kernel at.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION intpol_pol_get_gp_x(this, a_model, x_cart, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: intpol_pol_get_gp_x
      CLASS (intpol_pol_class), INTENT(in)   :: this
      TYPE (model_class), POINTER            :: a_model
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: flags

!  local variables
      REAL (rprec)                           :: start_time
      CHARACTER(len=1), ALLOCATABLE          :: context(:)
      INTEGER                                :: context_length
      TYPE (intpol_gp_context_x)             :: gp_context

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context%model => a_model
      gp_context%xcart = x_cart

!  Cast model into a data context. This is the equivalent to casting to a void
!  pointer in C.
      context_length = SIZE(TRANSFER(gp_context, context))
      ALLOCATE(context(context_length))
      context = TRANSFER(gp_context, context)

      intpol_pol_get_gp_x = intpol_polar_constant                              &
     &                    * (this%wavelength**2.0_dp)                          &
     &                    * path_integrate(a_model%int_params,                 &
     &                                     this%chord_path,                    &
     &                                     gp_pol_function_x,                  &
     &                                     context)

      DEALLOCATE(context)

      IF (this%in_degrees) THEN
         intpol_pol_get_gp_x = intpol_pol_get_gp_x/degree
      END IF
      CALL this%scale_and_offset(a_model, intpol_pol_get_gp_x)

      CALL profiler_set_stop_time('intpol_pol_get_gp_x', start_time)

      END FUNCTION

!*******************************************************************************
!  PRIVATE
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Interferometer callback function.
!>
!>  Returns the value of the denisty times the change in path length. This
!>  function is passed to @ref integration_path::path_integrate to act as a
!>  callback. The denisty is provided by @ref model::model_get_ne.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref model.
!>  @param[in] xcart   A integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration. Not used in this function.
!>  @param[in] dx      A scalar change in path.
!>  @returns The ne(x)*dl at point x.
!-------------------------------------------------------------------------------
      FUNCTION int_function(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx
      REAL (rprec)                           :: int_function

!  local variables
      TYPE (model_class)                     :: a_model
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      a_model = TRANSFER(context, a_model)
      int_function = model_get_ne(a_model, xcart)*dx

      CALL profiler_set_stop_time('int_function', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Polarmetry callback function.
!>
!>  Returns the value of the denisty times the dot product of the magnetic
!>  field vector and the path direction. This function is passed to
!>  @ref integration_path::path_integrate to act as a callback. The denisty is
!>  provided by @ref model::model_get_ne. The magnetic field vector is proved by
!>  @ref equilibrium::equilibrium_get_B_vec
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref model.
!>  @param[in] xcart   A integration point.
!>  @param[in] dxcart  A vector change in path.
!>  @param[in] length  Length along the integration. Not used in this function.
!>  @param[in] dx      A scalar change in path. Not used in this function.
!>  @returns The ne(x)*B.dl at point x.
!-------------------------------------------------------------------------------
      FUNCTION pol_function(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx
      REAL (rprec)                           :: pol_function

!  local variables
      TYPE (model_class)                     :: a_model
      REAL (rprec), DIMENSION(3)             :: bcart
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      a_model = TRANSFER(context, a_model)

      bcart = a_model%equilibrium%get_B_vec(xcart, .false.)
      pol_function =                                                           &
     &   model_get_ne(a_model, xcart)*DOT_PRODUCT(bcart, dxcart)

      CALL profiler_set_stop_time('pol_function', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Electron density gaussian process callback function for signal point
!>  kernel evaluation.
!>
!>  Returns the value of the density guassian process kernel times the change in
!>  path length. This function is passed to
!>  @ref integration_path::path_integrate to act as a callback. The density
!>  kernel is provided by @ref model::model_get_gp_ne. This is the second
!>  signal.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref intpol_gp_context_i for the model.
!>  @param[in] xcart   The integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration.
!>  @param[in] dx      A scalar change in path.
!>  @returns The k(x, i)*dl at point x.
!-------------------------------------------------------------------------------
      FUNCTION gp_function_i(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: gp_function_i
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx

! local variables
      TYPE (intpol_gp_context_i)             :: gp_context
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context = TRANSFER(context, gp_context)
      gp_function_i = model_get_gp_ne(gp_context%model, xcart,                 &
     &                                gp_context%i)*dx

      CALL profiler_set_stop_time('gp_function_i', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Electron density gaussian process callback function for signal point
!>  kernel evaluation for a polarimetry signal.
!>
!>  Returns the value of the density guassian process kernel times the change in
!>  path length. This function is passed to
!>  @ref integration_path::path_integrate to act as a callback. The density
!>  kernel is provided by @ref model::model_get_gp_ne. This is the second
!>  signal.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref intpol_gp_context_i for the model.
!>  @param[in] xcart   The integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration.
!>  @param[in] dx      A scalar change in path.
!>  @returns The k(x, i)*dl at point x.
!-------------------------------------------------------------------------------
      FUNCTION gp_pol_function_i(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: gp_pol_function_i
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx

! local variables
      TYPE (intpol_gp_context_i)             :: gp_context
      REAL (rprec), DIMENSION(3)             :: bcart
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context = TRANSFER(context, gp_context)
      bcart = gp_context%model%equilibrium%get_B_vec(xcart, .false.)
      gp_pol_function_i = model_get_gp_ne(gp_context%model, xcart,             &
     &                                    gp_context%i)                        *
     &                  * DOT_PRODUCT(bcart, dxcart)

      CALL profiler_set_stop_time('gp_pol_function_i', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Interferometry gaussian process callback function for signal signal
!>  kernel evaluation.
!>
!>  Returns the value of the density guassian process kernel times the change in
!>  path length. This function is passed to
!>  @ref integration_path::path_integrate to act as a callback. The density
!>  kernel is provided by @ref model::model_get_gp_ne. This is the first signal.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref intpol_gp_context_i for the model.
!>  @param[in] xcart   The integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration.
!>  @param[in] dx      A scalar change in path.
!>  @returns The k(x, i)*dl at point x.
!-------------------------------------------------------------------------------
      FUNCTION gp_function_s(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: gp_function_s
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx

! local variables
      TYPE (intpol_gp_context_s)             :: gp_context
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context = TRANSFER(context, gp_context)
      gp_function_s = gp_context%signal%get_gp(gp_context%model, xcart,        &
     &                                         gp_context%flags)*dx

      CALL profiler_set_stop_time('gp_function_s', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Polarimetry gaussian process callback function for signal signal
!>  kernel evaluation.
!>
!>  Returns the value of the density guassian process kernel times the change in
!>  path length dotted with the magnetic field vector. This function is passed
!>  to @ref integration_path::path_integrate to act as a callback. The density
!>  kernel is provided by @ref model::model_get_gp_ne. This is the first signal.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref intpol_gp_context_i for the model.
!>  @param[in] xcart   The integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration.
!>  @param[in] dx      A scalar change in path.
!>  @returns The k(x, i)*dl at point x.
!-------------------------------------------------------------------------------
      FUNCTION gp_pol_function_s(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: gp_pol_function_s
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx

! local variables
      TYPE (intpol_gp_context_s)             :: gp_context
      REAL (rprec), DIMENSION(3)             :: bcart
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context = TRANSFER(context, gp_context)
      bcart = gp_context%model%equilibrium%get_B_vec(xcart, .false.)
      gp_pol_function_s = gp_context%signal%get_gp(gp_context%model,           &
     &                                             xcart,                      &
     &                                             gp_context%flags)           &
     &                  * DOT_PRODUCT(bcart, dxcart)

      CALL profiler_set_stop_time('gp_pol_function_s', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Interferometry gaussian process callback function for kernel
!>  evaluation of two positions.
!>
!>  This is the second interferometry signal.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref intpol_gp_context_x for the model.
!>  @param[in] xcart   The integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration.
!>  @param[in] dx      A scalar change in path.
!>  @returns The k(x, y)*dl at point y.
!-------------------------------------------------------------------------------
      FUNCTION gp_function_x(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: gp_function_x
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx

! local variables
      TYPE (intpol_gp_context_x)             :: gp_context
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  This is the second signal so put xcart in the second position.
      gp_context = TRANSFER(context, gp_context)
      gp_function_x = model_get_gp_ne(gp_context%model,                        &
     &                                xcart, gp_context%xcart)*dx

      CALL profiler_set_stop_time('gp_function_x', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Polarimetry gaussian process callback function for kernel
!>  evaluation of two positions.
!>
!>  This is the second polarimetry signal.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref intpol_gp_context_x for the model.
!>  @param[in] xcart   The integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration.
!>  @param[in] dx      A scalar change in path.
!>  @returns The k(x, y)*B.dl at point y.
!-------------------------------------------------------------------------------
      FUNCTION gp_pol_function_x(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: gp_pol_function_x
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx

! local variables
      TYPE (intpol_gp_context_x)             :: gp_context
      REAL (rprec), DIMENSION(3)             :: bcart
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  This is the second signal so put xcart in the second position.
      gp_context = TRANSFER(context, gp_context)
      bcart = gp_context%model%equilibrium%get_B_vec(xcart, .false.)
      gp_pol_function_x = model_get_gp_ne(gp_context%model,                    &
     &                                    xcart, gp_context%xcart)             &
     &                  * DOT_PRODUCT(bcart, dxcart)

      CALL profiler_set_stop_time('gp_pol_function_x', start_time)

      END FUNCTION

      END MODULE

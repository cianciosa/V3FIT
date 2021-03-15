!*******************************************************************************
!>  @file ece.f
!>  @brief Contains module @ref ece.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref ece_class.
!>  @par Super Class:
!>  @ref diagnostic
!*******************************************************************************

      MODULE ece

      USE stel_kinds, only: rprec
      USE integration_path
      USE signal

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) ece base class
!  2) ece context
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing an ECE signal.
!>  @par Super Class:
!>  @ref diagnostic
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(signal_class) :: ece_class
!>  The complete path of the chord.
         TYPE (vertex), POINTER :: chord_path => null()
!>  Resonance factor.
         REAL (rprec)           :: resonance
      CONTAINS
         PROCEDURE              ::                                             &
     &      get_modeled_signal_last => ece_get_modeled_signal
         PROCEDURE              :: get_cart => ece_get_cart
         PROCEDURE              ::                                             &
     &      get_type => ece_get_type
         PROCEDURE              :: get_header => ece_get_header
         PROCEDURE              :: get_gp_i => ece_get_gp_i
         PROCEDURE              :: get_gp_s => ece_get_gp_s
         PROCEDURE              :: get_gp_x => ece_get_gp_x
         FINAL                  :: ece_destruct
      END TYPE ece_class

!-------------------------------------------------------------------------------
!> Structure to hold all memory needed to be sent to the callback function.
!-------------------------------------------------------------------------------
      TYPE ece_context
!>  The index of the emissivity profile model.
         REAL (rprec)                :: resonance
!>  Reference to a @ref model::model_class object.
         TYPE (model_class), POINTER :: model => null()
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for ece constructor.
!-------------------------------------------------------------------------------
      INTERFACE ece_class
         MODULE PROCEDURE ece_construct
      END INTERFACE

      PRIVATE :: ece_function, is_in_range

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref ece_class object.
!>
!>  Allocates memory and initializes a @ref ece_class object.
!>
!>  @param[in] start_path Starting point of an ece chord.
!>  @param[in] end_path   Ending point of an ece chord.
!>  @param[in] resonance        Geometric factor of the chord.
!>  @returns A pointer to a constructed @ref ece_class object.
!-------------------------------------------------------------------------------
      FUNCTION ece_construct(start_path, end_path, resonance)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (ece_class), POINTER             :: ece_construct
      REAL (rprec), DIMENSION(3), INTENT(in) :: start_path
      REAL (rprec), DIMENSION(3), INTENT(in) :: end_path
      REAL (rprec), INTENT(in)               :: resonance

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(ece_construct)

      CALL path_append_vertex(ece_construct%chord_path, start_path)
      CALL path_append_vertex(ece_construct%chord_path, end_path)

      ece_construct%resonance = resonance

      CALL profiler_set_stop_time('ece_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref ece_class object.
!>
!>  Deallocates memory and uninitializes a @ref ece_class object.
!>
!>  @param[inout] this A @ref ece_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE ece_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (ece_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%chord_path)) THEN
         CALL path_destruct(this%chord_path)
         this%chord_path => null()
      END IF

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled signal.
!>
!>  Calculates the ECE signal by searching along the chord for the resonance.
!>  The search criteria is provided by the @ref ece_function.
!>
!>  @param[inout] this       A @ref ece_class instance.
!>  @param[in]    a_model    A @ref model instance.
!>  @param[out]   sigma      The modeled sigma.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION ece_get_modeled_signal(this, a_model, sigma, last_value)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: ece_get_modeled_signal
      CLASS (ece_class), INTENT(inout)        :: this
      TYPE (model_class), POINTER             :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out) :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)  :: last_value

! local variables
      LOGICAL                                 :: found
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sigma = 0.0

      IF (BTEST(a_model%state_flags, model_state_vmec_flag)   .or.             &
     &    BTEST(a_model%state_flags, model_state_siesta_flag) .or.             &
     &    BTEST(a_model%state_flags, model_state_te_flag)     .or.             &
     &    BTEST(a_model%state_flags, model_state_shift_flag)  .or.             &
     &    BTEST(a_model%state_flags, model_state_signal_flag)) THEN

         ece_get_modeled_signal(2:4) = this%get_cart(a_model, found)

         IF (found) THEN
            ece_get_modeled_signal(1) =                                        &
     &         model_get_te(a_model, ece_get_modeled_signal(2:4))
         ELSE
            ece_get_modeled_signal(1) = 0.0
         END IF

         CALL this%scale_and_offset(a_model, ece_get_modeled_signal(1))
      ELSE
         ece_get_modeled_signal = last_value
      END IF

      CALL profiler_set_stop_time('ece_get_modeled_signal', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Calculates the signal measurement point.
!>
!>  Calculates the ECE measurement point by searching along the chord for the
!>  resonance. The search criteria is provided by the @ref ece_function.
!>
!>  @param[in]  this    A @ref ece_class instance.
!>  @param[in]  a_model A @ref model instance.
!>  @param[out] found   Flag to indicate if the resonance was found.
!>  @returns Position in cartesian coordinates of the resonance.
!-------------------------------------------------------------------------------
      FUNCTION ece_get_cart(this, a_model, found)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(3)    :: ece_get_cart
      CLASS (ece_class), INTENT(in) :: this
      TYPE (model_class), POINTER   :: a_model
      LOGICAL, INTENT(out)          :: found

! local variables
      CHARACTER(len=1), ALLOCATABLE :: context(:)
      INTEGER                       :: context_length
      TYPE (ece_context)            :: temp_context
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  The relevant data for the ece context.
      temp_context%resonance = this%resonance
      temp_context%model => a_model

!  Cast model into a data to a context. This is the equivalent to casting to a
!  void pointer in C.
      context_length = SIZE(TRANSFER(temp_context, context))
      ALLOCATE(context(context_length))
      context = TRANSFER(temp_context, context)

      ece_get_cart = path_search(this%chord_path, ece_function, context,       &
     &                           found)

      DEALLOCATE(context)

      CALL profiler_set_stop_time('ece_get_cart', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the ece type.
!>
!>  Returns a description of the ece type for use when writting output files.
!>
!>  @param[in] this A @ref ece_class instance.
!>  @returns A string describing the ece type.
!-------------------------------------------------------------------------------
      FUNCTION ece_get_type(this)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: ece_get_type
      CLASS (ece_class), INTENT(in)    :: this

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ece_get_type = 'ece'

      CALL profiler_set_stop_time('ece_get_type', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the model and model sigma array indices.
!>
!>  Returns a description of the array indices for use when writting output
!>  files.
!>
!>  @param[in]    this   A @ref ece_class instance.
!>  @param[inout] header Buffer arrays to write header strings to.
!>  @returns A string describing the model and model sigma array indices.
!-------------------------------------------------------------------------------
      SUBROUTINE ece_get_header(this, header)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CLASS (ece_class), INTENT(in)                  :: this
      CHARACTER (len=data_name_length), DIMENSION(7), INTENT(inout) ::         &
     &   header

!  local variables
      REAL (rprec)                                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      header(1) = 'x (m)'
      header(2) = 'y (m)'
      header(3) = 'z (m)'

      header(4) = 'model_sig(1)'
      header(5) = 'model_sig(2)'
      header(6) = 'model_sig(3)'
      header(7) = 'model_sig(4)'

      CALL profiler_set_stop_time('ece_get_header', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for an ece signal and a position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  Temperature kernels are provided by @ref model::model_get_gp_te.
!>
!>  @param[in] this         A @ref ece_class instance.
!>  @param[in] a_model      A @ref model instance.
!>  @param[in] i            Index of the position for the kernel.
!>  @param[in] flags        State flags to send to the kernel.
!>  @returns Kernel value for the position and the signal.
!-------------------------------------------------------------------------------
      FUNCTION ece_get_gp_i(this, a_model, i, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                  :: ece_get_gp_i
      CLASS (ece_class), INTENT(in) :: this
      TYPE (model_class), POINTER   :: a_model
      INTEGER, INTENT(in)           :: i
      INTEGER, INTENT(in)           :: flags

!  local variables
      REAL (rprec), DIMENSION(3)    :: x_cart
      LOGICAL                       :: found
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      x_cart = this%get_cart(a_model, found)
      IF (found) THEN
         ece_get_gp_i = model_get_gp_te(a_model, x_cart, i)
      ELSE
         ece_get_gp_i = 0.0
      END IF

      CALL this%scale_and_offset(a_model, ece_get_gp_i)

      CALL profiler_set_stop_time('ece_get_gp_i', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a ece signal and a signal.
!>
!>  Calculates the guassian process kernel between the signal and a signal.
!>  Calls back to the @ref signal module to call the other signal.
!>
!>  @param[in] this    A @ref ece_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] signal  A @ref signal_class instance for the second signal.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION ece_get_gp_s(this, a_model, signal, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                  :: ece_get_gp_s
      CLASS (ece_class), INTENT(in) :: this
      TYPE (model_class), POINTER   :: a_model
      CLASS (signal_class), POINTER :: signal
      INTEGER, INTENT(in)           :: flags

!  local variables
      REAL (rprec), DIMENSION(3)    :: x_cart
      LOGICAL                       :: found
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      x_cart = this%get_cart(a_model, found)
      IF (found) THEN
         ece_get_gp_s = signal%get_gp(a_model, x_cart, flags)
      ELSE
         ece_get_gp_s = 0.0
      END IF

      CALL this%scale_and_offset(a_model, ece_get_gp_s)

      CALL profiler_set_stop_time('ece_get_gp_s', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for an ece signal and a cartesian
!>  position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  Temperature kernels are provided by @ref model::model_get_gp_te. This is the
!>  second signal so x_cart goes in the second position and this signal in the
!>  second.
!>
!>  @param[in] this         A @ref ece_class instance.
!>  @param[in] a_model      A @ref model instance.
!>  @param[in] x_cart       The cartesian position of to get the kernel at.
!>  @param[in] flags        State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION ece_get_gp_x(this, a_model, x_cart, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: ece_get_gp_x
      CLASS (ece_class), INTENT(in)          :: this
      TYPE (model_class), POINTER            :: a_model
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: flags

!  local variables
      REAL (rprec), DIMENSION(3)             :: y_cart
      LOGICAL                                :: found
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      y_cart = this%get_cart(a_model, found)
      IF (found) THEN
         ece_get_gp_x = model_get_gp_te(a_model, y_cart, x_cart)
      ELSE
         ece_get_gp_x = 0.0
      END IF

      CALL this%scale_and_offset(a_model, ece_get_gp_x)

      CALL profiler_set_stop_time('ece_get_gp_x', start_time)

      END FUNCTION

!*******************************************************************************
!  PRIVATE
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief ECE callback function.
!>
!>  Returns true if the magnetic field resonance if found with in a specified
!>  range.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref ece_context for the model.
!>  @param[in] xcart1  The upper search point.
!>  @param[in] xcart2  The lower search point.
!>  @returns True if the resonace was found.
!-------------------------------------------------------------------------------
      FUNCTION ece_function(context, xcart1, xcart2)
      USE coordinate_utilities

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart1
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart2
      LOGICAL                                :: ece_function

! local variables
      TYPE (ece_context)                     :: temp_context
      REAL (rprec), DIMENSION(3)             :: bcart1
      REAL (rprec), DIMENSION(3)             :: bcart2
      REAL (rprec)                           :: bmod1
      REAL (rprec)                           :: bmod2
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ece_function = .false.

      temp_context = TRANSFER(context, temp_context)

      bcart1 = equilibrium_get_B_vec(temp_context%model%equilibrium,           &
     &                               xcart1, .false.)
      bcart2 = equilibrium_get_B_vec(temp_context%model%equilibrium,           &
     &                               xcart2, .false.)

      bmod1 = SQRT(DOT_PRODUCT(bcart1, bcart1))
      bmod2 = SQRT(DOT_PRODUCT(bcart2, bcart2))

      IF (ABS(bmod1 - bmod2) .lt.                                              &
     &    temp_context%model%resonace_range) THEN
         ece_function = is_in_range(temp_context%resonance,                    &
     &                              bmod1, bmod2)
      END IF

      CALL profiler_set_stop_time('ece_function', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Check if values is in range.
!>
!>  Returns true the value if x_low <= xp <= x_high
!>
!>  @param[in] xp Value to see if it is in range.
!>  @param[in] x1 First bounding point to check.
!>  @param[in] x2 Second bounding point to check.
!>  @returns True if the resonace was found.
!-------------------------------------------------------------------------------
      PURE FUNCTION is_in_range(xp, x1, x2)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL                 :: is_in_range
      REAL(rprec), INTENT(in) :: xp
      REAL(rprec), INTENT(in) :: x1
      REAL(rprec), INTENT(in) :: x2

!  Start of executable code
      IF (x1 .gt. x2) THEN
         is_in_range = xp .ge. x2 .and. xp .le. x1
      ELSE
         is_in_range = xp .ge. x1 .and. xp .le. x2
      END IF

      END FUNCTION

      END MODULE

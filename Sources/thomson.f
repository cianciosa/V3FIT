!*******************************************************************************
!>  @file thomson.f
!>  @brief Contains module @ref thomson.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Implements thomson scattering diagnostic. Defines the base class of the type
!>  @ref thomson_class.
!>  @par Super Class:
!>  @ref diagnostic
!*******************************************************************************

      MODULE thomson

      USE stel_kinds, only: rprec
      USE mpi_inc
      USE profiler
      USE model
      USE signal

      IMPLICIT NONE

!*******************************************************************************
!  thomson module parameters
!*******************************************************************************
!>  Assert message for pressure methods
      CHARACTER (len=*), PARAMETER ::                                          &
     &   thomson_p_error = 'Guassian processes are not supported ' //          &
     &                     'for thomson pressure measurements.'

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) thomson base class
!  2) thomson te class
!  3) thomson ne class
!  4) thomson p class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a thomson scattering signal.
!>  @par Super Class:
!>  @ref signal
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(signal_class) :: thomson_class
!>  Position of the thomson scattering point.
         REAL (rprec), DIMENSION(3) :: xcart
      CONTAINS
         PROCEDURE                  :: get_type => thomson_get_type
         FINAL                      :: thomson_destruct
      END TYPE

!-------------------------------------------------------------------------------
!>  Base class representing a thomson scattering te signal.
!>  @par Super Class:
!>  @ref thomson_class
!-------------------------------------------------------------------------------
      TYPE, EXTENDS (thomson_class) :: thomson_te_class
      CONTAINS
         PROCEDURE ::                                                          &
     &      get_modeled_signal_last => thomson_te_get_modeled_signal
         PROCEDURE :: get_gp_i => thomson_te_get_gp_i
         PROCEDURE :: get_gp_x => thomson_te_get_gp_x
      END TYPE

!-------------------------------------------------------------------------------
!>  Base class representing a thomson scattering te signal.
!>  @par Super Class:
!>  @ref thomson_class
!-------------------------------------------------------------------------------
      TYPE, EXTENDS (thomson_class) :: thomson_ne_class
      CONTAINS
         PROCEDURE ::                                                          &
     &      get_modeled_signal_last => thomson_ne_get_modeled_signal
         PROCEDURE :: get_gp_i => thomson_ne_get_gp_i
         PROCEDURE :: get_gp_x => thomson_ne_get_gp_x
      END TYPE

!-------------------------------------------------------------------------------
!>  Base class representing a thomson scattering te signal.
!>  @par Super Class:
!>  @ref thomson_class
!-------------------------------------------------------------------------------
      TYPE, EXTENDS (thomson_class) :: thomson_p_class
      CONTAINS
         PROCEDURE ::                                                          &
     &      get_modeled_signal_last => thomson_p_get_modeled_signal
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for thomson te constructor.
!-------------------------------------------------------------------------------
      INTERFACE thomson_te_class
         MODULE PROCEDURE thomson_te_construct
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for thomson te constructor.
!-------------------------------------------------------------------------------
      INTERFACE thomson_ne_class
         MODULE PROCEDURE thomson_ne_construct
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for thomson te constructor.
!-------------------------------------------------------------------------------
      INTERFACE thomson_p_class
         MODULE PROCEDURE thomson_p_construct
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref thomson_class object measureing temperature.
!>
!>  @param[in] xcart The measurement point.
!>  @returns A pointer to a constructed @ref thomson_class object.
!-------------------------------------------------------------------------------
      FUNCTION thomson_te_construct(xcart)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (thomson_te_class), POINTER      :: thomson_te_construct
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(thomson_te_construct)
      thomson_te_construct%xcart = xcart

      CALL profiler_set_stop_time('thomson_te_construct', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref thomson_class object measureing density.
!>
!>  @param[in] xcart The measurement point.
!>  @returns A pointer to a constructed @ref thomson_class object.
!-------------------------------------------------------------------------------
      FUNCTION thomson_ne_construct(xcart)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (thomson_ne_class), POINTER      :: thomson_ne_construct
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(thomson_ne_construct)
      thomson_ne_construct%xcart = xcart

      CALL profiler_set_stop_time('thomson_ne_construct', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref thomson_class object measureing pressure.
!>
!>  @param[in] xcart The measurement point.
!>  @returns A pointer to a constructed @ref thomson_class object.
!-------------------------------------------------------------------------------
      FUNCTION thomson_p_construct(xcart)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (thomson_p_class), POINTER       :: thomson_p_construct
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(thomson_p_construct)
      thomson_p_construct%xcart = xcart

      CALL profiler_set_stop_time('thomson_p_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref thomson_class object.
!>
!>  Uninitializes a @ref thomson_class object.
!>
!>  @param[inout] this A @ref thomson_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE thomson_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (thomson_class), INTENT(inout) :: this

!  Start of executable code
      this%xcart = 0.0

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled te signal.
!>
!>  Calculates the modeled signal at a point. Temperature is provided by
!>  @ref model::model_get_te.
!>
!>  @param[inout] this       A @ref thomson_te_class instance.
!>  @param[in]    a_model    A @ref model instance.
!>  @param[out]   sigma      The modeled sigma.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION thomson_te_get_modeled_signal(this, a_model, sigma,             &
     &                                       last_value)
      USE model

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: thomson_te_get_modeled_signal
      CLASS (thomson_te_class), INTENT(inout) :: this
      TYPE (model_class), POINTER             :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out) :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)  :: last_value

!  local variables
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sigma = 0.0

      IF (BTEST(a_model%state_flags, model_state_vmec_flag)   .or.             &
     &    BTEST(a_model%state_flags, model_state_siesta_flag) .or.             &
     &    BTEST(a_model%state_flags, model_state_te_flag)     .or.             &
     &    BTEST(a_model%state_flags, model_state_shift_flag)  .or.             &
     &    BTEST(a_model%state_flags, model_state_signal_flag)) THEN

         thomson_te_get_modeled_signal = 0.0
         thomson_te_get_modeled_signal(1) = model_get_te(a_model,              &
     &                                                   this%xcart)

         CALL this%scale_and_offset(a_model,                                   &
     &                              thomson_te_get_modeled_signal(1))
      ELSE
         thomson_te_get_modeled_signal = last_value
      END IF

      CALL profiler_set_stop_time('thomson_te_get_modeled_signal',             &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled ne signal.
!>
!>  Calculates the modeled signal at a point. Density is provided by
!>  @ref model::model_get_ne.
!>
!>  @param[inout]  this       A @ref thomson_ne_class instance.
!>  @param[in]     a_model    A @ref model instance.
!>  @param[out]    sigma      The modeled sigma.
!>  @param[in]     last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION thomson_ne_get_modeled_signal(this, a_model, sigma,             &
     &                                       last_value)
      USE model

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: thomson_ne_get_modeled_signal
      CLASS (thomson_ne_class), INTENT(inout) :: this
      TYPE (model_class), POINTER             :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out) :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)  :: last_value

!  local variables
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sigma = 0.0

      IF (BTEST(a_model%state_flags, model_state_vmec_flag)   .or.             &
     &    BTEST(a_model%state_flags, model_state_siesta_flag) .or.             &
     &    BTEST(a_model%state_flags, model_state_ne_flag)     .or.             &
     &    BTEST(a_model%state_flags, model_state_shift_flag)  .or.             &
     &    BTEST(a_model%state_flags, model_state_signal_flag)) THEN

         thomson_ne_get_modeled_signal = 0.0
         thomson_ne_get_modeled_signal(1) = model_get_ne(a_model,              &
     &                                                   this%xcart)

         CALL this%scale_and_offset(a_model,                                   &
     &                              thomson_ne_get_modeled_signal(1))
      ELSE
         thomson_ne_get_modeled_signal = last_value
      END IF

      CALL profiler_set_stop_time('thomson_ne_get_modeled_signal',             &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled pressure signal.
!>
!>  Calculates the modeled signal at a point. Pressure is provided by
!>  @ref model::model_get_p.
!>
!>  @param[inout] this       A @ref thomson_p_class instance.
!>  @param[in]    a_model    A @ref model instance.
!>  @param[out]   sigma      The modeled sigma.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION thomson_p_get_modeled_signal(this, a_model, sigma,              &
     &                                      last_value)
      USE model

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: thomson_p_get_modeled_signal
      CLASS (thomson_p_class), INTENT(inout)  :: this
      TYPE (model_class), POINTER             :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out) :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)  :: last_value

!  local variables
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sigma = 0.0

      IF (BTEST(a_model%state_flags, model_state_vmec_flag)   .or.             &
     &    BTEST(a_model%state_flags, model_state_siesta_flag) .or.             &
     &    BTEST(a_model%state_flags, model_state_shift_flag)  .or.             &
     &    BTEST(a_model%state_flags, model_state_signal_flag)) THEN

         thomson_p_get_modeled_signal = 0.0
         thomson_p_get_modeled_signal(1) =                                     &
     &      a_model%equilibrium%get_p(this%xcart, .false.)

         CALL this%scale_and_offset(a_model,                                   &
     &                              thomson_p_get_modeled_signal(1))
      ELSE
         thomson_p_get_modeled_signal = last_value
      END IF

      CALL profiler_set_stop_time('thomson_p_get_modeled_signal',              &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the thomson type.
!>
!>  Returns a description of the thomson type for use when writting output
!>  files.
!>
!>  @param[in] this A @ref thomson_class instance.
!>  @returns A string describing the thomson type.
!-------------------------------------------------------------------------------
      FUNCTION thomson_get_type(this)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length)  :: thomson_get_type
      CLASS (thomson_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      thomson_get_type = 'thscte'

      CALL profiler_set_stop_time('thomson_get_type', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a thomson signal and a signal.
!>
!>  Calculates the guassian process kernel between the signal and a signal.
!>  Calls back to the @ref signal module to call the other signal. This does not
!>  support pressure measurements.
!>
!>  @param[in] this     A @ref thomson_class instance.
!>  @param[in] context  A @ref signal_class instance.
!>  @param     callback Function pointer to call back to signal module.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION thomson_get_gp_s(this, a_model, signal, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                      :: thomson_get_gp_s
      CLASS (thomson_class), INTENT(in) :: this
      TYPE (model_class), POINTER       :: a_model
      CLASS (signal_class), INTENT(in)  :: signal
      INTEGER, INTENT(in)               :: flags

!  local variables
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      thomson_get_gp_s = signal%get_gp(a_model, this%xcart, flags)
      CALL this%scale_and_offset(a_model, thomson_get_gp_s)

      CALL profiler_set_stop_time('thomson_get_gp_s', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a thomson te signal and a
!>  position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  Temperature kernels are provided by @ref model::model_get_gp_te.
!>
!>  @param[in] this    A @ref thomson_te_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] i       Index of the position for the kernel.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the position and the signal.
!-------------------------------------------------------------------------------
      FUNCTION thomson_te_get_gp_i(this, a_model, i, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                         :: thomson_te_get_gp_i
      CLASS (thomson_te_class), INTENT(in) :: this
      TYPE (model_class), POINTER          :: a_model
      INTEGER, INTENT(in)                  :: i
      INTEGER, INTENT(in)                  :: flags

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      thomson_te_get_gp_i = model_get_gp_te(a_model, this%xcart, i)

      CALL this%scale_and_offset(a_model, thomson_te_get_gp_i)

      CALL profiler_set_stop_time('thomson_te_get_gp_i', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a thomson te signal and a
!>  cartesian position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  Temperature kernels are provided by @ref model::model_get_gp_te. This is the
!>  second signal so x_cart goes in the second position and this signal in the
!>  first.
!>
!>  @param[in] this    A @ref thomson_te_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] x_cart  The cartesian position of to get the kernel at.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION thomson_te_get_gp_x(this, a_model, x_cart, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: thomson_te_get_gp_x
      CLASS (thomson_te_class), INTENT(in)   :: this
      TYPE (model_class), POINTER            :: a_model
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: flags

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      thomson_te_get_gp_x = model_get_gp_te(a_model, this%xcart, x_cart)
      CALL this%scale_and_offset(a_model, thomson_te_get_gp_x)

      CALL profiler_set_stop_time('thomson_te_get_gp_x', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a thomson ne signal and a
!>  position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  Denisty kernels are provided by @ref model::model_get_gp_ne. This does not
!>  support pressure measurements.
!>
!>  @param[in] this    A @ref thomson_ne_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] i       Index of the position for the kernel.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the position and the signal.
!-------------------------------------------------------------------------------
      FUNCTION thomson_ne_get_gp_i(this, a_model, i, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                         :: thomson_ne_get_gp_i
      CLASS (thomson_ne_class), INTENT(in) :: this
      TYPE (model_class), POINTER          :: a_model
      INTEGER, INTENT(in)                  :: i
      INTEGER, INTENT(in)                  :: flags

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      thomson_ne_get_gp_i = model_get_gp_ne(a_model, this%xcart, i)
      CALL this%scale_and_offset(a_model, thomson_ne_get_gp_i)

      CALL profiler_set_stop_time('thomson_ne_get_gp_i', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a thomson ne signal and a
!>  cartesian position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  Denisty kernels are provided by @ref model::model_get_gp_ne. This is the
!>  second signal so x_cart goes in the second position and this signal in the
!>  first.
!>
!>  @param[in] this    A @ref thomson_p_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] x_cart  The cartesian position of to get the kernel at.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION thomson_ne_get_gp_x(this, a_model, x_cart, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: thomson_ne_get_gp_x
      CLASS (thomson_ne_class), INTENT(in)   :: this
      TYPE (model_class), POINTER            :: a_model
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: flags

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      thomson_ne_get_gp_x = model_get_gp_ne(a_model, this%xcart, x_cart)
      CALL this%scale_and_offset(a_model, thomson_ne_get_gp_x)

      CALL profiler_set_stop_time('thomson_ne_get_gp_x', start_time)

      END FUNCTION

      END MODULE

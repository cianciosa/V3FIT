!*******************************************************************************
!>  @file sxrem.f
!>  @brief Contains module @ref sxrem.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref sxrem_class.
!>  @par Super Class:
!>  @ref diagnostic
!*******************************************************************************

      MODULE sxrem

      USE stel_kinds, only: rprec
      USE integration_path
      USE model
      USE signal

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) sxrem base class
!  2) sxrem emiss class
!  3) sxrem ti class
!  2) sxrem context
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a soft x-ray signal.
!>  @par Super Class:
!>  @ref signal
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(signal_class) :: sxrem_class
!>  The complete path of the chord.
         TYPE (vertex), POINTER :: chord_path => null()
!>  Index of the soft x-ray emissivity profile for this chord.
         INTEGER                :: profile_number
      CONTAINS
         FINAL                  :: sxrem_destruct
      END TYPE

!-------------------------------------------------------------------------------
!>  Base class representing a soft x-ray emissivity signal.
!>  @par Super Class:
!>  @ref sxrem_class
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(sxrem_class) :: sxrem_emiss_class
!>  Geometric chord factor.
         REAL (rprec) :: geo
      CONTAINS
         PROCEDURE    ::                                                       &
     &      get_modeled_signal_last => sxrem_emiss_get_modeled_signal
         PROCEDURE    :: get_type => sxrem_emiss_get_type
         PROCEDURE    :: get_gp_i => sxrem_emiss_get_gp_i
         PROCEDURE    :: get_gp_s => sxrem_emiss_get_gp_s
         PROCEDURE    :: get_gp_x => sxrem_emiss_get_gp_x
         FINAL        :: sxrem_emiss_destruct
      END TYPE

!-------------------------------------------------------------------------------
!>  Base class representing a soft x-ray ti signal.
!>  @par Super Class:
!>  @ref sxrem_class
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(sxrem_class) :: sxrem_ti_class
      CONTAINS
         PROCEDURE ::                                                          &
     &      get_modeled_signal_last => sxrem_ti_get_modeled_signal
         PROCEDURE :: get_type => sxrem_ti_get_type
         PROCEDURE :: get_gp_i => sxrem_ti_get_gp_i
         PROCEDURE :: get_gp_s => sxrem_ti_get_gp_s
         PROCEDURE :: get_gp_x => sxrem_ti_get_gp_x
      END TYPE

!-------------------------------------------------------------------------------
!>  Structure to hold all memory needed to be sent to the callback function.
!-------------------------------------------------------------------------------
      TYPE sxrem_context
!>  The index of the emissivity profile model.
         INTEGER                     :: profile_number
!>  Reference to a @ref model::model_class object.
         TYPE (model_class), POINTER :: model => null()
      END TYPE

!-------------------------------------------------------------------------------
!>  Structure to hold all memory needed to be sent to the guassian process
!>  callback function for point.
!-------------------------------------------------------------------------------
      TYPE sxrem_gp_context_i
!>  The index of the emissivity profile model.
         INTEGER                     :: profile_number
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
      TYPE sxrem_gp_context_s
!>  The index of the emissivity profile model.
         INTEGER                       :: profile_number
!>  Reference to a @ref model::model_class object.
         TYPE (model_class), POINTER   :: model => null()
!>  Second signal
         CLASS (signal_class), POINTER :: signal => null()
!>  Gaussian process kernel flags.
         INTEGER                       :: flags
      END TYPE

!-------------------------------------------------------------------------------
!>  Structure to hold all memory needed to be sent to the guassian process
!>  callback function for signal.
!-------------------------------------------------------------------------------
      TYPE sxrem_gp_context_x
!>  The index of the emissivity profile model.
         INTEGER                     :: profile_number
!>  Reference to a @ref model::model_class object.
         TYPE (model_class), POINTER :: model => null()
!>  First position.
         REAL (rprec), DIMENSION(3)  :: xcart
!>  Gaussian process kernel flags.
         INTEGER                     :: flags
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for sxrem emission constructor.
!-------------------------------------------------------------------------------
      INTERFACE sxrem_emiss_class
         MODULE PROCEDURE sxrem_emiss_construct
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for sxrem ti constructor.
!-------------------------------------------------------------------------------
      INTERFACE sxrem_ti_class
         MODULE PROCEDURE sxrem_ti_construct
      END INTERFACE

      PRIVATE :: sxr_function, ti_function
      PRIVATE :: gp_emiss_function_i, gp_ti_function_i
      PRIVATE :: gp_emiss_function_s, gp_ti_function_s
      PRIVATE :: gp_emiss_function_x, gp_ti_function_x

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref sxrem_class object for emission.
!>
!>  Allocates memory and initializes a @ref sxrem_class object.
!>
!>  @param[in] start_path     Starting point of a sxrem chord.
!>  @param[in] end_path       Ending point of a sxrem chord.
!>  @param[in] geo            Geometric factor of the chord.
!>  @param[in] profile_number Index of the soft x-ray emissivity profile.
!>  @returns A pointer to a constructed @ref sxrem_class object.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_emiss_construct(start_path, end_path, geo,                &
     &                               profile_number)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (sxrem_emiss_class), POINTER     :: sxrem_emiss_construct
      REAL (rprec), DIMENSION(3), INTENT(in) :: start_path
      REAL (rprec), DIMENSION(3), INTENT(in) :: end_path
      REAL (rprec), INTENT(in)               :: geo
      INTEGER, INTENT(in)                    :: profile_number

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(sxrem_emiss_construct)

      CALL path_append_vertex(sxrem_emiss_construct%chord_path,                &
     &                        start_path)
      CALL path_append_vertex(sxrem_emiss_construct%chord_path,                &
     &                        end_path)

      sxrem_emiss_construct%geo = geo
      sxrem_emiss_construct%profile_number = profile_number

      CALL profiler_set_stop_time('sxrem_emiss_construct', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref sxrem_class object for ti.
!>
!>  Allocates memory and initializes a @ref sxrem_class object.
!>
!>  @param[in] start_path     Starting point of a sxrem chord.
!>  @param[in] end_path       Ending point of a sxrem chord.
!>  @param[in] profile_number Index of the soft x-ray emissivity profile.
!>  @returns A pointer to a constructed @ref sxrem_class object.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_ti_construct(start_path, end_path, profile_number)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (sxrem_ti_class), POINTER         :: sxrem_ti_construct
      REAL (rprec), DIMENSION(3), INTENT(in) :: start_path
      REAL (rprec), DIMENSION(3), INTENT(in) :: end_path
      INTEGER, INTENT(in)                    :: profile_number

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(sxrem_ti_construct)

      CALL path_append_vertex(sxrem_ti_construct%chord_path,                   &
     &                        start_path)
      CALL path_append_vertex(sxrem_ti_construct%chord_path, end_path)

      sxrem_ti_construct%profile_number = profile_number

      CALL profiler_set_stop_time('sxrem_ti_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref sxrem_class object.
!>
!>  Deallocates memory and uninitializes a @ref sxrem_class object.
!>
!>  @param[inout] this A @ref sxrem_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE sxrem_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (sxrem_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%chord_path)) THEN
         CALL path_destruct(this%chord_path)
         this%chord_path => null()
      END IF

      this%profile_number = 0

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref sxrem_emiss_class object.
!>
!>  Deallocates memory and uninitializes a @ref sxrem_emiss_class object.
!>
!>  @param[inout] this A @ref sxrem_emiss_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE sxrem_emiss_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (sxrem_emiss_class), INTENT(inout) :: this

!  Start of executable code
      this%geo = 0.0

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled emiss signal.
!>
!>  Calculates the soft x-ray emissivity signal by integrating along the chord
!>  path. The emissivity is provided by the @ref sxr_function.
!>
!>  @param[inout] this       A @ref sxrem_emiss_class instance.
!>  @param[in]    a_model    A @ref model instance.
!>  @param[out]   sigma      The modeled sigma.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_emiss_get_modeled_signal(this, a_model, sigma,            &
     &                                        last_value)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: sxrem_emiss_get_modeled_signal
      CLASS (sxrem_emiss_class), INTENT(inout) :: this
      TYPE (model_class), POINTER              :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out)  :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)   :: last_value

! local variables
      CHARACTER(len=1), ALLOCATABLE            :: context(:)
      INTEGER                                  :: context_length
      TYPE (sxrem_context)                     :: sxr_context
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sigma = 0.0

      IF (BTEST(a_model%state_flags, model_state_vmec_flag)     .or.           &
     &    BTEST(a_model%state_flags, model_state_siesta_flag)   .or.           &
     &    BTEST(a_model%state_flags, model_state_shift_flag)    .or.           &
     &    BTEST(a_model%state_flags, model_state_sxrem_flag +                  &
     &                               (this%profile_number - 1)) .or.           &
     &    BTEST(a_model%state_flags, model_state_signal_flag)) THEN

!  The relevant data for the soft x-ray context.
         sxr_context%profile_number = this%profile_number
         sxr_context%model => a_model

!  Cast data to a context. This is the equivalent to casting to a void pointer
!  in C.
         context_length = SIZE(TRANSFER(sxr_context, context))
         ALLOCATE(context(context_length))
         context = TRANSFER(sxr_context, context)

         sxrem_emiss_get_modeled_signal = 0.0
         sxrem_emiss_get_modeled_signal(1) =                                   &
     &      path_integrate(a_model%int_params, this%chord_path,                &
     &                     sxr_function, context)*this%geo

         DEALLOCATE(context)

         CALL this%scale_and_offset(a_model,                                   &
     &                              sxrem_emiss_get_modeled_signal(1))
      ELSE
         sxrem_emiss_get_modeled_signal = last_value
      END IF

      CALL profiler_set_stop_time('sxrem_emiss_get_modeled_signal',            &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled ion temperature signal.
!>
!>  Calculates the soft x-ray emissivity/ion temperature signal by integrating
!>  along the chord path. The emissivity is provided by the @ref ti_function.
!>
!>  @param[inout] this       A @ref sxrem_ti_class instance.
!>  @param[in]    a_model    A @ref model instance.
!>  @param[out]   sigma      The modeled sigma.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_ti_get_modeled_signal(this, a_model, sigma,               &
     &                                     last_value)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: sxrem_ti_get_modeled_signal
      CLASS (sxrem_ti_class), INTENT(inout)   :: this
      TYPE (model_class), POINTER             :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out) :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)  :: last_value

! local variables
      CHARACTER(len=1), ALLOCATABLE           :: context(:)
      INTEGER                                 :: context_length
      TYPE (sxrem_context)                    :: sxr_context
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sigma = 0.0

      IF (BTEST(a_model%state_flags, model_state_vmec_flag)     .or.           &
     &    BTEST(a_model%state_flags, model_state_siesta_flag)   .or.           &
     &    BTEST(a_model%state_flags, model_state_shift_flag)    .or.           &
     &    BTEST(a_model%state_flags, model_state_ti_flag)       .or.           &
     &    BTEST(a_model%state_flags, model_state_sxrem_flag +                  &
     &                               (this%profile_number - 1)) .or.           &
     &    BTEST(a_model%state_flags, model_state_signal_flag)) THEN

!  The relevant data for the soft x-ray context.
         sxr_context%profile_number = this%profile_number
         sxr_context%model => a_model

!  Cast model into a data to a context. This is the equivalent to casting to a
!  void pointer in C.
         context_length = SIZE(TRANSFER(sxr_context, context))
         ALLOCATE(context(context_length))
         context = TRANSFER(sxr_context, context)

         sxrem_ti_get_modeled_signal(1) =                                      &
     &      path_integrate(a_model%int_params, this%chord_path,                &
     &                     ti_function, context)

         DEALLOCATE(context)

         CALL this%scale_and_offset(a_model,                                   &
     &                              sxrem_ti_get_modeled_signal(1))
      ELSE
         sxrem_ti_get_modeled_signal = last_value
      END IF

      CALL profiler_set_stop_time('sxrem_ti_get_modeled_signal',               &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the sxrem emiss type.
!>
!>  Returns a description of the sxrem type for use when writting output files.
!>
!>  @param[in] this A @ref sxrem_emiss_class instance.
!>  @returns A string describing the sxrem type.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_emiss_get_type(this)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: sxrem_emiss_get_type
      CLASS (sxrem_emiss_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (sxrem_emiss_get_type, 1000) this%profile_number

      CALL profiler_set_stop_time('sxrem_emiss_get_type', start_time)

1000  FORMAT('sxrch(',i2,')')

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the sxrem emiss type.
!>
!>  Returns a description of the sxrem type for use when writting output files.
!>
!>  @param[in] this A @ref sxrem_ti_class instance.
!>  @returns A string describing the sxrem type.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_ti_get_type(this)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length)   :: sxrem_ti_get_type
      CLASS (sxrem_ti_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (sxrem_ti_get_type, 1000) this%profile_number

      CALL profiler_set_stop_time('sxrem_ti_get_type', start_time)

1000  FORMAT('sxrch_ti(',i2,')')

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a sxrem emiss signal and a
!>  position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  Integrates the @ref gp_emiss_function_i function.
!>
!>  @param[in] this    A @ref sxrem_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] i       Index of the position for the kernel.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the position and the signal.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_emiss_get_gp_i(this, a_model, i, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                          :: sxrem_emiss_get_gp_i
      CLASS (sxrem_emiss_class), INTENT(in) :: this
      TYPE (model_class), POINTER           :: a_model
      INTEGER, INTENT(in)                   :: i
      INTEGER, INTENT(in)                   :: flags

!  local variables
      REAL (rprec)                          :: start_time
      CHARACTER(len=1), ALLOCATABLE         :: context(:)
      INTEGER                               :: context_length
      TYPE (sxrem_gp_context_i)             :: gp_context

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context%profile_number = this%profile_number
      gp_context%model => a_model
      gp_context%i = i
      gp_context%flags = flags

!  Cast data to a context. This is the equivalent to casting to a void pointer
!  in C.
      context_length = SIZE(TRANSFER(gp_context, context))
      ALLOCATE(context(context_length))
      context = TRANSFER(gp_context, context)

      sxrem_emiss_get_gp_i = path_integrate(a_model%int_params,                &
     &                                      this%chord_path,                   &
     &                                      gp_emiss_function_i,               &
     &                                      context)*this%geo

      DEALLOCATE(context)

      CALL this%scale_and_offset(a_model, sxrem_emiss_get_gp_i)

      CALL profiler_set_stop_time('sxrem_emiss_get_gp_i', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a sxrem ti signal and a
!>  position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  Integrates the @ref gp_ti_function_i function.
!>
!>  @param[in] this    A @ref sxrem_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] i       Index of the position for the kernel.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the position and the signal.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_ti_get_gp_i(this, a_model, i, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                       :: sxrem_ti_get_gp_i
      CLASS (sxrem_ti_class), INTENT(in) :: this
      TYPE (model_class), POINTER        :: a_model
      INTEGER, INTENT(in)                :: i
      INTEGER, INTENT(in)                :: flags

!  local variables
      REAL (rprec)                       :: start_time
      CHARACTER(len=1), ALLOCATABLE      :: context(:)
      INTEGER                            :: context_length
      TYPE (sxrem_gp_context_i)          :: gp_context

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context%profile_number = this%profile_number
      gp_context%model => a_model
      gp_context%i = i
      gp_context%flags = flags

!  Cast data to a context. This is the equivalent to casting to a void pointer
!  in C.
      context_length = SIZE(TRANSFER(gp_context, context))
      ALLOCATE(context(context_length))
      context = TRANSFER(gp_context, context)

      sxrem_ti_get_gp_i = path_integrate(a_model%int_params,                   &
     &                                   this%chord_path,                      &
     &                                   gp_ti_function_i, context)

      DEALLOCATE(context)

      CALL this%scale_and_offset(a_model, sxrem_ti_get_gp_i)

      CALL profiler_set_stop_time('sxrem_ti_get_gp_i', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a sxrem emiss signal and a
!>  signal.
!>
!>  Calculates the guassian process kernel between the signal and a signal. This
!>  is the first signal.
!>
!>  @param[in] this    A @ref sxrem_emiss_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] signal  A @ref signal_class instance for the second signal.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_emiss_get_gp_s(this, a_model, signal, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                          :: sxrem_emiss_get_gp_s
      CLASS (sxrem_emiss_class), INTENT(in) :: this
      TYPE (model_class), POINTER           :: a_model
      CLASS (signal_class), POINTER         :: signal
      INTEGER, INTENT(in)                   :: flags

!  local variables
      REAL (rprec)                          :: start_time
      CHARACTER(len=1), ALLOCATABLE         :: context(:)
      INTEGER                               :: context_length
      TYPE (sxrem_gp_context_s)             :: gp_context

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context%profile_number = this%profile_number
      gp_context%model => a_model
      gp_context%signal => signal
      gp_context%flags = flags

!  Cast data to a context. This is the equivalent to casting to a void pointer
!  in C.
      context_length = SIZE(TRANSFER(gp_context, context))
      ALLOCATE(context(context_length))
      context = TRANSFER(gp_context, context)

      sxrem_emiss_get_gp_s = path_integrate(a_model%int_params,                &
     &                                      this%chord_path,                   &
     &                                      gp_emiss_function_s,               &
     &                                      context)*this%geo

      DEALLOCATE(context)

      CALL this%scale_and_offset(a_model, sxrem_emiss_get_gp_s)

      CALL profiler_set_stop_time('sxrem_emiss_get_gp_s', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a sxrem ti signal and a signal.
!>
!>  Calculates the guassian process kernel between the signal and a signal. This
!>  is the first signal.
!>
!>  @param[in] this    A @ref sxrem_emiss_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] signal  A @ref signal_class instance for the second signal.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_ti_get_gp_s(this, a_model, signal, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                       :: sxrem_ti_get_gp_s
      CLASS (sxrem_ti_class), INTENT(in) :: this
      TYPE (model_class), POINTER        :: a_model
      CLASS (signal_class), POINTER      :: signal
      INTEGER, INTENT(in)                :: flags

!  local variables
      REAL (rprec)                       :: start_time
      CHARACTER(len=1), ALLOCATABLE      :: context(:)
      INTEGER                            :: context_length
      TYPE (sxrem_gp_context_s)          :: gp_context

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context%profile_number = this%profile_number
      gp_context%model => a_model
      gp_context%signal => signal
      gp_context%flags = flags

!  Cast data to a context. This is the equivalent to casting to a void pointer
!  in C.
      context_length = SIZE(TRANSFER(gp_context, context))
      ALLOCATE(context(context_length))
      context = TRANSFER(gp_context, context)

      sxrem_ti_get_gp_s = path_integrate(a_model%int_params,                   &
     &                                   this%chord_path,                      &
     &                                   gp_ti_function_s, context)

      DEALLOCATE(context)

      CALL this%scale_and_offset(a_model, sxrem_ti_get_gp_s)

      CALL profiler_set_stop_time('sxrem_ti_get_gp_s', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a sxrem emiss signal and a
!>  cartesian position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  This is the second signal.
!>
!>  @param[in] this    A @ref sxrem_emiss_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] x_cart  The cartesian position of to get the kernel at.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_emiss_get_gp_x(this, a_model, x_cart, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: sxrem_emiss_get_gp_x
      CLASS (sxrem_emiss_class), INTENT(in)  :: this
      TYPE (model_class), POINTER            :: a_model
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: flags

!  local variables
      REAL (rprec)                           :: start_time
      CHARACTER (len=1), ALLOCATABLE         :: context(:)
      INTEGER                                :: context_length
      TYPE (sxrem_gp_context_x)              :: gp_context

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context%profile_number = this%profile_number
      gp_context%model => a_model
      gp_context%xcart = x_cart

!  Cast data to a context. This is the equivalent to casting to a void pointer
!  in C.
      context_length = SIZE(TRANSFER(gp_context, context))
      ALLOCATE(context(context_length))
      context = TRANSFER(gp_context, context)

      sxrem_emiss_get_gp_x = path_integrate(a_model%int_params,                &
     &                                      this%chord_path,                   &
     &                                      gp_emiss_function_x,               &
     &                                      context)*this%geo

      CALL this%scale_and_offset(a_model, sxrem_emiss_get_gp_x)

      CALL profiler_set_stop_time('sxrem_emiss_get_gp_x', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a sxrem ti signal and a
!>  cartesian position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  This is the second signal.
!>
!>  @param[in] this    A @ref sxrem_emiss_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @param[in] x_cart  The cartesian position of to get the kernel at.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_ti_get_gp_x(this, a_model, x_cart, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: sxrem_ti_get_gp_x
      CLASS (sxrem_ti_class), INTENT(in)     :: this
      TYPE (model_class), POINTER            :: a_model
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: flags

!  local variables
      REAL (rprec)                           :: start_time
      CHARACTER (len=1), ALLOCATABLE         :: context(:)
      INTEGER                                :: context_length
      TYPE (sxrem_gp_context_x)              :: gp_context

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context%profile_number = this%profile_number
      gp_context%model => a_model
      gp_context%xcart = x_cart
      gp_context%flags = flags

!  Cast data to a context. This is the equivalent to casting to a void pointer
!  in C.
      context_length = SIZE(TRANSFER(gp_context, context))
      ALLOCATE(context(context_length))
      context = TRANSFER(gp_context, context)

      sxrem_ti_get_gp_x = path_integrate(a_model%int_params,                   &
     &                                   this%chord_path,                      &
     &                                   gp_ti_function_x, context)

      CALL this%scale_and_offset(a_model, sxrem_ti_get_gp_x)

      CALL profiler_set_stop_time('sxrem_ti_get_gp_x', start_time)

      END FUNCTION

!*******************************************************************************
!  PRIVATE
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Soft x-ray callback function.
!>
!>  Returns the value of the soft x-ray emissivity times the change in path
!>  length. This function is passed to @ref integration_path::path_integrate to
!>  act as a callback. The soft x-ray emissivity is provided by
!>  @ref model::model_get_sxrem.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref sxrem_context for the model.
!>  @param[in] xcart   The integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration.
!>  @param[in] dx      A scalar change in path.
!>  @returns The sxrem(x)*dl at point x.
!-------------------------------------------------------------------------------
      FUNCTION sxr_function(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: sxr_function
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx

! local variables
      TYPE (sxrem_context)                   :: sxr_context
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sxr_context = TRANSFER(context, sxr_context)
      sxr_function = model_get_sxrem(sxr_context%model, xcart,                 &
     &                               sxr_context%profile_number)*dx

      CALL profiler_set_stop_time('sxr_function', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Ion temperature callback function.
!>
!>  Returns the value of the ion temperature times the soft x-ray emissivity
!>  times the change in path length. This function is passed to
!>  @ref integration_path::path_integrate to act as a callback. The soft x-ray
!>  emissivity is provided by @ref model::model_get_sxrem and the ion
!>  temperature is provided by @ref model::model_get_ti.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref sxrem_context for the model.
!>  @param[in] xcart   The integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration.
!>  @param[in] dx      A scalar change in path.
!>  @returns The te(x)*dl at point x.
!-------------------------------------------------------------------------------
      FUNCTION ti_function(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: ti_function
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx

! local variables
      TYPE (sxrem_context)                   :: sxr_context
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sxr_context = TRANSFER(context, sxr_context)
      ti_function = model_get_sxrem(sxr_context%model, xcart,                  &
     &                              sxr_context%profile_number)                &
     &            * model_get_ti(sxr_context%model, xcart)*dx

      CALL profiler_set_stop_time('ti_function', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Soft x-ray emission gaussian process callback function for signal
!>  point kernel evaluation.
!>
!>  Returns the value of the soft x-ray emission guassian process kernel times
!>  the change in path length. This function is passed to
!>  @ref integration_path::path_integrate to act as a callback. The soft x-ray
!>  kernel is provided by @ref model::model_get_gp_sxrem.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref sxrem_gp_context_i for the model.
!>  @param[in] xcart   The integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration.
!>  @param[in] dx      A scalar change in path.
!>  @returns The k(x, i)*dl at point x.
!-------------------------------------------------------------------------------
      FUNCTION gp_emiss_function_i(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: gp_emiss_function_i
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx

! local variables
      TYPE (sxrem_gp_context_i)              :: gp_context
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context = TRANSFER(context, gp_context)
      gp_emiss_function_i =                                                    &
     &   model_get_gp_sxrem(gp_context%model, xcart, gp_context%i,                   &
     &                      gp_context%profile_number)*dx

      CALL profiler_set_stop_time('gp_emiss_function_i', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief XICS gaussian process callback function for signal point kernel
!>  evaluation.
!>
!>  Returns the value of the soft x-ray guassian process kernel times the change
!>  in path length. This function is passed to
!>  @ref integration_path::path_integrate to act as a callback. The soft x-ray
!>  kernel is provided by @ref model::model_get_gp_sxrem. The ti kernel is
!>  provided by @ref model::model_get_gp_ti.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref sxrem_gp_context_i for the model.
!>  @param[in] xcart   The integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration.
!>  @param[in] dx      A scalar change in path.
!>  @returns The k(x, i)*f(x)*dl at point x. Where f(x) varies between the sxrem
!>           and ti profiles depending on the flags set.
!-------------------------------------------------------------------------------
      FUNCTION gp_ti_function_i(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: gp_ti_function_i
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx

! local variables
      TYPE (sxrem_gp_context_i)              :: gp_context
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context = TRANSFER(context, gp_context)
      IF (BTEST(gp_context%flags, model_state_sxrem_flag +                     &
     &                            (gp_context%profile_number - 1))) THEN
         gp_ti_function_i =                                                    &
     &      model_get_gp_sxrem(gp_context%model, xcart, gp_context%i,          &
     &                         gp_context%profile_number) *                    &
     &      model_get_ti(gp_context%model, xcart)
      ELSE IF (BTEST(gp_context%flags, model_state_ti_flag)) THEN
         gp_ti_function_i = model_get_gp_ti(gp_context%model, xcart,           &
     &                                      gp_context%i)                      &
     &                    * model_get_sxrem(gp_context%model, xcart,           &
     &                                      gp_context%profile_number)
      ELSE
         gp_ti_function_i = 0.0
      END IF

      gp_ti_function_i = gp_ti_function_i*dx

      CALL profiler_set_stop_time('gp_ti_function_i', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Soft x-ray gaussian process callback function for emiss signal signal
!>  kernel evaluation.
!>
!>  Returns the value of the soft x-ray emiss guassian process kernel times the
!>  change in path length.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref sxrem_gp_context_i for the model.
!>  @param[in] xcart   The integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration.
!>  @param[in] dx      A scalar change in path.
!>  @returns The k(x, s)*f(x)*dl at point x. Where f(x) varies between the sxrem
!>           and ti profiles depending on the flags set.
!-------------------------------------------------------------------------------
      FUNCTION gp_emiss_function_s(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: gp_emiss_function_s
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx

! local variables
      TYPE (sxrem_gp_context_s)              :: gp_context
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context = TRANSFER(context, gp_context)

      gp_emiss_function_s = gp_context%signal%get_gp(gp_context%model,         &
     &                                               xcart,                    &
     &                                               gp_context%flags)         &
     &                    * dx

      CALL profiler_set_stop_time('gp_function_s', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Soft x-ray gaussian process callback function for ti signal signal
!>  kernel evaluation.
!>
!>  Returns the value of the soft x-ray emiss guassian process kernel times the
!>  change in path length. The kernel will vary depending on the flags.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref sxrem_gp_context_i for the model.
!>  @param[in] xcart   The integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration.
!>  @param[in] dx      A scalar change in path.
!>  @returns The k(x, i)*dl at point x.
!-------------------------------------------------------------------------------
      FUNCTION gp_ti_function_s(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: gp_ti_function_s
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx

! local variables
      TYPE (sxrem_gp_context_s)              :: gp_context
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      gp_context = TRANSFER(context, gp_context)

      IF (BTEST(gp_context%flags, model_state_sxrem_flag +                     &
     &                            (gp_context%profile_number - 1))) THEN
         gp_ti_function_s = gp_context%signal%get_gp(gp_context%model,         &
     &                                               xcart,                    &
     &                                               gp_context%flags)         &
     &                    * model_get_ti(gp_context%model, xcart)
      ELSE IF (BTEST(gp_context%flags, model_state_ti_flag)) THEN
         gp_ti_function_s = gp_context%signal%get_gp(gp_context%model,         &
     &                                               xcart,                    &
     &                                               gp_context%flags)         &
     &                    * model_get_sxrem(gp_context%model, xcart,           &
     &                                      gp_context%profile_number)
      ELSE
         gp_ti_function_s = 0.0
      END IF

      gp_ti_function_s = gp_ti_function_s*dx

      CALL profiler_set_stop_time('gp_function_s', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Soft x-ray gaussian process callback function for kernel evaluation
!>  for an emiss position and position.
!>
!>  Calculates the guassian process kernel between the emiss position and a
!>  position.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref sxrem_gp_context_x for the model.
!>  @param[in] xcart   The integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration.
!>  @param[in] dx      A scalar change in path.
!>  @returns The k(x, y)*dl at point y.
!-------------------------------------------------------------------------------
      FUNCTION gp_emiss_function_x(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: gp_emiss_function_x
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx

! local variables
      TYPE (sxrem_gp_context_x)              :: gp_context
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  This is the second signal so put xcart in the second position.
      gp_context = TRANSFER(context, gp_context)
      gp_emiss_function_x =                                                    &
     &   model_get_gp_sxrem(gp_context%model, xcart, gp_context%xcart,         &
     &                      gp_context%profile_number)*dx

      CALL profiler_set_stop_time('gp_emiss_function_x', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Soft x-ray gaussian process callback function for kernel evaluation
!>  for an ti position and a position.
!>
!>  Calculates the guassian process kernel between the ti position and a
!>  position.
!>
!>  @see integration_path
!>
!>  @param[in] context A @ref sxrem_gp_context_x for the model.
!>  @param[in] xcart   The integration point.
!>  @param[in] dxcart  A vector change in path. Not used in this function.
!>  @param[in] length  Length along the integration.
!>  @param[in] dx      A scalar change in path.
!>  @returns The k(x, y)*f(x)*dl at point y. f(x) varies depending on the value
!>  of the flags.
!-------------------------------------------------------------------------------
      FUNCTION gp_ti_function_x(context, xcart, dxcart, length, dx)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: gp_ti_function_x
      CHARACTER (len=1), INTENT(in)          :: context(:)
      REAL (rprec), DIMENSION(3), INTENT(in) :: xcart
      REAL (rprec), DIMENSION(3), INTENT(in) :: dxcart
      REAL (rprec), INTENT(in)               :: length
      REAL (rprec), INTENT(in)               :: dx

! local variables
      TYPE (sxrem_gp_context_x)              :: gp_context
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  This is the second signal so put xcart in the second position.
      gp_context = TRANSFER(context, gp_context)
      IF (BTEST(gp_context%flags, model_state_sxrem_flag +                     &
     &                            (gp_context%profile_number - 1))) THEN
         gp_ti_function_x =                                                    &
     &      model_get_gp_sxrem(gp_context%model, xcart,                        &
     &                         gp_context%xcart,                               &
     &                         gp_context%profile_number) *                    &
     &      model_get_ti(gp_context%model, xcart)
      ELSE IF (BTEST(gp_context%flags, model_state_ti_flag)) THEN
         gp_ti_function_x = model_get_gp_ti(gp_context%model, xcart,           &
     &                                      gp_context%xcart)                  &
     &                    * model_get_sxrem(gp_context%model, xcart,          &
     &                                      gp_context%profile_number)
      ELSE
         gp_ti_function_x = 0.0
      END IF

      gp_ti_function_x = gp_ti_function_x*dx

      CALL profiler_set_stop_time('gp_ti_function_x', start_time)

      END FUNCTION

      END MODULE

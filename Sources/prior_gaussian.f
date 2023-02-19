!*******************************************************************************
!>  @file prior_gaussian.f
!>  @brief Contains module prior_gaussian
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref prior_gaussian_class. This class
!>  implements priors of the type
!>
!>  P(B) = Exp(-((B - mu)/sigma)^2)
!>
!>  @par Super Class:
!>  @ref prior
!*******************************************************************************

      MODULE prior_gaussian

      USE data_parameters
      USE model
      USE stel_kinds, only : rprec, dp
      USE signal

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) prior_gaussian base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a prior_guassian signal.
!>  @par Super Class:
!>  @ref signal
!>  @par Sub Classes:
!>  @ref prior_guassian
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(signal_class) :: prior_gaussian_class
!>  Parameter id to check the guassian against.
         INTEGER               :: param_id = data_no_id
!>  Indices of the parameter.
         INTEGER, DIMENSION(2) :: indices = 0
      CONTAINS
         PROCEDURE             ::                                              &
     &      get_modeled_signal_last => prior_gaussian_get_modeled_signal
         PROCEDURE             ::                                              &
     &      get_type => prior_gaussian_get_type
         PROCEDURE             :: get_gp_i => prior_gaussian_get_gp_i
         PROCEDURE             :: get_gp_s => prior_gaussian_get_gp_s
         PROCEDURE             :: get_gp_x => prior_gaussian_get_gp_x
         PROCEDURE             ::                                              &
     &      write_auxiliary => prior_gaussian_write_auxiliary
         FINAL                 :: prior_gaussian_destruct
      END TYPE prior_gaussian_class

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for thomson te constructor.
!-------------------------------------------------------------------------------
      INTERFACE prior_gaussian_class
         MODULE PROCEDURE prior_gaussian_construct
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref prior_gaussian_class object.
!>
!>  Allocates memory and initializes a @ref prior_gaussian_class object.
!>
!>  @param[in] a_model    An instance of a @ref model object.
!>  @param[in] param_name The name the parameter.
!>  @param[in] indices    Indicies of the parameter.
!>  @returns A pointer to a constructed @ref prior_gaussian_class object.
!-------------------------------------------------------------------------------
      FUNCTION prior_gaussian_construct(a_model, param_name, indices)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (prior_gaussian_class), POINTER ::                                 &
     &   prior_gaussian_construct
      CLASS (model_class), INTENT(in)       :: a_model
      CHARACTER (len=*), INTENT(in)         :: param_name
      INTEGER, DIMENSION(2), INTENT(in)     :: indices

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(prior_gaussian_construct)

      prior_gaussian_construct%param_id =                                      &
     &   a_model%get_param_id(TRIM(param_name))
      prior_gaussian_construct%indices = indices

      CALL profiler_set_stop_time('prior_gaussian_construct',                  &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref prior_gaussian_class object.
!>
!>  Deallocates memory and uninitializes a @ref prior_gaussian_class object.
!>
!>  @param[inout] this A @ref prior_gaussian_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE prior_gaussian_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (prior_gaussian_class), INTENT(inout) :: this

!  Start of executable code
      this%param_id = data_no_id
      this%indices = 0

      END SUBROUTINE

!*******************************************************************************
!  GETTERS SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled signal.
!>
!>  The modeled signal is just the value of the parameter.
!>
!>  @param[in]  this       A @ref prior_gaussian_class instance.
!>  @param[in]  a_model    A @ref model instance.
!>  @param[out] sigma      The modeled sigma.
!>  @param[in]  last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION prior_gaussian_get_modeled_signal(this, a_model, sigma,         &
     &                                           last_value)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: prior_gaussian_get_modeled_signal
      CLASS (prior_gaussian_class), INTENT(inout) :: this
      CLASS (model_class), POINTER                :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out)     :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)      :: last_value

!  local variables
      REAL (rprec)                                :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      prior_gaussian_get_modeled_signal = 0.0
      prior_gaussian_get_modeled_signal(1) =                                   &
     &   a_model%get_param_value(this%param_id, this%indices(1),               &
     &                           this%indices(2))                              &
      sigma = 0.0

      CALL this%scale_and_offset(a_model,                                      &
     &                           prior_gaussian_get_modeled_signal(1))

      CALL profiler_set_stop_time('prior_gaussian_get_modeled_signal',         &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the prior type.
!>
!>  @param[in] this A @ref prior_gaussian_class instance.
!>  @returns A string describing the prior type.
!-------------------------------------------------------------------------------
      FUNCTION prior_gaussian_get_type(this)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: prior_gaussian_get_type
      CLASS (prior_gaussian_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      prior_gaussian_get_type = 'guassian'

      CALL profiler_set_stop_time('prior_gaussian_get_type',                   &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a prior signal and a position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  Temperature kernels are provided by @ref model::model_get_gp_te. Denisty
!>  kernels are provided by @ref model::model_get_gp_ne. Soft X-ray Emission
!>  kernels are provided by @ref model::model_get_gp_sxrem.
!>
!>  @param[in] this         A @ref prior_gaussian_class instance.
!>  @param[in] a_model      A @ref model instance.
!>  @param[in] i            Index of the position for the kernel.
!>  @param[in] flags        State flags to send to the kernel.
!>  @returns Kernel value for the position and the signal.
!-------------------------------------------------------------------------------
      FUNCTION prior_gaussian_get_gp_i(this, a_model, i, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: prior_gaussian_get_gp_i
      CLASS (prior_gaussian_class), INTENT(in) :: this
      CLASS (model_class), POINTER             :: a_model
      INTEGER, INTENT(in)                      :: i
      INTEGER, INTENT(in)                      :: flags

!  local variables
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (BTEST(flags, model_state_ne_flag)) THEN
         prior_gaussian_get_gp_i =                                             &
     &      a_model%get_gp_ne(this%indices(1), i)
      ELSE IF (BTEST(flags, model_state_te_flag)) THEN
         prior_gaussian_get_gp_i =                                             &
     &      a_model%get_gp_te(this%indices(1), i)
      ELSE IF (BTEST(flags, model_state_ti_flag)) THEN
         prior_gaussian_get_gp_i =                                             &
     &      a_model%get_gp_ti(this%indices(1), i)
      ELSE IF (BTEST(flags, model_state_sxrem_flag +                           &
     &                      (this%indices(1) - 1))) THEN
         prior_gaussian_get_gp_i =                                             &
     &      a_model%get_gp_sxrem(this%indices(2), i, this%indices(1))
      ELSE
         prior_gaussian_get_gp_i = 0.0
      END IF

      CALL this%scale_and_offset(a_model, prior_gaussian_get_gp_i)

      CALL profiler_set_stop_time('prior_gaussian_get_gp_i', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a prior signal and a signal.
!>
!>  Calculates the guassian process kernel between the signal and a signal.
!>  Calls back to the @ref signal module to call the other signal. This does not
!>  support pressure measurements.
!>
!>  @param[in] this    A @ref prior_gaussian_class instance.
!>  @param[in] a_model A @ref model_class instance.
!>  @param[in] signal  A @ref signal_class instance for the second signal.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION prior_gaussian_get_gp_s(this, a_model, signal, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: prior_gaussian_get_gp_s
      CLASS (prior_gaussian_class), INTENT(in) :: this
      CLASS (model_class), POINTER             :: a_model
      CLASS (signal_class), POINTER            :: signal
      INTEGER, INTENT(in)                      :: flags

!  local variables
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (BTEST(flags, model_state_ne_flag) .or.                               &
     &    BTEST(flags, model_state_te_flag) .or.                               &
     &    BTEST(flags, model_state_ti_flag)) THEN
         prior_gaussian_get_gp_s =                                             &
     &      signal%get_gp(a_model, this%indices(1), flags)
      ELSE IF (BTEST(flags, model_state_sxrem_flag +                           &
     &                      (this%indices(1) - 1))) THEN
         prior_gaussian_get_gp_s =                                             &
     &      signal%get_gp(a_model, this%indices(2), flags)
      ELSE
         prior_gaussian_get_gp_s = 0.0
      END IF

      CALL this%scale_and_offset(a_model, prior_gaussian_get_gp_s)

      CALL profiler_set_stop_time('prior_gaussian_get_gp_s', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a prior signal and a cartesian
!>  position.
!>
!>  Calculates the guassian process kernel between the signal and the position.
!>  Temperature kernels are provided by @ref model::model_get_gp_te. Denisty
!>  kernels are provided by @ref model::model_get_gp_ne. Soft X-ray Emission
!>  kernels are provided by @ref model::model_get_gp_sxrem. This is the second
!>  signal.
!>
!>  @param[in] this         A @ref prior_gaussian_class instance.
!>  @param[in] a_model      A @ref model instance.
!>  @param[in] x_cart       The cartesian position of to get the kernel at.
!>  @param[in] flags        State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION prior_gaussian_get_gp_x(this, a_model, x_cart, flags)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: prior_gaussian_get_gp_x
      CLASS (prior_gaussian_class), INTENT(in) :: this
      CLASS (model_class), POINTER             :: a_model
      REAL (rprec), DIMENSION(3), INTENT(in)   :: x_cart
      INTEGER, INTENT(in)                      :: flags

!  local variables
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (BTEST(flags, model_state_ne_flag)) THEN
         prior_gaussian_get_gp_x = a_model%get_gp_ne(x_cart,                   &
     &                                               this%indices(1))
      ELSE IF (BTEST(flags, model_state_te_flag)) THEN
         prior_gaussian_get_gp_x = a_model%get_gp_te(x_cart,                   &
     &                                               this%indices(1))
      ELSE IF (BTEST(flags, model_state_ti_flag)) THEN
         prior_gaussian_get_gp_x = a_model%get_gp_ti(x_cart,                   &
     &                                               this%indices(1))
      ELSE IF (BTEST(flags, model_state_sxrem_flag +                           &
     &                      (this%indices(1) - 1))) THEN
         prior_gaussian_get_gp_x = a_model%get_gp_sxrem(x_cart,                &
     &                                                  this%indices(2),       &
     &                                                  this%indices(1))
      ELSE
         prior_gaussian_get_gp_x = 0.0
      END IF

      CALL this%scale_and_offset(a_model, prior_gaussian_get_gp_x)

      CALL profiler_set_stop_time('prior_gaussian_get_gp_x', start_time)

      END FUNCTION

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Write out auxiliary signal information to an output file.
!>
!>  Writes out the s_name and coefficient of the combined signals.
!>
!>  @param[in] this    A @ref prior_gaussian_class instance.
!>  @param[in] iou     A input/output representing the file to write to.
!>  @param[in] index   A index of a signal.
!>  @param[in] a_model The equilibrium model.
!-------------------------------------------------------------------------------
      SUBROUTINE prior_gaussian_write_auxiliary(this, iou, index,              &
     &                                          a_model)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (prior_gaussian_class), INTENT(in) :: this
      INTEGER, INTENT(in)                      :: iou
      INTEGER, INTENT(in)                      :: index
      CLASS (model_class), INTENT(in)          :: a_model

!  local variables
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (iou,*)
      WRITE (iou,1000) index, this%get_type()
      WRITE (iou,1001)
      WRITE (iou,1002) a_model%get_param_name(this%param_id),                  &
     &                 this%indices

      CALL profiler_set_stop_time('prior_gaussian_write_auxiliary',            &
     &                            start_time)

1000  FORMAT('Signal',1x,i4,1x,'is a prior signal, type: ',a)
1001  FORMAT('parameter name',8x,'inx1',2x,'inx2')
1002  FORMAT(a20,2(2x,i4))

      END SUBROUTINE

      END MODULE

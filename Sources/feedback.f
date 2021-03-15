!*******************************************************************************
!>  @file feedback.f
!>  @brief Contains module @ref feedback
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref feedback_class.
!>  @par Super Class:
!>  @ref signal
!>  @par Sub Classes:
!>  Sub Classes: @ref sxrem_ratio
!*******************************************************************************

      MODULE feedback

      USE signal

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) feedback base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a feedback signal.
!>  @par Super Class:
!>  @ref signal
!>  @par Sub Classes:
!>  @ref sxrem_ratio
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(signal_class) :: feedback_class
!>  An instance of a @ref sxrem object.
         CLASS (signal_class), POINTER :: signal => null()
      CONTAINS
         FINAL                         :: feedback_destruct
      END TYPE feedback_class

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the construction of @ref feedback_class types using
!>  @ref feedback_construct_ratio.
!-------------------------------------------------------------------------------
      INTERFACE feedback_class
         MODULE PROCEDURE feedback_construct
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref feedback_class containing a @ref sxrem_ratio object.
!>
!>  Allocates memory and initializes a @ref feedback_class object.
!>
!>  @param[in] ratio_object An instance of a @ref sxrem_ratio subclass.
!>  @returns A pointer to a constructed @ref feedback_class object.
!-------------------------------------------------------------------------------
      FUNCTION feedback_construct(signal)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (feedback_class), POINTER :: feedback_construct_ratio
      CLASS (signal_class), POINTER   :: signal

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(feedback_construct_ratio)

      feedback_construct_ratio%signal => signal

      CALL profiler_set_stop_time('feedback_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref feedback_class object.
!>
!>  Deallocates memory and uninitializes a @ref feedback_class object.
!>
!>  @param[inout] this A @ref feedback_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE feedback_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (feedback_class), POINTER :: this

!  Start of executable code
      IF (ASSOCIATED(this%signal)) THEN
         this%signal => null()
      END IF

      DEALLOCATE(this)

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled signal.
!>
!>  This method is meant to be overwritten by a subclass method. As a result,
!>  returns the get_modeled_signal method of the subclass instance
!>  @ref feedback_class was constructed with.
!>
!>  @see sxrem_ratio::sxrem_ratio_get_modeled_signal
!>
!>  @param[inout] this       A @ref feedback_class instance.
!>  @param[in]    a_model    A @ref model instance.
!>  @param[out]   sigma      The modeled sigma.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION feedback_get_modeled_signal(this, a_model, sigma,               &
     &                                     last_value)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: feedback_get_modeled_signal
      CLASS (feedback_class), INTENT(in)      :: this
      TYPE (model_class), POINTER             :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out) :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)  :: last_value

!  local variables
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE(this%type)

         CASE (feedback_sxrem_ratio_type)
            feedback_get_modeled_signal =                                      &
     &         sxrem_ratio_get_modeled_signal(this%ratio, a_model,             &
     &                                        sigma, last_value,               &
     &                                        scale_factor,                    &
     &                                        offset_factor)

      END SELECT

      CALL profiler_set_stop_time('feedback_get_modeled_signal',               &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Calculates the observed signal.
!>
!>  This method is meant to be overwritten by a subclass method. As a result,
!>  returns the get_observed_signal method of the subclass instance
!>  @ref feedback_class was constructed with. This observed value is actually a
!>  a second model. This is used in the chi^2 minimization for self consistent
!>  feedback of quanities derived from two different models.
!>
!>  @see sxrem_ratio::sxrem_ratio_get_observed_signal
!>
!>  @param[in] this    A @ref feedback_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @returns The observed value.
!-------------------------------------------------------------------------------
      FUNCTION feedback_get_observed_signal(this, a_model)

      IMPLICIT NONE

!  Declare Arguments
      REAL(rprec) :: feedback_get_observed_signal
      TYPE(feedback_class), INTENT(in) :: this
      TYPE (model_class), INTENT(in)   :: a_model

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE(this%type)

         CASE (feedback_sxrem_ratio_type)
            feedback_get_observed_signal =                                     &
     &         sxrem_ratio_get_observed_signal(this%ratio, a_model)

         CASE DEFAULT
            feedback_get_observed_signal = 0.0

      END SELECT

      CALL profiler_set_stop_time('feedback_get_observed_signal',              &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the feedback signal type.
!>
!>  This method is meant to be overwritten by a subclass method. As a result,
!>  returns the get_signal_type method of the subclass instance
!>  @ref feedback_class was constructed with.
!>
!>  @see sxrem_ratio::sxrem_ratio_get_signal_type
!>
!>  @param[in] this A @ref feedback_class instance.
!>  @returns A string describing the feedback type.
!-------------------------------------------------------------------------------
      FUNCTION feedback_get_signal_type(this)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length)  :: feedback_get_signal_type
      TYPE (feedback_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE(this%type)

         CASE (feedback_sxrem_ratio_type)
            feedback_get_signal_type = TRIM('feedback ' //                     &
     &         sxrem_ratio_get_signal_type(this%ratio))

      END SELECT

      CALL profiler_set_stop_time('feedback_get_signal_type',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the model and model sigma array indices.
!>
!>  This method is meant to be overwritten by a subclass method if need be. As a
!>  result, returns the get_header method of the subclass instance
!>  @ref feedback_class was constructed with.
!>
!>  @param[in] this A @ref feedback_class instance.
!>  @returns A string describing the model and model sigma array indices.
!-------------------------------------------------------------------------------
      FUNCTION feedback_get_header(this)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length), DIMENSION(7)                           &
     &   :: feedback_get_header
      TYPE (feedback_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE(this%type)

         CASE DEFAULT
            feedback_get_header(1:3) = 'N/A'
            feedback_get_header(4) = 'model_sig(1)'
            feedback_get_header(5) = 'model_sig(2)'
            feedback_get_header(6) = 'model_sig(3)'
            feedback_get_header(7) = 'model_sig(4)'

      END SELECT

      CALL profiler_set_stop_time('feedback_get_header', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the feedback type.
!>
!>  Returns a string describing the feedback subclass.
!>
!>  @see sxrem_ratio::sxrem_ratio_get_signal_type
!>
!>  @param[in] this A @ref feedback_class instance.
!>  @returns A string describing the feedback subclass.
!-------------------------------------------------------------------------------
      FUNCTION feedback_get_feedback_type(this)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length)  :: feedback_get_feedback_type
      TYPE (feedback_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE(this%type)

         CASE (feedback_sxrem_ratio_type)
            feedback_get_feedback_type =                                       &
     &         sxrem_ratio_get_signal_type(this%ratio)
      END SELECT

      CALL profiler_set_stop_time('feedback_get_feedback_type',                &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Write out auxiliary signal information to an output file.
!>
!>  Writes out the s_name and coefficient of the combined signals.
!>
!>  @param[in] this    A @ref feedback_class instance.
!>  @param[in] iou     A input/output representing the file to write to.
!>  @param[in] index   A index of this signal.
!-------------------------------------------------------------------------------
      SUBROUTINE feedback_write_auxiliary(this, iou, index)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (feedback_class), INTENT(in) :: this
      INTEGER, INTENT(in)               :: iou
      INTEGER, INTENT(in)               :: index

!  local variables
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (iou,*)
      WRITE (iou,1000) index, feedback_get_feedback_type(this)

      SELECT CASE(this%type)

         CASE (feedback_sxrem_ratio_type)
            CALL sxrem_ratio_write_auxiliary(this%ratio, iou)

      END SELECT

      CALL profiler_set_stop_time('feedback_write_auxiliary',                  &
     &                            start_time)

1000  FORMAT('Signal',1x,i4,1x,'is a feedback signal, type: ',a)

      END SUBROUTINE

      END MODULE

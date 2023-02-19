!*******************************************************************************
!>  @file sxrem_ratio.f
!>  @brief Contains module sxrem_ratio
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines a feedback signal based on the temperature based on the ration of
!>  the soft x-ray emissivity profile. This signal feeds back the temperature
!>  profile at a specified positon.
!>
!>  @par Super Class:
!>  @ref feedback
!*******************************************************************************

      MODULE sxrem_ratio

      USE stel_kinds, only: rprec, dp
      USE model
      USE signal

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) sxrem_ratio base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a sxrem_ratio signal.
!>  @par Super Class:
!>  @ref feedback
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(signal_class) :: sxrem_ratio_class
!>  Radial position to feedback the soft x-ray emission from.
         REAL (rprec), DIMENSION(3) :: x_cart
!>  Indicies of soft x-ray emissivity profiles to take the ratios of.
         INTEGER, DIMENSION(2)      :: indices
      CONTAINS
         PROCEDURE                  ::                                         &
     &      get_modeled_signal_last => sxrem_ratio_get_modeled_signal
         PROCEDURE                  ::                                         &
     &      get_observed_signal => sxrem_ratio_get_observed_signal
         PROCEDURE                  :: get_type => sxrem_ratio_get_type
         PROCEDURE                  ::                                         &
     &      write_auxiliary => sxrem_ratio_write_auxiliary
         FINAL                      :: sxrem_ratio_destruct
      END TYPE sxrem_ratio_class

!-------------------------------------------------------------------------------
!>  Interface for Soft X-ray ratio constructor.
!-------------------------------------------------------------------------------
      INTERFACE sxrem_ratio_class
         MODULE PROCEDURE sxrem_ratio_construct
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref sxrem_ratio_class object.
!>
!>  Allocates memory and initializes a @ref sxrem_ratio_class object.
!>
!>  @param[in] x_cart  Radial position to feedback the soft x-ray emission from.
!>  @param[in] indices Indicies of the two cords.
!>  @returns A pointer to a constructed @ref sxrem_ratio_class object.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_ratio_construct(x_cart, indices)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (sxrem_ratio_class), POINTER     :: sxrem_ratio_construct
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, DIMENSION(2), INTENT(in)      :: indices

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(sxrem_ratio_construct)

      sxrem_ratio_construct%x_cart = x_cart
      sxrem_ratio_construct%indices = indices

      CALL profiler_set_stop_time('sxrem_ratio_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref sxrem_ratio_class object.
!>
!>  Deallocates memory and uninitializes a @ref sxrem_ratio_class object.
!>
!>  @param[inout] this A @ref sxrem_ratio_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE sxrem_ratio_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (sxrem_ratio_class), INTENT(inout) :: this

!  Start of executable code
      this%x_cart = 0.0
      this%indices = 0

      END SUBROUTINE

!*******************************************************************************
!  GETTERS SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled signal.
!>
!>  The modeled signal is the ratio of the soft x-ray emissivity profiles.
!>
!>  @param[inout] this       A @ref sxrem_ratio_class instance.
!>  @param[in]    a_model    A @ref model instance.
!>  @param[out]   sigma      The modeled sigma.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_ratio_get_modeled_signal(this, a_model, sigma,            &
     &                                        last_value)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: sxrem_ratio_get_modeled_signal
      CLASS (sxrem_ratio_class), INTENT(inout) :: this
      CLASS (model_class), POINTER             :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out)  :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)   :: last_value

!  local variables
      REAL (rprec)                             :: emissivity
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sigma = 0.0

      IF (BTEST(a_model%state_flags, model_state_vmec_flag)   .or.             &
     &    BTEST(a_model%state_flags, model_state_siesta_flag) .or.             &
     &    BTEST(a_model%state_flags, model_state_shift_flag)  .or.             &
     &    BTEST(a_model%state_flags, model_state_sxrem_flag +                  &
     &                               (this%indices(1) - 1))   .or.             &
     &    BTEST(a_model%state_flags, model_state_sxrem_flag +                  &
     &                               (this%indices(2) - 1))   .or.             &
     &    BTEST(a_model%state_flags, model_state_signal_flag)) THEN

         emissivity = a_model%get_sxrem_cart(this%x_cart,                      &
     &                                       this%indices(2))

         IF (emissivity .eq. 0.0) THEN
            sxrem_ratio_get_modeled_signal(1) = 1.0
         ELSE
            sxrem_ratio_get_modeled_signal(1) =                                &
     &         a_model%get_sxrem_cart(this%x_cart,                             &
     &                                this%indices(1))/emissivity
         END IF

         CALL this%scale_and_offset(a_model,                                   &
     &                              sxrem_ratio_get_modeled_signal(1))
      ELSE
         sxrem_ratio_get_modeled_signal = last_value
      END IF

      CALL profiler_set_stop_time('sxrem_ratio_get_modeled_signal',            &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Calculates the observed signal.
!>
!>  The observed signal is the ratio computed as the ratio as a function of the
!>  temperature.
!>
!>  @param[in] this    A @ref sxrem_ratio_class instance.
!>  @param[in] a_model A @ref model instance.
!>  @returns The observed value.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_ratio_get_observed_signal(this, a_model)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: sxrem_ratio_get_observed_signal
      CLASS (sxrem_ratio_class), INTENT(in) :: this
      CLASS (model_class), INTENT(in)       :: a_model

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sxrem_ratio_get_observed_signal =                                        &
     &   a_model%get_sxrem_ratio(a_model%get_te_cart(this%x_cart))

      CALL profiler_set_stop_time('sxrem_ratio_get_observed_signal',           &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the feedback type.
!>
!>  Returns a description of the feedback type for use when writting output
!>  files.
!>
!>  @param[in] this A @ref sxrem_ratio_class instance.
!>  @returns A string describing the feedback type.
!-------------------------------------------------------------------------------
      FUNCTION sxrem_ratio_get_type(this)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length)      :: sxrem_ratio_get_type
      CLASS (sxrem_ratio_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sxrem_ratio_get_type = 'sxrem ratio'

      CALL profiler_set_stop_time('sxrem_ratio_get_type', start_time)

      END FUNCTION

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Write out auxiliary signal information to an output file.
!>
!>  Writes out the s_name and coefficient of the combined signals.
!>
!>  @param[in] this    A @ref sxrem_ratio_class instance.
!>  @param[in] iou     A input/output representing the file to write to.
!>  @param[in] index   A index of a signal.
!>  @param[in] a_model The equilibrium model.
!-------------------------------------------------------------------------------
      SUBROUTINE sxrem_ratio_write_auxiliary(this, iou, index, a_model)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (sxrem_ratio_class), INTENT(in) :: this
      INTEGER, INTENT(in)                   :: iou
      INTEGER, INTENT(in)                   :: index
      CLASS (model_class), INTENT(in)       :: a_model

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (iou,1000) this%indices(1), this%indices(2)

      CALL profiler_set_stop_time('sxrem_ratio_write_auxiliary',               &
     &                            start_time)

1000  FORMAT('sxrem profile ratio',1x,i2,'/',1x,i2)

      END SUBROUTINE

      END MODULE

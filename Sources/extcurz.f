!*******************************************************************************
!>  @file extcurz.f
!>  Contains module @ref extcurz.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Represents a signal obtained by integrating around the magnetic field to get
!>  the current enclosed in the Z direction using Ampere's Law. Defines the base
!>  class of the type @ref extcurz_class.
!>  @par Super Class:
!>  @ref diagnostic
!*******************************************************************************

      MODULE extcurz

      USE stel_kinds, only: rprec
      USE mpi_inc
      USE profiler
      USE signal

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) extcurz base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a external Z currents enclosed by an inegration
!>  loop.
!>  @par Super Class:
!>  @ref diagnostic
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(signal_class) :: extcurz_class
!>  Radial surface to compute external current from.
         REAL (rprec) :: r
!>  Poloidal angle to compute current from.
         REAL (rprec) :: theta
      CONTAINS
         PROCEDURE    ::                                                       &
     &      get_modeled_signal_last => extcurz_get_modeled_signal
         PROCEDURE    :: get_type => extcurz_get_type
         FINAL        :: extcurz_destruct
      END TYPE extcurz_class

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the construction of @ref extcurz_class types using
!>  @ref extcurz_construct.
!-------------------------------------------------------------------------------
      INTERFACE extcurz_class
         MODULE PROCEDURE extcurz_construct
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref extcurz_class object measureing temperature.
!>
!>  Allocates memory and initializes a @ref extcurz_class object.
!>
!>  @param[in] r     The radial position to integrate at.
!>  @param[in] theta The poloidal angle to integrate at.
!>  @returns A pointer to a constructed @ref extcurz_class object.
!-------------------------------------------------------------------------------
      FUNCTION extcurz_construct(r, theta)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (extcurz_class), POINTER :: extcurz_construct
      REAL(rprec), INTENT(in)        :: r
      REAL(rprec), INTENT(in)        :: theta

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(extcurz_construct)
      extcurz_construct%r = r
      extcurz_construct%theta = theta

      CALL profiler_set_stop_time('extcurz_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref extcurz_class object.
!>
!>  Deallocates memory and uninitializes a @ref extcurz_class object.
!>
!>  @param[inout] this A @ref extcurz_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE extcurz_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (extcurz_class), INTENT(inout) :: this

!  Start of executable code
      this%r = 0.0
      this%theta = 0.0

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled signal.
!>
!>  Calculates the inclused Z current from Ampere's Law from an loop integration
!>  of the magnetic field. The loop integrated magnetic field is provided by
!>  @ref equilibrium::equilibrium_get_Int_B_dphi
!>
!>  @param[inout] this       A @ref extcurz_class instance.
!>  @param[in]    a_model    A @ref model instance.
!>  @param[out]   sigma      The modeled sigma.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION extcurz_get_modeled_signal(this, a_model, sigma,                &
     &                                    last_value)
      USE model
      USE stel_constants, only: mu0

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: extcurz_get_modeled_signal
      CLASS (extcurz_class), INTENT(inout)    :: this
      TYPE (model_class), POINTER             :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out) :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)  :: last_value

!  local variables
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sigma = 0

      IF (BTEST(a_model%state_flags, model_state_vmec_flag)   .or.             &
     &    BTEST(a_model%state_flags, model_state_siesta_flag) .or.             &
     &    BTEST(a_model%state_flags, model_state_signal_flag)) THEN
         extcurz_get_modeled_signal(1) =                                       &
     &      equilibrium_get_Int_B_dphi(a_model%equilibrium, this%r,            &
     &                                 this%theta)/mu0

         CALL this%scale_and_offset(a_model,                                   &
     &                              extcurz_get_modeled_signal(1))
      ELSE
         extcurz_get_modeled_signal = last_value
      END IF

      CALL profiler_set_stop_time('extcurz_get_modeled_signal',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the extcurz type.
!>
!>  Returns a description of the extcurz type for use when writting output files.
!>
!>  @param[in] this A @ref extcurz_class instance.
!>  @returns A string describing the extcurz type.
!-------------------------------------------------------------------------------
      FUNCTION extcurz_get_type(this)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length)  :: extcurz_get_type
      CLASS (extcurz_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      extcurz_get_type = 'extcur'

      CALL profiler_set_stop_time('extcurz_get_type', start_time)

      END FUNCTION

      END MODULE

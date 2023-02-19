!*******************************************************************************
!>  @file limiter.f
!>  @brief Contains module @ref limiter
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref limiter_class.
!>  @par Super Class:
!>  @ref geometric
!>
!>  @see limiter_iso_T
!*******************************************************************************

      MODULE limiter

      USE stel_kinds, only: rprec, dp
      USE profiler
      USE signal

      IMPLICIT NONE
!*******************************************************************************
!  geometric module parameters
!*******************************************************************************
!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) limiter base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a limiter signal.
!>  @par Super class:
!>  @ref geometric
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(signal_class) :: limiter_class
!>  If true, the reconstruction tries to place the edge on the limiter.
!>  Otherwise, the plasma edge is kept inside the limiter.
         LOGICAL                             :: on_edge = .false.
!>  Limiter phi planes the limiters are defined at.
         REAL (rprec), DIMENSION(:), POINTER :: phi => null()
      CONTAINS
         PROCEDURE                           ::                                &
     &      get_modeled_signal_last => limiter_get_modeled_signal
         PROCEDURE                           ::
     &      get_header => limiter_get_header
         PROCEDURE                           ::                                &
     &      get_type => limiter_get_type
         PROCEDURE                           ::                                &
     &      get_max_fval => limiter_get_max_fval
         FINAL                               :: limiter_destruct
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************

      CONTAINS

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref limiter_class object.
!>
!>  Deallocates memory and uninitializes a @ref limiter_class object.
!> 
!>  @param[inout] this A @ref limiter_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE limiter_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (limiter_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%phi)) THEN
         DEALLOCATE(this%phi)
         this%phi => null()
      END IF

      this%on_edge = .false.

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled signal.
!>
!>  If the limiter is defined by an iso function returns the value of
!>  @ref limiter_get_modeled_iso_signal. Otherwise returns zero.
!>
!>  @param[inout] this       A @ref limiter_class instance.
!>  @param[in]    a_model    A @ref model instance.
!>  @param[out]   sigma      If the limiter type is defined by an iso function,
!>                           sigma value of @ref limiter_get_modeled_iso_signal.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns If the limiter type is defined by an iso function, return the value
!>           of @ref limiter_get_modeled_iso_signal. Otherwise returns zero.
!>  @note Polygon limiters have not be implemented yet.
!-------------------------------------------------------------------------------
      FUNCTION limiter_get_modeled_signal(this, a_model, sigma,                &
     &                                    last_value)
      USE model

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: limiter_get_modeled_signal
      CLASS (limiter_class), INTENT(inout)    :: this
      CLASS (model_class), POINTER            :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out) :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)  :: last_value

!  local variables
!  The r and z arrays will be allocated by the equilibrium.
      REAL (rprec), DIMENSION(:), POINTER     :: r
      REAL (rprec), DIMENSION(:), POINTER     :: z
      INTEGER                                 :: num_theta
      INTEGER                                 :: phi_index
      REAL (rprec), DIMENSION(3)              :: rphiz
      REAL (rprec)                            :: fval
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sigma = 0.0

      IF (BTEST(a_model%state_flags, model_state_vmec_flag)   .or.             &
     &    BTEST(a_model%state_flags, model_state_siesta_flag) .or.             &
     &    BTEST(a_model%state_flags, model_state_shift_flag)  .or.             &
     &    BTEST(a_model%state_flags, model_state_signal_flag)) THEN
         r => null()
         z => null()

!  Loop over each phi plane. r, z are allocated by equilibrium_get_plasma_edge.
!  There is no need to allocate and deallocate r and z for each phi angle.
         limiter_get_modeled_signal(1) = -1.0E10

         DO phi_index = 1, SIZE(this%phi)
            num_theta =                                                        &
     &         a_model%equilibrium%get_plasma_edge(this%phi(phi_index),        &
     &                                             r, z)

            fval = this%get_max_fval(num_theta, phi_index, r, z, rphiz)

            IF (fval .gt. limiter_get_modeled_signal(1)) THEN
               limiter_get_modeled_signal(1) = fval
               limiter_get_modeled_signal(2:4) = rphiz
            END IF
         END DO

         IF (.not.this%on_edge) THEN
            limiter_get_modeled_signal(1) =                                    &
     &         MAX(limiter_get_modeled_signal(1), 0.0)
         END IF

!  Dealloctae r and z since they are no longer needed.
         IF (ASSOCIATED(r)) THEN
            DEALLOCATE(r)
         END IF
         IF (ASSOCIATED(z)) THEN
            DEALLOCATE(z)
         END IF
         r => null()
         z => null()

         CALL this%scale_and_offset(a_model,                                   &
     &                              limiter_get_modeled_signal(1))
      ELSE
         limiter_get_modeled_signal = last_value
      END IF

      CALL profiler_set_stop_time('limiter_get_modeled_signal',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the limiter type.
!>
!>  Returns a description of the limiter type for use when writting output files.
!>
!>  @param[in] this A @ref limiter_class instance.
!>  @returns A string describing the limiter type.
!-------------------------------------------------------------------------------
      FUNCTION limiter_get_type(this)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length)  :: limiter_get_type
      CLASS (limiter_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      limiter_get_type = 'edge_lim '

      CALL profiler_set_stop_time('limiter_get_type', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the model and model sigma array indices.
!>
!>  Returns a description of the array indices for use when writting output
!>  files.
!>
!>  @param[in]    this   A @ref limiter_class instance.
!>  @param[inout] header Buffer arrays to write header strings to.
!-------------------------------------------------------------------------------
      SUBROUTINE limiter_get_header(this, header)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CLASS (limiter_class), INTENT(in)              :: this
      CHARACTER (len=data_name_length), DIMENSION(7), INTENT(inout) ::         &
     &   header

!  local variables
      REAL (rprec)                                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      header(1) = 'r (m)'
      header(2) = 'phi (rad)'
      header(3) = 'z (m)'
      header(4) = 'model_sig(1)'
      header(5) = 'model_sig(2)'
      header(6) = 'model_sig(3)'
      header(7) = 'model_sig(4)'

      CALL profiler_set_stop_time('limiter_get_header', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Calculates the maximum value of the limiter function.
!>
!>  This should be overwritten by a subclass method.
!>
!>  @param[in]  this      A @ref limiter_class instance.
!>  @param[in]  num_theta Number of points in the theta direction.
!>  @param[in]  phi_index Current phi index.
!>  @param[in]  r         R positions of the last closed flux surface.
!>  @param[in]  z         Z positions of the last closed flux surface.
!>  @param[out] rphiz_at_max R, Phi, Z position of the maximum function.
!>  @returns The maximum value of the iso function.
!-------------------------------------------------------------------------------
      FUNCTION limiter_get_max_fval(this, num_theta, phi_index,                &
     &                              r, z, rphiz_at_max)
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                            :: limiter_get_max_fval
      CLASS (limiter_class), INTENT(in)       :: this
      INTEGER, INTENT(in)                     :: num_theta
      INTEGER, INTENT(in)                     :: phi_index
      REAL (rprec), DIMENSION(:), INTENT(in)  :: r
      REAL (rprec), DIMENSION(:), INTENT(in)  :: z
      REAL (rprec), DIMENSION(3), INTENT(out) :: rphiz_at_max

!  Start of executable code
      CALL assert(.false., 'limiter_get_max_fval not over written' //          &
     &                     ' for ' // this%get_type())

      rphiz_at_max = 0.0
      limiter_get_max_fval = 0.0

      END FUNCTION

      END MODULE

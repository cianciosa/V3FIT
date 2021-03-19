!*******************************************************************************
!>  @file mse.f
!>  @brief Contains module @ref mse.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Implements motional stark effect diagnostic. Defines the base class of the
!>  type @ref mse_class.
!>  @par Super Class:
!>  @ref diagnostic
!*******************************************************************************

      MODULE mse

      USE stel_kinds, only: rprec, dp
      USE coordinate_utilities
      USE profiler
      USE mpi_inc
      USE signal

      IMPLICIT NONE

!*******************************************************************************
!  mse module parameters
!*******************************************************************************
!>  Bit position for the use coil response flag.
      INTEGER, PARAMETER :: mse_degrees_flag = 1
!>  Bit position for the force coil response flag.
      INTEGER, PARAMETER :: mse_ratio_flag   = 2

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) mse base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a mse signal.
!>  @par Super Class:
!>  @ref diagnostic
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(signal_class) :: mse_class
!>  Intersection point of the viewing angle and the neutral beam.
         REAL (rprec), DIMENSION(3) :: point

!>  Angle between the toroidal direction and neutral beam.
         REAL (rprec)               :: alpha
!>  Angle between the toroidal direction the view chord.
         REAL (rprec)               :: omega
!>  Pitch angle of the neutral beam with respect to the horizontal.
         REAL (rprec)               :: delta
!>  Pitch angle of the viewing chord with respect to the horizontal.
         REAL (rprec)               :: theta

!>  Control flags for the signal output
         INTEGER                    :: flags
      CONTAINS
         PROCEDURE                  ::                                         &
     &      get_modeled_signal_last => mse_get_modeled_signal
         PROCEDURE                  ::                                         &
     &      get_type => mse_get_type
         PROCEDURE                  :: get_header => mse_get_header
         PROCEDURE, PRIVATE         ::                                         &
     &      angle_to_bt_proj => mse_angle_to_bt_proj
         FINAL                      :: mse_destruct
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the construction of @ref mse_class types using
!>  @ref mse_construct_vec or @ref mse_construct_rad.
!-------------------------------------------------------------------------------
      INTERFACE mse_class
		 MODULE PROCEDURE mse_construct_vec,                                   &
     &                    mse_construct_rad
      END INTERFACE

      PRIVATE :: mse_angle_to_bt_proj, mse_angle_to_horizontal

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref mse_class object representing a motional stark
!>  effect diagnostic.
!>
!>  Allocates memory and initializes a @ref mse_class object. Uses vector
!>  representation.
!>
!>  @param[in] point      Measurement point of the mse diagnostic.
!>  @param[in] view_start Starting point of the view vector.
!>  @param[in] view_end   Ending point of the view vector.
!>  @param[in] beam_start Starting point of the beam vector.
!>  @param[in] beam_end   Ending point of the beam vector.
!>  @param[in] in_degrees Channel measures degrees.
!>  @param[in] is_ratio   Channel measures ratio.
!>  @returns A pointer to a constructed @ref mse_class object.
!-------------------------------------------------------------------------------
      FUNCTION mse_construct_vec(point, view_start, view_end,                  &
     &                           beam_start, beam_end, in_degrees,             &
     &                           is_ratio)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (mse_class), POINTER  :: mse_construct_vec
      REAL (rprec), DIMENSION(3), INTENT(in) :: point
      REAL (rprec), DIMENSION(3), INTENT(in) :: view_start
      REAL (rprec), DIMENSION(3), INTENT(in) :: view_end
      REAL (rprec), DIMENSION(3), INTENT(in) :: beam_start
      REAL (rprec), DIMENSION(3), INTENT(in) :: beam_end
      LOGICAL, INTENT(in)                    :: in_degrees
      LOGICAL, INTENT(in)                    :: is_ratio

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(mse_construct_vec)

      mse_construct_vec%flags = 0
      IF (in_degrees) THEN
         mse_construct_vec%flags = IBSET(mse_construct_vec%flags,              &
     &                                   mse_degrees_flag)
      END IF
      IF (is_ratio) THEN
         mse_construct_vec%flags = IBSET(mse_construct_vec%flags,              &
     &                                   mse_ratio_flag)
      END IF

      mse_construct_vec%point = point

!  Get the angles with respect to the toroidal field component.
!  The mse alpha is defined so that 0 points in the negative torodial direction
!  and 90 points in the negative radial direction. So this needs to be PI minus
!  the angle obtained from the dotproduct.
      mse_construct_vec%alpha = PI -                                           &
     &   mse_construct_vec%angle_to_bt_proj(beam_end - beam_start)
      mse_construct_vec%omega =                                                &
     &   mse_construct_vec%angle_to_bt_proj(view_end - view_start)

!  Get the angles with respect to the horizontal.
!  The mse delta is defined so that 90 points in the negative z direction. So
!  this needs to be the negation of the angle obtained from the dotproduct.
      mse_construct_vec%delta =                                                &
     &   -mse_angle_to_horizontal(beam_end - beam_start)
      mse_construct_vec%theta =                                                &
     &   mse_angle_to_horizontal(view_end - view_start)

      CALL profiler_set_stop_time('mse_construct_vec', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref mse_class object representing a motional stark
!>  effect diagnostic.
!>
!>  Allocates memory and initializes a @ref mse_class object. Uses angle
!>  representation.
!>
!>  @param[in] point      Measurement point of the mse diagnostic.
!>  @param[in] omega      Angle between the neutral beam and the toroidal in the
!>                        horizontal.
!>  @param[in] alpha      Angle between the view chord and the toroidal in the
!>                        horizontal.
!>  @param[in] delta      Pitch angle to the horizontal and the neutral beam.
!>  @param[in] theta      Pitch angle to the horizontal and the view chord.
!>  @param[in] in_degrees Channel measures degrees.
!>  @param[in] is_ratio   Channel measures ratio.
!>  @returns A pointer to a constructed @ref mse_class object.
!-------------------------------------------------------------------------------
      FUNCTION mse_construct_rad(point, alpha, omega, delta, theta,            &
     &                           in_degrees, is_ratio)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (mse_class), POINTER             :: mse_construct_rad
      REAL (rprec), DIMENSION(3), INTENT(in) :: point
      REAL (rprec), INTENT(in)               :: alpha
      REAL (rprec), INTENT(in)               :: omega
      REAL (rprec), INTENT(in)               :: delta
      REAL (rprec), INTENT(in)               :: theta
      LOGICAL, INTENT(in)                    :: in_degrees
      LOGICAL, INTENT(in)                    :: is_ratio

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(mse_construct_rad)

      mse_construct_rad%flags = 0
      IF (in_degrees) THEN
         mse_construct_rad%flags = IBSET(mse_construct_rad%flags,              &
     &                                   mse_degrees_flag)
      END IF
      IF (is_ratio) THEN
         mse_construct_rad%flags = IBSET(mse_construct_rad%flags,              &
     &                                   mse_ratio_flag)
      END IF

      mse_construct_rad%point = point

!  Get the angles with respect to the toroidal field component.
      mse_construct_rad%alpha = alpha
      mse_construct_rad%omega = omega
      mse_construct_rad%delta = delta
      mse_construct_rad%theta = theta

      CALL profiler_set_stop_time('mse_construct_rad', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref mse_class object.
!>
!>  Deallocates memory and uninitializes a @ref mse_class object.
!>
!>  @param[inout] this A @ref mse_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE mse_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (mse_class), INTENT(inout) :: this

!  Start of executable code
      this%point = 0.0
      this%alpha = 0.0
      this%omega = 0.0
      this%delta = 0.0
      this%theta = 0.0
      this%flags = 0

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled signal.
!>
!>  Calculates the modeled signal by Equation 1 of Hawkes et. al.
!>  doi:10.1063/1.1149415. The polarization angle gamma_m is determined from.
!>
!>    tan(gamma_m) = E_h/E_v                                                 (1)
!>
!>    E_h = -[B_v*Cos(delta)*Cos(omega + alpha) +
!>            Sin(delta)*(B_r*Sin(omega) - B_t*Cos(omega)]                   (2)
!>
!>    E_v = -B_v*Sin(theta)*Cos(delta)*Sin(omega + alpha)
!>        + Sin(theta)*Sin(delta)*(B_t*Sin(omega) + B_r*Cos(omega))
!>        + Cos(theta)*Sin(delta)*(B_r*Cos(alpha) - B_t*Sin(alpha))          (3)
!>
!>  The magnetic field components are defined as
!>  * B_r Radial component of the magnetic field
!>  * B_t Toroidal component of the magnetic field
!>  * B_v Vertical component of the magnetic field
!>
!>  The angles are defined as
!>  * alpha: Projected angle between the neutral beam and B_t. Defined such that
!>    0 is the negative toroidal direction. When 90, it is in the negative
!>    radial direction.
!>  * omega: Projected angle between the view and and B_t. Defined such that 0
!>    is the positive toroidal direction. When 90, it is in the negative
!>    radial direction.
!>  * delta: Angle between the neutral beam and the horizontal. Defined such
!>    that 90 is in the negative z direction.
!>  * theta: Angle between the view and the horizontal. Defined such that 90 is
!>    in the positive z direction.
!>
!>  @param[inout]  this       A @ref mse_class instance.
!>  @param[in]     a_model    A @ref model instance.
!>  @param[out]    sigma      The modeled sigma.
!>  @param[in]     last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION mse_get_modeled_signal(this, a_model, sigma, last_value)
      USE model

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: mse_get_modeled_signal
      CLASS (mse_class), INTENT(inout)        :: this
      TYPE (model_class), POINTER             :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out) :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)  :: last_value

!  local variables
      REAL (rprec), DIMENSION(3)              :: b_cyl
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sigma = 0.0

      IF (BTEST(a_model%state_flags, model_state_vmec_flag)   .or.             &
     &    BTEST(a_model%state_flags, model_state_siesta_flag) .or.             &
     &    BTEST(a_model%state_flags, model_state_shift_flag)  .or.             &
     &    BTEST(a_model%state_flags, model_state_signal_flag)) THEN

!  Get the magnetic field vector.
         b_cyl = a_model%equilibrium%get_B_vec(this%point, .true.)

         mse_get_modeled_signal(2) =                                           &
     &         -(b_cyl(3)*COS(this%delta)*COS(this%omega + this%alpha) +       &
     &           SIN(this%delta)*(b_cyl(1)*SIN(this%omega) -                   &
     &                            b_cyl(2)*COS(this%omega)))

         mse_get_modeled_signal(3) =                                           &
     &         -b_cyl(3)*SIN(this%theta)*COS(this%delta)*                      &
     &          SIN(this%omega +  this%alpha)                                  &
     &       + SIN(this%theta)*SIN(this%delta)*                                &
     &         (b_cyl(2)*SIN(this%omega) + b_cyl(1)*COS(this%omega))           &
     &       + COS(this%theta)*COS(this%delta)*                                &
     &         (b_cyl(1)*COS(this%alpha) - b_cyl(2)*SIN(this%alpha))

         mse_get_modeled_signal(4) = mse_get_modeled_signal(2)                 &
     &                             / mse_get_modeled_signal(3)

         IF (BTEST(this%flags, mse_ratio_flag)) THEN
            mse_get_modeled_signal(1) = mse_get_modeled_signal(4)
         ELSE
            mse_get_modeled_signal(1) = ATAN2(mse_get_modeled_signal(2),       &
     &                                        mse_get_modeled_signal(3))

            IF (BTEST(this%flags, mse_degrees_flag)) THEN
               mse_get_modeled_signal(1) = mse_get_modeled_signal(1)           &
     &                                   / degree
            END IF
         END IF

         CALL this%scale_and_offset(a_model, mse_get_modeled_signal(1))
      ELSE
         mse_get_modeled_signal = last_value
      END IF

      CALL profiler_set_stop_time('mse_get_modeled_signal', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the mse type.
!>
!>  Returns a description of the mse type for use when writting output files.
!>
!>  @param[in] this A @ref mse_class instance.
!>  @returns A string describing the mse type.
!-------------------------------------------------------------------------------
      FUNCTION mse_get_type(this)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: mse_get_type
      CLASS (mse_class), INTENT(in)    :: this

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      mse_get_type = 'mse'

      CALL profiler_set_stop_time('mse_get_type', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the model and model sigma array indices.
!>
!>  Returns a description of the array indices for use when writting output
!>  files.
!>
!>  @param[in]    this   A @ref mse_class instance.
!>  @param[inout] header Buffer arrays to write header strings to.
!>  @returns A string describing the model and model sigma array indices.
!-------------------------------------------------------------------------------
      SUBROUTINE mse_get_header(this, header)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CLASS (mse_class), INTENT(in)                  :: this
      CHARACTER (len=data_name_length), DIMENSION(7), INTENT(inout) ::         &
     &   header

!  local variables
      REAL (rprec)                                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      header(1) = 'e_h'
      header(2) = 'e_v'
      header(3) = 'ratio'

      header(4) = 'model_sig(1)'
      header(5) = 'model_sig(2)'
      header(6) = 'model_sig(3)'
      header(7) = 'model_sig(4)'

      CALL profiler_set_stop_time('mse_get_header', start_time)

      END SUBROUTINE

!*******************************************************************************
!  PRIVATE
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Computes the angle with respect to toroidal field.
!>
!>  Returns the angle projected to the horizontal between an arbitray vector and
!>  and the toroidal field. This angle is computed by.
!>
!>    angle = acos(n_t.vxy/|vxy|)                                            (1)
!>
!>  where n_t is the unit vector in the toroidal direction at the measurement
!>  point.
!>
!>    n_t = (-p_y, p_x)/|p|                                                  (2)
!>
!>  @param[in] this A @ref mse_class instance.
!>  @param[in] vec  An arbitray vector.
!>  @param[in] bvec The magnetic filed vector.
!>  @returns The projected angle between the vector and the toroidal
!-------------------------------------------------------------------------------
      FUNCTION mse_angle_to_bt_proj(this, vec)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                  :: mse_angle_to_bt_proj
      CLASS (mse_class), INTENT(in) :: this
      REAL (rprec), DIMENSION(3)    :: vec

!  local variables
      REAL (rprec), DIMENSION(2)    :: temp_vec
      REAL (rprec), DIMENSION(2)    :: norm_vec
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Unit vector in the direction of
      temp_vec = (/ -this%point(2), this%point(1) /)                           &
     &         / SQRT(DOT_PRODUCT(this%point(1:2), this%point(1:2)))

!  Compute the angle.
      norm_vec = vec(1:2)/SQRT(DOT_PRODUCT(vec(1:2), vec(1:2)))
      mse_angle_to_bt_proj = ACOS(DOT_PRODUCT(temp_vec, norm_vec))

      CALL profiler_set_stop_time('mse_angle_to_bt_proj', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Computes the angle with respect to horizontal.
!>
!>  Returns the angle to the horizontal for an arbitray vector. This angle is
!>  computed by.
!>
!>    angle = asin(vecz/|vecz|)
!>
!>  @param[in] vec  An arbitray vector.
!>  @returns The projected angle between the vector and horizontal
!-------------------------------------------------------------------------------
      FUNCTION mse_angle_to_horizontal(vec)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)               :: mse_angle_to_horizontal
      REAL (rprec), DIMENSION(3) :: vec

!  local variables
      REAL (rprec)               :: h
      REAL (rprec)               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      h = SQRT(DOT_PRODUCT(vec, vec))
      mse_angle_to_horizontal = ASIN(vec(3)/h)
      mse_angle_to_horizontal = PI/2.0 - mse_angle_to_horizontal

      CALL profiler_set_stop_time('mse_angle_to_horizontal', start_time)

	  END FUNCTION

      END MODULE

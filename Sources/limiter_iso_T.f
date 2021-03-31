!*******************************************************************************
!>  @file limiter_iso_T.f
!>  @brief Contains module @ref limiter_iso_T
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref limiter_iso.
!*******************************************************************************
      MODULE limiter_iso_T

      USE stel_kinds, only : rprec, cprec
      USE v3_utilities
      USE limiter

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) extcurz base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a limiter_iso function.
!>
!>  1) Limiter iso: limiter_iso
!>  Contains the data to define a scalar function of position, so that the
!>  iso-contour f=0 of the function corresponds to a geometric limit to the
!>  plasma. Also contains data to specify the minimum number poloidal points on
!>  the s=1 surface to use, and the toroidal planes on which to s=1 surface will
!>  be evaluated
!>
!>  Second attempt:
!>
!>  e = SUM_over_i(0,4)_j(0,4) [arz(i,j) (r - rc)^i (z - zc)^j]
!>  f = e / |grad(e)|
!>
!>  - @note
!>  1. For now, axisymmetric
!>  2. Easy to put in circles, ellipses, and planes
!>  3. Function f is approximately distance.
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(limiter_class) :: limiter_iso_class
!>  Coefficients for the iso function.
         REAL (rprec), DIMENSION(0:4,0:4)   :: arz
!>  R offset for function.
         REAL (rprec)                       :: rc
!>  Z offset for function.
         REAL (rprec)                       :: zc
!>  Minimum number of poloidal angles for s=1 surface.
         INTEGER                            :: numin
      CONTAINS
         PROCEDURE                          ::                                 &
     &      get_max_fval => limiter_iso_get_max_fval
         PROCEDURE                          ::                                 &
     &      get_type => limiter_iso_get_type
         PROCEDURE                          ::                                 &
     &      get_value => limiter_iso_get_value
         FINAL                              :: limiter_iso_destruct
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for iso contour limiter constructor.
!-------------------------------------------------------------------------------
      INTERFACE limiter_iso_class
         MODULE PROCEDURE limiter_iso_construct
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a limiter iso function.
!>
!>  Allocates memory and initializes a @ref limiter_iso object.
!>
!>  @param[in] arz   Coefficients for the iso function.
!>  @param[in] rc    R offset for function.
!>  @param[in] zc    Z offset for function.
!>  @param[in] numin Minimum number of poloidal angles for s=1 surface.
!>  @param[in] vgrid Values of toroidal angle at which to compute s=1 surface.
!>  @param[in] on_edge     Specifies if the edge should touch the limiter or
!>                         not. If true, the reconstruction tries to have the
!>                         limiter touch the limiter. If false, the edge will
!>                         only just fall inside the limiter.
!>  @returns A pointer to a constructed @ref limiter_iso object.
!-------------------------------------------------------------------------------
      FUNCTION limiter_iso_construct(arz, rc, zc, numin, vgrid, on_edge)

      IMPLICIT NONE

!  Declare Arguments 
      CLASS (limiter_iso_class), POINTER :: limiter_iso_construct
      REAL (rprec), DIMENSION(0:4,0:4), INTENT(in) :: arz
      REAL (rprec), INTENT(in)                     :: rc
      REAL (rprec), INTENT(in)                     :: zc
      INTEGER, INTENT(in)                          :: numin
      REAL (rprec), DIMENSION(:), INTENT(in)       :: vgrid
      LOGICAL, INTENT(in)                          :: on_edge

!  local variables
      REAL (rprec)                                 :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  First, destroy this
      ALLOCATE(limiter_iso_construct)
      
      limiter_iso_construct%arz = arz
      limiter_iso_construct%rc = rc
      limiter_iso_construct%zc = zc
      limiter_iso_construct%numin = numin
      limiter_iso_construct%on_edge = on_edge

      ALLOCATE(limiter_iso_construct%phi(SIZE(vgrid)))
      limiter_iso_construct%phi = vgrid

      CALL profiler_set_stop_time('limiter_iso_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref limiter_iso object.
!>
!>  Deallocates memory and uninitializes a @ref limiter_iso object.
!>
!>  @param[inout] this A @ref limiter_iso instance.
!-------------------------------------------------------------------------------
      SUBROUTINE limiter_iso_destruct(this)

      IMPLICIT NONE

!  Declare Arguments 
      TYPE (limiter_iso_class), INTENT(inout) :: this

!  Start of executable code

!  Get rid of all components
      this%arz = 0.0
      this%rc = 0.0
      this%zc = 0.0
      this%numin = 0

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the maximum value of the iso function.
!>
!>  @param[in]  this      A @ref limiter_class instance.
!>  @param[in]  num_theta Number of points in the theta direction.
!>  @param[in]  phi_index Current phi index.
!>  @param[in]  r         R positions of the last closed flux surface.
!>  @param[in]  z         Z positions of the last closed flux surface.
!>  @param[out] rphiz_at_max R, Phi, Z position of the maximum function.
!>  @returns The maximum value of the iso function.
!-------------------------------------------------------------------------------
      FUNCTION limiter_iso_get_max_fval(this, num_theta, phi_index,            &
     &                                  r, z, rphiz_at_max)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: limiter_iso_get_max_fval
      CLASS (limiter_iso_class), INTENT(in)   :: this
         INTEGER, INTENT(in)                  :: num_theta
      INTEGER, INTENT(in)                     :: phi_index
      REAL (rprec), DIMENSION(:), INTENT(in)  :: r
      REAL (rprec), DIMENSION(:), INTENT(in)  :: z
      REAL (rprec), DIMENSION(3), INTENT(out) :: rphiz_at_max

!  local variables
      INTEGER                                 :: theta_index
      REAL (rprec)                            :: fval
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      limiter_iso_get_max_fval = -1.0E10
      rphiz_at_max = 0.0

      DO theta_index = 1, num_theta
         fval = this%get_value(r(theta_index), z(theta_index))

         IF (fval .gt. limiter_iso_get_max_fval) THEN
            limiter_iso_get_max_fval = fval
            rphiz_at_max = (/ r(theta_index), this%phi(phi_index),             &
     &                        z(theta_index) /)
         END IF
      END DO

      CALL profiler_set_stop_time('limiter_iso_get_max_fval',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Computes the iso value at an r, phi, z position.
!>
!>  Limiter iso function is computed as
!>
!>  e = SUM_over_i(0,4)_j(0,4) [arz(i,j) (r - rc)^i (z - zc)^j]
!>  f = e / |grad(e)|
!>
!>
!>  @param[in] this    A @ref limiter_iso instance.
!>  @param[in] rpz_arg The r point to evaluate at.
!>  @param[in] rpz_arg The z point to evaluate at.
!>  @returns The value of iso function at the r, phi, z point.
!-------------------------------------------------------------------------------
      FUNCTION limiter_iso_get_value(this, r, z)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: limiter_iso_get_value
      CLASS (limiter_iso_class), INTENT(in)  :: this
      REAL (rprec), INTENT(in)               :: r
      REAL (rprec), INTENT(in)               :: z

!  local variables
      REAL (rprec)                           :: e
      REAL (rprec)                           :: er
      REAL (rprec)                           :: ez
      REAL (rprec)                           :: grad_e
      INTEGER                                :: i
      INTEGER                                :: j
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Compute function
      e = 0.0
      DO j = 0, 4
         DO i = 0, 4
            e = e                                                              &
     &        + this%arz(i,j)*((r - this%rc)**i)*((z - this%zc)**j)
         END DO
      END DO

!  Compute de/dr
      er = 0.0
      DO j = 0, 4
         DO i = 1, 4
            er = er                                                            &
     &         + this%arz(i,j)*i*((r - this%rc)**(i-1)) *                      &
     &           ((z - this%zc)**j)
         END DO
      END DO

!  Compute de/dz
      ez = 0.0
      DO j = 1, 4
         DO i = 0, 4
            ez = ez                                                            &
     &          + this%arz(i,j)*j*((r - this%rc)**i) *                         &
     &            ((z - this%zc)**(j - 1))
         END DO
      END DO

!  Avoid divide by zero errors.
      grad_e = MAX(1.E-12, SQRT(er*er + ez*ez))
      
      limiter_iso_get_value = e/grad_e

      CALL profiler_set_stop_time('limiter_iso_get_value', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the limiter iso type.
!>
!>  Returns a description of the limiter type for use when writting output files.
!>
!>  @param[in] this A @ref limiter_iso_class instance.
!>  @returns A string describing the limiter type.
!-------------------------------------------------------------------------------
      FUNCTION limiter_iso_get_type(this)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: limiter_iso_get_type
      CLASS (limiter_iso_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      limiter_iso_get_type =                                                   &
     &   TRIM(this%limiter_class%get_type()) // 'iso'

      CALL profiler_set_stop_time('limiter_iso_get_type', start_time)

      END FUNCTION

      END MODULE

!-------------------------------------------------------------------------------
!  The @header, @table_section, @table_subsection, @item and @end_table commands
!  are custom defined commands in Doxygen.in. They are defined under ALIASES.
!  For the page created here, the 80 column limit is exceeded. Arguments of
!  aliases are separated by ','. If you intended ',' to be a string you must use
!  an escaped comma '\,'.
!
!>  @page profile_sec Profiles types.
!>
!>  @tableofcontents
!>  @section profile_namelist_intro_sec Introduction
!>  This page contains discriptions of the types of profiles that can be used
!>  for parameterized profiles. Profiles are tagged in the
!>  @ref model_profile_sec, as @fixed_width{pp_*_} where @fixed_width{*} refers
!>  to the specific profile.
!>
!>  @section profile_types_sec Profile types
!>  Profiles can be defined as any of the following types.
!>
!>  @subsection profile_none none
!>  @fixed_width{none} No profile is used in this instance.
!>
!>  @subsection profile_two_power two_power
!>  @fixed_width{'two_power'} Profile is defined as a two power poly nomical
!>  with 4 coeffiecents. Coefficients are defined as in the first 4 indicies of
!>  the @fixed_width{'_b'} array. The profile is defined as follows.
!>
!>     a(x) = b[0] + b[1]*(1 - x^b[2])^b[3]
!>
!>  where x is valid from zero to one.
!>
!>  @subsection profile_two_power_gs two_power_gs
!>  @fixed_width{'two_power_gs'} Profile is defined as a two power poly nomical
!>  with 4 coefficients plus and an arbitrary number of additional guassian
!>  functions. The number of giassian functions if limited to the size of the
!>  @fixed_width{'_b'} array. The two power part is defined the same as
!>  @ref profile_two_power. The remaining guassian bits are defined as
!>
!>     a(x) = b[i]*Exp(-(x - b[i + 1])^2/b[i + 2]^2)
!>
!>  @subsection profile_two_power_r two_power_r
!>  @fixed_width{'two_power_r'} Reversed Two power profile defined the same as
!>  @ref profile_two_power except the argument is reversed. The function is
!>  maximum at x = 1 and minimum at x = 0.
!>
!>  @subsection profile_power_series_gs power_series
!>  @fixed_width{'power_series'} Profile is defined as a power series with
!>  @fixed_width{'_b'} array coefficients.
!>
!>     a(x) = b[0] + b[1]*x + b[2]*x^2 + b[3]*x^3 + etc...
!>
!>  @subsection profile_cubic_spline cubic_spline
!>  @fixed_width{'cubic_spline'} Profile defined as cubic spline segments.
!>  Splines knots are defined by the position, @fixed_width{'_as'} array, and
!>  amplitude, @fixed_width{'_af'} array.
!>
!>  @subsection profile_akima_spline akima_spline
!>  @fixed_width{'akima_spline'} Profile defined as akima spline segments.
!>  Splines knots are defined by the position, @fixed_width{'_as'} array, and
!>  amplitude, @fixed_width{'_af'} array.
!>
!>  @subsection profile_line_segment line_segment
!>  @fixed_width{'line_segment'} Profile defined as line segments. Segment knots
!>  are defined by the position, @fixed_width{'_as'} array, and amplitude,
!>  @fixed_width{'_af'} array.
!>
!>  @subsection profile_sq_ecp sq_exp_1d
!>  @fixed_width{'sq_exp_1d'} Guassian process profile are defined by hyper
!>  parameters in the first two elements of the @fixed_width{'_b'} array. The
!>  points the profile is computed to is defined by positions on the
!>  @fixed_width{'_as'} array.
!>
!>  @subsection profile_sq_ecp sq_exp_1d_ln
!>  @fixed_width{'sq_exp_1d_ln'} Guassian process profile are defined by hyper
!>  parameters in the first two elements of the @fixed_width{'_b'} array. The
!>  points the profile is computed to is defined by positions on the
!>  @fixed_width{'_as'} array.
!*******************************************************************************
!>  @file pprofile_T.f
!>  @brief Contains module @ref pprofile_T.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref pprofile_class. This module contains
!>  all the code necessary to define parameterized profiles.
!*******************************************************************************
      MODULE pprofile_T
      USE stel_kinds
      USE stel_constants
      USE v3_utilities
      USE mpi_inc
      USE profiler

      IMPLICIT NONE

!*******************************************************************************
!  model module parameters
!*******************************************************************************
!>  Maximum size for parameter profile name lengths.
      INTEGER, PARAMETER, PUBLIC :: p_type_len=20

!>  Lower array bound for function profiles.
      INTEGER, PARAMETER, PUBLIC :: ilb_b   = 0
!>  Upper array bound for function profiles.
      INTEGER, PARAMETER, PUBLIC :: iub_b   = 21
!>  Array size for spline profiles.
      INTEGER, PARAMETER, PUBLIC :: iub_asf = 101

!>  No profile type.
      INTEGER, PARAMETER, PRIVATE :: pprofile_none_type           = -1
!>  Two Power profile type.
      INTEGER, PARAMETER, PRIVATE :: pprofile_two_power_type      = 0
!>  Two Power with guassian profile type.
      INTEGER, PARAMETER, PRIVATE :: pprofile_two_power_gs_type   = 1
!>  Power Series profile type.
      INTEGER, PARAMETER, PRIVATE :: pprofile_power_series_type   = 2
!>  Cubic Spline profile type.
      INTEGER, PARAMETER, PRIVATE :: pprofile_cubic_spline_type   = 3
!>  Akima Spline profile type.
      INTEGER, PARAMETER, PRIVATE :: pprofile_akima_spline_type   = 4
!>  Line Segment profile type.
      INTEGER, PARAMETER, PRIVATE :: pprofile_line_segment_type   = 5
!>  Reverse Two Power profile type.
      INTEGER, PARAMETER, PRIVATE :: pprofile_two_power_r_type    = 6
!>  Guassian process profile 1D square exponetal type.
      INTEGER, PARAMETER, PRIVATE :: pprofile_gp_1d_sexp_type     = 7
!>  Guassian process profile 1D square exponetal with the ln of sigma x type.
      INTEGER, PARAMETER, PRIVATE :: pprofile_gp_1d_ln_sexp_type  = 8

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) model class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a parameterized profile.
!-------------------------------------------------------------------------------
      TYPE :: pprofile_class
!>  Parameterized profile type
         INTEGER :: p_type = pprofile_none_type
!>  Array of coefficients for the functional profiles.
         REAL (rprec), DIMENSION(ilb_b:iub_b) :: b = 0.0
!>  Array of radial position values for the segmented profiles.
         REAL (rprec), DIMENSION(:), POINTER  :: as => null()
!>  Array of profile values for the segmented profiles.
         REAL (rprec), DIMENSION(:), POINTER  :: af => null()
!>  Index of the last segemented value.
         INTEGER                              :: maxSplineIndex = 1

!>  Cached value of the af array.
         REAL (rprec), DIMENSION(:), POINTER  :: cache => null()
!>  Cached value of the af array.
         REAL (rprec), DIMENSION(:), POINTER  :: cache_hyper => null()
      CONTAINS
         PROCEDURE :: get_value => pprofile_get_value
         PROCEDURE :: get_p_type_name => pprofile_get_p_type_name
         PROCEDURE :: get_gp_ij => pprofile_get_gp_ij
         PROCEDURE :: get_gp_pi => pprofile_get_gp_pi
         PROCEDURE :: get_gp_pp => pprofile_get_gp_pp
         GENERIC   :: get_gp => get_gp_ij, get_gp_pi, get_gp_pp
         PROCEDURE :: get_gp_num_hyper_param =>                                &
     &                   pprofile_get_gp_num_hyper_param
         PROCEDURE :: write => pprofile_write                                  &
         PROCEDURE :: save_state => pprofile_save_state
         PROCEDURE :: reset_state => pprofile_reset_state
         FINAL     :: pprofile_destruct
      END TYPE

!-------------------------------------------------------------------------------
!>  Pointer to a pprofile object. Used for creating arrays of pprofile pointers.
!>  This is needed because fortran does not allow arrays of pointers directly.
!-------------------------------------------------------------------------------
      TYPE pprofile_pointer
         TYPE (pprofile_class), POINTER :: p => null()
      END TYPE

!*******************************************************************************
! SECTION III. INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for sxrem ti constructor.
!-------------------------------------------------------------------------------
      INTERFACE pprofile_class
         MODULE PROCEDURE pprofile_construct
      END INTERFACE

      PRIVATE findMaxIndex

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref pprofile_class.
!>
!>  Allocates memory and initializes a @ref pprofile_class.
!>
!>  @param[in] p_type Profile type discription.
!>  @param[in] b      Array of functional profile coefficients
!>  @param[in] as     Array of segmented radial positions.
!>  @param[in] af     Array of segmented profile values.
!>  @returns A pointer to a constructed @ref pprofile_class object.
!-------------------------------------------------------------------------------
      FUNCTION pprofile_construct(p_type, b, as, af)

      IMPLICIT NONE

!  Declare Arguments 
      TYPE (pprofile_class), POINTER :: pprofile_construct
      CHARACTER (len=*), INTENT(in)         :: p_type
      REAL(rprec), DIMENSION(:), INTENT(in) :: b
      REAL(rprec), DIMENSION(:), INTENT(in) :: as
      REAL(rprec), DIMENSION(:), INTENT(in) :: af

!  local variables
      CHARACTER (len=p_type_len)            :: p_type_lc
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(pprofile_construct)

! Store profile coeffients.
      pprofile_construct%b = b

! Find and store the maximum s index if specifying splines.
      pprofile_construct%maxSplineIndex = findMaxIndex(as)
      ALLOCATE(pprofile_construct%as(pprofile_construct%maxSplineIndex))
      pprofile_construct%as = as(1:pprofile_construct%maxSplineIndex)

      ALLOCATE(pprofile_construct%af(pprofile_construct%maxSplineIndex))
      pprofile_construct%af = af(1:pprofile_construct%maxSplineIndex)

!  Convert the type to lower case, and truncate
      p_type_lc = p_type
      CALL tolower(p_type_lc)
      SELECT CASE(TRIM(p_type_lc))
      
         CASE ('two_power')
            pprofile_construct%p_type = pprofile_two_power_type

         CASE ('two_power_gs')
            pprofile_construct%p_type = pprofile_two_power_gs_type

         CASE ('two_power_r')
            pprofile_construct%p_type = pprofile_two_power_r_type

         CASE ('power_series')
            pprofile_construct%p_type = pprofile_power_series_type

         CASE ('cubic_spline')
            IF (pprofile_construct%maxSplineIndex .lt. 4) THEN
               WRITE(*,*) 'pprofile:cubic spline: too few as values'
               WRITE(*,*) 'maxSplineIndex, as = ',                             &
     &                    pprofile_construct%maxSplineIndex
               WRITE(*,*) pprofile_construct%as
               CALL EXIT(1)
            END IF
            pprofile_construct%p_type = pprofile_cubic_spline_type

         CASE ('akima_spline')
            IF (pprofile_construct%maxSplineIndex .lt. 4) THEN
               WRITE(*,*) 'pprofile:akima spline: too few as values'
               WRITE(*,*) 'maxSplineIndex, as = ',                             &
     &                    pprofile_construct%maxSplineIndex
               WRITE(*,*) pprofile_construct%as
               CALL EXIT(1)
            END IF
            pprofile_construct%p_type = pprofile_akima_spline_type

         CASE ('line_segment')
            pprofile_construct%p_type = pprofile_line_segment_type

         CASE ('sq_exp_1d')
            pprofile_construct%p_type = pprofile_gp_1d_sexp_type
            ALLOCATE(pprofile_construct%cache(                                 &
     &                  SIZE(pprofile_construct%af)))
            ALLOCATE(pprofile_construct%cache_hyper(2))

         CASE ('sq_exp_1d_ln')
            pprofile_construct%p_type = pprofile_gp_1d_ln_sexp_type
            ALLOCATE(pprofile_construct%cache(                                 &
     &                  SIZE(pprofile_construct%af)))
            ALLOCATE(pprofile_construct%cache_hyper(2))

         CASE ('none')
            pprofile_construct%p_type = pprofile_none_type

         CASE DEFAULT
            pprofile_construct%p_type = pprofile_none_type
            WRITE(*,*) 'Unrecognized p_type:', p_type_lc
            WRITE(*,*) ' *** CHECK YOUR INPUT ***'
         
      END SELECT

      CALL profiler_set_stop_time('pprofile_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref pprofile_class object.
!>
!>  Deallocates memory and uninitializes a @ref pprofile_class object.
!>
!>  @param[inout] this A @ref pprofile_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE pprofile_destruct(this)

      IMPLICIT NONE

!  Declare Arguments 
      TYPE (pprofile_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%as)) THEN
         DEALLOCATE(this%as)
         this%as => null()
      END IF

      IF (ASSOCIATED(this%af)) THEN
         DEALLOCATE(this%af)
         this%af => null()
      END IF

      IF (ASSOCIATED(this%cache)) THEN
         DEALLOCATE(this%cache)
         this%cache => null()
      END IF

      IF (ASSOCIATED(this%cache_hyper)) THEN
         DEALLOCATE(this%cache_hyper)
         this%cache_hyper => null()
      END IF

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Gets the value of a profile at a radial position.
!>
!>  Evaluates the profile at the specifed radial position. If the radial
!>  position is out side the plasma, it returns the baseline value. If there is
!>  no profile type specified return the default value.
!>
!>  @param[in] this  A @ref pprofile_class instance.
!>  @param[in] s_arg Radial position to evaluate the profile at.
!>  @returns The value of the profile.
!>  @todo This function makes vmec specific assumptions of 1 being the maximum
!>  radial value.
!-------------------------------------------------------------------------------
      FUNCTION pprofile_get_value(this, s_arg)
      USE line_segment
      USE functions

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                        :: pprofile_get_value
      CLASS (pprofile_class), INTENT (in) :: this
      REAL (rprec), INTENT(in)            :: s_arg

!  local variables
      REAL (rprec)                        :: s_use
      REAL (rprec)                        :: s_01
      LOGICAL                             :: l_01
      INTEGER                             :: i
      INTEGER                             :: iflag
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Define argument and logical variable for s between zero and one
      s_01 = MAX(zero, MIN(one,s_arg))
      l_01 = (s_arg .ge. zero) .and. (s_arg .le. one)

      SELECT CASE(this%p_type)
      
         CASE (pprofile_none_type)
            pprofile_get_value = -1.E49_rprec

! v3fit uses an extra varible as an offset than vmec as a result the b array is
! one index shorter than what two_power takes.
         CASE (pprofile_two_power_type)
            pprofile_get_value = this%b(0)
            IF (l_01) THEN
               pprofile_get_value = pprofile_get_value                         &
     &                            + two_power(s_arg, this%b(1:iub_b))
            END IF

! v3fit uses an extra varible as an offset than vmec as a result the b array is
! one index shorter than what two_power takes. This profile reverses the s
! argument so that the maximum amplitude is at s=1 and the minimum is at s = 0.
         CASE (pprofile_two_power_r_type)
            pprofile_get_value = this%b(0)
            IF (l_01) THEN
               s_use = 1.0 - s_arg
               pprofile_get_value = pprofile_get_value                         &
     &                            + two_power(s_use, this%b(1:iub_b))
            END IF

! v3fit uses an extra varible as an offset than vmec as a result the b array is
! one index shorter than what two_power_gs takes. As a result only 5 guassian
! peaks maybe specified instead of the 6 vmec can use.
         CASE (pprofile_two_power_gs_type)
            pprofile_get_value = this%b(0)
            IF (l_01) THEN
               pprofile_get_value = pprofile_get_value                         &
     &                            + two_power_gs(s_arg,                        &
     &                                           this%b(1:iub_b))
            END IF
      
         CASE (pprofile_power_series_type)
            pprofile_get_value = zero
            DO i = iub_b, ilb_b, -1      ! all of b array, backwards
               pprofile_get_value = pprofile_get_value*s_01                    &
     &                            + this%b(i)
            END DO

         CASE (pprofile_cubic_spline_type)
            s_use = MIN(this%as(this%maxSplineIndex),                          &
     &                  MAX(s_arg, this%as(1)))
            CALL spline_cubic(s_use, pprofile_get_value,                       &
     &                        this%as, this%af,                                &
     &                        this%maxSplineIndex, iflag)
            IF (iflag .ge. 0) THEN

               CALL profiler_set_stop_time('pprofile_get_value',               &
     &                                     start_time)

               RETURN
            ELSE IF (iflag .eq. -1) THEN
               WRITE (*,*) 'ERROR: pprofile: outside value from ' //           &
     &                     'spline_cubic'
            ELSE IF(iflag .eq. -2) THEN
               WRITE (*,*) 'ERROR: pprofile:  decreasing s values ' //         &
     &                     'in spline_cubic'
            ELSE
               WRITE (*,*) 'ERROR: pprofile: unknown error from ' //           &
     &                     'spline_cubic'
            END IF
!  Splines shuld not reach this point without triggering an error.
            CALL EXIT(1)

         CASE (pprofile_akima_spline_type)
            s_use = MIN(this%as(this%maxSplineIndex),                          &
     &                  MAX(s_arg,this%as(1)))
            CALL spline_akima(s_use, pprofile_get_value,                       &
     &                        this%as, this%af,                                &
     &                        this%maxSplineIndex, iflag)
            IF (iflag < 0) THEN
               WRITE (*,*) 'ERROR: pprofile: bad value from ' //               &
     &                     'spline_akima requested'
               CALL EXIT(1)
            END IF

         CASE (pprofile_line_segment_type,                                     &
     &         pprofile_gp_1d_sexp_type,                                       &
     &         pprofile_gp_1d_ln_sexp_type)
            CALL line_seg(s_arg, pprofile_get_value, this%as, this%af,         &
     &                    this%maxSplineIndex)
         
      END SELECT

      CALL profiler_set_stop_time('pprofile_get_value', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the name of the profile type.
!>
!>  Convertes a profile type to a string discription.
!>
!>  @param[in] this  A @ref pprofile_class instance.
!>  @returns The name of the profile type.
!-------------------------------------------------------------------------------
      FUNCTION pprofile_get_p_type_name(this)

!  Declare Arguments
      CHARACTER (len=p_type_len)         :: pprofile_get_p_type_name
      CLASS (pprofile_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%p_type)

         CASE (pprofile_two_power_type)
            pprofile_get_p_type_name = 'two_power'

         CASE (pprofile_two_power_gs_type)
            pprofile_get_p_type_name = 'two_power_gs'

         CASE (pprofile_two_power_r_type)
            pprofile_get_p_type_name = 'two_power_r'

         CASE (pprofile_power_series_type)
            pprofile_get_p_type_name = 'power_series'

         CASE (pprofile_cubic_spline_type)
            pprofile_get_p_type_name = 'cubic_spline'

         CASE (pprofile_akima_spline_type)
            pprofile_get_p_type_name = 'akima_spline'

         CASE (pprofile_line_segment_type)
            pprofile_get_p_type_name = 'line_segment'

         CASE (pprofile_gp_1d_sexp_type)
            pprofile_get_p_type_name = 'sq_exp_1d'

         CASE (pprofile_gp_1d_ln_sexp_type)
            pprofile_get_p_type_name = 'sq_exp_1d_ln'

         CASE DEFAULT
            pprofile_get_p_type_name = 'none'

      END SELECT

      CALL profiler_set_stop_time('pprofile_get_p_type_name',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the guassian process kernel value for the two as indicies.
!>
!>  Indicies are the index of the as array. Only valid for guassian process
!>  profiles.
!>
!>  @param[in] this A @ref pprofile_class instance.
!>  @param[in] i    ith profile position.
!>  @param[in] j    jth profile position.
!>  @returns The name of the profile type.
!-------------------------------------------------------------------------------
      FUNCTION pprofile_get_gp_ij(this, i, j)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                        :: pprofile_get_gp_ij
      CLASS (pprofile_class), INTENT (in) :: this
      INTEGER, INTENT(in)                 :: i
      INTEGER, INTENT(in)                 :: j

!  local variables
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE(this%p_type)

         CASE (pprofile_gp_1d_sexp_type)
            pprofile_get_gp_ij = pprofile_gp_1d_sqexp_k(this,                  &
     &                                                  this%as(i),            &
     &                                                  this%as(j))

         CASE (pprofile_gp_1d_ln_sexp_type)
            pprofile_get_gp_ij = pprofile_gp_1d_sqexp_ln_k(this,               &
     &                                                     this%as(i),         &
     &                                                     this%as(j))

         CASE DEFAULT
            pprofile_get_gp_ij = 0.0

      END SELECT

      CALL profiler_set_stop_time('pprofile_get_gp_ij', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the guassian process kernel value for the point and index.
!>
!>  Index are the index of the as array. Point is a measurement point on the
!>  profile. Only valid for guassian process profiles.
!>
!>  @param[in] this  A @ref pprofile_class instance.
!>  @param[in] p     Point to evaluate the kernel at.
!>  @param[in] i     Profile position index.
!>  @returns The value of the gp kernel function for p and i.
!-------------------------------------------------------------------------------
      FUNCTION pprofile_get_gp_pi(this, p, i)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                        :: pprofile_get_gp_pi
      CLASS (pprofile_class), INTENT (in) :: this
      REAL (rprec), INTENT(in)            :: p
      INTEGER, INTENT(in)                 :: i

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE(this%p_type)

         CASE (pprofile_gp_1d_sexp_type)
            pprofile_get_gp_pi = pprofile_gp_1d_sqexp_k(this, p,               &
     &                                                  this%as(i))

         CASE (pprofile_gp_1d_ln_sexp_type)
            pprofile_get_gp_pi = pprofile_gp_1d_sqexp_ln_k(this, p,            &
     &                                                     this%as(i))

         CASE DEFAULT
            pprofile_get_gp_pi = 0.0

      END SELECT

      CALL profiler_set_stop_time('pprofile_get_gp_pi', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the guassian process kernel value for two points.
!>
!>  Points are a measurement point on the profile. Only valid for guassian
!>  process profiles.
!>
!>  @param[in] this A @ref pprofile_class instance.
!>  @param[in] p1   First point to evaluate the kernel at.
!>  @param[in] p2   SEOND point to evaluate the kernel at.
!>  @returns The value of the gp kernel function for p and i.
!-------------------------------------------------------------------------------
      FUNCTION pprofile_get_gp_pp(this, p1, p2)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                        :: pprofile_get_gp_pp
      CLASS (pprofile_class), INTENT (in) :: this
      REAL (rprec), INTENT(in)            :: p1
      REAL (rprec), INTENT(in)            :: p2

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE(this%p_type)

         CASE (pprofile_gp_1d_sexp_type)
            pprofile_get_gp_pp = pprofile_gp_1d_sqexp_k(this, p1, p2)

         CASE (pprofile_gp_1d_ln_sexp_type)
            pprofile_get_gp_pp = pprofile_gp_1d_sqexp_ln_k(this, p1, p2)

         CASE DEFAULT
            pprofile_get_gp_pp = 0.0

      END SELECT

      CALL profiler_set_stop_time('pprofile_get_gp_pp', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the number of hyper parameters for guassian process kernel.
!>
!>  Hyper parameters must be set to maximise the log of the evidence. If this
!>  profile is not a guassian process return zero.
!>
!>  @param[in] this A @ref pprofile_class instance.
!>  @returns Number of hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION pprofile_get_gp_num_hyper_param(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: pprofile_get_gp_num_hyper_param
      CLASS (pprofile_class), INTENT (in) :: this

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE(this%p_type)

         CASE (pprofile_gp_1d_sexp_type, pprofile_gp_1d_ln_sexp_type)
            pprofile_get_gp_num_hyper_param = 2

         CASE DEFAULT
            pprofile_get_gp_num_hyper_param = 0

      END SELECT

      CALL profiler_set_stop_time('pprofile_get_gp_num_hyper_param',           &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Write out the profile to an output file.
!>
!>  This formats and writes out information about a parameterized profile.
!>
!>  @param[in] this A @ref pprofile_class instance.
!>  @param[in] id   Identification of the profle.
!>  @param[in] iou  Input/output unit of the output file.
!-------------------------------------------------------------------------------
      SUBROUTINE pprofile_write(this, id, iou)
      IMPLICIT NONE

!  Declare Arguments
      CLASS (pprofile_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)      :: id
      INTEGER, INTENT(in)                :: iou

!  local variables
      INTEGER                           :: i
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Actual write. Could do a select-case on p_type.
      WRITE(iou,1100) id
      WRITE(iou,1200) TRIM(this%get_p_type_name())
      
      SELECT CASE(this%p_type)
         
         CASE (pprofile_two_power_type,                                        &
     &         pprofile_two_power_r_type)
            WRITE(iou,1210)
            WRITE(iou,1300) this%b(0:3)

         CASE (pprofile_power_series_type,                                     &
     &         pprofile_two_power_gs_type)
            WRITE(iou,1220)
            WRITE(iou,1300) this%b(ilb_b:iub_b)
            
         CASE (pprofile_cubic_spline_type,                                     &
     &         pprofile_akima_spline_type,                                     &
     &         pprofile_line_segment_type)
            WRITE(iou,1230)
            WRITE(iou,1231) (i, this%as(i), this%af(i),                        &
     &                       i = 1, this%maxSplineIndex)

         CASE (pprofile_gp_1d_sexp_type,                                       &
     &         pprofile_gp_1d_ln_sexp_type)
            WRITE(iou,1300) this%b(0:1)
            WRITE(iou,1231) (i, this%as(i), this%af(i),                        &
     &                       i = 1, this%maxSplineIndex)

      END SELECT

      CALL profiler_set_stop_time('pprofile_write', start_time)

1100  FORMAT(/' Parameterized Profile Write: id = ',a)
1200  FORMAT('   pp_type = ',a)
1210  FORMAT(' b_0 + Th(s)Th(1-s)(b_1 (1 - s ** b_2) ** b_3).',                &
     &       '   b(0:3) = ')
1220  FORMAT(' Th(s)Th(1-s)[Sum_0_n b_i s** i].   b(0:n) = ')
1230  FORMAT(' i       as(i)           af(i)')
1231  FORMAT(1x,i3,2x,es15.8,2x,es15.8)
1300  FORMAT(4(2x,es15.8))

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Save the internal state of the profile.
!>
!>  Saves the profile af array if using the guassian process.
!>
!>  @param[inout] this A @ref pprofile_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE pprofile_save_state(this)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (pprofile_class), INTENT(inout) :: this

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%p_type)

         CASE (pprofile_gp_1d_sexp_type,                                       &
     &         pprofile_gp_1d_ln_sexp_type)
            this%cache = this%af
            this%cache_hyper = this%b(0:1)

      END SELECT

      CALL profiler_set_stop_time('pprofile_save_state', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Reset the internal state of the profile.
!>
!>  Resets the profile af array if using the guassian process.
!>
!>  @param[inout] this A @ref pprofile_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE pprofile_reset_state(this)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (pprofile_class), INTENT(inout) :: this

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%p_type)

         CASE (pprofile_gp_1d_sexp_type,                                       &
     &         pprofile_gp_1d_ln_sexp_type)
            this%af = this%cache
            this%b(0:1) = this%cache_hyper

      END SELECT

      CALL profiler_set_stop_time('pprofile_reset_state', start_time)

      END SUBROUTINE

!*******************************************************************************
!  PRIVATE
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Finds the index of the maximum radial position.
!>
!>  This assumes that the radials positions are specifed in increasing order.
!>
!>  @param[in] s_array Array of radial positions
!>  @returns Index of the last position.
!-------------------------------------------------------------------------------
      FUNCTION findMaxIndex(s_array)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER                               :: findMaxIndex
      REAL(rprec), DIMENSION(:), INTENT(in) :: s_array

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      findMaxIndex = MAXLOC(s_array, dim=1)

      CALL profiler_set_stop_time('findMaxIndex', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Evaluate the one dimensional squared exponential kernel.
!>
!>  The squared exponential kernel is defined as
!>
!>    b(0)^2*exp(-(x1 - x2)^2/2*b(1)^2)                                      (1)
!>
!>  where b(0) and b(1) are the hyper parameters.
!>
!>  @param[in] this A @ref pprofile_class instance.
!>  @param[in] p1   First position.
!>  @param[in] p2   Second position.
!>  @returns Value of the kernel.
!-------------------------------------------------------------------------------
      PURE FUNCTION pprofile_gp_1d_sqexp_k(this, p1, p2)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: pprofile_gp_1d_sqexp_k
      TYPE (pprofile_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)          :: p1
      REAL (rprec), INTENT(in)          :: p2

!  Start of executable code
      pprofile_gp_1d_sqexp_k =                                                 &
     &   this%b(0)**2*EXP(-(p1 - p2)**2/(2.0*this%b(1)**2))

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Evaluate the one dimensional squared exponential kernel
!>
!>  This kernel is mathmatically the same as the squared exp kernel, but here
!>  we treat the amplitude [ b(0) ] differently. The value of b(0) used here is
!>  equal to the log of value of b(0) used in the 1d_sqexp. I'm testing this
!>  formulation to see if it helps convergence.
!>
!>    exp(2*b(0) - (x1- x2)**2)/2*b(1)**2)                                   (1)
!>
!>  @param[in] this A @ref pprofile_class instance.
!>  @param[in] p1 first position
!>  @param[in] p2 second position
!>  @param[in] b  Array of coefficients
!>  @returns Value of the kernel
!-------------------------------------------------------------------------------
      PURE FUNCTION pprofile_gp_1d_sqexp_ln_k(this, p1, p2)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: pprofile_gp_1d_sqexp_ln_k
      TYPE (pprofile_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)          :: p1
      REAL (rprec), INTENT(in)          :: p2

!  Start of executable code
      pprofile_gp_1d_sqexp_ln_k =                                              &
     &   EXP(2.0*this%b(0) - (p1 - p2)**2/(2.0*this%b(1)**2))

      END FUNCTION

      END MODULE

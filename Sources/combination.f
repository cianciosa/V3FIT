!*******************************************************************************
!>  @file combination.f
!>  @brief Contains module @ref combination.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref combination_class.
!>  @par
!>  combination Super Class:
!>  @ref signal
!*******************************************************************************
      MODULE combination
      USE stel_kinds, only: rprec
      USE data_parameters
      USE signal

!*******************************************************************************
!  combination module parameters
!*******************************************************************************
!>  Maximum length of the combination type descritpion string.
      INTEGER, PARAMETER :: combination_type_length = 4

!>  Type descriptor for combination type no type.
      INTEGER, PARAMETER :: combination_none = -1
!>  Type descriptor for combination type sum.
      INTEGER, PARAMETER :: combination_sum  = 0
!>  Type descriptor for combination type max.
      INTEGER, PARAMETER :: combination_max  = 1
!>  Type descriptor for combination type min.
      INTEGER, PARAMETER :: combination_min  = 2
!>  Type descriptor for combination type weighted average
      INTEGER, PARAMETER :: combination_wavg = 3

!-------------------------------------------------------------------------------
!>  Base class representing a combination signal.
!>  @par Super Class:
!>  @ref signal
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(signal_class) :: combination_class
!>  Type descirptor of the cobination type.
!>  @par Possible values are:
!>  * @ref combination_none
!>  * @ref combination_sum
!>  * @ref combination_max
!>  * @ref combination_min
         INTEGER :: type = combination_none
!>  Array of @ref signal_class pointer presenting the signals to combine.
         TYPE (signal_pointer), DIMENSION(:), POINTER :: signals
!>  Array of coeffients to for the combined signals.
         REAL(rprec), DIMENSION(:), POINTER           :: a
!>  Index of the average weighting factor.
         INTEGER                                      :: wgt_index
      CONTAINS
         PROCEDURE                                    ::                       &
     &      set_signal => combination_set_signal
         PROCEDURE                                    ::                       &
     &      set_weight => combination_set_weight
         PROCEDURE                                    ::                       &
     &      get_modeled_signal_last => combination_get_modeled_signal
         PROCEDURE                                    ::                       &
     &      get_type => combination_get_type
         PROCEDURE                                    ::                       &
     &      get_header => combination_get_header
         PROCEDURE                                    ::                       &
     &      write_auxiliary => combination_write_auxiliary
         FINAL                                        ::                       &
     &      combination_destruct
      END TYPE combination_class

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for thomson te constructor.
!-------------------------------------------------------------------------------
      INTERFACE combination_class
         MODULE PROCEDURE combination_construct
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref combination_class.
!>
!>  Allocates memory and initializes a @ref combination_class object. This just
!>  allocates the @ref combination_class::signals and @ref combination_class::a
!>  array. The indices of these arrays are set by @ref combination_set_signal
!>
!>  @param[in] n_signals        Number of signals in this combination.
!>  @param[in] combination_type The method to combine the signals. Possible
!>                              values are @ref combination_sum,
!>                              @ref combination_max, @ ref combination_min and
!>                              @ref combination_wavg
!>  @param[in] wgt_index        Index number of the average weighting.
!>  @returns A pointer to a constructed @ref combination_class object.
!-------------------------------------------------------------------------------
      FUNCTION combination_construct(n_signals, combination_type,              &
     &                               wgt_index)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (combination_class), POINTER :: combination_construct
      INTEGER, INTENT(in)               :: n_signals
      CHARACTER (len=combination_type_length), INTENT(in) ::                   &
     &   combination_type
      INTEGER, INTENT(IN)               :: wgt_index

!  Local Variables
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(combination_construct)

      ALLOCATE(combination_construct%signals(n_signals))
      ALLOCATE(combination_construct%a(n_signals))

      combination_construct%wgt_index = wgt_index

      SELECT CASE (combination_type)
         CASE ('sum')
            combination_construct%type = combination_sum

         CASE ('max')
            combination_construct%type = combination_max

         CASE ('min')
            combination_construct%type = combination_min

         CASE ('wavg')
            combination_construct%type = combination_wavg
            IF (n_signals .gt. 2) THEN
               WRITE (*,1000)
            END IF

      END SELECT

      CALL profiler_set_stop_time('combination_construct', start_time)

1000  FORMAT('More than two signals specified for weighted average.            &
     &       Only using the first two.')

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref combination_class object.
!>
!>  Deallocates memory and uninitializes a @ref combination_class object.
!>
!>  @param[inout] this A @ref combination_class instance.
!>  @note The destructors of the signals in the signal array are not called her
!>  because the combination signals do not own the memory for those signals.
!-------------------------------------------------------------------------------
      SUBROUTINE combination_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (combination_class), INTENT(inout) :: this

!  Local Variables
      INTEGER                                 :: i

!  Start of executable code

!  Null all pointers in the signals array. Do not deallocate the signals in the
!  array since this object is not the owner.
      IF (ASSOCIATED(this%signals)) THEN
         DO i = 1, SIZE(this%signals)
            this%signals(i)%p => null()
         END DO
         this%signals => null()
      END IF
      IF (ASSOCIATED(this%a)) THEN
         DEALLOCATE(this%a)
         this%a => null()
      END IF

      this%type = combination_none

      END SUBROUTINE

!*******************************************************************************
!  SETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Set the object and coefficient for an index.
!>
!>  @param[inout] this   A @ref combination_class instance.
!>  @param[in]    signal A @ref signal_class instance.
!>  @param[in]    a      A coefficient for the signal
!>  @param[in]    index  The index to place the combined @ref signal.
!>  @note Only signals that were created before this combination can be added.
!>  Otherwise there it will creat an infinitely recursive loop in
!>  @ref combination_get_modeled_signal
!-------------------------------------------------------------------------------
      SUBROUTINE combination_set_signal(this, signal, a, index)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (combination_class), INTENT(inout) :: this
      CLASS (signal_class), POINTER            :: signal
      INTEGER, INTENT(in)                      :: index
      REAL (rprec), INTENT(in)                 :: a

!  local Variables
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      this%a(index) = a
      this%signals(index)%p => signal

      CALL profiler_set_stop_time('combination_set_signal', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Sets the weight factors from from the weight index.
!>
!>  Only assumed valid when the combination type is weighted average. This also
!>  assumes that the average of only two signals.
!>
!>  @param[inout] this  A @ref combination_class instance.
!>  @param[in]  a_model A @ref model instance.
!-------------------------------------------------------------------------------
      SUBROUTINE combination_set_weight(this, a_model)
      USE model

      IMPLICIT NONE

!  Declare Arguments
      CLASS (combination_class), INTENT(inout) :: this
      TYPE (model_class), INTENT(in)           :: a_model

!  local Variables
      REAL (rprec)                             :: start_time

      this%a(1) = a_model%coosig_wgts(this%wgt_index)
      this%a(2) = 1.0 - this%a(1)

      CALL profiler_set_stop_time('combination_set_signal', start_time)

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled signal.
!>
!>  This method combines the signals pointed in the
!>  @ref combination_class::signals array.
!>
!>  @param[inout] this       A @ref combination_class instance.
!>  @param[in]    a_model    A @ref model instance.
!>  @param[out]   sigma      The modeled sigma.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION combination_get_modeled_signal(this, a_model, sigma,            &
     &                                        last_value)
      USE model

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: combination_get_modeled_signal
      CLASS (combination_class), INTENT(inout) :: this
      TYPE (model_class), POINTER              :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out)  :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)   :: last_value

!  Local Variables
      CLASS (signal_class), POINTER            :: temp_signal
      INTEGER                                  :: i
      REAL (rprec), DIMENSION(4)               :: sigma_local
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  NOTE: Using the syntax, this%signals(i)%p%get_modeled_signal(...) Causes an
!        internal compiler error in gfortran. Use the explicit version.
      temp_signal => this%signals(1)%p

      combination_get_modeled_signal = this%a(1)*                              &
     &   temp_signal%get_modeled_signal(a_model, sigma, .true.,                &
     &                                  last_value)

      SELECT CASE(this%type)
         CASE (combination_sum)
            DO i = 2, SIZE(this%signals)
               temp_signal => this%signals(i)%p
               combination_get_modeled_signal =                                &
     &            combination_get_modeled_signal +                             &
     &            temp_signal%get_modeled_signal(a_model,                      &
     &                                           sigma_local,                  &
     &                                           .true.,                       &
     &                                           last_value) *                 &
     &            this%a(i)
               sigma = sigma + sigma_local
            END DO

         CASE (combination_max)
            DO i = 2, SIZE(this%signals)
               temp_signal => this%signals(i)%p
               combination_get_modeled_signal = MAX(                           &
     &            combination_get_modeled_signal,                              &
     &            temp_signal%get_modeled_signal(a_model, sigma_local,         &
     &                                           .true., last_value) *         &
     &            this%a(i))
               sigma = MAX(sigma, sigma_local)
            END DO

         CASE (combination_min)
            DO i = 2, SIZE(this%signals)
               temp_signal => this%signals(i)%p
               combination_get_modeled_signal = MIN(                           &
     &            combination_get_modeled_signal,                              &
     &            temp_signal%get_modeled_signal(a_model, sigma_local,         &
     &                                           .true., sigma_local) *        &
     &            this%a(i))
               sigma = MIN(sigma, sigma_local)
            END DO

!  ECH I assume that the combination wavg is composed of only two signals and
!  the there is only 1 coosig wgt. Second do I need to account for the
!  uncertainity in the wgt_ids when calculaing the sigma value.
         CASE (combination_wavg)
            CALL combination_set_weight(this, a_model)
            temp_signal => this%signals(2)%p
            combination_get_modeled_signal =                                   &
     &         combination_get_modeled_signal +                                &
     &         temp_signal%get_modeled_signal(a_model, sigma_local,            &
     &                                        .true., last_value) *            &
     &         this%a(2)

            sigma = sigma + this%a(2)*sigma_local

!  It doesn't always make sense to average the values in model signal 2-4.
            combination_get_modeled_signal(2:4) = 0.0

      END SELECT

      CALL this%scale_and_offset(a_model,                                      &
     &                           combination_get_modeled_signal(1))

      CALL profiler_set_stop_time('combination_get_modeled_signal',            &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the combination type.
!>
!>  Returns a description of the combination type for use when writting output
!>  files.
!>
!>  @param[in] this A @ref combination_class instance.
!>  @returns A string describing the combination type.
!-------------------------------------------------------------------------------
      FUNCTION combination_get_type(this)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: combination_get_type
      CLASS (combination_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)
         CASE (combination_sum)
            combination_get_type = 'coosig sum'

         CASE (combination_max)
            combination_get_type = 'coosig max'

         CASE (combination_min)
            combination_get_type = 'coosig min'

         CASE (combination_wavg)
            combination_get_type = 'coosig wavg'

      END SELECT

      CALL profiler_set_stop_time('combination_get_type', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the model and model sigma array indices.
!>
!>  Returns a description of the array indices for use when writting output
!>  files.
!>
!>  @param[inout]
!>  @param[in]    this   A @ref combination_class instance.
!>  @param[inout] header Buffer arrays to write header strings to.
!>  @returns A string describing the model and model sigma array indices.
!-------------------------------------------------------------------------------
      SUBROUTINE combination_get_header(this, header)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (combination_class), INTENT(in) :: this
      CHARACTER (len=data_name_length), DIMENSION(7), INTENT(inout) ::         &
     &   header

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      header(1) = 'model(2)'
      header(2) = 'model(3)'
      header(3) = 'model(4)'
      header(4) = 'model_sig(1)'
      header(5) = 'model_sig(2)'
      header(6) = 'model_sig(3)'
      header(7) = 'model_sig(4)'

      CALL profiler_set_stop_time('combination_get_header', start_time)

      END SUBROUTINE

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Write out auxiliary signal information to an output file.
!>
!>  Writes out the s_name and coefficient of the combined signals.
!>
!>  @param[in] this    A @ref signal_class instance.
!>  @param[in] iou     A input/output representing the file to write to.
!>  @param[in] index   A index of a signal.
!>  @param[in] a_model The equilibrium model.
!-------------------------------------------------------------------------------
      SUBROUTINE combination_write_auxiliary(this, iou, index, a_model)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (combination_class), INTENT(in) :: this
      INTEGER, INTENT(in)                   :: iou
      INTEGER, INTENT(in)                   :: index
      TYPE (model_class), INTENT(in)        :: a_model

!  local variables
      INTEGER                               :: i
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (iou,*)
      WRITE (iou,1000) index, this%get_type()
      IF (this%type .eq. combination_wavg) THEN
         WRITE (iou,1001) this%wgt_index
      END IF
      WRITE (iou,1002)

      DO i = 1, SIZE(this%signals)
         WRITE (iou,1003) i, this%signals(i)%p%s_name, this%a(i)
      END DO

      CALL profiler_set_stop_time('combination_write_auxiliary',               &
     &                            start_time)

1000  FORMAT('Signal',1x,i4,1x,                                                &
     &       'is a combination of other signals, type: ',a)
1001  FORMAT('Weighted Average Index: ',i4)
1002  FORMAT('term #',2x,'s_name',16x,'Coefficient')
1003  FORMAT(2x,i4,2x,a20,2x,es12.5)

      END SUBROUTINE
      END MODULE

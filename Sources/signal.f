!*******************************************************************************
!>  @file signal.f
!>  @brief Contains module @ref signal.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref signal_class.
!>  @par
!>  signal Sub Classes:
!>  @ref magnetic, @ref sxrem, @ref intpol, @ref thomson, @ref extcurz,
!>  @ref mse, @ref ece, @ref limiter, @ref prior_gaussian, and @ref feedback
!*******************************************************************************
      MODULE signal
      USE stel_kinds, only: rprec
      USE data_parameters
      USE model

      IMPLICIT NONE
!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) signal base class
!  2) signal pointer type
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a signal.
!>  @par Sub Classes:
!>  @ref magnetic, @ref sxrem, @ref intpol, @ref thomson, @ref extcurz,
!>  @ref mse, @ref ece, @ref limiter, @ref prior_gaussian, and @ref feedback,
!>  @ref combination_class
!-------------------------------------------------------------------------------
      TYPE :: signal_class
!>  Short name of the signal.
         CHARACTER (len=data_short_name_length) :: s_name
!>  Long name of the signal.
         CHARACTER (len=data_name_length)       :: l_name
!>  Physical units the signal measures.
         CHARACTER (len=data_short_name_length) :: units
!>  Eperimentally measured signal value.
         REAL (rprec)                           :: observed
!>  Eperimentally measured signal uncertainty.
         REAL (rprec)                           :: observed_sigma
!>  Weighting parameter of the signal.
         REAL (rprec)                           :: weight
!>  Cached value of the modeled signal.
         REAL (rprec), DIMENSION(4)             :: modeled
!>  Cached value of the modeled sigma.
         REAL (rprec), DIMENSION(4)             :: modeled_sigma
!>  Scale factor index.
         INTEGER                                :: scale_index
!>  Offset factor index.
         INTEGER                                :: offset_index
      CONTAINS
         PROCEDURE                              ::                             &
     &      get_modeled_signal_cache => signal_get_modeled_signal_cache
         PROCEDURE                              ::                             &
     &      get_modeled_signal_last => signal_get_modeled_signal_last
         GENERIC                                ::                             &
     &      get_modeled_signal => get_modeled_signal_cache,                    &
     &                            get_modeled_signal_last
         PROCEDURE                              ::                             &
     &      get_observed_signal => signal_get_observed_signal
         PROCEDURE                              ::                             &
     &      get_g2 => signal_get_g2
         PROCEDURE                              ::                             &
     &      get_e => signal_get_e
         PROCEDURE                              ::                             &
     &      get_sigma2 => signal_get_sigma2
         PROCEDURE                              ::                             &
     &      get_type => signal_get_type
         PROCEDURE                              ::                             &
     &      get_header => signal_get_header
         PROCEDURE                              ::                             &
     &      get_gp_i => signal_get_gp_i
         PROCEDURE                              ::                             &
     &      get_gp_s => signal_get_gp_s
         PROCEDURE                              ::                             &
     &      get_gp_x => signal_get_gp_x
         GENERIC                                ::                             &
     &      get_gp => get_gp_i, get_gp_s, get_gp_x
         PROCEDURE                              ::                             &
     &      scale_and_offset => signal_scale_and_offset
         PROCEDURE                              ::                             &
     &      sync_child => signal_sync_child
         PROCEDURE                              ::                             &
     &      write_header => signal_write_header
         PROCEDURE                              :: write => signal_write
         PROCEDURE                              ::                             &
     &      write_auxiliary => signals_write_auxiliary
         PROCEDURE                              ::                             &
     &      write_step_data => signal_write_step_data
         FINAL                                  :: signal_destruct
      END TYPE signal_class

!-------------------------------------------------------------------------------
!>  Pointer to a signal object. Used for creating arrays of signal pointers.
!>  This is needed because fortran does not allow arrays of pointers directly.
!-------------------------------------------------------------------------------
      TYPE signal_pointer
!>  Pointer to a @ref signal_class. Used for building arrays of
!>  @ref signal_class objects.
         CLASS (signal_class), POINTER :: p => null()
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the construction of @ref signal_class types using
!>  @ref signal_construct_new or @ref signal_construct_diagnostic_netcdf
!-------------------------------------------------------------------------------
      INTERFACE signal_construct
         MODULE PROCEDURE signal_construct_new,                                &
     &                    signal_construct_diagnostic_netcdf
      END INTERFACE

      CONTAINS 
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct new @ref signal_class object.
!>
!>  Allocates memory and initializes a @ref signal_class object.
!>
!>  @param[inout] this     An instance of a @ref signal_class.
!>  @param[in]    s_name   The short name of the signal.
!>  @param[in]    l_name   The short name of the signal.
!>  @param[in]    units    The units of the signal.
!>  @param[in]    observed Observed value of the signal.
!>  @param[in]    sigma    Observed sigma of the signal.
!>  @param[in]    weight   Weight factor of the signal.
!>  @param[in]    s_index  Index of the model signal scale factor.
!>  @param[in]    o_index  Index of the model signal offset factor.
!-------------------------------------------------------------------------------
      SUBROUTINE signal_construct_new(this, s_name, l_name, units,             &
     &                                observed, sigma, weight, s_index,        &
     &                                o_index)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (signal_class), INTENT(inout) :: this
      CHARACTER(len=*), INTENT(in)        :: s_name
      CHARACTER(len=*), INTENT(in)        :: l_name
      CHARACTER(len=*), INTENT(in)        :: units
      REAL(rprec), INTENT(in)             :: observed
      REAL(rprec), INTENT(in)             :: sigma
      REAL(rprec), INTENT(in)             :: weight
      INTEGER, INTENT(in)                 :: s_index
      INTEGER, INTENT(in)                 :: o_index

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Warn the user that the signal is missing a non zero sigma.
      IF (sigma .eq. 0.0) THEN
         WRITE (*,1000) TRIM(s_name)
      END IF

      this%s_name = s_name
      this%l_name = l_name
      this%units = units
      this%observed = observed
      this%observed_sigma = sigma
      this%weight = weight
      this%modeled = 0.0
      this%modeled_sigma = 0.0
      this%scale_index = s_index
      this%offset_index = o_index

      CALL profiler_set_stop_time('signal_construct_new', start_time)

1000  FORMAT('Warning: sigma for ',a,' is zero.')

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref signal_class containing a @ref diagnostic object.
!>
!>  Reads the s_name, l_name and units from a netcdf file. Used for constructing
!>  the @ref magnetic objects.
!>
!>  @param[inout] signal_class An instance of a @ref signal_class.
!>  @param[in]    mdsig_iou    NetCDF id of the opened mdsig file.
!>  @param[in]    observed     Observed value of the signal.
!>  @param[in]    sigma        Observed sigma of the signal.
!>  @param[in]    weight       Weight factor of the signal.
!>  @param[in]    s_index      Index of the model signal scale factor.
!>  @param[in]    o_index      Index of the model signal offset factor.
!>  @returns      Apointer to a constructed @ref signal_class object.
!-------------------------------------------------------------------------------
      SUBROUTINE signal_construct_diagnostic_netcdf(this, mdsig_iou,           &
     &                                              observed, sigma,           &
     &                                              weight, s_index,           &
     &                                              o_index)
      USE ezcdf

      IMPLICIT NONE

!  Declare Arguments
      CLASS (signal_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                 :: mdsig_iou
      REAL(rprec), INTENT(in)             :: observed
      REAL(rprec), INTENT(in)             :: sigma
      REAL(rprec), INTENT(in)             :: weight
      INTEGER, INTENT(in)                 :: s_index
      INTEGER, INTENT(in)                 :: o_index

!  local variables
      CHARACTER (len=data_short_name_length) :: s_name
      CHARACTER (len=data_name_length)       :: l_name
      CHARACTER (len=data_short_name_length) :: units
      INTEGER                                :: varid
      INTEGER                                :: status
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      status = nf90_inq_varid(mdsig_iou, 'diagnostic_desc_s_name',             &
     &                        varid)
      status = nf90_get_var(mdsig_iou, varid, s_name)
      status = nf90_inq_varid(mdsig_iou, 'diagnostic_desc_l_name',             &
     &                        varid)
      status = nf90_get_var(mdsig_iou, varid, l_name)
      status = nf90_inq_varid(mdsig_iou, 'diagnostic_desc_units',              &
     &                        varid)
      status = nf90_get_var(mdsig_iou, varid, units)

      CALL signal_construct(this, s_name, l_name, units, observed,             &
     &                      sigma, weight, s_index, o_index)

      CALL profiler_set_stop_time('signal_construct_diagnostic_netcdf',        &
     &                            start_time)

      END SUBROUTINE

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref signal_class object.
!>
!>  Deallocates memory and uninitializes a @ref signal_class object.
!>
!>  @param[inout] this A @ref signal_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE signal_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (signal_class), INTENT(inout) :: this

!  Start of executable code
      this%s_name = ''
      this%l_name = ''
      this%units = ''
      this%observed = 0.0
      this%observed_sigma = 0.0
      this%weight = 0.0
      this%modeled = 0.0
      this%modeled_sigma = 0.0

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled signal.
!>
!>  This method is checks if the cached vales should be used first otherwise
!>  calls the subclass method.
!>
!>  @param[inout] this       A @ref diagnostic_class instance.
!>  @param[in]    a_model    A @ref model instance.
!>  @param[out]   sigma      The modeled sigma.
!>  @param[in]    use_cache  If false calculate the signal and sigma and write
!>                           them to the cache. Otherwise read from the cache
!>                           without recalculating the signal.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION signal_get_modeled_signal_cache(this, a_model, sigma,           &
     &                                         use_cache, last_value)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: signal_get_modeled_signal_cache
      CLASS (signal_class), INTENT(inout)     :: this
      CLASS (model_class), POINTER            :: a_model
      REAL (rprec), DIMENSION(:), INTENT(out) :: sigma
      LOGICAL, INTENT(in)                     :: use_cache
      REAL (rprec), DIMENSION(4), INTENT(in)  :: last_value

!  local variables
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (.not.use_cache) THEN
         this%modeled = this%get_modeled_signal(a_model,                       &
     &                                          this%modeled_sigma,            &
     &                                          last_value)
      END IF

      sigma = this%modeled_sigma
      signal_get_modeled_signal_cache = this%modeled

      CALL profiler_set_stop_time('signal_get_modeled_signal_cache',           &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled signal.
!>
!>  This method is meant to be overwritten by a subclass method. As a result,
!>  returns the get_modeled_signal method of the subclass instance
!>  @ref signal_class was constructed with unless the signal is read from the
!>  cached value. Read from the cached value when the signal doesn't need to be
!>  recalulated. If the signal is to be recalculated, write these to the cache.
!>
!>  @see magnetic::magnetic_get_modeled_signal
!>  @see sxrem::sxrem_get_modeled_signal
!>  @see intpol::intpol_get_modeled_signal
!>  @see thomson::thomson_te_get_modeled_signal
!>  @see thomson::thomson_p_get_modeled_signal
!>  @see thomson::thomson_ne_get_modeled_signal
!>  @see extcurz::extcurz_get_modeled_signal
!>  @see mse::mse_get_modeled_signal
!>  @see ece::ece_get_modeled_signal
!>  @see limiter::limiter_get_modeled_signal
!>  @see prior_guassian::prior_guassian_get_modeled_signal
!>  @see sxrem_ratio::sxrem_ratio_get_modeled_signal
!>  @see combination::combination_get_modeled_signal
!>
!>  @param[inout] this       A @ref signal_class instance.
!>  @param[in]    a_model    A @ref model instance.
!>  @param[out]   sigma      The modeled sigma.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION signal_get_modeled_signal_last(this, a_model, sigma,            &
     &                                        last_value)
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: signal_get_modeled_signal_last
      CLASS (signal_class), INTENT(inout)     :: this
      CLASS (model_class), POINTER            :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out) :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)  :: last_value

!  local variables
      REAL (rprec)                            :: start_time

!  Start of executable code
      CALL assert(.false., 'get_modeled_signal_last not over written' //       &
     &                     ' for ' // this%get_type())

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Calculates the observed signal.
!>
!>  This method is meant to be overwritten by a subclass method. As a result,
!>  returns the get_modeled_signal method of the subclass instance
!>  @ref signal_class was constructed with. All signals expect feed back signals
!>  use a fixed observed value.
!>
!>  @see feedback::feedback_get_modeled_signal
!>
!>  @param[in] this      A @ref diagnostic_class instance.
!>  @param[in] a_model   A @ref model instance.
!>  @returns The observed value.
!-------------------------------------------------------------------------------
      FUNCTION signal_get_observed_signal(this, a_model)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: signal_get_observed_signal
      CLASS (signal_class), INTENT(in) :: this
      CLASS (model_class), INTENT(in)  :: a_model

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      signal_get_observed_signal = this%observed

      CALL profiler_set_stop_time('signal_get_observed_signal',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Calculates the g^2 contribution of a signal.
!>
!>  Gets the g^2 contribution of an individual signal.
!>  g^2 = W*(O - M)^2/(sigma_o^2 + sigma_m^2) = e*e
!>
!>  @param[inout] this       A @ref diagnostic_class instance.
!>  @param[inout] a_model    A @ref model instance.
!>  @param[in]    use_cache  If the signals have been already calculated this
!>                           should be false. Otherwise set to true.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The g^2 value.
!-------------------------------------------------------------------------------
      FUNCTION signal_get_g2(this, a_model, use_cache, last_value)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: signal_get_g2
      CLASS (signal_class), INTENT(inout)    :: this
      CLASS (model_class), POINTER           :: a_model
      LOGICAL, INTENT(in)                    :: use_cache
      REAL (rprec), DIMENSION(4), INTENT(in) :: last_value

!  local Variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  g^2 = W*(O - M)^2/(sigma_o^2 + sigma_m^2) = e*e
      signal_get_g2 = this%get_e(a_model, use_cache, last_value)**2.0

      CALL profiler_set_stop_time('signal_get_g2', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Calculates the e contribution of a signal.
!>
!>  Gets the e contribution of an individual signal.
!>  e = SQRT(W)*(O - M)/SQRT(sigma_o^2 + sigma_m^2)
!>
!>  @param[inout] this       A @ref diagnostic_class instance.
!>  @param[inout] a_model    A @ref model instance.
!>  @param[in]    use_cache  If the signals have been already calculated this
!>                           should be false. Otherwise set to true.
!>  @param[in]    last_value Last good value in case the signal did not change.
!>  @returns The e value.
!-------------------------------------------------------------------------------
      FUNCTION signal_get_e(this, a_model, use_cache, last_value)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: signal_get_e
      CLASS (signal_class), INTENT(inout)    :: this
      CLASS (model_class), POINTER           :: a_model
      LOGICAL, INTENT(in)                    :: use_cache
      REAL (rprec), DIMENSION(4), INTENT(in) :: last_value

!  local Variables
      REAL (rprec), DIMENSION(4)             :: modeled_signal
      REAL (rprec), DIMENSION(4)             :: modeled_sigma
      REAL (rprec)                           :: observed_signal
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      observed_signal = this%get_observed_signal(a_model)

      modeled_signal = this%get_modeled_signal(a_model, modeled_sigma,         &
     &                                         use_cache, last_value)

!  e = SQRT(W)*(O - M)/SQRT(sigma_o^2 + sigma_m^2)
!  Users are can be lax when specifiying the sigma for signals weighted to zero.
!  Avoid a possible divide by zero by error checking the weight first. If the
!  weight is zero, set the error vector explicitly to zero.
      IF (this%weight .eq. 0.0) THEN
         signal_get_e = 0.0
      ELSE
         signal_get_e = (modeled_signal(1) - observed_signal)                  &
     &                * SQRT(this%weight/this%get_sigma2())
      END IF

      CALL profiler_set_stop_time('signal_get_e', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Calculates the total sigma^2 of a signal.
!>
!>  Gets the total sigma^2 of an individual signal.
!>  sigma^2 = sigma_o^2 + sigma_m^2
!>
!>  @param[in] this A @ref diagnostic_class instance.
!>  @returns The e value.
!>  @note this assumes that modeled sigma value has already been cached by a
!>  call to @ref signal_get_modeled_signal
!-------------------------------------------------------------------------------
      FUNCTION signal_get_sigma2(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                     :: signal_get_sigma2
      CLASS (signal_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  sigma_o^2 + sigma_m^2)
      signal_get_sigma2 = this%observed_sigma**2.0                             &
     &                  + this%modeled_sigma(1)**2.0

      CALL profiler_set_stop_time('signal_get_sigma2', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the signal type.
!>
!>  This method is meant to be overwritten by a subclass method. As a result,
!>  returns the get_type method of the subclass instance
!>  @ref signal_class was constructed with.
!>
!>  @see magnetic::magnetic_get_type
!>  @see sxrem::sxrem_get_type
!>  @see intpol::intpol_get_type
!>  @see thomson::thomson_get_type
!>  @see extcurz::extcurz_get_type
!>  @see mse::mse_get_type
!>  @see ece::ece_get_type
!>  @see limiter::limiter_get_type
!>  @see prior_guassian::prior_guassian_get_type
!>  @see sxrem_ratio::sxrem_ratio_get_type
!>  @see combination::combination_get_type
!>
!>  @param[in] this A @ref signal_class instance.
!>  @returns A string describing the signal type.
!-------------------------------------------------------------------------------
      FUNCTION signal_get_type(this)
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: signal_get_type
      CLASS (signal_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      CALL assert(.false., 'signal_get_type not over written for ' //          &
     &                     this%s_name)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the model and model sigma array indices.
!>
!>  This method is meant to be overwritten by a subclass method. As a result,
!>  returns the get_header method of the subclass instance
!>  @ref signal_class was constructed with.
!>
!>  @see magnetic::magnetic_get_header
!>  @see mse::mse_get_header
!>  @see ece::ece_get_header
!>  @see limiter::limiter_get_header
!>  @see feedback::feedback_get_header
!>  @see combination_get_header
!>  @see gaussp_get_header
!>
!>  @param[in]    this   A @ref signal_class instance.
!>  @param[inout] header Buffer arrays to write header strings to.
!-------------------------------------------------------------------------------
      SUBROUTINE signal_get_header(this, header)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (signal_class), INTENT(in) :: this
      CHARACTER (len=data_name_length), DIMENSION(7), INTENT(inout) ::         &
     &   header

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      header(1:3) = 'N/A'
      header(4) = 'model_sig(1)'
      header(5) = 'model_sig(2)'
      header(6) = 'model_sig(3)'
      header(7) = 'model_sig(4)'

      CALL profiler_set_stop_time('signal_get_header', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a signal and a position.
!>
!>  This method is meant to be overwritten by a subclass method. As a result,
!>  returns the get_gp_i method of the subclass instance @ref signal_class was
!>  constructed with.
!>
!>  @see sxrem::sxrem_get_gp_i
!>  @see intpol::intpol_get_gp_i
!>  @see thomson::thomson_get_gp_i
!>  @see ece::ece_get_gp_i
!>  @see prior_gaussian::prior_gaussian_get_gp_i
!>
!>  @param[in] this    A @ref signal_class instance.
!>  @param[in] a_model A @ref model_class instance.
!>  @param[in] i       Index of the position for the kernel.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the position and the signal.
!-------------------------------------------------------------------------------
      FUNCTION signal_get_gp_i(this, a_model, i, flags)
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                     :: signal_get_gp_i
      CLASS (signal_class), INTENT(in) :: this
      CLASS (model_class), POINTER     :: a_model
      INTEGER, INTENT(in)              :: i
      INTEGER, INTENT(in)              :: flags

!  local variables
      REAL (rprec)                     :: scale_factor
      REAL (rprec)                     :: start_time

!  Start of executable code
      CALL assert(.false., 'signal_get_gp_i not over written for ' //          &
     &                     this%get_type())

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a signal and a signal.
!>
!>  This method is meant to be overwritten by a subclass method. As a result,
!>  returns the get_gp_s method of the subclass instance @ref signal_class was
!>  constructed with. The second signal is cast to a void pointer so it can be
!>  send to the subclass instance.
!>
!>  @see sxrem::sxrem_get_gp_s
!>  @see intpol::intpol_get_gp_s
!>  @see thomson::thomson_get_gp_s
!>  @see ece::ece_get_gp_s
!>  @see prior_gaussian::prior_gaussian_get_gp_s
!>
!>  @param[in] this    A @ref signal_class instance.
!>  @param[in] a_model A @ref model_class instance.
!>  @param[in] signal  A @ref signal_class instance for the second signal.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for the signal and the signal.
!-------------------------------------------------------------------------------
      FUNCTION signal_get_gp_s(this, a_model, signal, flags)
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                     :: signal_get_gp_s
      CLASS (signal_class), INTENT(in) :: this
      CLASS (model_class), POINTER     :: a_model
      CLASS (signal_class), POINTER    :: signal
      INTEGER, INTENT(in)              :: flags

!  Start of executable code
      CALL assert(.false., 'signal_get_gp_s not over written for ' //          &
     &                     this%get_type())

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the guassian process kernel for a signal and cartesian position.
!>
!>  This method is meant to be overwritten by a subclass method. As a result,
!>  returns the get_gp_x method of the subclass instance @ref signal_class was
!>  constructed with.
!>
!>  @see sxrem::sxrem_get_gp_x
!>  @see intpol::intpol_get_gp_x
!>  @see thomson::thomson_get_gp_x
!>  @see ece::ece_get_gp_x
!>  @see prior_gaussian::prior_gaussian_get_gp_x
!>
!>  @param[in] this    A @ref signal_class instance.
!>  @param[in] a_model A @ref model_class instance.
!>  @param[in] x_cart  The cartesian position of to get the kernel at.
!>  @param[in] flags   State flags to send to the kernel.
!>  @returns Kernel value for position and the signal.
!-------------------------------------------------------------------------------
      FUNCTION signal_get_gp_x(this, a_model, x_cart, flags)
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: signal_get_gp_x
      CLASS (signal_class), INTENT(in)       :: this
      CLASS (model_class), POINTER           :: a_model
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: flags

!  Start of executable code
      CALL assert(.false., 'signal_get_gp_x not over written for ' //          &
     &                     this%get_type())

      END FUNCTION

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Apply scale and offset to the value.
!>
!>  Modeled signals can contain an extra scaling and offset. Apply these to the
!>  value. These are obtained from the model.
!>  @param[in]    this    A @ref intpol_class instance.
!>  @param[in]    a_model A @ref model instance.
!>  @param[inout] value   The value to scale and offset.
!-------------------------------------------------------------------------------
      SUBROUTINE signal_scale_and_offset(this, a_model, value)
      CLASS (signal_class), INTENT(in) :: this
      CLASS (model_class), INTENT(in)  :: a_model
      REAL (rprec), INTENT(inout)      :: value

!  local variables
      REAL (rprec)                     :: scale_factor
      REAL (rprec)                     :: offset_factor
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      scale_factor = a_model%get_signal_factor(this%scale_index)
      offset_factor = a_model%get_signal_offset(this%offset_index)

      value = value*scale_factor + offset_factor

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Generate a short name by appending an index.
!>
!>  Returns an appended string with no leading spaces. This check the number of
!>  digits in the index then chooses the appropriate formatting.
!>
!>  @param[in] name  A name of a signal to append to.
!>  @param[in] index A index of a signal to append.
!>  @returns A string an appended string.
!-------------------------------------------------------------------------------
      FUNCTION signal_make_short_name(name, index)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_short_name_length) :: signal_make_short_name
      CHARACTER (len=*), INTENT(in)          :: name
      INTEGER, INTENT(in)                    :: index

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Find the number of digits in the index
      IF (index .lt. 10) THEN
         WRITE (signal_make_short_name,'(a,i1)') TRIM(name), index
      ELSE IF (index .lt. 100) THEN
         WRITE (signal_make_short_name,'(a,i2)') TRIM(name), index
      ELSE IF (index .lt. 1000) THEN
         WRITE (signal_make_short_name,'(a,i3)') TRIM(name), index
      ELSE
         WRITE (signal_make_short_name,'(a,i4)') TRIM(name), index
      END IF

      CALL profiler_set_stop_time('signal_make_short_name', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Write out the signal information to an output file.
!>
!>  Signals information is formated as the index, signal type, s_name, g^2,
!>  weight, observed value, observed sigma, modeled value and modeled sigma.
!>
!>  @param[inout] this    A @ref signal_class instance.
!>  @param[in]    iou     A input/output representing the file to write to.
!>  @param[in]    index   A index of a signal.
!>  @param[inout] a_model The equilibrium model.
!>  @note This assumes that signals have already been calculated. The model
!>  isn't used by signal_get_g2 since it is reading from the cache but must be
!>  passed in anyway.
!-------------------------------------------------------------------------------
      SUBROUTINE signal_write(this, iou, index, a_model)
      USE model

      IMPLICIT NONE

!  Declare Arguments
      CLASS (signal_class), INTENT(inout)   :: this
      INTEGER, INTENT(in)                   :: iou
      INTEGER, INTENT(in)                   :: index
      CLASS (model_class), POINTER          :: a_model

!  local variables
      REAL (rprec), DIMENSION(4)            :: modeled_signal
      REAL (rprec), DIMENSION(4)            :: modeled_sigma
      REAL (rprec)                          :: start_time

!  local paramaters
      REAL (rprec), DIMENSION(4), PARAMETER :: dummy_value = 0.0

!  Start of executable code
      start_time = profiler_get_start_time()

!  When reading from the signal cache, the signal is not recomputed so any value
!  can be used for the last_value arument.
      modeled_signal = this%get_modeled_signal(a_model, modeled_sigma,         &
     &                                         .true., dummy_value)

      WRITE (iou,1000) index, this%get_type(), this%s_name,                    &
     &                 this%get_g2(a_model, .true., modeled_signal),           &
     &                 this%weight, this%get_observed_signal(a_model),         &
     &                 this%observed_sigma, modeled_signal,                    &
     &                 modeled_sigma

      CALL profiler_set_stop_time('signal_write', start_time)

1000  FORMAT(i4,2x,a23,2x,a20,12(2x,es12.5))

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write out the signal header information to an output file.
!>
!>  Signals information is formated as a the index, signal type, s_name, g^2,
!>  weight, observed value, observed sigma, modeled value and modeled sigma.
!>
!>  @param[in] this A @ref signal_class instance.
!>  @param[in] iou  A input/output representing the file to write to.
!>  @note This should only be called when the type of signal changes.
!-------------------------------------------------------------------------------
      SUBROUTINE signal_write_header(this, iou)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (signal_class), INTENT(in)               :: this
      INTEGER, INTENT(in)                            :: iou

!  local variables
      CHARACTER (len=data_name_length), DIMENSION(7) :: header
      REAL (rprec)                                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL this%get_header(header)
      WRITE (iou,*)
      WRITE (iou,1000) header

      CALL profiler_set_stop_time('signal_write_header', start_time)

1000  FORMAT(3x,'#',2x,'type',21x,'s_name',16x,'g^2',11x,'weight',8x,          &
     &       'observed',6x,'sigma', 9x,'model',7x,2(2x,a12),7(2x,a12))

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write out any auxiliary signal information to an output file.
!>
!>  This method is meant to be overwritten by a subclass method if need be. As a
!>  result, returns the write_auxiliary method of the subclass instance
!>  @ref signal_class was constructed with. Otherwise uses the default. nothing
!>  is written.
!>
!>  @see sxrem_ratio::sxrem_ratio_write_auxilary
!>  @see prior_gaussian::prior_gaussian_write_auxiliary
!>  @see combination::combination_write_auxiliary
!>
!>  @param[in] this    A @ref signal_class instance.
!>  @param[in] iou     A input/output representing the file to write to.
!>  @param[in] index   A index of a signal.
!>  @param[in] a_model The equilibrium model.
!-------------------------------------------------------------------------------
      SUBROUTINE signals_write_auxiliary(this, iou, index, a_model)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (signal_class), INTENT(in) :: this
      INTEGER, INTENT(in)              :: iou
      INTEGER, INTENT(in)              :: index
      CLASS (model_class), INTENT(in)  :: a_model

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      CALL profiler_set_stop_time('signals_write_auxiliary', start_time)

      END SUBROUTINE

!*******************************************************************************
!  NETCDF SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Write out the signal data for a step to the result netcdf file.
!>
!>  Writes out the observed, modeled and total sigma value of the signal.
!>
!>  @param[inout] this                  A @ref diagnostic_class instance.
!>  @param[inout] a_model               The equilibrium model.
!>  @param[in]    result_ncid           A NetCDF id of the result file.
!>  @param[in]    current_step          The surrent reconstruction step.
!>  @param[in]    index                 A index of a signal.
!>  @param[in]    signal_model_value_id NetCDF variable id of the model value.
!>  @param[in]    signal_sigma_value_id NetCDF variable id of the total sigma.
!>  @note This assumes that signals have already been calculated. The model
!>  isn't used by signal_get_g2 since it is reading from the cache but must be
!>  passed in anyway.
!-------------------------------------------------------------------------------
      SUBROUTINE signal_write_step_data(this, a_model, result_ncid,            &
     &                                  current_step, index,                   &
     &                                  signal_model_value_id,                 &
     &                                  signal_sigma_value_id)
      USE model
      USE ezcdf

      IMPLICIT NONE

!  Declare Arguments
      CLASS (signal_class), INTENT(inout)   :: this
      CLASS (model_class), POINTER          :: a_model
      INTEGER, INTENT(in)                   :: result_ncid
      INTEGER, INTENT(in)                   :: current_step
      INTEGER, INTENT(in)                   :: index
      INTEGER, INTENT(in)                   :: signal_model_value_id
      INTEGER, INTENT(in)                   :: signal_sigma_value_id

!  local variables
      INTEGER                               :: status
      INTEGER                               :: varid
      REAL (rprec), DIMENSION(4)            :: modeled_sigma
      REAL (rprec), DIMENSION(4)            :: modeled_signal
      REAL (rprec)                          :: start_time

!  local paramaters
      REAL (rprec), DIMENSION(4), PARAMETER :: dummy_value = 0.0

!  Start of executable code
      start_time = profiler_get_start_time()

!  When reading from the signal cache, the signal is not recomputed so any value
!  can be used for the last_value arument.
      modeled_signal = this%get_modeled_signal(a_model, modeled_sigma,         &
     &                                         .true., dummy_value)

      status = nf90_put_var(result_ncid, signal_model_value_id,                &
     &                      modeled_signal,                                    &
     &                      start=(/ 1, index, current_step /),                &
     &                      count=(/ 4, 1, 1 /))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_put_var(result_ncid, signal_sigma_value_id,                &
     &                      SQRT(this%get_sigma2()),                           &
     &                      start=(/ index, current_step /))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

!  Feedback signals change the observed signal. Write the current observed
!  value at eash step.
#if 0
!  FIXME: Disable this for now until it is moved into the refactored feed
!         back class.
      SELECT CASE(this%type)

         CASE (signal_feedback_type)
            status = nf90_inq_varid(result_ncid,                               &
     &                              'signal_observed_value', varid)
            CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

            status =                                                           &
     &         nf90_put_var(result_ncid, varid,                                &
     &                      this%get_observed_signal(a_model),                 &
     &                      start=index)
            CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      END SELECT
#endif
      CALL profiler_set_stop_time('signal_write_step_data', start_time)

      END SUBROUTINE

!*******************************************************************************
!  MPI SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Syncronize a child signal state to the parent.
!>
!>  Syncs data between a child and the parent process. If MPI support is not
!>  compiled in this subroutine reduces to a no op.
!>
!>  @param[inout] this       A @ref signal_class instance.
!>  @param[in]    index      Reconstruction rank to sync.
!>  @param[in]    recon_comm MPI communicator for the reconstruction processes.
!-------------------------------------------------------------------------------
      SUBROUTINE signal_sync_child(this, index, recon_comm)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (signal_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                 :: index
      INTEGER, INTENT(in)                 :: recon_comm

#if defined(MPI_OPT)
!  local variables
      INTEGER                             :: error
      INTEGER                             :: mpi_rank
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL MPI_COMM_RANK(recon_comm, mpi_rank, error)

      IF (mpi_rank .eq. index) THEN
         CALL MPI_SSEND(this%modeled, 4, MPI_REAL8, 0, mpi_rank,               &
     &                  recon_comm, error)
         CALL MPI_SSEND(this%modeled_sigma, 4, MPI_REAL8, 0, mpi_rank,         &
     &                  recon_comm, error)
       ELSE IF (mpi_rank .eq. 0) THEN
         CALL MPI_RECV(this%modeled, 4, MPI_REAL8, index, index,               &
     &                 recon_comm, MPI_STATUS_IGNORE, error)
         CALL MPI_RECV(this%modeled_sigma, 4, MPI_REAL8, index, index,         &
     &                 recon_comm, MPI_STATUS_IGNORE, error)
      END IF

      CALL profiler_set_stop_time('signal_sync_child', start_time)
#endif

      END SUBROUTINE

      END MODULE

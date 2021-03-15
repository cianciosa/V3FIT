!*******************************************************************************
!>  @file magnetic.f
!>  @brief Contains module @ref magnetic
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref magnetic_class.
!>  @par Super Class:
!>  @ref diagnostic
!*******************************************************************************

      MODULE magnetic
      USE stel_kinds, only: rprec
      USE magnetic_response
      USE profiler
      USE signal

      IMPLICIT NONE

!*******************************************************************************
!  magnetic module parameters
!*******************************************************************************
!>  Bit position for the use coil response flag.
      INTEGER, PARAMETER :: magnetic_force_coil_flag = 1
!>  Bit position for the force coil response flag.
      INTEGER, PARAMETER :: magnetic_3D_flag         = 2

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) magnetic base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a magnetic signal.
!>  @par Super Class:
!>  @ref diagnostic
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(signal_class) :: magnetic_class
!>  Setting to force the computing of the coil constribution.
         INTEGER                                 :: control_flags
!>  Magnetic response function object.
         TYPE (magnetic_response_class), POINTER :: response => null()
      CONTAINS
         PROCEDURE                               ::                            &
     &      get_modeled_signal_last => magnetic_get_modeled_signal
         PROCEDURE                               ::                            &
     &      get_type => magnetic_get_type
         PROCEDURE                               ::                            &
     &      get_header => magnetic_get_header
         FINAL                                   :: magnetic_destruct
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the construction of @ref magnetic_class types using
!>  @ref magnetic_construct_netcdf
!-------------------------------------------------------------------------------
      INTERFACE magnetic_class
         MODULE PROCEDURE magnetic_construct_netcdf
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref magnetic_class object containing a
!>  @ref magnetic_response::magnetic_response_class object.
!>
!>  Allocates memory and initializes a @ref magnetic_class object.
!>
!>  @param[in] mdsig_iou          An instance of a the netcdf id of the open
!>                                mdsig file.
!>  @param[in] use_coil_response  Toggle if the total signal contains the
!>                                induced signal in addition to the plasma only
!>                                signal.
!>  @param[in] force_coil_reponse Force the coil response to be computed.
!>  @param[in] use_3D_only        Subtract of the axisymmtric porition of the
!>                                signal.
!>  @param[in] svd_cut_off        Cutoff value for the number of singular values
!>                                to retain when compressing the response
!>                                function arrays.
!>  @returns A pointer to a constructed @ref magnetic_class object.
!-------------------------------------------------------------------------------
      FUNCTION magnetic_construct_netcdf(mdsig_iou, use_coil_response,         &
     &                                   force_coil_reponse,                   &
     &                                   use_3D_only, svd_cut_off)
      USE ezcdf
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      CLASS (magnetic_class), POINTER :: magnetic_construct_netcdf
      INTEGER, INTENT(in)             :: mdsig_iou
      LOGICAL, INTENT(in)             :: use_coil_response
      LOGICAL, INTENT(in)             :: force_coil_reponse
      LOGICAL, INTENT(in)             :: use_3D_only
      REAL (rprec), INTENT(in)        :: svd_cut_off

!  local variables
      INTEGER, DIMENSION(3)           :: dim_lengths
      LOGICAL                         :: temp_flag
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(magnetic_construct_netcdf)

      magnetic_construct_netcdf%response =>                                    &
     &   magnetic_response_construct(mdsig_iou, svd_cut_off)

      magnetic_construct_netcdf%control_flags = 0

      IF (force_coil_reponse) THEN
         CALL assert(magnetic_response_use_coil(                               &
     &                  magnetic_construct_netcdf%response),                   &
     &               'Cannot force coil response. Coil responses ' //          &
     &               'not found in mdsig file.')
         magnetic_construct_netcdf%control_flags =                             &
     &      IBSET(magnetic_construct_netcdf%control_flags,                     &
     &            magnetic_force_coil_flag)
      END IF

      IF (use_3D_only) THEN
         magnetic_construct_netcdf%control_flags =                             &
     &      IBSET(magnetic_construct_netcdf%control_flags,                     &
     &            magnetic_3D_flag)
      END IF

!  Check if using the coil response for this diagnostic. If not using it clear
!  the responce flag bit.
      IF (.not.use_coil_response) THEN
         CALL magnetic_response_clr_use_coil(                                  &
     &           magnetic_construct_netcdf%response)
      END IF

      CALL profiler_set_stop_time('magnetic_construct_netcdf',                 &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref magnetic_class object.
!>
!>  Deallocates memory and uninitializes a @ref magnetic_class object.
!>
!>  @param[inout] this A @ref magnetic_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE magnetic_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (magnetic_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%response)) THEN
         CALL magnetic_response_destruct(this%response)
         this%response => null()
      END IF

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the modeled signal.
!>
!>  The modeled signal can be computed in one of two ways. Modeled as a full
!>  coil or as a point measurement.
!>
!>  @param[inout]  this          A @ref magnetic_class instance.
!>  @param[in]  a_model       A @ref model instance.
!>  @param[out] sigma         The modeled sigma.
!>  @param[in]  last_value    Last good value in case the signal did not change.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION magnetic_get_modeled_signal(this, a_model, sigma,               &
     &                                     last_value)
      USE model

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: magnetic_get_modeled_signal
      CLASS (magnetic_class), INTENT(inout)   :: this
      TYPE (model_class), POINTER             :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out) :: sigma
      REAL (rprec), DIMENSION(4), INTENT(in)  :: last_value

!  local variables
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      sigma = 0.0

      IF (BTEST(a_model%state_flags, model_state_vmec_flag)   .or.             &
     &    BTEST(a_model%state_flags, model_state_siesta_flag) .or.             &
     &    BTEST(a_model%state_flags, model_state_shift_flag)  .or.             &
     &    BTEST(a_model%state_flags, model_state_signal_flag)) THEN

         IF (magnetic_response_is_point(this%response)) THEN
            magnetic_get_modeled_signal =                                      &
     &         magnetic_get_modeled_signal_point(this, a_model, sigma)
         ELSE
            magnetic_get_modeled_signal =                                      &
     &         magnetic_get_modeled_signal_coil(this, a_model, sigma)
         END IF

         CALL this%scale_and_offset(a_model,                                   &
     &                              magnetic_get_modeled_signal(1))
      ELSE
         magnetic_get_modeled_signal = last_value
      END IF

      CALL profiler_set_stop_time('magnetic_get_modeled_signal',               &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Calculates the point modeled signal.
!>
!>  The plasma signal is the surface integral of the virtual current denisty.
!>  This is computed using the virtual casing routines in LIBSTELL.
!-------------------------------------------------------------------------------
!>
!>  External current contribrutions are included by the dot product of the
!>  external currents with a linear combination of the external fields.
!-------------------------------------------------------------------------------
!>
!>  The plasma only signal is stored in the 2nd index.
!>  The external current signal is stored in the 3rd index.
!>  Total signal is the sum of plasma signal and external signal.
!>
!>  @param[in]    this    A @ref magnetic_class instance.
!>  @param[inout] a_model A @ref model instance.
!>  @param[out]   sigma   The modeled sigma.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION magnetic_get_modeled_signal_point(this, a_model, sigma)
      USE model

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: magnetic_get_modeled_signal_point
      TYPE (magnetic_class), INTENT(in)       :: this
      TYPE (model_class), INTENT(in)          :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out) :: sigma

!  local variables
      REAL (rprec), DIMENSION(3)              :: b_field
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      magnetic_get_modeled_signal_point = 0.0

      b_field = equilibrium_get_ext_b_plasma(a_model%equilibrium,              &
     &                                       this%response%position,           &
     &                                       .false.)

      magnetic_get_modeled_signal_point(2) =                                   &
     &   DOT_PRODUCT(b_field, this%response%direction)

!  Axisymmetric component.
      IF (BTEST(this%control_flags, magnetic_3D_flag)) THEN
         b_field = equilibrium_get_ext_b_plasma(a_model%equilibrium,           &
     &                                          this%response%position,        &
     &                                          .true.)

         magnetic_get_modeled_signal_point(4) =                                &
     &      DOT_PRODUCT(b_field, this%response%direction)
      END IF

!  Coil Pickup -----------------------------------------------------------------
      magnetic_get_modeled_signal_point(3) =                                   &
     &   magnetic_get_modeled_signal_coil_pickup(this, a_model)

!  Total Signal ----------------------------------------------------------------

!  Add plasma signals and induced signal to create total signal.
      IF (magnetic_response_use_coil(this%response)) THEN
         magnetic_get_modeled_signal_point(1) =                                &
     &      SUM(magnetic_get_modeled_signal_point(2:3))
      ELSE
         magnetic_get_modeled_signal_point(1) =                                &
     &      magnetic_get_modeled_signal_point(2)
      END IF

!  Subtract off the axisymmtric component. When not using the 3D only signal
!  the value stored in the forth position is zero so there is no need to check
!  the flag.
      magnetic_get_modeled_signal_point(1) =                                   &
     &   magnetic_get_modeled_signal_point(1) -                                &
     &   magnetic_get_modeled_signal_point(4)

      sigma = 0.0

      CALL profiler_set_stop_time('magnetic_get_modeled_signal_point',         &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Calculates the coil modeled signal.
!>
!>  The plasma signal is the volume integral of the current denisty dotted with
!>  the diagnostic response function vector. The response_function contains a rz
!>  grid of reponce function vectors. These grid points are then interpolated
!>  into the equilibrium grid positions for a single toroidal plane.
!>  Hishman et. al. Phys. Plasmas 11, 595 (2004); doi: 10.1063/1.1637347
!>
!>  phi = Int[dV J_plasma * A_diag]
!>
!>  J_plasma and dV are obtained from the @ref equilibrium. A_diag is obtained
!>  from the @ref magnetic_response
!-------------------------------------------------------------------------------
!>
!>  External current contribrutions are included by the dot product of the
!>  external currents with mutual inductance values. These currents are obtained
!>  from @ref equilibrium if they exist.
!-------------------------------------------------------------------------------
!>
!>  The plasma only signal is stored in the 2nd index.
!>  The external current signal is stored in the 3rd index.
!>  Total signal is the sum of plasma signal and external signal.
!>
!>  @param[in]    this    A @ref magnetic_class instance.
!>  @param[inout] a_model A @ref model instance.
!>  @param[out]   sigma   The modeled sigma.
!>  @returns The model value.
!-------------------------------------------------------------------------------
      FUNCTION magnetic_get_modeled_signal_coil(this, a_model, sigma)
      USE model
      USE bivariate
      USE v3_utilities, only: err_warn

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(4) :: magnetic_get_modeled_signal_coil
      TYPE (magnetic_class), INTENT(in)         :: this
      TYPE (model_class), INTENT(in)            :: a_model
      REAL (rprec), DIMENSION(4), INTENT(out)   :: sigma

!  local variables
      INTEGER                                   :: phi, num_phi, r, z
      INTEGER                                   :: numrvol, numzvol
      TYPE (bivariate_class), POINTER           :: bivariate_obj
      REAL (rprec), DIMENSION(:), ALLOCATABLE   :: rgrid
      REAL (rprec), DIMENSION(:), ALLOCATABLE   :: zgrid
      REAL (rprec), DIMENSION(:,:,:), POINTER   :: rvolgrid
      REAL (rprec), DIMENSION(:,:,:), POINTER   :: zvolgrid
      REAL (rprec), DIMENSION(:,:,:), POINTER   :: jrvolgrid
      REAL (rprec), DIMENSION(:,:,:), POINTER   :: jphivolgrid
      REAL (rprec), DIMENSION(:,:,:), POINTER   :: jzvolgrid
      REAL (rprec), DIMENSION(:,:), POINTER     :: krgrid
      REAL (rprec), DIMENSION(:,:), POINTER     :: kphigrid
      REAL (rprec), DIMENSION(:,:), POINTER     :: kzgrid
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE :: response_rvol
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE :: response_phivol
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE :: response_zvol
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE :: sumrz
      REAL (rprec), DIMENSION(:), ALLOCATABLE   :: sumphi
      INTEGER                                   :: num_r, num_z

      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      magnetic_get_modeled_signal_coil = 0.0

!  Calculate the plasma only signal from the response function.
!  Get the volume integration grid points.
!  NOTE: Do not deallocate any of these arrays. They are owned by the
!  equilibrium
      rvolgrid =>                                                              &
     &   equilibrium_get_magnetic_volume_rgrid(a_model%equilibrium)
      zvolgrid =>                                                              &
     &   equilibrium_get_magnetic_volume_zgrid(a_model%equilibrium)
      jrvolgrid =>                                                             &
     &   equilibrium_get_magnetic_volume_jrgrid(a_model%equilibrium)
      jphivolgrid =>                                                           &
     &   equilibrium_get_magnetic_volume_jphigrid(a_model%equilibrium)
      jzvolgrid =>                                                             &
     &   equilibrium_get_magnetic_volume_jzgrid(a_model%equilibrium)

!  The equilibrium may return null pointers indicating the lack of a plasma.
      IF (ASSOCIATED(rvolgrid) .and. ASSOCIATED(zvolgrid) .and.                &
     &    ASSOCIATED(jrvolgrid) .and. ASSOCIATED(jphivolgrid) .and.            &
     &    ASSOCIATED(jzvolgrid)) THEN

!  Allocate space for the interpolated response functions. Only need two
!  dimensions because the interpolation is performed one phi position at a time.
         numrvol = SIZE(rvolgrid, 1)
         numzvol = SIZE(rvolgrid, 2)
         ALLOCATE(response_rvol(numrvol, numzvol))
         ALLOCATE(response_phivol(numrvol, numzvol))
         ALLOCATE(response_zvol(numrvol, numzvol))

!  Allocate work arrays to hold the results of the integration.
         num_phi = SIZE(this%response%a_r)
         ALLOCATE(sumrz(numrvol, numzvol))
         ALLOCATE(sumphi(num_phi))

!  Create the r and z gridpoints for the magnetic response function.
!>  @todo FIXME: For a single diagnostic these never change. Allocate these once
!>  and cache these in the @ref magnetic_class object.
         num_r = compression_get_dimension1(this%response%a_r(1)%p)
         ALLOCATE(rgrid(num_r))
         DO r = 1, num_r
            rgrid(r) = this%response%rmin                                      &
     &               + (r - 1)*(this%response%rmax - this%response%rmin)       &
     &               / (num_r - 1)
         END DO

         num_z = compression_get_dimension2(this%response%a_r(1)%p)
         ALLOCATE(zgrid(num_z))
         DO z = 1, num_z
            zgrid(z) = this%response%zmin                                      &
     &               + (z - 1)*(this%response%zmax - this%response%zmin)       &
     &               / (num_z - 1)
         END DO

!  Create a bivariate object to do the interpolation.
         bivariate_obj => bivariate_class(numrvol, numzvol)

!  Sum over grid points for each toroidal plane
         DO phi = 1, num_phi
            CALL bivariate_obj%set_grids(rvolgrid(:,:,phi),                    &
     &                                   zvolgrid(:,:,phi),                    &
     &                                   rgrid, zgrid)

!  Interpolate from the diagnotic grid points to the volume grid points.
            CALL compression_decompress(this%response%a_r(phi)%p)
            CALL bivariate_obj%get_4pt(                                        &
     &              this%response%a_r(phi)%p%data_buffer, response_rvol)
            CALL compression_cleanup(this%response%a_r(phi)%p)

            CALL compression_decompress(this%response%a_f(phi)%p)
            CALL bivariate_obj%get_4pt(                                        &
     &               this%response%a_f(phi)%p%data_buffer,                     &
     &               response_phivol)
            CALL compression_cleanup(this%response%a_f(phi)%p)

            CALL compression_decompress(this%response%a_z(phi)%p)
            CALL bivariate_obj%get_4pt(                                        &
     &               this%response%a_z(phi)%p%data_buffer,                     &
     &               response_zvol)
            CALL compression_cleanup(this%response%a_z(phi)%p)

!  J_plasma * A_diag
!  First compute the rz grid signal. Then compute the signal for a single phi
!  plane. Over the radial component, the ends need to be taken as half.
            sumrz = jrvolgrid(:,:,phi)*response_rvol                           &
     &            + jphivolgrid(:,:,phi)*response_phivol                       &
     &            + jzvolgrid(:,:,phi)*response_zvol
            sumphi(phi) = SUM(sumrz(2:numrvol - 1,:))                          &
     &                  + SUM(sumrz(1,:) + sumrz(numrvol,:))/2.0
         END DO

         DEALLOCATE(bivariate_obj)

!  Correct for stellarator symetry.
         magnetic_get_modeled_signal_coil(2) = SUM(sumphi)
         IF (magnetic_response_is_stell_sym(this%response)) THEN
            magnetic_get_modeled_signal_coil(2)                                &
     &         = magnetic_get_modeled_signal_coil(2)                           &
     &         - (sumphi(1) + sumphi(num_phi))/2.0
         END IF

!  Finish integration with the volume element.
         magnetic_get_modeled_signal_coil(2)                                   &
     &      = magnetic_get_modeled_signal_coil(2)                              &
     &      * equilibrium_get_volume_int_element(a_model%equilibrium)

!  Deallocate work arrays.
!  NOTE: Do not deallocate any of the vol grid arrays that were obtained from
!  the equilibrium_get_magnetic_volume_*grid routines. These arrays are owned by
!  by the equilibrium.
         DEALLOCATE(response_rvol, response_phivol, response_zvol)
         DEALLOCATE(sumrz, sumphi)
         DEALLOCATE(rgrid, zgrid)

      END IF

!  Conducting Shell ------------------------------------------------------------
      IF (magnetic_response_use_shell(this%response)) THEN

         num_phi = compression_get_dimension2(this%response%a_s_r)

         krgrid =>                                                             &
     &      equilibrium_get_con_surface_krgrid(a_model%equilibrium)
         kphigrid =>                                                           &
     &      equilibrium_get_con_surface_kphigrid(a_model%equilibrium)
         kzgrid =>                                                             &
     &      equilibrium_get_con_surface_kzgrid(a_model%equilibrium)

         IF (ASSOCIATED(krgrid) .and. ASSOCIATED(kphigrid) .and.               &
     &       ASSOCIATED(kzgrid)) THEN
            ALLOCATE(sumphi(num_phi))

            CALL compression_decompress(this%response%a_s_r)
            CALL compression_decompress(this%response%a_s_f)
            CALL compression_decompress(this%response%a_s_z)

            DO phi = 1, num_phi
               sumphi(phi)                                                     &
     &            = SUM(krgrid(:,phi)*                                         &
     &                  this%response%a_s_r%data_buffer(:,phi))                &
     &            + SUM(kphigrid(:,phi)*                                       &
     &                  this%response%a_s_f%data_buffer(:,phi))                &
     &            + SUM(kzgrid(:,phi)*                                         &
     &                  this%response%a_s_z%data_buffer(:,phi))
            END DO

            CALL compression_cleanup(this%response%a_s_r)
            CALL compression_cleanup(this%response%a_s_f)
            CALL compression_cleanup(this%response%a_s_z)

!  Correct for stellarator symetry.
            magnetic_get_modeled_signal_coil(4) = SUM(sumphi)
            IF (magnetic_response_is_stell_sym(this%response)) THEN
               magnetic_get_modeled_signal_coil(4)                             &
     &            = magnetic_get_modeled_signal_coil(4)                        &
     &            - (sumphi(1) + sumphi(num_phi))/2.0
            END IF

!  Finish integration with the area element.
            magnetic_get_modeled_signal_coil(4)                                &
     &         = magnetic_get_modeled_signal_coil(4)                           &
     &         * equilibrium_get_area_int_element(a_model%equilibrium)

            DEALLOCATE(sumphi)
         END IF

      END IF

!  Coil Pickup -----------------------------------------------------------------
      magnetic_get_modeled_signal_coil(3) =                                    &
     &   magnetic_get_modeled_signal_coil_pickup(this, a_model)

!  Total Signal ----------------------------------------------------------------

!  Add plasma signals and induced signal to create total signal.
      IF (magnetic_response_use_coil(this%response)) THEN
         magnetic_get_modeled_signal_coil(1) =                                 &
     &      SUM(magnetic_get_modeled_signal_coil(2:3))
      ELSE
         magnetic_get_modeled_signal_coil(1) =                                 &
     &      magnetic_get_modeled_signal_coil(2)
      END IF

!  Add shell currents if used.
      IF (magnetic_response_use_shell(this%response)) THEN
         magnetic_get_modeled_signal_coil(1)                                   &
     &      = magnetic_get_modeled_signal_coil(1)                              &
     &      + magnetic_get_modeled_signal_coil(4)
      END IF

      sigma = 0.0

      CALL profiler_set_stop_time('magnetic_get_modeled_signal_coil',          &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the model signal contribution from the external coils.
!>
!>  Return the protion of the modeled signal for the induced pickup of the
!>  external coils. If no external coils exist or the induced current is not
!>  used this returns zero.
!>
!>  @param[in]    this    A @ref magnetic_class instance.
!>  @param[inout] a_model A @ref model instance.
!>  @returns The modeled signal from the induced coils.
!-------------------------------------------------------------------------------
      FUNCTION magnetic_get_modeled_signal_coil_pickup(this, a_model)
      USE model

      IMPLICIT NONE

!  Declare Arguments
      Real (rprec) :: magnetic_get_modeled_signal_coil_pickup
      TYPE (magnetic_class), INTENT(in)   :: this
      TYPE (model_class), INTENT(in)      :: a_model

!  local variables
      LOGICAL                             :: scale_current
      INTEGER                             :: num_currents
      REAL (rprec), DIMENSION(:), POINTER :: extcur
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      magnetic_get_modeled_signal_coil_pickup = 0.0

      IF (magnetic_response_use_coil(this%response)) THEN
         IF (BTEST(this%control_flags, magnetic_force_coil_flag)) THEN
            num_currents = SIZE(this%response%inductance)
         ELSE
            num_currents = -1
         END IF

!  Calculate the induced signal from external currents.
         extcur => equilibrium_get_ext_currents(a_model%equilibrium,           &
     &                                          num_currents,                  &
     &                                          scale_current)

!  If there are no external current extcur will be a null pointer.
         IF (ASSOCIATED(extcur)) THEN

!  Make sure we don't go out of bounds on any array.
            num_currents = MIN(SIZE(extcur),                                   &
     &                         SIZE(this%response%inductance))
            IF (num_currents .lt. this%response%n_field_cg) THEN
               CALL err_warn('magnetic_get_modeled_signal_coil: ' //           &
     &                       'mdsig_file expected more currents')
            END IF

            IF (scale_current) THEN
               magnetic_get_modeled_signal_coil_pickup = DOT_PRODUCT(          &
     &            this%response%inductance(1:num_currents),                    &
     &            extcur(1:num_currents)/this%response%current_scale)
            ELSE
               magnetic_get_modeled_signal_coil_pickup = DOT_PRODUCT(          &
     &            this%response%inductance(1:num_currents),                    &
     &            extcur(1:num_currents))
            END IF
         END IF
      END IF

      CALL profiler_set_stop_time(                                             &
     &        'magnetic_get_modeled_signal_coil_pickup', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the magnetic type.
!>
!>  Returns a description of the magnetic type for use when writting output
!>  files.
!>
!>  @param[in] this A @ref magnetic_class instance.
!>  @returns A string describing the magnetic type.
!-------------------------------------------------------------------------------
      FUNCTION magnetic_get_type(this)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length)   :: magnetic_get_type
      CLASS (magnetic_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      magnetic_get_type = 'mddc'

      CALL profiler_set_stop_time('magnetic_get_type', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the model and model sigma array indices.
!>
!>  Returns a description of the array indices for use when writting output
!>  files.
!>
!>  @param[in]    this   A @ref magnetic_class instance.
!>  @param[inout] header Buffer arrays to write header strings to.
!>  @returns A string describing the model and model sigma array indices.
!-------------------------------------------------------------------------------
      SUBROUTINE magnetic_get_header(this, header)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CLASS (magnetic_class), INTENT(in)             :: this
      CHARACTER (len=data_name_length), DIMENSION(7), INTENT(inout) ::         &
     &   header

!  local variables
      REAL (rprec)                                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      header(1) = 'plasma only'
      header(2) = 'eq currents'

!  Default to the 4th label as the axi symmtric only signal.
      IF (magnetic_response_use_shell(this%response)) THEN
         header(3) = 'shell currents'
      ELSE
         header(3) = 'Axi signal'
      END IF

      header(4) = 'model_sig(1)'
      header(5) = 'model_sig(2)'
      header(6) = 'model_sig(3)'
      header(7) = 'model_sig(4)'

      CALL profiler_set_stop_time('magnetic_get_header', start_time)

      END SUBROUTINE

      END MODULE

!-------------------------------------------------------------------------------
!  The @header, @table_section, @table_subsection, @item and @end_table commands
!  are custom defined commands in Doxygen.in. They are defined under ALIASES.
!  For the page created here, the 80 column limit is exceeded. Arguments of
!  aliases are separated by ','. If you intended ',' to be a string you must use
!  an escaped comma '\,'.
!
!>  @page model_sec Model Manual
!>
!>  @tableofcontents
!>  @section model_intro_sec Introduction
!>  This page documents the V3FIT interface to the model. This lists all the
!>  parameters associated with a model. Model parameters are common for all
!>  equilibrium types. All parameters are documented in a table of the following
!>  form.
!>  @header{Input variable, Description, Code Reference}
!>
!>  @section model_recon_param_sec Model Reconstruction Parameters
!>  @begin_table
!>     @item{ne_unit,           Electron density scaling.,                                @ref model::model_class::ne_unit}
!>     @item{ne_min,            Minimum electron density.,                                @ref model::model_class::ne_min}
!>     @item{te_min,            Minimum electron temperature.,                            @ref model::model_class::te_min}
!>     @item{ti_min,            Minimum ion temperature.,                                 @ref model::model_class::ti_min}
!>     @item{sxrem_min,         Minimum soft x-ray emission.,                             @ref model::model_class::sxrem_min}
!>     @item{pressure_fraction, Fractional portion of the pressure due to the electrons., @ref model::model_class::pressure_fraction}
!>     @item{coosig_wgts,       Combination signal weights.,                              @ref model::model_class::coosig_wgts}
!>     @item{signal_factor,     Signal scaling factor for signal models.,                 @ref model::model_class::signal_factor}
!>     @item{signal_offset,     Signal offset factor for signal models.,                  @ref model::model_class::signal_offset}
!>  @end_table
!>
!>  @section model_derived_param_sec Model Derived Parameters
!>  The following derived parameters are defined on a grid. The size of that
!>  grid will depend on equilibrium used.
!>  @begin_table
!>     @item{ne_grid,    Electron density grid.,      @ref model::model_class::ne_grid}
!>     @item{te_grid,    Electron temperature grid.,  @ref model::model_class::te_grid}
!>     @item{ti_grid,    Ion temperature grid.,       @ref model::model_class::ti_grid}
!>     @item{sxrem_grid, Soft X-ray emissivity grid., @ref model::model_class::sxrem_grid}
!>  @end_table
!>
!>  @section model_sxrem_sec Model Soft X-ray Emissivity
!>  The soft x-ray emissivity can be modeled as a plain emissivity profile, or
!>  soft x-ray emission can be computed directly. The latter requires a soft
!>  x-ray emissivity file to be defined.
!>  @see @ref emission_file_sec 
!*******************************************************************************
!>  @file model.f
!>  @brief Contains module @ref model.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref model_class. The model contains
!>  information not specific to the equilibrium.
!*******************************************************************************

      MODULE model
      USE equilibrium
      USE emission
      USE model_state
      USE integration_path, ONLY: path_int_class
      USE stel_constants
      USE data_parameters

      IMPLICIT NONE

!*******************************************************************************
!  model module parameters
!*******************************************************************************
!>  No model.
      INTEGER, PARAMETER, PRIVATE :: model_none_type        = -1

!>  Electron denisty model.
      INTEGER, PARAMETER, PRIVATE :: model_ne_type          =  0
!>  Electron denisty model is derived from the temperature and pressure.
      INTEGER, PARAMETER, PRIVATE :: model_ne_te_p_type     =  1

!>  Soft X-ray Emissivity.
      INTEGER, PARAMETER, PRIVATE :: model_sxrem_type       =  0
!>  Soft X-ray Emissivity model is derived from the density and temperature.
      INTEGER, PARAMETER, PRIVATE :: model_sxrem_te_ne_type =  1

!>  Electron temperature model.
      INTEGER, PARAMETER, PRIVATE :: model_te_type          =  0
!>  Electron temperature model is derived from the density and pressure.
      INTEGER, PARAMETER, PRIVATE :: model_te_ne_p_type     =  1

!>  Ion temperature model.
      INTEGER, PARAMETER, PRIVATE :: model_ti_type          =  0

!  Model parameter ids
!>  Parameter id for the electron density units.
      INTEGER, PARAMETER :: model_ne_unit_id           = 0
!>  Parameter id for the minimum electron density.
      INTEGER, PARAMETER :: model_ne_min_id            = 1
!>  Parameter id for the minimum electron temperature.
      INTEGER, PARAMETER :: model_te_min_id            = 2
!>  Parameter id for the minimum electron temperature.
      INTEGER, PARAMETER :: model_ti_min_id            = 3
!>  Parameter id for the electrion fraction of the pressure.
      INTEGER, PARAMETER :: model_pressure_fraction_id = 4
!>  Parameter id for the electrion fraction of the pressure.
      INTEGER, PARAMETER :: model_sxrem_min_id         = 5
!>  Parameter id for the combination signal weights
      INTEGER, PARAMETER :: model_coosig_wgts_id       = 6
!>  Parameter id for the modeled signal scale factors.
      INTEGER, PARAMETER :: model_signal_factor_id     = 7
!>  Parameter id for the modeled signal offset factors.
      INTEGER, PARAMETER :: model_signal_offset_id     = 8

!  Derived Parameters
!>  Parameter id for the electrion density grid.
      INTEGER, PARAMETER :: model_ne_grid_id           = 9
!>  Parameter id for the electrion temperature grid.
      INTEGER, PARAMETER :: model_te_grid_id           = 10
!>  Parameter id for the ion temperature grid.
      INTEGER, PARAMETER :: model_ti_grid_id           = 11
!>  Parameter id for the soft x-ray emissivity grid.
      INTEGER, PARAMETER :: model_sxrem_grid_id        = 12
!  NOTE: When model parameters are added here, the equilibrium id's need to be
!  updated in additon.

!>  Conversion from Joules to eV.
      REAL(rprec), PARAMETER :: eV_per_Joule = one/1.602e-19_rprec

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) model class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a model.
!-------------------------------------------------------------------------------
      TYPE model_class
!>  State flags indicating with parts of the model have changes.
         INTEGER      :: state_flags = model_state_all_off

!>  Type decritption of the electron denisty. Possible values are
!>  @ref model_none_type, @ref model_ne_type and @ref model_ne_te_p_type.
         INTEGER      :: ne_type = model_none_type
!>  Type decritption of the soft x-ray emissivity. Possible values are
!>  @ref model_none_type and @ref model_sxrem_type.
         INTEGER, DIMENSION(:), POINTER :: sxrem_type => null()
!>  Type decritption of the electron temperature. Possible values are
!>  @ref model_none_type, @ref model_te_type and @ref model_te_ne_p_type.
         INTEGER      :: te_type = model_none_type
!>  Type decritption of the ion temperature. Possible values are
!>  @ref model_none_type and @ref model_ti_type.
         INTEGER      :: ti_type = model_none_type
!>  Unit scaling of the electron density.
         REAL (rprec)                        :: ne_unit = 1.0
!>  Minimum electron density.
         REAL (rprec)                        :: ne_min = 0.0
!>  Minimum electron temperature.
         REAL (rprec)                        :: te_min = 0.0
!>  Minimum ion temperature.
         REAL (rprec)                        :: ti_min = 0.0
!>  Minimum soft x-ray emission.
         REAL (rprec), DIMENSION(:), POINTER :: sxrem_min => null()
!>  Electron pressure fraction.
         REAL (rprec)                        :: pressure_fraction = 0.0
!>  Array of weights for combination signals
         REAL (rprec), DIMENSION(:), POINTER :: coosig_wgts => null()

!>  Soft X-Ray emission function.
         TYPE (emission_class), POINTER        :: emission => null()
!>  Filter transmission functions.
         REAL (rprec), DIMENSION(:,:), POINTER :: transmission => null()

!>  The @ref equilibrium.
         CLASS (equilibrium_class), POINTER :: equilibrium => null()

!>  Grid start
         REAL (rprec)                          :: grid_start
!>  Grid stop
         REAL (rprec)                          :: grid_step
!>  Girded electron density profile.
         REAL (rprec), DIMENSION(:), POINTER   :: ne_grid => null()
!>  Grided electron soft x-ray profile.
         REAL (rprec), DIMENSION(:,:), POINTER :: sxrem_grid => null()
!>  Grided electron temperature profile.
         REAL (rprec), DIMENSION(:), POINTER   :: te_grid => null()
!>  Grided ion temperature profile.
         REAL (rprec), DIMENSION(:), POINTER   :: ti_grid => null()

!>  Soft X-ray emissivity tempurature.
         REAL (rprec), DIMENSION(:), POINTER :: sxrem_te => null()
!>  Soft X-ray emissivity ratio.
         REAL (rprec), DIMENSION(:), POINTER :: sxrem_ratio => null()

!>  ECE resonance range.
         REAL (rprec) :: resonace_range

!>  Model Signal factor
         REAL (rprec), DIMENSION(:), POINTER :: signal_factor => null()
!>  Model Signal offset
         REAL (rprec), DIMENSION(:), POINTER :: signal_offset => null()

!>  Integration parameters.
         TYPE (path_int_class), POINTER      :: int_params => null()
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the model density profile values.
!-------------------------------------------------------------------------------
      INTERFACE model_get_ne
         MODULE PROCEDURE model_get_ne_cart,                                   &
     &                    model_get_ne_radial
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the model guassian process density profile values.
!-------------------------------------------------------------------------------
      INTERFACE model_get_gp_ne
         MODULE PROCEDURE model_get_gp_ne_ij,                                  &
     &                    model_get_gp_ne_pi,                                  &
     &                    model_get_gp_ne_pp
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the model electron temperature profile values.
!-------------------------------------------------------------------------------
      INTERFACE model_get_te
         MODULE PROCEDURE model_get_te_cart,                                   &
     &                    model_get_te_radial
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the model guassian process electron temperature profile
!>  values.
!-------------------------------------------------------------------------------
      INTERFACE model_get_gp_te
         MODULE PROCEDURE model_get_gp_te_ij,                                  &
     &                    model_get_gp_te_pi,                                  &
     &                    model_get_gp_te_pp
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the model ion temperature profile values.
!-------------------------------------------------------------------------------
      INTERFACE model_get_ti
         MODULE PROCEDURE model_get_ti_cart,                                   &
     &                    model_get_ti_radial
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the model guassian process ion temperature profile values.
!-------------------------------------------------------------------------------
      INTERFACE model_get_gp_ti
         MODULE PROCEDURE model_get_gp_ti_ij,                                  &
     &                    model_get_gp_ti_pi,                                  &
     &                    model_get_gp_ti_pp
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the model soft x-ray emissivity profile values.
!-------------------------------------------------------------------------------
      INTERFACE model_get_sxrem
         MODULE PROCEDURE model_get_sxrem_cart,                                &
     &                    model_get_sxrem_radial
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the mdoel guassian process soft x-ray emissivity profile
!>  values.
!-------------------------------------------------------------------------------
      INTERFACE model_get_gp_sxrem
         MODULE PROCEDURE model_get_gp_sxrem_ij,                               &
     &                    model_get_gp_sxrem_pi,                               &
     &                    model_get_gp_sxrem_pp
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref model_class containing an @ref equilibrium object.
!>
!>  Allocates memory and initializes a @ref model_class object.
!>  @param[in] ne_type           Model type for the electron density.
!>  @param[in] sxrem_type        Model type for the soft x-ray emissivity.
!>  @param[in] te_type           Model type for the electron temperature.
!>  @param[in] ti_type           Model type for the ion temperature.
!>  @param[in] ne_unit           Scaleing of the electron denisty.
!>  @param[in] ne_min            Minimum electron density.
!>  @param[in] te_min            Minimum electron temperature.
!>  @param[in] ti_min            Minimum ion temperature.
!>  @param[in] sxrem_min         Minimum soft x-ray emission.
!>  @param[in] pressure_fraction Fraction of the pressure provided by electrons.
!>  @param[in] equilibrium       An instance of an @ref equilibrium.
!>  @param[in] sxrem_te          Soft x-ray ratio function te.
!>  @param[in] sxrem_ratio       Soft x-ray ratio function ratio.
!>  @param[in] resonace_range    ECE resonance interval.
!>  @param[in] coosig_wgts       Combination Signal Weights
!>  @param[in] not_converged     If true the equilibrium was constructed in an
!>                               unconverged state.
!>  @param[in] state_flags       Bitwise flags to indicate which parts of the
!>                               model changed.
!>  @param[in] signal_factor     Array of signal factors to scale model signals
!>                               by.
!>  @param[in] signal_offset     Array of signal factors to offset model
!>                               signals by.
!>  @param[in] int_params        Parameters for the integration method.
!>  @returns A pointer to a constructed @ref model_class object.
!-------------------------------------------------------------------------------
      FUNCTION model_construct(ne_type, sxrem_type, te_type, ti_type,          &
     &                         ne_unit, ne_min, te_min, ti_min,                &
     &                         sxrem_min, pressure_fraction,                   &
     &                         emission, equilibrium, sxrem_te,                &
     &                         sxrem_ratio, resonace_range, coosig_wgts,       &
     &                         state_flags, signal_factor,                     &
     &                         signal_offset, int_params)
      USE model_state

      IMPLICIT NONE

!  Declare Arguments
      TYPE (model_class), POINTER :: model_construct
      CHARACTER (len=data_name_length), INTENT(in)  :: ne_type
      CHARACTER (len=data_name_length), DIMENSION(:), INTENT(in) ::            &
     &   sxrem_type
      CHARACTER (len=data_name_length), INTENT(in)  :: te_type
      CHARACTER (len=data_name_length), INTENT(in)  :: ti_type
      REAL (rprec), INTENT(in)                      :: ne_unit
      REAL (rprec), INTENT(in)                      :: ne_min
      REAL (rprec), INTENT(in)                      :: te_min
      REAL (rprec), INTENT(in)                      :: ti_min
      REAL (rprec), DIMENSION(:), INTENT(in)        :: sxrem_min
      REAL (rprec), INTENT(in)                      :: pressure_fraction
      TYPE (emission_class), POINTER                :: emission
      TYPE (equilibrium_class), POINTER             :: equilibrium
      REAL (rprec), DIMENSION(:), INTENT(in)        :: sxrem_te
      REAL (rprec), DIMENSION(:), INTENT(in)        :: sxrem_ratio
      REAL (rprec), INTENT(in)                      :: resonace_range
      REAL (rprec), DIMENSION(:), INTENT(in)        :: coosig_wgts
      INTEGER, INTENT(in)                           :: state_flags
      REAL (rprec), DIMENSION(:)                    :: signal_factor
      REAL (rprec), DIMENSION(:)                    :: signal_offset
      TYPE (path_int_class), POINTER                :: int_params

!  local variables
      INTEGER                                       :: i
      INTEGER                                       :: grid_size
      REAL (rprec)                                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(model_construct)

      model_construct%state_flags = state_flags

      SELECT CASE (TRIM(ADJUSTL(ne_type)))

         CASE ('none')
            model_construct%ne_type = model_none_type

         CASE ('pp_ne')
            model_construct%ne_type = model_ne_type

         CASE ('pp_te_p')
            model_construct%ne_type = model_ne_te_p_type

         CASE DEFAULT
            WRITE (*,*) 'ne type: ' // TRIM(ADJUSTL(ne_type)) //               &
     &                  ' setting type to none.'
            model_construct%ne_type = model_none_type

      END SELECT

      ALLOCATE(model_construct%sxrem_type(SIZE(sxrem_type)))
      DO i = 1, SIZE(sxrem_type)

         SELECT CASE (TRIM(ADJUSTL(sxrem_type(i))))

            CASE ('none')
               model_construct%sxrem_type(i) = model_none_type

            CASE ('pp_sxrem')
               model_construct%sxrem_type(i) = model_sxrem_type

            CASE ('pp_sxrem_te_ne')
               model_construct%sxrem_type(i) =                                 &
     &            model_sxrem_te_ne_type

            CASE DEFAULT
               WRITE (*,*)                                                     &
     &            'sxrem type: ' // TRIM(ADJUSTL(sxrem_type(i))) //            &
     &            ' setting type to none.'
               model_construct%sxrem_type(i) = model_none_type

         END SELECT

      END DO

      SELECT CASE (TRIM(ADJUSTL(te_type)))

         CASE ('none')
            model_construct%te_type = model_none_type

         CASE ('pp_te')
            model_construct%te_type = model_te_type

!>  @todo FIXME: Name here shouldn't have vmec in it
         CASE ('pp_ne_vmec_p', 'pp_ne_p')
            model_construct%te_type = model_te_ne_p_type

         CASE DEFAULT
            WRITE (*,*) 'te type: ' // TRIM(ADJUSTL(te_type)) //               &
     &                  ' setting type to none.'
            model_construct%te_type = model_none_type

      END SELECT

      SELECT CASE (TRIM(ADJUSTL(ti_type)))

         CASE ('none')
            model_construct%ti_type = model_none_type

         CASE ('pp_ti')
            model_construct%ti_type = model_ti_type

         CASE DEFAULT
            WRITE (*,*) 'ti type: ' // TRIM(ADJUSTL(ti_type)) //               &
     &                  ' setting type to none.'
            model_construct%ti_type = model_none_type

      END SELECT

!  Cannot have the temperature derived from the denisty at the same time the
!  density is derived from the temperature.
      CALL assert(.not.(                                                       &
     &        (model_construct%te_type .eq. model_te_ne_p_type)                &
     &        .and.                                                            &
     &        (model_construct%ne_type .eq. model_ne_te_p_type)),              &
     &        'Cannot derive both the denisty and temperature' //              &
     &        ' from pressure in the same model.')

      model_construct%ne_unit = ne_unit
      model_construct%ne_min = ne_min
      model_construct%te_min = te_min
      model_construct%ti_min = ti_min
      ALLOCATE(model_construct%sxrem_min(SIZE(sxrem_min)))
      model_construct%sxrem_min = sxrem_min
      model_construct%pressure_fraction = pressure_fraction

      ALLOCATE(model_construct%coosig_wgts(SIZE(coosig_wgts)))
      DO i = 1, SIZE(coosig_wgts)
         model_construct%coosig_wgts(i)=coosig_wgts(i)
      ENDDO

      model_construct%emission => emission
      IF (ASSOCIATED(model_construct%emission)) THEN

      END IF

      model_construct%equilibrium => equilibrium

      grid_size = equilibrium%get_grid_size()

      IF (grid_size .gt. 0) THEN
         CALL model_set_grid_params(model_construct, grid_size)

         ALLOCATE(model_construct%ne_grid(grid_size))
         ALLOCATE(model_construct%sxrem_grid(grid_size,                        &
     &                                       SIZE(sxrem_type)))
         ALLOCATE(model_construct%te_grid(grid_size))
         ALLOCATE(model_construct%ti_grid(grid_size))
      END IF

!  Find the size of the sxrem ratio arrays.
      grid_size = MINLOC(sxrem_te(2:), dim=1)
      IF (grid_size .gt. 1) THEN
         ALLOCATE(model_construct%sxrem_te(grid_size))
         model_construct%sxrem_te = sxrem_te(1:grid_size)

         ALLOCATE(model_construct%sxrem_ratio(grid_size))
         model_construct%sxrem_ratio = sxrem_ratio(1:grid_size)
      END IF

      ALLOCATE(model_construct%signal_factor(SIZE(signal_factor)))
      model_construct%signal_factor = signal_factor

      ALLOCATE(model_construct%signal_offset(SIZE(signal_offset)))
      model_construct%signal_offset = signal_offset

      model_construct%resonace_range = resonace_range

      model_construct%int_params => int_params

      CALL profiler_set_stop_time('model_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref model_class object.
!>
!>  Deallocates memory and uninitializes a @ref model_class object.
!>
!>  @param[inout] this A @ref model_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE model_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (model_class), POINTER :: this

!  Start of executable code
      IF (ASSOCIATED(this%sxrem_type)) THEN
         DEALLOCATE(this%sxrem_type)
         this%sxrem_type => null()
      END IF

      IF (ASSOCIATED(this%emission)) THEN
         CALL emission_destruct(this%emission)
         this%emission => null()
      END IF

      IF (ASSOCIATED(this%equilibrium)) THEN
         DEALLOCATE(this%equilibrium)
         this%equilibrium => null()
      END IF

      IF (ASSOCIATED(this%sxrem_min)) THEN
         DEALLOCATE(this%sxrem_min)
         this%sxrem_min => null()
      END IF

      IF (ASSOCIATED(this%ne_grid)) THEN
         DEALLOCATE(this%ne_grid)
         this%ne_grid => null()
      END IF

      IF (ASSOCIATED(this%sxrem_grid)) THEN
         DEALLOCATE(this%sxrem_grid)
         this%sxrem_grid => null()
      END IF

      IF (ASSOCIATED(this%te_grid)) THEN
         DEALLOCATE(this%te_grid)
         this%te_grid => null()
      END IF

      IF (ASSOCIATED(this%ti_grid)) THEN
         DEALLOCATE(this%ti_grid)
         this%ti_grid => null()
      END IF

      IF (ASSOCIATED(this%sxrem_te)) THEN
         DEALLOCATE(this%sxrem_te)
         this%sxrem_te => null()
      END IF

      IF (ASSOCIATED(this%sxrem_ratio)) THEN
         DEALLOCATE(this%sxrem_ratio)
         this%sxrem_ratio => null()
      END IF

      IF (ASSOCIATED(this%coosig_wgts)) THEN
         DEALLOCATE(this%coosig_wgts)
         this%coosig_wgts => null()
      END IF

      IF (ASSOCIATED(this%signal_factor)) THEN
         DEALLOCATE(this%signal_factor)
         this%signal_factor => null()
      END IF

      IF (ASSOCIATED(this%signal_offset)) THEN
         DEALLOCATE(this%signal_offset)
         this%signal_offset => null()
      END IF

      IF (ASSOCIATED(this%int_params)) THEN
         DEALLOCATE(this%int_params)
         this%int_params => null()
      END IF

      DEALLOCATE(this)

      END SUBROUTINE

!*******************************************************************************
!  SETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Sets the value of a reconstruction model parameter.
!>
!>  Sets a model reconstruction parameter. If the id is not a model parameter,
!>  it is deferred to the @ref equilibrium.
!>
!>  @param[inout] this    A @ref model_class instance.
!>  @param[in]    id      ID of the parameter.
!>  @param[in]    i_index The ith index of the parameter.
!>  @param[in]    j_index The jth index of the parameter.
!>  @param[in]    value   The value of the parameter.
!>  @param[in]    eq_comm MPI communicator for the child equilibrium processes.
!>  @note Model parameter do not use the indicies. However an equilibrium might.
!-------------------------------------------------------------------------------
      SUBROUTINE model_set_param(this, id, i_index, j_index, value,            &
     &                           eq_comm)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (model_class), INTENT(inout) :: this
      INTEGER, INTENT(in)               :: id
      INTEGER, INTENT(in)               :: i_index
      INTEGER, INTENT(in)               :: j_index
      REAL (rprec), INTENT(in)          :: value
      INTEGER, INTENT(in)               :: eq_comm

!  local variables
      INTEGER                           :: i
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (id)

         CASE (model_ne_unit_id)
            this%state_flags = IBSET(this%state_flags,                         &
     &                               model_state_ne_flag)
            this%ne_unit = value

         CASE (model_ne_min_id)
            this%state_flags = IBSET(this%state_flags,                         &
     &                               model_state_ne_flag)
            this%ne_min = value

         CASE (model_te_min_id)
            this%state_flags = IBSET(this%state_flags,                         &
     &                               model_state_te_flag)
            this%te_min = value

         CASE (model_ti_min_id)
            this%state_flags = IBSET(this%state_flags,                         &
     &                               model_state_ti_flag)
            this%ti_min = value

         CASE (model_sxrem_min_id)
            this%state_flags = IBSET(this%state_flags,                         &
     &                               model_state_sxrem_flag +                  &
     &                               (i_index - 1))
            this%sxrem_min(i_index) = value

         CASE (model_pressure_fraction_id)
            IF (this%te_type .eq. model_te_ne_p_type) THEN
               this%state_flags = IBSET(this%state_flags,                      &
     &                                  model_state_te_flag)
            END IF
            IF (this%ne_type .eq. model_ne_te_p_type) THEN
               this%state_flags = IBSET(this%state_flags,                      &
     &                                  model_state_ne_flag)
            END IF
            this%pressure_fraction = value

         CASE (model_coosig_wgts_id)
            this%coosig_wgts(i_index) = value

         CASE (model_signal_factor_id)
            this%state_flags = IBSET(this%state_flags,                         &
     &                               model_state_signal_flag)
            this%signal_factor(i_index) = value

         CASE (model_signal_offset_id)
            this%state_flags = IBSET(this%state_flags,                         &
     &                               model_state_signal_flag)
            this%signal_offset(i_index) = value

         CASE DEFAULT
            CALL this%equilibrium%set_param(id, i_index, j_index,              &
     &                                      value, eq_comm,                    &
     &                                      this%state_flags)

      END SELECT

!  Some parts of the model depend on different parts of the model. Update the
!  state flags to refect these changes.

      IF ((this%ne_type .eq. model_ne_te_p_type) .and.                         &
     &    (BTEST(this%state_flags, model_state_te_flag))) THEN
         this%state_flags = IBSET(this%state_flags, model_state_ne_flag)
      END IF

!  There are multiple sxrem profiles. State flags bit position needs to be
!  off set by the i index.
      DO i = 1, SIZE(this%sxrem_type)
         IF ((this%sxrem_type(i) .eq. model_sxrem_te_ne_type) .and.            &
     &       (BTEST(this%state_flags, model_state_te_flag) .or.                &
     &        BTEST(this%state_flags, model_state_ne_flag))) THEN
            this%state_flags = IBSET(this%state_flags,                         &
     &                               model_state_sxrem_flag + (i - 1))
         END IF
      END DO

      IF ((this%te_type .eq. model_te_ne_p_type) .and.                         &
     &    (BTEST(this%state_flags, model_state_ne_flag))) THEN
         this%state_flags = IBSET(this%state_flags, model_state_te_flag)
      END IF

      CALL profiler_set_stop_time('model_set_param', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Sets grid parameters.
!>
!>  Sets the start and step size of the radial grids.
!>
!>  @param[inout] this A @ref model_class instance.
!>  @param[in]    size Number of grid points.
!-------------------------------------------------------------------------------
      SUBROUTINE model_set_grid_params(this, size)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (model_class), INTENT(inout) :: this
      INTEGER, INTENT(in)               :: size

!  local variables
      REAL (rprec)                      :: grid_stop
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      this%grid_start = this%equilibrium%get_grid_start()
      grid_stop = this%equilibrium%get_grid_stop()

      this%grid_step = (grid_stop - this%grid_start)/(size - 1)

      CALL profiler_set_stop_time('model_set_grid_params', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Sets grid profile values.
!>
!>  Sets the grided profiles from the current equilibrium mode.
!>
!>  @param[inout] this A @ref model_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE model_set_grid_profiles(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (model_class), INTENT(inout) :: this

!  local variables
      INTEGER                           :: i
      INTEGER                           :: j
      REAL (rprec)                      :: r
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%ne_grid) .and.                                       &
     &    BTEST(this%state_flags, model_state_ne_flag)) THEN
         DO i = 1, SIZE(this%ne_grid)
            r = (i - 1)*this%grid_step + this%grid_start
            this%ne_grid(i) = model_get_ne(this, r)
         END DO
      END IF

      IF (ASSOCIATED(this%sxrem_grid)) THEN
         DO j = 1, SIZE(this%sxrem_grid, 2)
            IF (BTEST(this%state_flags,                                        &
     &                model_state_sxrem_flag + (j - 1))) THEN
               DO i = 1, SIZE(this%sxrem_grid, 1)
                  r = (i - 1)*this%grid_step + this%grid_start
                  this%sxrem_grid(i,j) = model_get_sxrem(this, r, j)
               END DO
            END IF
         END DO
      END IF

      IF (ASSOCIATED(this%te_grid) .and.                                       &
     &    BTEST(this%state_flags, model_state_te_flag)) THEN
         DO i = 1, SIZE(this%te_grid)
            r = (i - 1)*this%grid_step + this%grid_start
            this%te_grid(i) = model_get_te(this, r)
         END DO
      END IF

      IF (ASSOCIATED(this%ti_grid) .and.                                       &
     &    BTEST(this%state_flags, model_state_ti_flag)) THEN
         DO i = 1, SIZE(this%ti_grid)
            r = (i - 1)*this%grid_step + this%grid_start
            this%ti_grid(i) = model_get_ti(this, r)
         END DO
      END IF

      CALL profiler_set_stop_time('model_set_grid_params', start_time)

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Get the id for a parameter.
!>
!>  Gets a model reconstruction parameter. If the id is not a model parameter,
!>  it is deferred to the @ref equilibrium.
!>
!>  @param[in] this       A @ref model_class instance.
!>  @param[in] param_name Name of a reconstruction parameter.
!>  @returns The id for a reconstruction parameter.
!-------------------------------------------------------------------------------
      FUNCTION model_get_param_id(this, param_name)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER                        :: model_get_param_id
      TYPE (model_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)  :: param_name

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (TRIM(param_name))

         CASE ('ne_unit')
            model_get_param_id = model_ne_unit_id

         CASE ('ne_min')
            model_get_param_id = model_ne_min_id

         CASE ('te_min')
            model_get_param_id = model_te_min_id

         CASE ('ti_min')
            model_get_param_id = model_ti_min_id

         CASE ('sxrem_min')
            model_get_param_id = model_sxrem_min_id

         CASE ('pressure_fraction', 'e_pressure_fraction')
            model_get_param_id = model_pressure_fraction_id

         CASE ('signal_factor', 'sfactor_spec_fac')
            model_get_param_id = model_signal_factor_id

         CASE ('signal_offset', 'soffset_spec_fac')
            model_get_param_id = model_signal_offset_id

         CASE ('ne_grid')
            model_get_param_id = model_ne_grid_id

         CASE ('te_grid')
            model_get_param_id = model_te_grid_id

         CASE ('ti_grid')
            model_get_param_id = model_ti_grid_id

         CASE ('sxrem_grid')
            model_get_param_id = model_sxrem_grid_id

         CASE ('coosig_wgts')
            model_get_param_id = model_coosig_wgts_id

         CASE DEFAULT
            model_get_param_id =                                               &
     &         this%equilibrium%get_param_id(param_name)

      END SELECT

      CALL profiler_set_stop_time('model_get_param_id', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the value of a model parameter.
!>
!>  Gets a model reconstruction parameter value. If the id is not a model
!>  parameter, it is deferred to the @ref equilibrium.
!>
!>  @param[in] this    A @ref model_class instance.
!>  @param[in] id      ID of the parameter.
!>  @param[in] i_index The ith index of the parameter.
!>  @param[in] j_index The jth index of the parameter.
!>  @returns The value of the parameter.
!>  @note Model parameter do not use the indicies. However an equilibrium might.
!-------------------------------------------------------------------------------
      FUNCTION model_get_param_value(this, id, i_index, j_index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                   :: model_get_param_value
      TYPE (model_class), INTENT(in) :: this
      INTEGER, INTENT(in)            :: id
      INTEGER, INTENT(in)            :: i_index
      INTEGER, INTENT(in)            :: j_index

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (id)

         CASE (model_ne_unit_id)
            model_get_param_value = this%ne_unit

         CASE (model_ne_min_id)
            model_get_param_value = this%ne_min

         CASE (model_te_min_id)
            model_get_param_value = this%te_min

         CASE (model_ti_min_id)
            model_get_param_value = this%ti_min

         CASE (model_sxrem_min_id)
            model_get_param_value = this%sxrem_min(i_index)

         CASE (model_pressure_fraction_id)
            model_get_param_value = this%pressure_fraction

         CASE (model_signal_factor_id)
            model_get_param_value = this%signal_factor(i_index)

         CASE (model_signal_offset_id)
            model_get_param_value = this%signal_offset(i_index)

!  Not every equilibrium will provide a grid for these profiles.
         CASE (model_ne_grid_id)
            IF (ASSOCIATED(this%ne_grid)) THEN
               model_get_param_value = this%ne_grid(i_index)
            ELSE
               model_get_param_value = 0.0
            END IF

         CASE (model_te_grid_id)
            IF (ASSOCIATED(this%te_grid)) THEN
               model_get_param_value = this%te_grid(i_index)
            ELSE
               model_get_param_value = 0.0
            END IF

         CASE (model_ti_grid_id)
            IF (ASSOCIATED(this%ti_grid)) THEN
               model_get_param_value = this%ti_grid(i_index)
            ELSE
               model_get_param_value = 0.0
            END IF

         CASE (model_sxrem_grid_id)
            IF (ASSOCIATED(this%sxrem_grid)) THEN
               model_get_param_value = this%sxrem_grid(i_index,                &
     &                                                 j_index)
            ELSE
               model_get_param_value = 0.0
            END IF

         CASE (model_coosig_wgts_id)
            IF (ASSOCIATED(this%coosig_wgts)) THEN
               model_get_param_value = this%coosig_wgts(i_index)
            ELSE
               model_get_param_value = 0.0
            END IF

         CASE DEFAULT
            model_get_param_value =                                            &
     &         this%equilibrium%get_param_value(id, i_index, j_index)

      END SELECT

      CALL profiler_set_stop_time('model_get_param_value', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the name of a model parameter.
!>
!>  Gets a model reconstruction parameter name. If the id is not a model
!>  parameter, it is deferred to the @ref equilibrium.
!>
!>  @param[in] this A @ref model_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns The name of the parameter.
!-------------------------------------------------------------------------------
      FUNCTION model_get_param_name(this, id)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER(len=data_name_length) :: model_get_param_name
      TYPE (model_class), INTENT(in)  :: this
      INTEGER, INTENT(in)             :: id

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (id)

         CASE (model_ne_unit_id)
            model_get_param_name = 'ne_unit'

         CASE (model_ne_min_id)
            model_get_param_name = 'ne_min'

         CASE (model_te_min_id)
            model_get_param_name = 'te_min'

         CASE (model_ti_min_id)
            model_get_param_name = 'ti_min'

          CASE (model_sxrem_min_id)
             model_get_param_name = 'sxrem_min'

         CASE (model_pressure_fraction_id)
            model_get_param_name = 'pressure_fraction'

         CASE (model_signal_factor_id)
            model_get_param_name = 'signal_factor'

         CASE (model_signal_offset_id)
            model_get_param_name = 'signal_offset'

         CASE (model_ne_grid_id)
            model_get_param_name = 'ne_grid'

         CASE (model_te_grid_id)
            model_get_param_name = 'te_grid'

         CASE (model_ti_grid_id)
            model_get_param_name = 'ti_grid'

         CASE (model_sxrem_grid_id)
            model_get_param_name = 'sxrem_grid'

         CASE (model_coosig_wgts_id)
            model_get_param_name = 'coosig_wgts'

         CASE DEFAULT
            model_get_param_name =                                             &
     &         this%equilibrium%get_param_name(id)

      END SELECT

      CALL profiler_set_stop_time('model_get_param_name', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the number of electron density gp kernel hyper parameters.
!>
!>  Returns the number of availible hyper parameters in the electron density
!>  guassian process kernel.
!>
!>  @param[in] this A @ref model_class instance.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_ne_num_hyper_param(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER                        :: model_get_gp_ne_num_hyper_param
      TYPE (model_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_ne_num_hyper_param =                                        &
     &   this%equilibrium%get_gp_ne_num_hyper_param()

      CALL profiler_set_stop_time('model_get_gp_ne_num_hyper_param',           &
     &                            start_time)

       END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron density profile af array.
!>
!>  Returns the af array for the electron density profile.
!>
!>  @param[in] this A @ref model_class instance.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION model_get_ne_af(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER :: model_get_ne_af
      TYPE (model_class), INTENT(in)      :: this

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_ne_af => this%equilibrium%get_ne_af()

      CALL profiler_set_stop_time('model_get_ne_af', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron density gp kernel value for the two indicies.
!>
!>  Gets the electron density gp kernel for two points. This also sets the
!>  scaling and min values.
!>
!>  @param[in] this A @ref model_class instance.
!>  @param[in] i    ith profile position.
!>  @param[in] j    jth profile position.
!>  @returns The value of the gp kernel function for i, j.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_ne_ij(this, i, j)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                   :: model_get_gp_ne_ij
      TYPE (model_class), INTENT(in) :: this
      INTEGER, INTENT(in)            :: i
      INTEGER, INTENT(in)            :: j

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_ne_ij = this%ne_unit                                        &
     &                   * this%equilibrium%get_gp_ne(i, j)
      model_get_gp_ne_ij = MAX(model_get_gp_ne_ij, this%ne_min)

      CALL profiler_set_stop_time('model_get_gp_ne_ij', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron density gp kernel value for the position and index.
!>
!>  Gets the electron density gp kernel for point and index. This also sets the
!>  scaling and min values.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @param[in] x_cart Cartesian position to get the electron density at.
!>  @param[in] i      ith profile position.
!>  @returns The value of the gp kernel function for x_cart and i.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_ne_pi(this, x_cart, i)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: model_get_gp_ne_pi
      TYPE (model_class), INTENT(in)         :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: i

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_ne_pi = this%ne_unit                                        &
     &                   * this%equilibrium%get_gp_ne(x_cart, i)
      model_get_gp_ne_pi = MAX(model_get_gp_ne_pi, this%ne_min)

      CALL profiler_set_stop_time('model_get_gp_ne_pi', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron density gp kernel value for the position and
!>  position.
!>
!>  Gets the electron density gp kernel for point and index. This also sets the
!>  scaling and min values.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @param[in] x_cart Cartesian position to get the kernel at.
!>  @param[in] y_cart Cartesian position to get the kernel at.
!>  @returns The value of the gp kernel function for x_cart and y_cart.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_ne_pp(this, x_cart, y_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: model_get_gp_ne_pp
      TYPE (model_class), INTENT(in)         :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      REAL (rprec), DIMENSION(3), INTENT(in) :: y_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_ne_pp = this%ne_unit                                        &
     &                   * this%equilibrium%get_gp_ne(x_cart, y_cart)
      model_get_gp_ne_pp = MAX(model_get_gp_ne_pp, this%ne_min)

      CALL profiler_set_stop_time('model_get_gp_ne_pp', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron density at a cartesian position.
!>
!>  Gets the electron density at a cartesian position. Electron density is
!>  computed based on the type of @ref model_class::ne_type.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @param[in] x_cart Cartesian position to get the electron density at.
!>  @returns The electron density at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION model_get_ne_cart(this, x_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: model_get_ne_cart
      TYPE (model_class), INTENT(in)         :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%ne_type)

         CASE (model_ne_type)
            model_get_ne_cart = this%equilibrium%get_ne(x_cart)
            model_get_ne_cart = MAX(this%ne_min,                               &
     &                              this%ne_unit*model_get_ne_cart)

         CASE (model_ne_te_p_type)
!  Electron temperature of zero can cause divide by zero errors. Check the value
!  of the temperature first. If it is zero return zero density.
            model_get_ne_cart = model_get_te(this, x_cart)
            IF (model_get_ne_cart .gt. 0.0) THEN
               model_get_ne_cart = eV_per_Joule*this%pressure_fraction         &
     &                           * this%equilibrium%get_p(x_cart,              &
     &                                                    .false.)             &
     &                           / model_get_ne_cart
            END IF

         CASE DEFAULT
            model_get_ne_cart = 0.0

      END SELECT

      CALL profiler_set_stop_time('model_get_ne_cart', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron density at a radial position.
!>
!>  Gets the electron density at a radial position. Electron density is computed
!>  based on the type of @ref model_class::ne_type.
!>
!>  @param[in] this A @ref model_class instance.
!>  @param[in] s    Radial position to get the electron density at.
!>  @returns The electron density at r.
!-------------------------------------------------------------------------------
      FUNCTION model_get_ne_radial(this, s)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                   :: model_get_ne_radial
      TYPE (model_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)       :: s

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%ne_type)

         CASE (model_ne_type)
            model_get_ne_radial = this%equilibrium%get_ne(s)
            model_get_ne_radial = MAX(this%ne_min,                             &
     &                                this%ne_unit*model_get_ne_radial)

         CASE (model_ne_te_p_type)
!  Electron temperature of zero can cause divide by zero errors. Check the value
!  of the temperature first. If it is zero return zero density.
            model_get_ne_radial = model_get_te(this, s)
            IF (model_get_ne_radial .gt. 0.0) THEN
               model_get_ne_radial = eV_per_Joule*this%pressure_fraction       &
     &                             * this%equilibrium%get_p(s, .false.)        &
     &                             / model_get_ne_radial
            END IF

         CASE DEFAULT
            model_get_ne_radial = 0.0_rprec

      END SELECT

      CALL profiler_set_stop_time('model_get_ne_radial', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the number of electron temperature gp kernel hyper parameters.
!>
!>  Returns the number of availible hyper parameters in the electron temperature
!>  guassian process kernel.
!>
!>  @param[in] this A @ref model_class instance.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_te_num_hyper_param(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER                        :: model_get_gp_te_num_hyper_param
      TYPE (model_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_te_num_hyper_param =                                        &
     &   this%equilibrium%get_gp_te_num_hyper_param()

      CALL profiler_set_stop_time('model_get_gp_te_num_hyper_param',           &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron temperature profile af array.
!>
!>  Returns the af array for the electron temperature profile.
!>
!>  @param[in] this A @ref model_class instance.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION model_get_te_af(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER :: model_get_te_af
      TYPE (model_class), INTENT(in)      :: this

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_te_af => this%equilibrium%get_te_af()

      CALL profiler_set_stop_time('model_get_te_af', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron temperature gp kernel value for the two indicies.
!>
!>  Gets the electron temperature gp kernel for two points. This also sets the
!>  min value.
!>
!>  @param[in] this A @ref model_class instance.
!>  @param[in] i    ith profile position.
!>  @param[in] j    jth profile position.
!>  @returns The value of the gp kernel function for i, j.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_te_ij(this, i, j)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                   :: model_get_gp_te_ij
      TYPE (model_class), INTENT(in) :: this
      INTEGER, INTENT(in)            :: i
      INTEGER, INTENT(in)            :: j

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_te_ij = this%equilibrium%get_gp_te(i, j)
      model_get_gp_te_ij = MAX(model_get_gp_te_ij, this%te_min)

      CALL profiler_set_stop_time('model_get_gp_te_ij', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron temperature gp kernel value for the position and
!>  index.
!>
!>  Gets the electron temperature gp kernel for point and index. This also sets
!>  the scaling and min values.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @param[in] x_cart Cartesian position to get the electron temperature at.
!>  @param[in] i      ith profile position.
!>  @returns The value of the gp kernel function for x_cart and i.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_te_pi(this, x_cart, i)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: model_get_gp_te_pi
      TYPE (model_class), INTENT(in)         :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: i

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_te_pi = this%equilibrium%get_gp_te(x_cart, i)
      model_get_gp_te_pi = MAX(model_get_gp_te_pi, this%te_min)

      CALL profiler_set_stop_time('model_get_gp_te_pi', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron temperature gp kernel value for the position and
!>  position.
!>
!>  Gets the electron temperature gp kernel for point and index.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @param[in] x_cart Cartesian position to get the kernel at.
!>  @param[in] y_cart Cartesian position to get the kernel at.
!>  @returns The value of the gp kernel function for x_cart and y_cart.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_te_pp(this, x_cart, y_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: model_get_gp_te_pp
      TYPE (model_class), INTENT(in)         :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      REAL (rprec), DIMENSION(3), INTENT(in) :: y_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_te_pp = this%equilibrium%get_gp_te_pp(x_cart, y_cart)
      model_get_gp_te_pp = MAX(model_get_gp_te_pp, this%te_min)

      CALL profiler_set_stop_time('model_get_gp_te_pp', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron temperature at a cartesian position.
!>
!>  Gets the electron temperature at a cartesian position. Electron temperature
!>  is computed based on the type of @ref model_class::te_type.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @param[in] x_cart Cartesian position to get the electron temperature at.
!>  @returns The electron temperature at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION model_get_te_cart(this, x_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: model_get_te_cart
      TYPE (model_class), INTENT(in)         :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%te_type)

         CASE (model_te_type)
            model_get_te_cart = this%equilibrium%get_te(x_cart)
            model_get_te_cart = MAX(this%te_min, model_get_te_cart)

         CASE (model_te_ne_p_type)
!  Electron densities of zero can cause divide by zero errors. Check the value
!  of the density first. If it is zero return zero temperature.
            model_get_te_cart = model_get_ne(this, x_cart)
            IF (model_get_te_cart .gt. 0.0) THEN
               model_get_te_cart = eV_per_Joule*this%pressure_fraction         &
     &                           * this%equilibrium%get_p(x_cart,              &
     &                                                    .false.)             &
     &                           / model_get_te_cart
            END IF

         CASE DEFAULT
            model_get_te_cart = 0.0_rprec

      END SELECT

      CALL profiler_set_stop_time('model_get_te_cart', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron temperature at a radial position.
!>
!>  Gets the electron temperature at a radial position. Electron temperature is
!>  computed based on the type of @ref model_class::te_type.
!>
!>  @param[in] this A @ref model_class instance.
!>  @param[in] s    Radial position to get the electron temperature at.
!>  @returns The electron temperature at r.
!-------------------------------------------------------------------------------
      FUNCTION model_get_te_radial(this, s)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                   :: model_get_te_radial
      TYPE (model_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)       :: s

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%te_type)

         CASE (model_te_type)
            model_get_te_radial = this%equilibrium%get_te(s)
            model_get_te_radial = MAX(this%te_min, model_get_te_radial)

         CASE (model_te_ne_p_type)
!  Electron densities of zero can cause divide by zero errors. Check the value
!  of the density first. If it is zero return zero temperature.
            model_get_te_radial = model_get_ne(this, s)
            IF (model_get_te_radial .gt. 0.0) THEN
               model_get_te_radial = eV_per_Joule*this%pressure_fraction       &
     &                             * this%equilibrium%get_p(s, .false.)        &
     &                             / model_get_te_radial
            END IF

         CASE DEFAULT
            model_get_te_radial = 0.0_rprec

      END SELECT

      CALL profiler_set_stop_time('model_get_te_radial', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the number of ion temperature gp kernel hyper parameters.
!>
!>  Returns the number of availible hyper parameters in the ion temperature
!>  guassian process kernel.
!>
!>  @param[in] this A @ref model_class instance.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_ti_num_hyper_param(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER                        :: model_get_gp_ti_num_hyper_param
      TYPE (model_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_ti_num_hyper_param =                                        &
     &   this%equilibrium%get_gp_ti_num_hyper_param()

      CALL profiler_set_stop_time('model_get_gp_ti_num_hyper_param',           &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the ion temperature profile af array.
!>
!>  Returns the af array for the ion temperature profile.
!>
!>  @param[in] this A @ref model_class instance.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION model_get_ti_af(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER :: model_get_ti_af
      TYPE (model_class), INTENT(in)      :: this

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_ti_af => this%equilibrium%get_ti_af()

      CALL profiler_set_stop_time('model_get_ti_af', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the ion temperature gp kernel value for the two indicies.
!>
!>  Gets the ion temperature gp kernel for two points. This also sets the min
!>  value.
!>
!>  @param[in] this A @ref model_class instance.
!>  @param[in] i    ith profile position.
!>  @param[in] j    jth profile position.
!>  @returns The value of the gp kernel function for i, j.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_ti_ij(this, i, j)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                   :: model_get_gp_ti_ij
      TYPE (model_class), INTENT(in) :: this
      INTEGER, INTENT(in)            :: i
      INTEGER, INTENT(in)            :: j

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_ti_ij = this%equilibrium%get_gp_ti(i, j)
      model_get_gp_ti_ij = MAX(model_get_gp_ti_ij, this%ti_min)

      CALL profiler_set_stop_time('model_get_gp_ti_ij', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the ion temperature gp kernel value for the position and index.
!>
!>  Gets the ion temperature gp kernel for point and index. This also sets
!>  the scaling and min values.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @param[in] x_cart Cartesian position to get the ion temperature at.
!>  @param[in] i      ith profile position.
!>  @returns The value of the gp kernel function for x_cart and i.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_ti_pi(this, x_cart, i)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: model_get_gp_ti_pi
      TYPE (model_class), INTENT(in)         :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: i

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_ti_pi = this%equilibrium%get_gp_ti(x_cart, i)
      model_get_gp_ti_pi = MAX(model_get_gp_ti_pi, this%ti_min)

      CALL profiler_set_stop_time('model_get_gp_ti_pi', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron temperature gp kernel value for the position and
!>  position.
!>
!>  Gets the electron temperature gp kernel for point and index.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @param[in] x_cart Cartesian position to get the kernel at.
!>  @param[in] y_cart Cartesian position to get the kernel at.
!>  @returns The value of the gp kernel function for x_cart and y_cart.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_ti_pp(this, x_cart, y_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: model_get_gp_ti_pp
      TYPE (model_class), INTENT(in)         :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      REAL (rprec), DIMENSION(3), INTENT(in) :: y_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_ti_pp = this%equilibrium%get_gp_ti(x_cart, y_cart)
      model_get_gp_ti_pp = MAX(model_get_gp_ti_pp, this%ti_min)

      CALL profiler_set_stop_time('model_get_gp_ti_pp', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the ion temperature at a cartesian position.
!>
!>  Gets the ion temperature at a cartesian position. Ion temperature is
!>  computed based on the type of @ref model_class::ti_type.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @param[in] x_cart Cartesian position to get the ion temperature at.
!>  @returns The ion temperature at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION model_get_ti_cart(this, x_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: model_get_ti_cart
      TYPE (model_class), INTENT(in)         :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%ti_type)

         CASE (model_ti_type)
            model_get_ti_cart = this%equilibrium%get_ti(x_cart)
            model_get_ti_cart = MAX(this%ti_min, model_get_ti_cart)

         CASE DEFAULT
            model_get_ti_cart = 0.0_rprec

      END SELECT

      CALL profiler_set_stop_time('model_get_ti_cart', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the ion temperature at a radial position.
!>
!>  Gets the ion temperature at a radial position. Electron temperature is
!>  computed based on the type of @ref model_class::ti_type.
!>
!>  @param[in] this A @ref model_class instance.
!>  @param[in] s    Radial position to get the ion temperature at.
!>  @returns The ion temperature at r.
!-------------------------------------------------------------------------------
      FUNCTION model_get_ti_radial(this, s)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                   :: model_get_ti_radial
      TYPE (model_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)       :: s

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%ti_type)

         CASE (model_ti_type)
            model_get_ti_radial = this%equilibrium%get_ti(s)
            model_get_ti_radial = MAX(this%ti_min, model_get_ti_radial)

         CASE DEFAULT
            model_get_ti_radial = 0.0_rprec

      END SELECT

      CALL profiler_set_stop_time('model_get_ti_radial', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the number of soft x-ray emission gp kernel hyper parameters.
!>
!>  Returns the number of availible hyper parameters in the soft x-ray emission
!>  guassian process kernel.
!>
!>  @param[in] this  A @ref model_class instance.
!>  @param[in] index Index of the soft x-ray emissivity profile.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_sxrem_num_hyper_param(this, index)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: model_get_gp_sxrem_num_hyper_param
      TYPE (model_class), INTENT(in) :: this
      INTEGER, INTENT(in)            :: index

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_sxrem_num_hyper_param =                                     &
     &   this%equilibrium%get_gp_sxrem_num_hyper_param(index)

      CALL profiler_set_stop_time('model_get_gp_sxrem_num_hyper_param',        &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the soft x-ray emissivity profile af array.
!>
!>  Returns the af array for the electron density profile.
!>
!>  @param[in] this  A @ref model_class instance.
!>  @param[in] index Index of the soft x-ray emissivity profile.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION model_get_sxrem_af(this, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER :: model_get_sxrem_af
      TYPE (model_class), INTENT(in)      :: this
      INTEGER, INTENT(in)                 :: index

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_sxrem_af => this%equilibrium%get_sxrem_af(index)

      CALL profiler_set_stop_time('model_get_sxrem_af', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the soft x-ray emissivity gp kernel value for the two indicies.
!>
!>  Gets the soft x-ray emissivity gp kernel for two points. This also sets the
!>  min value.
!>
!>  @param[in] this  A @ref model_class instance.
!>  @param[in] i     ith profile position.
!>  @param[in] j     jth profile position.
!>  @param[in] index Index of the soft x-ray emissivity profile.
!>  @returns The value of the gp kernel function for i, j.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_sxrem_ij(this, i, j, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                   :: model_get_gp_sxrem_ij
      TYPE (model_class), INTENT(in) :: this
      INTEGER, INTENT(in)            :: i
      INTEGER, INTENT(in)            :: j
      INTEGER, INTENT(in)            :: index

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_sxrem_ij =                                                  &
     &   this%equilibrium%get_gp_sxrem(i, j, index)
      model_get_gp_sxrem_ij = MAX(model_get_gp_sxrem_ij,                       &
     &                            this%sxrem_min(index))

      CALL profiler_set_stop_time('model_get_gp_sxrem_ij', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the soft x-ray emissivity gp kernel value for the position and
!>  index.
!>
!>  Gets the soft x-ray emissivity gp kernel for point and index. This also sets
!>  the scaling and min values.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @param[in] x_cart Cartesian position to get the electron temperature at.
!>  @param[in] i      ith profile position.
!>  @param[in] index  Index of the soft x-ray emissivity profile.
!>  @returns The value of the gp kernel function for x_cart and i.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_sxrem_pi(this, x_cart, i, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: model_get_gp_sxrem_pi
      TYPE (model_class), INTENT(in)         :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: i
      INTEGER, INTENT(in)                    :: index

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_sxrem_pi =                                                  &
     &   this%equilibrium%get_gp_sxrem(x_cart, i, index)
      model_get_gp_sxrem_pi = MAX(model_get_gp_sxrem_pi,                       &
     &                            this%sxrem_min(index))

      CALL profiler_set_stop_time('model_get_gp_sxrem_pi', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the soft x-ray emissivity gp kernel value for the position and
!>  position.
!>
!>  Gets the soft x-ray emissivity gp kernel for point and index. This also sets
!>  the min value.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @param[in] x_cart Cartesian position to get the kernel at.
!>  @param[in] y_cart Cartesian position to get the kernel at.
!>  @param[in] index  Index of the soft x-ray emissivity profile to use.
!>  @returns The value of the gp kernel function for x_cart and y_cart.
!-------------------------------------------------------------------------------
      FUNCTION model_get_gp_sxrem_pp(this, x_cart, y_cart, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: model_get_gp_sxrem_pp
      TYPE (model_class), INTENT(in)         :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      REAL (rprec), DIMENSION(3), INTENT(in) :: y_cart
      INTEGER, INTENT(in)                    :: index

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      model_get_gp_sxrem_pp =                                                  &
     &   this%equilibrium%get_gp_sxrem(x_cart, y_cart, index)
      model_get_gp_sxrem_pp = MAX(model_get_gp_sxrem_pp,                       &
     &                            this%sxrem_min(index))

      CALL profiler_set_stop_time('model_get_gp_sxrem_pp', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the soft x-ray emissivity at a cartesian position.
!>
!>  Gets the soft x-ray emissivity at a cartesian position. Soft x-ray
!>  emissivity is computed based on the type of @ref model_class::sxrem_type.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @param[in] x_cart Cartesian position to get the soft x-ray emissivity at.
!>  @param[in] index  Index of the soft x-ray emissivity model.
!>  @returns The soft x-ray emissivity at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION model_get_sxrem_cart(this, x_cart, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: model_get_sxrem_cart
      TYPE (model_class), INTENT(in)         :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: index

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%sxrem_type(index))

         CASE (model_sxrem_type)
            model_get_sxrem_cart =                                             &
     &         this%equilibrium%get_sxrem(x_cart, index)
            model_get_sxrem_cart = MAX(model_get_sxrem_cart,                   &
     &                                 this%sxrem_min(index))

         CASE (model_sxrem_te_ne_type)
            model_get_sxrem_cart =                                             &
     &         emission_get_emission(this%emission,                            &
     &                               model_get_te(this, x_cart),               &
     &                               model_get_ne(this, x_cart), index)

         CASE DEFAULT
            model_get_sxrem_cart = 0.0_rprec

      END SELECT

      CALL profiler_set_stop_time('model_get_sxrem_cart', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the soft x-ray emissivity at a radial position.
!>
!>  Gets the soft x-ray emissivity at a radial position. Soft x-ray emissivity
!>  is computed based on the type of @ref model_class::sxrem_type.
!>
!>  @param[in] this  A @ref model_class instance.
!>  @param[in] s     Radial position to get the soft x-ray emissivity at.
!>  @param[in] index Index of the soft x-ray emissivity model.
!>  @returns The soft x-ray emissivity at r.
!-------------------------------------------------------------------------------
      FUNCTION model_get_sxrem_radial(this, s, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                   :: model_get_sxrem_radial
      TYPE (model_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)       :: s
      INTEGER, INTENT(in)            :: index

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%sxrem_type(index))

         CASE (model_sxrem_type)
            model_get_sxrem_radial =                                           &
     &         this%equilibrium%get_sxrem(s, index)
            model_get_sxrem_radial = MAX(model_get_sxrem_radial,               &
     &                                   this%sxrem_min(index))

         CASE (model_sxrem_te_ne_type)
            model_get_sxrem_radial =                                           &
     &         emission_get_emission(this%emission,                            &
     &                               model_get_te(this, s),                    &
     &                               model_get_ne(this, s), index)

         CASE DEFAULT
            model_get_sxrem_radial = 0.0_rprec

      END SELECT

      CALL profiler_set_stop_time('model_get_sxrem_radial', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the soft x-ray emissivity ratio.
!>
!>  Gets the soft x-ray emissivity at a radial position. Soft x-ray emissivity
!>  is computed based on the type of @ref model_class::sxrem_type.
!>
!>  @param[in] this  A @ref model_class instance.
!>  @param[in] r     Radial position to get the soft x-ray emissivity at.
!>  @param[in] length Distance to the camera.
!>  @param[in] index Index of the soft x-ray emissivity model.
!>  @returns The soft x-ray emissivity at r.
!-------------------------------------------------------------------------------
      FUNCTION model_get_sxrem_ratio(this, te)
      USE line_segment

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                   :: model_get_sxrem_ratio
      TYPE (model_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)       :: te

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL line_seg(te, model_get_sxrem_ratio, this%sxrem_te,                  &
     &              this%sxrem_ratio, SIZE(this%sxrem_te))

      CALL profiler_set_stop_time('model_get_sxrem_ratio', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron density type as a string.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @returns A string description of the electron density type.
!-------------------------------------------------------------------------------
      FUNCTION model_get_ne_type(this)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: model_get_ne_type
      TYPE (model_class), INTENT(in)   :: this

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%ne_type)

         CASE (model_ne_type)
            model_get_ne_type = 'pp_ne'

         CASE (model_ne_te_p_type)
            model_get_ne_type = 'pp_te_p'

         CASE DEFAULT
            model_get_ne_type = 'none'

      END SELECT

      CALL profiler_set_stop_time('model_get_ne_type', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron temperature type as a string.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @returns A string description of the electron temperature type.
!-------------------------------------------------------------------------------
      FUNCTION model_get_te_type(this)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: model_get_te_type
      TYPE (model_class), INTENT(in)   :: this

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%te_type)

         CASE (model_te_type)
            model_get_te_type = 'pp_te'

         CASE (model_te_ne_p_type)
            model_get_te_type = 'pp_ne_p'

         CASE DEFAULT
            model_get_te_type = 'none'

      END SELECT

      CALL profiler_set_stop_time('model_get_te_type', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the ion temperature type as a string.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @returns A string description of the ion temperature type.
!-------------------------------------------------------------------------------
      FUNCTION model_get_ti_type(this)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: model_get_ti_type
      TYPE (model_class), INTENT(in)   :: this

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%te_type)

         CASE (model_ti_type)
            model_get_ti_type = 'pp_ti'

         CASE DEFAULT
            model_get_ti_type = 'none'

      END SELECT

      CALL profiler_set_stop_time('model_get_ti_type', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the soft x-ray emissivity type as a string.
!>
!>  @param[in] this   A @ref model_class instance.
!>  @param[in] index  Index of the soft x-ray emissivity model.
!>  @returns A string description of the soft x-ray emissivity type.
!-------------------------------------------------------------------------------
      FUNCTION model_get_sxrem_type(this, index)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: model_get_sxrem_type
      TYPE (model_class), INTENT(in)   :: this
      INTEGER, INTENT(in)              :: index

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%sxrem_type(index))

         CASE (model_sxrem_type)
            model_get_sxrem_type = 'pp_sxrem'

         CASE (model_sxrem_te_ne_type)
            model_get_sxrem_type = 'pp_sxrem_te_ne'

         CASE DEFAULT
            model_get_sxrem_type = 'none'

      END SELECT

      CALL profiler_set_stop_time('model_get_sxrem_type', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the scale factor for a signal.
!>
!>  Gets the model scale factor. The scaling defaults to 1.
!>
!>  @param[in] this  A @ref model_class instance.
!>  @param[in] index Index of the scale factor.
!>  @returns Scale factor value.
!-------------------------------------------------------------------------------
      FUNCTION model_get_signal_factor(this, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                   :: model_get_signal_factor
      TYPE (model_class), INTENT(in) :: this
      INTEGER, INTENT(in)            :: index

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (index .lt. 1) THEN
         model_get_signal_factor = 1.0
      ELSE
         model_get_signal_factor = this%signal_factor(index)
      END IF

      CALL profiler_set_stop_time('model_get_signal_factor', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the offset factor for a signal.
!>
!>  Gets the model offset factor. The offset defaults to 0.
!>
!>  @param[in] this  A @ref model_class instance.
!>  @param[in] index Index of the scale factor.
!>  @returns Scale factor value.
!-------------------------------------------------------------------------------
      FUNCTION model_get_signal_offset(this, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                   :: model_get_signal_offset
      TYPE (model_class), INTENT(in) :: this
      INTEGER, INTENT(in)            :: index

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (index .lt. 1) THEN
         model_get_signal_offset = 0.0
      ELSE
         model_get_signal_offset = this%signal_offset(index)
      END IF

      CALL profiler_set_stop_time('model_get_signal_offset', start_time)

      END FUNCTION

!*******************************************************************************
!  QUERY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Determines if a parameter id is a reconstruction parameter.
!>
!>  Determines if a parameter id is a reconstruction parameter. If the parameter
!>  is not a model parameter, it is deferred to the @ref equilibrium.
!>
!>  @param[in] this A @ref model_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns True if the id is a reconstruction parameter and false if not.
!-------------------------------------------------------------------------------
      FUNCTION model_is_recon_param(this, id)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL                        :: model_is_recon_param
      TYPE (model_class), INTENT(in) :: this
      INTEGER, INTENT(in)            :: id

!  local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (id)

         CASE (model_ne_unit_id, model_ne_min_id, model_te_min_id,             &
     &         model_ti_min_id, model_sxrem_min_id,                            &
     &         model_pressure_fraction_id, model_coosig_wgts_id,               &
     &         model_signal_factor_id, model_signal_offset_id)
            model_is_recon_param = .true.

         CASE DEFAULT
            model_is_recon_param =                                             &
     &         this%equilibrium%is_recon_param(id)

      END SELECT

      CALL profiler_set_stop_time('model_is_recon_param', start_time)

      END FUNCTION

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Reset the internal state of the model.
!>
!>  Clears out the model state flags then resets the equilibrium. This resets
!>  the profile grid if they were changed.
!>
!>  @param[inout] this A @ref equilibrium_class instance.
!-------------------------------------------------------------------------------
       SUBROUTINE model_reset_state(this)

       IMPLICIT NONE

!  Declare Arguments
       TYPE (model_class), INTENT(inout) :: this

!  local variables
       REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL this%equilibrium%reset_state()
      CALL model_set_grid_profiles(this)
      this%state_flags = model_state_all_off

      CALL profiler_set_stop_time('model_reset_state', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Save the internal state of the model.
!>
!>  Clears out the model state flags then resets the equilibrium.
!>
!>  @param[inout] this A @ref equilibrium_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE model_save_state(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (model_class), INTENT(inout) :: this

!  local variables
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL this%equilibrium%save_state()

      CALL profiler_set_stop_time('model_save_state', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Solves the model.
!>
!>  This solves the equilibirum model and computes modeled values. The modeled
!>  values computed are the grided model values.
!>
!>  @param[inout] this     A @ref model_class instance.
!>  @param[inout] num_iter Counter to track the number of iterations.
!>  @param[in]    iou      Input/output unit of the file to write logs to.
!>  @param[in]    eq_comm  MPI communicator pool for the equilibrium.
!>  @param[in]    param_name Name of the peturbed parameter.
!>  @returns True if the convergece was sucessful and false otherwise.
!-------------------------------------------------------------------------------
      FUNCTION model_converge(this, num_iter, iou, eq_comm, param_name)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL                           :: model_converge
      TYPE (model_class), INTENT(inout) :: this
      INTEGER, INTENT(inout)            :: num_iter
      INTEGER, INTENT(in)               :: iou
      INTEGER, INTENT(in)               :: eq_comm
      CHARACTER (len=*), INTENT(in)     :: param_name

!  local variables
      INTEGER                           :: init_num_iter
      REAL (rprec)                      :: r
      INTEGER                           :: error
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (this%equilibrium%force_solve) THEN
         this%state_flags = model_state_all_on
      END IF

      init_num_iter = num_iter

      IF (BTEST(this%state_flags, model_state_vmec_flag) .or.                  &
     &    BTEST(this%state_flags, model_state_siesta_flag)) THEN
#if defined (MPI_OPT)
         CALL MPI_BCAST(mpi_equilibrium_task, 1, MPI_INTEGER, 0,               &
     &                  eq_comm, error)
         CALL MPI_BCAST(this%state_flags, 1, MPI_INTEGER, 0, eq_comm,          &
     &                  error)
#endif

         model_converge = this%equilibrium%converge(num_iter, iou,             &
     &                                              eq_comm,                   &
     &                                              this%state_flags)
      ELSE
         model_converge = .true.
      END IF

!  If this is the first time running make sure all signals are computed.
      IF (init_num_iter .eq. 1) THEN
         this%state_flags = model_state_all_on
      END IF

      IF (model_converge) THEN

         WRITE (*,1000) model_converge, num_iter,                              &
     &                  num_iter - init_num_iter, TRIM(param_name)
         WRITE (iou,1000) model_converge, num_iter,                            &
     &                    num_iter - init_num_iter, TRIM(param_name)

         CALL model_set_grid_profiles(this)

      END IF

      CALL profiler_set_stop_time('model_converge', start_time)

1000  FORMAT('Model converged ',l,2(2x,i7),' ',a)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Write out the model to an output file.
!>
!>  Write out information about the model to an output file. This also then
!>  calls the @ref equilibrium_write subroutine to write out the equilibrium
!>  information as well.
!>
!>  @param[in] this A @ref model_class instance.
!>  @param[in] iou  Input/output unit of the output file.
!-------------------------------------------------------------------------------
      SUBROUTINE model_write(this, iou)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (model_class), INTENT(in) :: this
      INTEGER, INTENT(in)            :: iou

!  local variables
      INTEGER                        :: i
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (iou,*)
      WRITE (iou,*) ' *** Model Parameters'
      WRITE (iou,*) 'model_ne_type is ',                                       &
     &              TRIM(model_get_ne_type(this))

      DO i = 1, SIZE(this%sxrem_type)
         WRITE (iou,*) 'model_sxrem_type is ',                                 &
     &                 TRIM(model_get_sxrem_type(this, i))
      END DO

      WRITE (iou,*) 'model_te_type is ',                                       &
     &              TRIM(model_get_te_type(this))
      WRITE (iou,*) 'model_ti_type is ',                                       &
     &              TRIM(model_get_ti_type(this))
      WRITE (iou, 1000) 'ne_pp_unit is ', this%ne_unit
      WRITE (iou, 1000) 'ne_min is ', this%ne_min
      WRITE (iou, 1000) 'te_min is ', this%te_min
      WRITE (iou, 1000) 'ti_min is ', this%ti_min
      WRITE (iou, 1000) 'e_pressure_fraction is ',                             &
     &                  this%pressure_fraction

      IF (ASSOCIATED(this%coosig_wgts)) THEN
         DO i = 1, SIZE(this%coosig_wgts)
! Only write coosig_wgts if they are non-zero.
! @todo need better check
            IF (.NOT.(this%coosig_wgts(i) .EQ. 0.0)) THEN
               WRITE (iou,1001) i,this%coosig_wgts(i)
            END IF
         END DO
      END IF

      WRITE (iou, 1000) 'signal_factors are ',                                 &
     &                  this%signal_factor

1000  FORMAT(1x,a,2x,es12.5)
1001  FORMAT(' coosig_wgt(',i4,') is ',es12.5)

      CALL this%equilibrium%write(iou)

      CALL profiler_set_stop_time('model_write', start_time)

      END SUBROUTINE

!*******************************************************************************
!  NETCDF SUBROUTINES
!*******************************************************************************
!>  @page result_file_model Model Result File
!>
!>  @tableofcontents
!>  @section result_file_model_intro_sec Introduction
!>  This page documents the contents of a result NetCDF file contributed by the
!>  model. The remaining parts of the result file are documented in
!>  the @ref result_file_main page.
!>
!>  @section result_file_model_dim_sec Dimensions
!>  @header{Dimension, Description, Code Reference}
!>  @begin_table
!>     @item{model_grid_size, Size of the radial profile grids,          equilibrium::equilibrium_get_grid_size}
!>     @item{model_num_sxrem, Number of soft x-ray emissivity profiles., v3fit_input::v3fit_max_sxrem_profiles}
!>     @item{model_num_coosig_w, Number of combination signal weights.,  model::model_class::coosig_wgts}
!>  @end_table
!>
!>  @section result_file_model_var_sec Variables
!>  @header{Variable(Dimensions), Description, Code Reference}
!>  @begin_table
!>      @item{model_ne_type (string_len),                      Discription of the electron density model.,      model::model_class::ne_type}
!>      @item{model_te_type (string_len),                      Discription of the electron temperature model.,  model::model_class::te_type}
!>      @item{model_ti_type (string_len),                      Discription of the ion temperature model.,       model::model_class::ti_type}
!>      @item{model_sxrem_type (string_len\, model_num_sxrem), Discription of the soft x-ray emissivity model., model::model_class::sxrem_type}
!>      @item{ne_unit (maxnsteps),                             Electron density unit scaling,                   model::model_class::ne_unit}
!>      @item{ne_min (maxnsteps),                              Minimum electron density,                        model::model_class::ne_min}
!>      @item{te_min (maxnsteps),                              Minimum electron temperature,                    model::model_class::te_min}
!>      @item{ti_min (maxnsteps),                              Minimum ion temperature,                         model::model_class::ti_min}
!>      @item{pressure_fraction (maxnsteps),                   Fractional Pressure,                             model::model_class::pressure_fraction}
!>      @item{coosig_wgts (num_wgts\, maxnsteps),              Combination Signal Weights,                      model::model_class::coosig_wgts}
!>  @end_table
!>  @table_section{result_file_model_grid_sec, Model Profiles}
!>      @item{ne_grid (model_grid_size\, maxnsteps),                      Radial profile of the electron density,      model::model_class::ne_grid}
!>      @item{te_grid (model_grid_size\, maxnsteps),                      Radial profile of the electron temperature,  model::model_class::te_grid}
!>      @item{ti_grid (model_grid_size\, maxnsteps),                      Radial profile of the ion temperature,       model::model_class::ti_grid}
!>      @item{sxrem_grid (model_grid_size\, model_num_sxrem\, maxnsteps), Radial profile of the soft x-ray emissivity, model::model_class::sxrem_grid}
!>  @end_table
!-------------------------------------------------------------------------------
!>  @brief Define NetCDF variables for the result file
!>
!>  Defines dimensions and variables for the model contribution of the result
!>  file. Multi dimensional arrays need to be transposed so arrays appear in the
!>  correct order in non fortran languages.
!>
!>  @param[in] this             A @ref model_class instance.
!>  @param[in] result_ncid      NetCDF file id of the result file.
!>  @param[in] maxnsetps_dim_id NetCDF dimension id of the number of steps
!>                              dimension.
!-------------------------------------------------------------------------------
      SUBROUTINE model_def_result(this, result_ncid, maxnsteps_dim_id,         &
     &                            string_len_dim_id)
      USE ezcdf

      IMPLICIT NONE

!  Declare Arguments
      TYPE (model_class), INTENT(in) :: this
      INTEGER, INTENT(in)            :: result_ncid
      INTEGER, INTENT(in)            :: maxnsteps_dim_id
      INTEGER, INTENT(in)            :: string_len_dim_id

!  local variables
      INTEGER                        :: status
      INTEGER                        :: model_grid_size_dim_id
      INTEGER                        :: model_num_sxrem_dim_id
      INTEGER                        :: model_num_coosig_w_dim_id
      INTEGER                        :: ne_type_var_id
      INTEGER                        :: te_type_var_id
      INTEGER                        :: ti_type_var_id
      INTEGER                        :: sxrem_type_var_id
      INTEGER                        :: ne_unit_var_id
      INTEGER                        :: ne_min_var_id
      INTEGER                        :: te_min_var_id
      INTEGER                        :: ti_min_var_id
      INTEGER                        :: pressure_fraction_var_id
      INTEGER                        :: ne_grid_var_id
      INTEGER                        :: te_grid_var_id
      INTEGER                        :: ti_grid_var_id
      INTEGER                        :: sxrem_grid_var_id
      INTEGER                        :: coosig_w_var_id
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Define dimensions
      IF (ASSOCIATED(this%ne_grid) .or.                                        &
     &    ASSOCIATED(this%te_grid) .or.                                        &
     &    ASSOCIATED(this%sxrem_grid)) THEN
         status = nf90_def_dim(result_ncid, 'model_grid_size',                 &
     &                         SIZE(this%ne_grid),                             &
     &                         model_grid_size_dim_id)
      ELSE
         status = nf90_def_dim(result_ncid, 'model_grid_size', 1,              &
     &                         model_grid_size_dim_id)
      END IF
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      IF (ASSOCIATED(this%sxrem_type)) THEN
         status = nf90_def_dim(result_ncid, 'model_num_sxrem',                 &
     &                         SIZE(this%sxrem_type, 1),                       &
     &                         model_num_sxrem_dim_id)
      END IF
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      IF (ASSOCIATED(this%coosig_wgts)) THEN
         status = nf90_def_dim(result_ncid, 'model_num_coosig',                &
     &                         SIZE(this%coosig_wgts, 1),                      &
     &                         model_num_coosig_w_dim_id)
      END IF
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

!  Define variables
      status = nf90_def_var(result_ncid, 'model_ne_type', nf90_char,           &
     &                      dimids=(/ string_len_dim_id /),                    &
     &                      varid=ne_type_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_def_var(result_ncid, 'model_te_type', nf90_char,           &
     &                      dimids=(/ string_len_dim_id /),                    &
     &                      varid=te_type_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_def_var(result_ncid, 'model_ti_type', nf90_char,           &
     &                      dimids=(/ string_len_dim_id /),                    &
     &                      varid=ti_type_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      IF (ASSOCIATED(this%sxrem_type)) THEN
         status = nf90_def_var(result_ncid, 'model_sxrem_type',                &
     &                         nf90_char,                                      &
     &                         dimids=(/ string_len_dim_id,                    &
     &                                   model_num_sxrem_dim_id /),            &
     &                         varid=sxrem_type_var_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      status = nf90_def_var(result_ncid, 'ne_unit', nf90_double,               &
     &                      dimids=(/ maxnsteps_dim_id /),                     &
     &                      varid=ne_unit_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_def_var(result_ncid, 'ne_min', nf90_double,                &
     &                      dimids=(/ maxnsteps_dim_id /),                     &
     &                      varid=ne_min_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_def_var(result_ncid, 'te_min', nf90_double,                &
     &                      dimids=(/ maxnsteps_dim_id /),                     &
     &                      varid=te_min_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_def_var(result_ncid, 'ti_min', nf90_double,                &
     &                      dimids=(/ maxnsteps_dim_id /),                     &
     &                      varid=ti_min_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_def_var(result_ncid, 'pressure_fraction',                  &
     &                      nf90_double, dimids=(/ maxnsteps_dim_id /),        &
     &                      varid=pressure_fraction_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      IF (ASSOCIATED(this%ne_grid)) THEN
         status = nf90_def_var(result_ncid, 'ne_grid', nf90_double,            &
     &                         dimids=(/ model_grid_size_dim_id,               &
     &                                   maxnsteps_dim_id /),                  &
     &                         varid=ne_grid_var_id)
      END IF

      IF (ASSOCIATED(this%te_grid)) THEN
         status = nf90_def_var(result_ncid, 'te_grid', nf90_double,            &
     &                         dimids=(/ model_grid_size_dim_id,               &
     &                                   maxnsteps_dim_id /),                  &
     &                         varid=te_grid_var_id)
      END IF

      IF (ASSOCIATED(this%te_grid)) THEN
         status = nf90_def_var(result_ncid, 'ti_grid', nf90_double,            &
     &                         dimids=(/ model_grid_size_dim_id,               &
     &                                   maxnsteps_dim_id /),                  &
     &                         varid=ti_grid_var_id)
      END IF

      IF (ASSOCIATED(this%sxrem_grid)) THEN
         status = nf90_def_var(result_ncid, 'sxrem_grid', nf90_double,         &
     &                         dimids=(/ model_grid_size_dim_id,               &
     &                                   model_num_sxrem_dim_id,               &
     &                                   maxnsteps_dim_id /),                  &
     &                         varid=sxrem_grid_var_id)
      END IF

      IF (ASSOCIATED(this%coosig_wgts)) THEN
         status = nf90_def_var(result_ncid, 'coosig_wgts', nf90_double,        &
     &                         dimids=(/ model_num_coosig_w_dim_id,            &
     &                                   maxnsteps_dim_id /),                  &
     &                         varid=coosig_w_var_id)
      END IF

      CALL this%equilibrium%def_result(result_ncid, maxnsteps_dim_id)

      CALL profiler_set_stop_time('model_def_result', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write inital data to NetCDF result file
!>
!>  This write the data that doesn't change.
!>
!>  @param[in] this        A @ref model_class instance.
!>  @param[in] result_ncid NetCDF file id of the result file.
!-------------------------------------------------------------------------------
      SUBROUTINE model_write_init_data(this, result_ncid)
      USE ezcdf

      IMPLICIT NONE

!  Declare Arguments
      TYPE (model_class), INTENT(in) :: this
      INTEGER, INTENT(in)            :: result_ncid

!  Local variables
      INTEGER                        :: i, status
      INTEGER                        :: ne_type_var_id
      INTEGER                        :: te_type_var_id
      INTEGER                        :: ti_type_var_id
      INTEGER                        :: sxrem_type_var_id
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      status = nf90_inq_varid(result_ncid, 'model_ne_type',                    &
     &                        ne_type_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_put_var(result_ncid, ne_type_var_id,                       &
     &                      model_get_ne_type(this))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_inq_varid(result_ncid, 'model_te_type',                    &
     &                        te_type_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_put_var(result_ncid, te_type_var_id,                       &
     &                      model_get_te_type(this))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_inq_varid(result_ncid, 'model_ti_type',                    &
     &                        ti_type_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_put_var(result_ncid, ti_type_var_id,                       &
     &                      model_get_ti_type(this))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      IF (ASSOCIATED(this%sxrem_type)) THEN
         status = nf90_inq_varid(result_ncid, 'model_sxrem_type',              &
     &                           sxrem_type_var_id)
         DO i = 1, SIZE(this%sxrem_type)
            status = nf90_put_var(result_ncid, sxrem_type_var_id,              &
     &                            model_get_sxrem_type(this, i),               &
     &                            start=(/ 1, i /),                            &
     &                            count=(/ data_name_length, 1 /))
            CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         END DO
      END IF

      CALL this%equilibrium%write_init_data(result_ncid)

      CALL profiler_set_stop_time('model_write_init_data', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write step data to NetCDF result file
!>
!>  This write the data that can change each reconstruction step.
!>
!>  @param[in] this         A @ref model_class instance.
!>  @param[in] result_ncid  NetCDF file id of the result file.
!>  @param[in] current_step Step index to write variables to.
!-------------------------------------------------------------------------------
      SUBROUTINE model_write_step_data(this, result_ncid, current_step)
      USE ezcdf

!  Declare Arguments
      TYPE (model_class), INTENT(in) :: this
      INTEGER, INTENT(in)            :: result_ncid
      INTEGER, INTENT(in)            :: current_step

!  Local variables
      INTEGER                        :: i, status
      INTEGER                        :: ne_unit_var_id
      INTEGER                        :: ne_min_var_id
      INTEGER                        :: te_min_var_id
      INTEGER                        :: ti_min_var_id
      INTEGER                        :: pressure_fraction_var_id
      INTEGER                        :: ne_grid_var_id
      INTEGER                        :: te_grid_var_id
      INTEGER                        :: ti_grid_var_id
      INTEGER                        :: sxrem_grid_var_id
      INTEGER                        :: coosig_w_var_id
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      status = nf90_inq_varid(result_ncid, 'ne_unit', ne_unit_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_put_var(result_ncid, ne_unit_var_id, this%ne_unit,         &
     &                      start=(/current_step/))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_inq_varid(result_ncid, 'ne_min', ne_min_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_put_var(result_ncid, ne_min_var_id, this%ne_min,           &
     &                      start=(/current_step/))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_inq_varid(result_ncid, 'te_min', te_min_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_put_var(result_ncid, te_min_var_id, this%te_min,           &
     &                      start=(/current_step/))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_inq_varid(result_ncid, 'ti_min', ti_min_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_put_var(result_ncid, ti_min_var_id, this%ti_min,           &
     &                      start=(/current_step/))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_inq_varid(result_ncid, 'pressure_fraction',                &
     &                        pressure_fraction_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_put_var(result_ncid, pressure_fraction_var_id,             &
     &                      this%pressure_fraction,                            &
     &                      start=(/current_step/))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      IF (ASSOCIATED(this%ne_grid)) THEN
         status = nf90_inq_varid(result_ncid, 'ne_grid', ne_grid_var_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         status = nf90_put_var(result_ncid, ne_grid_var_id,                    &
     &                         this%ne_grid,                                   &
     &                         start=(/ 1, current_step /),                    &
     &                         count=(/ SIZE(this%ne_grid), 1 /))
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      IF (ASSOCIATED(this%te_grid)) THEN
         status = nf90_inq_varid(result_ncid, 'te_grid', te_grid_var_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         status = nf90_put_var(result_ncid, te_grid_var_id,                    &
     &                         this%te_grid,                                   &
     &                         start=(/ 1, current_step /),                    &
     &                         count=(/ SIZE(this%te_grid), 1 /))
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      IF (ASSOCIATED(this%ti_grid)) THEN
         status = nf90_inq_varid(result_ncid, 'ti_grid', ti_grid_var_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         status = nf90_put_var(result_ncid, ti_grid_var_id,                    &
     &                         this%ti_grid,                                   &
     &                         start=(/ 1, current_step /),                    &
     &                         count=(/ SIZE(this%ti_grid), 1 /))
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      IF (ASSOCIATED(this%sxrem_grid)) THEN
         status = nf90_inq_varid(result_ncid, 'sxrem_grid',                    &
     &                           sxrem_grid_var_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         status = nf90_put_var(result_ncid, sxrem_grid_var_id,                 &
     &                         this%sxrem_grid,                                &
     &                         start=(/ 1, 1, current_step /),                 &
     &                         count=(/ SIZE(this%sxrem_grid, 1),              &
     &                                  SIZE(this%sxrem_grid, 2), 1 /))
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      IF (ASSOCIATED(this%coosig_wgts)) THEN
         status = nf90_inq_varid(result_ncid, 'coosig_wgts',                   &
     &                           coosig_w_var_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         status = nf90_put_var(result_ncid, coosig_w_var_id,                   &
     &                         this%coosig_wgts,                               &
     &                         start=(/ 1, current_step /),                    &
     &                         count=(/ SIZE(this%coosig_wgts), 1 /))
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      CALL this%equilibrium%write_step_data(result_ncid, current_step)

      CALL profiler_set_stop_time('model_write_step_data', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Restart the model.
!>
!>  Restarts the model from the result file.
!>
!>  @param[inout] this         A @ref model_class instance.
!>  @param[in]    result_ncid  NetCDF file id of the result file.
!>  @param[in]    current_step Step index to read variables from.
!-------------------------------------------------------------------------------
      SUBROUTINE model_restart(this, result_ncid, current_step)
      USE ezcdf

      IMPLICIT NONE

!  Declare Arguments
      TYPE (model_class), INTENT(inout) :: this
      INTEGER, INTENT(in)               :: result_ncid
      INTEGER, INTENT(in)               :: current_step

!  Local variables
      INTEGER                           :: i, status
      INTEGER                           :: ne_unit_var_id
      INTEGER                           :: ne_min_var_id
      INTEGER                           :: te_min_var_id
      INTEGER                           :: ti_min_var_id
      INTEGER                           :: pressure_fraction_var_id
      INTEGER                           :: ne_grid_var_id
      INTEGER                           :: te_grid_var_id
      INTEGER                           :: ti_grid_var_id
      INTEGER                           :: sxrem_grid_var_id
      INTEGER                           :: coosig_w_var_id
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      status = nf90_inq_varid(result_ncid, 'ne_unit', ne_unit_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_get_var(result_ncid, ne_unit_var_id, this%ne_unit,         &
     &                      start=(/current_step/))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_inq_varid(result_ncid, 'ne_min', ne_min_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_get_var(result_ncid, ne_min_var_id, this%ne_min,           &
     &                      start=(/current_step/))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_inq_varid(result_ncid, 'te_min', te_min_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_get_var(result_ncid, te_min_var_id, this%te_min,           &
     &                      start=(/current_step/))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_inq_varid(result_ncid, 'ti_min', ti_min_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_get_var(result_ncid, ti_min_var_id, this%ti_min,           &
     &                      start=(/current_step/))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_inq_varid(result_ncid, 'pressure_fraction',                &
     &                        pressure_fraction_var_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_get_var(result_ncid, pressure_fraction_var_id,             &
     &                      this%pressure_fraction,                            &
     &                      start=(/current_step/))
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      IF (ASSOCIATED(this%ne_grid)) THEN
         status = nf90_inq_varid(result_ncid, 'ne_grid', ne_grid_var_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         status = nf90_get_var(result_ncid, ne_grid_var_id,                    &
     &                         this%ne_grid,                                   &
     &                         start=(/ 1, current_step /),                    &
     &                         count=(/ SIZE(this%ne_grid), 1 /))
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      IF (ASSOCIATED(this%te_grid)) THEN
         status = nf90_inq_varid(result_ncid, 'te_grid', te_grid_var_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         status = nf90_get_var(result_ncid, te_grid_var_id,                    &
     &                         this%te_grid,                                   &
     &                         start=(/ 1, current_step /),                    &
     &                         count=(/ SIZE(this%te_grid), 1 /))
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      IF (ASSOCIATED(this%ti_grid)) THEN
         status = nf90_inq_varid(result_ncid, 'ti_grid', ti_grid_var_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         status = nf90_get_var(result_ncid, ti_grid_var_id,                    &
     &                         this%ti_grid,                                   &
     &                         start=(/ 1, current_step /),                    &
     &                         count=(/ SIZE(this%ti_grid), 1 /))
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      IF (ASSOCIATED(this%sxrem_grid)) THEN
         status = nf90_inq_varid(result_ncid, 'sxrem_grid',                    &
     &                           sxrem_grid_var_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         status = nf90_get_var(result_ncid, sxrem_grid_var_id,                 &
     &                         this%sxrem_grid,                                &
     &                         start=(/ 1, 1, current_step /),                 &
     &                         count=(/ SIZE(this%sxrem_grid, 1),              &
     &                                  SIZE(this%sxrem_grid, 2), 1 /))
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      IF (ASSOCIATED(this%coosig_wgts)) THEN
         status = nf90_inq_varid(result_ncid, 'coosig_wgts',                   &
     &                           coosig_w_var_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         status = nf90_get_var(result_ncid, coosig_w_var_id,                   &
     &                         this%coosig_wgts,                               &
     &                         start=(/ 1, current_step /),                    &
     &                         count=(/ SIZE(this%coosig_wgts), 1 /))
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      CALL this%equilibrium%restart(result_ncid, current_step)

      this%state_flags = model_state_all_off

      CALL profiler_set_stop_time('model_restart', start_time)

      END SUBROUTINE

!*******************************************************************************
!  MPI SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Syncronize the model state to children.
!>
!>  Syncs data between the parent and child processes. If MPI support is not
!>  compiled in this subroutine reduces to a no op.
!>
!>  @param[inout] this       A @ref model_class instance.
!>  @param[in]    recon_comm MPI communicator for the reconstruction processes.
!-------------------------------------------------------------------------------
      SUBROUTINE model_sync_state(this, recon_comm)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (model_class), INTENT(inout) :: this
      INTEGER, INTENT(in)               :: recon_comm

#if defined(MPI_OPT)
!  local variables
      INTEGER                           :: error
      INTEGER                           :: grid_size
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL this%equilibrium%sync_state(recon_comm)

      CALL MPI_BCAST(this%state_flags, 1, MPI_INTEGER, 0, recon_comm,          &
     &               error)

      grid_size = this%equilibrium%get_grid_size()
      IF (grid_size .gt. 0) THEN
         CALL MPI_BCAST(this%ne_grid, grid_size, MPI_REAL8, 0,                 &
     &                  recon_comm, error)
         CALL MPI_BCAST(this%sxrem_grid,                                       &
     &                  grid_size*SIZE(this%sxrem_type), MPI_REAL8, 0,         &
     &                  recon_comm, error)
         CALL MPI_BCAST(this%te_grid, grid_size, MPI_REAL8, 0,                 &
     &                  recon_comm, error)
         CALL MPI_BCAST(this%ti_grid, grid_size, MPI_REAL8, 0,                 &
     &                  recon_comm, error)
      END IF

      CALL profiler_set_stop_time('model_sync_state', start_time)
#endif

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Syncronize a child model state to the parent.
!>
!>  Syncs data between a child and the parent process. If MPI support is not
!>  compiled in this subroutine reduces to a no op.
!>
!>  @param[inout] this       A @ref model_class instance.
!>  @param[in]    index      Reconstruction rank to sync.
!>  @param[in]    recon_comm MPI communicator for the reconstruction processes.
!-------------------------------------------------------------------------------
      SUBROUTINE model_sync_child(this, index, recon_comm)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (model_class), INTENT(inout) :: this
      INTEGER, INTENT(in)               :: index
      INTEGER, INTENT(in)               :: recon_comm

#if defined(MPI_OPT)
!  local variables
      INTEGER                           :: error
      INTEGER                           :: grid_size
      INTEGER                           :: mpi_rank
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL this%equilibrium%sync_child(index, recon_comm)

      grid_size = this%equilibrium%get_grid_size()
      IF (grid_size .gt. 0) THEN
         CALL MPI_COMM_RANK(recon_comm, mpi_rank, error)

         IF (mpi_rank .eq. index) THEN

            CALL MPI_SSEND(this%ne_grid, grid_size, MPI_REAL8, 0,              &
     &                     mpi_rank, recon_comm, error)
            CALL MPI_SSEND(this%sxrem_grid,                                    &
     &                     grid_size*SIZE(this%sxrem_type), MPI_REAL8,         &
     &                     0, mpi_rank, recon_comm, error)
            CALL MPI_SSEND(this%te_grid, grid_size, MPI_REAL8, 0,              &
     &                     mpi_rank, recon_comm, error)
            CALL MPI_SSEND(this%ti_grid, grid_size, MPI_REAL8, 0,              &
     &                     mpi_rank, recon_comm, error)

         ELSE IF (mpi_rank .eq. 0) THEN

            CALL MPI_RECV(this%ne_grid, grid_size, MPI_REAL8, index,           &
     &                    index, recon_comm, MPI_STATUS_IGNORE, error)
            CALL MPI_RECV(this%sxrem_grid,                                     &
     &                    grid_size*SIZE(this%sxrem_type), MPI_REAL8,          &
     &                    index, index, recon_comm, MPI_STATUS_IGNORE,         &
     &                    error)
            CALL MPI_RECV(this%te_grid, grid_size, MPI_REAL8, index,           &
     &                    index, recon_comm, MPI_STATUS_IGNORE, error)
            CALL MPI_RECV(this%ti_grid, grid_size, MPI_REAL8, index,           &
     &                    index, recon_comm, MPI_STATUS_IGNORE, error)

         END IF
      END IF

      CALL profiler_set_stop_time('model_sync_child', start_time)
#endif

      END SUBROUTINE

      END MODULE

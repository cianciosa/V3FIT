!-------------------------------------------------------------------------------
!  The @header, @table_section, @table_subsection, @item and @end_table commands
!  are custom defined commands in Doxygen.in. They are defined under ALIASES.
!  For the page created here, the 80 column limit is exceeded. Arguments of
!  aliases are separated by ','. If you intended ',' to be a string you must use
!  an escaped comma '\,'.
!
!>  @page siesta_equilibrium_sec SIESTA Equilibrium Manual
!>
!>  @tableofcontents
!>  @section siesta_equilibrium_intro_sec Introduction
!>  This page documents the V3FIT interface to the SIESTA equilibrium solver.
!>  This lists all the parameters associated with a SIESTA equilibrium. These
!>  parameters are divided input two types, reconstruction parameters and
!>  derived parameters. All parameters are documented in a table of the
!>  following form.
!>  @header{Input variable, Description, Code Reference}
!>
!>  @section siesta_equilibrium_recon_param_sec SIESTA Reconstruction Parameters
!>  Reconstruction parameters are divided into two types, SIESTA reconstruction
!>  parameters and auxillary parameters.
!>  @subsection siesta_equilibrium_vmec_recon_param_sec SIESTA Equilibrium Reconstruction Parameters
!>  SIESTA Equilibrium reconstruction parameters are parameters that maybe
!>  reconstructed that change the equilibirum. These parameters require
!>  the equilibrium to reconverge.
!>  @begin_table
!>     @item{helpert,       1D Array of helical perturbation amplitudes.,         @ref siesta_context::helpert}
!>  @end_table
!>  @subsection siesta_equilibrium_aux_recon_param_sec SIESTA Auxiliary Reconstruction Parameters
!>  SIESTA Auxiliary reconstruction parameters are parameters that maybe
!>  reconstructed that do not change the equilibirum. These parameters do not
!>  require the equilibrium to reconverge.
!>  @begin_table
!>     @item{pp_ne_b,       1D Array of function density profile parameters.,              @ref pprofile_T::pprofile_class::b}
!>     @item{pp_ne_as,      1D Array of segment density profile s poitions.,               @ref pprofile_T::pprofile_class::as}
!>     @item{pp_ne_af,      1D Array of segment density profile f values.,                 @ref pprofile_T::pprofile_class::af}
!>     @item{pp_sxrem_b,    1D Array of function sxrem profile parameters.
!>                          DEPREICATED only use id when using the old profile
!>                          specification.,                                                @ref pprofile_T::pprofile_class::b}
!>     @item{pp_sxrem_as,   1D Array of segment sxrem profile s poitions.
!>                          DEPREICATED only use id when using the old profile
!>                          specification.,                                                @ref pprofile_T::pprofile_class::as}
!>     @item{pp_sxrem_af,   1D Array of segment sxrem profile f values.
!>                          DEPREICATED only use id when using the old profile
!>                          specification.,                                                @ref pprofile_T::pprofile_class::af}
!>     @item{pp_sxrem_b_a,  2D Array of function sxrem profile parameters.
!>                          First index is the profile number.,                            @ref siesta_equilibrium::siesta_class::sxrem}
!>     @item{pp_sxrem_as_a, 2D Array of segment sxrem profile s poitions.
!>                          First index is the profile number.,                            @ref siesta_equilibrium::siesta_class::sxrem}
!>     @item{pp_sxrem_af_a, 2D Array of segment sxrem profile f values.
!>                          First index is the profile number.,                            @ref siesta_equilibrium::siesta_class::sxrem}
!>     @item{pp_te_b,       1D Array of function electron temperature profile parameters., @ref pprofile_T::pprofile_class::b}
!>     @item{pp_te_as,      1D Array of segment electron temperature profile s poitions.,  @ref pprofile_T::pprofile_class::as}
!>     @item{pp_te_af,      1D Array of segment electron temperature profile f values.,    @ref pprofile_T::pprofile_class::af}
!>     @item{pp_ti_b,       1D Array of function ion temperature profile parameters.,      @ref pprofile_T::pprofile_class::b}
!>     @item{pp_ti_as,      1D Array of segment ion temperature profile s poitions.,       @ref pprofile_T::pprofile_class::as}
!>     @item{pp_ti_af,      1D Array of segment ion temperature profile f values.,         @ref pprofile_T::pprofile_class::af}
!>     @item{phi_offset,    Phi angle offset in radians,                                   @ref siesta_equilibrium::siesta_class::phi_offset}
!>     @item{z_offset,      Z offset in meters,                                            @ref siesta_equilibrium::siesta_class::z_offset}
!>  @end_table
!*******************************************************************************
!>  @file siesta_equilibrium.f
!>  @brief Contains module @ref siesta_equilibrium.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref siesta_class. This module contains
!>  all the code necessary to interface V3FIT with SIESTA.
!>  @par Super Class:
!>  @ref equilibrium
!*******************************************************************************

      MODULE siesta_equilibrium
      USE siesta_context
      USE vmec_equilibrium
      USE siesta_run

      IMPLICIT NONE

!*******************************************************************************
!  siesta equilibrium module parameters
!*******************************************************************************
!  Define id's only for values that can change. All others shouldn't be needed
!  outside of the vmec interface. Parameter id's start at 14 since 0-13 are
!  reserved for non equilibrium model parameters. These numbers will need to be
!  updated if any new model parameters are added. Siesta uses the VMEC model
!  parameters but overwrites the auxilary parameters.

!  Auxiliary Model Parameters
!>  1D Array of function density profile parameters. Defined in @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_ne_b_id     = 61
!>  1D Array of segment density profile s poitions. Defined in @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_ne_as_id    = 62
!>  1D Array of segment density profile f values. Defined in @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_ne_af_id    = 63
!>  1D Array of function sxrem profile parameters. Defined in @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_sxrem_b_id  = 64
!>  1D Array of segment sxrem profile s poitions. Defined in @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_sxrem_as_id = 65
!>  1D Array of segment sxrem profile f values. Defined in @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_sxrem_af_id = 66
!>  1D Array of function electron temperature profile parameters. Defined in
!>  @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_te_b_id     = 67
!>  1D Array of segment electron temperature profile s poitions. Defined in
!>  @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_te_as_id    = 68
!>  1D Array of segment electron temperature profile f values. Defined in
!>  @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_te_af_id    = 69
!>  1D Array of function ion temperature profile parameters. Defined in
!>  @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_ti_b_id     = 70
!>  1D Array of segment ion temperature profile s poitions. Defined in
!>  @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_ti_as_id    = 71
!>  1D Array of segment ion temperature profile f values. Defined in
!>  @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_ti_af_id    = 72
!>  1D Array of function zeff profile parameters. Defined in
!>  @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_ze_b_id     = 73
!>  1D Array of segment zeff profile s poitions. Defined in
!>  @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_ze_as_id    = 74
!>  1D Array of segment zeff profile f values. Defined in
!>  @ref pprofile_T.
      INTEGER, PARAMETER :: siesta_pp_ze_af_id    = 75

!  Skip 74-82 since these parameters have a VMEC value but not a SIESTA analog.
!>  Plasma Phi offset. This is a parameter to allow changing the phi angle of a
!>  quasi helical state in an RFP. Defined in @ref vmec_equilibrium.
      INTEGER, PARAMETER :: siesta_phi_offset_id  = 85
!>  Plasma z offset. This is a parameter to allow changing the vertical shift of
!>  the plasma
      INTEGER, PARAMETER :: siesta_z_offset_id    = 86

!>  Helical perturbation parameter.
      INTEGER, PARAMETER :: siesta_helpert_id     = 87

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) siesta magnetic cache
!  2) siesta base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Object to cache data needed to compute magnetic signals.
!-------------------------------------------------------------------------------
      TYPE siesta_magnetic_cache
!>  Volume differential element for the s direction.
         REAL (rprec)                            :: ds
!>  Volume differential element for the u direction.
         REAL (rprec)                            :: du
!>  Volume differential element for the v direction.
         REAL (rprec)                            :: dv

!>  Area differential elements for the u direction.
         REAL (rprec)                            :: du_a
!>  Area differential elements for the v direction.
         REAL (rprec)                            :: dv_a

!>  Full surface area differential elements for the u direction.
         REAL (rprec)                            :: du_full
!>  Full surface area differential elements for the v direction.
         REAL (rprec)                            :: dv_full

!>  R positions on the suv gird points.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: rsuv => null()
!>  Z positions on the suv gird points.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: zsuv => null()

!>  Current density in the R direction. Has a 1/Sqrt(g) factor embeded.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jrsuv => null()
!>  Current density in the Phi direction. Has a 1/Sqrt(g) factor embeded.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jphisuv => null()
!>  Current density in the Z direction. Has a 1/Sqrt(g) factor embeded.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jzsuv => null()

!>  Surface current density in the R direction. Has all Jacobian and
!>  normalization factors.
         REAL (rprec), DIMENSION(:,:), POINTER :: kruv => null()
!>  Surface current density in the Phi direction. Has all Jacobian and
!>  normalization factors.
         REAL (rprec), DIMENSION(:,:), POINTER :: kphiuv => null()
!>  Surface current density in the Z direction. Has all Jacobian and
!>  normalization factors.
         REAL (rprec), DIMENSION(:,:), POINTER :: kzuv => null()

!>  X' vector on the full uv grid.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: x_prime => null()
!>  Full surface current density in the R direction. Has all Jacobian and
!>  normalization factors.
         REAL (rprec), DIMENSION(:,:), POINTER :: kxuv_full => null()
!>  Full surface current density in the Phi direction. Has all Jacobian and
!>  normalization factors.
         REAL (rprec), DIMENSION(:,:), POINTER :: kyuv_full => null()
!>  Full surface current density in the Z direction. Has all Jacobian and
!>  normalization factors.
         REAL (rprec), DIMENSION(:,:), POINTER :: kzuv_full => null()

!>  Axisymmtric X' vector on the full uv grid.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: x_axi => null()
!>  Axisymmtric  surface current density in the R direction. Has all Jacobian
!>  and normalization factors.
         REAL (rprec), DIMENSION(:,:), POINTER :: kxuv_axi => null()
!>  Full surface current density in the Phi direction. Has all Jacobian and
!>  normalization factors.
         REAL (rprec), DIMENSION(:,:), POINTER :: kyuv_axi => null()
!>  Full surface current density in the Z direction. Has all Jacobian and
!>  normalization factors.
         REAL (rprec), DIMENSION(:,:), POINTER :: kzuv_axi => null()
      END TYPE

!-------------------------------------------------------------------------------
!>  Base class representing a siesta_equilibrium.
!>  @par Super Class:
!>  @ref equilibrium
!-------------------------------------------------------------------------------
      TYPE siesta_class
!>  File name of the output of siesta.
         CHARACTER (len=path_length)            :: restart_file_name
!>  File name of the siesta namelist inout file.
         CHARACTER (len=path_length)            :: siesta_file_name

!>  @ref pprofile_T describing a flux surface constant electron denisty profile.
         TYPE (pprofile_class), POINTER         :: ne => null()
!>  @ref pprofile_T describing a flux surface constant electron temperature
!>  profile.
         TYPE (pprofile_class), POINTER         :: te => null()
!>  @ref pprofile_T describing a flux surface conatsnt soft x-ray emissivity
!>  profile.
         TYPE (pprofile_class), POINTER         :: ti => null()
!>  @ref pprofile_T describing a flux surface conatsnt soft x-ray emissivity
!>  profile.
         TYPE (pprofile_pointer), DIMENSION(:), POINTER ::                     &
     &      sxrem => null()

!>  Ratio of the poloidal grid points to the poloidal grid points.
         REAL (rprec)                           :: pol_rad_ratio
!>  Instance of a @ref siesta_magnetic_cache object.
         TYPE (siesta_magnetic_cache), POINTER  ::                             &
     &      magnetic_cache => null()

!>  siesta context.
         TYPE (siesta_context_class), POINTER   :: context => null()

!  Extra reconstruction parameters
!>  Plasma phi offset.
         REAL (rprec)                           :: phi_offset = 0
!>  Plasma z offset.
         REAL (rprec)                           :: z_offset = 0

!>  Base VMEC equilibrium.
         TYPE (vmec_class), POINTER             :: vmec => null()
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the setting the @ref siesta_magnetic_cache types using
!>  @ref siesta_set_magnetic_cache_responce,
!>  @ref siesta_set_magnetic_cache_point or @ref siesta_set_magnetic_cache_calc.
!-------------------------------------------------------------------------------
      INTERFACE siesta_set_magnetic_cache
         MODULE PROCEDURE siesta_set_magnetic_cache_responce,                  &
     &                    siesta_set_magnetic_cache_point,                     &
     &                    siesta_set_magnetic_cache_calc
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the siesta density profile values.
!-------------------------------------------------------------------------------
      INTERFACE siesta_get_ne
         MODULE PROCEDURE siesta_get_ne_cart,                                  &
     &                    siesta_get_ne_radial
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the siesta guassian process density profile values.
!-------------------------------------------------------------------------------
      INTERFACE siesta_get_gp_ne
         MODULE PROCEDURE siesta_get_gp_ne_ij,                                 &
     &                    siesta_get_gp_ne_pi,                                 &
     &                    siesta_get_gp_ne_pp
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the siesta electron temperature profile values.
!-------------------------------------------------------------------------------
      INTERFACE siesta_get_te
         MODULE PROCEDURE siesta_get_te_cart,                                  &
     &                    siesta_get_te_radial
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the siesta guassian process electron temperature profile
!>  values.
!-------------------------------------------------------------------------------
      INTERFACE siesta_get_gp_te
         MODULE PROCEDURE siesta_get_gp_te_ij,                                 &
     &                    siesta_get_gp_te_pi,                                 &
     &                    siesta_get_gp_te_pp
      END INTERFACE


!-------------------------------------------------------------------------------
!>  Interface for the siesta ion temperature profile values.
!-------------------------------------------------------------------------------
      INTERFACE siesta_get_ti
         MODULE PROCEDURE siesta_get_ti_cart,                                  &
     &                    siesta_get_ti_radial
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the siesta guassian process ion temperature profile values.
!-------------------------------------------------------------------------------
      INTERFACE siesta_get_gp_ti
         MODULE PROCEDURE siesta_get_gp_ti_ij,                                 &
     &                    siesta_get_gp_ti_pi,                                 &
     &                    siesta_get_gp_ti_pp
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the siesta soft x-ray emission profile values.
!-------------------------------------------------------------------------------
      INTERFACE siesta_get_sxrem
         MODULE PROCEDURE siesta_get_sxrem_cart,                               &
     &                    siesta_get_sxrem_radial
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the siesta guassian process soft x-ray emissivity profile
!>  values.
!-------------------------------------------------------------------------------
      INTERFACE siesta_get_gp_sxrem
         MODULE PROCEDURE siesta_get_gp_sxrem_ij,                              &
     &                    siesta_get_gp_sxrem_pi,                              &
     &                    siesta_get_gp_sxrem_pp
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the siesta pressure profile values.
!-------------------------------------------------------------------------------
      INTERFACE siesta_get_p
         MODULE PROCEDURE siesta_get_p_cart,                                   &
     &                    siesta_get_p_radial
      END INTERFACE

      CONTAINS

!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref siesta_class object.
!>
!>  Allocates memory and initializes a @ref siesta_class object. Currently this
!>  is limited to just loading an already created restart file but in the future
!>  it will load initalize and load siesta as well.
!>
!>  @param[in]    file_name         Filename of the siesta namelist input file.
!>  @param[in]    restart_file_name Filename of the siesta restart file.
!>  @param[in]    ne                @ref pprofile_T for the electron density.
!>  @param[in]    te                @ref pprofile_T for the electron
!>                                  temperature.
!>  @param[in]    ti                @ref pprofile_T for the ion temperature.
!>  @param[in]    sxrem             @ref pprofile_T for the soft x-ray
!>                                  emissivity.
!>  @param[in]    phi_offset        Initial phi offset of the plasma relative to
!>                                  the diagnostics in radians.
!>  @param[in]    z_offset          Initial Z offset of the plasma relative to
!>                                  the machine center.
!>  @param[in]    iou               Input/output unit to log messages to.
!>  @param[in]    eq_comm           MPI communicator pool for SIESTA.
!>  @param[in]    recon_comm        MPI communicator pool for reconstruction.
!>  @param[in]    vmec              An instance of a @ref vmec_equilibrium
!>                                  object.
!>  @param[inout] state_flags       Bitwise flags to indicate which parts of the
!>                                  model changed.
!>  @returns A pointer to a constructed @ref siesta_class object.
!-------------------------------------------------------------------------------
      FUNCTION siesta_construct(file_name, restart_file_name,                  &
     &                          ne, te, ti, sxrem, phi_offset, z_offset,       &
     &                          pol_rad_ratio, iou, eq_comm, recon_comm,       &
     &                          vmec, state_flags)
      USE safe_open_mod
      USE model_state
      USE file_opts
      USE siesta_namelist, ONLY: restart_ext, wout_file,                       &
     &                           siesta_namelist_read

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), POINTER         :: siesta_construct
      CHARACTER (len=*), INTENT(in)        :: file_name
      CHARACTER (len=*), INTENT(in)        :: restart_file_name
      TYPE (pprofile_class), POINTER       :: ne
      TYPE (pprofile_class), POINTER       :: te
      TYPE (pprofile_class), POINTER       :: ti
      TYPE (pprofile_pointer), DIMENSION(:), POINTER :: sxrem
      REAL (rprec), INTENT(in)             :: phi_offset
      REAL (rprec), INTENT(in)             :: z_offset
      REAL (rprec), INTENT(in)             :: pol_rad_ratio
      INTEGER, INTENT(in)                  :: iou
      INTEGER, INTENT(in)                  :: eq_comm
      INTEGER, INTENT(in)                  :: recon_comm
      TYPE (vmec_class), POINTER           :: vmec
      INTEGER, INTENT(inout)               :: state_flags

!  local variables
      INTEGER                              :: eq_rank
      INTEGER                              :: recon_rank
      INTEGER                              :: recon_size
      INTEGER                              :: error
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(siesta_construct)

      eq_rank = 0
      recon_rank = 0
      recon_size = 1
#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(eq_comm, eq_rank, error)

      IF (eq_rank .eq. 0) THEN
         CALL MPI_COMM_RANK(recon_comm, recon_rank, error)
         CALL MPI_COMM_SIZE(recon_comm, recon_size, error)
      END IF
      CALL MPI_BCAST(recon_rank, 1, MPI_INTEGER, 0, eq_comm, error)
#endif

      siesta_construct%vmec => vmec

      IF (eq_rank .eq. 0) THEN
!  Save a copy of the jcf orginal jcf file.
         CALL copy_file(TRIM(file_name), TRIM(file_name) // '_save',           &
     &               error)

         WRITE (*,*) ' *** Initializing SIESTA equilibrium from ' //           &
     &               'file ' // TRIM(file_name)
         WRITE (iou,*) ' *** Initializing SIESTA equilibrium from ' //         &
     &                 'file ' // TRIM(file_name)

         siesta_construct%ne => ne
         siesta_construct%te => te
         siesta_construct%ti => ti
         siesta_construct%sxrem => sxrem

         siesta_construct%phi_offset = phi_offset
         siesta_construct%z_offset = z_offset

         siesta_construct%pol_rad_ratio = pol_rad_ratio
         siesta_construct%siesta_file_name = TRIM(file_name)

         CALL siesta_namelist_read(TRIM(file_name))
         wout_file = vmec%wout_file_name
         WRITE (siesta_construct%restart_file_name,1000)                       &
     &      TRIM(restart_ext)

         state_flags = IBSET(state_flags, model_state_siesta_flag)

         IF (restart_file_name .ne. '') THEN
            state_flags = IBCLR(state_flags, model_state_siesta_flag)
            siesta_construct%context =>                                        &
     &         siesta_context_construct(restart_file_name)
         END IF
      END IF

      CALL profiler_set_stop_time('siesta_construct', start_time)

1000  FORMAT('siesta_',a,'.nc')

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref siesta_class object.
!>
!>  Deallocates memory and uninitializes a @ref siesta_class object.
!>
!>  @param[inout] this A @ref siesta_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_destruct(this)
      USE read_wout_mod, only: read_wout_deallocate
      USE file_opts

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), POINTER :: this

!  local variables
      INTEGER                      :: i, error

!  Start of executable code
      IF (ASSOCIATED(this%ne)) THEN
         CALL pprofile_destruct(this%ne)
         this%ne => null()
      END IF

      IF (ASSOCIATED(this%te)) THEN
         CALL pprofile_destruct(this%te)
         this%te => null()
      END IF

      IF (ASSOCIATED(this%ti)) THEN
         CALL pprofile_destruct(this%ti)
         this%ti => null()
      END IF

      IF (ASSOCIATED(this%sxrem)) THEN
         DO i = 1, SIZE(this%sxrem)
            IF (ASSOCIATED(this%sxrem(i)%p)) THEN
               CALL pprofile_destruct(this%sxrem(i)%p)
               this%sxrem(i)%p => null()
            END IF
         END DO
         DEALLOCATE(this%sxrem)
         this%sxrem => null()
      END IF

      IF (ASSOCIATED(this%magnetic_cache)) THEN
         IF (ASSOCIATED(this%magnetic_cache%rsuv)) THEN
            DEALLOCATE(this%magnetic_cache%rsuv)
            this%magnetic_cache%rsuv => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%zsuv)) THEN
            DEALLOCATE(this%magnetic_cache%zsuv)
            this%magnetic_cache%zsuv => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%jrsuv)) THEN
            DEALLOCATE(this%magnetic_cache%jrsuv)
            this%magnetic_cache%jrsuv => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%jphisuv)) THEN
            DEALLOCATE(this%magnetic_cache%jphisuv)
            this%magnetic_cache%jphisuv => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%jzsuv)) THEN
            DEALLOCATE(this%magnetic_cache%jzsuv)
            this%magnetic_cache%jzsuv => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%kruv)) THEN
            DEALLOCATE(this%magnetic_cache%kruv)
            this%magnetic_cache%kruv => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%kphiuv)) THEN
            DEALLOCATE(this%magnetic_cache%kphiuv)
            this%magnetic_cache%kphiuv => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%kzuv)) THEN
            DEALLOCATE(this%magnetic_cache%kzuv)
            this%magnetic_cache%kzuv => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%x_prime)) THEN
            DEALLOCATE(this%magnetic_cache%x_prime)
            this%magnetic_cache%x_prime => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%kxuv_full)) THEN
            DEALLOCATE(this%magnetic_cache%kxuv_full)
            this%magnetic_cache%kxuv_full => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%kyuv_full)) THEN
            DEALLOCATE(this%magnetic_cache%kyuv_full)
            this%magnetic_cache%kyuv_full => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%kzuv_full)) THEN
            DEALLOCATE(this%magnetic_cache%kzuv_full)
            this%magnetic_cache%kzuv_full => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%x_axi)) THEN
            DEALLOCATE(this%magnetic_cache%x_axi)
            this%magnetic_cache%x_axi => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%kxuv_axi)) THEN
            DEALLOCATE(this%magnetic_cache%kxuv_axi)
            this%magnetic_cache%kxuv_axi => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%kyuv_axi)) THEN
            DEALLOCATE(this%magnetic_cache%kyuv_axi)
            this%magnetic_cache%kyuv_axi => null()
         END IF

         IF (ASSOCIATED(this%magnetic_cache%kzuv_axi)) THEN
            DEALLOCATE(this%magnetic_cache%kzuv_axi)
            this%magnetic_cache%kzuv_axi => null()
         END IF

         DEALLOCATE(this%magnetic_cache)
         this%magnetic_cache => null()
      END IF

!  Delete the restart file. Errors here can safely be ignored.
      CALL delete_file(TRIM(this%restart_file_name) // '_cache', error)

      IF (ASSOCIATED(this%context)) THEN
         CALL siesta_context_destruct(this%context)
         this%context => null()
      END IF

      IF (ASSOCIATED(this%vmec)) THEN
         CALL vmec_destruct(this%vmec)
         this%vmec => null()
      END IF

      DEALLOCATE(this)

      END SUBROUTINE

!*******************************************************************************
!  SETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Sets the value of a reconstruction equilibrium parameter.
!>
!>  This method overrides @ref equilibrium::equilibrium_set_param. When a SIESTA
!>  parameter is changed, propagate the changes to the SIESTA internal state and
!>  inform the caller that the equilibrium needs reconvergence.
!>
!>  @param[inout] this        A @ref siesta_class instance.
!>  @param[in]    id          ID of the parameter.
!>  @param[in]    i_index     The ith index of the parameter.
!>  @param[in]    j_index     The jth index of the parameter.
!>  @param[in]    value       The value of the parameter.
!>  @param[in]    eq_comm     MPI communicator for the child equilibrium
!>                            processes.
!>  @param[inout] state_flags Bitwise flags to indicate which parts of the model
!>                            changed.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_set_param(this, id, i_index, j_index, value,           &
     &                            eq_comm, state_flags)
      USE model_state
      USE siesta_namelist, ONLY: helpert

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                :: id
      INTEGER, INTENT(in)                :: i_index
      INTEGER, INTENT(in)                :: j_index
      REAL (rprec), INTENT(in)           :: value
      INTEGER, INTENT(in)                :: eq_comm
      INTEGER, INTENT(inout)             :: state_flags

!  local variables
      INTEGER                            :: error
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (id)

         CASE (siesta_pp_ne_b_id)
            state_flags = IBSET(state_flags, model_state_ne_flag)
            this%ne%b(i_index) = value

         CASE (siesta_pp_ne_as_id)
            state_flags = IBSET(state_flags, model_state_ne_flag)
            this%ne%as(i_index) = value

         CASE (siesta_pp_ne_af_id)
            state_flags = IBSET(state_flags, model_state_ne_flag)
            this%ne%af(i_index) = value

         CASE (siesta_pp_te_b_id)
            state_flags = IBSET(state_flags, model_state_te_flag)
            this%te%b(i_index) = value

         CASE (siesta_pp_te_as_id)
            state_flags = IBSET(state_flags, model_state_te_flag)
            this%te%as(i_index) = value

         CASE (siesta_pp_te_af_id)
            state_flags = IBSET(state_flags, model_state_te_flag)
            this%te%af(i_index) = value

         CASE (siesta_pp_ti_b_id)
            state_flags = IBSET(state_flags, model_state_ti_flag)
            this%ti%b(i_index) = value

         CASE (siesta_pp_ti_as_id)
            state_flags = IBSET(state_flags, model_state_ti_flag)
            this%ti%as(i_index) = value

         CASE (siesta_pp_ti_af_id)
            state_flags = IBSET(state_flags, model_state_ti_flag)
            this%ti%af(i_index) = value

!  There are multiple soft x-ray emission profiles. These need to be offset by
!  the array index.
         CASE (siesta_pp_sxrem_b_id)
            state_flags = IBSET(state_flags, model_state_sxrem_flag +          &
     &                                       (i_index - 1))
            this%sxrem(i_index)%p%b(j_index) = value

         CASE (siesta_pp_sxrem_as_id)
            state_flags = IBSET(state_flags, model_state_sxrem_flag +          &
     &                                       (i_index - 1))
            this%sxrem(i_index)%p%as(j_index) = value

         CASE (siesta_pp_sxrem_af_id)
            state_flags = IBSET(state_flags, model_state_sxrem_flag +          &
     &                                       (i_index - 1))
            this%sxrem(i_index)%p%af(j_index) = value

!  For the phi_offset, the magnetic cache needs to be updated. Since the phi
!  offset doesn't alter the equilibrium, the magnetics were not getting updated
!  since the equilibrium was not reconverged. Call vmec_set_magnetic_cache_calc
!  to update the magnetic cache without reconverging VMEC.
         CASE (siesta_phi_offset_id)
            state_flags = IBSET(state_flags, model_state_shift_flag)
            this%phi_offset = value
            IF (ASSOCIATED(this%magnetic_cache)) THEN
               CALL siesta_set_magnetic_cache(this)
            END IF

         CASE (siesta_z_offset_id)
            state_flags = IBSET(state_flags, model_state_shift_flag)
            this%z_offset = value
            IF (ASSOCIATED(this%magnetic_cache)) THEN
               CALL siesta_set_magnetic_cache(this)
            END IF

         CASE (siesta_helpert_id)
            state_flags = IBSET(state_flags, model_state_siesta_flag)
            helpert(i_index) = value

         CASE DEFAULT
            CALL vmec_set_param(this%vmec, id, i_index, j_index, value,        &
     &                          eq_comm, state_flags)

      END SELECT

      CALL profiler_set_stop_time('siesta_set_param', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Set magnetic cache for plasma responce.
!>
!>  This method overrides @ref equilibrium::equilibrium_set_magnetic_cache. This
!>  allocates a @ref siesta_magnetic_cache structure. Point measurements require
!>  no array allocations.
!>
!>  @param[inout] this            A @ref siesta_class instance.
!>  @param[in]    response_object A @ref magnetic_response::magnetic_response_class
!>                                instance.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_set_magnetic_cache_responce(this,                      &
     &                                              response_object)
      USE stel_constants, only: twopi
      USE magnetic_response
      USE siesta_namelist, ONLY: mpolin, nsin, nsin_ext

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(inout)         :: this
      TYPE (magnetic_response_class), INTENT(in) :: response_object

!  local variables
      INTEGER                                    :: numU
      INTEGER                                    :: ns
      INTEGER                                    :: mpol
      REAL (rprec)                               :: start_time

!  Start of executable code.
      start_time = profiler_get_start_time()

      IF (.not.ASSOCIATED(this%magnetic_cache)) THEN
         ALLOCATE(this%magnetic_cache)
      END IF

      IF (magnetic_response_use_plasma(response_object)) THEN
!  In order to sample the fields on a grid, the sampling frequency of the grid
!  must be at least twice the frequency of the highest mode.
         IF (ASSOCIATED(this%context)) THEN
            ns = this%context%ns
            mpol = this%context%mpol
         ELSE
            ns = nsin + nsin_ext
            mpol = mpolin
         END IF
         numU = MAX(INT(this%pol_rad_ratio*ns), 2*mpol)

!  Set the differental volume elements.
         this%magnetic_cache%ds = 1.0/(ns - 1.0)
         this%magnetic_cache%du = twopi/numU
         this%magnetic_cache%dv = twopi/response_object%n_field_periods        &
     &                          / response_object%num_t

!  When stellarator symmetry is used, do not need to store all the toroidal
!  planes in a single field period.
         ALLOCATE(this%magnetic_cache%rsuv(ns,numU,                            &
     &            SIZE(response_object%a_r)))
         ALLOCATE(this%magnetic_cache%zsuv(ns,numU,                            &
     &            SIZE(response_object%a_r)))
         ALLOCATE(this%magnetic_cache%jrsuv(ns,numU,                           &
     &            SIZE(response_object%a_r)))
         ALLOCATE(this%magnetic_cache%jphisuv(ns,numU,                         &
     &            SIZE(response_object%a_r)))
         ALLOCATE(this%magnetic_cache%jzsuv(ns,numU,                           &
     &            SIZE(response_object%a_r)))
      END IF

!  When conducting shell is used allocate the surface vector arrays.
      IF (magnetic_response_use_shell(response_object)) THEN
!  Set the differental area elements.
         this%magnetic_cache%du_a = twopi/compression_get_dimension1(          &
     &                                       response_object%a_s_r)
         this%magnetic_cache%dv_a                                              &
     &      = (twopi/response_object%n_field_periods)                          &
     &      / response_object%num_t_shell

         ALLOCATE(this%magnetic_cache%kruv(                                    &
     &               compression_get_dimension1(response_object%a_s_r),        &
     &               compression_get_dimension2(response_object%a_s_r)))
         ALLOCATE(this%magnetic_cache%kphiuv(                                  &
     &               compression_get_dimension1(response_object%a_s_r),        &
     &               compression_get_dimension2(response_object%a_s_r)))
         ALLOCATE(this%magnetic_cache%kzuv(                                    &
     &               compression_get_dimension1(response_object%a_s_r),        &
     &               compression_get_dimension2(response_object%a_s_r)))
      END IF

      CALL profiler_set_stop_time('siesta_set_magnetic_cache_responce',        &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Set magnetic cache initialy.
!>
!>  This method overrides @ref equilibrium::equilibrium_set_magnetic_cache. This
!>  allocates a @ref sieste_magnetic_cache structure. Point measurements require
!>  no array allocations.
!>
!>  @param[inout] this    A @ref siesta_class instance.
!>  @param[in]    use_axi Magnetics can subtract off axisymmetric components.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_set_magnetic_cache_point(this, use_axi)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(inout) :: this
      LOGICAL, INTENT(in)                :: use_axi

!  local variables
      INTEGER                            :: u_size, v_size
      REAL (rprec)                       :: rbc00, rbc01, zbs01
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (.not.ASSOCIATED(this%magnetic_cache)) THEN
         ALLOCATE(this%magnetic_cache)
      END IF

      rbc00 = vmec_get_param_value(this%vmec, vmec_rbc_id, 0, 0)
      rbc01 = vmec_get_param_value(this%vmec, vmec_rbc_id, 0, 1)
      zbs01 = vmec_get_param_value(this%vmec, vmec_zbs_id, 0, 1)

!  Set the grid size based on the size of the rough size of the plasma.
      v_size = MAX(INT(twopi*rbc00/magnetic_cache_vc_grid_dim),             &
     &             magnetic_cache_vc_min_grid_points)
      IF (MOD(v_size, 2) .eq. 0) THEN
         v_size = v_size + 1
      END IF
      this%magnetic_cache%dv_full = twopi/v_size

      u_size = MAX(INT(twopi*MAX(rbc01, zbs01)/                                &
     &                 magnetic_cache_vc_grid_dim),                            &
     &             magnetic_cache_vc_min_grid_points)
      IF (MOD(u_size, 2) .eq. 0) THEN
         u_size = u_size + 1
      END IF
      this%magnetic_cache%du_full = twopi/u_size

      ALLOCATE(this%magnetic_cache%kxuv_full(u_size,v_size))
      ALLOCATE(this%magnetic_cache%kyuv_full(u_size,v_size))
      ALLOCATE(this%magnetic_cache%kzuv_full(u_size,v_size))

      ALLOCATE(this%magnetic_cache%x_prime(u_size,v_size,3))

      IF (use_axi) THEN
         ALLOCATE(this%magnetic_cache%kxuv_axi(u_size,v_size))
         ALLOCATE(this%magnetic_cache%kyuv_axi(u_size,v_size))
         ALLOCATE(this%magnetic_cache%kzuv_axi(u_size,v_size))

         ALLOCATE(this%magnetic_cache%x_axi(u_size,v_size,3))

      END IF

      CALL profiler_set_stop_time('siesta_set_magnetic_cache_point',           &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Set magnetic cache.
!>
!>  After the equilibrium has been converged calculate the r, z, jr, jphi and
!>  jz on grid of s, u, v points.
!>
!>  @param[inout] this A @ref siesta_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_set_magnetic_cache_calc(this)
      USE stel_constants, only: mu0
      USE read_wout_mod, only: mnmax, xm, xn, rmnc, rmns, zmnc, zmns,          &
     &                         lasym, nsvmec => ns
      USE siesta_namelist, ONLY: mpolin, ntorin, nfpin

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(inout)        :: this

!  local variables
      INTEGER                                   :: i, j, k
      REAL (rprec)                              :: s, u, v
      INTEGER                                   :: numS, numU, numV
      INTEGER                                   :: m, n
      INTEGER                                   :: mpol, ntor
      INTEGER                                   :: nfp
      REAL (rprec)                              :: r, rs, ru, rv
      REAL (rprec)                              :: z, zs, zu, zv
      REAL (rprec)                              :: js, ju, jv
      REAL (rprec)                              :: bs, bu, bv
      REAL (rprec)                              :: kr, kp
      REAL (rprec)                              :: theta, phi
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE :: bsubuc, bsubus
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE :: bsubvc, bsubvs
      REAL (rprec), DIMENSION(:), ALLOCATABLE   :: rmncint, rmnsint
      REAL (rprec), DIMENSION(:), ALLOCATABLE   :: zmncint, zmnsint
      REAL (rprec)                              :: cosphi
      REAL (rprec)                              :: sinphi
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE :: cosmn
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE :: sinmn
      REAL (rprec), DIMENSION(:), ALLOCATABLE   :: cosmn_vmec
      REAL (rprec), DIMENSION(:), ALLOCATABLE   :: sinmn_vmec
      REAL (rprec)                              :: wlow, whigh
      INTEGER                                   :: ilow, ihigh
      REAL (rprec)                              :: start_time

!  Start of executable code.
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%magnetic_cache%rsuv)) THEN
!  Get the size of the SUV grid.
         numS = SIZE(this%magnetic_cache%rsuv, 1)
         numU = SIZE(this%magnetic_cache%rsuv, 2)
         numV = SIZE(this%magnetic_cache%rsuv, 3)

         IF (ASSOCIATED(this%context)) THEN
            mpol = this%context%mpol
            ntor = this%context%ntor
            nfp = this%context%nfp
         ELSE
            mpol = mpolin
            ntor = ntorin
            nfp = nfpin
         END IF

!$OMP PARALLEL
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i,j,k,s,u,v,cosmn,sinmn,cosmn_vmec,sinmn_vmec,m,n,r,z,
!$OMP&         rs,zs,ru,zu,rv,zv,js,ju,jv,rmncint,rmnsint,zmncint,
!$OMP&         zmnsint,ilow,ihigh,wlow,whigh)

         ALLOCATE(cosmn(0:mpol,-ntor:ntor))
         ALLOCATE(sinmn(0:mpol,-ntor:ntor))

         ALLOCATE(cosmn_vmec(mnmax))
         ALLOCATE(sinmn_vmec(mnmax))

         ALLOCATE(rmncint(mnmax))
         ALLOCATE(zmnsint(mnmax))
         IF (lasym) THEN
            ALLOCATE(rmnsint(mnmax))
            ALLOCATE(zmncint(mnmax))
         END IF

!  Set the suv grids
!
!  ru = dR/du
!  rv = dR/dv   Note: dRHat/dv = PhiHat
!
!  zu = dZ/du
!  zu = dZ/dv
!
!  jr = ju*ru + jv*rv
!  jphi = jv*R
!  jz = ju*zu + jv*zv

!$OMP DO
!$OMP& SCHEDULE(STATIC)
         DO k = 1, numV
            v = (k - 1)*this%magnetic_cache%dv + this%phi_offset

            DO j = 1, numU
               u = (j - 1)*this%magnetic_cache%du

               cosmn_vmec = COS(xm*u - xn*v)
               sinmn_vmec = SIN(xm*u - xn*v)

               DO n = -ntor, ntor
                  DO m = 0, mpol
                     cosmn(m,n) = COS(m*u + nfp*n*v)
                     sinmn(m,n) = SIN(m*u + nfp*n*v)
                  END DO
               END DO

               DO i = 1, numS
                  s = (i - 1.0)/(numS - 1.0)
                  ilow = siesta_to_i_vmec_low(s)
                  IF (ilow .eq. nsvmec) THEN
                     ilow = nsvmec - 1
                  END IF
                  ihigh = ilow + 1
                  wlow = siesta_to_w_vmec_low(s, ilow)
                  whigh = 1.0 - wlow

                  rmncint = wlow*rmnc(:,ilow) + whigh*rmnc(:,ihigh)
                  zmnsint = wlow*zmns(:,ilow) + whigh*zmns(:,ihigh)

                  this%magnetic_cache%rsuv(i,j,k) =                            &
     &               SUM(rmncint*cosmn_vmec)
                  this%magnetic_cache%zsuv(i,j,k) =                            &
     &               SUM(zmnsint*sinmn_vmec)

                  ru = -SUM(xm*rmncint*sinmn_vmec)
                  rv =  SUM(xn*rmncint*sinmn_vmec)
                  zu =  SUM(xm*zmnsint*cosmn_vmec)
                  zv = -SUM(xn*zmnsint*cosmn_vmec)

!  dR(s_v)/ds_s = dR(s_s)/ds_v*ds_v/ds_s = 2sdR(s_v)/ds_v
                  rmncint = 2.0*s*(rmnc(:,ihigh) - rmnc(:,ilow))               &
     &                    * (nsvmec - 1.0)
!  dZ(s_v)/ds_s = dZ(s_s)/ds_v*ds_v/ds_s = 2sdZ(s_v)/ds_v
                  zmnsint = 2.0*s*(zmns(:,ihigh) - zmns(:,ilow))               &
     &                    * (nsvmec - 1.0)

                  rs = SUM(rmncint*cosmn_vmec)
                  zs = SUM(zmnsint*sinmn_vmec)

                  js = SUM(this%context%jksupsmnsf(:,:,i)*sinmn)
                  ju = SUM(this%context%jksupumncf(:,:,i)*cosmn)
                  jv = SUM(this%context%jksupvmncf(:,:,i)*cosmn)


                  IF (this%context%l_asym) THEN

                     IF (lasym) THEN
                        rmnsint = wlow*rmns(:,ilow)                            &
     &                          + whigh*rmns(:,ihigh)
                        zmncint = wlow*zmnc(:,ilow)                            &
     &                          + whigh*zmnc(:,ihigh)

                        this%magnetic_cache%rsuv(i,j,k) =                      &
     &                     this%magnetic_cache%rsuv(i,j,k) +                   &
     &                     SUM(rmnsint*sinmn_vmec)
                        this%magnetic_cache%zsuv(i,j,k) =                      &
     &                     this%magnetic_cache%zsuv(i,j,k) +                   &
     &                     SUM(zmncint*cosmn_vmec)

                        ru = ru + SUM(xm*rmnsint*cosmn_vmec)
                        rv = rv - SUM(xn*rmnsint*cosmn_vmec)
                        zu = zu - SUM(xm*zmncint*sinmn_vmec)
                        zv = zv + SUM(xn*zmncint*sinmn_vmec)

!  dR(s_v)/ds_s = dR(s_s)/ds_v*ds_v/ds_s = 2sdR(s_v)/ds_v
                        rmnsint = 2.0*s*(rmns(:,ihigh) - rmns(:,ilow))         &
     &                          * (nsvmec - 1.0)
!  dZ(s_v)/ds_s = dZ(s_s)/ds_v*ds_v/ds_s = 2sdZ(s_v)/ds_v
                        zmncint = 2.0*s*(zmnc(:,ihigh) - zmnc(:,ilow))         &
     &                          * (nsvmec - 1.0)

                        rs = rs + SUM(rmnsint*sinmn_vmec)
                        zs = zs + SUM(zmncint*cosmn_vmec)

                     END IF

                     js = js + SUM(this%context%jksupsmncf(:,:,i)*cosmn)
                     ju = ju + SUM(this%context%jksupumnsf(:,:,i)*sinmn)
                     jv = jv + SUM(this%context%jksupvmnsf(:,:,i)*sinmn)

                  END IF

                  js = js/(this%context%b_factor*mu0)
                  ju = ju/(this%context%b_factor*mu0)
                  jv = jv/(this%context%b_factor*mu0)

                  this%magnetic_cache%jrsuv(i,j,k) =                           &
     &               (js*rs + ju*ru + jv*rv)
                  this%magnetic_cache%jphisuv(i,j,k) =                         &
     &               jv*this%magnetic_cache%rsuv(i,j,k)
                  this%magnetic_cache%jzsuv(i,j,k) =                           &
     &               js*zs + ju*zu + jv*zv
               END DO
            END DO
         END DO
!$OMP END DO

         DEALLOCATE(cosmn)
         DEALLOCATE(sinmn)

         DEALLOCATE(cosmn_vmec)
         DEALLOCATE(sinmn_vmec)

         DEALLOCATE(rmncint)
         DEALLOCATE(zmnsint)
         IF (lasym) THEN
            DEALLOCATE(rmnsint)
            DEALLOCATE(zmncint)
         END IF

!$OMP END PARALLEL

         this%magnetic_cache%zsuv = this%magnetic_cache%zsuv                   &
     &                            + this%z_offset
      END IF

!  Compute the conducting shell if needed. If the kruv array is associated, the
!  the conducting shell is being used. The total conducting shell signal for a
!  magnetic diagnostic is
!
!    S_i = AreaIntegral K(u,v) . R_i(u,v) da                                 (1)
!
!  For now, It will be assumed that the last closed flux surface of the plasma
!  is the conducting shell. This can be refined later. The total magnetic signal
!  in flux coordinates becomes
!
!    S_i = AreaIntegral (-e^s/|e^s| X B/mu0) . R_i * |J||e^s|dudv            (2)
!
!  Using the idenity e^s = 1/J*e_u X e_v equation 2 becomes
!
!    S_i = AreaIntegral -e_u X e_v X B/mu0 . R_i |J|/J dudv                  (3)
!
!  From the triple cross product idenity equation 3 reduces to
!
!    S_i = AreaIntegral -1/mu0(B_u e_v - B_v e_u) . R_i |J|/J dudv           (4)
!
!  Computing the conducting shell surface current is simply.
!
!    K(u,v) = -1/mu0(B_u*e_v - B_v*e_u)                                      (5)
!
!  NOTE: The shell current cannot be used with the Z position shift.
!
!  NOTE: Equation 2.5.49a in "Flux Coordinates and Magnetic Field Structure" by
!        W.D.D'haeseleer, W.N.G.Hitchon, J.D.Callen and J.L.Shohet assumes a
!        positive jacobian. The correct equation should be
!
!            dS(i)=|J||e^i| du^j dv^k
!
!        As a result the signal due to the conducting shell contains a |J|/J
!        term. The sign of the jacobian needs to be taken into account. This is
!        taken care of in vmec_get_area_int_element.
!
!  Compute virtual casing points if needed, If the kruv_full array is
!  associated, the point magnetics are being used. The total signal for a point
!  is
!
!    B(x) = mu0/2Pi AreaIntegral K(u,v) x (x - x')/|x - x'|^3 da             (6)
!
!  The surface used is the last closed flux surface. Simular to the procedure
!  for the conducting shell, the different area element and surface current in
!  equation 2 reduce the point field to.
!
!    B(x) = mu0/2Pi AreaIntegral -1/mu0(B_u*e_v - B_v*e_u)
!         x (x - x')/|x - x'|^3 |J|/J dudv                                   (7)
!
!  Computing the virtual casing surface current is simply.
!
!    K(u,v) = -1/mu0(B_u*e_v - B_v*e_u)
!
!  NOTE: Unlike the shell current, the virtual casing can be used with the Z
!        position shift.

      IF (ASSOCIATED(this%magnetic_cache%kruv) .or.                            &
     &    ASSOCIATED(this%magnetic_cache%kxuv_full)) THEN

         ALLOCATE(bsubuc(0:mpol,-ntor:ntor))
         ALLOCATE(bsubvc(0:mpol,-ntor:ntor))

         numS = SIZE(this%context%bsubumnch, 3)

!  Interpolate the b_u and b_v onto the last closed flux surface.
         bsubuc = 1.5*this%context%bsubumnch(:,:,numS)                          &
     &          - 0.5*this%context%bsubumnch(:,:,numS - 1)
         bsubvc = 1.5*this%context%bsubvmnch(:,:,numS)                          &
     &          - 0.5*this%context%bsubvmnch(:,:,numS - 1)

         IF (this%context%l_asym) THEN
            ALLOCATE(bsubus(0:mpol,-ntor:ntor))
            ALLOCATE(bsubvs(0:mpol,-ntor:ntor))

            bsubus = 1.5*this%context%bsubumnsh(:,:,numS)                       &
     &             - 0.5*this%context%bsubumnsh(:,:,numS - 1)
            bsubvs = 1.5*this%context%bsubvmnsh(:,:,numS)                       &
     &             - 0.5*this%context%bsubvmnsh(:,:,numS - 1)
         END IF
      END IF

      IF (ASSOCIATED(this%magnetic_cache%kruv)) THEN
         numU = SIZE(this%magnetic_cache%kruv, 1)
         numV = SIZE(this%magnetic_cache%kruv, 2)

!  U grid is square with the s grid.
!$OMP PARALLEL
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(u,v,cosmn,sinmn,cosmn_vmec,sinmn_vmec,j,k,m,n,ru,rv,zu,
!$OMP&         zv,r,bu,bv)

         ALLOCATE(cosmn(0:mpol,-ntor:ntor))
         IF (this%context%l_asym) THEN
            ALLOCATE(sinmn(0:mpol,-ntor:ntor))
         END IF

         ALLOCATE(cosmn_vmec(mnmax))
         ALLOCATE(sinmn_vmec(mnmax))

!$OMP DO
!$OMP& SCHEDULE(STATIC)
         DO k = 1, numV
            v = (k - 1)*this%magnetic_cache%dv_a + this%phi_offset

            DO j = 1, numU
               u = (j - 1)*this%magnetic_cache%du_a

               cosmn_vmec = COS(xm*u - xn*v)
               sinmn_vmec = SIN(xm*u - xn*v)

               DO n = -ntor, ntor
                  DO m = 0, mpol
                     cosmn(m,n) = COS(m*u + nfp*n*v)
                  END DO
               END DO

               r = SUM(rmncint*cosmn_vmec)
               ru = -SUM(xm*rmnc(:,nsvmec)*sinmn_vmec)
               rv =  SUM(xn*rmnc(:,nsvmec)*sinmn_vmec)
               zu =  SUM(xm*zmns(:,nsvmec)*cosmn_vmec)
               zv = -SUM(xn*zmns(:,nsvmec)*cosmn_vmec)

               bu = SUM(bsubuc*cosmn)
               bv = SUM(bsubvc*cosmn)

               IF (this%context%l_asym) THEN
                  IF (lasym) THEN
                     r = r + SUM(rmnsint*sinmn_vmec)
                     ru = ru + SUM(xm*rmns(:,nsvmec)*cosmn_vmec)
                     rv = rv - SUM(xn*rmns(:,nsvmec)*cosmn_vmec)
                     zu = zu - SUM(xm*zmnc(:,nsvmec)*sinmn_vmec)
                     zv = zv + SUM(xn*zmnc(:,nsvmec)*sinmn_vmec)
                  END IF

                  DO n = -ntor, ntor
                     DO m = 0, mpol
                        sinmn(m,n) = SIN(m*u + nfp*n*v)
                     END DO
                  END DO

                  bu = bu + SUM(bsubus*sinmn)
                  bv = bv + SUM(bsubvs*sinmn)

               END IF

               this%magnetic_cache%kruv(j,k) = -(bu*rv - bv*ru)/mu0
               this%magnetic_cache%kphiuv(j,k) = -bu*r/mu0
               this%magnetic_cache%kzuv(j,k) = -(bu*zv - bv*zu)/mu0

            END DO
         END DO
!$OMP END DO

         DEALLOCATE(cosmn)
         IF (this%context%l_asym) THEN
            DEALLOCATE(sinmn)
         END IF

         DEALLOCATE(cosmn_vmec)
         DEALLOCATE(sinmn_vmec)
!$OMP END PARALLEL

      END IF

      IF (ASSOCIATED(this%magnetic_cache%kxuv_full)) THEN
         numU = SIZE(this%magnetic_cache%kxuv_full, 1)
         numV = SIZE(this%magnetic_cache%kxuv_full, 2)
!$OMP PARALLEL
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(cosmn,sinmn,cosmn_vmec,sinmn_vmec,k,j,v,u,n,m,r,z,
!$OMP&         ru,rv,zu,zv,bu,bv,cosphi,sinphi,kr,kp,i)

         ALLOCATE(cosmn(0:mpol,-ntor:ntor))
         IF (this%context%l_asym) THEN
            ALLOCATE(sinmn(0:mpol,-ntor:ntor))
         END IF

         ALLOCATE(cosmn_vmec(mnmax))
         ALLOCATE(sinmn_vmec(mnmax))

!$OMP DO
!$OMP& SCHEDULE(STATIC)
         DO k = 1, numV
!  Rotate the signal with respect to a fixed plasma. The phi and z offsets are
!  applied in siesta_get_ext_b_plasma.
            v = (k - 1)*this%magnetic_cache%dv_full

            cosphi = COS(v)
            sinphi = SIN(v)

            DO j = 1, numU
               u = (j - 1)*this%magnetic_cache%du_full

               cosmn_vmec = COS(xm*u - xn*v)
               sinmn_vmec = SIN(xm*u - xn*v)

               DO n = -ntor, ntor
                  DO m = 0, mpol
                     cosmn(m,n) = COS(m*u + nfp*n*v)
                  END DO
               END DO

               r = SUM(rmnc(:,nsvmec)*cosmn_vmec)
               z = SUM(zmns(:,nsvmec)*sinmn_vmec)

               ru = -SUM(xm*rmnc(:,nsvmec)*sinmn_vmec)
               rv =  SUM(xn*rmnc(:,nsvmec)*sinmn_vmec)
               zu =  SUM(xm*zmns(:,nsvmec)*cosmn_vmec)
               zv = -SUM(xn*zmns(:,nsvmec)*cosmn_vmec)

               bu = SUM(bsubuc*cosmn)
               bv = SUM(bsubvc*cosmn)

               IF (this%context%l_asym) THEN

                  DO n = -ntor, ntor
                     DO m = 0, mpol
                        sinmn(m,n) = SIN(m*u + nfp*n*v)
                     END DO
                  END DO

                  IF (lasym) THEN
                     r = r + SUM(rmns(:,nsvmec)*sinmn_vmec)
                     z = z + SUM(zmnc(:,nsvmec)*cosmn_vmec)

                     ru = ru + SUM(xm*rmns(:,nsvmec)*cosmn_vmec)
                     rv = rv - SUM(xn*rmns(:,nsvmec)*cosmn_vmec)
                     zu = zu - SUM(xm*zmnc(:,nsvmec)*sinmn_vmec)
                     zv = zv + SUM(xn*zmnc(:,nsvmec)*sinmn_vmec)
                  END IF

                  bu = bu + SUM(bsubus*sinmn)
                  bv = bv + SUM(bsubvs*sinmn)
               END IF

               kr = -(bu*rv - bv*ru)/mu0
               kp = -bu*r/mu0

               this%magnetic_cache%kxuv_full(j,k) = kr*cosphi                  &
     &                                            - kp*sinphi
               this%magnetic_cache%kyuv_full(j,k) = kr*sinphi                  &
     &                                            + kp*cosphi
               this%magnetic_cache%kzuv_full(j,k) = -(bu*zv - bv*zu)/mu0

               this%magnetic_cache%x_prime(j,k,1) = r*cosphi
               this%magnetic_cache%x_prime(j,k,2) = r*sinphi
               this%magnetic_cache%x_prime(j,k,3) = z

               IF (ASSOCIATED(this%magnetic_cache%kxuv_axi)) THEN
                  r = 0.0
                  z = 0.0
                  ru = 0.0
                  zu = 0.0

                  bu = 0.0
                  bv = 0.0

                  DO i = 1, SIZE(xn)
                     IF (xn(i) .eq. 0.0) THEN
                        r = r + rmnc(i,nsvmec)*cosmn_vmec(i)
                        z = z + zmns(i,nsvmec)*sinmn_vmec(i)
                        ru = ru - xm(i)*rmnc(i,nsvmec)*sinmn_vmec(i)
                        zu = zu + xm(i)*zmns(i,nsvmec)*cosmn_vmec(i)
                     END IF
                  END DO

                  bu = SUM(bsubuc(:,0)*cosmn(:,0))
                  bv = SUM(bsubvc(:,0)*cosmn(:,0))

                  IF (this%context%l_asym) THEN

                     DO i = 1, SIZE(xn)
                        IF (xn(i) .eq. 0.0) THEN
                           r = r + rmns(i,nsvmec)*sinmn_vmec(i)
                           z = z + zmnc(i,nsvmec)*cosmn_vmec(i)
                           ru = ru + xm(i)*rmns(i,nsvmec)*cosmn_vmec(i)
                           zu = zu - xm(i)*zmnc(i,nsvmec)*sinmn_vmec(i)
                        END IF
                     END DO

                     bu = bu + SUM(bsubus(:,0)*sinmn(:,0))
                     bv = bv + SUM(bsubvs(:,0)*sinmn(:,0))

                  END IF

                  bu = bu/this%context%b_factor
                  bv = bv/this%context%b_factor

                  kr =  bv*ru/mu0
                  kp = -bu*r/mu0

                  this%magnetic_cache%kxuv_axi(j,k) = kr*cosphi                &
     &                                              - kp*sinphi
                  this%magnetic_cache%kyuv_axi(j,k) = kr*sinphi                &
     &                                              + k*cosphi
                  this%magnetic_cache%kzuv_axi(j,k) = 1.0/mu0*bv*zu

                  this%magnetic_cache%x_axi(j,k,1) = r*cosphi
                  this%magnetic_cache%x_axi(j,k,2) = r*sinphi
                  this%magnetic_cache%x_axi(j,k,3) = z

               END IF
            END DO
         END DO
!$OMP END DO

         DEALLOCATE(cosmn)
         IF (this%context%l_asym) THEN
            DEALLOCATE(sinmn)
         END IF

         DEALLOCATE(cosmn_vmec)
         DEALLOCATE(sinmn_vmec)

!$OMP END PARALLEL
      END IF

      IF (ASSOCIATED(this%magnetic_cache%kruv) .or.                            &
     &    ASSOCIATED(this%magnetic_cache%kxuv_full)) THEN
         DEALLOCATE(bsubuc)
         DEALLOCATE(bsubvc)

         IF (this%context%l_asym) THEN
            DEALLOCATE(bsubus)
            DEALLOCATE(bsubvs)
         END IF
      END IF

      CALL profiler_set_stop_time('siesta_set_magnetic_cache_calc',            &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Sets namelist variables from the solved siesta equilibrium.
!>
!>  This method updates the pertubration size and vmec namelist inputs.
!>
!>  @todo FIXME: Currently not implemented for siesta.
!>
!>  @param[in] this A @ref siesta_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_set_namelist(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL vmec_set_namelist(this%vmec)

      CALL profiler_set_stop_time('siesta_set_namelist', start_time)

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Get the id for a reconstruction parameter.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_param_id.
!>
!>  @param[in] this       A @ref siesta_class instance.
!>  @param[in] param_name Name of a reconstruction parameter.
!>  @returns The id for a reconstruction parameter.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_param_id(this, param_name)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER                         :: siesta_get_param_id
      TYPE (siesta_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)   :: param_name

!  local variables
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (TRIM(param_name))

         CASE ('pp_ne_b')
            siesta_get_param_id = siesta_pp_ne_b_id

         CASE ('pp_ne_as')
            siesta_get_param_id = siesta_pp_ne_as_id

         CASE ('pp_ne_af')
            siesta_get_param_id = siesta_pp_ne_af_id

         CASE ('pp_sxrem_b_a')
            siesta_get_param_id = siesta_pp_sxrem_b_id

         CASE ('pp_sxrem_as_a')
            siesta_get_param_id = siesta_pp_sxrem_as_id

         CASE ('pp_sxrem_af_a')
            siesta_get_param_id = siesta_pp_sxrem_af_id

         CASE ('pp_te_b')
            siesta_get_param_id = siesta_pp_te_b_id

         CASE ('pp_te_as')
            siesta_get_param_id = siesta_pp_te_as_id

         CASE ('pp_te_af')
            siesta_get_param_id = siesta_pp_te_af_id

         CASE ('pp_ti_b')
            siesta_get_param_id = siesta_pp_ti_b_id

         CASE ('pp_ti_as')
            siesta_get_param_id = siesta_pp_ti_as_id

         CASE ('pp_ti_af')
            siesta_get_param_id = siesta_pp_ti_af_id

         CASE ('phi_offset')
            siesta_get_param_id = siesta_phi_offset_id

         CASE ('z_offset')
            siesta_get_param_id = siesta_z_offset_id

         CASE ('helpert')
            siesta_get_param_id = siesta_helpert_id

         CASE DEFAULT
            siesta_get_param_id = vmec_get_param_id(this%vmec,                 &
     &                                              param_name)

      END SELECT

      CALL profiler_set_stop_time('siesta_get_param_id', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the value of a reconstruction siesta parameter.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_param_value.
!>
!>  @param[in] this    A @ref siesta_class instance.
!>  @param[in] id      ID of the parameter.
!>  @param[in] i_index The ith index of the parameter.
!>  @param[in] j_index The jth index of the parameter.
!>  @returns The value of the parameter.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_param_value(this, id, i_index, j_index)
      USE siesta_namelist, ONLY: helpert

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: siesta_get_param_value
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: id
      INTEGER, INTENT(in)             :: i_index
      INTEGER, INTENT(in)             :: j_index

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (id)

         CASE (siesta_pp_ne_b_id)
            siesta_get_param_value = this%ne%b(i_index)

         CASE (siesta_pp_ne_as_id)
            siesta_get_param_value = this%ne%as(i_index)

         CASE (siesta_pp_ne_af_id)
            siesta_get_param_value = this%ne%af(i_index)

         CASE (siesta_pp_sxrem_b_id)
            siesta_get_param_value = this%sxrem(i_index)%p%b(j_index)

         CASE (siesta_pp_sxrem_as_id)
            siesta_get_param_value = this%sxrem(i_index)%p%as(j_index)

         CASE (siesta_pp_sxrem_af_id)
            siesta_get_param_value = this%sxrem(i_index)%p%af(j_index)

         CASE (siesta_pp_te_b_id)
            siesta_get_param_value = this%te%b(i_index)

         CASE (siesta_pp_te_as_id)
            siesta_get_param_value = this%te%as(i_index)

         CASE (siesta_pp_te_af_id)
            siesta_get_param_value = this%te%af(i_index)

         CASE (siesta_pp_ti_b_id)
            siesta_get_param_value = this%ti%b(i_index)

         CASE (siesta_pp_ti_as_id)
            siesta_get_param_value = this%ti%as(i_index)

         CASE (siesta_pp_ti_af_id)
            siesta_get_param_value = this%ti%af(i_index)

         CASE (siesta_phi_offset_id)
            siesta_get_param_value = this%phi_offset

         CASE (siesta_z_offset_id)
            siesta_get_param_value = this%z_offset

         CASE (siesta_helpert_id)
            siesta_get_param_value = helpert(i_index)

         CASE DEFAULT
            siesta_get_param_value =                                           &
     &         vmec_get_param_value(this%vmec, id, i_index, j_index)

      END SELECT

      CALL profiler_set_stop_time('siesta_get_param_value', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the name of a reconstruction siesta parameter.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_param_name.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns The name of the parameter.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_param_name(this, id)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER(len=data_name_length) :: siesta_get_param_name
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: id

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (id)

         CASE (siesta_pp_ne_b_id)
            siesta_get_param_name = 'pp_ne_b'

         CASE (siesta_pp_ne_as_id)
            siesta_get_param_name = 'pp_ne_as'

         CASE (siesta_pp_ne_af_id)
            siesta_get_param_name = 'pp_ne_af'

         CASE (siesta_pp_sxrem_b_id)
            siesta_get_param_name = 'pp_sxrem_b_a'

         CASE (siesta_pp_sxrem_as_id)
            siesta_get_param_name = 'pp_sxrem_as_a'

         CASE (siesta_pp_sxrem_af_id)
            siesta_get_param_name = 'pp_sxrem_af_a'

         CASE (siesta_pp_te_b_id)
            siesta_get_param_name = 'pp_te_b'

         CASE (siesta_pp_te_as_id)
            siesta_get_param_name = 'pp_te_as'

         CASE (siesta_pp_te_af_id)
            siesta_get_param_name = 'pp_te_af'

         CASE (siesta_pp_ti_b_id)
            siesta_get_param_name = 'pp_ti_b'

         CASE (siesta_pp_ti_as_id)
            siesta_get_param_name = 'pp_ti_as'

         CASE (siesta_pp_ti_af_id)
            siesta_get_param_name = 'pp_ti_af'

         CASE (siesta_phi_offset_id)
            siesta_get_param_name = 'phi_offset'

         CASE (siesta_z_offset_id)
            siesta_get_param_name = 'z_offset'

         CASE (siesta_helpert_id)
            siesta_get_param_name = 'helpert'

         CASE DEFAULT
            siesta_get_param_name =                                            &
     &         vmec_get_param_name(this%vmec, id)

      END SELECT

      CALL profiler_set_stop_time('siesta_get_param_name', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the number of electron density gp kernel hyper parameters.
!>
!>  This method overrides
!>  @ref equilibrium::equilibrium_get_gp_ne_num_hyper_param. If no density
!>  profile was created zero is returned.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_ne_num_hyper_param(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: siesta_get_gp_ne_num_hyper_param
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%ne)) THEN
         siesta_get_gp_ne_num_hyper_param =                                    &
     &      pprofile_get_gp_num_hyper_param(this%ne)
      ELSE
         siesta_get_gp_ne_num_hyper_param = 0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_ne_num_hyper_param',          &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron density profile af array.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_ne_af. If no density
!>  profile was created a null pointer is returned.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns Pointer to the electron density profile af array.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_ne_af(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER :: siesta_get_ne_af
      TYPE (siesta_class), INTENT(in)     :: this

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%ne)) THEN
         siesta_get_ne_af => this%ne%af
      ELSE
         siesta_get_ne_af => null()
      END IF

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron density gp kernel value for the two indicies.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_gp_ne_ij. If no
!>  density profile was created zero is returned.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @param[in] i    ith profile position.
!>  @param[in] j    jth profile position.
!>  @returns The value of the gp kernel function for i, j.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_ne_ij(this, i, j)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: siesta_get_gp_ne_ij
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: i
      INTEGER, INTENT(in)             :: j

!  local variables
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%ne)) THEN
         siesta_get_gp_ne_ij = pprofile_get_gp(this%ne, i, j)
      ELSE
         siesta_get_gp_ne_ij = 0.0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_ne_ij', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron density gp kernel value for the position and index.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_gp_ne_pi. If no
!>  density profile was created zero is returned.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] x_cart Cartesian position to get the electron density at.
!>  @param[in] i      Profile position index.
!>  @returns The value of the gp kernel function for x_cart and i.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_ne_pi(this, x_cart, i)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: siesta_get_gp_ne_pi
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: i

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%ne)) THEN
         siesta_get_gp_ne_pi = pprofile_get_gp(this%ne,                        &
     &                                         siesta_get_p(this,              &
     &                                                      x_cart,            &
     &                                                      .true.),           &
     &                                         i)
      ELSE
         siesta_get_gp_ne_pi = 0.0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_ne_pi', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron density gp kernel value for the position and
!>  position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_gp_ne_pp. If no
!>  density profile was created zero is returned.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] x_cart First cartesian position to get the electron density at.
!>  @param[in] y_cart Second cartesian position to get the electron density at.
!>  @returns The value of the gp kernel function for x_cart and y_cart.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_ne_pp(this, x_cart, y_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: siesta_get_gp_ne_pp
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      REAL (rprec), DIMENSION(3), INTENT(in) :: y_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%ne)) THEN
         siesta_get_gp_ne_pp =                                                 &
     &      pprofile_get_gp(this%ne, siesta_get_p(this, x_cart, .true.),       &
     &                      siesta_get_p(this, y_cart, .true.))
      ELSE
         siesta_get_gp_ne_pp = 0.0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_ne_pp', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron density at a cartesian position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_ne. If no density
!>  profile was created, return zero. Density is defined by a mapping function
!>  of the pressure.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] x_cart Cartesian position to get the electron density at.
!>  @returns The electron density at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_ne_cart(this, x_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: siesta_get_ne_cart
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      siesta_get_ne_cart =                                                     &
     &   pprofile_get_value(this%ne, siesta_get_p(this, x_cart, .true.))

      CALL profiler_set_stop_time('siesta_get_ne_cart', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron density at a s, u=0, v=0 position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_ne. If no density
!>  profile was created, return zero. Density is defined by a mapping function
!>  of the pressure.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @param[in] s    Radial position to get the electron density at.
!>  @returns The electron density at s, u=0, v=0.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_ne_radial(this, s)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: siesta_get_ne_radial
      TYPE (siesta_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)        :: s

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      siesta_get_ne_radial =                                                   &
     &   pprofile_get_value(this%ne, siesta_get_p(this, s, .true.))

      CALL profiler_set_stop_time('siesta_get_ne_radial', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the number of electron temperature gp kernel hyper parameters.
!>
!>  This method overrides
!>  @ref equilibrium::equilibrium_get_gp_te_num_hyper_param. If no electron
!>  temperature profile was created zero is returned.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_te_num_hyper_param(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: siesta_get_gp_te_num_hyper_param
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%te)) THEN
         siesta_get_gp_te_num_hyper_param =                                    &
     &      pprofile_get_gp_num_hyper_param(this%te)
      ELSE
         siesta_get_gp_te_num_hyper_param = 0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_te_num_hyper_param',          &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron temperature profile af array.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_te_af. If no
!>  electron temperature profile was created a null pointer is returned.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns Pointer to the electron temperature profile af array.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_te_af(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER :: siesta_get_te_af
      TYPE (siesta_class), INTENT(in)     :: this

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%te)) THEN
         siesta_get_te_af => this%te%af
      ELSE
         siesta_get_te_af => null()
      END IF

      CALL profiler_set_stop_time('siesta_get_te_af', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron temperature gp kernel value for the two indicies.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_gp_te_ij. If no
!>  electron temperature profile was created zero is returned.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @param[in] i    ith profile position.
!>  @param[in] j    jth profile position.
!>  @returns The value of the gp kernel function for i, j.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_te_ij(this, i, j)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: siesta_get_gp_te_ij
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: i
      INTEGER, INTENT(in)             :: j

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%te)) THEN
         siesta_get_gp_te_ij = pprofile_get_gp(this%te, i, j)
      ELSE
         siesta_get_gp_te_ij = 0.0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_te_ij', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron temperature gp kernel value for the position and
!>  index.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_gp_te_pi. If no
!>  electron temperature profile was created zero is returned.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] x_cart Cartesian position to get the electron temperature at.
!>  @param[in] i      Profile position index.
!>  @returns The value of the gp kernel function for x_cart and i.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_te_pi(this, x_cart, i)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: siesta_get_gp_te_pi
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: i

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%te)) THEN
         siesta_get_gp_te_pi = pprofile_get_gp(this%te,                        &
     &                                         siesta_get_p(this,              &
     &                                                      x_cart,            &
     &                                                      .true.),           &
     &                                         i)
      ELSE
         siesta_get_gp_te_pi = 0.0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_te_pi', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron temperature gp kernel value for the position and
!>  position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_gp_te_pp. If no
!>  electron temperature profile was created zero is returned.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] x_cart First cartesian position to get the electron temperature
!>                    at.
!>  @param[in] y_cart Second cartesian position to get the electron temperature
!>                    at.
!>  @returns The value of the gp kernel function for x_cart and y_cart.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_te_pp(this, x_cart, y_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: siesta_get_gp_te_pp
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      REAL (rprec), DIMENSION(3), INTENT(in) :: y_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%te)) THEN
         siesta_get_gp_te_pp =                                                 &
     &      pprofile_get_gp(this%te, siesta_get_p(this, x_cart, .true.),       &
     &                      siesta_get_p(this, y_cart, .true.))
      ELSE
         siesta_get_gp_te_pp = 0.0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_te_pp', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron temperature at a cartesian position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_te. If no
!>  electron temperature profile was created, return zero. Electron temperature
!>  is defined by a mapping function of the pressure.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] x_cart Cartesian position to get the electron temperature at.
!>  @returns The electron temperature at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_te_cart(this, x_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: siesta_get_te_cart
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      siesta_get_te_cart =                                                     &
     &   pprofile_get_value(this%te, siesta_get_p(this, x_cart, .true.))

      CALL profiler_set_stop_time('siesta_get_te_cart', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron temperature at a s, u=0, v=0 position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_te. If no
!>  electron temperature profile was created, return zero. Electron temperature
!>  is defined by a mapping function of the pressure.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @param[in] s    Radial position to get the electron density at.
!>  @returns The electron temperature at s, u=0, v=0.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_te_radial(this, s)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: siesta_get_te_radial
      TYPE (siesta_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)        :: s

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      siesta_get_te_radial =                                                   &
     &   pprofile_get_value(this%te, siesta_get_p(this, s, .true.))

      CALL profiler_set_stop_time('siesta_get_te_radial', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the number of ion temperature gp kernel hyper parameters.
!>
!>  This method overrides
!>  @ref equilibrium::equilibrium_get_gp_ti_num_hyper_param. If no ion
!>  temperature profile was created zero is returned.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_ti_num_hyper_param(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: siesta_get_gp_ti_num_hyper_param
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%ti)) THEN
         siesta_get_gp_ti_num_hyper_param =                                    &
     &      pprofile_get_gp_num_hyper_param(this%ti)
      ELSE
         siesta_get_gp_ti_num_hyper_param = 0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_ti_num_hyper_param',          &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the ion temperature profile af array.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_ti_af. If no ion
!>  temperature profile was created a null pointer is returned.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns Pointer to the electron temperature profile af array.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_ti_af(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER :: siesta_get_ti_af
      TYPE (siesta_class), INTENT(in)     :: this

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%ti)) THEN
         siesta_get_ti_af => this%ti%af
      ELSE
         siesta_get_ti_af => null()
      END IF

      CALL profiler_set_stop_time('siesta_get_ti_af', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the ion temperature gp kernel value for the two indicies.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_gp_te_ij. If no ion
!>  temperature profile was created zero is returned.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @param[in] i    ith profile position.
!>  @param[in] j    jth profile position.
!>  @returns The value of the gp kernel function for i, j.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_ti_ij(this, i, j)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: siesta_get_gp_ti_ij
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: i
      INTEGER, INTENT(in)             :: j

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%ti)) THEN
         siesta_get_gp_ti_ij = pprofile_get_gp(this%ti, i, j)
      ELSE
         siesta_get_gp_ti_ij = 0.0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_ti_ij', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the ion temperature gp kernel value for the position and index.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_gp_ti_pi. If no ion
!>  temperature profile was created zero is returned.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] x_cart Cartesian position to get the ion temperature at.
!>  @param[in] i      Profile position index.
!>  @returns The value of the gp kernel function for x_cart and i.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_ti_pi(this, x_cart, i)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: siesta_get_gp_ti_pi
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: i

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%ti)) THEN
         siesta_get_gp_ti_pi =                                                 &
     &      pprofile_get_gp(this%ti, siesta_get_p(this, x_cart, .true.),       &
     &                      i)
      ELSE
         siesta_get_gp_ti_pi = 0.0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_ti_pi', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the ion temperature gp kernel value for the position and
!>  position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_gp_ti_pp. If no ion
!>  temperature profile was created zero is returned.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] x_cart First cartesian position to get the ion temperature at.
!>  @param[in] y_cart Second cartesian position to get the ion temperature at.
!>  @returns The value of the gp kernel function for x_cart and y_cart.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_ti_pp(this, x_cart, y_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: siesta_get_gp_ti_pp
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      REAL (rprec), DIMENSION(3), INTENT(in) :: y_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%ti)) THEN
         siesta_get_gp_ti_pp =                                                 &
     &      pprofile_get_gp(this%ti, siesta_get_p(this, x_cart, .true.),       &
     &                      siesta_get_p(this, y_cart, .true.))
      ELSE
         siesta_get_gp_ti_pp = 0.0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_ti_pp', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the ion temperature at a cartesian position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_ti. If no ion
!>  temperature profile was created, return zero. Ion temperature is defined by
!>  a mapping function of the pressure.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] x_cart Cartesian position to get the ion temperature at.
!>  @returns The ion temperature at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_ti_cart(this, x_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: siesta_get_ti_cart
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      siesta_get_ti_cart =                                                     &
     &   pprofile_get_value(this%ti, siesta_get_p(this, x_cart, .true.))

      CALL profiler_set_stop_time('siesta_get_ti_cart', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the ion temperature at a s, u=0, v=0 position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_ti. If no ion
!>  ion temperature profile was created, return zero. Ion temperature is defined
!>  by a mapping function of the pressure.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @param[in] s    Radial position to get the ion density at.
!>  @returns The ion temperature at s, u=0, v=0.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_ti_radial(this, s)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: siesta_get_ti_radial
      TYPE (siesta_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)        :: s

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      siesta_get_ti_radial =                                                   &
     &   pprofile_get_value(this%ti, siesta_get_p(this, s, .true.))

      CALL profiler_set_stop_time('siesta_get_ti_radial', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the number of soft x-ray emission gp kernel hyper parameters.
!>
!>  This method overrides
!>  @ref equilibrium::equilibrium_get_gp_sxrem_num_hyper_param. If no soft x-ray
!>  emission profile was created zero is returned.
!>
!>  @param[in] this  A @ref vmec_class instance.
!>  @param[in] index Index of the soft x-ray emissivity profile.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_sxrem_num_hyper_param(this, index)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: siesta_get_gp_sxrem_num_hyper_param
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: index

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%sxrem) .and.                                         &
     &    (index .le. SIZE(this%sxrem))) THEN
         siesta_get_gp_sxrem_num_hyper_param =                                 &
     &      pprofile_get_gp_num_hyper_param(this%sxrem(index)%p)
      ELSE
         siesta_get_gp_sxrem_num_hyper_param = 0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_sxrem_num_hyper_param',       &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the soft x-ray emissivity profile af array.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_sxrem_af. If no soft
!>  x-ray emissivity profile was created a null pointer is returned.
!>
!>  @param[in] this  A @ref siesta_class instance.
!>  @param[in] index Index of the soft x-ray emissivity profile.
!>  @returns Pointer to the electron temperature profile af array.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_sxrem_af(this, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER :: siesta_get_sxrem_af
      TYPE (siesta_class), INTENT(in)     :: this
      INTEGER, INTENT(in)                 :: index

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%sxrem) .and.                                         &
     &    (index .le. SIZE(this%sxrem))) THEN
         siesta_get_sxrem_af => this%sxrem(index)%p%af
      ELSE
         siesta_get_sxrem_af => null()
      END IF

      CALL profiler_set_stop_time('siesta_get_sxrem_af', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the soft x-ray emissivity gp kernel value for the two indicies.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_gp_sxrem_ij. If no
!>  soft x-ray emissivity profile was created zero is returned.
!>
!>  @param[in] this  A @ref siesta_class instance.
!>  @param[in] i     ith profile position.
!>  @param[in] j     jth profile position.
!>  @param[in] index Index of the soft x-ray emissivity profile.
!>  @returns The value of the gp kernel function for i, j.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_sxrem_ij(this, i, j, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: siesta_get_gp_sxrem_ij
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: i
      INTEGER, INTENT(in)             :: j
      INTEGER, INTENT(in)             :: index

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%sxrem) .and.                                         &
     &    (index .le. SIZE(this%sxrem))) THEN
         siesta_get_gp_sxrem_ij =                                              &
     &      pprofile_get_gp(this%sxrem(index)%p, i, j)
      ELSE
         siesta_get_gp_sxrem_ij = 0.0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_sxrem_ij', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the soft x-ray emissivity gp kernel value for the position and
!>  index.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_gp_sxrem_pi. If no
!>  density profile was created zero is returned.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] x_cart Cartesian position to get the soft x-ray emissivity at.
!>  @param[in] i      Profile position index.
!>  @param[in] index  Index of the soft x-ray emissivity profile.
!>  @returns The value of the gp kernel function for x_cart and i.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_sxrem_pi(this, x_cart, i, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: siesta_get_gp_sxrem_pi
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: i
      INTEGER, INTENT(in)                    :: index

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%sxrem) .and.                                         &
     &    (index .le. SIZE(this%sxrem))) THEN
         siesta_get_gp_sxrem_pi = pprofile_get_gp(this%sxrem(index)%p,         &
     &                                            siesta_get_p(this,           &
     &                                                         x_cart,         &
     &                                                         .true.),        &
     &                                            i)
      ELSE
         siesta_get_gp_sxrem_pi = 0.0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_sxrem_pi', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the soft x-ray emissivity gp kernel value for the position and
!>  position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_gp_sxrem_pp. If no
!>  soft x-ray emissivity profile was created zero is returned.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] x_cart First cartesian position to get the soft x-ray emissivity
!>                    at.
!>  @param[in] y_cart Second cartesian position to get the soft x-ray emissivity
!>                    at.
!>  @param[in] index  Index of the soft x-ray emissivity profile.
!>  @returns The value of the gp kernel function for x_cart and y_cart.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_gp_sxrem_pp(this, x_cart, y_cart, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: siesta_get_gp_sxrem_pp
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      REAL (rprec), DIMENSION(3), INTENT(in) :: y_cart
      INTEGER, INTENT(in)                    :: index

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%sxrem) .and.                                         &
     &    (index .le. SIZE(this%sxrem))) THEN
         siesta_get_gp_sxrem_pp =                                              &
     &      pprofile_get_gp(this%sxrem(index)%p,                               &
     &                      siesta_get_p(this, x_cart, .true.),                &
     &                      siesta_get_p(this, y_cart, .true.))
      ELSE
         siesta_get_gp_sxrem_pp = 0.0
      END IF

      CALL profiler_set_stop_time('siesta_get_gp_sxrem_pp', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the soft x-ray emissivity at a cartesian position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_sxrem. If no soft
!>  x-ray emissivity profile was created, return zero. Soft x-ray emissivity is
!>  defined by a mapping function of the pressure.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] x_cart Cartesian position to get the soft x-ray emissivity at.
!>  @param[in] index  Index of the soft x-ray emissivity profile.
!>  @returns The soft x-ray emissivity at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_sxrem_cart(this, x_cart, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: siesta_get_sxrem_cart
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: index

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%sxrem) .and.                                         &
     &    (index .le. SIZE(this%sxrem))) THEN
         siesta_get_sxrem_cart =                                               &
     &      pprofile_get_value(this%sxrem(index)%p,                            &
     &                         siesta_get_p(this, x_cart, .true.))
      ELSE
         siesta_get_sxrem_cart = 0.0
      END IF

      CALL profiler_set_stop_time('siesta_get_sxrem_cart', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the soft x-ray emissivity at a s, u=0, v=0 position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_sxrem. If no soft
!>  x-ray emissivity profile was created, return zero. Soft x-ray emissivity is
!>  defined by a mapping function of the pressure.
!>
!>  @param[in] this  A @ref siesta_class instance.
!>  @param[in] s     Radial position to get the electron density at.
!>  @param[in] index Index of the soft x-ray emissivity profile.
!>  @returns The soft x-ray emissivity at s, u=0, v=0.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_sxrem_radial(this, s, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: siesta_get_sxrem_radial
      TYPE (siesta_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)        :: s
      INTEGER, INTENT(in)             :: index

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%sxrem) .and.                                         &
     &    (index .le. SIZE(this%sxrem))) THEN
         siesta_get_sxrem_radial =                                             &
     &      pprofile_get_value(this%sxrem(index)%p,                            &
     &                         siesta_get_p(this, s, .true.))
      ELSE
         siesta_get_sxrem_radial = 0.0
      END IF

      CALL profiler_set_stop_time('siesta_get_sxrem_radial', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the plasma pressure at a cartesian position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_p.
!>
!>  @param[in] this      A @ref siesta_class instance.
!>  @param[in] x_cart    Cartesian position to get the plasma pressure at.
!>  @param[in] normalize Normalize the pressure value.
!>  @returns The plasma pressure at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_p_cart(this, x_cart, normalize)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: siesta_get_p_cart
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      LOGICAL, INTENT(in)                    :: normalize

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      siesta_get_p_cart =                                                      &
     &   siesta_get_p_flux(this, siesta_get_suv(this, x_cart),                 &
     &                     normalize)

      CALL profiler_set_stop_time('siesta_get_p_cart', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the plasma pressure at a s, u=0, v=0 position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_p.
!>
!>  @param[in] this      A @ref siesta_class instance.
!>  @param[in] s         Cartesian position to get the plasma pressure at.
!>  @param[in] normalize Normalize the pressure value.
!>  @returns The plasma pressure at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_p_radial(this, s, normalize)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: siesta_get_p_radial
      TYPE (siesta_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)        :: s
      LOGICAL, INTENT(in)             :: normalize

!  local variables
      REAL (rprec), DIMENSION(3)      :: flux
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      flux(1) = MIN(s, 1.0)
      flux(2:3) = 0.0
      siesta_get_p_radial = siesta_get_p_flux(this, flux, normalize)

      CALL profiler_set_stop_time('siesta_get_p_cart', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the plasma pressure at a flux position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_p.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] s      Radial position to get the electron density at.
!>  @param[in] normalize Normalize the pressure value.
!>  @returns The plasma pressure at flux.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_p_flux(this, flux, normalize)
      USE stel_constants, only: mu0

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                           :: siesta_get_p_flux
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: flux
      LOGICAL, INTENT(in)                    :: normalize

!  local variables
      REAL (rprec)                           :: ds
      REAL (rprec)                           :: wlow
      REAL (rprec)                           :: whigh
      INTEGER                                :: ilow
      INTEGER                                :: ihigh
      INTEGER                                :: mpol
      INTEGER                                :: ntor
      INTEGER                                :: nfp
      INTEGER                                :: m
      INTEGER                                :: n
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ds = 1.0/(this%context%ns - 1.0)

      IF (flux(1) .lt. ds/2.0) THEN
         ilow = 2
         ihigh = 3
         whigh = flux(1) - ds/2.0
         wlow = 1.0 - whigh
      ELSE IF (flux(1) .gt. 1.0 - ds/2.0) THEN
         ihigh = this%context%ns
         ilow = ihigh - 1
         wlow = -flux(1)/ds - 0.5 + 1.0/ds
         whigh = 1.0 - wlow
      ELSE
         ilow = FLOOR(flux(1)/ds + 1.5)
         ihigh = ilow + 1
         whigh = flux(1)/ds + 1.5 - ilow
         wlow = 1.0 - whigh
      END IF

      mpol = this%context%mpol
      ntor = this%context%ntor
      nfp = this%context%nfp

      siesta_get_p_flux = 0.0
      DO n = -ntor, ntor
         DO m = 0, mpol
            siesta_get_p_flux = siesta_get_p_flux +                            &
     &         (wlow*this%context%pmnch(m,n,ilow) +                            &
     &          whigh*this%context%pmnch(m,n,ihigh))*                          &
     &         COS(m*flux(2) + nfp*n*flux(3))

            IF (this%context%l_asym) THEN
               siesta_get_p_flux = siesta_get_p_flux +                         &
     &            (wlow*this%context%pmnsh(m,n,ilow) +                         &
     &             whigh*this%context%pmnsh(m,n,ihigh))*                       &
     &            SIN(m*flux(2) + nfp*n*flux(3))
            END IF
         END DO
      END DO

!  The normalizes pressure if requested, scale the pressure to the min and max
!  pressure. Otherwise apply constants to comvert to pascals.
      IF (normalize) THEN
!  Ensure that the pressure is not outside the bounds of pmin and pmax.
         siesta_get_p_flux = MIN(siesta_get_p_flux, this%context%p_max)
         siesta_get_p_flux = MAX(siesta_get_p_flux, this%context%p_min)

         siesta_get_p_flux = (siesta_get_p_flux - this%context%p_min)          &
     &                     / (this%context%p_max - this%context%p_min)
      END IF

      CALL profiler_set_stop_time('siesta_get_p_flux', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the magnetic field vector at a position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_B_vec.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] x_cart Cartesian position to get the magnetic field vector at.
!>  @param[in] cyl    Flag that specifies if the bfield should be returned in
!>                    cartesian or cylindical coordinates.
!>  @returns The magnetic field vector at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_B_vec(this, x_cart, cyl)
      USE coordinate_utilities, ONLY : cyl_to_cart_vec, cart_to_cyl
      USE mgrid_mod

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(3)             :: siesta_get_B_vec
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      LOGICAL, INTENT(in)                    :: cyl

!  local variables
      REAL (rprec), DIMENSION(3)             :: r_cyl
      REAL (rprec), DIMENSION(3)             :: flux
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Determine if the point is inside or outside the last closed flux surface.
      flux = siesta_get_suv(this, x_cart)

!  There is no need to shift this.
      r_cyl = cart_to_cyl(x_cart)

      siesta_get_B_vec = 0.0
      IF (flux(1) .gt. 1.0) THEN
         IF (lfreeb) THEN
            siesta_get_B_vec = vmec_get_B_vac(this%vmec, r_cyl)                &
     &                       + siesta_get_ext_b_plasma(this, r_cyl,            &
     &                                                 .false.)
         END IF
      ELSE
         siesta_get_B_vec = siesta_get_int_b_plasma(this, flux)
      END IF

      IF (.not.cyl) THEN
         siesta_get_B_vec = cyl_to_cart_vec(r_cyl, siesta_get_B_vec)
      END IF

      CALL profiler_set_stop_time('siesta_get_B_vec', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the loop integrated magnetic field at a position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_Int_B_dphi. This
!>  computes Int[B*dl]
!>
!>  @param[in] this  A @ref siesta_class instance.
!>  @param[in] r     S position to integrate about.
!>  @param[in] theta U angle to integrate about.
!>  @returns The loop integrated magnetic field at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_Int_B_dphi(this, r, theta)
      USE line_segment, only: line_seg
      USE stel_constants, only: twopi
      USE read_wout_mod, only: bsubvmnc, ns

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: siesta_get_Int_B_dphi
      TYPE (siesta_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)        :: r
      REAL (rprec), INTENT(in)        :: theta

!  local variables
      REAL (rprec)                    :: bsubv00c, ds
      INTEGER                         :: i
      REAL (rprec), DIMENSION(2)      :: s
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      ds = 1.0/(ns - 1)

      s(1) = 1.0 - 1.5*ds
      s(2) = 1.0 - 0.5*ds

      CALL line_seg(r, bsubv00c,                                               &
     &              s, this%context%bsubvmnch(0,0,ns - 1:ns), 2)

      siesta_get_Int_B_dphi = twopi*bsubv00c/this%context%b_factor

      CALL profiler_set_stop_time('siesta_get_Int_B_dphi', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets siesta s value at position.
!>
!>  Converts from cartesian coordinates to siesta's flux coordinates. Unlike
!>  vmec where only the s position in flux surface space is needed, the full
!>  flux surface space potition is needed here.
!>
!>  @param[in] this   A @ref siesta_class instance.
!>  @param[in] x_cart Cartesian position to get the s position at.
!>  @returns The suv position at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_suv(this, x_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(3)             :: siesta_get_suv
      TYPE (siesta_class), INTENT(in)        :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart

!  local variables
      REAL (rprec), DIMENSION(3)             :: flux
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      flux = vmec_get_suv(this%vmec, x_cart)

      siesta_get_suv =                                                         &
     &   (/ siesta_to_siesta_s(flux(1)), flux(2), flux(3) /)

      CALL profiler_set_stop_time('siesta_get_suv', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the r and z positions of the outer surface at a toroidal angle.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_plasma_edge.
!>
!>  @todo FIXME: The vmec boundary won't be the case for Free boundary siesta.
!>
!>  @param[in]    this A @ref siesta_class instance.
!>  @param[in]    phi  Toroidal angle to determine the outer surface at.
!>  @param[inout] r    The radial postions of the other surface in a single
!>                     toroidal angle.
!>  @param[inout] z    The Z postions of the other surface in a single toroidal
!>                     angle.
!>  @returns The number of elements in the r and z arrays.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_plasma_edge(this, phi, r, z)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: siesta_get_plasma_edge
      TYPE (siesta_class), INTENT(in)     :: this
      REAL (rprec), INTENT (in)           :: phi
      REAL (rprec), DIMENSION(:), POINTER :: r
      REAL (rprec), DIMENSION(:), POINTER :: z

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  The inital VMEC defines the boundary.
!  FIXME: Won't be the case for free boundary siesta.
      siesta_get_plasma_edge = vmec_get_plasma_edge(this%vmec, phi, r,         &
     &                                              z)

      CALL profiler_set_stop_time('siesta_get_plasma_edge', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get volume magnetic volume integration radial grid points.
!>
!>  This method overrides
!>  @ref equilibrium::equilibrium_get_magnetic_volume_rgrid.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns The radial grid points.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_magnetic_volume_rgrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:,:), POINTER ::                               &
     &   siesta_get_magnetic_volume_rgrid
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      siesta_get_magnetic_volume_rgrid => this%magnetic_cache%rsuv

      CALL profiler_set_stop_time('siesta_get_magnetic_volume_rgrid',          &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get volume magnetic volume integration z grid points.
!>
!>  This method overrides
!>  @ref equilibrium::equilibrium_get_magnetic_volume_zgrid.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns The radial grid points.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_magnetic_volume_zgrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:,:), POINTER ::                               &
     &   siesta_get_magnetic_volume_zgrid
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      siesta_get_magnetic_volume_zgrid => this%magnetic_cache%zsuv

      CALL profiler_set_stop_time('siesta_get_magnetic_volume_zgrid',          &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get volume magnetic volume integration jr grid points.
!>
!>  This method overrides
!>  @ref equilibrium::equilibrium_get_magnetic_volume_jrgrid.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns The radial grid points.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_magnetic_volume_jrgrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:,:), POINTER ::                               &
     &   siesta_get_magnetic_volume_jrgrid
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      siesta_get_magnetic_volume_jrgrid => this%magnetic_cache%jrsuv

      CALL profiler_set_stop_time('siesta_get_magnetic_volume_jrgrid',         &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get volume magnetic volume integration jphi grid points.
!>
!>  This method overrides
!>  @ref equilibrium::equilibrium_get_magnetic_volume_jphigrid.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns The radial grid points.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_magnetic_volume_jphigrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:,:), POINTER ::                               &
     &   siesta_get_magnetic_volume_jphigrid
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      siesta_get_magnetic_volume_jphigrid => this%magnetic_cache%jphisuv

      CALL profiler_set_stop_time('siesta_get_magnetic_volume_jphigrid',       &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get volume magnetic volume integration jz grid points.
!>
!>  This method overrides
!>  @ref equilibrium::equilibrium_get_magnetic_volume_jzgrid.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns The radial grid points.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_magnetic_volume_jzgrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:,:), POINTER ::                               &
     &   siesta_get_magnetic_volume_jzgrid
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      siesta_get_magnetic_volume_jzgrid => this%magnetic_cache%jzsuv

      CALL profiler_set_stop_time('siesta_get_magnetic_volume_jzgrid',         &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get volume integration element.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_volume_int_element.
!>  For vmec this is J*ds*du*dv where J is the jacobian. The jacobian is
!>  integrated into Jr, Jphi and Jz but still need to account for the sign of
!>  the jacobian.
!>
!>  * ds = 1/(ns - 1)
!>  * du = 2*Pi/number of flux surfaces
!>  * dv = 2*Pi/number of field periods/number of toroidal planes
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns The volume integration element.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_volume_int_element(this)
      USE read_wout_mod, only: isigng

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: siesta_get_volume_int_element
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      siesta_get_volume_int_element = isigng*this%magnetic_cache%ds            &
     &                              * this%magnetic_cache%du                   &
     &                              * this%magnetic_cache%dv

      CALL profiler_set_stop_time('siesta_get_volume_int_element',             &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the conducting surface integration kr grid points.
!>
!>  This method overrides
!>  @ref equilibrium::equilibrium_get_con_surface_krgrid.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns The radial grid points.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_con_surface_krgrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:), POINTER ::                                 &
     &   siesta_get_con_surface_krgrid
      TYPE (siesta_class), INTENT(in)       :: this

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      siesta_get_con_surface_krgrid => this%magnetic_cache%kruv

      CALL profiler_set_stop_time('siesta_get_con_surface_krgrid',             &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the conducting surface integration kphi grid points.
!>
!>  This method overrides
!>  @ref equilibrium::equilibrium_get_con_surface_kphigrid.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns The radial grid points.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_con_surface_kphigrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:), POINTER ::                                 &
     &   siesta_get_con_surface_kphigrid
      TYPE (siesta_class), INTENT(in)       :: this

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      siesta_get_con_surface_kphigrid => this%magnetic_cache%kphiuv

      CALL profiler_set_stop_time('siesta_get_con_surface_kphigrid',           &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the conducting surface integration kz grid points.
!>
!>  This method overrides
!>  @ref equilibrium::equilibrium_get_con_surface_kzgrid.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns The radial grid points.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_con_surface_kzgrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:), POINTER ::                                 &
     &   siesta_get_con_surface_kzgrid
      TYPE (siesta_class), INTENT(in)       :: this

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      siesta_get_con_surface_kzgrid => this%magnetic_cache%kzuv

      CALL profiler_set_stop_time('siesta_get_con_surface_kzgrid',             &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get area integration element.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_area_int_element.
!>  For vmec this is |J||e^s|*du*dv where J is the jacobian. A 1/J term is
!>  integrated into kr, kphi and kz. Since due to the resulting |J|/J term, the
!>  the sign of the jacobian must be taken into account.
!>  @see siesta_equilibrium::siesta_set_magnetic_cache_calc equation 3.
!>
!>  * du = 2*Pi/number of u grid
!>  * dv = 2*Pi/number of field periods/number of toroidal planes
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns The area integration element.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_area_int_element(this)
      USE read_wout_mod, only: isigng

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: siesta_get_area_int_element
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      siesta_get_area_int_element = this%magnetic_cache%du_a                   &
     &                            * this%magnetic_cache%dv_a*isigng

      CALL profiler_set_stop_time('siesta_get_area_int_element',               &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get external current.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_ext_currents.
!>  External currents are defered to the underlying vmec.
!>
!>  @param[in]  this           A @ref siesta_class instance.
!>  @param[in]  num_currents   Forces the number of currents to return if
!>                             greater than zero.
!>  @param[out] scale_currents Informs the caller that currents need to be
!>                             scaled.
!>  @returns The external currents.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_ext_currents(this, num_currents,                     &
     &                                 scale_currents)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER :: siesta_get_ext_currents
      TYPE (siesta_class), INTENT(in)     :: this
      INTEGER, INTENT(in)                 :: num_currents
      LOGICAL, INTENT(out)                :: scale_currents

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      siesta_get_ext_currents =>                                               &
     &   vmec_get_ext_currents(this%vmec, num_currents, scale_currents)

      CALL profiler_set_stop_time('vmec_get_ext_currents', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get external plasma magnetic field.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_ext_b_plasma. The b
!>  field is returned in cyclindical coordinates. This function does not require
!>  shifting the plasma relative to the diagnostics. The plasma has already been
!>  shifted when computing the surface currents and prime position.
!>
!>  @param[in] this     A @ref siesta_class instance.
!>  @param[in] position Position to compute the fields at in cylindrical
!>                      coordinates.
!>  @param[in] axi_only Gives only the axisymmtric component of the magnetic
!>                      field.
!>  @returns The external field contributed by the plasma.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_ext_b_plasma(this, position, axi_only)
      USE coordinate_utilities
      USE read_wout_mod, only: isigng

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(3)              :: siesta_get_ext_b_plasma
      TYPE (siesta_class), INTENT(in)         :: this
      REAL (rprec), DIMENSION(3), INTENT(in)  :: position
      LOGICAL, INTENT(in)                     :: axi_only

!  local variables
      REAL (rprec), DIMENSION(3)              :: x_cart
      REAL (rprec), DIMENSION(3)              :: r_vec
      INTEGER                                 :: u, v
      REAL (rprec)                            :: length
      REAL (rprec), DIMENSION(:,:,:), POINTER :: x_prime
      REAL (rprec), DIMENSION(:,:), POINTER   :: kxuv
      REAL (rprec), DIMENSION(:,:), POINTER   :: kyuv
      REAL (rprec), DIMENSION(:,:), POINTER   :: kzuv
      REAL (rprec), DIMENSION(3)              :: sum_b
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Convert the position to cartesian coordinates. Rotate and shift the signal
!  position with respect to the fixed plasma.
      x_cart = cyl_to_cart(position - (/ 0.0_dp,                               &
     &                                   this%phi_offset,                      &
     &                                   this%z_offset /))

      IF (axi_only) THEN
         CALL assert(ASSOCIATED(this%magnetic_cache%kxuv_axi),                 &
     &               'Cannot get the axisymmtric only ext bfield.')

         x_prime => this%magnetic_cache%x_axi
         kxuv => this%magnetic_cache%kxuv_axi
         kyuv => this%magnetic_cache%kyuv_axi
         kzuv => this%magnetic_cache%kzuv_axi
      ELSE
         CALL assert(ASSOCIATED(this%magnetic_cache%kxuv_full),                &
     &               'Cannot get the ext bfield.')

         x_prime => this%magnetic_cache%x_prime
         kxuv => this%magnetic_cache%kxuv_full
         kyuv => this%magnetic_cache%kyuv_full
         kzuv => this%magnetic_cache%kzuv_full
      END IF

!  The magnetic field produced by a sheet of current is
!
!    B(x) = Int K x (x-x')/|x-x'|^3 da                                       (1)
      sum_b = 0.0
      DO v = 1, SIZE(kxuv,2)
         DO u = 1, SIZE(kxuv,1)
            r_vec = x_cart - x_prime(u,v,:)
            length = SQRT(DOT_PRODUCT(r_vec, r_vec))**3.0

            sum_b(1) = sum_b(1)                                                &
     &               + (kyuv(u,v)*r_vec(3) - kzuv(u,v)*r_vec(2))               &
     &               / length
            sum_b(2) = sum_b(2)                                                &
     &               + (kzuv(u,v)*r_vec(1) - kxuv(u,v)*r_vec(3))               &
     &               / length
            sum_b(3) = sum_b(3)                                                &
     &               + (kxuv(u,v)*r_vec(2) - kyuv(u,v)*r_vec(1))               &
     &               / length
         END DO
      END DO

!  The virtual casing principal acts as if there is a perfectly conducting shell
!  around the last closed flux surface. In this shell, eddy currents induced
!  from the volume currents get canceled out by the shell currents.
!
!    B_v + B_s = 0                                                           (2)
!
!  Thus the shell field is the same magnetude but opposite sign to the volume
!  current.
!
!    B_v = -B_s                                                              (3)
!
!  After the surface integral is completed, negate the result to produce a field
!  that would be the same as a volume integral.
      siesta_get_ext_b_plasma = -mu0/(4.0*pi)*sum_b*isigng                     &
     &                        * this%magnetic_cache%du_full                    &
     &                        * this%magnetic_cache%dv_full

      siesta_get_ext_b_plasma = cart_to_cyl_vec(x_cart,                        &
     &                                          siesta_get_ext_b_plasma)

      CALL profiler_set_stop_time('siesta_get_ext_b_plasma', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get internal plasma magnetic field.
!>
!>  The internal b field is returned in cyclindical coordinates. This function
!>  does not require shifting the plasma relative to the diagnostics. The plasma
!>  has already been shifted when converting to the flux surface position.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @param[in] flux Position to compute the fields at in cylindrical
!>                  coordinates.
!>  @returns The internal field.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_int_b_plasma(this, flux)
      USE read_wout_mod, only: mnmax, xm, xn, rmnc, rmns, zmnc, zmns,          &
     &                         lasym, nsvmec => ns

      USE vmec_utils, ONLY : GetBcyl_WOUT

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(3)              :: siesta_get_int_b_plasma
      TYPE (siesta_class), INTENT(in)         :: this
      REAL (rprec), DIMENSION(3), INTENT(in)  :: flux

!  local variables
      REAL (rprec)                            :: ds
      REAL (rprec)                            :: wlow_v
      REAL (rprec)                            :: whigh_v
      INTEGER                                 :: ilow_v
      INTEGER                                 :: ihigh_v
      REAL (rprec)                            :: wlow_s
      REAL (rprec)                            :: whigh_s
      INTEGER                                 :: ilow_s
      INTEGER                                 :: ihigh_s
      INTEGER                                 :: mpol
      INTEGER                                 :: ntor
      INTEGER                                 :: nfp
      INTEGER                                 :: m
      INTEGER                                 :: n
      REAL (rprec)                            :: r
      REAL (rprec)                            :: rs
      REAL (rprec)                            :: ru
      REAL (rprec)                            :: rv
      REAL (rprec)                            :: zu
      REAL (rprec)                            :: zs
      REAL (rprec)                            :: zv
      REAL (rprec)                            :: bs
      REAL (rprec)                            :: bu
      REAL (rprec)                            :: bv
      REAL (rprec), DIMENSION(:), ALLOCATABLE :: vmecint
      REAL (rprec), DIMENSION(:), ALLOCATABLE :: cosmn_vmec
      REAL (rprec), DIMENSION(:), ALLOCATABLE :: sinmn_vmec
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ds = 1.0/(this%context%ns - 1.0)

!  siesta intepolation parameters.
      IF (flux(1) .lt. ds/2.0) THEN
         ilow_s = 2
         ihigh_s = 3
         whigh_s = flux(1) - ds/2.0
         wlow_s = 1.0 - whigh_s
      ELSE IF (flux(1) .gt. 1.0 - ds/2.0) THEN
         ihigh_s = this%context%ns
         ilow_s = ihigh_s - 1
         wlow_s = -flux(1)/ds - 0.5 + 1.0/ds
         whigh_s = 1.0 - wlow_s
      ELSE
         ilow_s = FLOOR(flux(1)/ds + 1.5)
         ihigh_s = ilow_s + 1
         whigh_s = flux(1)/ds + 1.5 - ilow_s
         wlow_s = 1.0 - whigh_s
      END IF

      ilow_v = siesta_to_i_vmec_low(flux(1))
      ihigh_v = ilow_v + 1
      wlow_v = siesta_to_w_vmec_low(flux(1), ilow_v)
      whigh_v = 1.0 - wlow_v

      ALLOCATE(vmecint(mnmax))
      ALLOCATE(cosmn_vmec(mnmax))
      ALLOCATE(sinmn_vmec(mnmax))

      cosmn_vmec = COS(xm*flux(2) - xn*flux(3))
      sinmn_vmec = SIN(xm*flux(2) - xn*flux(3))

      vmecint = wlow_v*rmnc(:,ilow_v) + whigh_v*rmnc(:,ihigh_v)
      r = SUM(vmecint*cosmn_vmec)
      ru = -SUM(xm*vmecint*sinmn_vmec)
      rv =  SUM(xn*vmecint*sinmn_vmec)

      vmecint = 2.0*flux(1)*(rmnc(:,ihigh_v) - rmnc(:,ilow_v))                 &
     &        * (nsvmec - 1.0)
      rs = SUM(vmecint*cosmn_vmec)

      vmecint = wlow_v*zmns(:,ilow_v) + whigh_v*zmns(:,ihigh_v)
      zu =  SUM(xm*vmecint*cosmn_vmec)
      zv = -SUM(xn*vmecint*cosmn_vmec)

      vmecint = 2.0*flux(1)*(zmns(:,ihigh_v) - zmns(:,ilow_v))                 &
     &        * (nsvmec - 1.0)
      zs = SUM(vmecint*sinmn_vmec)

      mpol = this%context%mpol
      ntor = this%context%ntor
      nfp = this%context%nfp

      bs = 0.0
      bu = 0.0
      bv = 0.0

      DO n = -ntor, ntor
         DO m = 0, mpol
            bs = bs +                                                          &
     &         (wlow_s*this%context%bsupsmnsh(m,n,ilow_s) +                    &
     &          whigh_s*this%context%bsupsmnsh(m,n,ihigh_s))*                  &
     &         SIN(m*flux(2) + nfp*n*flux(3))

            bu = bu +                                                          &
     &         (wlow_s*this%context%bsupumnch(m,n,ilow_s) +                    &
     &          whigh_s*this%context%bsupumnch(m,n,ihigh_s))*                  &
     &         COS(m*flux(2) + nfp*n*flux(3))

            bv = bv +                                                          &
     &         (wlow_s*this%context%bsupvmnch(m,n,ilow_s) +                    &
     &          whigh_s*this%context%bsupvmnch(m,n,ihigh_s))*                  &
     &         COS(m*flux(2) + nfp*n*flux(3))
         END DO
      END DO

      IF (this%context%l_asym) THEN

         IF (lasym) THEN
            vmecint = wlow_v*rmns(:,ilow_v) + whigh_v*rmns(:,ihigh_v)
            r = r + SUM(vmecint*sinmn_vmec)
            ru = ru + SUM(xm*vmecint*cosmn_vmec)
            rv = rv - SUM(xn*vmecint*cosmn_vmec)

            vmecint = 2.0*flux(1)*(rmns(:,ihigh_v) - rmns(:,ilow_v))           &
     &              * (nsvmec - 1.0)
            rs = rs + SUM(vmecint*sinmn_vmec)

            vmecint = wlow_v*zmnc(:,ilow_v) + whigh_v*zmnc(:,ihigh_v)
            zu = zu - SUM(xm*vmecint*sinmn_vmec)
            zv = zv + SUM(xn*vmecint*sinmn_vmec)

            vmecint = 2.0*flux(1)*(zmnc(:,ihigh_v) - zmnc(:,ilow_v))           &
     &              * (nsvmec - 1.0)
            zs = zs + SUM(vmecint*cosmn_vmec)
         END IF

         DO n = -ntor, ntor
            DO m = 0, mpol
               bs = bs +                                                       &
     &              (wlow_s*this%context%bsupsmnch(m,n,ilow_s) +               &
     &               whigh_s*this%context%bsupsmnch(m,n,ihigh_s))*             &
     &              COS(m*flux(2) + nfp*n*flux(3))

               bu = bu +                                                       &
     &              (wlow_s*this%context%bsupumnsh(m,n,ilow_s) +               &
     &               whigh_s*this%context%bsupumnsh(m,n,ihigh_s))*             &
     &              SIN(m*flux(2) + nfp*n*flux(3))

               bv = bv +                                                       &
     &              (wlow_s*this%context%bsupvmnsh(m,n,ilow_s) +               &
     &               whigh_s*this%context%bsupvmnsh(m,n,ihigh_s))*             &
     &              SIN(m*flux(2) + nfp*n*flux(3))
            END DO
         END DO
      END IF

      DEALLOCATE(vmecint)
      DEALLOCATE(cosmn_vmec)
      DEALLOCATE(sinmn_vmec)

      siesta_get_int_b_plasma(1) = bs*rs + bu*ru + bv*rv
      siesta_get_int_b_plasma(2) = bv*r
      siesta_get_int_b_plasma(3) = bs*zs + bu*zu + bv*zv

      siesta_get_int_b_plasma = siesta_get_int_b_plasma                        &
     &                        / this%context%b_factor

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get radial grid size.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_grid_size.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns Size of the radial grid.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_grid_size(this)
      USE siesta_namelist, ONLY: nsin, nsin_ext

      IMPLICIT NONE

!  Declare Arguments
      INTEGER                         :: siesta_get_grid_size
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (ASSOCIATED(this%context)) THEN
         siesta_get_grid_size = this%context%ns
      ELSE
         siesta_get_grid_size = nsin + nsin_ext
      END IF

      CALL profiler_set_stop_time('siesta_get_grid_size', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get start of the radial grid.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_grid_start.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns Start of the radial grid.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_grid_start(this)

      IMPLICIT NONE

!  Declare Arguments
      REAl (rprec)                    :: siesta_get_grid_start
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      siesta_get_grid_start = 0.0
      CALL profiler_set_stop_time('siesta_get_grid_start', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get radial grid size.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_grid_size.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns Size of the radial grid.
!-------------------------------------------------------------------------------
      FUNCTION siesta_get_grid_stop(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: siesta_get_grid_stop
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      siesta_get_grid_stop = 1.0
      CALL profiler_set_stop_time('siesta_get_grid_stop', start_time)

      END FUNCTION

!*******************************************************************************
!  QUERY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Checks if a parameter id is a scaler value.
!>
!>  This method overrides @ref equilibrium::equilibrium_is_scaler_value.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns True if the parameter is a scaler and false if otherwise.
!-------------------------------------------------------------------------------
      FUNCTION siesta_is_scaler_value(this, id)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL                         :: siesta_is_scaler_value
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: id

!  local variables
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      SELECT CASE (id)

         CASE (siesta_phi_offset_id, siesta_z_offset_id)
            siesta_is_scaler_value = .true.

         CASE DEFAULT
            siesta_is_scaler_value = vmec_is_scaler_value(this%vmec, id)

      END SELECT

      CALL profiler_set_stop_time('siesta_is_scaler_value', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Checks if a parameter id is a 1d array.
!>
!>  This method overrides @ref equilibrium::equilibrium_is_1d_array.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns True if the parameter is a 1d array and false if otherwise.
!-------------------------------------------------------------------------------
      FUNCTION siesta_is_1d_array(this, id)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL                         :: siesta_is_1d_array
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: id

!  local variables
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      SELECT CASE (id)

         CASE (siesta_pp_ne_b_id, siesta_pp_ne_as_id,                          &
     &         siesta_pp_ne_af_id, siesta_pp_te_b_id,                          &
     &         siesta_pp_te_as_id, siesta_pp_te_af_id,                         &
     &         siesta_pp_ti_b_id, siesta_pp_ti_as_id,                          &
     &         siesta_pp_ti_af_id, siesta_helpert_id)
            siesta_is_1d_array = .true.

         CASE DEFAULT
            siesta_is_1d_array = vmec_is_1d_array(this%vmec, id)

      END SELECT

      CALL profiler_set_stop_time('siesta_is_1d_array', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Checks if a parameter id is a 2d array.
!>
!>  This method overrides @ref equilibrium::equilibrium_is_2d_array.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns True if the parameter is a 2d array and false if otherwise.
!-------------------------------------------------------------------------------
      FUNCTION siesta_is_2d_array(this, id)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL                         :: siesta_is_2d_array
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: id

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      SELECT CASE (id)

         CASE (siesta_pp_sxrem_b_id, siesta_pp_sxrem_as_id,                    &
     &         siesta_pp_sxrem_af_id)
            siesta_is_2d_array = .true.

         CASE DEFAULT
            siesta_is_2d_array = vmec_is_2d_array(this%vmec, id)

      END SELECT

      CALL profiler_set_stop_time('siesta_is_2d_array', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Checks if a parameter id is a reconstruction parameter.
!>
!>  This method overrides @ref equilibrium::equilibrium_is_recon_param.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns True if the parameter is a reconstruction parameter and false if
!>  otherwise.
!-------------------------------------------------------------------------------
      FUNCTION siesta_is_recon_param(this, id)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL                         :: siesta_is_recon_param
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: id

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      SELECT CASE (id)

         CASE (siesta_pp_ne_b_id, siesta_pp_ne_as_id,                          &
     &         siesta_pp_ne_af_id, siesta_pp_te_b_id,                          &
     &         siesta_pp_te_as_id, siesta_pp_te_af_id,                         &
     &         siesta_pp_ti_b_id, siesta_pp_ti_as_id,                          &
     &         siesta_pp_ti_af_id, siesta_pp_sxrem_b_id,                       &
     &         siesta_pp_sxrem_as_id, siesta_pp_sxrem_af_id,                   &
     &         siesta_phi_offset_id, siesta_z_offset_id,                       &
     &         siesta_helpert_id)
            siesta_is_recon_param = .true.

         CASE DEFAULT
            siesta_is_recon_param =                                            &
     &         vmec_is_recon_param(this%vmec, id)

      END SELECT

      CALL profiler_set_stop_time('siesta_is_recon_param', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Checks if a the point magnetics are being used.
!>
!>  This method overrides @ref equilibrium::equilibrium_is_using_point.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @returns True if the point magnetic are being used.
!-------------------------------------------------------------------------------
      FUNCTION siesta_is_using_point(this)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL                         :: siesta_is_using_point
      TYPE (siesta_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Some compilers will generate code that will early terminate when a first
!  operand to a .and. operator evaluates to false. Others are not so clever.
!  Avoid running the second ASSOCIATED check if the magnetic cache is null.
      siesta_is_using_point = ASSOCIATED(this%magnetic_cache)
      IF (siesta_is_using_point) THEN
         siesta_is_using_point =                                               &
     &      ASSOCIATED(this%magnetic_cache%kxuv_full)
      END IF

      CALL profiler_set_stop_time('vmec_is_using_point', start_time)

      END FUNCTION

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Solves the siesta equilibrium.
!>
!>  This method overrides @ref equilibrium::equilibrium_converge. Solves the
!>  SIESTA equilibrium and loads the resulting wout file in preparation of
!>  computing modeled signals. If the vmec equilibrium needs updating, it solves
!>  that in addition.
!>
!>  @param[inout] this        A @ref siesta_class instance.
!>  @param[inout] num_iter    Counter to track the number of iterations.
!>  @param[in]    iou         Input/Output unit of the runlog file.
!>  @param[in]    eq_comm     MPI communicator pool for siesta/vmec.
!>  @param[in]    state_flags Bitwise flags to indicate which parts of the model
!>                            changed.
!>  @returns True if the convergece was sucessful and false otherwise.
!-------------------------------------------------------------------------------
      FUNCTION siesta_converge(this, num_iter, iou, eq_comm,                   &
     &                         state_flags)
      USE model_state
      USE data_parameters
      USE safe_open_mod
      USE siesta_error
      USE siesta_namelist

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL                            :: siesta_converge
      TYPE (siesta_class), INTENT(inout) :: this
      INTEGER, INTENT(inout)             :: num_iter
      INTEGER, INTENT(in)                :: iou
      INTEGER, INTENT(in)                :: eq_comm
      INTEGER, INTENT(in)                :: state_flags

!  local variables
      INTEGER                            :: eq_size
      INTEGER                            :: eq_rank
      INTEGER                            :: status
      INTEGER                            :: child_comm
      TYPE (siesta_run_class), POINTER   :: run_context
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      siesta_converge = .true.
      IF (BTEST(state_flags, model_state_vmec_flag)) THEN
         siesta_converge = vmec_converge(this%vmec, num_iter, iou,             &
     &                                   eq_comm)
      END IF

!  FIXME: This is currently a hack. Eventuially siesta will be updated to have
!         a run siesta routine like vmec. For now run vmec as an external
!         command.

      eq_size = 1
      eq_rank = 0
#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(eq_comm, eq_rank, status)
      CALL MPI_COMM_SIZE(eq_comm, eq_size, status)
#endif
      IF (eq_rank .eq. 0) THEN
         ladd_pert = .true.
         lresistive = .false.
         lrestart = .false.

         CALL siesta_namelist_write(TRIM(this%siesta_file_name))
      END IF

#if defined(MPI_OPT)
      CALL MPI_BARRIER(eq_comm, status)
#endif
      run_context => siesta_run_class(eq_comm, .false., .false.,               &
     &                                .false., this%siesta_file_name)
      CALL run_context%set_vmec
      CALL run_context%converge
      DEALLOCATE(run_context)

      siesta_converge = siesta_error_state .eq. siesta_error_no_error

      status = 0
      siesta_converge = siesta_converge .and. status .eq. 0

      IF (siesta_converge .and. eq_rank .eq. 0) THEN
         IF (ASSOCIATED(this%context)) THEN
            CALL siesta_context_read(this%context,                             &
     &                                  this%restart_file_name)
         ELSE
            this%context =>                                                    &
     &         siesta_context_construct(this%restart_file_name)
         END IF

         IF (ASSOCIATED(this%magnetic_cache)) THEN
            CALL siesta_set_magnetic_cache(this)
         END IF
      END IF

      CALL profiler_set_stop_time('siesta_converge', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Loads the vacuum magnetic field file.
!>
!>  Loads the vacuum magnetic field file. This will get on multiple processes to
!>  allow parallel loading of the mgrid file. The extcur array will need to be
!>  broadcast to the child processes.
!>
!>  @param[in] this    A @ref siesta_class instance.
!>  @param[in] index   Index of the changed current.
!>  @param[in] eq_comm MPI communicator pool for siesta.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_read_vac_file(this, index, eq_comm)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: index
      INTEGER, INTENT(in)             :: eq_comm

!  Start of executable code
      CALL vmec_read_vac_file(this%vmec, index, eq_comm)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Save the internal state of the equilibrium.
!>
!>  Saves the siesta state. Also saves the vmec state.
!>
!>  @param[inout] this A @ref siesta_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_save_state(this)
      USE file_opts

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(inout) :: this

!  local variables
      INTEGER                            :: error
      INTEGER                            :: i
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL vmec_save_state(this%vmec)

!  Cache the restart file by appending _cache to a copy. Use copy here to keep
!  the orginal file intact.
      CALL copy_file(TRIM(this%restart_file_name),                             &
     &               TRIM(this%restart_file_name) // '_cache',                 &
     &               error)
      CALL assert_eq(error, 0, 'Error copying restart file.')

      IF (ASSOCIATED(this%ne)) THEN
         CALL pprofile_save_state(this%ne)
      END IF
      IF (ASSOCIATED(this%te)) THEN
         CALL pprofile_save_state(this%te)
      END IF
      IF (ASSOCIATED(this%ti)) THEN
         CALL pprofile_save_state(this%ti)
      END IF
      IF (ASSOCIATED(this%sxrem)) THEN
         DO i = 1, SIZE(this%sxrem)
            IF (ASSOCIATED(this%sxrem(i)%p)) THEN
               CALL pprofile_save_state(this%sxrem(i)%p)
            END IF
         END DO
      END IF

      CALL profiler_set_stop_time('siesta_save_state', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Reset the internal state of the equilibrium.
!>
!>  Copies the saved siesta state back to siesta. Also resets the vmec state.
!>
!>  @param[inout] this A @ref siesta_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_reset_state(this)
      USE file_opts

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(inout) :: this

!  local variables
      INTEGER                            :: error
      INTEGER                            :: i
      REAL (rprec)                       :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL vmec_reset_state(this%vmec)

!  Reset the restart file.
      CALL copy_file(TRIM(this%restart_file_name) // '_cache',                 &
     &               TRIM(this%restart_file_name), error)
      CALL assert_eq(error, 0, 'Error moving wout file.')

      CALL siesta_context_read(this%context, this%restart_file_name)

      IF (ASSOCIATED(this%ne)) THEN
         CALL pprofile_reset_state(this%ne)
      END IF
      IF (ASSOCIATED(this%te)) THEN
         CALL pprofile_reset_state(this%te)
      END IF
      IF (ASSOCIATED(this%ti)) THEN
         CALL pprofile_reset_state(this%ti)
      END IF
      IF (ASSOCIATED(this%sxrem)) THEN
         DO i = 1, SIZE(this%sxrem)
            IF (ASSOCIATED(this%sxrem(i)%p)) THEN
               CALL pprofile_reset_state(this%sxrem(i)%p)
            END IF
         END DO
      END IF

      CALL profiler_set_stop_time('siesta_reset_state', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write out the equilibrium to an output file.
!>
!>  This method overrides @ref equilibrium::equilibrium_write.
!>
!>  @param[in] this A @ref siesta_class instance.
!>  @param[in] iou  Input/output unit of the output file.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_write(this, iou)
      USE safe_open_mod
      USE siesta_context
      USE file_opts
      USE siesta_namelist

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: iou

!  local variables
      INTEGER                       :: i
      INTEGER                       :: status
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()
      WRITE (iou,*)
      WRITE (iou,*) 'Equilibrium Type : SIESTA'
      IF (ASSOCIATED(this%ne)) THEN
         CALL pprofile_write(this%ne, 'pp_ne', iou)
      END IF

      IF (ASSOCIATED(this%sxrem)) THEN
         DO i = 1, SIZE(this%sxrem)
            IF (ASSOCIATED(this%sxrem(i)%p)) THEN
               CALL pprofile_write(this%sxrem(i)%p, 'pp_sxrem', iou)
            END IF
         END DO
      END IF

      IF (ASSOCIATED(this%te)) THEN
         CALL pprofile_write(this%te, 'pp_te', iou)
      END IF

      IF (ASSOCIATED(this%ti)) THEN
         CALL pprofile_write(this%ti, 'pp_ti', iou)
      END IF

!  Update the namelist input variables from the equilibrium solution.
      CALL siesta_set_namelist(this)

      CALL siesta_namelist_write(TRIM(this%siesta_file_name) // '_out')

      CALL move_file(TRIM(this%siesta_file_name) // '_save',                   &
     &               TRIM(this%siesta_file_name), status)

      CALL vmec_write(this%vmec, iou)

      CALL profiler_set_stop_time('siesta_write', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write the current valid input.
!>
!>  This method overrides @ref equilibrium::equilibrium_write_input. The
!>  boundary and other fixed parameters do not get updated as the reconstruction
!>  progresses. Need to update them manually if in free boundary mode.
!>
!>  @param[in] this         A @ref siesta_class instance.
!>  @param[in] current_step Step number to append to input filename.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_write_input(this, current_step)
      USE safe_open_mod
      USE file_opts
      USE siesta_context
      USE siesta_namelist

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(in)      :: this
      INTEGER, INTENT(in)                  :: current_step

!  local variables
      INTEGER                              :: iou_nl
      INTEGER                              :: status
      CHARACTER (len=path_length)          :: filename
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Update the namelist input variables from the equilibrium solution.
      CALL siesta_set_namelist(this)

!  Write out the final namelist input file.
      iou_nl = 0
      status = 0

      WRITE (filename,1000) TRIM(this%siesta_file_name), current_step

      CALL siesta_namelist_write(TRIM(filename))
      CALL assert_eq(status, 0, 'Error saving input file.')

!  Save the restart file by making a copy with a new name.
      WRITE (filename,1000) TRIM(this%restart_file_name), current_step

      CALL copy_file(this%restart_file_name, TRIM(filename), status)
      CALL assert_eq(status, 0, 'Error copying wout file.')

      CALL vmec_write_input(this%vmec, current_step)

      CALL profiler_set_stop_time('vmec_write_input', start_time)

1000  FORMAT(a,'_',i0.3)

      END SUBROUTINE

!*******************************************************************************
!  NETCDF SUBROUTINES
!*******************************************************************************
!>  @page result_file_siesta SIESTA Result File
!>
!>  @tableofcontents
!>  @section result_file_siesta_intro_sec Introduction
!>  This page documents the contents of a result NetCDF file contributed by the
!>  SIESTA equilibrium. The remaining parts of the result file are documented in
!>  the @ref result_file_main page.
!>
!>  @section result_file_siesta_dim_sec Dimensions
!>  @header{Dimension, Description, Code Reference}
!>  @begin_table
!>  @end_table
!>
!>  @section result_file_siesta_var_sec Variables
!>  @header{Variable(Dimensions), Description, Code Reference}
!>  @begin_table
!>  @end_table
!-------------------------------------------------------------------------------
!>  @brief Define NetCDF variables for the result file
!>
!>  This method overrides @ref equilibrium::equilibrium_def_result. Defines
!>  dimensions and variables for the SIESTA contribution of the result file.
!>  Multidimensional arrays need to be transposed so arrays appear in the
!>  correct order in non fortran languages.
!>
!>  @param[in] this             A @ref siesta_class instance.
!>  @param[in] result_ncid      NetCDF file id of the result file.
!>  @param[in] maxnsetps_dim_id NetCDF dimension id of the number of steps
!>                              dimension.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_def_result(this, result_ncid, maxnsetps_dim_id)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: result_ncid
      INTEGER, INTENT(in)             :: maxnsetps_dim_id

!  Local variables
      REAL (rprec)                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL vmec_def_result(this%vmec, result_ncid, maxnsetps_dim_id)

      CALL profiler_set_stop_time('siesta_def_result', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write inital data to NetCDF result file
!>
!>  This method overrides @ref equilibrium::equilibrium_write_init_data.
!>
!>  @param[in] this        A @ref siesta_class instance.
!>  @param[in] result_ncid NetCDF file id of the result file.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_write_init_data(this, result_ncid)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: result_ncid

!  Local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  vmec_write_init_data will write step data for the first step. Call this
!  before the call to siesta_write_init_data to avoid writing the step twice.
      CALL vmec_write_init_data(this%vmec, result_ncid)
      CALL siesta_write_step_data(this, result_ncid, 1)

      CALL profiler_set_stop_time('siesta_write_init_data', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write step data to NetCDF result file
!>
!>  This method overrides @ref equilibrium::equilibrium_write_step_data.
!>
!>  @param[in] this         A @ref siesta_class instance.
!>  @param[in] result_ncid  NetCDF file id of the result file.
!>  @param[in] current_step Step index to write variables to.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_write_step_data(this, result_ncid, current_step)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: result_ncid
      INTEGER, INTENT(in)             :: current_step

!  Local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  vmec_write_init_data called from siesta_write_init_data already write the
!  first vmec step.
      IF (current_step .ne. 1) THEN
         CALL vmec_write_step_data(this%vmec, result_ncid, current_step)
      END IF

      CALL profiler_set_stop_time('siesta_write_step_data', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Restart from a result file.
!>
!>  This method overrides @ref equilibrium::equilibrium_restart.
!>
!>  @param[in] this         A @ref siesta_class instance.
!>  @param[in] result_ncid  NetCDF file id of the result file.
!>  @param[in] current_step Step index to write variables to.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_restart(this, result_ncid, current_step)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(in) :: this
      INTEGER, INTENT(in)             :: result_ncid
      INTEGER, INTENT(in)             :: current_step

!  Local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL vmec_restart(this%vmec, result_ncid, current_step)

      CALL profiler_set_stop_time('siesta_write_step_data', start_time)

      END SUBROUTINE

!*******************************************************************************
!  MPI SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Syncronize the siesta state to children.
!>
!>  Syncs data between the parent and child processes. If MPI support is not
!>  compiled in this subroutine reduces to a no op.
!>
!>  @param[inout] this       A @ref siesta_class instance.
!>  @param[in]    recon_comm MPI communicator for the reconstruction processes.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_sync_state(this, recon_comm)
      USE file_opts

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                :: recon_comm

#if defined(MPI_OPT)
!  local variables
      INTEGER                            :: i
      INTEGER                            :: error
      INTEGER                            :: mpi_rank

!  Start of executable code
      CALL MPI_COMM_RANK(recon_comm, mpi_rank, error)

!  The barrier ensures that the wout is ready to be copied from the parent
!  directory. Otherwise a child process can try to copy a wout file that does
!  not exist.
      CALL MPI_BARRIER(recon_comm, error)

!  Copy the vmec state first to ensure the wout file is loaded before seting the
!  siesta magnetic cache.
      CALL vmec_sync_state(this%vmec, recon_comm)

!  If this is the child process, load the wout file.
      IF (mpi_rank .gt. 0) THEN
         CALL copy_file(build_path('..', this%restart_file_name),              &
     &                  TRIM(this%restart_file_name), error)

         IF (ASSOCIATED(this%context)) THEN
            CALL siesta_context_read(this%context,                             &
     &                               this%restart_file_name)
         ELSE
            this%context =>                                                    &
     &         siesta_context_construct(this%restart_file_name)
         END IF

         IF (ASSOCIATED(this%magnetic_cache)) THEN
            CALL siesta_set_magnetic_cache(this)
         END IF
      END IF
#endif
      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Syncronize a child siesta state to the parent.
!>
!>  Syncs data between a child and the parent process. If MPI support is not
!>  compiled in this subroutine reduces to a no op.
!>
!>  @param[inout] this       A @ref siesta_class instance.
!>  @param[in]    index      Reconstruction rank to sync.
!>  @param[in]    recon_comm MPI communicator for the reconstruction processes.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_sync_child(this, index, recon_comm)
      USE file_opts

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                :: index
      INTEGER, INTENT(in)                :: recon_comm

#if defined(MPI_OPT)
!  local variables
      INTEGER                            :: error
      INTEGER                            :: mpi_rank

!  Start of executable code
      CALL MPI_COMM_RANK(recon_comm, mpi_rank, error)

!  Copy the vmec state first to ensure the wout file is loaded before seting the
!  siesta magnetic cache.
      CALL vmec_sync_child(this%vmec, index, recon_comm)

!  If this is the parent process, load the wout file.
      IF (mpi_rank .eq. 0) THEN
         CALL copy_file(build_path(process_dir(index + 1),                     &
     &                             this%restart_file_name),                    &
     &                  TRIM(this%restart_file_name), error)
         CALL assert_eq(error, 0, 'Error reading synced restart file.')

         CALL siesta_context_read(this%context, this%restart_file_name)

         IF (ASSOCIATED(this%magnetic_cache)) THEN
            CALL siesta_set_magnetic_cache(this)
         END IF
      END IF

#endif
      END SUBROUTINE

!*******************************************************************************
!  PRIVATE
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Map the siesta s to the vmec s.
!>
!>  The vmec s_v is function of the siesta s_s using the forumla,
!>
!>    s_v(s_s) = s_s^2                                                       (1)
!>
!>  @param[in] s A position on the siesta radial grid.
!>  @returns The vmec s for a given siesta s.
!-------------------------------------------------------------------------------
      PURE FUNCTION siesta_to_vmec_s(s)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)             :: siesta_to_vmec_s
      REAL (rprec), INTENT(in) :: s

!  Start of executable code
      siesta_to_vmec_s = s*s

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Map the vmec s to the siesta s.
!>
!>  The siesta s_v is function of the vmec s_s using the forumla,
!>
!>    s_s(s_v) = Sqrt(s_v)                                                   (1)
!>
!>  @param[in] s A position on the siesta radial grid.
!>  @returns The siesta s for a given vmec s.
!-------------------------------------------------------------------------------
      PURE FUNCTION siesta_to_siesta_s(s)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)             :: siesta_to_siesta_s
      REAL (rprec), INTENT(in) :: s

!  Start of executable code
      siesta_to_siesta_s = SQRT(s)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Find the vmec s index.
!>
!>  Given a siesta s_s, find the index corresponding to the vmec grid. This is
!>  the non integer position coresponding to the absolute i position. The
!>  normalized vmec toroidal flux s_v corresponding to a vmec index i is.
!>
!>     s_v = (i - 1)*ds_v                                                    (1)
!>
!>  where radial grid spacing ds_v is
!>
!>     ds_v = 1/(ns - 1)                                                     (2)
!>
!>  Inverting equation (1), provides the index for a given s.
!>
!>     i = s_v/ds_v + 1                                                      (3)
!>
!>  @param[in] s A position on the siesta radial grid.
!>  @returns Index of the closest radial grid point in vmec space.
!-------------------------------------------------------------------------------
      PURE FUNCTION siesta_to_i_vmec(s)
      USE read_wout_mod, only: ns

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)             :: siesta_to_i_vmec
      REAL (rprec), INTENT(in) :: s

!  Start of executable code
      siesta_to_i_vmec = siesta_to_vmec_s(s)*(ns - 1.0) + 1.0

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Find the lower vmec s index.
!>
!>  Given a siesta s_s, round down to the nearist integer after mapping the
!>  siesta radial position to the absolute vmec index.
!>
!>  @param[in] s A position on the siesta radial grid.
!>  @returns Index of the closest radial grid point in vmec space.
!-------------------------------------------------------------------------------
      PURE FUNCTION siesta_to_i_vmec_low(s)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER                  :: siesta_to_i_vmec_low
      REAL (rprec), INTENT(in) :: s

!  Start of executable code
      siesta_to_i_vmec_low = FLOOR(siesta_to_i_vmec(s))

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Find the lower interplation weighting.
!>
!>  Given a siesta s_s and a the lower vmec index i, find how much the lower
!>  index is biased. When s_s corresponds to the lower index, it should be fully
!>  weighed.
!>
!>    1 = m*i_low + b                                                        (1)
!>
!>  When s_s corresponds to the lower index + 1, it should be weighted to zero.
!>
!>    0 = m*(i_low + 1) + b                                                  (2)
!>
!>  Solving this system of equations, maps the the s_s to the weight.
!>
!>    w_low = m*i + b                                                        (3)
!>
!>  The slope m and y intercept b are
!>
!>    m = -1                                                                 (4)
!>
!>    b = 1 + i_low                                                          (5)
!>
!>  @param[in] s    A position on the siesta radial grid.
!>  @param[in] ilow Nearest lower index.
!>  @returns Weight of the lower vmec index.
!-------------------------------------------------------------------------------
      PURE FUNCTION siesta_to_w_vmec_low(s, ilow)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)             :: siesta_to_w_vmec_low
      REAL (rprec), INTENT(in) :: s
      INTEGER, INTENT(in)      :: ilow

!  Start of executable code
      siesta_to_w_vmec_low = -siesta_to_i_vmec(s) + 1.0 + ilow

      END FUNCTION

      END MODULE

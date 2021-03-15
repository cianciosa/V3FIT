!*******************************************************************************
!>  @file equilibrium.f
!>  @brief Contains module @ref equilibrium
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref equilibrium_class. Equilibrium is
!>  an abstract interface to the equilibrium solver. Every method is meant to be
!>  overwritten by a subclass method with a few exceptions. For instance a
!>  vacuum equilibrium could use the default return values for
!>  @ref equilibrium_get_ne, equilibrium_get_te and equilibrium_get_sxrem
!>  methods.
!>  @par Sub Classes:
!>  @ref vmec_equilibrium
!>  @ref vacuum_equilibrium
!*******************************************************************************

      MODULE equilibrium
      USE vacuum_equilibrium
      USE siesta_equilibrium

      IMPLICIT NONE

!*******************************************************************************
!  equilibrium module parameters
!*******************************************************************************
!>  Type descriptor for the @ref equilibrium_class no subclass.
      INTEGER, PARAMETER :: equilibrium_no_type     = -1
!>  Type descriptor for the @ref equilibrium_class subclass
!>  @ref vmec_equilibrium.
      INTEGER, PARAMETER :: equilibrium_vmec_type   = 0
!>  Type descriptor for the @ref equilibrium_class subclass
!>  @ref vacuum_equilibrium.
      INTEGER, PARAMETER :: equilibrium_vacuum_type = 1
!>  Type descriptor for the @ref equilibrium_class subclass
!>  @ref siesta_equilibrium.
      INTEGER, PARAMETER :: equilibrium_siesta_type = 2

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) equilibrium base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a equilibrium.
!>  @par Sub Classes:
!>  @ref vmec_equilibrium
!-------------------------------------------------------------------------------
      TYPE equilibrium_class
!>  Type descirptor of the equilibrium subclass type.
!>  @par Possible values are:
!>  * @ref equilibrium_no_type
!>  * @ref equilibrium_vmec_type
         INTEGER                      :: type = equilibrium_no_type
!>  Controls if an equilibrium is forced to be resolved or not.
         LOGICAL                      :: force_solve
!>  An instance of a @ref vmec_equilibrium object.
         TYPE (vmec_class), POINTER   :: vmec => null()
!>  An instance of a @ref vacuum_equilibrium object.
         TYPE (vacuum_class), POINTER :: vacuum => null()
!>  An instance of a @ref vacuum_equilibrium object.
         TYPE (siesta_class), POINTER :: siesta => null()
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the construction of @ref equilibrium_class types using
!>  @ref equilibrium_construct_vmec, @ref equilibrium_construct_vacuum or
!>  @ref equilibrium_construct_siesta
!-------------------------------------------------------------------------------
      INTERFACE equilibrium_construct
         MODULE PROCEDURE equilibrium_construct_vmec,                          &
     &                    equilibrium_construct_vacuum,                        &
     &                    equilibrium_construct_siesta
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the equilibrium set magnetic cache
!-------------------------------------------------------------------------------
      INTERFACE equilibrium_set_magnetic_cache
         MODULE PROCEDURE equilibrium_set_magnetic_cache_response,             &
     &                    equilibrium_set_magnetic_cache_point
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the equilibrium density profile values.
!-------------------------------------------------------------------------------
      INTERFACE equilibrium_get_ne
         MODULE PROCEDURE equilibrium_get_ne_cart,                             &
     &                    equilibrium_get_ne_radial
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the equilibrium guassian process density profile values.
!-------------------------------------------------------------------------------
      INTERFACE equilibrium_get_gp_ne
         MODULE PROCEDURE equilibrium_get_gp_ne_ij,                            &
     &                    equilibrium_get_gp_ne_pi,                            &
     &                    equilibrium_get_gp_ne_pp
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the equilibrium electron temperature profile values.
!-------------------------------------------------------------------------------
      INTERFACE equilibrium_get_te
         MODULE PROCEDURE equilibrium_get_te_cart,                             &
     &                    equilibrium_get_te_radial
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the equilibrium guassian process electron temperature profile
!>  values.
!-------------------------------------------------------------------------------
      INTERFACE equilibrium_get_gp_te
         MODULE PROCEDURE equilibrium_get_gp_te_ij,                            &
     &                    equilibrium_get_gp_te_pi,                            &
     &                    equilibrium_get_gp_te_pp
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the equilibrium ion temperature profile values.
!-------------------------------------------------------------------------------
      INTERFACE equilibrium_get_ti
         MODULE PROCEDURE equilibrium_get_ti_cart,                             &
     &                    equilibrium_get_ti_radial
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the equilibrium guassian process ion temperature profile
!>  values.
!-------------------------------------------------------------------------------
      INTERFACE equilibrium_get_gp_ti
         MODULE PROCEDURE equilibrium_get_gp_ti_ij,                            &
     &                    equilibrium_get_gp_ti_pi,                            &
     &                    equilibrium_get_gp_ti_pp
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the effective charge profile values.
!-------------------------------------------------------------------------------
      INTERFACE equilibrium_get_ze
         MODULE PROCEDURE equilibrium_get_ze_cart,                             &
     &                    equilibrium_get_ze_radial
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the equilibrium soft x-ray emissivity profile values.
!-------------------------------------------------------------------------------
      INTERFACE equilibrium_get_sxrem
         MODULE PROCEDURE equilibrium_get_sxrem_cart,                          &
     &                    equilibrium_get_sxrem_radial
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the equilibrium guassian process soft x-ray emissivity profile
!>  values.
!-------------------------------------------------------------------------------
      INTERFACE equilibrium_get_gp_sxrem
         MODULE PROCEDURE equilibrium_get_gp_sxrem_ij,                         &
     &                    equilibrium_get_gp_sxrem_pi,                         &
     &                    equilibrium_get_gp_sxrem_pp
      END INTERFACE

!-------------------------------------------------------------------------------
!>  Interface for the equilibrium pressure profile values.
!-------------------------------------------------------------------------------
      INTERFACE equilibrium_get_p
         MODULE PROCEDURE equilibrium_get_p_cart,                              &
     &                    equilibrium_get_p_radial
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref equilibrium_class containing a @ref vmec_equilibrium
!>  object.
!>
!>  Allocates memory and initializes a @ref equilibrium_class object.
!>
!>  @param[in] vmec_object An instance of a @ref vmec_equilibrium subclass.
!>  @param[in] force_solve If true, forces the equilbirum to resolve every time.
!>  @returns A pointer to a constructed @ref equilibrium_class object.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_construct_vmec(vmec_object, force_solve)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), POINTER :: equilibrium_construct_vmec
      TYPE (vmec_class), POINTER        :: vmec_object
      LOGICAL, INTENT(in)               :: force_solve

!  local variables
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(equilibrium_construct_vmec)

      equilibrium_construct_vmec%type = equilibrium_vmec_type
      equilibrium_construct_vmec%vmec => vmec_object
      equilibrium_construct_vmec%force_solve = force_solve

      CALL profiler_set_stop_time('equilibrium_construct_vmec',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref equilibrium_class containing a
!>  @ref vacuum_equilibrium object.
!>
!>  Allocates memory and initializes a @ref equilibrium_class object.
!>
!>  @param[in] vacuum_object An instance of a @ref vacuum_equilibrium subclass.
!>  @param[in] force_solve   If true, forces the equilbirum to resolve every
!>                           time.
!>  @returns A pointer to a constructed @ref equilibrium_class object.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_construct_vacuum(vacuum_object, force_solve)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), POINTER :: equilibrium_construct_vacuum
      TYPE (vacuum_class), POINTER      :: vacuum_object
      LOGICAL, INTENT(in)               :: force_solve

!  local variables
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(equilibrium_construct_vacuum)

      equilibrium_construct_vacuum%type = equilibrium_vacuum_type
      equilibrium_construct_vacuum%vacuum => vacuum_object
      equilibrium_construct_vacuum%force_solve = force_solve

      CALL profiler_set_stop_time('equilibrium_construct_vacuum',              &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref equilibrium_class containing a
!>  @ref siesta_equilibrium object.
!>
!>  Allocates memory and initializes a @ref equilibrium_class object.
!>
!>  @param[in] siesta_object An instance of a @ref siesta_equilibrium subclass.
!>  @param[in] force_solve   If true, forces the equilbirum to resolve every
!>                           time.
!>  @returns A pointer to a constructed @ref equilibrium_class object.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_construct_siesta(siesta_object, force_solve)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), POINTER :: equilibrium_construct_siesta
      TYPE (siesta_class), POINTER      :: siesta_object
      LOGICAL, INTENT(in)               :: force_solve

!  local variables
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(equilibrium_construct_siesta)

      equilibrium_construct_siesta%type = equilibrium_siesta_type
      equilibrium_construct_siesta%siesta => siesta_object
      equilibrium_construct_siesta%force_solve = force_solve

      CALL profiler_set_stop_time('equilibrium_construct_siesta',              &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref equilibrium_class object.
!>
!>  Deallocates memory and uninitializes a @ref equilibrium_class object.
!>
!>  @param[inout] this A @ref equilibrium_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), POINTER :: this

!  Start of executable code
      IF (ASSOCIATED(this%vmec)) THEN
         CALL vmec_destruct(this%vmec)
         this%vmec => null()
      END IF

      IF (ASSOCIATED(this%vacuum)) THEN
         CALL vacuum_destruct(this%vacuum)
         this%vacuum => null()
      END IF

      IF (ASSOCIATED(this%siesta)) THEN
         CALL siesta_destruct(this%siesta)
         this%siesta => null()
      END IF

      this%type = equilibrium_no_type
      this%force_solve = .false.

      DEALLOCATE(this)

      END SUBROUTINE

!*******************************************************************************
!  SETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Sets the value of a reconstruction equilibrium parameter.
!>
!>  This method is virtual. The actual setting of the parameter should be
!>  handled by a subclass method. The subclass is responsible updating the
!>  state flags.
!>  @see vmec_equilibrium::vmec_set_param
!>  @see vacuum_equilibrium::vacuum_set_param
!>  @see siesta_equilibrium::siesta_set_param
!>
!>  @param[inout] this        A @ref equilibrium_class instance.
!>  @param[in]    id          ID of the parameter.
!>  @param[in]    i_index     The ith index of the parameter.
!>  @param[in]    j_index     The jth index of the parameter.
!>  @param[in]    value       The value of the parameter.
!>  @param[in]    eq_comm     MPI communicator for the child equilibrium processes.
!>  @param[inout] state_flags Bitwise flags to indicate which parts of the model
!>                            changed.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_set_param(this, id, i_index, j_index,             &
     &                                 value, eq_comm, state_flags)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                     :: id
      INTEGER, INTENT(in)                     :: i_index
      INTEGER, INTENT(in)                     :: j_index
      REAL (rprec), INTENT(in)                :: value
      INTEGER, INTENT(in)                     :: eq_comm
      INTEGER, INTENT(inout)                  :: state_flags

!  local variables
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

!  Set param needs to come first so that function gets executed. Otherwise, when
!  this%not_converged was true, the expression will evaluate to false all the
!  time and not bother calling vmec_set_param.
         CASE (equilibrium_vmec_type)
            CALL vmec_set_param(this%vmec, id, i_index, j_index, value,        &
     &                          eq_comm, state_flags)

         CASE (equilibrium_vacuum_type)
            CALL vacuum_set_param(this%vacuum, id, i_index, value)

         CASE (equilibrium_siesta_type)
            CALL siesta_set_param(this%siesta, id, i_index, j_index,           &
     &                            value, eq_comm, state_flags)

      END SELECT

      CALL profiler_set_stop_time('equilibrium_set_param', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Sets the magnetic cache of the equilibrium for the magnetic responce.
!>
!>  This method is virtual. The actual setting of the magnetic cache should be
!>  handled a subclass method. The magnetic cache is any pre compuation that
!>  needs to be performed so that magnetic signals may compute a modeled signal.
!>  If the equilibrium does not require any information to be cached, this
!>  method does not need to be overridden. This method is meant to only be
!>  called once to tell the equilibrium it needs to use a magnetic cache.
!>  @see vmec_equilibrium::vmec_set_magnetic_cache
!>  @see siesta_equilibrium::siesta_set_magnetic_cache
!>
!>  @param[inout] this            A @ref equilibrium_class instance.
!>  @param[in]    response_object A @ref magnetic_response::magnetic_response_class
!>                                instance.
!>  @param[in]    state_flags     Bitwise flags to indicate which parts of the
!>                                model changed.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_set_magnetic_cache_response(this,                 &
     &                                                  response_object,       &
     &                                                  state_flags)
      USE magnetic_response
      USE model_state

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), INTENT(inout)    :: this
      TYPE (magnetic_response_class), INTENT(in) :: response_object
      INTEGER, INTENT(in)                        :: state_flags

!  local variables
      REAL (rprec)                               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            CALL vmec_set_magnetic_cache(this%vmec, response_object)

!  If the equilibrium is already converged, compute the magnetic cache as well.
            IF (.not.BTEST(state_flags, model_state_vmec_flag)) THEN
               CALL vmec_set_magnetic_cache(this%vmec)
            END IF

         CASE (equilibrium_siesta_type)
            CALL siesta_set_magnetic_cache(this%siesta, response_object)

!  If the equilibrium is already converged, compute the magnetic cache as well.
            IF (.not.BTEST(state_flags, model_state_siesta_flag)) THEN
               CALL siesta_set_magnetic_cache(this%siesta)
            END IF

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'equilibrium_set_magnetic_cache_response', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Sets the magnetic cache of the equilibrium for points.
!>
!>  This method is virtual. The actual setting of the magnetic cache should be
!>  handled a subclass method. The magnetic cache is any pre compuation that
!>  needs to be performed so that magnetic signals may compute a modeled signal.
!>  If the equilibrium does not require any information to be cached, this
!>  method does not need to be overridden. This method is meant to only be
!>  called once to tell the equilibrium it needs to use a magnetic cache.
!>  @see vmec_equilibrium::vmec_set_magnetic_cache
!>  @see siesta_equilibrium::siesta_set_magnetic_cache
!>
!>  @param[inout] this        A @ref equilibrium_class instance.
!>  @param[in]    use_axi     Magnetics can subtract off axisymmetric
!>                            components.
!>  @param[in]    state_flags Bitwise flags to indicate which parts of the model
!>                            changed.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_set_magnetic_cache_point(this, use_axi,           &
     &                                                state_flags)
      USE model_state

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), INTENT(inout) :: this
      LOGICAL, INTENT(in)                     :: use_axi
      INTEGER, INTENT(in)                     :: state_flags

!  local variables
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            CALL vmec_set_magnetic_cache(this%vmec, use_axi)

!  If the equilibrium is already converged, compute the magnetic cache as well.
            IF (.not.BTEST(state_flags, model_state_vmec_flag)) THEN
               CALL vmec_set_magnetic_cache(this%vmec)
            END IF

         CASE (equilibrium_siesta_type)
            CALL siesta_set_magnetic_cache(this%siesta, use_axi)

!  If the equilibrium is already converged, compute the magnetic cache as well.
            IF (.not.BTEST(state_flags, model_state_siesta_flag)) THEN
               CALL siesta_set_magnetic_cache(this%siesta)
            END IF

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'equilibrium_set_magnetic_cache_point', start_time)

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Get the id for a reconstruction parameter.
!>
!>  This method is virtual. The actual getting of the reconstruction parameter
!>  id should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_param_id
!>  @see vacuum_equilibrium::vacuum_get_param_id
!>  @see siesta_equilibrium::siesta_get_param_id
!>
!>  @param[in] this       A @ref equilibrium_class instance.
!>  @param[in] param_name Name of a reconstruction parameter.
!>  @returns The id for a reconstruction parameter.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_param_id(this, param_name)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: equilibrium_get_param_id
      TYPE (equilibrium_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)        :: param_name

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_param_id =                                         &
     &         vmec_get_param_id(this%vmec, param_name)

         CASE (equilibrium_vacuum_type)
            equilibrium_get_param_id =                                         &
     &         vacuum_get_param_id(this%vacuum, param_name)

         CASE (equilibrium_siesta_type)
            equilibrium_get_param_id =                                         &
     &         siesta_get_param_id(this%siesta, param_name)

         CASE DEFAULT
            equilibrium_get_param_id = data_no_id

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_param_id',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the value of a reconstruction equilibrium parameter.
!>
!>  This method is virtual. The actual getting of the reconstruction parameter
!>  value should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_param_value
!>  @see vacuum_equilibrium::vacuum_get_param_value
!>  @see siesta_equilibrium::siesta_get_param_value
!>
!>  @param[in] this    A @ref equilibrium_class instance.
!>  @param[in] id      ID of the parameter.
!>  @param[in] i_index The ith index of the parameter.
!>  @param[in] j_index The jth index of the parameter.
!>  @returns The value of the parameter.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_param_value(this, id, i_index, j_index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_param_value
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: id
      INTEGER, INTENT(in)                  :: i_index
      INTEGER, INTENT(in)                  :: j_index

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_param_value =                                      &
     &         vmec_get_param_value(this%vmec, id, i_index, j_index)

         CASE (equilibrium_vacuum_type)
            equilibrium_get_param_value =                                      &
     &         vacuum_get_param_value(this%vacuum, id, i_index)

         CASE (equilibrium_siesta_type)
            equilibrium_get_param_value =                                      &
     &         siesta_get_param_value(this%siesta, id, i_index, j_index)

         CASE DEFAULT
            equilibrium_get_param_value = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_param_value',               &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the name of a reconstruction equilibrium parameter.
!>
!>  This method is virtual. The actual getting of the reconstruction parameter
!>  name should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_param_name
!>  @see vacuum_equilibrium::vacuum_get_param_name
!>  @see siesta_equilibrium::siesta_get_param_name
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns The name of the parameter.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_param_name(this, id)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER(len=data_name_length) :: equilibrium_get_param_name
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: id

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_param_name =                                       &
     &         vmec_get_param_name(this%vmec, id)

         CASE (equilibrium_vacuum_type)
            equilibrium_get_param_name =                                       &
     &         vacuum_get_param_name(this%vacuum, id)

         CASE (equilibrium_siesta_type)
            equilibrium_get_param_name =                                       &
     &         siesta_get_param_name(this%siesta, id)

         CASE DEFAULT
            equilibrium_get_param_name = ''

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_param_name',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the number of electron density gp kernel hyper parameters.
!>
!>  This method is virtual. The actual getting of the number of hyper parameters
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_ne_num_hyper_param
!>  @see siesta_equilibrium::siesta_get_gp_ne_num_hyper_param
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_ne_num_hyper_param(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: equilibrium_get_gp_ne_num_hyper_param
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_ne_num_hyper_param =                            &
     &         vmec_get_gp_ne_num_hyper_param(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_ne_num_hyper_param =                            &
     &         siesta_get_gp_ne_num_hyper_param(this%siesta)

         CASE DEFAULT
            equilibrium_get_gp_ne_num_hyper_param = 0

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'equilibrium_get_gp_ne_num_hyper_param', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron density profile af array.
!>
!>  This method is virtual. The actual getting of the af array should be handled
!>  by a subclass method.
!>  @see vmec_equilibrium::vmec_get_ne_af
!>  @see siesta_equilibrium::siesta_get_ne_af
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns Pointer to the electron density profile af array.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_ne_af(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER  :: equilibrium_get_ne_af
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_ne_af => vmec_get_ne_af(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_ne_af => siesta_get_ne_af(this%siesta)

         CASE DEFAULT
            equilibrium_get_ne_af => null()

      END SELECT

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron density gp kernel value for the two indicies.
!>
!>  This method is virtual. The actual getting of the kernel function
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_ne_ij
!>  @see siesta_equilibrium::siesta_get_gp_ne_ij
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @param[in] i    ith profile position.
!>  @param[in] j    jth profile position.
!>  @returns The value of the gp kernel function for i, j.
!>  @note Note the gp kernel is not implmented for all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_ne_ij(this, i, j)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_gp_ne_ij
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: i
      INTEGER, INTENT(in)                  :: j

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_ne_ij = vmec_get_gp_ne(this%vmec, i, j)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_ne_ij = siesta_get_gp_ne(this%siesta,           &
     &                                                  i, j)

         CASE DEFAULT
            equilibrium_get_gp_ne_ij = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_gp_ne_ij',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron density gp kernel value for the position and index.
!>
!>  This method is virtual. The actual getting of the kernel function
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_ne_pi
!>  @see siesta_equilibrium::siesta_get_gp_ne_pi
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the electron density at.
!>  @param[in] i      Profile position index.
!>  @returns The value of the gp kernel function for x_cart and i.
!>  @note Note the gp kernel is not implmented for all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_ne_pi(this, x_cart, i)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_gp_ne_pi
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: i

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_ne_pi = vmec_get_gp_ne(this%vmec,               &
     &                                                x_cart, i)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_ne_pi = siesta_get_gp_ne(this%siesta,           &
     &                                                  x_cart, i)

         CASE DEFAULT
            equilibrium_get_gp_ne_pi = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_gp_ne_pi',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron density gp kernel value for the position and
!>  position.
!>
!>  This method is virtual. The actual getting of the kernel function
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_ne_pp
!>  @see siesta_equilibrium::siesta_get_gp_ne_pp
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the kernel at.
!>  @param[in] y_cart Cartesian position to get the kernel at.
!>  @returns The value of the gp kernel function for x_cart and y_cart.
!>  @note Note the gp kernel is not implmented for all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_ne_pp(this, x_cart, y_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_gp_ne_pp
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      REAL (rprec), DIMENSION(3), INTENT(in) :: y_cart

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_ne_pp = vmec_get_gp_ne(this%vmec,               &
     &                                                x_cart, y_cart)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_ne_pp = siesta_get_gp_ne(this%siesta,           &
     &                                                  x_cart, y_cart)

         CASE DEFAULT
            equilibrium_get_gp_ne_pp = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_gp_ne_pp',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron density at a cartesian position.
!>
!>  This method is virtual. The actual getting of the electron density should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_ne
!>  @see siesta_equilibrium::siesta_get_ne
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the electron density at.
!>  @returns The electron density at x_cart.
!>  @note Note the electron density is not relevant to all equilibrium types.
!>  For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_ne_cart(this, x_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_ne_cart
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_ne_cart = vmec_get_ne(this%vmec, x_cart)

         CASE (equilibrium_siesta_type)
            equilibrium_get_ne_cart = siesta_get_ne(this%siesta, x_cart)

         CASE DEFAULT
            equilibrium_get_ne_cart = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_ne_cart',                   &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron density at a radial position.
!>
!>  This method is virtual. The actual getting of the electron density should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_ne
!>  @see siesta_equilibrium::siesta_get_ne
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @param[in] r    Radial position to get the electron density at.
!>  @returns The electron density at r.
!>  @note Note the electron density is not relevant to all equilibrium types.
!>  For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_ne_radial(this, r)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_ne_radial
      TYPE (equilibrium_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)             :: r

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_ne_radial = vmec_get_ne(this%vmec, r)

         CASE (equilibrium_siesta_type)
            equilibrium_get_ne_radial = siesta_get_ne(this%siesta, r)

         CASE DEFAULT
            equilibrium_get_ne_radial = 0.0

         END SELECT

      CALL profiler_set_stop_time('equilibrium_get_ne_radial',                 &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the number of electron temperature gp kernel hyper parameters.
!>
!>  This method is virtual. The actual getting of the number of hyper parameters
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_te_num_hyper_param
!>  @see siesta_equilibrium::siesta_get_gp_te_num_hyper_param
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_te_num_hyper_param(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: equilibrium_get_gp_te_num_hyper_param
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_te_num_hyper_param =                            &
     &         vmec_get_gp_te_num_hyper_param(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_te_num_hyper_param =                            &
     &         siesta_get_gp_te_num_hyper_param(this%siesta)

         CASE DEFAULT
            equilibrium_get_gp_te_num_hyper_param = 0

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'equilibrium_get_gp_te_num_hyper_param', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron temperature profile af array.
!>
!>  This method is virtual. The actual getting of the af array should be handled
!>  by a subclass method.
!>  @see vmec_equilibrium::vmec_get_te_af
!>  @see siesta_equilibrium::siesta_get_te_af
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns Pointer to the electron temperature profile af array.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_te_af(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER  :: equilibrium_get_te_af
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_te_af => vmec_get_te_af(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_te_af => siesta_get_te_af(this%siesta)

         CASE DEFAULT
            equilibrium_get_te_af => null()

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_te_af', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron temperature gp kernel value for the two indicies.
!>
!>  This method is virtual. The actual getting of the kernel function
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_te_ij
!>  @see siesta_equilibrium::siesta_get_gp_te_ij
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @param[in] i    ith profile position.
!>  @param[in] j    ith profile position.
!>  @returns The value of the gp kernel function for i, j.
!>  @note Note the gp kernel is not implmented for all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_te_ij(this, i, j)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_gp_te_ij
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: i
      INTEGER, INTENT(in)                  :: j

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_te_ij = vmec_get_gp_te(this%vmec, i, j)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_te_ij = siesta_get_gp_te(this%siesta,           &
     &                                                  i, j)

         CASE DEFAULT
            equilibrium_get_gp_te_ij = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_gp_te_ij',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron temperature gp kernel value for the position and
!>  index.
!>
!>  This method is virtual. The actual getting of the kernel function
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_te_pi
!>  @see siesta_equilibrium::siesta_get_gp_te_pi
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the electron temperature at.
!>  @param[in] i      Profile position index.
!>  @returns The value of the gp kernel function for x_cart and i.
!>  @note Note the gp kernel is not implmented for all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_te_pi(this, x_cart, i)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_gp_te_pi
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: i

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_te_pi = vmec_get_gp_te(this%vmec,               &
     &                                                x_cart, i)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_te_pi = siesta_get_gp_te(this%siesta,           &
     &                                                  x_cart, i)

         CASE DEFAULT
            equilibrium_get_gp_te_pi = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_gp_te_pi',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron temperature gp kernel value for the position and
!>  position.
!>
!>  This method is virtual. The actual getting of the kernel function
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_te_pp
!>  @see siesta_equilibrium::siesta_get_gp_te_pp
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the kernel at.
!>  @param[in] y_cart Cartesian position to get the kernel at.
!>  @returns The value of the gp kernel function for x_cart and y_cart.
!>  @note Note the gp kernel is not implmented for all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_te_pp(this, x_cart, y_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_gp_te_pp
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      REAL (rprec), DIMENSION(3), INTENT(in) :: y_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_te_pp = vmec_get_gp_te(this%vmec,               &
     &                                                x_cart, y_cart)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_te_pp = siesta_get_gp_te(this%siesta,           &
     &                                                  x_cart, y_cart)

         CASE DEFAULT
            equilibrium_get_gp_te_pp = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_gp_te_pp',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron temperature at a cartesian position.
!>
!>  This method is virtual. The actual getting of the electron temperature
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_te
!>  @see siesta_equilibrium::siesta_get_te
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the electron temperature at.
!>  @returns The electron temperature at x_cart.
!>  @note Note the electron temperature is not relevant to all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_te_cart(this, x_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_te_cart
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_te_cart = vmec_get_te(this%vmec, x_cart)

         CASE (equilibrium_siesta_type)
            equilibrium_get_te_cart = siesta_get_te(this%siesta, x_cart)

         CASE DEFAULT
            equilibrium_get_te_cart = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_te_cart',                   &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the electron temperature at a radial position.
!>
!>  This method is virtual. The actual getting of the electron temperature
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_te
!>  @see siesta_equilibrium::siesta_get_te
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @param[in] r    Cartesian position to get the electron temperature at.
!>  @returns The electron temperature at r.
!>  @note Note the electron temperature is not relevant to all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_te_radial(this, r)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_te_radial
      TYPE (equilibrium_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)             :: r

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_te_radial = vmec_get_te(this%vmec, r)

         CASE (equilibrium_siesta_type)
            equilibrium_get_te_radial = siesta_get_te(this%siesta, r)

         CASE DEFAULT
            equilibrium_get_te_radial = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_te_radial',                 &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the number of ion temperature gp kernel hyper parameters.
!>
!>  This method is virtual. The actual getting of the number of hyper parameters
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_ti_num_hyper_param
!>  @see siesta_equilibrium::siesta_get_gp_ti_num_hyper_param
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_ti_num_hyper_param(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: equilibrium_get_gp_ti_num_hyper_param
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_ti_num_hyper_param =                            &
     &         vmec_get_gp_ti_num_hyper_param(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_ti_num_hyper_param =                            &
     &         siesta_get_gp_ti_num_hyper_param(this%siesta)

         CASE DEFAULT
            equilibrium_get_gp_ti_num_hyper_param = 0

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'equilibrium_get_gp_ti_num_hyper_param', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the ion temperature profile af array.
!>
!>  This method is virtual. The actual getting of the af array should be handled
!>  by a subclass method.
!>  @see vmec_equilibrium::vmec_get_ti_af
!>  @see siesta_equilibrium::siesta_get_ti_af
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns Pointer to the electron ion profile af array.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_ti_af(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER  :: equilibrium_get_ti_af
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_ti_af => vmec_get_ti_af(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_ti_af => siesta_get_ti_af(this%siesta)

         CASE DEFAULT
            equilibrium_get_ti_af => null()

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_ti_af', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the ion temperature gp kernel value for the two indicies.
!>
!>  This method is virtual. The actual getting of the kernel function
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_ti_ij
!>  @see siesta_equilibrium::siesta_get_gp_ti_ij
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @param[in] i    ith profile position.
!>  @param[in] j    ith profile position.
!>  @returns The value of the gp kernel function for i, j.
!>  @note Note the gp kernel is not implmented for all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_ti_ij(this, i, j)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_gp_ti_ij
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: i
      INTEGER, INTENT(in)                  :: j

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_ti_ij = vmec_get_gp_ti(this%vmec, i, j)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_ti_ij = siesta_get_gp_ti(this%siesta,           &
     &                                                  i, j)

         CASE DEFAULT
            equilibrium_get_gp_ti_ij = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_gp_ti_ij',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the ion temperature gp kernel value for the position and index.
!>
!>  This method is virtual. The actual getting of the kernel function
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_ti_pi
!>  @see siesta_equilibrium::siesta_get_gp_ti_pi
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the ion temperature at.
!>  @param[in] i      Profile position index.
!>  @returns The value of the gp kernel function for x_cart and i.
!>  @note Note the gp kernel is not implmented for all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_ti_pi(this, x_cart, i)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_gp_ti_pi
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: i

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_ti_pi = vmec_get_gp_ti(this%vmec,               &
     &                                                x_cart, i)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_ti_pi = siesta_get_gp_ti(this%siesta,           &
     &                                                  x_cart, i)

         CASE DEFAULT
            equilibrium_get_gp_ti_pi = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_gp_ti_pi',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the ion temperature gp kernel value for the position and
!>  position.
!>
!>  This method is virtual. The actual getting of the kernel function
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_ti_pp
!>  @see siesta_equilibrium::siesta_get_gp_ti_pp
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the kernel at.
!>  @param[in] y_cart Cartesian position to get the kernel at.
!>  @returns The value of the gp kernel function for x_cart and y_cart.
!>  @note Note the gp kernel is not implmented for all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_ti_pp(this, x_cart, y_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_gp_ti_pp
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      REAL (rprec), DIMENSION(3), INTENT(in) :: y_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_ti_pp = vmec_get_gp_ti(this%vmec,               &
     &                                                x_cart, y_cart)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_ti_pp = siesta_get_gp_ti(this%siesta,           &
     &                                                  x_cart, y_cart)

         CASE DEFAULT
            equilibrium_get_gp_ti_pp = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_gp_ti_pp',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the ion temperature at a cartesian position.
!>
!>  This method is virtual. The actual getting of the electron temperature
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_ti
!>  @see siesta_equilibrium::siesta_get_ti
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the electron temperature at.
!>  @returns The ion temperature at x_cart.
!>  @note Note the ion temperature is not relevant to all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_ti_cart(this, x_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_ti_cart
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_ti_cart = vmec_get_ti(this%vmec, x_cart)

         CASE (equilibrium_siesta_type)
            equilibrium_get_ti_cart = siesta_get_ti(this%siesta, x_cart)

         CASE DEFAULT
            equilibrium_get_ti_cart = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_ti_cart',                   &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the ion temperature at a radial position.
!>
!>  This method is virtual. The actual getting of the ion temperature should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_ti
!>  @see siesta_equilibrium::siesta_get_ti
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @param[in] r    Cartesian position to get the ion temperature at.
!>  @returns The ion temperature at r.
!>  @note Note the ion temperature is not relevant to all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_ti_radial(this, r)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_ti_radial
      TYPE (equilibrium_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)             :: r

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_ti_radial = vmec_get_ti(this%vmec, r)

         CASE (equilibrium_siesta_type)
            equilibrium_get_ti_radial = siesta_get_ti(this%siesta, r)

         CASE DEFAULT
            equilibrium_get_ti_radial = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_ti_radial',                 &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the effective charge at a cartesian position.
!>
!>  This method is virtual. The actual getting of the effective charge
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_ze
!>  @see siesta_equilibrium::siesta_get_ze
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the effective charge at.
!>  @returns The effective charge at x_cart.
!>  @note  Note the default choice for the effective charge is 1.0
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_ze_cart(this, x_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_ze_cart
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_ze_cart = vmec_get_ze(this%vmec, x_cart)

! @TODO add ze to siesta
!         CASE (equilibrium_siesta_type)
!            equilibrium_get_ze_cart = siesta_get_ze(this%siesta, x_cart)

         CASE DEFAULT
            equilibrium_get_ze_cart = 1.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_ze_cart',                   &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the effective charge at a radial position.
!>
!>  This method is virtual. The actual getting of the effective charge
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_ze
!>  @see siesta_equilibrium::siesta_get_ze
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @param[in] r    Cartesian position to get the effective charge at.
!>  @returns The effective charge at r.
!>  @note Note the default choice for the effective charge is 1.0
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_ze_radial(this, r)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_ze_radial
      TYPE (equilibrium_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)             :: r

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_ze_radial = vmec_get_ze(this%vmec, r)

! @TODO add ze to siesta
!         CASE (equilibrium_siesta_type)
!            equilibrium_get_ze_radial = siesta_get_ze(this%siesta, r)

         CASE DEFAULT
            equilibrium_get_ze_radial = 1.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_ze_radial',                 &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the number of soft x-ray emissivity gp kernel hyper parameters.
!>
!>  This method is virtual. The actual getting of the number of hyper parameters
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_sxrem_num_hyper_param
!>  @see siesta_equilibrium::siesta_get_gp_sxrem_num_hyper_param
!>
!>  @param[in] this  A @ref equilibrium_class instance.
!>  @param[in] index Index of the soft x-ray emissivity profile to use.
!>  @returns Number of kernel hyper parameters.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_sxrem_num_hyper_param(this, index)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: equilibrium_get_gp_sxrem_num_hyper_param
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: index

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_sxrem_num_hyper_param =                         &
     &         vmec_get_gp_sxrem_num_hyper_param(this%vmec, index)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_sxrem_num_hyper_param =                         &
     &         siesta_get_gp_sxrem_num_hyper_param(this%siesta, index)

         CASE DEFAULT
            equilibrium_get_gp_sxrem_num_hyper_param = 0

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'equilibrium_get_gp_sxrem_num_hyper_param', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the soft x-ray emissivity profile af array.
!>
!>  This method is virtual. The actual getting of the af array should be handled
!>  by a subclass method.
!>  @see vmec_equilibrium::vmec_get_sxrem_af
!>  @see siesta_equilibrium::siesta_get_sxrem_af
!>
!>  @param[in] this  A @ref equilibrium_class instance.
!>  @param[in] index Index of the soft x-ray emissivity profile to use.
!>  @returns Pointer to the soft x-ray emissivity profile af array.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_sxrem_af(this, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER  :: equilibrium_get_sxrem_af
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: index

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_sxrem_af => vmec_get_sxrem_af(this%vmec,           &
     &                                                    index)

         CASE (equilibrium_siesta_type)
            equilibrium_get_sxrem_af => siesta_get_sxrem_af(this%siesta,       &
     &                                                      index)

         CASE DEFAULT
            equilibrium_get_sxrem_af => null()

      END SELECT

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the electron density gp kernel value for the two indicies.
!>
!>  This method is virtual. The actual getting of the kernel function
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_ne_ij
!>  @see siesta_equilibrium::siesta_get_gp_ne_ij
!>
!>  @param[in] this  A @ref equilibrium_class instance.
!>  @param[in] i     ith profile position.
!>  @param[in] j     ith profile position.
!>  @param[in] index Index of the soft x-ray emissivity profile to use.
!>  @returns The value of the gp kernel function for i, j.
!>  @note Note the gp kernel is not implmented for all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_sxrem_ij(this, i, j, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_gp_sxrem_ij
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: i
      INTEGER, INTENT(in)                  :: j
      INTEGER, INTENT(in)                  :: index

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_sxrem_ij =                                      &
     &         vmec_get_gp_sxrem(this%vmec, i, j, index)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_sxrem_ij =                                      &
     &         siesta_get_gp_sxrem(this%siesta, i, j, index)

         CASE DEFAULT
            equilibrium_get_gp_sxrem_ij = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_gp_sxrem_ij',               &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the soft x-ray emissivity gp kernel value for the position and
!>  index.
!>
!>  This method is virtual. The actual getting of the kernel function
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_sxrem_pi
!>  @see siesta_equilibrium::siesta_get_gp_sxrem_pi
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the soft x-ray emissivity at.
!>  @param[in] i      Profile position index.
!>  @param[in] index  Index of the soft x-ray emissivity profile to use.
!>  @returns The value of the gp kernel function for x_cart and i.
!>  @note Note the gp kernel is not implmented for all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_sxrem_pi(this, x_cart, i, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_gp_sxrem_pi
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: i
      INTEGER, INTENT(in)                    :: index

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_sxrem_pi =                                      &
     &         vmec_get_gp_sxrem(this%vmec, x_cart, i, index)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_sxrem_pi =                                      &
     &         siesta_get_gp_sxrem(this%siesta, x_cart, i, index)

         CASE DEFAULT
            equilibrium_get_gp_sxrem_pi = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_gp_sxrem_pi',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the soft x-ray emissivity gp kernel value for the position and
!>  position.
!>
!>  This method is virtual. The actual getting of the kernel function
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_gp_sxrem_pp
!>  @see siesta_equilibrium::siesta_get_gp_sxrem_pp
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the electron density at.
!>  @param[in] y_cart Profile position index.
!>  @param[in] index  Index of the soft x-ray emissivity profile to use.
!>  @returns The value of the gp kernel function for x_cart and y_cart.
!>  @note Note the gp kernel is not implmented for all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_gp_sxrem_pp(this, x_cart, y_cart, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_gp_sxrem_pp
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      REAL (rprec), DIMENSION(3), INTENT(in) :: y_cart
      INTEGER, INTENT(in)                    :: index

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_gp_sxrem_pp =                                      &
     &         vmec_get_gp_sxrem(this%vmec, x_cart, y_cart, index)

         CASE (equilibrium_siesta_type)
            equilibrium_get_gp_sxrem_pp =                                      &
     &         siesta_get_gp_sxrem(this%siesta, x_cart, y_cart, index)

         CASE DEFAULT
            equilibrium_get_gp_sxrem_pp = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_gp_sxrem_pp',               &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the soft x-ray emissivity at a cartesian position.
!>
!>  This method is virtual. The actual getting of the soft x-ray emissivity
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_sxrem
!>  @see siesta_equilibrium::siesta_get_sxrem
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the soft x-ray emissivity at.
!>  @param[in] index  Index of the soft x-ray emissivity profile to use.
!>  @returns The soft x-ray emissivity at x_cart.
!>  @note Note the soft x-ray emissivity is not relevant to all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_sxrem_cart(this, x_cart, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_sxrem_cart
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      INTEGER, INTENT(in)                    :: index

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_sxrem_cart = vmec_get_sxrem(this%vmec,             &
     &                                                  x_cart, index)

         CASE (equilibrium_siesta_type)
            equilibrium_get_sxrem_cart = siesta_get_sxrem(this%siesta,         &
     &                                                    x_cart, index)

         CASE DEFAULT
            equilibrium_get_sxrem_cart = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_sxrem_cart',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the soft x-ray emissivity at a radial position.
!>
!>  This method is virtual. The actual getting of the soft x-ray emissivity
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_sxrem
!>  @see siesta_equilibrium::siesta_get_sxrem
!>
!>  @param[in] this  A @ref equilibrium_class instance.
!>  @param[in] r     Cartesian position to get the soft x-ray emissivity at.
!>  @param[in] index Index of the soft x-ray emissivity profile to use.
!>  @returns The soft x-ray emissivity at x_cart.
!>  @note Note the soft x-ray emissivity is not relevant to all equilibrium
!>  types. For instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_sxrem_radial(this, r, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_sxrem_radial
      TYPE (equilibrium_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)             :: r
      INTEGER, INTENT(in)                  :: index

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_sxrem_radial = vmec_get_sxrem(this%vmec,           &
     &                                                    r, index)

         CASE (equilibrium_siesta_type)
            equilibrium_get_sxrem_radial = siesta_get_sxrem(this%siesta,       &
     &                                                      r, index)

         CASE DEFAULT
            equilibrium_get_sxrem_radial = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_sxrem_radial',              &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the plasma pressure at a cartesian position.
!>
!>  This method is virtual. The actual getting of the plasma pressure should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_p
!>  @see siesta_equilibrium::siesta_get_p
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the plasma pressure at.
!>  @returns The plasma pressure at x_cart.
!>  @note Note the plasma pressure is not relevant to all equilibrium types. For
!>  instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_p_cart(this, x_cart)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_p_cart
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_p_cart = vmec_get_p(this%vmec, x_cart)

         CASE (equilibrium_siesta_type)
            equilibrium_get_p_cart = siesta_get_p(this%siesta, x_cart,         &
     &                                            .false.)

         CASE DEFAULT
            equilibrium_get_p_cart = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_p_cart', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the plasma pressure at a radial position.
!>
!>  This method is virtual. The actual getting of the plasma pressure should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_p
!>  @see siesta_equilibrium::siesta_get_p
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @param[in] r    Cartesian position to get the plasma pressure at.
!>  @returns The plasma pressure at r.
!>  @note Note the plasma pressure is not relevant to all equilibrium types. For
!>  instance, vacuum equilibrium should not override this method.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_p_radial(this, r)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_p_radial
      TYPE (equilibrium_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)             :: r

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_p_radial = vmec_get_p(this%vmec, r)

         CASE (equilibrium_siesta_type)
            equilibrium_get_p_radial = siesta_get_p(this%siesta, r,            &
     &                                              .false.)

         CASE DEFAULT
            equilibrium_get_p_radial = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_p_radial',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the magnetic field vector at a position.
!>
!>  This method is virtual. The actual getting of the magnetic field vector
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_B_vec
!>  @see vacuum_equilibrium::vacuum_get_B_vec
!>  @see siesta_equilibrium::siesta_get_B_vec
!>
!>  @param[in] this   A @ref equilibrium_class instance.
!>  @param[in] x_cart Cartesian position to get the magnetic field vector at.
!>  @param[in] cyl    Flag that specifies if the bfield should be returned in
!>                    cartesian or cylindical coordinates.
!>  @returns The magnetic field vector at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_B_vec(this, x_cart, cyl)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(3) :: equilibrium_get_B_vec
      TYPE (equilibrium_class), INTENT(in)   :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      LOGICAL, INTENT(in)                    :: cyl

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_B_vec = vmec_get_B_vec(this%vmec, x_cart,          &
     &                                             cyl)

         CASE (equilibrium_vacuum_type)
            equilibrium_get_B_vec = vacuum_get_B_vec(this%vacuum,              &
     &                                               x_cart, cyl)

         CASE (equilibrium_siesta_type)
            equilibrium_get_B_vec = siesta_get_B_vec(this%siesta,              &
     &                                               x_cart, cyl)

         CASE DEFAULT
            equilibrium_get_B_vec = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_B_vec', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the loop integrated magnetic field at a position.
!>
!>  This method is virtual. The actual getting of the loop integrated magnetic
!>  field should be handled by a subclass method. Returns the closed loop
!>  integration of the magnetic field to determine the enclosed current.
!>  @see vmec_equilibrium::vmec_get_Int_B_dphi
!>  @see vacuum_equilibrium::vacuum_get_Int_B_dphi
!>  @see siesta_equilibrium::siesta_get_Int_B_dphi
!>
!>  @param[in] this  A @ref equilibrium_class instance.
!>  @param[in] r     Radial position to integrate about.
!>  @param[in] theta Poloidal angle to integrate about.
!>  @returns The loop integrated magnetic field at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_Int_B_dphi(this, r, theta)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_Int_B_dphi
      TYPE (equilibrium_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)             :: r
      REAL (rprec), INTENT(in)             :: theta

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_Int_B_dphi =                                       &
     &         vmec_get_Int_B_dphi(this%vmec, r, theta)

         CASE (equilibrium_vacuum_type)
            equilibrium_get_Int_B_dphi =                                       &
     &         vacuum_get_Int_B_dphi(this%vacuum, r, theta)

         CASE (equilibrium_siesta_type)
            equilibrium_get_Int_B_dphi =                                       &
     &         siesta_get_Int_B_dphi(this%siesta, r, theta)

         CASE DEFAULT
            equilibrium_get_Int_B_dphi = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_Int_B_dphi',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the r and z positions of the outer surface at a toroidal angle.
!>
!>  This method is virtual. The actual getting of the magnetic field vector
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_plasma_edge
!>  @see siesta_equilibrium::siesta_get_plasma_edge
!>
!>  @param[in]  this A @ref equilibrium_class instance.
!>  @param[in]  phi  Toroidal angle to determine the outer surface at.
!>  @param[out] r    The radial postions of the other surface in a single
!>                   toroidal angle.
!>  @param[out] z    The Z postions of the other surface in a single toroidal
!>                   angle.
!>  @returns The number of elements in the r and z arrays.
!>
!>  @todo An "edge" could be defined as a the last field line that intersects a
!>  limiter for the vacuum case. By defineing a r_last parameter, a feild line
!>  can be followed around the machine. When that field line intersecs the
!>  desired phi plane, record the r and z positions. Field line following will
!>  also need to be performed for siesta.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_plasma_edge(this, phi, r, z)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: equilibrium_get_plasma_edge
      TYPE (equilibrium_class), INTENT(in)             :: this
      REAL (rprec), INTENT (in)                        :: phi
      REAL (rprec), DIMENSION(:), POINTER              :: r
      REAL (rprec), DIMENSION(:), POINTER              :: z

!  local variables
      REAL (rprec)                                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_plasma_edge =                                      &
     &         vmec_get_plasma_edge(this%vmec, phi, r, z)

         CASE (equilibrium_siesta_type)
            equilibrium_get_plasma_edge =                                      &
     &         siesta_get_plasma_edge(this%siesta, phi, r, z)

         CASE DEFAULT
            equilibrium_get_plasma_edge = 0
            r => null()
            z => null()

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_plasma_edge',               &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get volume magnetic volume integration radial grid points.
!>
!>  This method is virtual. The actual getting of the radial grid points
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_magnetic_volume_rgrid
!>  @see siesta_equilibrium::siesta_get_magnetic_volume_rgrid
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns The radial grid points. 
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_magnetic_volume_rgrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:,:), POINTER ::                               &
     &   equilibrium_get_magnetic_volume_rgrid
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_magnetic_volume_rgrid =>                           &
     &         vmec_get_magnetic_volume_rgrid(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_magnetic_volume_rgrid =>                           &
     &         siesta_get_magnetic_volume_rgrid(this%siesta)

         CASE DEFAULT
            equilibrium_get_magnetic_volume_rgrid => null()

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'equilibrium_get_magnetic_volume_rgrid', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get volume magnetic volume integration z grid points.
!>
!>  This method is virtual. The actual getting of the z grid points should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_magnetic_volume_zgrid
!>  @see siesta_equilibrium::siesta_get_magnetic_volume_zgrid
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns The z grid points.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_magnetic_volume_zgrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:,:), POINTER ::                               &
     &   equilibrium_get_magnetic_volume_zgrid
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_magnetic_volume_zgrid =>                           &
     &         vmec_get_magnetic_volume_zgrid(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_magnetic_volume_zgrid =>                           &
     &         siesta_get_magnetic_volume_zgrid(this%siesta)

         CASE DEFAULT
            equilibrium_get_magnetic_volume_zgrid => null()

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'equilibrium_get_magnetic_volume_zgrid', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get volume magnetic volume integration jr grid points.
!>
!>  This method is virtual. The actual getting of the jr grid points should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_magnetic_volume_jrgrid
!>  @see siesta_equilibrium::siesta_get_magnetic_volume_jrgrid
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns The jr grid points.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_magnetic_volume_jrgrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:,:), POINTER ::                               &
     &   equilibrium_get_magnetic_volume_jrgrid
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_magnetic_volume_jrgrid =>                          &
     &         vmec_get_magnetic_volume_jrgrid(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_magnetic_volume_jrgrid =>                          &
     &         siesta_get_magnetic_volume_jrgrid(this%siesta)

         CASE DEFAULT
            equilibrium_get_magnetic_volume_jrgrid => null()

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'equilibrium_get_magnetic_volume_jrgrid', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get volume magnetic volume integration jphi grid points.
!>
!>  This method is virtual. The actual getting of the jphi grid points should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_magnetic_volume_jphigrid
!>  @see siesta_equilibrium::siesta_get_magnetic_volume_jphigrid
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns The jphi grid points.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_magnetic_volume_jphigrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:,:), POINTER ::                               &
     &   equilibrium_get_magnetic_volume_jphigrid
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_magnetic_volume_jphigrid =>                        &
     &         vmec_get_magnetic_volume_jphigrid(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_magnetic_volume_jphigrid =>                        &
     &         siesta_get_magnetic_volume_jphigrid(this%siesta)

         CASE DEFAULT
            equilibrium_get_magnetic_volume_jphigrid => null()

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'equilibrium_get_magnetic_volume_jphigrid', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get volume magnetic volume integration jz grid points.
!>
!>  This method is virtual. The actual getting of the jz grid points should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_magnetic_volume_jzgrid
!>  @see siesta_equilibrium::siesta_get_magnetic_volume_jzgrid
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns The jz grid points.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_magnetic_volume_jzgrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:,:), POINTER ::                               &
     &   equilibrium_get_magnetic_volume_jzgrid
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_magnetic_volume_jzgrid =>                          &
     &         vmec_get_magnetic_volume_jzgrid(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_magnetic_volume_jzgrid =>                          &
     &         siesta_get_magnetic_volume_jzgrid(this%siesta)

         CASE DEFAULT
            equilibrium_get_magnetic_volume_jzgrid => null()

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'equilibrium_get_magnetic_volume_jzgrid', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get volume integration element.
!>
!>  This method is virtual. The actual getting of the volume integration element
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_volume_int_element
!>  @see siesta_equilibrium::siesta_get_volume_int_element
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns The volume integration element.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_volume_int_element(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_volume_int_element
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

        CASE (equilibrium_vmec_type)
            equilibrium_get_volume_int_element =                               &
     &         vmec_get_volume_int_element(this%vmec)

        CASE (equilibrium_siesta_type)
           equilibrium_get_volume_int_element =                               &
     &         siesta_get_volume_int_element(this%siesta)

         CASE DEFAULT
            equilibrium_get_volume_int_element = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_volume_int_element',        &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the conducting surface integration kr grid points.
!>
!>  This method is virtual. The actual getting of the kr grid points should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_con_surface_krgrid
!>  @see siesta_equilibrium::siesta_get_con_surface_krgrid
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns The kr grid points.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_con_surface_krgrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:), POINTER ::                                 &
     &   equilibrium_get_con_surface_krgrid
      TYPE (equilibrium_class), INTENT(in)  :: this

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_con_surface_krgrid =>                              &
     &         vmec_get_con_surface_krgrid(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_con_surface_krgrid =>                              &
     &         siesta_get_con_surface_krgrid(this%siesta)

         CASE DEFAULT
            equilibrium_get_con_surface_krgrid => null()

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'equilibrium_get_con_surface_krgrid', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the conducting surface integration kphi grid points.
!>
!>  This method is virtual. The actual getting of the kphi grid points should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_con_surface_kphigrid
!>  @see siesta_equilibrium::siesta_get_con_surface_kphigrid
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns The kphi grid points.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_con_surface_kphigrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:), POINTER ::                                 &
     &   equilibrium_get_con_surface_kphigrid
      TYPE (equilibrium_class), INTENT(in)  :: this

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_con_surface_kphigrid =>                            &
     &         vmec_get_con_surface_kphigrid(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_con_surface_kphigrid =>                            &
     &         siesta_get_con_surface_kphigrid(this%siesta)

         CASE DEFAULT
            equilibrium_get_con_surface_kphigrid => null()

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'equilibrium_get_con_surface_kphigrid', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the conducting surface integration kz grid points.
!>
!>  This method is virtual. The actual getting of the kz grid points should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_con_surface_kzgrid
!>  @see siesta_equilibrium::siesta_get_con_surface_kzgrid
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns The kz grid points.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_con_surface_kzgrid(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:,:), POINTER ::                                 &
     &   equilibrium_get_con_surface_kzgrid
      TYPE (equilibrium_class), INTENT(in)  :: this

!  local variables
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_con_surface_kzgrid =>                              &
     &         vmec_get_con_surface_kzgrid(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_con_surface_kzgrid =>                              &
     &         siesta_get_con_surface_kzgrid(this%siesta)

         CASE DEFAULT
            equilibrium_get_con_surface_kzgrid => null()

      END SELECT

      CALL profiler_set_stop_time(                                             &
     &        'equilibrium_get_con_surface_kzgrid', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get area integration element.
!>
!>  This method is virtual. The actual getting of the area integration element
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_area_int_element
!>  @see siesta_equilibrium::siesta_get_area_int_element
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns The volume integration element.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_area_int_element(this)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: equilibrium_get_area_int_element
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_area_int_element =                                 &
     &         vmec_get_area_int_element(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_area_int_element =                                 &
     &         siesta_get_area_int_element(this%siesta)

         CASE DEFAULT
            equilibrium_get_area_int_element = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_area_int_element',          &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get external current.
!>
!>  This method is virtual. The actual getting of the external current should be
!>  handled by a subclass method. To void extra memory operations, arrays are
!>  returned as pointers. The memory management is handled by the equilibrium
!>  subclass instance. If the equilibrium has no external currents return a
!>  null() pointer or don't override this method. MAKEGRID has a mode where
!>  currents maybe scaled. Inform the signal using this to scale the currents
!>  if need be.
!>  @see vmec_equilibrium::vmec_get_ext_currents
!>  @see vacuum_equilibrium::vacuum_get_ext_currents
!>  @see siesta_equilibrium::siesta_get_ext_currents
!>
!>  @param[in]  this           A @ref equilibrium_class instance.
!>  @param[in]  num_currents   Forces the number of currents to return if
!>                             greater than zero.
!>  @param[out] scale_currents Informs the caller that currents need to be
!>                             scaled.
!>  @returns The external currents.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_ext_currents(this, num_currents,                &
     &                                      scale_currents)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER  ::                                  &
     &   equilibrium_get_ext_currents
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER                              :: num_currents
      LOGICAL, INTENT(out)                 :: scale_currents

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_ext_currents =>                                    &
     &         vmec_get_ext_currents(this%vmec, num_currents,                  &
     &                               scale_currents)

         CASE (equilibrium_vacuum_type)
!  Vacuum equilibira always return currents. No need to force it.
            equilibrium_get_ext_currents =>                                    &
     &         vacuum_get_ext_currents(this%vacuum, scale_currents)

         CASE (equilibrium_siesta_type)
            equilibrium_get_ext_currents =>                                    &
     &         siesta_get_ext_currents(this%siesta, num_currents,              &
     &                                 scale_currents)

         CASE DEFAULT
            scale_currents = .false.
            equilibrium_get_ext_currents => null()

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_ext_currents',              &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get external plasma magnetic field.
!>
!>  This method is virtual. The actual getting of the external current should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_ext_b_plasma
!>  @see siesta_equilibrium::siesta_get_ext_b_plasma
!>
!>  @param[in] this     A @ref equilibrium_class instance.
!>  @param[in] position Position to compute the fields at.
!>  @param[in] axi_only Gives only the axisymmtric component of the magnetic
!>                      field.
!>  @returns The external currents.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_ext_b_plasma(this, position, axi_only)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(3) :: equilibrium_get_ext_b_plasma
      TYPE (equilibrium_class), INTENT(in) :: this
      REAL (rprec), DIMENSION(3)           :: position
      LOGICAL, INTENT(in)                  :: axi_only

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_ext_b_plasma =                                     &
     &         vmec_get_ext_b_plasma(this%vmec, position, axi_only)

         CASE (equilibrium_siesta_type)
            equilibrium_get_ext_b_plasma =                                     &
     &         siesta_get_ext_b_plasma(this%siesta, position, axi_only)

         CASE DEFAULT
            equilibrium_get_ext_b_plasma = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_ext_b_plasma',              &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get radial grid size.
!>
!>  This method is virtual. The actual getting of the radial grid size should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_grid_size
!>  @see siesta_equilibrium::siesta_get_grid_size
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns Size of the radial grid.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_grid_size(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: equilibrium_get_grid_size
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_grid_size = vmec_get_grid_size(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_grid_size =                                        &
     &         siesta_get_grid_size(this%siesta)

         CASE DEFAULT
            equilibrium_get_grid_size = 0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_grid_size',                 &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get start of the radial grid.
!>
!>  This method is virtual. The actual getting of the radial grid start should
!>  be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_grid_start
!>  @see siesta_equilibrium::siesta_get_grid_start
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns Start of the radial grid.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_grid_start(this)

      IMPLICIT NONE

!  Declare Arguments
      REAl (rprec) :: equilibrium_get_grid_start
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_grid_start = vmec_get_grid_start(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_grid_start =                                       &
     &         siesta_get_grid_start(this%siesta)

         CASE DEFAULT
            equilibrium_get_grid_start = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_grid_start',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get stop of the radial grid.
!>
!>  This method is virtual. The actual getting of the radial grid stop should
!>  be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_get_grid_stop
!>  @see siesta_equilibrium::siesta_get_grid_stop
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns Stop of the radial grid.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_get_grid_stop(this)

      IMPLICIT NONE

!  Declare Arguments
      REAl (rprec) :: equilibrium_get_grid_stop
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_get_grid_stop = vmec_get_grid_stop(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_get_grid_stop =                                        &
     &         siesta_get_grid_stop(this%siesta)

         CASE DEFAULT
            equilibrium_get_grid_stop = 0.0

      END SELECT

      CALL profiler_set_stop_time('equilibrium_get_grid_stop',                 &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  QUERY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Checks if a parameter id is a scaler value.
!>
!>  This method is virtual. The actual check should be handled by a subclass
!>  method.
!>  @see vmec_equilibrium::vmec_is_scaler_value
!>  @see siesta_equilibrium::siesta_is_scaler_value
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns True if the parameter is a scaler and false if otherwise.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_is_scaler_value(this, id)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL :: equilibrium_is_scaler_value
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: id

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_is_scaler_value =                                      &
     &         vmec_is_scaler_value(this%vmec, id)

         CASE (equilibrium_siesta_type)
            equilibrium_is_scaler_value =                                      &
     &         siesta_is_scaler_value(this%siesta, id)

         CASE DEFAULT
            equilibrium_is_scaler_value = .false.

      END SELECT

      CALL profiler_set_stop_time('equilibrium_is_scaler_value',               &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Checks if a parameter id is a 1d array.
!>
!>  This method is virtual. The actual check should be handled by a subclass
!>  method.
!>  @see vmec_equilibrium::vmec_is_1d_array
!>  @see vacuum_equilibrium::vacuum_is_1d_array
!>  @see siesta_equilibrium::siesta_is_1d_array
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns True if the parameter is a 1d array and false if otherwise.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_is_1d_array(this, id)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL :: equilibrium_is_1d_array
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: id

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_is_1d_array = vmec_is_1d_array(this%vmec, id)

         CASE (equilibrium_vacuum_type)
            equilibrium_is_1d_array = vacuum_is_1d_array(this%vacuum,          &
     &                                                   id)

         CASE (equilibrium_siesta_type)
            equilibrium_is_1d_array = siesta_is_1d_array(this%siesta,          &
     &                                                   id)

         CASE DEFAULT
            equilibrium_is_1d_array = .false.

      END SELECT

      CALL profiler_set_stop_time('equilibrium_is_1d_array', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Checks if a parameter id is a 2d array.
!>
!>  This method is virtual. The actual check should be handled by a subclass
!>  method.
!>  @see vmec_equilibrium::vmec_is_2d_array
!>  @see siesta_equilibrium::siesta_is_2d_array
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns True if the parameter is a 2d array and false if otherwise.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_is_2d_array(this, id)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL :: equilibrium_is_2d_array
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: id

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_is_2d_array = vmec_is_2d_array(this%vmec, id)

         CASE (equilibrium_siesta_type)
            equilibrium_is_2d_array = siesta_is_2d_array(this%siesta,          &
     &                                                   id)

         CASE DEFAULT
            equilibrium_is_2d_array = .false.

      END SELECT

      CALL profiler_set_stop_time('equilibrium_is_2d_array', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Checks if a parameter id is a reconstruction parameter.
!>
!>  This method is virtual. The actual check should be handled by a subclass
!>  method.
!>  @see vmec_equilibrium::vmec_is_recon_param
!>  @see vacuum_equilibrium::vacuum_is_recon_param
!>  @see siesta_equilibrium::siesta_is_recon_param
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns True if the parameter is a reconstruction parameter and false if
!>  otherwise.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_is_recon_param(this, id)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL :: equilibrium_is_recon_param
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: id

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_is_recon_param =                                       &
     &         vmec_is_recon_param(this%vmec, id)

         CASE (equilibrium_vacuum_type)
            equilibrium_is_recon_param =                                       &
     &         vacuum_is_recon_param(this%vacuum, id)

         CASE (equilibrium_siesta_type)
            equilibrium_is_recon_param =                                       &
     &         siesta_is_recon_param(this%siesta, id)

         CASE DEFAULT
            equilibrium_is_recon_param = .false.

      END SELECT

      CALL profiler_set_stop_time('equilibrium_is_recon_param',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Checks if a the point magnetics are being used.
!>
!>  This method is virtual. The actual check should be handled by a subclass
!>  method.
!>  @see vmec_equilibrium::vmec_is_using_point
!>  @see siesta_equilibrium::siesta_is_using_point
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @returns True if the point magnetic are being used.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_is_using_point(this)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL :: equilibrium_is_using_point
      TYPE (equilibrium_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_is_using_point = vmec_is_using_point(this%vmec)

         CASE (equilibrium_siesta_type)
            equilibrium_is_using_point =                                       &
     &         siesta_is_using_point(this%siesta)

         CASE DEFAULT
            equilibrium_is_using_point = .false.

      END SELECT

      CALL profiler_set_stop_time('equilibrium_is_using_point',                &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Solves the equilibrium.
!>
!>  This method is virtual. The actual solving of the equilibrium should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_converge
!>  @see siesta_equilibrium::siesta_converge
!>
!>  @param[inout] this        A @ref equilibrium_class instance.
!>  @param[inout] num_iter    Counter to track the number of iterations.
!>  @param[in]    iou         Input/output unit of the file to write logs to.
!>  @param[in]    eq_comm     MPI communicator pool for the equilibrium.
!>  @param[in]    state_flags Bitwise flags to indicate which parts of the model
!>                            changed.
!>  @returns True if the convergece was sucessful and false otherwise.
!-------------------------------------------------------------------------------
      FUNCTION equilibrium_converge(this, num_iter, iou, eq_comm,              &
     &                              state_flags)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL                                 :: equilibrium_converge
      TYPE (equilibrium_class), INTENT(inout) :: this
      INTEGER, INTENT(inout)                  :: num_iter
      INTEGER, INTENT(in)                     :: iou
      INTEGER, INTENT(in)                     :: eq_comm
      INTEGER, INTENT(in)                     :: state_flags

!  local variables
      INTEGER                                 :: error
      INTEGER                                 :: eq_rank
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

#if defined(MPI_OPT)
      CALL MPI_BCAST(num_iter, 1, MPI_INTEGER, 0, eq_comm, error)
#endif
      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            equilibrium_converge = vmec_converge(this%vmec, num_iter,          &
     &                                           iou, eq_comm)

         CASE (equilibrium_siesta_type)
            equilibrium_converge = siesta_converge(this%siesta,                &
     &                                             num_iter, iou,              &
     &                                             eq_comm, state_flags)

         CASE DEFAULT
            equilibrium_converge = .false.

      END SELECT

      CALL profiler_set_stop_time('equilibrium_converge', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Loads the vacuum magnetic field file.
!>
!>  Loads the vacuum magnetic field file. This will get on multiple processes to
!>  allow parallel loading of vacuum files.
!>  @see vmec_equilibrium::vmec_read_vac_file
!>  @see siesta_equilibrium::siesta_read_vac_file
!>
!>  @param[in] this    A @ref equilibrium_class instance.
!>  @param[in] index   Index of the changed current.
!>  @param[in] eq_comm MPI communicator pool for VMEC.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_read_vac_file(this, index, eq_comm)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: index
      INTEGER, INTENT(in)                  :: eq_comm

!  Start of executable code
       SELECT CASE (this%type)

          CASE (equilibrium_vmec_type)
             CALL vmec_read_vac_file(this%vmec, index, eq_comm)

          CASE (equilibrium_siesta_type)
             CALL siesta_read_vac_file(this%siesta, index, eq_comm)

       END SELECT

       END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Save the internal state of the equilibrium.
!>
!>  This method is virtual. The actual saving of the equilibrium state should be
!>  handled by a subclass method. 
!>  @see vmec_equilibrium::vmec_save_state
!>  @see siesta_equilibrium::siesta_save_state
!>
!>  @param[inout] this A @ref equilibrium_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_save_state(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), INTENT(inout) :: this

!  local variables
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            CALL vmec_save_state(this%vmec)

         CASE (equilibrium_siesta_type)
            CALL siesta_save_state(this%siesta)

      END SELECT

      CALL profiler_set_stop_time('equilibrium_save_state', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Reset the internal state of the equilibrium.
!>
!>  This method is virtual. The actual reseting of the equilibrium state should
!>  be handled by a subclass method. Sub classes need to make sure that the
!>  equilibrium is reset to a converged state.
!>  @see vmec_equilibrium::vmec_reset_state
!>  @see siesta_equilibrium::siesta_reset_state
!>
!>  @param[inout] this A @ref equilibrium_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_reset_state(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), INTENT(inout) :: this

!  local variables
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            CALL vmec_reset_state(this%vmec)

         CASE (equilibrium_siesta_type)
            CALL siesta_reset_state(this%siesta)

      END SELECT

      CALL profiler_set_stop_time('equilibrium_reset_state',                   &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write out the equilibrium to an output file.
!>
!>  This method is virtual. The actual writing of the equilibrium should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_write
!>  @see vacuum_equilibrium::vacuum_write
!>  @see siesta_equilibrium::siesta_write
!>
!>  @param[in] this A @ref equilibrium_class instance.
!>  @param[in] iou  Input/output unit of the output file.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_write(this, iou)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: iou

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            CALL vmec_write(this%vmec, iou)

         CASE (equilibrium_vacuum_type)
            CALL vacuum_write(this%vacuum, iou)

         CASE (equilibrium_siesta_type)
            CALL siesta_write(this%siesta, iou)

      END SELECT

      CALL profiler_set_stop_time('equilibrium_write', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write the current valid input.
!>
!>  This method is virtual. The actual writing of the input file should be
!>  handled by a subclass method.
!>  @see vmec_equilibrium::vmec_write_input
!>  @see vaccum_equilibrium::vacuum_write_input
!>  @see siesta_equilibrium::siesta_write_input
!>
!>  @param[in] this         A @ref equilibrium_class instance.
!>  @param[in] current_step Step number to append to input filename.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_write_input(this, current_step)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER                              :: current_step

!  local variables
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            CALL vmec_write_input(this%vmec, current_step)

         CASE (equilibrium_vacuum_type)
            CALL vacuum_write_input(this%vacuum, current_step)

         CASE (equilibrium_siesta_type)
            CALL siesta_write_input(this%siesta, current_step)

      END SELECT

      CALL profiler_set_stop_time('equilibrium_write_input', start_time)

      END SUBROUTINE

!*******************************************************************************
!  NETCDF SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Define NetCDF variables for the result file
!>
!>  This method is virtual. The actual defining of the equilibrium variables
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_def_result
!>  @see siesta_equilibrium::siesta_def_result
!>
!>  @param[in] this             A @ref equilibrium_class instance.
!>  @param[in] result_ncid      NetCDF file id of the result file.
!>  @param[in] maxnsetps_dim_id NetCDF dimension id of the number of steps
!>                              dimension.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_def_result(this, result_ncid,                     &
     &                                  maxnsetps_dim_id)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: result_ncid
      INTEGER, INTENT(in)                  :: maxnsetps_dim_id

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            CALL vmec_def_result(this%vmec, result_ncid,                       &
     &                           maxnsetps_dim_id)

         CASE (equilibrium_siesta_type)
            CALL siesta_def_result(this%siesta, result_ncid,                   &
     &                             maxnsetps_dim_id)

      END SELECT

      CALL profiler_set_stop_time('equilibrium_def_result', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write inital data to NetCDF result file
!>
!>  This method is virtual. The actual writing of the equilibrium variables
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_write_init_data
!>  @see siesta_equilibrium::siesta_write_init_data
!>
!>  @param[in] this        A @ref equilibrium_class instance.
!>  @param[in] result_ncid NetCDF file id of the result file.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_write_init_data(this, result_ncid)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: result_ncid

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            CALL vmec_write_init_data(this%vmec, result_ncid)

         CASE (equilibrium_siesta_type)
            CALL siesta_write_init_data(this%siesta, result_ncid)

      END SELECT

      CALL profiler_set_stop_time('equilibrium_write_init_data',               &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write step data to NetCDF result file
!>
!>  This method is virtual. The actual writing of the equilibrium variables
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_write_step_data and
!>  @see siesta_equilibrium::siesta_write_step_data
!>
!>  @param[in] this         A @ref equilibrium_class instance.
!>  @param[in] result_ncid  NetCDF file id of the result file.
!>  @param[in] current_step Step index to write variables to.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_write_step_data(this, result_ncid,                &
     &                                       current_step)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: result_ncid
      INTEGER, INTENT(in)                  :: current_step

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            CALL vmec_write_step_data(this%vmec, result_ncid,                  &
     &                                current_step)

         CASE (equilibrium_siesta_type)
            CALL siesta_write_step_data(this%siesta, result_ncid,              &
     &                                  current_step)

      END SELECT

      CALL profiler_set_stop_time('equilibrium_write_step_data',               &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Restart equilibrium.
!>
!>  This method is virtual. The actual restarting of the equilibrium variables
!>  should be handled by a subclass method.
!>  @see vmec_equilibrium::vmec_restart and
!>  @see siesta_equilibrium::siesta_restart
!>
!>  @param[in] this         A @ref equilibrium_class instance.
!>  @param[in] result_ncid  NetCDF file id of the result file.
!>  @param[in] current_step Step index to write variables to.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_restart(this, result_ncid, current_step)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), INTENT(in) :: this
      INTEGER, INTENT(in)                  :: result_ncid
      INTEGER, INTENT(in)                  :: current_step

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            CALL vmec_restart(this%vmec, result_ncid, current_step)

         CASE (equilibrium_siesta_type)
            CALL siesta_restart(this%siesta, result_ncid, current_step)

      END SELECT

      CALL profiler_set_stop_time('equilibrium_restart', start_time)

      END SUBROUTINE

!*******************************************************************************
!  MPI SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Syncronize the equilibrium state to children.
!>
!>  Syncs data between the parent and child processes. If MPI support is not
!>  compiled in this subroutine reduces to a no op.
!>
!>  @param[inout] this       A @ref equilibrium_class instance.
!>  @param[in]    recon_comm MPI communicator for the reconstruction processes.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_sync_state(this, recon_comm)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                     :: recon_comm

#if defined(MPI_OPT)
!  local variables
      REAL (rprec)                            :: start_time
      INTEGER                                 :: error

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            CALL vmec_sync_state(this%vmec, recon_comm)

         CASE (equilibrium_siesta_type)
            CALL siesta_sync_state(this%siesta, recon_comm)

      END SELECT

      CALL profiler_set_stop_time('equilibrium_sync_state', start_time)

#endif
      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Syncronize a child equilibrium state to the parent.
!>
!>  Syncs data between a child and the parent process. If MPI support is not
!>  compiled in this subroutine reduces to a no op.
!>
!>  @param[inout] this       A @ref equilibrium_class instance.
!>  @param[in]    index      Reconstruction rank to sync.
!>  @param[in]    recon_comm MPI communicator for the reconstruction processes.
!-------------------------------------------------------------------------------
      SUBROUTINE equilibrium_sync_child(this, index, recon_comm)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (equilibrium_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                     :: index
      INTEGER, INTENT(in)                     :: recon_comm

#if defined(MPI_OPT)
!  local variables
      REAL (rprec)                            :: start_time
      INTEGER                                 :: mpi_rank
      INTEGER                                 :: error

!  Start of executable code
      start_time = profiler_get_start_time()

      CALL MPI_COMM_RANK(recon_comm, mpi_rank, error)

      SELECT CASE (this%type)

         CASE (equilibrium_vmec_type)
            CALL vmec_sync_child(this%vmec, index, recon_comm)

         CASE (equilibrium_siesta_type)
            CALL siesta_sync_child(this%siesta, index, recon_comm)

      END SELECT

      CALL profiler_set_stop_time('equilibrium_sync_child', start_time)

#endif
      END SUBROUTINE

      END MODULE

!-------------------------------------------------------------------------------
!  The @header, @table_section, @table_subsection, @item and @end_table commands
!  are custom defined commands in Doxygen.in. They are defined under ALIASES.
!  For the page created here, the 80 column limit is exceeded. Arguments of
!  aliases are separated by ','. If you intended ',' to be a string you must use
!  an escaped comma '\,'.
!
!>  @page vacuum_equilibrium_sec Vacuum Equilibrium Manual
!>
!>  @tableofcontents
!>  @section vacuum_equilibrium_model_intro_sec Introduction
!>  This page documents the V3FIT interface to the vacuum equilibrium. This
!>  lists all the parameters associated with a vacuum equilibrium. All
!>  parameters are documented in a table of the following form.
!>  @header{Input variable, Description, Code Reference}
!>
!>  @section vacuum_equilibrium_recon_param_sec Vacuum Reconstruction Parameters
!>  @begin_table
!>     @item{extcur, 1D Array of external currents., @ref vacuum_equilibrium::vacuum_class::extcur}
!>  @end_table
!*******************************************************************************
!>  @file vacuum_equilibrium.f
!>  @brief Contains module @ref vacuum_equilibrium.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref vacuum_class. This module contains
!>  all the code necessary to interface V3FIT with a vacuum equilibrium.
!>  @par Super Class:
!>  @ref equilibrium
!*******************************************************************************

      MODULE vacuum_equilibrium
      USE stel_kinds
      USE data_parameters
      USE biotsavart
      USE mpi_inc
      USE profiler

      IMPLICIT NONE

!*******************************************************************************
!  vacuum equilibrium module parameters
!*******************************************************************************
!  Define id's only for values that can change. All others shouldn't be needed
!  outside of the vacuum interface. Parameter id's start at 14 since 0-13 are
!  reserved for non equilibrium model parameters. These numbers will need to be
!  updated if any new model parameters are added.

!  Variable Parameters
!>  1D Array of external currents.
      INTEGER, PARAMETER :: vacuum_extcur_id = 14

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) vacuum base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a vacuum_equilibrium.
!>  @par Super Class:
!>  @ref equilibrium
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(equilibrium_class) :: vacuum_class
!>  File name of the vacuum namelist inout file.
         CHARACTER (len=path_length) :: vacuum_file_name
!>  Array of external currents
         REAL (rprec), DIMENSION(:), POINTER  :: extcur => null()
      CONTAINS
         PROCEDURE :: set_param => vacuum_set_param
         PROCEDURE :: get_type => vacuum_get_type()
         PROCEDURE :: get_param_id => vacuum_get_param_id
         PROCEDURE :: get_param_value => vacuum_get_param_value
         PROCEDURE :: get_param_name => vacuum_get_param_name
         PROCEDURE :: get_B_vec => vacuum_get_B_vec
         PROCEDURE :: get_Int_B_dphi => vacuum_get_Int_B_dphi
         PROCEDURE :: get_ext_currents => vacuum_get_ext_currents
         PROCEDURE :: is_1d_array => vacuum_is_1d_array
         PROCEDURE :: is_recon_param => vacuum_is_recon_param
         PROCEDURE :: write => vacuum_write
         PROCEDURE :: write_input => vacuum_write_input
         FINAL     :: vacuum_destruct
      END TYPE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref vacuum_class object.
!>
!>  Allocates memory and initializes a @ref vacuum_class object.
!>  @param[in] file_name Filename of the vacuum namelist input file.
!>  @param[in] iou       Input/output unit to log messages to.
!>  @returns A pointer to a constructed @ref vacuum_class object.
!-------------------------------------------------------------------------------
      FUNCTION vacuum_construct(file_name, iou)
      USE vacuum_input

      IMPLICIT NONE

!  Declare Arguments
      TYPE (vacuum_class), POINTER  :: vacuum_construct
      CHARACTER (len=*), INTENT(in) :: file_name
      INTEGER, INTENT(in)           :: iou

!  local variables
      TYPE (bsc_rs)                 :: bsc_rs_object
      REAL (rprec), DIMENSION(3)    :: center, mean_r
      REAL (rprec)                  :: total_current
      INTEGER                       :: i, j
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (*,*) ' *** Initializing vacuum equilibrium from file ' //         &
     &            TRIM(file_name)
      WRITE (iou,*) ' *** Initializing vacuum equilibrium from file ' //       &
     &              TRIM(file_name)

      ALLOCATE(vacuum_construct)

      vacuum_construct%vacuum_file_name = file_name
      CALL vacuum_input_read_namelist(file_name)

      ALLOCATE(vacuum_construct%extcur(n_extcur))
      vacuum_construct%extcur = extcur(1:n_extcur)

      CALL parse_coils_file(coils_dot_file)

      DO i = 1, SIZE(vacuum_construct%extcur)

!  Compute the current averaged center of the coil group.
         center = 0.0
         total_current = 0.0
         IF (l_rot_coil_center(i)) THEN
            center = cg_rot_xcent(i,:)
         ELSE
            DO j = 1, coil_group(i)%ncoil
               total_current = total_current                                   &
     &                       + coil_group(i)%coils(j)%current
               CALL bsc_mean_r(coil_group(i)%coils(j), mean_r)
               center = center + mean_r*coil_group(i)%coils(j)%current
            END DO
            IF (total_current .ne. 0.0) THEN
               center = center/total_current
            END IF
         END IF

!  Generate bsc_rs for first shift, and apply it.
         CALL bsc_construct_rs(bsc_rs_object, 0.0_dp, 0.0_dp, 0.0_dp,          &
     &                         (/ 0.0_dp, 0.0_dp, 0.0_dp /),                   &
     &                         cg_shift_1(i,:))
         CALL bsc_rot_shift(coil_group(i), bsc_rs_object)

!  Generate bsc_rs for second shift, and apply it.
         CALL bsc_construct_rs(bsc_rs_object, cg_rot_theta(i),                 &
     &                         cg_rot_phi(i), cg_rot_angle(i),                 &
     &                         center, cg_shift_2(i,:))
         CALL bsc_rot_shift(coil_group(i), bsc_rs_object)
      END DO

      CALL profiler_set_stop_time('vacuum_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref vacuum_class object.
!>
!>  Deallocates memory and uninitializes a @ref vacuum_class object.
!>
!>  @param[inout] this A @ref vacuum_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE vacuum_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (vacuum_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%extcur)) THEN
         DEALLOCATE(this%extcur)
         this%extcur => null()
      END IF

      END SUBROUTINE

!*******************************************************************************
!  SETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Sets the value of a reconstruction equilibrium parameter.
!>
!>  This method overrides @ref equilibrium::equilibrium_set_param. This function
!>  always returns false because a vacuum equilibrium never needs to be
!>  reconverged.
!>
!>  @param[inout] this        A @ref vaccum_class instance.
!>  @param[in]    id          ID of the parameter.
!>  @param[in]    i_index     The ith index of the parameter.
!>  @param[in]    j_index     The jth index of the parameter.
!>  @param[in]    value       The value of the parameter.
!>  @param[in]    eq_comm     MPI communicator for the child equilibrium processes.
!>  @param[inout] state_flags Bitwise flags to indicate which parts of the model
!>                            changed.
!-------------------------------------------------------------------------------
      SUBROUTINE vacuum_set_param(this, id, i_index, value)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (vacuum_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                 :: id
      INTEGER, INTENT(in)                 :: i_index
      INTEGER, INTENT(in)                 :: j_index
      REAL (rprec), INTENT(in)            :: value
      INTEGER, INTENT(in)                 :: eq_comm
      INTEGER, INTENT(inout)              :: state_flags

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (id)

         CASE (vacuum_extcur_id)
            this%extcur(i_index) = value

      END SELECT

      CALL profiler_set_stop_time('vacuum_set_param', start_time)

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the equilibrium type.
!>
!>  @param[in] this A @ref vacuum_class instance.
!>  @returns A string describing the signal type.
!-------------------------------------------------------------------------------
      FUNCTION vacuum_get_type(this)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: vacuum_get_type
      CLASS (vacuum_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      vacuum_get_type = 'vacuum_class'

      CALL profiler_set_stop_time('vacuum_get_type', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the id for a reconstruction parameter.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_param_id.
!>
!>  @param[in] this       A @ref vacuum_class instance.
!>  @param[in] param_name Name of a reconstruction parameter.
!>  @returns The id for a reconstruction parameter.
!-------------------------------------------------------------------------------
      FUNCTION vacuum_get_param_id(this, param_name)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER                          :: vacuum_get_param_id
      CLASS (vacuum_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)    :: param_name

!  local variables
      REAL (rprec)                    :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (TRIM(param_name))

         CASE ('extcur')
            vacuum_get_param_id = vacuum_extcur_id

         CASE DEFAULT
            WRITE (*,1000) TRIM(param_name)
            CALL EXIT(1)

      END SELECT

      CALL profiler_set_stop_time('vacuum_get_param_id', start_time)

1000  FORMAT('ERROR: ',a,' is not a valid parameter.')

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the value of a reconstruction vacuum parameter.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_param_value.
!>
!>  @param[in] this    A @ref vacuum_class instance.
!>  @param[in] id      ID of the parameter.
!>  @param[in] i_index The ith index of the parameter.
!>  @returns The value of the parameter.
!-------------------------------------------------------------------------------
      FUNCTION vacuum_get_param_value(this, id, i_index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                     :: vacuum_get_param_value
      CLASS (vacuum_class), INTENT(in) :: this
      INTEGER, INTENT(in)              :: id
      INTEGER, INTENT(in)              :: i_index

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (id)

         CASE (vacuum_extcur_id)
            vacuum_get_param_value = this%extcur(i_index)

         CASE DEFAULT
            vacuum_get_param_value = 0.0

      END SELECT

      CALL profiler_set_stop_time('vacuum_get_param_value', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the name of a reconstruction vacuum parameter.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_param_name.
!>
!>  @param[in] this A @ref vacuum_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns The name of the parameter.
!-------------------------------------------------------------------------------
      FUNCTION vacuum_get_param_name(this, id)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER(len=data_name_length)  :: vacuum_get_param_name
      CLASS (vacuum_class), INTENT(in) :: this
      INTEGER, INTENT(in)              :: id

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (id)

         CASE (vacuum_extcur_id)
            vacuum_get_param_name = 'extcur'

         CASE DEFAULT
            WRITE (*,*) id
            STOP 'Invalid vacuum parameter id.'

      END SELECT

      CALL profiler_set_stop_time('vacuum_get_param_name', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the magnetic field vector at a position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_B_vec.
!>
!>  @param[in] this   A @ref vacuum_class instance.
!>  @param[in] x_cart Cartesian position to get the magnetic field vector at.
!>  @param[in] cyl    Flag that specifies if the bfield should be returned in
!>                    cartesian or cylindical coordinates.
!>  @returns The magnetic field vector at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION vacuum_get_B_vec(this, x_cart, cyl)
      USE coordinate_utilities, ONLY : cart_to_cyl_vec

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(3)             :: vacuum_get_B_vec
      CLASS (vacuum_class), INTENT(in)       :: this
      REAL (rprec), DIMENSION(3), INTENT(in) :: x_cart
      LOGICAL, INTENT(in)                    :: cyl

!  local variables
      INTEGER                                :: i
      REAL (rprec), DIMENSION(3)             :: b_vec
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      vacuum_get_B_vec = 0.0
      DO i = 1, SIZE(this%extcur)
         CALL bsc_b(coil_group(i), x_cart, b_vec, this%extcur(i))
         vacuum_get_B_vec = vacuum_get_B_vec + b_vec
      END DO

      IF (cyl) THEN
         vacuum_get_B_vec = cart_to_cyl_vec(x_cart, vacuum_get_B_vec)
      END IF

      CALL profiler_set_stop_time('vacuum_get_B_vec', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets the loop integrated magnetic field at a position.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_Int_B_dphi. In this
!>  function, r and theta are spherical coordinates. The r is transformed into a
!>  cylindical radius at some z position. The magnetic field is integrated about
!>  a loop at that r-z position. This computes Int[B*dl]
!>
!>  @param[in] this  A @ref vacuum_class instance.
!>  @param[in] r     Radial position to integrate about.
!>  @param[in] theta Theta angle to integrate about.
!>  @returns The loop integrated magnetic field at x_cart.
!-------------------------------------------------------------------------------
      FUNCTION vacuum_get_Int_B_dphi(this, r, theta)
      USE stel_constants, only: twopi
      USE coordinate_utilities, ONLY : cyl_to_cart_vec, cyl_to_cart

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                     :: vacuum_get_Int_B_dphi
      CLASS (vacuum_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)         :: r
      REAL (rprec), INTENT(in)         :: theta

!  local variables
      REAL (rprec), DIMENSION(3)       :: b_cart
      REAL (rprec), DIMENSION(3)       :: r_cyl
      REAL (rprec), DIMENSION(3)       :: dl
      REAL (rprec)                     :: dphi
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      vacuum_get_Int_B_dphi = 0.0

      r_cyl(1) = r*cos(theta)
      r_cyl(2) = 0.0
      r_cyl(3) = r*sin(theta)

      dphi = 0.001/r_cyl(1)
      DO WHILE (r_cyl(2) .lt. twopi)
         dl = cyl_to_cart_vec(r_cyl, (/ 0.0_dp, dphi, 0.0_dp /))
         b_cart = vacuum_get_B_vec(this, cyl_to_cart(r_cyl), .false.)
         vacuum_get_Int_B_dphi = vacuum_get_Int_B_dphi                         &
     &                         + DOT_PRODUCT(b_cart, dl)
         r_cyl(2) = r_cyl(2) + dphi
      END DO

      CALL profiler_set_stop_time('vacuum_get_Int_B_dphi', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get external current.
!>
!>  This method overrides @ref equilibrium::equilibrium_get_ext_currents. The
!>  array memory containing the external currents is owned by the
!>  @ref vacuum_class. Return a pointer to it.
!>
!>  @param[in]  this           A @ref vacuum_class instance.
!>  @param[out] scale_currents Informs the caller that currents need to be
!>                             scaled.
!>  @returns The external currents.
!-------------------------------------------------------------------------------
      FUNCTION vacuum_get_ext_currents(this, scale_currents)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(:), POINTER :: vacuum_get_ext_currents
      CLASS (vacuum_class), INTENT(in)    :: this
      LOGICAL, INTENT(out)                :: scale_currents

!  local variables
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      vacuum_get_ext_currents => this%extcur
      scale_currents = .false.

      CALL profiler_set_stop_time('vacuum_get_ext_currents', start_time)

      END FUNCTION

!*******************************************************************************
!  QUERY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Checks if a parameter id is a 1d array.
!>
!>  This method overrides @ref equilibrium::equilibrium_is_1d_array.
!>
!>  @param[in] this A @ref vacuum_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns True if the parameter is a 1d array and false if otherwise.
!-------------------------------------------------------------------------------
      FUNCTION vacuum_is_1d_array(this, id)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL                          :: vacuum_is_1d_array
      CLASS (vacuum_class), INTENT(in) :: this
      INTEGER, INTENT(in)              :: id

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (id)

         CASE (vacuum_extcur_id)
            vacuum_is_1d_array = .true.

         CASE DEFAULT
            vacuum_is_1d_array = .false.

      END SELECT

      CALL profiler_set_stop_time('vacuum_is_1d_array', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Checks if a parameter id is a reconstruction parameter.
!>
!>  This method overrides @ref equilibrium::equilibrium_is_recon_param.
!>
!>  @param[in] this A @ref vacuum_class instance.
!>  @param[in] id   ID of the parameter.
!>  @returns True if the parameter is a reconstruction parameter and false if
!>  otherwise.
!-------------------------------------------------------------------------------
      FUNCTION vacuum_is_recon_param(this, id)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL                          :: vacuum_is_recon_param
      CLASS (vacuum_class), INTENT(in) :: this
      INTEGER, INTENT(in)              :: id

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      SELECT CASE (id)

         CASE (vacuum_extcur_id)
            vacuum_is_recon_param = .true.

         CASE DEFAULT
            vacuum_is_recon_param = .false.

      END SELECT

      CALL profiler_set_stop_time('vacuum_is_recon_param', start_time)

      END FUNCTION

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Write out the equilibrium to an output file.
!>
!>  This method overrides @ref equilibrium::equilibrium_write.
!>
!>  @param[in] this A @ref vacuum_class instance.
!>  @param[in] iou  Input/output unit of the output file.
!-------------------------------------------------------------------------------
      SUBROUTINE vacuum_write(this, iou)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (vacuum_class), INTENT(in) :: this
      INTEGER, INTENT(in)              :: iou

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (iou,*)
      WRITE (iou,*) 'Equilibrium Type : vacuum'

      CALL profiler_set_stop_time('vacuum_write', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write the current valid input.
!>
!>  This method overrides @ref equilibrium::equilibrium_write_input. The
!>  boundary and other fixed parameters do not get updated as the reconstruction
!>  progresses. Need to update them manually if in free boundary mode.
!>
!>  @param[in] this         A @ref vacuum_class instance.
!>  @param[in] current_step Step number to append to input filename.
!-------------------------------------------------------------------------------
      SUBROUTINE vacuum_write_input(this, current_step)
      USE vacuum_input

      IMPLICIT NONE

!  Declare Arguments
      CLASS (vacuum_class), INTENT(in) :: this
      INTEGER, INTENT(in)              :: current_step

!  local variables
      CHARACTER (len=path_length)      :: filename
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (filename,1000) TRIM(this%vacuum_file_name), current_step
      CALL vacuum_input_write_namelist(TRIM(filename))

      CALL profiler_set_stop_time('vacuum_write_input', start_time)

1000  FORMAT(a,'_',i0.3)

      END SUBROUTINE

      END MODULE

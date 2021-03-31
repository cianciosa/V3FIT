!-------------------------------------------------------------------------------
!  The @header, @table_section, @table_subsection, @item and @end_table commands
!  are custom defined commands in Doxygen.in. They are defined under ALIASES.
!  For the page created here, the 80 column limit is exceeded. Arguments of
!  aliases are separated by ','. If you intended ',' to be a string you must use an
!  escaped comma '\,'.
!
!>  @page vacuum_namelist_sec Namelist vacuum_main_nli definition
!>
!>  @tableofcontents
!>  @section vacuum_namelist_intro_sec Introduction
!>  This page documents the contents of a vacuum namelist input file. Vacuum
!>  namelist variables are defined in the @fixed_width{vacuum_main_nli} common
!>  block.
!>
!>  @section vacuum_namelist_var_sec Namelist Variables
!>  @header{Input variable, Description, Code Reference}
!>
!>  @table_section{vacuum_filename_sec, Filename variables}
!>     @item{coils_dot_file, File name for the mgrid namelist input file., vacuum_input::coils_dot_file}
!>  @end_table
!>
!>  @table_section{vacuum_size_sec, Array allocation sizes.}
!>     @item{n_extcur, Size of the extcur array, vacuum_input::n_extcur}
!>  @end_table
!>
!>  @table_section{vacuum_eq_var_sec, Equilibrium variables}
!>     @item{extcur, Array of external currents., vacuum_input::extcur}
!>  @end_table
!>
!>  @table_section{vacuum_sr_var_sec, Shifts and rotations of external coils.}
!>     @item{cg_shift_1,        Vector to shift all the coils. (Before rotation),                          @ref vacuum_input::cg_shift_1}
!>     @item{cg_rot_theta,      Spherical polar angle to specify axis of rotation.,                        @ref vacuum_input::cg_rot_theta}
!>     @item{cg_rot_phi,        Spherical azimuthal angle to specify axis of rotation.,                    @ref vacuum_input::cg_rot_phi}
!>     @item{cg_rot_angle,      Angle to rotate about axis of rotation. NB - LEFT HAND
!>                              convention. Put left thumb along axis of rotation\,
!>                              fingers indicate direction of positive rotation.,                          @ref vacuum_input::cg_rot_angle}
!>     @item{cg_rot_xcent,      Position of center of rotation.,                                           @ref vacuum_input::cg_rot_xcent}
!>     @item{l_rot_coil_center, Chooses the center of rotation.
!>                              * True - use current-averaged center of coil-group for center of rotation.
!>                              * False - use position specified in cg_rot_xcent for center of rotation.,  @ref vacuum_input::l_rot_coil_center}
!>     @item{cg_shift_2,        Vector to shift all the coils. (After rotation),                           @ref vacuum_input::cg_shift_2}
!>  @end_table
!>
!>  @section vacuum_namelist_exam_sec Example Files
!>  @code
!>  ! Example vacuum input file.
!>  &vacuum_main_nli
!>  coils_dot_file = input.coils
!>
!>  ! Currents
!>  n_extcur = 5
!>  extcur(1:5) = 1000.0
!>  /
!>  @endcode
!>
!>  @section vacuum_namelist_prog_ref_sec Programmers Reference
!>  Reference material for the coding to implement this namelist is found in the
!>  @ref vacuum_input module.
!-------------------------------------------------------------------------------
!*******************************************************************************
!>  @file vacuum_input.f
!>  @brief Contains module @ref vacuum_input.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  This file contains all the variables and maximum sizes of the inputs for a
!>  vacuum namelist input file. The module contained within does not represent
!>  an object instance. Instead all variables are contained in a global context.
!>  This is required due to limitations of FORTRAN 95 and namelist inputs. Any
!>  information needed by a vacuum equilibrium should be copied to a
!>  @ref vacuum_equilibrium object. All non-parameters are inputs to the
!>  namelist input file.
!>
!>  @ref vacuum_namelist_sec "Namelist vacuum_main_nli definition"
!*******************************************************************************
      MODULE vacuum_input
      USE stel_kinds
      USE data_parameters
      USE profiler

      IMPLICIT NONE

!*******************************************************************************
!  v3fit input module parameters
!*******************************************************************************
!>  Maximum number of diagnostic signals.
      INTEGER, PARAMETER :: vacuum_max_currents = 1000

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) vacuum_namelist_class
!
!*******************************************************************************
!>  File name for vacuum namelist input.
      CHARACTER (len=path_length) :: coils_dot_file = ''

!>  Number of external currents.
      INTEGER                                      :: n_extcur = 0
!>  External currents.
      REAL (rprec), DIMENSION(vacuum_max_currents) :: extcur = 0.0

!  Variables for shifts and rotations of coil_groups of coils_dot file 
!>  Vector to shift all the coils. (Before rotation)
      REAL (rprec), DIMENSION(vacuum_max_currents,3) ::                        &
     &   cg_shift_1 = 0.0
!>  Spherical polar angle to specify axis of rotation.
      REAL (rprec), DIMENSION(vacuum_max_currents)   ::                        &
     &   cg_rot_theta = 0.0
!>  Spherical azimuthal angle to specify axis of rotation.
      REAL (rprec), DIMENSION(vacuum_max_currents)   ::                        &
     &   cg_rot_phi = 0.0
!>  Angle to rotate about axis of rotation. NB - LEFT HAND convention. Put left
!>  thumb along axis of rotation, fingers indicate direction of positive
!>  rotation.
      REAL (rprec), DIMENSION(vacuum_max_currents)   ::                        &
     &   cg_rot_angle = 0.0
!>  Position of center of rotation.
      REAL (rprec), DIMENSION(vacuum_max_currents,3) ::                        &
     &   cg_rot_xcent = 0.0
!>  * True - use current-averaged center of coil-group for center of rotation.
!>  * False - use position specified in cg_rot_xcent for center of rotation.
      LOGICAL, DIMENSION(vacuum_max_currents)        ::                        &
     &   l_rot_coil_center = .false.
!>  Vector to shift all the coils. (After rotation)
      REAL (rprec), DIMENSION(vacuum_max_currents,3) ::                        &
     &   cg_shift_2 = 0.0

!  Declare Namelist
      NAMELIST/vacuum_main_nli/                                                &
     &   coils_dot_file,                                                       &
!  External currents
     &   n_extcur, extcur,                                                     &
!  Variables for shifts and rotations of coil_groups of coils_dot file
     &   cg_shift_1, cg_rot_theta, cg_rot_phi, cg_rot_angle,                   &
     &   cg_rot_xcent, l_rot_coil_center, cg_shift_2

      CONTAINS

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Reads the namelist input file.
!>
!>  Reads the namelist input file.
!>
!>  @param[in] namelist_file The file name of the namelist input file.
!-------------------------------------------------------------------------------
      SUBROUTINE vacuum_input_read_namelist(namelist_file)
      USE safe_open_mod
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=*), INTENT(in) :: namelist_file

!  local variables
      INTEGER                       :: iou_vacnli, status
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      iou_vacnli = 0
      status = 0

      CALL safe_open(iou_vacnli, status, TRIM(namelist_file),                  &
     &               'old', 'formatted')
      CALL assert_eq(0, status, 'vacuum_input_read_namelist' //                &
     &   ': Safe_open of ' // TRIM(namelist_file) // ' failed')

!  Read the namelist input file.
      READ (iou_vacnli, nml=vacuum_main_nli)
      CLOSE (iou_vacnli, iostat=status)
      CALL assert_eq(0, status, 'vacuum_input_read_namelist' //                &
     &   ': Error closing ' // TRIM(namelist_file) // ' failed')

      CALL profiler_set_stop_time('vacuum_input_read_namelist',                &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Writes the namelist input file.
!>
!>  Writes the namelist input file.
!>
!>  @param[in] namelist_file The file name of the namelist input file.
!-------------------------------------------------------------------------------
      SUBROUTINE vacuum_input_write_namelist(namelist_file)
      USE safe_open_mod
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=*), INTENT(in) :: namelist_file

!  local variables
      INTEGER                       :: iou_vacnli, status
      REAL (rprec)                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      iou_vacnli = 0
      status = 0

      CALL safe_open(iou_vacnli, status, TRIM(namelist_file),                  &
     &               'old', 'formatted')
      CALL assert_eq(0, status, 'vacuum_input_write_namelist' //               &
     &   ': Safe_open of ' // TRIM(namelist_file) // ' failed')

!  Read the namelist input file.
      WRITE (iou_vacnli, nml=vacuum_main_nli)
      CLOSE (iou_vacnli, iostat=status)
      CALL assert_eq(0, status, 'vacuum_input_write_namelist' //               &
     &   ': Error closing ' // TRIM(namelist_file) // ' failed')

      CALL profiler_set_stop_time('vacuum_input_write_namelist',               &
     &                            start_time)

      END SUBROUTINE

      END MODULE

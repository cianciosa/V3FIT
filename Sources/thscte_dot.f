!-------------------------------------------------------------------------------
!  The @fixed_width, @begin_table, @item2 and @end_table commands are custom
!  defined commands in Doxygen.in. They are defined under ALIASES. For the page
!  created here, the 80 column limit is exceeded. Arguments of aliases are
!  separated by ','. If you intended ',' to be a string you must use an escaped
!  comma '\,'.
!
!>  @page thomson_dot_sec Thomson Scattering Diagnostic Dot File
!>
!>  @tableofcontents
!>  @section thomson_dot_intro_sec Introduction
!>  Diagnostic dot files specify geometric and auxiliary information about each
!>  diagnostic. Parsing of a diagnostic dot file begins by searching for a valid
!>  keyword. All lines before a valid keyword is reached are ignored allowing
!>  for comments and other information to be inserted in the file. Once a
!>  keyword is reached, the parsing becomes strict. The diagnostic description
!>  format is dependant on the specific keyword. All diagnostic dot files must
!>  end with an @fixed_width{end_of_file} keyword.
!>
!>  This document contains information about the thomson scattering point
!>  specifications. Points are specified in a structured text file. This file
!>  is read in by the @ref v3fit_input::thscte_dot_filename variable of the name
!>  list input file. Points are specified as a verticies in either meters or
!>  radians.
!>
!>  @section thomson_dot_key_sec Keywords
!>  @begin_table
!>  @item2{@fixed_width{thscte_XYZ}\n @fixed_width{thscte_XYZ_te},           A thomson scattering point specified in Cartesian coordinates measuring electron temperature.}
!>  @item2{@fixed_width{thscte_RPhiDegZ}\n @fixed_width{thscte_RPhiDegZ_te}, A thomson scattering point specified in Cylindrical coordinates measuring electron temperature.}
!>  @item2{@fixed_width{thscte_XYZ_ne},                                      A thomson scattering point specified in Cartesian coordinates measuring electron denisty.}
!>  @item2{@fixed_width{thscte_RPhiDegZ_ne},                                 A thomson scattering point specified in Cylindrical coordinates measuring electron denisty.}
!>  @item2{@fixed_width{thscte_XYZ_pres},                                    A thomson scattering point specified in Cartesian coordinates measuring plasma pressure.}
!>  @item2{@fixed_width{thscte_RPhiDegZ_pres},                               A thomson scattering point specified in Cylindrical coordinates measuring plasma pressure.}
!>  @end_table
!>
!>  @section thomson_dot_chord_spec_sec Chord Specification
!>  All thomson scattering points are specified in the following manner.
!>
!>  @fixed_width{key_word\n}
!>  @fixed_width{name(STRING len:@ref data_parameters::data_short_name_length)\n}
!>  @fixed_width{x1(REAL) y1(REAL) z1(REAL)\n}
!>
!>  @section thomson_dot_exam_sec Example File
!>  @code
!>  !  A thomson scattering point specified in Cartesian coordinates measuring electron temperature.
!>  thscte_XYZ_te
!>  tsc001
!>  1.0 0.0 0.0
!>
!>  !  A soft x-ray chord specified in Cylindrical coordinates measuring electron denisty.
!>  thscte_RPhiDegZ_ne
!>  tsc002
!>  1.0 0.0 0.0
!>
!>  !  All files must end with
!>  end_of_file
!>  @endcode
!>
!>  @section thomson_dot_prog_ref_sec Programmers Reference
!>  Reference material for the coding to parse these files can be found in the
!>  @ref signal_dot and @ref thscte_dot modules.
!-------------------------------------------------------------------------------
!*******************************************************************************
!>  @file thscte_dot.f
!>  @brief Contains module @ref thscte_dot
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Module for opening and reading a 'thscte.' file. The file format for these
!>  files are documented in @ref thomson_dot_sec.
!*******************************************************************************
      MODULE thscte_dot

      USE stel_kinds
      USE stel_constants
      USE signal_dot

      IMPLICIT NONE

      CONTAINS
!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Read an thomson scattering diagnostic dot file.
!>
!>  Parses the structure of a dignostic dot file and constructs a @ref thomson
!>  object. Each chord is parsed by @ref thscte_dot_parse_chord.
!>
!>  @param[in]    thscte_file     Name of the thscte. file.
!>  @param[inout] signals         Array of pointers to the a signal
!>  @param[inout] signals_created Number valid signals in the array, or index of
!>                                the last signal created.
!>  @param[in]    observed        Array of observed values.
!>  @param[in]    sigma           Array of observed sigma values
!>  @param[in]    weight          Array of signal weight values
!>  @param[inout] first_index     Index of the first instance of an intpol
!>                                signal.
!-------------------------------------------------------------------------------
      SUBROUTINE thscte_dot_read(thscte_file, signals, signals_created,        &
     &                           observed, sigma, weight, first_index)
      USE signal
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=path_length), INTENT(in)            :: thscte_file
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      INTEGER, INTENT(inout) :: signals_created
      REAL (rprec), DIMENSION(:), INTENT(in)             :: observed
      REAL (rprec), DIMENSION(:), INTENT(in)             :: sigma
      REAL (rprec), DIMENSION(:), INTENT(in)             :: weight
      INTEGER, INTENT(inout)                             :: first_index

!  local parameters
!  n_thscted_keyword    integer - number of thscted_keyword s.
      INTEGER, PARAMETER                :: n_thscted_keyword = 9

!  local variables
!  thscted_keyword   character array - keywords for various thscte input types
      CHARACTER(len=data_name_length),                                         &
     &   DIMENSION(1:n_thscted_keyword) :: thscted_keyword
      TYPE(signal_dot_file)             :: thscte_dot_file_ref
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Initialize the keywords and keyword lengths
      thscted_keyword(1) = 'thscte_XYZ'
      thscted_keyword(2) = 'thscte_RPhiDegZ'
      thscted_keyword(3) = 'thscte_XYZ_te'
      thscted_keyword(4) = 'thscte_RPhiDegZ_te'
      thscted_keyword(5) = 'thscte_XYZ_ne'
      thscted_keyword(6) = 'thscte_RPhiDegZ_ne'
      thscted_keyword(7) = 'thscte_XYZ_pres'
      thscted_keyword(8) = 'thscte_RPhiDegZ_pres'
      thscted_keyword(9) = 'end_of_file'

!  Open up the 'thscte.' file
      thscte_dot_file_ref = signal_dot_open(TRIM(thscte_file),                 &
     &                                      'thscte')

!  Infinite Loop
!  Character variable line should be defined on entry
      DO
!  Branch on the keyword
         SELECT CASE (signal_dot_read_keyword(thscte_dot_file_ref,             &
     &                                        thscted_keyword))

            CASE DEFAULT ! This case should never fire.
               EXIT ! Exit out of infinte loop.

            CASE ('end_of_file')
               EXIT ! Exit out of infinte loop.

            CASE ('thscte_XYZ','thscte_XYZ_te')
               CALL thscte_dot_parse_chord(thscte_dot_file_ref,                &
     &                                     'XYZ', 't',                         &
     &                                     signals, signals_created,           &
     &                                     observed, sigma, weight,            &
     &                                     first_index)

            CASE ('thscte_RPhiDegZ','thscte_RPhiDegZ_te')
               CALL thscte_dot_parse_chord(thscte_dot_file_ref,                &
     &                                     'RPHiDegZ', 't',                    &
     &                                     signals, signals_created,           &
     &                                     observed, sigma, weight,            &
     &                                     first_index)

            CASE ('thscte_XYZ_ne')
               CALL thscte_dot_parse_chord(thscte_dot_file_ref,                &
     &                                     'XYZ', 'd',                         &
     &                                     signals, signals_created,           &
     &                                     observed, sigma, weight,            &
     &                                     first_index)

            CASE ('thscte_RPhiDegZ_ne')
               CALL thscte_dot_parse_chord(thscte_dot_file_ref,                &
     &                                     'RPHiDegZ', 'd',                    &
     &                                     signals, signals_created,           &
     &                                     observed, sigma, weight,            &
     &                                     first_index)

            CASE ('thscte_XYZ_pres')
               CALL thscte_dot_parse_chord(thscte_dot_file_ref,                &
     &                                     'XYZ', 'p',                         &
     &                                     signals, signals_created,           &
     &                                     observed, sigma, weight,            &
     &                                     first_index)

            CASE ('thscte_RPhiDegZ_pres')
               CALL thscte_dot_parse_chord(thscte_dot_file_ref,                &
     &                                     'RPHiDegZ', 'p',                    &
     &                                     signals, signals_created,           &
     &                                     observed, sigma, weight,            &
     &                                     first_index)

         END SELECT

      END DO

!  Close the 'thscte.' file
      CALL signal_dot_close(thscte_dot_file_ref)

      CALL profiler_set_stop_time('thscte_dot_read', start_time)

      END SUBROUTINE

!*******************************************************************************
!  PARSING SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Parse thomson scattering diagnostic chord.
!>
!>  Parses a signle chord. Chords are laid out as,
!>
!>  * chord_name
!>  * Starting point as three reals.
!>  * Ending point as three reals.
!>
!>  @param[in]    thscte_dot_file_ref Reference of the thscte file.
!>  @param[in]    coordinate_type     Specifies the cartesian or cylindrical
!>                                    coordinate type.
!>  @param[in]    point_type          Specifies a 't'emperature, 'd'ensity or
!>                                    'p'ressure point.
!>  @param[inout] signals             Array of pointers to the a signal
!>  @param[inout] signals_created     Number valid signals in the array, or
!>                                    index of the last signal created.
!>  @param[in]    observed            Array of observed values.
!>  @param[in]    sigma               Array of observed sigma values
!>  @param[in]    weight              Array of signal weight values
!>  @param[inout] first_index         Index of the first instance of an intpol
!>                                    signal.
!>  @param[in]    scale_index         Index of the signal scale factor.
!-------------------------------------------------------------------------------
      SUBROUTINE thscte_dot_parse_chord(thscte_dot_file_ref,                   &
     &                                  coordinate_type,                       &
     &                                  point_type,                            &
     &                                  signals,                               &
     &                                  signals_created,                       &
     &                                  observed, sigma, weight,               &
     &                                  first_index)
      USE thomson
      USE v3fit_input, only: v3fit_max_diagnostics,                            &
     &                       v3fit_input_find_scale_index,                     &
     &                       v3fit_input_find_offset_index
      USE v3_utilities, only: err_fatal
      IMPLICIT NONE

!  Declare Arguments
      TYPE(signal_dot_file), INTENT(inout)   :: thscte_dot_file_ref
      CHARACTER (len=*)                      :: coordinate_type
      CHARACTER (len=1)                      :: point_type
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      INTEGER, INTENT(inout)                 :: signals_created
      REAL (rprec), DIMENSION(:), INTENT(in) :: observed
      REAL (rprec), DIMENSION(:), INTENT(in) :: sigma
      REAL (rprec), DIMENSION(:), INTENT(in) :: weight
      INTEGER, INTENT(inout)                 :: first_index

!  local variables
      REAL (rprec), DIMENSION(3)             :: xcart
      CHARACTER (len=data_short_name_length) :: point_name
      CLASS (thomson_class), POINTER         :: thomson_obj => null()
      CHARACTER (len=data_short_name_length) :: units = ''
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Check if there are two many signals.
      IF (signals_created + 1 .gt. v3fit_max_diagnostics) THEN
         CALL err_fatal('thscte_dot_parse_chord: created signals' //           &
     &                  ' exceeds v3fit_max_diagnostics')
      END IF

!  Get the position and name of the point.
      CALL signal_dot_parse_chord(thscte_dot_file_ref,                         &
     &                            coordinate_type,                             &
     &                            point_name,                                  &
     &                            xcart)

      SELECT CASE (point_type)

         CASE ('t')
            thomson_obj => thomson_te_class(xcart)
            units = 'ev'

         CASE ('d')
            thomson_obj => thomson_ne_class(xcart)
            units = 'm^-3'

         CASE ('p')
            thomson_obj => thomson_p_class(xcart)
            units = 'Pa'

      END SELECT

      CALL signal_construct( thomson_obj, point_name, point_name,              &
     &        units, observed(signals_created + 1),                            &
     &        sigma(signals_created + 1), weight(signals_created + 1),         &
     &        v3fit_input_find_scale_index(signals_created + 1),               &
     &        v3fit_input_find_offset_index(signals_created + 1))

      signals(signals_created + 1)%p => thomson_obj

      signals_created = signals_created + 1

!  At lease one thomson scattering signal was made. Set the first index. This
!  should only be run once.
      IF (first_index .eq. -1) THEN
         first_index = signals_created
      END IF

      CALL profiler_set_stop_time('thscte_dot_parse_chord', start_time)

      END SUBROUTINE

      END MODULE

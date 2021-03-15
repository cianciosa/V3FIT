!-------------------------------------------------------------------------------
!  The @fixed_width, @begin_table, @item2 and @end_table commands are custom
!  defined commands in Doxygen.in. They are defined under ALIASES. For the page
!  created here, the 80 column limit is exceeded. Arguments of aliases are
!  separated by ','. If you intended ',' to be a string you must use an escaped
!  comma '\,'.
!
!>  @page ece_dot_sec ECE Diagnostic Dot File
!>
!>  @tableofcontents
!>  @section ece_dot_intro_sec Introduction
!>  Diagnostic dot files specify geometric and auxiliary information about each
!>  diagnostic. Parsing of a diagnostic dot file begins by searching for a valid
!>  keyword. All lines before a valid keyword is reached are ignored allowing
!>  for comments and other information to be inserted in the file. Once a
!>  keyword is reached, the parsing becomes strict. The diagnostic description
!>  format is dependant on the specific keyword. All diagnostic dot files must
!>  end with an @fixed_width{end_of_file} keyword.
!>
!>  This document contains information about the ECE chord specifications.
!>  Chords are specified in a structured text file. This file is read in by the
!>  @ref v3fit_input::ece_dot_filename variable of the name list input file.
!>  Chords are specified as two verticies in either meters or radians.
!>
!>  @section ece_dot_key_sec Keywords
!>  @begin_table
!>  @item2{@fixed_width{ece_chord_XYZ},          An ECE chord specified in Cartesian coordinates.}
!>  @item2{@fixed_width{ece_chord_RPhiDegZ},     An ECE chord specified in Cylindrical coordinates.}
!>  @end_table
!>
!>  @section ece_dot_chord_spec_sec Chord Specification
!>  All ECE chords are specified in the following manner.
!>
!>  @fixed_width{key_word\n}
!>  @fixed_width{name(STRING len:@ref data_parameters::data_short_name_length)\n}
!>  @fixed_width{x1(REAL) y1(REAL) z1(REAL)\n}
!>  @fixed_width{x2(REAL) y2(REAL) z2(REAL)\n}
!>  @fixed_width{frequency(REAL)}
!>
!>  @section ece_dot_exam_sec Example File
!>  @code
!>  !  An ECE chord specified in Cartesian coordinates.
!>  ece_chord_XYZ
!>  ece001
!>  1.0 0.0 0.0
!>  0.5 0.0 0.0
!>  0.5
!>
!>  !  An ECE chord specified in Cylindrical coordinates.
!>  ece_chord_RPhiDegZ
!>  ece002
!>  1.0 0.0 0.0
!>  0.5 0.0 0.0
!>  0.5
!>
!>  !  All files must end with
!>  end_of_file
!>  @endcode
!>
!>  @section ece_dot_prog_ref_sec Programmers Reference
!>  Reference material for the coding to parse these files can be found in the
!>  @ref signal_dot and @ref ece_dot modules.
!-------------------------------------------------------------------------------
!*******************************************************************************
!>  @file ece_dot.f
!>  @brief Contains module @ref ece_dot
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Module for opening and reading a 'ece.' file. The file format for these
!>  files are documented in @ref ece_dot_sec.
!*******************************************************************************

      MODULE ece_dot

      USE stel_kinds
      USE stel_constants
      USE signal_dot

      IMPLICIT NONE

      CONTAINS
!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Read an ECE diagnostic dot file.
!>
!>  Parses the structure of a dignostic dot file and constructs a @ref ece
!>  object. Each chord is parsed by @ref ece_dot_parse_chord.
!>
!>  @param[in]    ece_file        Name of the ece. file.
!>  @param[inout] signals         Array of pointers to the a signal
!>  @param[inout] signals_created Number valid signals in the array, or index of
!>                                the last signal created.
!>  @param[in]    observed        Array of observed values.
!>  @param[in]    sigma           Array of observed sigma values
!>  @param[in]    weight          Array of signal weight values
!>  @param[inout] first_index     Index of the first instance of an intpol
!>                                signal.
!>  @todo FIXME: Remove the _new when removing the old code.
!-------------------------------------------------------------------------------
      SUBROUTINE ece_dot_read(ece_file, signals, signals_created,              &
     &                        observed, sigma, weight, first_index)
      USE signal
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=path_length), INTENT(in)            :: ece_file
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      INTEGER, INTENT(inout) :: signals_created
      REAL (rprec), DIMENSION(:), INTENT(in)             :: observed
      REAL (rprec), DIMENSION(:), INTENT(in)             :: sigma
      REAL (rprec), DIMENSION(:), INTENT(in)             :: weight
      INTEGER, INTENT(inout)                             :: first_index

!  local parameters
!  n_ece_keyword        integer - number of ece_keyword s.
      INTEGER, PARAMETER :: n_ece_keyword = 3

!  local variables
!  ece_keyword    character array - keywords for various ece input types
      CHARACTER(len=data_name_length), DIMENSION(1:n_ece_keyword) ::           &
     &   ece_keyword
      TYPE(signal_dot_file)                 :: ece_dot_file_ref
      INTEGER                               :: current_profile
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Initialize the keywords
      ece_keyword(1) = 'ece_chord_XYZ'
      ece_keyword(2) = 'ece_chord_RPhiDegZ'
      ece_keyword(3) = 'end_of_file'

!  Open up the 'ece.' file
      ece_dot_file_ref = signal_dot_open(TRIM(ece_file), 'ece')

!  Infinite Loop
!  Character variable line should be defined on entry
      DO
!  Branch on the keyword
         SELECT CASE (signal_dot_read_keyword(ece_dot_file_ref,                &
     &                                        ece_keyword))

            CASE DEFAULT ! This case should never fire.
               EXIT ! Exit out of infinte loop.

            CASE ('end_of_file')
               EXIT ! Exit out of infinte loop.

            CASE ('ece_chord_XYZ')
               CALL ece_dot_parse_chord(ece_dot_file_ref, 'XYZ',               &
     &                                  signals, signals_created,              &
     &                                  observed, sigma, weight,               &
     &                                  first_index)

            CASE ('ece_chord_RPhiDegZ')
               CALL ece_dot_parse_chord(ece_dot_file_ref, 'RPHiDegZ',          &
     &                                  signals, signals_created,              &
     &                                  observed, sigma, weight,               &
     &                                  first_index)

         END SELECT

      END DO

!  Close the 'ece.' file
      CALL signal_dot_close(ece_dot_file_ref)

      CALL profiler_set_stop_time('ece_dot_read', start_time)

      END SUBROUTINE

!*******************************************************************************
!  PARSING SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Parse ECE diagnostic chord.
!>
!>  Parses a single chord. Chords are laid out as,
!>
!>  * chord_name
!>  * Resonance as a single real.
!>  * Starting point as three reals.
!>  * Ending point as three reals.
!>
!>  @param[in]    ece_dot_file_ref Reference to the ece. file.
!>  @param[in]    coordinate_type  Specifies the cartesian or cylindrical
!>                                 coordinate type.
!>  @param[inout] signals          Array of pointers to the a signal
!>  @param[inout] signals_created  Number valid signals in the array, or index
!>                                 of the last signal created.
!>  @param[in]    observed         Array of observed values.
!>  @param[in]    sigma            Array of observed sigma values
!>  @param[in]    weight           Array of signal weight values
!>  @param[inout] first_index      Index of the first instance of an intpol
!>                                 signal.
!-------------------------------------------------------------------------------
      SUBROUTINE ece_dot_parse_chord(ece_dot_file_ref, coordinate_type,        &
     &                               signals, signals_created, observed,       &
     &                               sigma, weight, first_index)
      USE ece
      USE v3fit_input, only: v3fit_max_diagnostics,                            &
     &                       v3fit_input_find_scale_index,                     &
     &                       v3fit_input_find_offset_index
      USE v3_utilities, only: err_fatal

      IMPLICIT NONE

!  Declare Arguments
      TYPE (signal_dot_file), INTENT(inout)  :: ece_dot_file_ref
      CHARACTER (len=*), INTENT(in)          :: coordinate_type
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      INTEGER, INTENT(inout)                 :: signals_created
      REAL (rprec), DIMENSION(:), INTENT(in) :: observed
      REAL (rprec), DIMENSION(:), INTENT(in) :: sigma
      REAL (rprec), DIMENSION(:), INTENT(in) :: weight
      INTEGER, INTENT(inout)                 :: first_index

!  local variables
      CLASS (ece_class), POINTER             :: ece_obj
      REAL(rprec), DIMENSION(3)              :: xcart_i
      REAL(rprec), DIMENSION(3)              :: xcart_f
      CHARACTER (len=data_short_name_length) :: chord_name
      REAL (rprec)                           :: resonance
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Check if there are two many signals.
      IF (signals_created + 1 .gt. v3fit_max_diagnostics) THEN
         CALL err_fatal('ece_dot_parse_chord_new: created signals' //          &
     &                  ' exceeds v3fit_max_diagnostics')
      END IF

!  Get the start, end and name of chord.
      CALL signal_dot_parse_chord(ece_dot_file_ref, coordinate_type,           &
     &                            chord_name, xcart_i, xcart_f)

!  Default the geometric factor to 1.
      resonance = signal_dot_parse_real(ece_dot_file_ref,                      &
     &               'Expected resonance for ECE chord')

      ece_obj => ece_class(xcart_i, xcart_f, resonance)
      CALL signal_construct_new(ece_obj, chord_name, chord_name, 'arb',        &
     &        observed(signals_created + 1),                                   &
     &        sigma(signals_created + 1), weight(signals_created + 1),         &
     &        v3fit_input_find_scale_index(signals_created + 1),               &
     &        v3fit_input_find_offset_index(signals_created + 1))

      signals(signals_created + 1)%p => ece_obj
      signals_created = signals_created + 1

!  At lease one ece signal was made. Set the first index. This should only be
!  run once.
      IF (first_index .eq. -1) THEN
         first_index = signals_created
      END IF

      CALL profiler_set_stop_time('ece_dot_parse_chord', start_time)

      END SUBROUTINE

      END MODULE

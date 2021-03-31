!-------------------------------------------------------------------------------
!  The @fixed_width, @begin_table, @item2 and @end_table commands are custom
!  defined commands in Doxygen.in. They are defined under ALIASES. For the page
!  created here, the 80 column limit is exceeded. Arguments of aliases are
!  separated by ','. If you intended ',' to be a string you must use an escaped
!  comma '\,'.
!
!>  @page sxrem_dot_sec Soft X-Ray Diagnostic Dot File
!>
!>  @tableofcontents
!>  @section sxrem_dot_intro_sec Introduction
!>  Diagnostic dot files specify geometric and auxiliary information about each
!>  diagnostic. Parsing of a diagnostic dot file begins by searching for a valid
!>  keyword. All lines before a valid keyword is reached are ignored allowing
!>  for comments and other information to be inserted in the file. Once a
!>  keyword is reached, the parsing becomes strict. The diagnostic description
!>  format is dependant on the specific keyword. All diagnostic dot files must
!>  end with an @fixed_width{end_of_file} keyword.
!>
!>  This document contains information about the soft x-ray chord
!>  specifications. Chords are specified in a structured text file. This file is
!>  read in by the @ref v3fit_input::sxrch_dot_filename variable of the name
!>  list input file. Chords are specified as two verticies in either meters or
!>  radians.
!>
!>  @section sxrem_dot_key_sec Keywords
!>  @begin_table
!>  @item2{@fixed_width{sxr_chord_XYZ},                 A soft x-ray chord specified in Cartesian coordinates.}
!>  @item2{@fixed_width{sxr_chord_RPhiDegZ},            A soft x-ray chord specified in Cylindrical coordinates.}
!>  @item2{@fixed_width{sxr_chord_XYZ_geo},             A soft x-ray chord specified in Cartesian coordinates with the geometric factor.}
!>  @item2{@fixed_width{sxr_chord_RPhiDegZ_geo},        A soft x-ray chord specified in Cylindrical coordinates with the geometric factor.}
!>  @item2{@fixed_width{sxr_chord_XYZ_xics_emiss},      A XICS emission chord specified in Cartesian coordinates.}
!>  @item2{@fixed_width{sxr_chord_RPhiDegZ_xics_emiss}, A XICS emission chord specified in Cylindrical coordinates.}
!>  @item2{@fixed_width{sxr_chord_XYZ_xics_ti},         A XICS electron temperature chord specified in Cartesian coordinates.}
!>  @item2{@fixed_width{sxr_chord_RPhiDegZ_xics_ti},    A XICS electron temperature chord specified in Cylindrical coordinates.}
!>  @item2{@fixed_width{sxr_new_profile},        Mark the start of chords that begin the next profile.}
!>  @end_table
!>
!>  @section sxrem_dot_chord_spec_sec Chord Specification
!>  All soft x-ray chords are specified in the following manner.
!>
!>  @fixed_width{key_word\n}
!>  @fixed_width{name(STRING len:@ref data_parameters::data_short_name_length)\n}
!>  @fixed_width{x1(REAL) y1(REAL) z1(REAL)\n}
!>  @fixed_width{x2(REAL) y2(REAL) z2(REAL)\n}
!>
!>  Keywords that end with _geo, contain an extra geomtric factor.
!>
!>  @fixed_width{key_word_geo\n}
!>  @fixed_width{name(STRING len:@ref data_parameters::data_short_name_length)\n}
!>  @fixed_width{x1(REAL) y1(REAL) z1(REAL)\n}
!>  @fixed_width{x2(REAL) y2(REAL) z2(REAL)\n}
!>  @fixed_width{geo(REAL)\n}
!>
!>  @section sxrem_dot_exam_sec Example File
!>  @code
!>  !  A soft x-ray chord specified in Cartesian coordinates.
!>  sxr_chord_XYZ
!>  sxr001
!>  1.0 0.0 0.0
!>  0.5 0.0 0.0
!>
!>  !  A soft x-ray chord specified in Cylindrical coordinates.
!>  sxr_chord_RPhiDegZ
!>  sxr002
!>  1.0 0.0 0.0
!>  0.5 0.0 0.0
!>  
!>  !  All files must end with
!>  end_of_file
!>  @endcode
!>
!>  @subsection sxrem_dot_exam_two_color_sec Two Color Example File
!>  @code
!>  !  A soft x-ray chord specified in Cartesian coordinates.
!>  sxr_chord_XYZ
!>  sxr001
!>  1.0 0.0 0.0
!>  0.5 0.0 0.0
!>
!>  sxr_new_profile
!>
!>  !  A soft x-ray chord specified in Cylindrical coordinates.
!>  sxr_chord_RPhiDegZ
!>  sxr002
!>  1.0 0.0 0.0
!>  0.5 0.0 0.0
!>
!>  !  All files must end with
!>  end_of_file
!>  @endcode
!>
!>  @section sxrem_dot_prog_ref_sec Programmers Reference
!>  Reference material for the coding to parse these files can be found in the
!>  @ref signal_dot and @ref sxrch_dot modules.
!-------------------------------------------------------------------------------
!*******************************************************************************
!>  @file sxrch_dot.f
!>  @brief Contains module @ref sxrch_dot
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Module for opening and reading a 'sxrch.' file. The file format for these
!>  files are documented in @ref sxrem_dot_sec.
!*******************************************************************************

      MODULE sxrch_dot

      USE stel_kinds
      USE stel_constants
      USE signal_dot

      IMPLICIT NONE

      CONTAINS
!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Read a soft x-ray diagnostic dot file.
!>
!>  Parses the structure of a dignostic dot file and constructs a @ref sxrem
!>  object. Each chord is parsed by @ref sxrch_dot_parse_chord.
!>
!>  @param[in]    sxrch_file      Name of the sxrch. file.
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
      SUBROUTINE sxrch_dot_read(sxrch_file, signals, signals_created,          &
     &                          observed, sigma, weight, first_index)
      USE signal
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=path_length), INTENT(in)            :: sxrch_file
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      INTEGER, INTENT(inout) :: signals_created
      REAL (rprec), DIMENSION(:), INTENT(in)             :: observed
      REAL (rprec), DIMENSION(:), INTENT(in)             :: sigma
      REAL (rprec), DIMENSION(:), INTENT(in)             :: weight
      INTEGER, INTENT(inout)                             :: first_index

!  local parameters
!  n_sxrchd_keyword        integer - number of sxrchd_keyword s.
      INTEGER, PARAMETER :: n_sxrchd_keyword = 10

!  local variables
!  sxrchd_keyword    character array - keywords for various sxrch input types
      CHARACTER(len=data_name_length), DIMENSION(1:n_sxrchd_keyword) ::        &
     &   sxrchd_keyword
      TYPE(signal_dot_file)                 :: sxrch_dot_file_ref
      INTEGER                               :: current_profile
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Initialize the keywords
      sxrchd_keyword(1) = 'sxr_chord_XYZ'
      sxrchd_keyword(2) = 'sxr_chord_RPhiDegZ'
      sxrchd_keyword(3) = 'sxr_chord_XYZ_geo'
      sxrchd_keyword(4) = 'sxr_chord_RPhiDegZ_geo'
      sxrchd_keyword(5) = 'sxr_chord_XYZ_xics_emiss'
      sxrchd_keyword(6) = 'sxr_chord_RPhiDegZ_xics_emiss'
      sxrchd_keyword(7) = 'sxr_chord_XYZ_xics_ti'
      sxrchd_keyword(8) = 'sxr_chord_RPhiDegZ_xics_ti'
      sxrchd_keyword(9) = 'sxr_new_profile'
      sxrchd_keyword(10) = 'end_of_file'

!  Open up the 'sxrch.' file
      sxrch_dot_file_ref = signal_dot_open(TRIM(sxrch_file), 'sxrch')

!  Start with the first profile
      current_profile = 1

!  Infinite Loop
!  Character variable line should be defined on entry
      DO
!  Branch on the keyword
         SELECT CASE (signal_dot_read_keyword(sxrch_dot_file_ref,              &
     &                                        sxrchd_keyword))

            CASE DEFAULT ! This case should never fire.
               EXIT ! Exit out of infinte loop.

            CASE ('end_of_file')
               EXIT ! Exit out of infinte loop.

            CASE ('sxr_chord_XYZ', 'sxr_chord_XYZ_xics_emiss')
               CALL sxrch_dot_parse_chord(sxrch_dot_file_ref,                  &
     &                                    'XYZ', signals,                      &
     &                                    signals_created,                     &
     &                                    observed, sigma, weight,             &
     &                                    current_profile, first_index,        &
     &                                    .false., .false.)

            CASE ('sxr_chord_RPhiDegZ', 'sxr_chord_RPhiDegZ_xics_emiss')
               CALL sxrch_dot_parse_chord(sxrch_dot_file_ref,                  &
     &                                    'RPHiDegZ', signals,                 &
     &                                    signals_created,                     &
     &                                    observed, sigma, weight,             &
     &                                    current_profile, first_index,        &
     &                                    .false., .false.)

            CASE ('sxr_chord_XYZ_geo')
               CALL sxrch_dot_parse_chord(sxrch_dot_file_ref,                  &
     &                                    'XYZ', signals,                      &
     &                                    signals_created,                     &
     &                                    observed, sigma, weight,             &
     &                                    current_profile, first_index,        &
     &                                    .true., .false.)

            CASE ('sxr_chord_RPhiDegZ_geo')
               CALL sxrch_dot_parse_chord(sxrch_dot_file_ref,                  &
     &                                    'RPHiDegZ', signals,                 &
     &                                    signals_created,                     &
     &                                    observed, sigma, weight,             &
     &                                    current_profile, first_index,        &
     &                                    .true., .false.)

            CASE ('sxr_chord_XYZ_xics_ti')
               CALL sxrch_dot_parse_chord(sxrch_dot_file_ref,                  &
     &                                    'XYZ', signals,                      &
     &                                    signals_created,                     &
     &                                    observed, sigma, weight,             &
     &                                    current_profile, first_index,        &
     &                                    .false., .true.)

            CASE ('sxr_chord_RPhiDegZ_xics_ti')
               CALL sxrch_dot_parse_chord(sxrch_dot_file_ref,                  &
     &                                    'RPHiDegZ', signals,                 &
     &                                    signals_created,                     &
     &                                    observed, sigma, weight,             &
     &                                    current_profile, first_index,        &
     &                                    .false., .true.)

            CASE ('sxr_new_profile')
               current_profile = current_profile + 1

         END SELECT

      END DO

!  Close the 'sxrch.' file
      CALL signal_dot_close(sxrch_dot_file_ref)

      CALL profiler_set_stop_time('sxrch_dot_read', start_time)

      END SUBROUTINE

!*******************************************************************************
!  PARSING SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Parse soft x-ray diagnostic chord.
!>
!>  Parses a single chord. Chords are laid out as,
!>
!>  * chord_name
!>  * Starting point as three reals.
!>  * Ending point as three reals.
!>
!>  @param[in]    sxrch_dot_file_ref Reference to the sxrch. file.
!>  @param[in]    coordinate_type    Specifies the cartesian or cylindrical
!>                                   coordinate type.
!>  @param[inout] signals            Array of pointers to the a signal
!>  @param[inout] signals_created    Number valid signals in the array, or index
!>                                   of the last signal created.
!>  @param[in]    observed           Array of observed values.
!>  @param[in]    sigma              Array of observed sigma values
!>  @param[in]    weight             Array of signal weight values
!>  @param[in]    current_profile    Index of sxrem profile used.
!>  @param[inout] first_index        Index of the first instance of an intpol
!>                                   signal.
!>  @param[in]    use_geo            Chord contains geomtric factor.
!>  @param[in]    is_ti              Chord used electron temperature.
!-------------------------------------------------------------------------------
      SUBROUTINE sxrch_dot_parse_chord(sxrch_dot_file_ref,                     &
     &                                 coordinate_type,                        &
     &                                 signals,                                &
     &                                 signals_created,                        &
     &                                 observed, sigma, weight,                &
     &                                 current_profile, first_index,           &
     &                                 use_geo, is_ti)
      USE sxrem
      USE v3fit_input, only: v3fit_max_diagnostics,                            &
     &                       v3fit_input_find_scale_index,                     &
     &                       v3fit_input_find_offset_index
      USE v3_utilities, only: err_fatal

      IMPLICIT NONE

!  Declare Arguments
      TYPE (signal_dot_file), INTENT(inout)  :: sxrch_dot_file_ref
      CHARACTER (len=*), INTENT(in)          :: coordinate_type
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      INTEGER, INTENT(inout)                 :: signals_created
      REAL (rprec), DIMENSION(:), INTENT(in) :: observed
      REAL (rprec), DIMENSION(:), INTENT(in) :: sigma
      REAL (rprec), DIMENSION(:), INTENT(in) :: weight
      INTEGER, INTENT(in)                    :: current_profile
      INTEGER, INTENT(inout)                 :: first_index
      LOGICAL, INTENT(in)                    :: use_geo
      LOGICAL, INTENT(in)                    :: is_ti

!  local variables
      REAL(rprec), DIMENSION(3)              :: xcart_i, xcart_f
      CHARACTER (len=data_short_name_length) :: chord_name
      REAL (rprec)                           :: geo
      CLASS (sxrem_class), POINTER           :: sxrem_obj
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Check if there are two many signals.
      IF (signals_created + 1 .gt. v3fit_max_diagnostics) THEN
         CALL err_fatal('sxrch_dot_parse_chord_new: created signals' //        &
     &                  ' exceeds v3fit_max_diagnostics')
      END IF

!  Get the start, end and name of chord.
      CALL signal_dot_parse_chord(sxrch_dot_file_ref,                          &
     &                            coordinate_type,                             &
     &                            chord_name,                                  &
     &                            xcart_i, xcart_f)

!  Default the geometric factor to 1.
      geo = 1.0
      IF (use_geo) THEN
         geo = signal_dot_parse_real(sxrch_dot_file_ref,                       &
     &            'Expected geometric factor for sxrem chord')
      END IF

      IF (is_ti) THEN
         sxrem_obj => sxrem_ti_class(xcart_i, xcart_f, current_profile)
         CALL signal_construct(sxrem_obj, chord_name, chord_name, 'eVm',       &
     &           observed(signals_created + 1),                                &
     &           sigma(signals_created + 1),                                   &
     &           weight(signals_created + 1),                                  &
     &           v3fit_input_find_scale_index(signals_created + 1),            &
     &           v3fit_input_find_offset_index(signals_created + 1))
      ELSE
         sxrem_obj => sxrem_emiss_class(xcart_i, xcart_f, geo,                 &
     &                                  current_profile)
         CALL signal_construct(sxrem_obj, chord_name, chord_name, 'arb',       &
     &           observed(signals_created + 1),                                &
     &           sigma(signals_created + 1),                                   &
     &           weight(signals_created + 1),                                  &
     &           v3fit_input_find_scale_index(signals_created + 1),            &
     &           v3fit_input_find_offset_index(signals_created + 1))
      END IF

      signals(signals_created + 1)%p => sxrem_obj

      signals_created = signals_created + 1

!  At lease one sxrem signal was made. Set the first index. This should only be
!  run once.
      IF (first_index .eq. -1) THEN
         first_index = signals_created
      END IF

      CALL profiler_set_stop_time('sxrch_dot_parse_chord', start_time)

      END SUBROUTINE

      END MODULE

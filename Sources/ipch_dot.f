!-------------------------------------------------------------------------------
!  The @fixed_width, @begin_table, @item2 and @end_table commands are custom
!  defined commands in Doxygen.in. They are defined under ALIASES. For the page
!  created here, the 80 column limit is exceeded. Arguments of aliases are
!  separated by ','. If you intended ',' to be a string you must use an escaped
!  comma '\,'.
!
!>  @page intpol_dot_sec Inteferometry/Polarimetry Diagnostic Dot File
!>
!>  @tableofcontents
!>  @section intpol_dot_intro_sec Introduction
!>  Diagnostic dot files specify geometric and auxiliary information about each
!>  diagnostic. Parsing of a diagnostic dot file begins by searching for a valid
!>  keyword. All lines before a valid keyword is reached are ignored allowing
!>  for comments and other information to be inserted in the file. Once a
!>  keyword is reached, the parsing becomes strict. The diagnostic description
!>  format is dependant on the specific keyword. All diagnostic dot files must
!>  end with an @fixed_width{end_of_file} keyword.
!>
!>  This document contains information about the interferometry/polarimetry
!>  chord specifications. Chords are specified in a structured text file. This
!>  file is read in by the @ref v3fit_input::ipch_dot_filename variable of the
!>  name list input file. Chords are specified as two verticies in either meters
!>  or radians. For polarimetry, wavelengths are specified in meters.
!>
!>  @section intpol_dot_key_sec Keywords
!>  @begin_table
!>  @item2{@fixed_width{ip_chord_XYZ}\n @fixed_width{ip_chord_XYZ_int},           An interferometer chord specified in Cartesian coordinates.}
!>  @item2{@fixed_width{ip_chord_RPhiDegZ}\n @fixed_width{ip_chord_RPhiDegZ_int}, An interferometer chord specified in Cylindrical coordinates.}
!>  @item2{@fixed_width{ip_chord_XYZ_pol_rad},                                    An polarimetry chord specified in Cartesian coordinates outputting the signal in radians.}
!>  @item2{@fixed_width{ip_chord_RPhiDegZ_pol_rad},                               An polarimetry chord specified in Cylindrical coordinates outputting the signal in radians.}
!>  @item2{@fixed_width{ip_chord_XYZ_pol_deg},                                    An polarimetry chord specified in Cartesian coordinates outputting the signal in degress.}
!>  @item2{@fixed_width{ip_chord_RPhiDegZ_pol_deg},                               An polarimetry chord specified in Cylindrical coordinates outputting the signal in degress.}
!>  @end_table
!>
!>  @section intpol_dot_chord_spec_sec Chord Specification
!>  All interferometry chords are specified in the following manner.
!>
!>  @fixed_width{key_word\n}
!>  @fixed_width{name(STRING len:@ref data_parameters::data_short_name_length)\n}
!>  @fixed_width{x1(REAL) y1(REAL) z1(REAL)\n}
!>  @fixed_width{x2(REAL) y2(REAL) z2(REAL)\n}
!>
!>  All Polarimetry chords are specified in the following manner with the wave
!>  length .
!>
!>  @fixed_width{key_word\n}
!>  @fixed_width{name(STRING len:@ref data_parameters::data_short_name_length)\n}
!>  @fixed_width{x1(REAL) y1(REAL) z1(REAL)\n}
!>  @fixed_width{x2(REAL) y2(REAL) z2(REAL)\n}
!>  @fixed_width{wavelength(REAL)\n}
!>
!>  @section intpol_dot_exam_sec Example File
!>  @code
!>  !  An interferometer chord specified in Cartesian coordinates.
!>  ip_chord_XYZ_int
!>  int001
!>  1.0 0.0 0.0
!>  0.5 0.0 0.0
!>
!>  !  An interferometer chord specified in Cylindrical coordinates.
!>  ip_chord_RPhiDegZ_int
!>  int002
!>  1.0 0.0 0.0
!>  0.5 0.0 0.0
!>
!>  !  A polarimetry chord specified in Cartesian coordinates.
!>  ip_chord_XYZ_pol_deg
!>  pol001
!>  1.0 0.0 0.0
!>  0.5 0.0 0.0
!>  1.0E-5
!>
!>  !  A polarimetry chord specified in Cylindrical coordinates.
!>  ip_chord_RPhiDegZ_pol_deg
!>  int002
!>  1.0 0.0 0.0
!>  0.5 0.0 0.0
!>  1.0E-5
!>
!>  !  All files must end with
!>  end_of_file
!>  @endcode
!>
!>  @section intpol_dot_prog_ref_sec Programmers Reference
!>  Reference material for the coding to parse these files can be found in the
!>  @ref signal_dot and @ref ipch_dot modules.
!-------------------------------------------------------------------------------
!*******************************************************************************
!>  @file ipch_dot.f
!>  @brief Contains module @ref ipch_dot
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Module for opening and reading a 'ipch.' file. The file format for these
!>  files are documented in @ref intpol_dot_sec.
!*******************************************************************************

      MODULE ipch_dot

      USE stel_kinds
      USE stel_constants
      USE signal_dot

      IMPLICIT NONE

      CONTAINS
!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Read an interferometry/polarimetry diagnostic dot file.
!>
!>  Parses the structure of a dignostic dot file and constructs a @ref intpol
!>  object. Each chord is parsed by @ref ipch_dot_parse_chord.
!>
!>  @param[in]    ipch_file       Name of the ipch. file.
!>  @param[inout] signals         Array of pointers to the a signal
!>  @param[inout] signals_created Number valid signals in the array, or index of
!>                                the last signal created.
!>  @param[in]    observed        Array of observed values.
!>  @param[in]    sigma           Array of observed sigma values
!>  @param[in]    weight          Array of signal weight values
!>  @param[inout] first_index     Index of the first instance of an intpol
!>                                signal.
!>  @param[out]   use_polarimetry True is a polarimetry signal was constructed.
!-------------------------------------------------------------------------------
      SUBROUTINE ipch_dot_read(ipch_file, signals, signals_created,            &
     &                         observed, sigma, weight, first_index,           &
     &                         use_polarimetry)
      USE signal
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=path_length), INTENT(in)            :: ipch_file
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      INTEGER, INTENT(inout) :: signals_created
      REAL (rprec), DIMENSION(:), INTENT(in)             :: observed
      REAL (rprec), DIMENSION(:), INTENT(in)             :: sigma
      REAL (rprec), DIMENSION(:), INTENT(in)             :: weight
      INTEGER, INTENT(inout)                             :: first_index
      LOGICAL, INTENT(out) :: use_polarimetry

!  local parameters
!  n_ipchd_keyword        integer - number of ipchd_keyword s.
      INTEGER, PARAMETER :: n_ipchd_keyword = 9

!  local variables
!  ipchd_keyword    character array - keywords for various ipch input types
      CHARACTER(len=data_name_length), DIMENSION(1:n_ipchd_keyword) ::         &
     &   ipchd_keyword
      TYPE(signal_dot_file) :: ipch_dot_file_ref
      REAL (rprec)          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Initialize the keywords and keyword lengths
      ipchd_keyword(1) = 'ip_chord_XYZ'
      ipchd_keyword(2) = 'ip_chord_RPhiDegZ'
      ipchd_keyword(3) = 'ip_chord_XYZ_int'
      ipchd_keyword(4) = 'ip_chord_RPhiDegZ_int'
      ipchd_keyword(5) = 'ip_chord_XYZ_pol_rad'
      ipchd_keyword(6) = 'ip_chord_RPhiDegZ_pol_rad'
      ipchd_keyword(7) = 'ip_chord_XYZ_pol_deg'
      ipchd_keyword(8) = 'ip_chord_RPhiDegZ_pol_deg'
      ipchd_keyword(9) = 'end_of_file'

!  Open up the 'ipch.' file
      ipch_dot_file_ref = signal_dot_open(TRIM(ipch_file), 'ipch')

      use_polarimetry = .false.

!  Infinite Loop
!  Character variable line should be defined on entry
      DO
!  Branch on the keyword
         SELECT CASE (signal_dot_read_keyword(ipch_dot_file_ref,               &
     &                                        ipchd_keyword))

            CASE DEFAULT ! This case should never fire.
               EXIT ! Exit out of infinte loop.

            CASE ('end_of_file')
               EXIT ! Exit out of infinte loop.

            CASE ('ip_chord_XYZ','ip_chord_XYZ_int')
               CALL ipch_dot_parse_chord(ipch_dot_file_ref,                    &
     &                                   'XYZ', 'i', .false.,                  &
     &                                    signals, signals_created,            &
     &                                    observed, sigma, weight,             &
     &                                    first_index)


            CASE ('ip_chord_RPhiDegZ','ip_chord_RPhiDegZ_int')
               CALL ipch_dot_parse_chord(ipch_dot_file_ref,                    &
     &                                   'RPHiDegZ', 'i', .false.,             &
     &                                    signals, signals_created,            &
     &                                    observed, sigma, weight,             &
     &                                    first_index)


            CASE ('ip_chord_XYZ_pol_rad')
               CALL ipch_dot_parse_chord(ipch_dot_file_ref,                    &
     &                                   'XYZ', 'p', .false.,                  &
     &                                    signals, signals_created,            &
     &                                    observed, sigma, weight,             &
     &                                    first_index)
               use_polarimetry = .true.


            CASE ('ip_chord_RPhiDegZ_pol_rad')
               CALL ipch_dot_parse_chord(ipch_dot_file_ref,                    &
     &                                   'RPHiDegZ', 'p', .false.,             &
     &                                    signals, signals_created,            &
     &                                    observed, sigma, weight,             &
     &                                    first_index)
               use_polarimetry = .true.


            CASE ('ip_chord_XYZ_pol_deg')
               CALL ipch_dot_parse_chord(ipch_dot_file_ref,                    &
     &                                   'XYZ', 'p', .true.,                   &
     &                                    signals, signals_created,            &
     &                                    observed, sigma, weight,             &
     &                                    first_index)
               use_polarimetry = .true.


            CASE ('ip_chord_RPhiDegZ_pol_deg')
               CALL ipch_dot_parse_chord(ipch_dot_file_ref,                    &
     &                                   'RPHiDegZ', 'p', .true.,              &
     &                                    signals, signals_created,            &
     &                                    observed, sigma, weight,             &
     &                                    first_index)
               use_polarimetry = .true.


         END SELECT

      END DO

!  Close the 'ipch.' file
      CALL signal_dot_close(ipch_dot_file_ref)

      CALL profiler_set_stop_time('ipch_dot_read', start_time)

      END SUBROUTINE

!*******************************************************************************
!  PARSING SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Parse interferometry/polarimetry diagnostic chord.
!>
!>  Parses a signle chord. Chords are laid out as,
!>
!>  * chord_name
!>  * Starting point as three reals.
!>  * Ending point as three reals.
!>
!>  If the signal is a polarimetry chord, then also read a single real for the
!>  wavelength.
!>
!>  @param[in]    ipch_dot_file_ref Name of the ipch. file.
!>  @param[in]    coordinate_type   Specifies the cartesian or cylindrical
!>                                  coordinate type.
!>  @param[in]    chord_type        Specifies if the cord is a 'i'nterferometer
!>                                  or 'p'olarimetry.
!>  @param[in]    inDegrees         Specifies if signal measures degrees or
!>                                  radians.
!>  @param[inout] signals           Array of pointers to the a signal
!>  @param[inout] signals_created   Number valid signals in the array, or index
!>                                  or the last signal created.
!>  @param[in]    observed          Array of observed values.
!>  @param[in]    sigma             Array of observed sigma values
!>  @param[in]    weight            Array of signal weight values
!>  @param[inout] first_index       Index of the first instance of an intpol
!>                                  signal.
!-------------------------------------------------------------------------------
      SUBROUTINE ipch_dot_parse_chord(ipch_dot_file_ref,                       &
     &                                coordinate_type,                         &
     &                                chord_type,                              &
     &                                inDegrees,                               &
     &                                signals,                                 &
     &                                signals_created,                         &
     &                                observed, sigma, weight,                 &
     &                                first_index)
      USE intpol
      USE v3fit_input, only: v3fit_max_diagnostics,                            &
     &                       v3fit_input_find_scale_index,                     &
     &                       v3fit_input_find_offset_index
      USE v3_utilities, only: err_fatal

      IMPLICIT NONE

!  Declare Arguments
      TYPE (signal_dot_file), INTENT(inout)  :: ipch_dot_file_ref
      CHARACTER (len=*), INTENT(in)          :: coordinate_type
      CHARACTER (len=1), INTENT(in)          :: chord_type
      LOGICAL, INTENT(in)                    :: inDegrees
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      INTEGER, INTENT(inout)                 :: signals_created
      REAL (rprec), DIMENSION(:), INTENT(in) :: observed
      REAL (rprec), DIMENSION(:), INTENT(in) :: sigma
      REAL (rprec), DIMENSION(:), INTENT(in) :: weight
      INTEGER, INTENT(inout)                 :: first_index

!  local variables
      REAL (rprec), DIMENSION(2,3)           :: xcart
      CHARACTER (len=data_name_length)       :: chord_name
      REAL (rprec)                           :: wavelength = 0.0
      REAL (rprec)                           :: start_time
      CHARACTER (len=6)                      :: units
      CLASS (intpol_class), POINTER          :: intpol_obj

!  Start of executable code
      start_time = profiler_get_start_time()

!  Check if there are two many signals.
      IF (signals_created + 1 .gt. v3fit_max_diagnostics) THEN
         CALL err_fatal('ipch_dot_parse_chord_new: created signals' //         &
     &                  ' exceeds v3fit_max_diagnostics')
      END IF

!  Get the start, end and name of chord.
      CALL signal_dot_parse_chord(ipch_dot_file_ref,                           &
     &                            coordinate_type,                             &
     &                            chord_name,                                  &
     &                            xcart(1,:), xcart(2,:))

      IF (chord_type .eq. 'p') THEN
         wavelength = signal_dot_parse_real(ipch_dot_file_ref,                 &
     &                   'Expected wavelength for polarimetry chord')
         intpol_obj => intpol_pol_class(wavelength, inDegrees, xcart)

         IF (inDegrees) THEN
            units = 'degree'
         ELSE
            units = 'radian'
         END IF

         CALL signal_construct(intpol_obj, chord_name, chord_name,             &
     &           units, observed(signals_created + 1),                         &
     &           sigma(signals_created + 1),                                   &
     &           weight(signals_created + 1),                                  &
     &           v3fit_input_find_scale_index(signals_created + 1),            &
     &           v3fit_input_find_offset_index(signals_created + 1))

         signals(signals_created + 1)%p => intpol_obj
      ELSE
         intpol_obj => intpol_class(xcart)
         CALL signal_construct(intpol_obj, chord_name, chord_name,             &
     &           'm^-2', observed(signals_created + 1),                        &
     &           sigma(signals_created + 1),                                   &
     &           weight(signals_created + 1),                                  &
     &           v3fit_input_find_scale_index(signals_created + 1),            &
     &           v3fit_input_find_offset_index(signals_created + 1))

         signals(signals_created + 1)%p => intpol_obj
      END IF

      signals_created = signals_created + 1

!  At lease one intpol signal was made. Set the first index. This should only be
!  run once.
      IF (first_index .eq. -1) THEN
         first_index = signals_created
      END IF

      CALL profiler_set_stop_time('ipch_dot_parse_chord', start_time)

      END SUBROUTINE

      END MODULE

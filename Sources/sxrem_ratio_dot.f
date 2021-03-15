!-------------------------------------------------------------------------------
!  The @fixed_width, @begin_table, @item2 and @end_table commands are custom
!  defined commands in Doxygen.in. They are defined under ALIASES. For the page
!  created here, the 80 column limit is exceeded. Arguments of aliases are
!  separated by ','. If you intended ',' to be a string you must use an escaped
!  comma '\,'.
!
!>  @page sxrem_ratio_dot_sec Soft X-ray Emissivity Ratio Dot File
!>
!>  @tableofcontents
!>  @section sxrem_ratio_dot_intro_sec Introduction
!>  Diagnostic dot files specify geometric and auxiliary information about each
!>  diagnostic. Parsing of a diagnostic dot file begins by searching for a valid
!>  keyword. All lines before a valid keyword is reached are ignored allowing
!>  for comments and other information to be inserted in the file. Once a
!>  keyword is reached, the parsing becomes strict. The diagnostic description
!>  format is dependant on the specific keyword. All diagnostic dot files must
!>  end with an @fixed_width{end_of_file} keyword.
!>
!>  This document contains information about the sxrem ratio point
!>  specifications. Points are specified in a structured text file. This file
!>  is read in by the @ref v3fit_input::sxrem_ratio_dot_filename variable of the
!>  name list input file. Points are specified as a verticies in either meters
!>  or radians.
!>
!>  @section sxrem_ratio_dot_key_sec Keywords
!>  @begin_table
!>  @item2{@fixed_width{sxrem_ratio_XYZ},      A sxrem ratio point specified in Cartesian coordinates measuring electron temperature.}
!>  @item2{@fixed_width{sxrem_ratio_RPhiDegZ}, A sxrem ratio point specified in Cylindrical coordinates measuring electron temperature.}
!>  @end_table
!>
!>  @section sxrem_ratio_dot_chord_spec_sec Point Specification
!>  All thomson scattering points are specified in the following manner. Indices
!>  a specified such that the order defines the ratio as index1/index2.
!>
!>  @fixed_width{key_word\n}
!>  @fixed_width{name(STRING len:@ref data_parameters::data_short_name_length)\n}
!>  @fixed_width{x1(REAL) y1(REAL) z1(REAL)\n}
!>  @fixed_width{index1(INTEGER index2(INTEGER)}
!>
!>  @section sxrem_ratio_dot_exam_sec Example File
!>  @code
!>  !  A sxrem ratio point specified in Cartesian coordinates for the profile ration of indices 2/3.
!>  sxrem_ratio_XYZ
!>  ratio1
!>  1.0 0.0 0.0
!>  2 3
!>
!>  !  A sxrem ratio point specified in Cartesian coordinates for the profile ration of indices 4/1.
!>  sxrem_ratio_RPhiDegZ
!>  ratio2
!>  1.0 0.0 0.0
!>  4 1
!>
!>  !  All files must end with
!>  end_of_file
!>  @endcode
!>
!>  @section sxrem_ratio_dot_prog_ref_sec Programmers Reference
!>  Reference material for the coding to parse these files can be found in the
!>  @ref signal_dot and @ref sxrem_ratio_dot modules.
!-------------------------------------------------------------------------------
!*******************************************************************************
!>  @file sxrem_ratio_dot.f
!>  @brief Contains module @ref sxrem_ratio_dot
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Module for opening and reading a 'sxrem_ratio.' file. The file format for these
!>  files are documented in @ref sxrem_ratio_dot_sec.
!*******************************************************************************
      MODULE sxrem_ratio_dot

      USE stel_kinds
      USE stel_constants
      USE signal_dot

      IMPLICIT NONE

      CONTAINS
!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Read an sxrem ratio dot file.
!>
!>  Parses the structure of a dignostic dot file and constructs a
!>  @ref sxrem_ratio object. Each chord is parsed by
!>  @ref sxrem_ratio_dot_parse_chord.
!>
!>  @param[in]    sxrem_ratio_file Name of the sxrem_ratio. file.
!>  @param[inout] signals          Array of pointers to the a signal
!>  @param[inout] signals_created  Number valid signals in the array, or index
!>                                 of the last signal created.
!>  @param[in]    observed         Array of observed values.
!>  @param[in]    sigma            Array of observed sigma values
!>  @param[in]    weight           Array of signal weight values
!>  @param[inout] first_index      Index of the first instance of an intpol
!>                                 signal.
!-------------------------------------------------------------------------------
      SUBROUTINE sxrem_ratio_dot_read(sxrem_ratio_file, signals,               &
     &                                signals_created, observed, sigma,        &
     &                                weight, first_index)
      USE signal
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=path_length), INTENT(in) :: sxrem_ratio_file
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      INTEGER, INTENT(inout) :: signals_created
      REAL (rprec), DIMENSION(:), INTENT(in)             :: observed
      REAL (rprec), DIMENSION(:), INTENT(in)             :: sigma
      REAL (rprec), DIMENSION(:), INTENT(in)             :: weight
      INTEGER, INTENT(inout)                             :: first_index

!  local parameters
!  n_thscted_keyword    integer - number of thscted_keyword s.
      INTEGER, PARAMETER :: n_sxrem_ratio_keyword = 3

!  local variables
!  sxrem_ratio_keyword   character array - keywords for various thscte input types
      CHARACTER(len=data_name_length),                                         &
     &   DIMENSION(1:n_sxrem_ratio_keyword) :: sxrem_ratio_keyword
      TYPE(signal_dot_file)                 :: sxrem_ratio_dot_file_ref
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Initialize the keywords and keyword lengths
      sxrem_ratio_keyword(1) = 'sxrem_ratio_XYZ'
      sxrem_ratio_keyword(2) = 'sxrem_ratio_RPhiDegZ'
      sxrem_ratio_keyword(3) = 'end_of_file'

!  Open up the 'thscte.' file
      sxrem_ratio_dot_file_ref = signal_dot_open(TRIM(sxrem_ratio_file),            &
     &                                           'sxrem_ratio')

!  Infinite Loop
!  Character variable line should be defined on entry
      DO
!  Branch on the keyword
         SELECT CASE (signal_dot_read_keyword(sxrem_ratio_dot_file_ref,        &
     &                                        sxrem_ratio_keyword))

            CASE DEFAULT ! This case should never fire.
               EXIT ! Exit out of infinte loop.

            CASE ('end_of_file')
               EXIT ! Exit out of infinte loop.

            CASE ('sxrem_ratio_XYZ')
               CALL sxrem_ratio_dot_parse_chord(                               &
     &                 sxrem_ratio_dot_file_ref, 'XYZ', signals,               &
     &                 signals_created, observed, sigma, weight,               &
     &                 first_index)

            CASE ('sxrem_ratio_RPhiDegZ')
               CALL sxrem_ratio_dot_parse_chord(                               &
     &                 sxrem_ratio_dot_file_ref, 'RPHiDegZ', signals,          &
     &                 signals_created, observed, sigma, weight,               &
     &                 first_index)
         END SELECT

      END DO

!  Close the 'thscte.' file
      CALL signal_dot_close(sxrem_ratio_dot_file_ref)

      CALL profiler_set_stop_time('sxrem_ratio_dot_read', start_time)

      END SUBROUTINE

!*******************************************************************************
!  PARSING SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Parse soft x-ray ratio point.
!>
!>  Parses a signle chord. Chords are laid out as,
!>
!>  * chord_name
!>  * Starting point as three reals.
!>  * Ending point as three reals.
!>
!>  @param[in]    sxrem_ratio_dot_file_ref Name of the sxrem_ratio file.
!>  @param[in]    coordinate_type          Specifies the cartesian or
!>                                         cylindrical coordinate type.
!>  @param[inout] signals                  Array of pointers to the a signal.
!>  @param[inout] signals_created          Number valid signals in the array, or
!>                                         index of the last signal created.
!>  @param[in]    observed                 Array of observed values.
!>  @param[in]    sigma                    Array of observed sigma values.
!>  @param[in]    weight                   Array of signal weight values.
!>  @param[inout] first_index              Index of the first instance of an
!>                                         intpol.
!-------------------------------------------------------------------------------
      SUBROUTINE sxrem_ratio_dot_parse_chord(sxrem_ratio_dot_file_ref,         &
     &                                       coordinate_type,                  &
     &                                       signals, signals_created,         &
     &                                       observed, sigma, weight,          &
     &                                       first_index)
      USE sxrem_ratio
      USE v3fit_input, only: v3fit_max_diagnostics,                            &
     &                       v3fit_input_find_scale_index,                     &
     &                       v3fit_input_find_offset_index
      USE v3_utilities, only: err_fatal
      IMPLICIT NONE

!  Declare Arguments
      TYPE(signal_dot_file), INTENT(inout)   :: sxrem_ratio_dot_file_ref
      CHARACTER (len=*)                      :: coordinate_type
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      INTEGER, INTENT(inout)                 :: signals_created
      REAL (rprec), DIMENSION(:), INTENT(in) :: observed
      REAL (rprec), DIMENSION(:), INTENT(in) :: sigma
      REAL (rprec), DIMENSION(:), INTENT(in) :: weight
      INTEGER, INTENT(inout)                 :: first_index

!  local variables
      REAL (rprec), DIMENSION(3)             :: xcart
      INTEGER, DIMENSION(2)                  :: indices
      CHARACTER (len=data_short_name_length) :: point_name
      CLASS (sxrem_ratio_class), POINTER      :: ratio_obj => null()
      CHARACTER (len=data_short_name_length) :: units = ''
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Check if there are two many signals.
      IF (signals_created + 1 .gt. v3fit_max_diagnostics) THEN
         CALL err_fatal('sxrem_ratio_dot_parse_chord: created signals'         &
     &                  // ' exceeds v3fit_max_diagnostics')
      END IF

!  Get the position and name of the point.
      CALL signal_dot_parse_chord(sxrem_ratio_dot_file_ref,                    &
     &                            coordinate_type,                             &
     &                            point_name,                                  &
     &                            xcart)

      indices = signal_dot_parse_2_int(sxrem_ratio_dot_file_ref,               &
     &             'Expected sxrem profile indices')

      ratio_obj => sxrem_ratio_class(xcart, indices)

      CALL signal_construct(ratio_obj, point_name, point_name, units,          &
     &        observed(signals_created + 1),                                   &
     &        sigma(signals_created + 1), weight(signals_created + 1),         &
     &        v3fit_input_find_scale_index(signals_created + 1),               &
     &        v3fit_input_find_offset_index(signals_created + 1))

      signals(signals_created + 1)%p => ratio_obj

      signals_created = signals_created + 1

!  At lease one sxrem_ratio signal was made. Set the first index. This should
!  only be run once.
      IF (first_index .eq. -1) THEN
         first_index = signals_created
      END IF

      CALL profiler_set_stop_time('sxrem_ratio_dot_parse_chord',               &
     &                            start_time)

      END SUBROUTINE

      END MODULE

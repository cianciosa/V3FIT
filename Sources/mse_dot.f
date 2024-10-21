!-------------------------------------------------------------------------------
!  The @fixed_width, @begin_table, @item2 and @end_table commands are custom
!  defined commands in Doxygen.in. They are defined under ALIASES. For the page
!  created here, the 80 column limit is exceeded. Arguments of aliases are
!  separated by ','. If you intended ',' to be a string you must use an escaped
!  comma '\,'.
!
!>  @page mse_dot_sec Motional Stark Effect Diagnostic Dot File
!>
!>  @tableofcontents
!>  @section mse_dot_intro_sec Introduction
!>  Diagnostic dot files specify geometric and auxiliary information about each
!>  diagnostic. Parsing of a diagnostic dot file begins by searching for a valid
!>  keyword. All lines before a valid keyword is reached are ignored allowing
!>  for comments and other information to be inserted in the file. Once a
!>  keyword is reached, the parsing becomes strict. The diagnostic description
!>  format is dependant on the specific keyword. All diagnostic dot files must
!>  end with an @fixed_width{end_of_file} keyword.
!>
!>  This document contains information about the motional stark effect point
!>  chord specifications. Points are specified in a structured text file. This
!>  file is read in by the @ref v3fit_input::mse_dot_filename variable of the
!>  name list input file. There are two types of MSE points.
!>  * Points specified as vectors of the viewing and neutral beam.
!>  * Points specified directly with angles with respect to the toroidal
!>    direction and the horizontal.
!>
!>  @note The point of the MSE measurement could be infered from the
!>  intersection of the neutral beam vector and the viewing vector. However for
!>  numercial reasons the intersection point is expicity defined.
!>  @note The neutral beam vector could have been defined once however it's
!>  simpler to define it for each measurement point.
!>
!>  @section mse_dot_key_sec Keywords
!>  @begin_table
!>  @item2{@fixed_width{mse_point_XYZ_deg},            A MSE point specified in Cartesian Coordinates outputting the signal in degrees.}
!>  @item2{@fixed_width{mse_point_RPhiDegZ_deg},       A MSE point specified in Cylindrical Coordinates outputting the signal in degrees.}
!>  @item2{@fixed_width{mse_point_XYZ_rad},            A MSE point specified in Cartesian Coordinates outputting the signal in radians.}
!>  @item2{@fixed_width{mse_point_RPhiDegZ_rad},       A MSE point specified in Cylindrical Coordinates outputting the signal in radians.}
!>  @item2{@fixed_width{mse_point_XYZ_deg_deg},        A MSE point specified in Cartesian Coordinates outputting the signal in degrees. Direction angles are specifed in degrees.}
!>  @item2{@fixed_width{mse_point_RPhiDegZ_deg_deg},   A MSE point specified in Cylindrical Coordinates outputting the signal in degrees. Direction angles are specifed in degrees.}
!>  @item2{@fixed_width{mse_point_XYZ_rad_deg},        A MSE point specified in Cartesian Coordinates outputting the signal in degrees. Direction angles are specifed in radians.}
!>  @item2{@fixed_width{mse_point_RPhiDegZ_rad_deg},   A MSE point specified in Cylindrical Coordinates outputting the signal in degrees. Direction angles are specifed in radians.}
!>  @item2{@fixed_width{mse_point_XYZ_deg_rad},        A MSE point specified in Cartesian Coordinates outputting the signal in radians. Direction angles are specifed in degrees.}
!>  @item2{@fixed_width{mse_point_RPhiDegZ_deg_rad},   A MSE point specified in Cylindrical Coordinates outputting the signal in radians. Direction angles are specifed in degrees.}
!>  @item2{@fixed_width{mse_point_XYZ_rad_rad},        A MSE point specified in Cartesian Coordinates outputting the signal in radians. Direction angles are specifed in radians.}
!>  @item2{@fixed_width{mse_point_RPhiDegZ_rad_rad},   A MSE point specified in Cylindrical Coordinates outputting the signal in radians. Direction angles are specifed in radians.}
!>  @item2{@fixed_width{mse_point_XYZ_ratio},          A MSE point specified in Cartesian Coordinates outputting the signal ratio.}
!>  @item2{@fixed_width{mse_point_RPhiDegZ_ratio},     A MSE point specified in Cylindrical Coordinates outputting the signal ratio.}
!>  @item2{@fixed_width{mse_point_XYZ_deg_ratio},      A MSE point specified in Cartesian Coordinates outputting the signal ratio. Direction angles are specifed in degrees.}
!>  @item2{@fixed_width{mse_point_RPhiDegZ_deg_ratio}, A MSE point specified in Cylindrical Coordinates outputting the signal ratio. Direction angles are specifed in degrees.}
!>  @item2{@fixed_width{mse_point_XYZ_rad_ratio},      A MSE point specified in Cartesian Coordinates outputting the signal ratio. Direction angles are specifed in radians.}
!>  @item2{@fixed_width{mse_point_RPhiDegZ_rad_ratio}, A MSE point specified in Cylindrical Coordinates outputting the signal ratio. Direction angles are specifed in radians.}
!>  @end_table
!>
!>  @section mse_dot_point_spec_sec Point Specification
!>  All motional stark effect points are specified in the following manner. A
!>  string containing the diagnostic name. Three reals specfy the measurement
!>  point in 3D.
!>
!>  Keywords that only have a single angle specification to the vector
!>  discription. For these keyword two sets of three reals are start and end
!>  points of the viewing cord. The last two sets of real three reals are the
!>  start and end points of the neutral beam. Each set of three reals is a point
!>  in either Cartesian or Cyclindrical Coordinates depending on the keyword.
!>
!>  Keywords that contain two angle specification use the angle discription.
!>  These keywords specify the angle in using 4 angles. The first line contains
!>  the angles alpha and omega. The second set of angles are delta and theta.
!>  These angles are defined such that.
!>  * alpha: The angle between the neutral beam and the toroidal direction. When alpha is 0, the neutral beam points in the negative toroidal direction. When alpha is 90, the beam points in the negative radial direction.
!>  * omgea: The angle between the view chord and the torodial direction. When omega is 0, the chord points in the positive toroidal direction. When alpha is 90, the beam points in the negative radial direction.
!>  * delta: The angle between the neutral beam and the horizontal. When delta is 90, the beam points in the negative z direction.
!>  * theta: The angle between the view chord and the horizontal. When delta is 90, the beam points in the positive z direction.
!>
!>  @fixed_width{key_word_ang}\n
!>  @fixed_width{name(STRING len:@ref data_parameters::data_short_name_length)}\n
!>  @fixed_width{x(REAL) y(REAL) z(REAL)}\n
!>  @fixed_width{vx1(REAL) vy1(REAL) vz1(REAL)}\n
!>  @fixed_width{vx2(REAL) vy2(REAL) vz2(REAL)}\n
!>  @fixed_width{nx1(REAL) ny1(REAL) nz1(REAL)}\n
!>  @fixed_width{nx2(REAL) ny2(REAL) nz2(REAL)}\n\n
!>
!>  @fixed_width{key_word_ang_ang}\n
!>  @fixed_width{name(STRING len:@ref data_parameters::data_short_name_length)}\n
!>  @fixed_width{x(REAL) y(REAL) z(REAL)}\n
!>  @fixed_width{alpha(REAL) omega(REAL)}\n
!>  @fixed_width{delta(REAL) theta(REAL)}\n
!>
!>  @section mse_dot_exam_sec Example File
!>  @code
!>  !  A motional stark effect chord specified in Cylindrical coordinates using
!>  !  the vector representation.
!>  mse_point_RPhiDegZ_deg
!>  mse001
!>  0.5 0.0 0.0
!>  0.0 0.0 0.0
!>  0.0 1.0 0.0
!>  -1.0 0.5 0.0
!>  1.0 0.5 0.0
!>
!>  !  A motional stark effect chord specified in Cylindrical coordinates using
!>  !  the angle representation.
!>  mse_point_RPhiDegZ_deg_deg
!>  mse002
!>  0.5 0.0 0.0
!>  1.0 10.0
!>  0.0 0.0
!>
!>  !  All files must end with
!>  end_of_file
!>  @endcode
!>
!>  @section mse_dot_prog_ref_sec Programmers Reference
!>  Reference material for the coding to parse these files can be found in the
!>  @ref signal_dot and @ref mse_dot modules.
!-------------------------------------------------------------------------------
!*******************************************************************************
!>  @file mse_dot.f
!>  @brief Contains module @ref mse_dot
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Module for opening and reading a 'mse.' file. The file format for these
!>  files are documented in @ref mse_dot_sec.
!*******************************************************************************

      MODULE mse_dot

      USE stel_kinds
      USE stel_constants
      USE signal_dot

      IMPLICIT NONE

      CONTAINS
!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Read an motional stark effect diagnostic dot file.
!>
!>  Parses the structure of a dignostic dot file and constructs a @ref mse
!>  object. Each point is parsed by @ref mse_dot_parse_point.
!>
!>  @param[in]    mse_file       Name of the mse. file.
!>  @param[inout] signals         Array of pointers to the a signal
!>  @param[inout] signals_created Number valid signals in the array, or index of
!>                                the last signal created.
!>  @param[in]    observed        Array of observed values.
!>  @param[in]    sigma           Array of observed sigma values
!>  @param[in]    weight          Array of signal weight values
!>  @param[inout] first_index     Index of the first instance of an intpol
!>                                signal.
!-------------------------------------------------------------------------------
      SUBROUTINE mse_dot_read(mse_file, signals, signals_created,              &
     &                        observed, sigma, weight, first_index)
      USE signal
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=path_length), INTENT(in)            :: mse_file
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      INTEGER, INTENT(inout) :: signals_created
      REAL (rprec), DIMENSION(:), INTENT(in)             :: observed
      REAL (rprec), DIMENSION(:), INTENT(in)             :: sigma
      REAL (rprec), DIMENSION(:), INTENT(in)             :: weight
      INTEGER, INTENT(inout)                             :: first_index

!  local parameters
!  n_mse_keyword        integer - number of mse_keyword s.
      INTEGER, PARAMETER :: n_mse_keyword = 19

!  local variables
!  mse_keyword    character array - keywords for various ipch input types
      CHARACTER(len=data_name_length), DIMENSION(1:n_mse_keyword) ::           &
     &   mse_keyword
      TYPE (signal_dot_file) :: mse_dot_file_ref
      REAL (rprec)           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Initialize the keywords and keyword lengths
      mse_keyword(1) = 'mse_point_XYZ_deg'
      mse_keyword(2) = 'mse_point_RPhiDegZ_deg'
      mse_keyword(3) = 'mse_point_XYZ_rad'
      mse_keyword(4) = 'mse_point_RPhiDegZ_rad'
      mse_keyword(5) = 'mse_point_XYZ_deg_deg'
      mse_keyword(6) = 'mse_point_RPhiDegZ_deg_deg'
      mse_keyword(7) = 'mse_point_XYZ_rad_deg'
      mse_keyword(8) = 'mse_point_RPhiDegZ_rad_deg'
      mse_keyword(9) = 'mse_point_XYZ_deg_rad'
      mse_keyword(10) = 'mse_point_RPhiDegZ_deg_rad'
      mse_keyword(11) = 'mse_point_XYZ_rad_rad'
      mse_keyword(12) = 'mse_point_RPhiDegZ_rad_rad'
      mse_keyword(13) = 'mse_point_XYZ_ratio'
      mse_keyword(14) = 'mse_point_RPhiDegZ_ratio'
      mse_keyword(15) = 'mse_point_XYZ_deg_ratio'
      mse_keyword(16) = 'mse_point_RPhiDegZ_deg_ratio'
      mse_keyword(17) = 'mse_point_XYZ_rad_ratio'
      mse_keyword(18) = 'mse_point_RPhiDegZ_rad_ratio'
      mse_keyword(19) = 'end_of_file'

!  Open up the 'mse.' file
      mse_dot_file_ref = signal_dot_open(TRIM(mse_file), 'mse')

!  Infinite Loop
!  Character variable line should be defined on entry
      DO
!  Branch on the keyword
         SELECT CASE (signal_dot_read_keyword(mse_dot_file_ref,                &
     &                                        mse_keyword))

            CASE DEFAULT ! This case should never fire.
               EXIT ! Exit out of infinte loop.

            CASE ('end_of_file')
               EXIT ! Exit out of infinte loop.

            CASE ('mse_point_XYZ_deg')
               CALL mse_dot_parse_vec(mse_dot_file_ref, 'XYZ', signals,        &
     &                                signals_created, observed, sigma,        &
     &                                weight, first_index, .true.,             &
     &                                .false.)

            CASE ('mse_point_RPhiDegZ_deg')
               CALL mse_dot_parse_vec(mse_dot_file_ref, 'RPhiDegZ',            &
     &                                signals, signals_created,                &
     &                                observed, sigma, weight,                 &
     &                                first_index, .true., .false.)

            CASE ('mse_point_XYZ_rad')
               CALL mse_dot_parse_vec(mse_dot_file_ref, 'XYZ', signals,        &
     &                                signals_created, observed, sigma,        &
     &                                weight, first_index, .false.,            &
     &                                .false.)

            CASE ('mse_point_RPhiDegZ_rad')
               CALL mse_dot_parse_vec(mse_dot_file_ref, 'RPhiDegZ',            &
     &                                signals, signals_created,                &
     &                                observed, sigma, weight,                 &
     &                                first_index, .false., .false.)

            CASE ('mse_point_XYZ_deg_deg')
               CALL mse_dot_parse_ang(mse_dot_file_ref, 'XYZ', signals,        &
     &                                signals_created, observed, sigma,        &
     &                                weight, first_index, .true.,             &
     &                                .true., .false.)

            CASE ('mse_point_RPhiDegZ_deg_deg')
               CALL mse_dot_parse_ang(mse_dot_file_ref, 'RPhiDegZ',            &
     &                                signals, signals_created,                &
     &                                observed, sigma, weight,                 &
     &                                first_index, .true., .true.,             &
     &                                .false.)

            CASE ('mse_point_XYZ_rad_deg')
               CALL mse_dot_parse_ang(mse_dot_file_ref, 'XYZ', signals,        &
     &                                signals_created, observed, sigma,        &
     &                                weight, first_index, .false.,            &
     &                                .true., .false.)

            CASE ('mse_point_RPhiDegZ_rad_deg')
               CALL mse_dot_parse_ang(mse_dot_file_ref, 'RPhiDegZ',            &
     &                                signals, signals_created,                &
     &                                observed, sigma, weight,                 &
     &                                first_index, .false., .true.,            &
     &                                .false.)

            CASE ('mse_point_XYZ_deg_rad')
               CALL mse_dot_parse_ang(mse_dot_file_ref, 'XYZ', signals,        &
     &                                signals_created, observed, sigma,        &
     &                                weight, first_index, .true.,             &
     &                                .false., .false.)

            CASE ('mse_point_RPhiDegZ_deg_rad')
               CALL mse_dot_parse_ang(mse_dot_file_ref, 'RPhiDegZ',            &
     &                                signals, signals_created,                &
     &                                observed, sigma, weight,                 &
     &                                first_index, .true., .false.,            &
     &                                .false.)

            CASE ('mse_point_XYZ_rad_rad')
               CALL mse_dot_parse_ang(mse_dot_file_ref, 'XYZ', signals,        &
     &                                signals_created, observed, sigma,        &
     &                                weight, first_index, .false.,            &
     &                                .false., .false.)

            CASE ('mse_point_RPhiDegZ_rad_rad')
               CALL mse_dot_parse_ang(mse_dot_file_ref, 'RPhiDegZ',            &
     &                                signals, signals_created,                &
     &                                observed, sigma, weight,                 &
     &                                first_index, .false., .false.,           &
     &                                .false.)

            CASE ('mse_point_XYZ_ratio')
               CALL mse_dot_parse_vec(mse_dot_file_ref, 'XYZ', signals,        &
     &                                signals_created, observed, sigma,        &
     &                                weight, first_index, .true.,             &
     &                                .true.)

            CASE ('mse_point_RPhiDegZ_ratio')
               CALL mse_dot_parse_vec(mse_dot_file_ref, 'RPhiDegZ',            &
     &                                signals, signals_created,                &
     &                                observed, sigma, weight,                 &
     &                                first_index, .true., .true.)

            CASE ('mse_point_XYZ_deg_ratio')
               CALL mse_dot_parse_ang(mse_dot_file_ref, 'XYZ', signals,        &
     &                                signals_created, observed, sigma,        &
     &                                weight, first_index, .true.,             &
     &                                .true., .true.)

            CASE ('mse_point_RPhiDegZ_deg_ratio')
               CALL mse_dot_parse_ang(mse_dot_file_ref, 'RPhiDegZ',            &
     &                                signals, signals_created,                &
     &                                observed, sigma, weight,                 &
     &                                first_index, .true., .true.,             &
     &                                .true.)

            CASE ('mse_point_XYZ_rad_ratio')
               CALL mse_dot_parse_ang(mse_dot_file_ref, 'XYZ', signals,        &
     &                                signals_created, observed, sigma,        &
     &                                weight, first_index, .false.,            &
     &                                .true., .true.)

            CASE ('mse_point_RPhiDegZ_rad_ratio')
               CALL mse_dot_parse_ang(mse_dot_file_ref, 'RPhiDegZ',            &
     &                                signals, signals_created,                &
     &                                observed, sigma, weight,                 &
     &                                first_index, .false., .true.,            &
     &                                .true.)

         END SELECT

      END DO

!  Close the 'mse.' file
      CALL signal_dot_close(mse_dot_file_ref)

      CALL profiler_set_stop_time('mse_dot_read', start_time)

      END SUBROUTINE

!*******************************************************************************
!  PARSING SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Parse motional stark effect diagnostic point defined as a vector.
!>
!>  Parses a signle point. Points are laid out as,
!>
!>  * chord_name
!>  * Point of measurement as three reals.
!>  * Starting point of view as three reals.
!>  * Ending point of view as three reals.
!>  * Starting point of beam as three reals.
!>  * Ending point of beam as three reals.
!>
!>  @param[in]    mse_dot_file_ref Reference to the mse. file.
!>  @param[in]    coordinate_type  Specifies the cartesian or cylindrical
!>                                 coordinate type.
!>  @param[inout] signals          Array of pointers to the a signal
!>  @param[inout] signals_created  Number valid signals in the array, or index
!>                                 of the last signal created.
!>  @param[in]    observed         Array of observed values.
!>  @param[in]    sigma            Array of observed sigma values
!>  @param[in]    weight           Array of signal weight values
!>  @param[inout] first_index      Index of the first instance of an mse
!>                                 signal.
!>  @param[in]    in_degrees       Signal measures degrees if true.
!>  @param[in]    is_ratio         Signal measures ratio if true.
!-------------------------------------------------------------------------------
      SUBROUTINE mse_dot_parse_vec(mse_dot_file_ref, coordinate_type,          &
     &                             signals, signals_created,                   &
     &                             observed, sigma, weight,                    &
     &                             first_index, in_degrees, is_ratio)
      USE mse
      USE v3fit_input, only: v3fit_max_diagnostics,                            &
     &                       v3fit_input_find_scale_index,                     &
     &                       v3fit_input_find_offset_index
      USE v3_utilities, only: err_fatal
      USE coordinate_utilities

      IMPLICIT NONE

!  Declare Arguments
      TYPE (signal_dot_file), INTENT(inout)  :: mse_dot_file_ref
      CHARACTER (len=*), INTENT(in)          :: coordinate_type
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      INTEGER, INTENT(inout)                 :: signals_created
      REAL (rprec), DIMENSION(:), INTENT(in) :: observed
      REAL (rprec), DIMENSION(:), INTENT(in) :: sigma
      REAL (rprec), DIMENSION(:), INTENT(in) :: weight
      INTEGER, INTENT(inout)                 :: first_index
      LOGICAL, INTENT(in)                    :: in_degrees
      LOGICAL, INTENT(in)                    :: is_ratio

!  local variables
      CLASS (mse_class), POINTER             :: mse_obj
      REAL (rprec), DIMENSION(3)             :: xcart
      REAL (rprec), DIMENSION(3)             :: view_start
      REAL (rprec), DIMENSION(3)             :: view_end
      REAL (rprec), DIMENSION(3)             :: beam_start
      REAL (rprec), DIMENSION(3)             :: beam_end
      CHARACTER (len=data_short_name_length) :: point_name
      CHARACTER (len=signal_dot_line_len)    :: line
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Get the position and name of the point.
      CALL signal_dot_parse_chord(mse_dot_file_ref, coordinate_type,           &
     &                            point_name, xcart)

!  Get the start and end positons of the viewing chord.
      view_start = signal_dot_parse_3_real(mse_dot_file_ref,                   &
     &                                     'Failed to parse view ' //          &
     &                                     'start position.')
      view_end = signal_dot_parse_3_real(mse_dot_file_ref,                     &
     &                                   'Failed to parse view ' //            &
     &                                   'end position.')

!  Get the start and end positons of the beam chord.
      view_start = signal_dot_parse_3_real(mse_dot_file_ref,                   &
     &                                     'Failed to parse beam ' //          &
     &                                     'start position.')
      view_end = signal_dot_parse_3_real(mse_dot_file_ref,                     &
     &                                   'Failed to parse beam ' //            &
     &                                   'end position.')

!  Convert Coordinates if necessay.
      IF (TRIM(coordinate_type) .eq. 'RPhiDegZ') THEN
		 view_start(2) = view_start(2)*degree ! Convert from degrees to radians.
         view_start = cyl_to_cart(view_start)
         view_end(2) = view_end(2)*degree ! Convert from degrees to radians.
         view_end = cyl_to_cart(view_end)

         beam_start(2) = beam_start(2)*degree ! Convert from degrees to radians.
         beam_start = cyl_to_cart(beam_start)
         beam_end(2) = beam_end(2)*degree ! Convert from degrees to radians.
         view_end = cyl_to_cart(beam_end)
      END IF

      mse_obj => mse_class(xcart, view_start, view_end, beam_start,            &
     &                     beam_end, in_degrees, is_ratio)

      CALL signal_construct(mse_obj, point_name, point_name, 'arb',            &                                &
     &   observed(signals_created + 1), sigma(signals_created + 1),            &
     &   weight(signals_created + 1),                                          &
     &   v3fit_input_find_scale_index(signals_created + 1),                    &
     &   v3fit_input_find_offset_index(signals_created + 1))

      signals(signals_created + 1)%p => mse_obj

      signals_created = signals_created + 1

!  At lease one mse signal was made. Set the first index. This should only be
!  run once.
      IF (first_index .eq. -1) THEN
         first_index = signals_created
      END IF

      CALL profiler_set_stop_time('mse_dot_parse_vec', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Parse motional stark effect diagnostic point defined as angles.
!>
!>  Parses a signle point. Points are laid out as,
!>
!>  * chord_name
!>  * Point of measurement as three reals.
!>  * Angles with respect to the toroidal as two reals.
!>  * Angles with respect to the horizontal as two reals.
!>
!>  @param[in]    mse_dot_file_ref Reference to the mse. file.
!>  @param[in]    coordinate_type  Specifies the cartesian or cylindrical
!>                                 coordinate type.
!>  @param[inout] signals          Array of pointers to the a signal
!>  @param[inout] signals_created  Number valid signals in the array, or index
!>                                 of the last signal created.
!>  @param[in]    observed         Array of observed values.
!>  @param[in]    sigma            Array of observed sigma values
!>  @param[in]    weight           Array of signal weight values
!>  @param[inout] first_index      Index of the first instance of an mse
!>                                 signal.
!>  @param[in]    in_degrees1      Angles specified in degrees if true.
!>  @param[in]    in_degrees2      Signal measures degrees if true.
!>  @param[in]    is_ratio         Signal measures ratio if true.
!-------------------------------------------------------------------------------
      SUBROUTINE mse_dot_parse_ang(mse_dot_file_ref, coordinate_type,          &
     &                             signals, signals_created,                   &
     &                             observed, sigma, weight,                    &
     &                             first_index, in_degrees1,                   &
     &                             in_degrees2, is_ratio)
      USE mse
      USE v3fit_input, only: v3fit_max_diagnostics,                            &
     &                       v3fit_input_find_scale_index,                     &
     &                       v3fit_input_find_offset_index
      USE v3_utilities, only: err_fatal
      USE coordinate_utilities

      IMPLICIT NONE

!  Declare Arguments
      TYPE (signal_dot_file), INTENT(inout)  :: mse_dot_file_ref
      CHARACTER (len=*), INTENT(in)          :: coordinate_type
      TYPE (signal_pointer), DIMENSION(:), INTENT(inout) :: signals
      INTEGER, INTENT(inout)                 :: signals_created
      REAL (rprec), DIMENSION(:), INTENT(in) :: observed
      REAL (rprec), DIMENSION(:), INTENT(in) :: sigma
      REAL (rprec), DIMENSION(:), INTENT(in) :: weight
      INTEGER, INTENT(inout)                 :: first_index
      LOGICAL, INTENT(in)                    :: in_degrees1
      LOGICAL, INTENT(in)                    :: in_degrees2
      LOGICAL, INTENT(in)                    :: is_ratio

!  local variables
      CLASS (mse_class), POINTER             :: mse_obj
      REAL (rprec), DIMENSION(3)             :: xcart
      REAL (rprec), DIMENSION(2)             :: t_angles
      REAL (rprec), DIMENSION(2)             :: h_angles
      CHARACTER (len=data_short_name_length) :: point_name
      CHARACTER (len=signal_dot_line_len)    :: line
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Get the position and name of the point.
      CALL signal_dot_parse_chord(mse_dot_file_ref, coordinate_type,           &
     &                            point_name, xcart)

!  Get the alpha and omega.
      t_angles = signal_dot_parse_2_real(mse_dot_file_ref,                     &
     &                                   'Failed to parse angles ' //          &
     &                                   'to toroidal')

!  Get the delta and theta.
      h_angles = signal_dot_parse_2_real(mse_dot_file_ref,                     &
     &                                   'Failed to parse angles ' //          &
     &                                   'to horizontal')

!  Convert Coordinates if necessay.
      IF (in_degrees1) THEN
         t_angles = t_angles*degree
         h_angles = h_angles*degree
      END IF

      mse_obj => mse_class(xcart, t_angles(1), t_angles(2), h_angles(1),       &
     &             h_angles(2), in_degrees2, is_ratio)

      CALL signal_construct(mse_obj, point_name, point_name, 'arb',            &
     &   observed(signals_created + 1),                                        &
     &   sigma(signals_created + 1), weight(signals_created + 1),              &
     &   v3fit_input_find_scale_index(signals_created + 1),                    &
     &   v3fit_input_find_offset_index(signals_created + 1))

      signals(signals_created + 1)%p => mse_obj

      signals_created = signals_created + 1

!  At lease one mse signal was made. Set the first index. This should only be
!  run once.
      IF (first_index .eq. -1) THEN
         first_index = signals_created
      END IF

      CALL profiler_set_stop_time('mse_dot_parse_ang', start_time)

      END SUBROUTINE

      END MODULE

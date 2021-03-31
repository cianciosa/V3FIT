!*******************************************************************************
!>  @file signal_dot.f
!>  @brief Contains module @ref signal_dot
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of type @ref signal_dot_file. This module contains
!>  common code used in parsing diagnostic dot files. The actual parsing of the
!>  diagnostic dot file is handled by @ref ipch_dot, @ref thscte_dot and
!>  @ref sxrch_dot.
!*******************************************************************************

      MODULE signal_dot
      USE stel_kinds
      USE stel_constants
      USE data_parameters
      USE profiler

      IMPLICIT NONE

!*******************************************************************************
!  signal_dot module parameters
!*******************************************************************************
!>  Maximum line length.
      INTEGER, PARAMETER :: signal_dot_line_len = 200

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) signal_dot_file base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a diagnostic dot file. This is an opaqe class.
!-------------------------------------------------------------------------------
      TYPE signal_dot_file
         PRIVATE
!>  Input/output unit of the file.
         INTEGER :: iou
!>  Current line being parsed.
         INTEGER :: i_line
!>  Identify specifying the signal being parsed.
         CHARACTER (len=6) :: signal_id
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
      PUBLIC  :: signal_dot_open, signal_dot_close,                            &
     &           signal_dot_parse_chord, signal_dot_read_keyword,              &
     &           signal_dot_parse_real, signal_dot_read_line
      PRIVATE :: signal_dot_check_status

      CONTAINS
!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Open a diagnostic dot file.
!>
!>  Opens a diagnostic dot file for reading.
!>
!>  @param[in] file      Filename of the diagnostic dot file.
!>  @param[in] signal_id Description of the diagnostic dot file.
!>  @returns A constructed @ref signal_dot_file instance.
!-------------------------------------------------------------------------------
      FUNCTION signal_dot_open(file, signal_id)
      USE safe_open_mod

      IMPLICIT NONE

!  Declare Arguments
      TYPE(signal_dot_file)         :: signal_dot_open
      CHARACTER (len=*), INTENT(in) :: file
      CHARACTER (len=*), INTENT(in) :: signal_id

!  local variables
      INTEGER                       :: status
      REAL (rprec)                  :: start_time

!  local parameters
      INTEGER, PARAMETER            :: iou_0 = 32

!  Start of executable code
      start_time = profiler_get_start_time()

!  Open up the file
      signal_dot_open%iou       = iou_0
      signal_dot_open%signal_id = signal_id
      CALL safe_open(signal_dot_open%iou, status, file, 'old',                 &
     &               'formatted')
      CALL signal_dot_check_status(status, signal_dot_open,                    &
     &                             'Error opening ', ' file.')

      CALL profiler_set_stop_time('signal_dot_open', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Close a diagnostic dot file.
!>
!>  Closes a diagnostic dot file.
!>
!>  @param[in] signal_dot_file_ref An instance of a @ref signal_dot_file object.
!-------------------------------------------------------------------------------
      SUBROUTINE signal_dot_close(signal_dot_file_ref)

      IMPLICIT NONE

!  Declare Arguments
      TYPE(signal_dot_file), INTENT(in) :: signal_dot_file_ref

!  local variables
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Close the file
      CLOSE(signal_dot_file_ref%iou)

      CALL profiler_set_stop_time('signal_dot_close', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Parse a chord from a diagnostic dot file.
!>
!>  Chords start with the first line as the chord name. The it is followed by
!>  one or two lines of 3 coordinates. All coodinates for the cord are returned
!>  in carteasian coordinates.
!>
!>  @param[inout] signal_dot_file_ref An instance of a @ref signal_dot_file
!>                                    object.
!>  @param[in]    coordinate_type     Specifies if the coordinate is cartesian
!>                                    or cylindrical.
!>  @param[out]   chord_name          The name of the chord.
!>  @param[out]   xcart_i             The initial position of the chord in
!>                                    cartesian coordinates.
!>  @param[out]   xcart_f             The final position of the chord in
!>                                    cartesian coordinates.
!-------------------------------------------------------------------------------
      SUBROUTINE signal_dot_parse_chord(signal_dot_file_ref,                   &
     &                                  coordinate_type,                       &
     &                                  chord_name,                            &
     &                                  xcart_i, xcart_f)
      USE coordinate_utilities

      IMPLICIT NONE

!  Declare Arguments
      TYPE(signal_dot_file), INTENT(inout) :: signal_dot_file_ref
      CHARACTER (len=*), INTENT(in)        :: coordinate_type
      CHARACTER (len=data_short_name_length), INTENT(out) :: chord_name
      REAL(rprec), DIMENSION(3), INTENT(out)              :: xcart_i
      REAL(rprec), DIMENSION(3), OPTIONAL, INTENT(out)    :: xcart_f

!  local variables
      INTEGER                              :: status
      CHARACTER (len=signal_dot_line_len)  :: line
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Read the chord identification.
      line = signal_dot_read_line(signal_dot_file_ref,                         &
     &                            'Error reading ID for ', ' chord.')
      chord_name = TRIM(line)

!  Read the start position.
	  xcart_i = signal_dot_parse_3_real(signal_dot_file_ref,                   &
     &                                  'Error reading start ' //              &
     &                                  'position for chord.')

      IF (PRESENT(xcart_f)) THEN
!  Read the end position.
         xcart_f = signal_dot_parse_3_real(signal_dot_file_ref,                &
     &                                    'Error reading end ' //              &
     &                                    'position for chord.')
      END IF

!  Done reading information for this chord. Generate the chord
      IF (TRIM(coordinate_type) .eq. 'RPHiDegZ') THEN
         xcart_i(2) = xcart_i(2)*degree ! Convert from degrees to radians.
         xcart_i = cyl_to_cart(xcart_i)

         IF (PRESENT(xcart_f)) THEN
            xcart_f(2) = xcart_f(2)*degree  ! Convert from degrees to radians.
            xcart_f = cyl_to_cart(xcart_f)
         END IF
      END IF

      CALL profiler_set_stop_time('signal_dot_parse_chord', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Parse a single real from a diagnostic dot file.
!>
!>  Reads a single real from the diagnostic dot file. This is used to be able to
!>  get extra information about a diagnostic in the diagnostic dot file.
!>
!>  @param[inout] signal_dot_file_ref An instance of a @ref signal_dot_file
!>                                    object.
!>  @param[in]    message             Error message to report back if there was
!>                                    a problem.
!>  @returns The parsed real value.
!-------------------------------------------------------------------------------
      FUNCTION signal_dot_parse_real(signal_dot_file_ref, message)
      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                          :: signal_dot_parse_real
      TYPE (signal_dot_file), INTENT(inout) :: signal_dot_file_ref
      CHARACTER (len=*), INTENT(in)         :: message

!  local variables
      INTEGER                               :: status
      CHARACTER (len=signal_dot_line_len)   :: line
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Read the start position.
      line = signal_dot_read_line(signal_dot_file_ref,                         &
     &                            'Error ', message)
!  Reread the line, looking for a single real
      READ(line, *, iostat=status) signal_dot_parse_real
      CALL signal_dot_check_status(status, signal_dot_file_ref,                &
     &                             'Error ', message)

      CALL profiler_set_stop_time('signal_dot_parse_real', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Parse a two reals from a diagnostic dot file.
!>
!>  Reads three reals from the diagnostic dot file. This is used to be able to
!>  get extra information about a diagnostic in the diagnostic dot file.
!>
!>  @param[inout] signal_dot_file_ref An instance of a @ref signal_dot_file
!>                                    object.
!>  @param[in]    message             Error message to report back if there was
!>                                    a problem.
!>  @returns The parsed real values.
!-------------------------------------------------------------------------------
      FUNCTION signal_dot_parse_2_real(signal_dot_file_ref, message)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(2)            :: signal_dot_parse_2_real
      TYPE (signal_dot_file), INTENT(inout) :: signal_dot_file_ref
      CHARACTER (len=*), INTENT(in)         :: message

!  local variables
      INTEGER                               :: status
      CHARACTER (len=signal_dot_line_len)   :: line
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Read the start position.
      line = signal_dot_read_line(signal_dot_file_ref,                         &
     &                            'Error ', message)
!  Reread the line, looking for three reals
      READ(line, *, iostat=status) signal_dot_parse_2_real
      CALL signal_dot_check_status(status, signal_dot_file_ref,                &
     &                             'Error ', message)

      CALL profiler_set_stop_time('signal_dot_parse_2_real', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Parse a three reals from a diagnostic dot file.
!>
!>  Reads three reals from the diagnostic dot file. This is used to be able to
!>  get extra information about a diagnostic in the diagnostic dot file.
!>
!>  @param[inout] signal_dot_file_ref An instance of a @ref signal_dot_file
!>                                    object.
!>  @param[in]    message             Error message to report back if there was
!>                                    a problem.
!>  @returns The parsed real values.
!-------------------------------------------------------------------------------
      FUNCTION signal_dot_parse_3_real(signal_dot_file_ref, message)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec), DIMENSION(3)            :: signal_dot_parse_3_real
      TYPE (signal_dot_file), INTENT(inout) :: signal_dot_file_ref
      CHARACTER (len=*), INTENT(in)         :: message

!  local variables
      INTEGER                               :: status
      CHARACTER (len=signal_dot_line_len)   :: line
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Read the start position.
      line = signal_dot_read_line(signal_dot_file_ref,                         &
     &                            'Error ', message)
!  Reread the line, looking for three reals
      READ(line, *, iostat=status) signal_dot_parse_3_real
      CALL signal_dot_check_status(status, signal_dot_file_ref,                &
     &                             'Error ', message)

      CALL profiler_set_stop_time('signal_dot_parse_3_real', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Parse a single integer from a diagnostic dot file.
!>
!>  Reads a single integer from the diagnostic dot file. This is used to be able
!>  to get extra information about a diagnostic in the diagnostic dot file.
!>
!>  @param[inout] signal_dot_file_ref An instance of a @ref signal_dot_file
!>                                    object.
!>  @param[in]    message             Error message to report back if there was
!>                                    a problem.
!>  @returns The parsed integer value.
!-------------------------------------------------------------------------------
      FUNCTION signal_dot_parse_int(signal_dot_file_ref, message)
      IMPLICIT NONE

!  Declare Arguments
      INTEGER                               :: signal_dot_parse_int
      TYPE (signal_dot_file), INTENT(inout) :: signal_dot_file_ref
      CHARACTER (len=*), INTENT(in)         :: message

!  local variables
      INTEGER                               :: status
      CHARACTER (len=signal_dot_line_len)   :: line
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Read the start position.
      line = adjustl(signal_dot_read_line(signal_dot_file_ref,                 &
     &                                    'Error ', message))
!  Reread the line, looking for a single real
      READ(line, *, iostat=status) signal_dot_parse_int
      CALL signal_dot_check_status(status, signal_dot_file_ref,                &
     &                             'Error ', message)

      CALL profiler_set_stop_time('signal_dot_parse_int', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Parse two integers from a diagnostic dot file.
!>
!>  Reads two integers from the diagnostic dot file. This is used to be able to
!>  get extra information about a diagnostic in the diagnostic dot file.
!>
!>  @param[inout] signal_dot_file_ref An instance of a @ref signal_dot_file
!>                                    object.
!>  @param[in]    message             Error message to report back if there was
!>                                    a problem.
!>  @returns The parsed real value.
!-------------------------------------------------------------------------------
      FUNCTION signal_dot_parse_2_int(signal_dot_file_ref, message)
      IMPLICIT NONE

!  Declare Arguments
      INTEGER, DIMENSION(2)                 :: signal_dot_parse_2_int
      TYPE (signal_dot_file), INTENT(inout) :: signal_dot_file_ref
      CHARACTER (len=*), INTENT(in)         :: message

!  local variables
      INTEGER                               :: status
      CHARACTER (len=signal_dot_line_len)   :: line
      REAL (rprec)                          :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Read the start position.
      line = signal_dot_read_line(signal_dot_file_ref,                         &
     &                            'Error ', message)
!  Reread the line, looking for a single real
      READ(line, *, iostat=status) signal_dot_parse_2_int
      CALL signal_dot_check_status(status, signal_dot_file_ref,                &
     &                             'Error ', message)

      CALL profiler_set_stop_time('signal_dot_parse_2_int', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Read a keyword from the diagnostic dot file.
!>
!>  Read lines until a valid keyword is reached ot the end of the file is
!>  reached.
!>
!>  @param[inout] signal_dot_file_ref An instance of a @ref signal_dot_file
!>                                    object.
!>  @param[in]    keywords            List of keywords to search for.
!>  @returns The found keyword.
!-------------------------------------------------------------------------------
      FUNCTION signal_dot_read_keyword(signal_dot_file_ref, keywords)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length)     :: signal_dot_read_keyword
      TYPE(signal_dot_file), INTENT(inout) :: signal_dot_file_ref
      CHARACTER (len=*), DIMENSION(:), INTENT(in) :: keywords

!  local variables
      INTEGER                              :: i
      CHARACTER (len=signal_dot_line_len)  :: line
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Contine looping until a vaild keyword is read. If the end of the file is
!  reached an error will kill the program.
      DO
         line = signal_dot_read_line(signal_dot_file_ref,                      &
     &                               'Failed to read ',                        &
     &                               ' keyword before end of file.')
         DO i = 1, SIZE(keywords)
             IF (TRIM(ADJUSTL(line)) .eq. TRIM(keywords(i))) THEN
                signal_dot_read_keyword = keywords(i)

                CALL profiler_set_stop_time('signal_dot_read_keyword',         &
     &                                      start_time)

                RETURN
             END IF
         END DO
      END DO

      CALL profiler_set_stop_time('signal_dot_read_keyword', start_time)

      END FUNCTION

!*******************************************************************************
!  Private Functions and Subroutines.
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Read line from diagnostic dot file.
!>
!>  Reads single line from the diagnostic dot file.
!>
!>  @param[inout] signal_dot_file_ref An instance of a @ref signal_dot_file
!>                                    object.
!>  @param[in]    pre_message         Prefix message incase of error.
!>  @param[in]    post_message        Post message incase of error.
!>  @returns The line that was read.
!-------------------------------------------------------------------------------
      FUNCTION signal_dot_read_line(signal_dot_file_ref,                       &
     &                              pre_message,                               &
     &                              post_message)
      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=signal_dot_line_len)  :: signal_dot_read_line
      TYPE(signal_dot_file), INTENT(inout) :: signal_dot_file_ref
      CHARACTER (len=*), INTENT(in)        :: pre_message
      CHARACTER (len=*), INTENT(in)        :: post_message

!  local variables
      INTEGER                              :: status
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      READ (signal_dot_file_ref%iou, '(a)', iostat=status)                     &
     &   signal_dot_read_line
      signal_dot_file_ref%i_line = signal_dot_file_ref%i_line + 1
      CALL signal_dot_check_status(status, signal_dot_file_ref,                &
     &                             pre_message, post_message)

      CALL profiler_set_stop_time('signal_dot_read_line', start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Check and handle errors.
!>
!>  Checks the staus for an error condition. If there was a error, report it.
!>  Error messages are formatted as:
!>
!>  pre_message @ref signal_dot_file::signal_id post message
!>  @ref signal_dot_file::i_line
!>
!>  @param[in]    status              The error code to check.
!>  @param[inout] signal_dot_file_ref An instance of a @ref signal_dot_file
!>                                    object.
!>  @param[in]    pre_message         Prefix message incase of error.
!>  @param[in]    post_message        Post message incase of error.
!>  @returns The line that was read.
!-------------------------------------------------------------------------------
      SUBROUTINE signal_dot_check_status(status,                               &
     &                                   signal_dot_file_ref,                  &
     &                                   pre_message,                          &
     &                                   post_message)
      IMPLICIT NONE

!  Declare Arguments
      INTEGER, INTENT(in)                  :: status
      TYPE(signal_dot_file), INTENT(inout) :: signal_dot_file_ref
      CHARACTER (len=*), INTENT(in)        :: pre_message
      CHARACTER (len=*), INTENT(in)        :: post_message

!  local variables
      REAL (rprec)                         :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (status .ne. 0) THEN
         WRITE(*,*) pre_message // TRIM(signal_dot_file_ref%signal_id)         &
     &              // post_message // ' line ',                               &
     &              signal_dot_file_ref%i_line
         STOP
      END IF

      CALL profiler_set_stop_time('signal_dot_check_status', start_time)

      END SUBROUTINE

      END MODULE

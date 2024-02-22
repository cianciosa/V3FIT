!-------------------------------------------------------------------------------
!  The @header2, @begin_table, @item3 and @end_table commands are custom defined
!  commands in Doxygen.in. They are defined under ALIASES. For the page created
!  here, the 80 column limit is exceeded. Arguments of aliases are separated by
!  ','. If you intended ',' to be a string you must use an escaped comma '\,'.
!
!>  @page cl_parsing_sec Command Line Arguments
!>
!>  @tableofcontents
!>
!>  @section cl_parsing_intro Introduction
!>  This contains a description of the command line arguments. All arguments
!>  take the form of
!>
!>  @fixed_width{-arg=value}
!>
!>  If no @fixed_width{-arg=} is found, then the @fixed_width{-file} is implied.
!>  In other words, a command line of
!>
!>  @fixed_width{xv3fit input.file}
!>
!>  is the same as
!>
!>  @fixed_width{xv3fit -file=input.file}
!>
!>  @section cl_parsing_arg_sec Command Line Arguments
!>  @header2{Argument, Takes Value, Discription}
!>  @begin_table
!>     @item3{@fixed_width{-h},       N, Displays the help text and exits the program.}
!>     @item3{@fixed_width{-d},       N, Use the default namelist input file. The default input filename is
!>                                       @fixed_width{v3fit.in}.}
!>     @item3{@fixed_width{-file},    Y, Specify the v3fit namelist input file.}
!>     @item3{@fixed_width{-test},    N, Overwrites the @ref v3fit_input::my_task variable and runs the internal
!>                                       unit tests instead.}
!>     @item3{@fixed_width{-force},   N, Forces the equilibrium to re-solve on every reconstruction parameter}
!>     @item3{@fixed_width{-para},    Y, Configures openmp parallelism. The value of this flag sets the maximum
!>                                       number of threads to use. A value of @fixed_width{-1} uses the default.}
!>     @item3{@fixed_width{-out},     N, Writes out the input file for each reconstruction step.}
!>     @item3{@fixed_width{-c},       Y, Compress magnetic response functions using the cutoff.}
!>     @item3{@fixed_width{-c_diff},  N, Use central differencing to compute the Jacobian.}
!>     @item3{@fixed_width{-serial},  N, Run the reconstruction serially and with parallel equilibria.}
!>     @item3{@fixed_width{-restart}, Y, Specify v3fit to restart from the provided result file. Must have
!>                                       the last valid wout and/or siesta restart file.}
!>
!>  @end_table
!>
!>  @note
!>  OpenMP and MPI support must be configured at compile time to use. A simple way to check if support has been
!>  included is to check the command line help @fixed_width{-h}.
!>
!>  @section cl_pasring_prog_ref_sec Programmers Reference
!>  Reference material for the coding to implement command line parsing is found
!>  in the @ref commandline_parser module.
!-------------------------------------------------------------------------------
!*******************************************************************************
!>  @file commandline_parser.f
!>  @brief Contains module @ref commandline_parser
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref commandline_parser_class.
!*******************************************************************************

      MODULE commandline_parser
      USE stel_kinds
      USE file_opts, only: path_length
      USE profiler

      IMPLICIT NONE

!*******************************************************************************
!  commandline parser module parameters
!*******************************************************************************
!>  Maximum length of the argument including the '-' character.
      INTEGER, PARAMETER, PRIVATE :: max_arg_length = 8
!>  Maximum length of the complete flag. All command line flags take the form of
!>  '-flag=value'.
      INTEGER, PARAMETER, PRIVATE :: max_length = path_length                  &
     &                                          + max_arg_length + 1

!  Commandline parser error codes.
!>  Commandline argument not found.
      INTEGER, PARAMETER :: commandline_parser_no_error      = 0
!>  Commandline argument not found.
      INTEGER, PARAMETER :: commandline_parser_arg_not_found = -1

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) commandline parser base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class containing a parsed commandline.
!-------------------------------------------------------------------------------
      TYPE :: commandline_parser_class
!>  Command name of current process.
         CHARACTER (len=max_length) :: command
!>  Array of arguments. An argument is the form of -flag
         CHARACTER (len=max_arg_length), DIMENSION(:), POINTER ::              &
     &      arg => null()
!>  Array of value corresponding to the flag. A value is everything after the
!>  '=' character. The value maybe blank indicating there was no value provided.
         CHARACTER (len=path_length), DIMENSION(:), POINTER ::                 &
     &      value => null()
      CONTAINS
         FINAL     :: commandline_parser_destruct
         PROCEDURE :: get_string => commandline_parser_get_string
         PROCEDURE :: get_integer => commandline_parser_get_integer
         PROCEDURE :: get_real => commandline_parser_get_real
         PROCEDURE :: is_flag_set => commandline_parser_is_flag_set
      END TYPE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref commandline_parser_class object.
!>
!>  Allocates memory and initializes a @ref commandline_parser_class object. The
!>  command line is parsed assuming the the flags take the form of -flag=value.
!>  If the '=' is missing, it is assumed there is no value. If the '-' is
!>  missing the flag is treated as the value to an implicit -file flag. If -h is
!>  is found, the help text is printed out and the program terminated.
!>
!>  @returns A pointer to a constructed @ref commandline_parser_class object.
!-------------------------------------------------------------------------------
      FUNCTION commandline_parser_construct()

      IMPLICIT NONE

!  Declare Arguments
      CLASS (commandline_parser_class), POINTER ::                             &
     &   commandline_parser_construct

!  local variables
      CHARACTER (len=max_length) :: temp
      INTEGER                    :: num_args, i, value_index
      REAL (rprec)               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      temp = ''
      num_args = 0
      value_index = 0
      ALLOCATE(commandline_parser_construct)

!  Read the zeroith arg to get the number of arguments. This should also be the
!  command name.
      CALL getcarg(0, commandline_parser_construct%command, num_args)

      IF (num_args .le. 0) THEN
         CALL commandline_parser_print_help
      END IF

!  Allocate the arrays and
      ALLOCATE(commandline_parser_construct%arg(num_args))
      ALLOCATE(commandline_parser_construct%value(num_args))

!  Loop through the command line arguments, and setup the argument and value
!  arrays
      DO i = 1, num_args
         CALL getcarg(i, temp, num_args)

!  Check for a - as the first character.
         IF (temp(1:1) .eq. '-') THEN
            value_index = INDEX(temp, '=')
            IF (value_index .eq. 0) THEN
!  Check for help command.
               IF (TRIM(temp) .eq. '-h') THEN
                  CALL commandline_parser_print_help
               END IF

               commandline_parser_construct%arg(i) = TRIM(temp)
               commandline_parser_construct%value(i) = ''
            ELSE
               commandline_parser_construct%arg(i) =                           &
     &            temp(1:value_index - 1)
               commandline_parser_construct%value(i) =                         &
     &            temp(value_index + 1:LEN_TRIM(temp))
            END IF
         ELSE
!  Implicity set the argument to -file
            commandline_parser_construct%arg(i) = '-file'
            commandline_parser_construct%value(i) =                            &
     &         temp(1:path_length)
         END IF
      END DO

      CALL profiler_set_stop_time('commandline_parser_construct',              &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref commandline_parser_class object.
!>
!>  Deallocates memory and uninitializes a @ref commandline_parser_class object.
!>
!>  @param[inout] this A @ref commandline_parser_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE commandline_parser_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (commandline_parser_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%arg)) THEN
         DEALLOCATE(this%arg)
         this%arg => null()
      END IF

      IF (ASSOCIATED(this%value)) THEN
         DEALLOCATE(this%value)
         this%value => null()
      END IF

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Get the value of an argument as a string.
!>
!>  Searches the parsed command line flags for a matching command like argument.
!>  If no matching argument is found or no value was set, a blank string is
!>  returned. command line arguments take the form of -flag.
!>
!>  @param[in] this A @ref commandline_parser_class instance.
!>  @param[in] arg  A argument string to search for.
!>  @returns The argument value.
!-------------------------------------------------------------------------------
      FUNCTION commandline_parser_get_string(this, arg)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=path_length) :: commandline_parser_get_string
      CLASS (commandline_parser_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)                :: arg

!  Local arguments
      INTEGER                                      :: i
      REAL (rprec)                                 :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Loop through the arguments until the correct arg is found.
      commandline_parser_get_string = ''

      IF (ASSOCIATED(this%arg)) THEN
         DO i = 1, SIZE(this%arg)
            IF (TRIM(this%arg(i)) .eq. TRIM(arg)) THEN
               commandline_parser_get_string = this%value(i)

               CALL profiler_set_stop_time(                                    &
     &                 'commandline_parser_get_string', start_time)

               RETURN
            END IF
         END DO
      END IF

      CALL profiler_set_stop_time('commandline_parser_get_string',             &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the value of an argument as an integer.
!>
!>  Searches the parsed command line flags for a matching command like argument.
!>  If no matching argument is found, no value was set or the value cannot be
!>  converted to an integer, the default value is returned. Command line
!>  arguments take the form of -flag.
!>
!>  @param[in] this          A @ref commandline_parser_class instance.
!>  @param[in] arg           A argument string to search for.
!>  @param[in] default_value Default value in case flag is not set.
!>  @returns The argument value.
!-------------------------------------------------------------------------------
      FUNCTION commandline_parser_get_integer(this, arg,                       &
     &                                        default_value)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: commandline_parser_get_integer
      CLASS (commandline_parser_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)                :: arg
      INTEGER, INTENT(in)                          :: default_value

!  Local arguments
      CHARACTER (len=path_length)                  :: value
      INTEGER                                      :: status
      REAL (rprec)                                 :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      value = commandline_parser_get_string(this, arg)

      IF (TRIM(value) .eq. '') THEN
         commandline_parser_get_integer = default_value

         CALL profiler_set_stop_time('commandline_parser_get_integer',         &
     &                               start_time)

         RETURN
      END IF

      READ (value,1000,iostat=status) commandline_parser_get_integer

      IF (status .ne. 0) THEN
         commandline_parser_get_integer = default_value
      END IF

      CALL profiler_set_stop_time('commandline_parser_get_integer',            &
     &                            start_time)

1000  FORMAT(i20)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the value of an argument as a Real.
!>
!>  Searches the parsed command line flags for a matching command like argument.
!>  If no matching argument is found, no value was set or the value cannot be
!>  converted to an integer, the default value is returned. Command line
!>  arguments take the form of -flag.
!>
!>  @param[in] this          A @ref commandline_parser_class instance.
!>  @param[in] arg           A argument string to search for.
!>  @param[in] default_value Default value in case flag is not set.
!>  @returns The argument value.
!-------------------------------------------------------------------------------
      FUNCTION commandline_parser_get_real(this, arg, default_value)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: commandline_parser_get_real
      CLASS (commandline_parser_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)                :: arg
      REAL (rprec), INTENT(in)                     :: default_value

!  Local arguments
      CHARACTER (len=path_length)                  :: value
      INTEGER                                      :: status
      REAL (rprec)                                 :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      value = commandline_parser_get_string(this, arg)

      IF (TRIM(value) .eq. '') THEN
         commandline_parser_get_real = default_value

         CALL profiler_set_stop_time('commandline_parser_get_real',            &
     &                               start_time)

         RETURN
      END IF

      READ (value,*,iostat=status) commandline_parser_get_real

      IF (status .ne. 0) THEN
         commandline_parser_get_real = default_value
      END IF

      CALL profiler_set_stop_time('commandline_parser_get_real',               &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  QUERY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Check if a command line argument was set.
!>
!>  Searches the parsed command line flags for a matching command line argument.
!>  If no matching argument is found the flag was not set.
!>
!>  @param[in] this A @ref commandline_parser_class instance.
!>  @param[in] arg  A argument string to search for.
!>  @returns True if the argument was found, false if it was not found.
!-------------------------------------------------------------------------------
      FUNCTION commandline_parser_is_flag_set(this, arg)

      IMPLICIT NONE

!  Declare Arguments
      LOGICAL :: commandline_parser_is_flag_set
      CLASS (commandline_parser_class), INTENT(in) :: this
      CHARACTER (len=*), INTENT(in)                :: arg

!  Local arguments
      INTEGER                                      :: i
      REAL (rprec)                                 :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Loop through the arguments until the correct arg is found.
      commandline_parser_is_flag_set = .false.

      IF (ASSOCIATED(this%arg)) THEN
         DO i = 1, SIZE(this%arg)
            IF (TRIM(this%arg(i)) .eq. TRIM(arg)) THEN
               commandline_parser_is_flag_set = .true.

               CALL profiler_set_stop_time(                                    &
     &                 'commandline_parser_is_flag_set', start_time)

               RETURN
            END IF
         END DO
      END IF

      CALL profiler_set_stop_time('commandline_parser_is_flag_set',            &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Print out help text.
!>
!>  Command line help text should take the form of '-flag' that is maximum
!>  length of @ref max_arg_length followed by a space. A Y or N indicating the
!>  flag takes a value or not followed by a space. A short message describing
!>  the flag. A reference for all the command line arguments can be found at
!>  @ref cl_parsing_sec.
!-------------------------------------------------------------------------------
      SUBROUTINE commandline_parser_print_help

      IMPLICIT NONE

!  Start of executable code
!  All command line messages need to fit within this width.
!                '           s              c                          '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '                        V3FIT                        '
      WRITE(*,*) '                                                     '
      WRITE(*,*) 'Configured with:                                     '
!$    WRITE(*,*) '  OpenMP support                                     '
#if defined(MPI_OPT)
      WRITE(*,*) '  MPI support                                        '
#endif
      WRITE(*,*) '                                                     '
      WRITE(*,*) 'Usage: xv3fit [-arg][=option] ...                    '
      WRITE(*,*) '                                                     '
      WRITE(*,*) 'Options:                                             '
      WRITE(*,*) 'All options are displayed as [arg][takesoption][text]'
      WRITE(*,*) '  -h       N Display this information                '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -d       N Use default namelist input file         '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -file    Y Specify the v3fit namelist input file   '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -test    N Run unit tests                          '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -force   N Forces the equilibrium to resolve on    '
      WRITE(*,*) '             every reconstruction parameter.         '
      WRITE(*,*) '                                                     '
!$    WRITE(*,*) '  -para    Y Determines number of parallel insatnces '
!$    WRITE(*,*) '             to run with. A value of -1 means use the'
!$    WRITE(*,*) '             default number of threads.              '
!$    WRITE(*,*) '                                                     '
      WRITE(*,*) '  -out     N Write out the input file and at each    '
      WRITE(*,*) '             step.                                   '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -c       Y Compress magnetic response functions    '
      WRITE(*,*) '             using the cutoff. Overwrites the value  '
      WRITE(*,*) '             in the namelist input.                  '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -c_diff  N Use central differencing to compute the '
      WRITE(*,*) '             Jacobian.                               '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -serial  N Run the reconstruction serially and with'
      WRITE(*,*) '             parallel equilibria.                    '
      WRITE(*,*) '                                                     '
      WRITE(*,*) '  -restart Y Specify v3fit to restart from the       '
      WRITE(*,*) '             provided result file. Must have the last'
      WRITE(*,*) '             valid wout and/or siesta restart file.  '
      WRITE(*,*) '                                                     '
      WRITE(*,*) 'Notes:                                               '
      WRITE(*,*) '  If no -arg is found -file is implicitly implied.   '
      WRITE(*,*) 'The default input filename is v3fit.in.              '
      WRITE(*,*) '                                                     '

      STOP

      END SUBROUTINE

      END MODULE

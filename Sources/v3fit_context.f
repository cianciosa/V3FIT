!*******************************************************************************
!>  @file v3fit_context.f
!>  @brief Contains module @ref v3fit_context.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines a @ref v3fit_context_class object to contain all the memory for
!>  running v3fit. Contains methods to write memory to disk.
!*******************************************************************************

      MODULE v3fit_context
      USE v3fit_input
      USE v3fit_params
      USE commandline_parser
      USE reconstruction
      USE ezcdf

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) v3fit context
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a v3fit context. This contains all memory needed to
!>  operate v3fit.
!-------------------------------------------------------------------------------
      TYPE :: v3fit_context_class
!>  Stopping criteria for change in g^2.
         REAL (rprec)                                 :: recon_stop

!>  File input output unit for the runlog file. This file is a human readable
!>  output history of a v3fit run.
         INTEGER                                      :: runlog_iou
!>  File input output unit for the recout file. This file is a human readable
!>  output of the final result.
         INTEGER                                      :: recout_iou

!>  The parsed command line options.
         CLASS (commandline_parser_class), POINTER    ::                       &
     &      cl_parser => null()
!>  The equilibrium model.
         CLASS (model_class), POINTER                 :: model => null()
!>  Guassian process models.
         TYPE (gaussp_class_pointer), DIMENSION(:), POINTER ::                 &
     &      gp => null()
!>  Array of signals.
!>  @see signal
         TYPE (signal_pointer), DIMENSION(:), POINTER ::                       &
     &      signals => null()
!>  Array of derived parameters.
!>  @see param
         TYPE (param_pointer), DIMENSION(:), POINTER  ::                       &
     &      derived_params => null()
!>  Arrays of reconstruction parameters.
!>  @see param
         TYPE (param_pointer), DIMENSION(:), POINTER :: params => null()
!>  Array of locking parameters.
         TYPE (param_pointer), DIMENSION(:), POINTER  :: locks => null()
!>  The reconstruction algorithm object.
         CLASS (reconstruction_class), POINTER        :: recon => null()

!  The default for the following index is -1 indicating that this signal hasn't
!  been created.
!>  Index of the first instance of a @ref magnetic signal. The default -1
!>  represents no @ref magnetic signal created.
         INTEGER   :: magnetic_index = -1
!>  Index of the first instance of a @ref sxrem signal. The default -1
!>  represents no @ref sxrem signal created.
         INTEGER   :: sxrem_index = -1
!>  Index of the first instance of a @ref intpol signal. The default -1
!>  represents no @ref intpol signal created.
         INTEGER   :: intpol_index = -1
!>  Index of the first instance of a @ref thomson signal. The default -1
!>  represents no @ref thomson signal created.
         INTEGER   :: thomson_index = -1
!>  Index of the first instance of a @ref extcurz signal. The default -1
!>  represents no @ref extcurz signal created.
         INTEGER   :: extcurz_index = -1
!>  Index of the first instance of a @ref mse signal. The default -1
!>  represents no @ref mse signal created.
         INTEGER   :: mse_index = -1
!>  Index of the first instance of a @ref ece signal. The default -1
!>  represents no @ref ece signal created.
         INTEGER   :: ece_index = -1
!>  Index of the first instance of a @ref limiter signal. The default -1
!>  represents no @ref limiter signal created.
         INTEGER   :: limiter_index = -1
!>  Index of the first instance of a @ref prior_gaussian signal. The default -1
!>  represents no @ref prior_gaussian signal created.
         INTEGER   :: prior_gaussian_index = -1
!>  Index of the first instance of a @ref prior_gaussian signal. The default -1
!>  represents no @ref prior_gaussian signal created.
         INTEGER   :: sxrem_ratio_index = -1
!>  Index of the first instance of a @ref combination_class signal. The default
!>  -1 represents no @ref combination_class signal created.
         INTEGER   :: combination_index = -1

!>  NetCDF result file to write out the results in machine readable form.
         INTEGER   :: result_ncid

!--- MPI -----------------------------------------------------------------------
#if defined (MPI_OPT)
!>  MPI communicator reference for full domain.
         INTEGER   :: global_comm = MPI_COMM_NULL
!>  MPI communicator reference for the equilibrium domain.
         INTEGER   :: equilibrium_comm = MPI_COMM_NULL
!>  MPI communicator reference for the reconstruction domain.
         INTEGER   :: reconstruction_comm = MPI_COMM_NULL
#endif
      CONTAINS
         FINAL     :: v3fit_context_destruct
         PROCEDURE :: resize => v3fit_context_resize
         PROCEDURE :: create_files => v3fit_context_create_files
         PROCEDURE :: close_files => v3fit_context_close_files
         PROCEDURE :: write => v3fit_context_write
         PROCEDURE ::                                                          &
     &      write_param_header => v3fit_context_write_param_header
         PROCEDURE :: init_data => v3fit_context_init_data
         PROCEDURE :: write_step_data => v3fit_context_write_step_data
         PROCEDURE :: restart => v3fit_context_restart
         PROCEDURE :: get_eq_comm => v3fit_context_get_eq_comm
         PROCEDURE :: get_eq_rank => v3fit_context_get_eq_rank
         PROCEDURE :: get_recon_comm => v3fit_context_get_recon_comm
         PROCEDURE :: get_recon_rank => v3fit_context_get_recon_rank
      END TYPE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref v3fit_context_class object.
!>
!>  Allocates memory and initializes a @ref v3fit_context_class object with an
!>  instance of a @ref commandline_parser object. This opens files for the
!>  recout, runlog, and result files. Over allocates space for the number of
!>  signals.
!>
!>  @param[in] cl_parser An instance of a commandline parser object.
!>  @returns A pointer to a constructed @ref v3fit_context_class object.
!-------------------------------------------------------------------------------
      FUNCTION v3fit_context_construct(cl_parser)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), POINTER :: v3fit_context_construct
      CLASS (commandline_parser_class), POINTER :: cl_parser

!  local variables
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(v3fit_context_construct)

      v3fit_context_construct%cl_parser => cl_parser

      CALL profiler_set_stop_time('v3fit_context_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref v3fit_context_class object.
!>
!>  Deallocates memory and uninitializes a @ref v3fit_context_class object. This
!>  all the v3fit constructed objects are destroyed here.
!>
!>  @param[inout] this A @ref v3fit_context_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE v3fit_context_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: this

!  local variables
      INTEGER                                   :: i

!  Start of executable code
      IF (ASSOCIATED(this%cl_parser)) THEN
         DEALLOCATE(this%cl_parser)
         this%cl_parser => null()
      END IF

      IF (ASSOCIATED(this%model)) THEN
         DEALLOCATE(this%model)
         this%model => null()
      END IF

!  Deconstruct and deallocate all the signals.
      IF (ASSOCIATED(this%gp)) THEN
         DO i = 1, SIZE(this%gp)
            IF (ASSOCIATED(this%gp(i)%p)) THEN
               CALL gaussp_destruct(this%gp(i)%p)
               this%gp(i)%p => null()
            END IF
         END DO
         DEALLOCATE(this%gp)
         this%signals => null()
      END IF

!  Deconstruct and deallocate all the signals.
      IF (ASSOCIATED(this%signals)) THEN
         DO i = 1, SIZE(this%signals)
            IF (ASSOCIATED(this%signals(i)%p)) THEN
               DEALLOCATE(this%signals(i)%p)
               this%signals(i)%p => null()
            END IF
         END DO
         DEALLOCATE(this%signals)
         this%signals => null()
      END IF

!  Deconstruct and deallocate all the derived parameters.
      IF (ASSOCIATED(this%derived_params)) THEN
         DO i = 1, SIZE(this%derived_params)
            IF (ASSOCIATED(this%derived_params(i)%p)) THEN
               CALL param_destruct(this%derived_params(i)%p)
               this%derived_params(i)%p => null()
            END IF
         END DO
         DEALLOCATE(this%derived_params)
         this%derived_params => null()
      END IF

!  Deconstruct and deallocate all the parameters.
      IF (ASSOCIATED(this%params)) THEN
         DO i = 1, SIZE(this%params)
            IF (ASSOCIATED(this%params(i)%p)) THEN
               CALL param_destruct(this%params(i)%p)
               this%params(i)%p => null()
            END IF
         END DO
         DEALLOCATE(this%params)
         this%params => null()
      END IF

!  Deconstruct and deallocate all the parameters.
      IF (ASSOCIATED(this%params)) THEN
         DO i = 1, SIZE(this%locks)
            IF (ASSOCIATED(this%locks(i)%p)) THEN
               CALL param_destruct(this%locks(i)%p)
               this%locks(i)%p => null()
            END IF
         END DO
         DEALLOCATE(this%locks)
         this%locks => null()
      END IF

      IF (ASSOCIATED(this%recon)) THEN
         DEALLOCATE(this%recon)
         this%recon => null()
      END IF

!  Reset the signal index
      this%prior_gaussian_index = -1
      this%magnetic_index = -1
      this%sxrem_index = -1
      this%intpol_index = -1
      this%thomson_index = -1
      this%extcurz_index = -1
	  this%mse_index = -1
	  this%ece_index = -1
      this%limiter_index = -1
      this%sxrem_ratio_index = -1
      this%combination_index = -1

      END SUBROUTINE

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Resize the arrays.
!>
!>  Resizes the signal arrays to contain only the minimun necessary to hold all
!>  signals. If no signals were constructed, the signal array is not allocated.
!>
!>  @param[inout] this A @ref v3fit_context_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE v3fit_context_resize(this)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (v3fit_context_class), INTENT(inout)    :: this

!  local variables
      INTEGER                                       :: i
      INTEGER                                       :: minsize
      TYPE (signal_pointer), DIMENSION(:), POINTER  :: temp_signal
      REAL (rprec)                                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (*,*) ' *** Resizing context arrays'
      WRITE (this%runlog_iou,*) ' *** Resizing context arrays'

!  Search an array for the first instance of a null pointer.
      IF (ASSOCIATED(this%signals)) THEN
         minsize = SIZE(this%signals)
         DO i = 1, SIZE(this%signals)
            IF (.not.ASSOCIATED(this%signals(i)%p)) THEN
               minsize = i - 1
               EXIT
            END IF
         END DO

!  Deallocate the array if no signals are used. Reallocate the array if it is
!  smaller.
         IF (minsize .eq. 0) THEN
            DEALLOCATE(this%signals)
            this%signals => null()

            CALL profiler_set_stop_time('v3fit_context_resize',                &
     &                                  start_time)

            RETURN
         ELSE IF (minsize .lt. SIZE(this%signals)) THEN
            ALLOCATE(temp_signal(minsize))
            DO i = 1, minsize
               temp_signal(i)%p => this%signals(i)%p
            END DO

            DEALLOCATE(this%signals)
            this%signals => temp_signal
         END IF

      END IF

      CALL profiler_set_stop_time('v3fit_context_resize', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Create output files.
!>
!>  Creates output files that reconstructed results are written to.
!>
!>  @param[inout] this A @ref v3fit_context_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE v3fit_context_create_files(this)
      USE safe_open_mod

      IMPLICIT NONE

!  Declare Arguments
      CLASS (v3fit_context_class), INTENT(inout) :: this

!  local variables
      CHARACTER (len=path_length)                :: filename
      INTEGER                                    :: status
      INTEGER                                    :: recon_rank
      REAL (rprec)                               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(this%reconstruction_comm, recon_rank, status)
#else
      recon_rank = 0
#endif

      filename = commandline_parser_get_string(this%cl_parser, '-file')

!  Setup the runlog file.
!  Initalize a default value of the I\O unit. V3FIT increments from there.
      this%runlog_iou = 0
      CALL safe_open(this%runlog_iou, status,                                  &
     &               TRIM('runlog.' // filename_base(filename)),               &
     &               'replace', 'formatted', delim_in='none')
      CALL assert_eq(0, status, 'v3fit_context_construct: ' //                 &
     &               'Safe_open of runlog. ' //                                &
     &               TRIM(filename_base(filename)) // 'failed')

!  Setup the recout file.
!  Initalize a default value of the I\O unit. V3FIT increments from there.
      this%recout_iou = 0
      CALL safe_open(this%recout_iou, status,                                  &
     &               TRIM('recout.' // filename_base(filename)),               &
     &               'replace', 'formatted', delim_in='none')
      CALL assert_eq(0, status, 'v3fit_context_construct: ' //                 &
     &               'Safe_open of recout. ' //                                &
     &               TRIM(filename_base(filename)) // 'failed')

      IF (commandline_parser_is_flag_set(this%cl_parser,                       &
     &                                   '-restart') .and.                     &
     &    recon_rank .eq. 0) THEN
         filename = commandline_parser_get_string(this%cl_parser,              &
     &                                            '-restart')
         status = nf90_open(filename, NF90_WRITE, this%result_ncid)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      ELSE
!  Create a result file.
         status = nf90_create('result.' // TRIM(filename_base(filename))       &
     &                        // '.nc', nf90_clobber,                          &
     &                        this%result_ncid)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      WRITE (this%runlog_iou, *) 'V3FITA RUN'
      WRITE (this%runlog_iou, *) '  Namelist Input from file ',                &
     &                           TRIM(filename)

      WRITE (this%recout_iou, *) 'V3FITA RUN'
      WRITE (this%recout_iou, *) '  Namelist Input from file ',                &
     &                           TRIM(filename)

      CALL profiler_set_stop_time('v3fit_context_create_files',                &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Close output files.
!>
!>  Closes output files that reconstructed results are written to.
!>
!>  @param[inout] this A @ref v3fit_context_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE v3fit_context_close_files(this)

      IMPLICIT NONE


!  Declare Arguments
      CLASS (v3fit_context_class), INTENT(inout) :: this

!  local variables
      INTEGER                                    :: status
      REAL (rprec)                               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      CLOSE(this%runlog_iou)
      CLOSE(this%recout_iou)

      status = nf90_close(this%result_ncid)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      CALL profiler_set_stop_time('v3fit_context_close_files',                 &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write the @ref v3fit_context_class out to disk.
!>
!>  Writes out the results of v3fit to the recout file.
!>
!>  @param[inout] this A @ref v3fit_context_class instance.
!>  @note The 'this' parameter is intent(inout) because
!>        @ref signal::signal_write calls @ref signal::signal_get_g2 which needs
!>        to be intent(inout).
!-------------------------------------------------------------------------------
      SUBROUTINE v3fit_context_write(this)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (v3fit_context_class), INTENT(inout) :: this

!  local variables
      INTEGER                                    :: i, j
      REAL (rprec), DIMENSION(:), ALLOCATABLE    :: sem_row
      INTEGER                                    :: sem_offset
!  Fortran 95 doesn't allow allocatable strings. Need to allocate a string large
!  enough to hold every possible parameter. Fortran 95 doesn't allow allocatable
!  scalar types so make this a pointer. This large string needs to be allocated
!  to avoid a stack overflow.
      CHARACTER (len=26 + 14*v3fit_max_parameters), POINTER ::                 &
     &   sem_header
      INTEGER, DIMENSION(:), ALLOCATABLE         :: indices
      CLASS (signal_class), POINTER              :: temp_signal
      REAL (rprec)                               :: start_time

!  local parameters
      CHARACTER (len=12), PARAMETER :: prefix = '            '

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (*,*) ' *** Writing context to disk'
      WRITE (this%runlog_iou,*) ' *** Writing context to disk'

!  Make the sem_header null so it may be allocated and deallocated.
      sem_header => null()

!  Write out the reconstruction
      IF (ASSOCIATED(this%recon)) THEN
         CALL this%recon%write(this%recout_iou)
      END IF

!  Write out the derived parameters
      IF (ASSOCIATED(this%derived_params)) THEN
         CALL param_write_header_short(this%recout_iou)
         DO j = 1, SIZE(this%derived_params)
            CALL param_write_short(this%derived_params(j)%p,                   &
     &                             this%recout_iou, j, this%model)
         END DO

!  Write out the derived parameter correlation matrix.
         IF (SIZE(this%derived_params) .gt. 0) THEN
            IF (ASSOCIATED(this%derived_params(1)%p%correlation)) THEN
               CALL v3fit_context_write_param_header(this,                     &
     &                  this%derived_params, prefix,                           &
     &                  ' *** Derived parameter covariance matrix')

               DO j = 1, SIZE(this%derived_params)
                  CALL param_write_correlation(this%derived_params(j)%p,       &
     &                                         this%recout_iou,                &
     &                                         this%model)
               END DO
            END IF
         END IF
      END IF

!  Write out the parameters
      IF (ASSOCIATED(this%params)) THEN
         CALL param_write_header(this%recout_iou)
         DO j = 1, SIZE(this%params)
            CALL param_write(this%params(j)%p, this%recout_iou, j,             &
     &                       this%model)
         END DO

!  Write out the reconstruction parameter correlation matrix.
         IF (SIZE(this%params) .gt. 0 .and.                                    &
     &       ASSOCIATED(this%params(1)%p%correlation)) THEN
            CALL v3fit_context_write_param_header(this, this%params,           &
     &                                            prefix,                      &
     &                                            ' *** Reconstruction '       &
     &                                            // 'parameter ' //           &
     &                                            'covariance ' //             &
     &                                            'matrix')

            DO j = 1, SIZE(this%params)
               CALL param_write_correlation(this%params(j)%p,                  &
     &                                      this%recout_iou, this%model)
            END DO
         END IF

!  Only need to write out the signal effectiveness matrix if signals exist.
         IF (ASSOCIATED(this%signals)) THEN

!  Write out the signal effectiveness matrix. This matrix will be written out
!  so that each parameter is one a column and each row is a a signal. Start by
!  generating the header.
            CALL v3fit_context_write_param_header(this, this%params,           &
     &                                            '   #  s_name     ' //       &
     &                                            '         ',                 &
     &                                            ' *** Signal ' //            &
     &                                            'Effectiveness ' //          &
     &                                            'Matrix')

!  Write out each row of the sem matrix.
!  Reuse the sem_header string to generate the format string. The format string
!  will be
!
!  i4,2x,a20,<num_params>(2x,es12.5)
            ALLOCATE(sem_row(SIZE(this%params)))
            ALLOCATE(sem_header)
            WRITE (sem_header, 1002) SIZE(this%params)

            DO i = 1, SIZE(this%signals)
               temp_signal => this%signals(i)%p

               DO j = 1, SIZE(this%params)
                  sem_row(j) = this%params(j)%p%recon%sem(i)
               END DO

               WRITE (this%recout_iou, sem_header(1:26))                       &
     &            i, temp_signal%s_name, sem_row
            END DO

!  Write out the total for each column. Sum over all the signals for a single
!  parameter. This should be 1.0.
            WRITE (sem_header, 1003) SIZE(this%params)
            DO j = 1, SIZE(this%params)
               sem_row(j) = SUM(this%params(j)%p%recon%sem)
            END DO

            WRITE (this%recout_iou, *)
            WRITE (this%recout_iou, sem_header) sem_row

!  Write out the most effective signal.
            ALLOCATE(indices(SIZE(this%params)))

            WRITE (sem_header, 1001) '    Most Effective Signal:'
            sem_offset = 26
            DO j = 1, SIZE(this%params)
               indices(j) = MAXLOC(this%params(j)%p%recon%sem, 1)
               sem_row(j) = this%params(j)%p%recon%sem(indices(j))

               WRITE (sem_header, 1000)                                        &
     &            sem_header(1:sem_offset),                                    &
     &            TRIM(this%signals(indices(j))%p%s_name)
               sem_offset = sem_offset + 14
            END DO
            WRITE (this%recout_iou, 1001)                                      &
     &         sem_header(1:26 + 14*SIZE(this%params))

            WRITE (sem_header, 1005) SIZE(this%params)
            WRITE (this%recout_iou, sem_header) indices

            WRITE (sem_header, 1006) SIZE(this%params)
            WRITE (this%recout_iou, sem_header) sem_row

            DEALLOCATE(indices)
            DEALLOCATE(sem_header)
            DEALLOCATE(sem_row)
         END IF

1000  FORMAT (a,2x,a12)
1001  FORMAT (a)
1002  FORMAT ('(i4,2x,a20,',i3,'(2x,es12.5))')
1003  FORMAT ('(20x,''Total:''',i3,'(2x,es12.5))')
1005  FORMAT ('(13x,''Signal Index:''',i3,'(2x,i12))')
1006  FORMAT ('(16x,''Max Value:''',i3,'(2x,es12.5))')
      END IF

!  Write out the model.
      IF (ASSOCIATED(this%model)) THEN
         CALL this%model%write(this%recout_iou)
      END IF

!  Write out the signals
      IF (ASSOCIATED(this%signals)) THEN
         WRITE (this%recout_iou, *)
         WRITE (this%recout_iou, *) ' *** Signals'

         DO i = 1, SIZE(this%signals)
            temp_signal => this%signals(i)%p

!  Check if the index has reached a new signal type
            IF ((1                         .eq. i) .or.                        &
     &          (this%magnetic_index       .eq. i) .or.                        &
     &          (this%sxrem_index          .eq. i) .or.                        &
     &          (this%intpol_index         .eq. i) .or.                        &
     &          (this%thomson_index        .eq. i) .or.                        &
     &          (this%extcurz_index        .eq. i) .or.                        &
     &          (this%mse_index            .eq. i) .or.                        &
     &          (this%ece_index            .eq. i) .or.                        &
     &          (this%limiter_index        .eq. i) .or.                        &
     &          (this%prior_gaussian_index .eq. i) .or.                        &
     &          (this%sxrem_ratio_index    .eq. i) .or.                        &
     &          (this%combination_index    .eq. i)) THEN

               CALL temp_signal%write_header(this%recout_iou)
            END IF

            CALL temp_signal%write(this%recout_iou, i, this%model)
         END DO

!  All the signals should be written out before auxiliary information.
         DO i = 1, SIZE(this%signals)
            temp_signal => this%signals(i)%p
            CALL temp_signal%write_auxiliary(this%recout_iou, i,               &
     &                                       this%model)
         END DO
      END IF

      CALL profiler_set_stop_time('v3fit_context_write', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write the @ref param_class::correlation header out to disk.
!>
!>  Writes out the results of header for a correlation matric. This header is
!>  formatted by a an inital col left blank followed by eack parameter name
!>  spaced 14 spaced apart from each other.
!>
!>  @param[inout] this      A @ref v3fit_context_class instance.
!>  @param[in]    params    Array of parameter objects.
!>  @param[in]    prefix    Prefix string to format the header.
!>  @param[in]    type_name Type name of the parameters.
!>  @note The 'this' parameter is intent(inout) because
!>        @ref signal::signal_write calls @ref signal::signal_get_g2 which needs
!>        to be intent(inout).
!>  @node The 'prefix' parameter must be an exact length string with all white
!>        space predefined.
!-------------------------------------------------------------------------------
      SUBROUTINE v3fit_context_write_param_header(this, params, prefix,        &
     &                                            type_name)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (v3fit_context_class), INTENT(inout) :: this
      TYPE (param_pointer), DIMENSION(:)         :: params
      CHARACTER (len=*)                          :: prefix
      CHARACTER (len=*)                          :: type_name

!  local variables
!  Fortran 95 doesn't allow allocatable strings. Need to allocate a string large
!  enough to hold every possible parameter. Fortran 95 doesn't allow allocatable
!  scalar types so make this a pointer. This large string needs to be allocated
!  to avoid a stack overflow. The longest header generate will be the signal
!  effectiveness matrix. Make the start of this string at least 26 characters.
      CHARACTER (len=26 + 14*v3fit_max_parameters), POINTER :: header
      INTEGER                                    :: offset, j
      REAL (rprec)                               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (this%recout_iou, *)
      WRITE (this%recout_iou, *) type_name

! The header is formatted as
!
!  <prefix> (param_name[12]) ...
!
!  Start by allocating a string large enough to hold the entire header. The
!  length of this string needs to be the size of the prefix plus an additional
!  14 for each parameter. The prefix string must be an exact length.
      header => null()
      ALLOCATE(header)

      WRITE (header, 1000) prefix
      offset = LEN(prefix)
      DO j = 1, SIZE(params)
         WRITE (header, 1001) header(1:offset),                                &
     &                        param_get_name(params(j)%p, this%model)
         offset = offset + 14
      END DO

      WRITE (this%recout_iou, 1000) header(1:offset)

      DEALLOCATE(header)

      CALL profiler_set_stop_time('v3fit_context_write_param_header',          &
     &                            start_time)

1000  FORMAT (a)
1001  FORMAT (a,2x,a12)

      END SUBROUTINE

!*******************************************************************************
!  NETCDF SUBROUTINES
!*******************************************************************************
!>  @page result_file_main Result File
!>
!>  @tableofcontents
!>  @section result_file_main_intro_sec Introduction
!>  This page documents the contents of a result NetCDF file.
!>
!>  @section result_file_main_dim_sec Dimensions
!>  @header{Dimension, Description, Code Reference}
!>  @begin_table
!>     @item{maxnsteps,   The max number of reconstruction steps., v3fit_input::nrstep}
!>     @item{ndparam,     Number of derived parameters.,           v3fit_input::n_dp}
!>     @item{nparam,      Number of reconstruction parameters.,    v3fit_input::n_rp}
!>     @item{nparamindex, Number of parameter indices.,            data_parameters::data_max_indices}
!>     @item{nsignal,     Number of constructed signals.,          v3fit_context::v3fit_context_class::signals}
!>     @item{string_len,  Max lenth of strings.,                   data_parameters::data_name_length}
!>  @end_table
!>  @subsection result_file_main_eq_dim_sec Equilibrium Dimensions
!>  Equilibrium dimensions change depending on the equilibrium used.
!>  * Model @ref result_file_model_dim_sec
!>  * VMEC @ref result_file_vmec_dim_sec
!>
!>  @section result_file_main_var_sec Variables
!>  @header{Variable(Dimensions), Description, Code Reference}
!>  @begin_table
!>     @item{nsteps,         Actual number of steps taken.,           reconstruction::reconstruction_class::current_step}
!>     @item{g2 (maxnsteps), g^2 value for each reconstruction step., reconstruction::reconstruction_get_g2}
!>  @end_table
!>  @table_section{result_file_main_dir_param_var_sec, Derived Parameter Variables}
!>     @item{derived_param_name (string_len\, ndparam),           Name of the derived parameter.,               v3fit_params::param_get_name}
!>     @item{derived_param_index (nparamindex\, ndparam),         Indices of the derived parameter.,            v3fit_params::param_class::indices}
!>     @item{derived_param_value (ndparam\, maxnsteps),           Parameter values for each derived step.,      v3fit_params::param_get_value}
!>     @item{derived_param_sigma (ndparam\, maxnsteps),           Parameter sigmas for each derived step.,      v3fit_params::param_class::sigma}
!>     @item{derived_param_corr  (ndparam\, ndparam\, maxnsteps), Correlation matrix of the derived parameters, v3fit_params::param_class::correlation}
!>  @end_table
!>  @table_section{result_file_main_param_var_sec, Reconstruction Parameter Variables}
!>     @item{param_name (string_len\, nparam),                 Name of the reconstruction parameter.,               v3fit_params::param_get_name}
!>     @item{param_index (nparamindex\, nparam),               Indices of the reconstruction parameter.,            v3fit_params::param_class::indices}
!>     @item{param_value (nparam\, maxnsteps),                 Parameter values for each reconstruction step.,      v3fit_params::param_get_value}
!>     @item{param_sigma (nparam\, maxnsteps),                 Parameter sigmas for each reconstruction step.,      v3fit_params::param_class::sigma}
!>     @item{param_corr  (nparam\, nparam\, maxnsteps),        Correlation matrix of the reconstruction parameters, v3fit_params::param_class::correlation}
!>     @item{signal_eff_matrix (nsignal\, nparam\, maxnsteps), Signal effectiveness for each reconstruction step.,  v3fit_params::param_recon_class::sem}
!>  @end_table
!>  @table_section{result_file_main_sig_var_sec, Signal Variables}
!>     @item{signal_name (string_len\, nsignal),       Short name of the signal.,                                signal::signal_class::s_name}
!>     @item{signal_type (string_len\, nsignal),       Description of the signal.,                               signal::signal_get_type}
!>     @item{signal_weight (nsignal),                  Weight value of the signal.,                              signal::signal_class::weight}
!>     @item{signal_observed_value (nsignal),          Observed value of the signal.,                            signal::signal_class::observed}
!>     @item{signal_model_value (nsignal\, maxnsteps), Modeled value of the signal at each reconstruction step., signal::signal_get_modeled_signal}
!>     @item{signal_sigma_value (nsignal\, maxnsteps), Sigma value of the signal at each reconstruction step.,   signal::signal_get_g2}
!>  @end_table
!>  @subsection result_file_main_eq_var_sec Equilibrium Variables
!>  Equilibrium variables change depending on the equilibrium used.
!>  * Model @ref result_file_model_var_sec
!>  * VMEC @ref result_file_vmec_var_sec
!-------------------------------------------------------------------------------
!>  @brief Initialize the dimensions and variables of the result file.
!>
!>  Defines dimensions, variables and writes out the initial step of v3fit to
!>  result netcdf file. Multi dimensional arrays need to be transposed so arrays
!>  appear in the correct order in non fortran languages.
!>
!>  @param[inout] this     A @ref v3fit_context_class instance.
!>  @param[in]    eq_steps Number of steps taken by the equilibrium.
!-------------------------------------------------------------------------------
      SUBROUTINE v3fit_context_init_data(this, eq_steps)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (v3fit_context_class), INTENT(inout) :: this
      INTEGER, INTENT(in)                        :: eq_steps

!  local variables
      INTEGER :: i, status
      INTEGER :: maxnsteps_dim_id
      INTEGER :: ndparam_dim_id
      INTEGER :: nparam_dim_id
      INTEGER :: nparamindex_dim_id
      INTEGER :: nsignal_dim_id
      INTEGER :: n_sig_models_dim_id
      INTEGER :: string_len_dim_id
      INTEGER :: nsteps_id
      INTEGER :: eq_steps_id
      INTEGER :: g2_id
      INTEGER :: derived_param_name_id
      INTEGER :: derived_param_index_id
      INTEGER :: derived_param_value_id
      INTEGER :: derived_param_sigma_id
      INTEGER :: derived_param_corr_id
      INTEGER :: param_name_id
      INTEGER :: param_index_id
      INTEGER :: param_value_id
      INTEGER :: param_sigma_id
      INTEGER :: param_corr_id
      INTEGER :: param_sem_id
      INTEGER :: signal_name_id
      INTEGER :: signal_type_id
      INTEGER :: signal_weight_id
      INTEGER :: signal_observed_value_id
      INTEGER :: signal_model_value_id
      INTEGER :: signal_sigma_value_id

      CLASS (signal_class), POINTER              :: temp_signal
      REAL (rprec)                               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Define dimensions
      status = nf90_noerr

      IF (ASSOCIATED(this%recon)) THEN
         status = nf90_def_dim(this%result_ncid, 'maxnsteps',                  &
     &                         nf90_unlimited, maxnsteps_dim_id)
      ELSE
         status = nf90_def_dim(this%result_ncid, 'maxnsteps', 1,               &
     &                         maxnsteps_dim_id)
      END IF
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      IF (ASSOCIATED(this%params)  .or.                                        &
     &    ASSOCIATED(this%signals) .or.                                        &
     &    ASSOCIATED(this%model)) THEN
         status = nf90_def_dim(this%result_ncid, 'string_len',                 &
     &                         data_name_length, string_len_dim_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      IF (ASSOCIATED(this%derived_params) .and.                                &
     &    SIZE(this%derived_params) .gt. 0) THEN
         status = nf90_def_dim(this%result_ncid, 'ndparam',                    &
     &                         SIZE(this%derived_params),                      &
     &                         ndparam_dim_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      IF (ASSOCIATED(this%params)) THEN
         status = nf90_def_dim(this%result_ncid, 'nparam',                     &
     &                         SIZE(this%params), nparam_dim_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_dim(this%result_ncid, 'nparamindex',                &
     &                         2, nparamindex_dim_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      IF (ASSOCIATED(this%signals)) THEN
         status = nf90_def_dim(this%result_ncid, 'nsignal',                    &
     &                         SIZE(this%signals), nsignal_dim_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      IF (ASSOCIATED(this%signals)) THEN
         status = nf90_def_dim(this%result_ncid, 'n_sig_models', 4,            &
     &                         n_sig_models_dim_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

!  Define variables
      status = nf90_def_var(this%result_ncid, 'nsteps', nf90_int,              &
     &                      varid=nsteps_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      status = nf90_def_var(this%result_ncid, 'eq_steps', nf90_int,            &
     &                      varid=eq_steps_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      IF (ASSOCIATED(this%recon)) THEN
         status = nf90_def_var(this%result_ncid, 'g2', nf90_double,            &
     &                         dimids=(/ maxnsteps_dim_id /),                  &
     &                         varid=g2_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

!  Define derived parameter variables.
      IF (ASSOCIATED(this%derived_params) .and.                                &
     &    SIZE(this%derived_params) .gt. 0) THEN
         status = nf90_def_var(this%result_ncid, 'derived_param_name',         &
     &                         nf90_char,                                      &
     &                         dimids=(/ string_len_dim_id,                    &
     &                                   ndparam_dim_id /),                    &
     &                         varid=derived_param_name_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_var(this%result_ncid, 'derived_param_index',        &
     &                         nf90_int,                                       &
     &                         dimids=(/ nparamindex_dim_id,                   &
     &                                   ndparam_dim_id /),                    &
     &                         varid=derived_param_index_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_var(this%result_ncid, 'derived_param_value',        &
     &                         nf90_double,                                    &
     &                         dimids=(/ ndparam_dim_id,                       &
     &                                   maxnsteps_dim_id /),                  &
     &                         varid=derived_param_value_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_var(this%result_ncid, 'derived_param_sigma',        &
     &                         nf90_double,                                    &
     &                         dimids=(/ ndparam_dim_id,                       &
     &                                   maxnsteps_dim_id /),                  &
     &                         varid=derived_param_sigma_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_var(this%result_ncid, 'derived_param_corr',         &
     &                         nf90_double,                                    &
     &                         dimids=(/ ndparam_dim_id,                       &
     &                                   ndparam_dim_id,                       &
     &                                   maxnsteps_dim_id /),                  &
     &                         varid=derived_param_corr_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

!  Define reconstruction parameter variables.
      IF (ASSOCIATED(this%params)) THEN
         status = nf90_def_var(this%result_ncid, 'param_name',                 &
     &                         nf90_char, dimids=(/ string_len_dim_id,         &
     &                                              nparam_dim_id /),          &
     &                         varid=param_name_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_var(this%result_ncid, 'param_index',                &
     &                         nf90_int, dimids=(/ nparamindex_dim_id,         &
     &                                             nparam_dim_id /),           &
     &                         varid=param_index_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_var(this%result_ncid, 'param_value',                &
     &                         nf90_double,                                    &
     &                         dimids=(/ nparam_dim_id,                        &
     &                                   maxnsteps_dim_id /),                  &
     &                         varid=param_value_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_var(this%result_ncid, 'param_sigma',                &
     &                         nf90_double,                                    &
     &                         dimids=(/ nparam_dim_id,                        &
     &                                   maxnsteps_dim_id /),                  &
     &                         varid=param_sigma_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_var(this%result_ncid, 'param_corr',                 &
     &                         nf90_double,                                    &
     &                         dimids=(/ nparam_dim_id,                        &
     &                                   nparam_dim_id,                        &
     &                                   maxnsteps_dim_id /),                  &
     &                         varid=param_corr_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_var(this%result_ncid, 'signal_eff_matrix',          &
     &                         nf90_double,                                    &
     &                         dimids=(/ nsignal_dim_id,                       &
     &                                   nparam_dim_id,                        &
     &                                   maxnsteps_dim_id /),                  &
     &                         varid=param_sem_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

!  Define signal variables.
      IF (ASSOCIATED(this%signals)) THEN
         status = nf90_def_var(this%result_ncid, 'signal_name',                &
     &                         nf90_char, dimids=(/ string_len_dim_id,         &
     &                                              nsignal_dim_id /),         &
     &                         varid=signal_name_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_var(this%result_ncid, 'signal_type',                &
     &                         nf90_char, dimids=(/ string_len_dim_id,         &
     &                                              nsignal_dim_id /),         &
     &                         varid=signal_type_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_var(this%result_ncid, 'signal_weight',              &
     &                         nf90_double, dimids=(/ nsignal_dim_id /),       &
     &                         varid=signal_weight_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_var(this%result_ncid,                               &
     &                         'signal_observed_value',                        &
     &                         nf90_double, dimids=(/ nsignal_dim_id /),       &
     &                         varid=signal_observed_value_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_var(this%result_ncid, 'signal_model_value',         &
     &                         nf90_double,                                    &
     &                         dimids=(/ n_sig_models_dim_id,                  &
     &                                   nsignal_dim_id,                       &
     &                                   maxnsteps_dim_id /),                  &
     &                         varid=signal_model_value_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_def_var(this%result_ncid, 'signal_sigma',               &
     &                         nf90_double,                                    &
     &                         dimids=(/ nsignal_dim_id,                       &
     &                                   maxnsteps_dim_id /),                  &
     &                         varid=signal_sigma_value_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      IF (ASSOCIATED(this%model)) THEN
         CALL model_def_result(this%model, this%result_ncid,                   &
     &                         maxnsteps_dim_id, string_len_dim_id)
      END IF

! Finished defining netcdf file. Exit out of define mode.
      status = nf90_enddef(this%result_ncid)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      IF (ASSOCIATED(this%derived_params)) THEN
         DO i = 1, SIZE(this%derived_params)
            status = nf90_put_var(this%result_ncid,                            &
     &                  derived_param_name_id,                                 &
     &                  param_get_name(this%derived_params(i)%p,               &
     &                                 this%model),                            &
     &                  start=(/ 1, i /),                                      &
     &                  count=(/ data_name_length, 1 /))
            CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

            status = nf90_put_var(this%result_ncid,                            &
     &                            derived_param_index_id,                      &
     &                            this%derived_params(i)%p%indices,            &
     &                            start=(/ 1, i /),                            &
     &                            count=(/ 2, 1 /))
            CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         END DO
      END IF

      IF (ASSOCIATED(this%params)) THEN
         DO i = 1, SIZE(this%params)
            status = nf90_put_var(this%result_ncid, param_name_id,             &
     &                            param_get_name(this%params(i)%p,             &
     &                                               this%model),              &
     &                            start=(/ 1, i /),                            &
     &                            count=(/ data_name_length, 1 /))
            CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

            status = nf90_put_var(this%result_ncid, param_index_id,            &
     &                            this%params(i)%p%indices,                    &
     &                            start=(/ 1, i /),                            &
     &                            count=(/ 2, 1 /))
            CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         END DO
      END IF

      IF (ASSOCIATED(this%signals)) THEN
         DO i = 1, SIZE(this%signals)
            temp_signal => this%signals(i)%p

            status = nf90_put_var(this%result_ncid, signal_name_id,            &
     &                            this%signals(i)%p%s_name,                    &
     &                            start=(/ 1, i /),                              &
     &                            count=(/ data_name_length, 1 /))
            CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

            status = nf90_put_var(this%result_ncid, signal_type_id,            &
     &                            temp_signal%get_type(),                      &
     &                            start=(/ 1, i /),                            &
     &                            count=(/ data_name_length, 1 /))
            CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

            status = nf90_put_var(this%result_ncid,                            &
     &                            signal_weight_id,                            &
     &                            this%signals(i)%p%weight, start=(/i/))
            CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

            status = nf90_put_var(this%result_ncid,                            &
     &                            signal_observed_value_id,                    &
     &                            this%signals(i)%p%observed,                  &
     &                            start=(/i/))
            CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         END DO
      END IF

      IF (ASSOCIATED(this%model)) THEN
         CALL this%model%write_init_data(this%result_ncid)
      END IF

      CALL v3fit_context_write_step_data(this, .true., eq_steps)

      CALL profiler_set_stop_time('v3fit_context_init_data', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Write step data to the defined variables.
!>
!>  Writes out the data for a reconstruction step to the result file. If this
!>  is being called for the first time. Do not read the number of steps.
!>
!>  @param[inout] this       A @ref v3fit_context_class instance.
!>  @param[in]    first_step Flags to determine if this is the first instance
!>                           this has been called.
!>  @param[in]    eq_steps   Number of steps the equilibrium took.
!>  @note The only caller of this subrotine that should set first_step to true
!>  should be @ref v3fit_context_init_data. All others should callers should set
!>  first_step to false.
!-------------------------------------------------------------------------------
      SUBROUTINE v3fit_context_write_step_data(this, first_step,               &
     &                                         eq_steps)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (v3fit_context_class), INTENT(inout) :: this
      LOGICAL, INTENT(in)                        :: first_step
      INTEGER, INTENT(in)                        :: eq_steps

!  local variables
      INTEGER :: i, status
      INTEGER :: current_step
      INTEGER :: nsteps_id
      INTEGER :: eq_steps_id
      INTEGER :: g2_id
      INTEGER :: derived_param_value_id
      INTEGER :: derived_param_sigma_id
      INTEGER :: derived_param_corr_id
      INTEGER :: param_value_id
      INTEGER :: param_sigma_id
      INTEGER :: param_corr_id
      INTEGER :: param_sem_id
      INTEGER :: signal_model_value_id
      INTEGER :: signal_sigma_value_id

      CLASS (signal_class), POINTER              :: temp_signal
      REAL (rprec)                               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Update number of steps taken.
      status = nf90_inq_varid(this%result_ncid, 'nsteps', nsteps_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      IF (first_step) THEN
         status = nf90_put_var(this%result_ncid, nsteps_id, 0)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         current_step = 1
      ELSE
         status = nf90_get_var(this%result_ncid, nsteps_id,                    &
     &                         current_step)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         current_step = current_step + 1
         status = nf90_put_var(this%result_ncid, nsteps_id,                    &
     &                         current_step)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

!  Use current_step as an array index. The Netcdf arrays start at 1 so this
!  needs to be incremented to point to the correct index.
         current_step = current_step + 1
      END IF

      status = nf90_inq_varid(this%result_ncid, 'eq_steps', eq_steps_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_put_var(this%result_ncid, eq_steps_id, eq_steps)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      CALL this%model%write_step_data(this%result_ncid, current_step)

      IF (ASSOCIATED(this%recon)) THEN
         status = nf90_inq_varid(this%result_ncid, 'g2', g2_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
         status = nf90_put_var(this%result_ncid, g2_id,                        &
     &                         this%recon%get_g2(),                            &
     &                         start=(/current_step/))
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      END IF

      IF (ASSOCIATED(this%derived_params) .and.                                &
     &    SIZE(this%derived_params) .gt. 0) THEN
         status = nf90_inq_varid(this%result_ncid,                             &
     &                           'derived_param_value',                        &
     &                           derived_param_value_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_inq_varid(this%result_ncid,                             &
     &                           'derived_param_sigma',                        &
     &                           derived_param_sigma_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_inq_varid(this%result_ncid,                             &
     &                           'derived_param_corr',                         &
     &                           derived_param_corr_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         DO i = 1, SIZE(this%derived_params)
            CALL param_write_step_data(this%derived_params(i)%p,               &
     &                                 this%model, this%result_ncid,           &
     &                                 current_step, i,                        &
     &                                 derived_param_value_id,                 &
     &                                 derived_param_sigma_id,                 &
     &                                 derived_param_corr_id)
         END DO
      END IF

      IF (ASSOCIATED(this%params)) THEN
         status = nf90_inq_varid(this%result_ncid, 'param_value',              &
     &                           param_value_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_inq_varid(this%result_ncid, 'param_sigma',              &
     &                           param_sigma_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_inq_varid(this%result_ncid, 'param_corr',               &
     &                           param_corr_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_inq_varid(this%result_ncid, 'signal_eff_matrix',        &
     &                           param_sem_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         DO i = 1, SIZE(this%params)
            CALL param_write_step_data(this%params(i)%p, this%model,           &
     &                                 this%result_ncid, current_step,         &
     &                                 i, param_value_id,                      &
     &                                 param_sigma_id,                         &
     &                                 param_corr_id,                          &
     &                                 param_sem_id)
         END DO
      END IF

      IF (ASSOCIATED(this%signals)) THEN
         status = nf90_inq_varid(this%result_ncid, 'signal_model_value',       &
     &                           signal_model_value_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_inq_varid(this%result_ncid, 'signal_sigma',             &
     &                           signal_sigma_value_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         DO i = 1, SIZE(this%signals)
            temp_signal => this%signals(i)%p

            CALL temp_signal%write_step_data(this%model,                       &
     &                                       this%result_ncid,                 &
     &                                       current_step, i,                  &
     &                                       signal_model_value_id,            &
     &                                       signal_sigma_value_id)
         END DO
      END IF

!  Flush the step data to disk. This ensures that data is recorded in the event
!  of a fatal error that stops v3fit execution.
      status = nf90_sync(this%result_ncid)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      CALL profiler_set_stop_time('v3fit_context_write_step_data',             &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Restart the reconstruction.
!>
!>  Reloads a reconstruction from the result file.
!>
!>  @param[inout] this         A @ref v3fit_context_class instance.
!>  @param[inout] current_step Reconstruction step left off from.
!>  @returns The current step.
!-------------------------------------------------------------------------------
      FUNCTION v3fit_context_restart(this, current_step)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: v3fit_context_restart
      CLASS (v3fit_context_class), INTENT(inout) :: this
      INTEGER, INTENT(inout)                     :: current_step

!  local variables
      INTEGER                                    :: i
      INTEGER                                    :: status
      INTEGER                                    :: nsteps_id
      INTEGER                                    :: eq_steps_id
      INTEGER                                    :: param_value_id
      INTEGER                                    :: param_sigma_id
      INTEGER                                    :: param_corr_id
      REAL (rprec)                               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  The current step starts at zero but the NetCDF file starts at 1. Increment
!  the current step when passing it into subroutines.
      status = nf90_inq_varid(this%result_ncid, 'nsteps', nsteps_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_get_var(this%result_ncid, nsteps_id, current_step)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      current_step = current_step + 1

      IF (ASSOCIATED(this%params)) THEN
         status = nf90_inq_varid(this%result_ncid, 'param_value',              &
     &                           param_value_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_inq_varid(this%result_ncid, 'param_sigma',              &
     &                           param_sigma_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         status = nf90_inq_varid(this%result_ncid, 'param_corr',               &
     &                           param_corr_id)
         CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

         DO i = 1, SIZE(this%params)
            CALL param_restart(this%params(i)%p, this%model,                   &
     &                         this%result_ncid,                               &
     &                         current_step,                                   &
     &                         i, param_value_id, param_sigma_id,              &
     &                         param_corr_id, this%equilibrium_comm,           &
     &                         this%recon%use_central)
         END DO

         CALL this%model%restart(this%result_ncid, current_step)

         CALL this%recon%restart(this%result_ncid, current_step,               &
     &                           this%signals, this%derived_params,            &
     &                           this%model)

      END IF

      status = nf90_inq_varid(this%result_ncid, 'eq_steps', eq_steps_id)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))
      status = nf90_get_var(this%result_ncid, eq_steps_id,                     &
     &                      v3fit_context_restart)
      CALL assert_eq(status, nf90_noerr, nf90_strerror(status))

      CALL profiler_set_stop_time('v3fit_context_restart', start_time)

      END FUNCTION

!*******************************************************************************
!  NETCDF SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Get the mpi comm for the equilibrium.
!>
!>  @param[inout] this A @ref v3fit_context_class instance.
!>  @returns The equilibrium comm.
!-------------------------------------------------------------------------------
      FUNCTION v3fit_context_get_eq_comm(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: v3fit_context_get_eq_comm
      CLASS (v3fit_context_class), INTENT(inout) :: this

!  local variables
      REAL (rprec)                               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

#if defined(MPI_OPT)
      v3fit_context_get_eq_comm = this%equilibrium_comm
#else
      v3fit_context_get_eq_comm = 0
#endif

      CALL profiler_set_stop_time('v3fit_context_get_eq_comm',                 &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the mpi rank for the equilibrium.
!>
!>  @param[inout] this A @ref v3fit_context_class instance.
!>  @returns The rank of the equilibrium comm.
!-------------------------------------------------------------------------------
      FUNCTION v3fit_context_get_eq_rank(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: v3fit_context_get_eq_rank
      CLASS (v3fit_context_class), INTENT(inout) :: this

!  local variables
      INTEGER                                    :: error
      REAL (rprec)                               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      v3fit_context_get_eq_rank = 0
#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(this%equilibrium_comm,                                &
     &                   v3fit_context_get_eq_rank, error)
#endif

      CALL profiler_set_stop_time('v3fit_context_get_eq_rank',                 &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the mpi comm for the reconstruction.
!>
!>  @param[inout] this A @ref v3fit_context_class instance.
!>  @returns The equilibrium comm.
!-------------------------------------------------------------------------------
      FUNCTION v3fit_context_get_recon_comm(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: v3fit_context_get_recon_comm
      CLASS (v3fit_context_class), INTENT(inout) :: this

!  local variables
      REAL (rprec)                               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

#if defined(MPI_OPT)
      v3fit_context_get_recon_comm = this%reconstruction_comm
#else
      v3fit_context_get_recon_comm = 0
#endif

      CALL profiler_set_stop_time('v3fit_context_get_recon_comm',              &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Get the mpi rank for the reconstruction.
!>
!>  @param[inout] this A @ref v3fit_context_class instance.
!>  @returns The rank of the equilibrium comm.
!-------------------------------------------------------------------------------
      FUNCTION v3fit_context_get_recon_rank(this)

      IMPLICIT NONE

!  Declare Arguments
      INTEGER :: v3fit_context_get_recon_rank
      CLASS (v3fit_context_class), INTENT(inout) :: this

!  local variables
      INTEGER                                    :: error
      REAL (rprec)                               :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      v3fit_context_get_recon_rank = 0
#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(this%reconstruction_comm,                             &
     &                   v3fit_context_get_recon_rank, error)
#endif

      CALL profiler_set_stop_time('v3fit_context_get_recon_rank',              &
     &                            start_time)

      END FUNCTION

      END MODULE

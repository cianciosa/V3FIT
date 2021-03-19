!*******************************************************************************
!>  @file v3fita.f
!>  @brief Contains the main routines for V3FIT.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  V3FIT is a code for reconstructing 3D equilibria. V3FIT determined the
!>  optimal parameters of a 3D equilibrium by minimizing the missmatch between
!>  modeled and observed signals. V3FIT has been created with contributions
!>  from.
!>
!>  @author James D. Hanson
!>  @author Mark Cianciosa
!>  @author Steve Hirshman
!>  @author Ed Lazarus
!>  @author Lang Lao
!>  @author Steve Knowlton
!>  @author Gregorio L. Trevisan
!>  @author Eric C. Howell
!>
!>  The theory behind V3FIT is outlined in
!>     Hanson et. al. doi:10.1088/0029-5515/49/7/075031
!>
!>  Below is a brief discription of the major top level objects of the code. For
!>  discriptions of lower level objects consult the referenced top level
!>  objects.
!>
!>  @ref model defines the equilibrium model.
!>  @ref signal defined the interface to the signals.
!>  @ref reconstruction contain the minimization algorithm.
!>  @ref data_parameters defined the various fixed parameters utility functions
!>                       used throughout.
!>  @ref v3fit_input Defines the V3FIT namelist input file.
!>  @ref v3fit_context defined the main pointer to all allocated memory and
!>                     objects.
!>  @ref commandline_parser defines and parses commandline arguments.
!*******************************************************************************
!*******************************************************************************
!  MAIN PROGRAM
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief V3FIT main program.
!>
!>  Highest level V3FIT routine.
!-------------------------------------------------------------------------------
      PROGRAM v3fit
      USE v3fit_context
      USE profiler

      IMPLICIT NONE

!  local variables
      TYPE (v3fit_context_class), POINTER :: context => null()
      REAL (rprec)                        :: time_start, time_end
      INTEGER                             :: eq_rank
      INTEGER                             :: recon_rank
#if defined(MPI_OPT)
      INTEGER                             :: error

!  Start of executable code
!  The Intel version of the MPI libraries does not provide a correct value for
!  time until after MPI_INIT is called. Make sure this is the first think called
!  so that correct timing information can be used.
      CALL MPI_INIT(error)
#endif

      CALL second0(time_start)
      CALL profiler_construct

!  Create a context and read in the v3fit namelist input file.
      context => v3fit_context_construct(commandline_parser_construct())

!  Run unit tests if the -test flag is set.
      IF (commandline_parser_is_flag_set(context%cl_parser,                    &
     &                                   '-test')) THEN
         my_task = 'unit_test'
      ELSE
         CALL v3fit_input_read_namelist(                                       &
     &           commandline_parser_get_string(context%cl_parser,              &
     &                                         '-file'))
      END IF

!  Configure parallel computation.
      CALL config_parallelism(context)

      eq_rank = 0
#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(context%equilibrium_comm, eq_rank, error)
#endif

      IF (eq_rank .eq. 0) THEN
         CALL v3fit_context_create_files(context)

         WRITE (context%recout_iou,*) '  my_task is ', my_task
         WRITE (context%runlog_iou,*) '  my_task is ', my_task
      END IF

      SELECT CASE (my_task)

         CASE ('equilibrium')
            CALL task_equilibrium(context)

         CASE ('v3post', 'vmec_v3post')
            CALL task_v3post(context)

         CASE ('reconstruct', 'reconstruct_a1')
            CALL task_reconstruct(context)

         CASE ('child_recon')
            CALL task_child_recon(context)

         CASE ('child_equilibrium')
            CALL task_child_equilibrium(context)

         CASE ('unit_test')
            CALL task_unit_tests

         CASE DEFAULT
            CALL err_fatal('Unkown task type ' // TRIM(my_task))

      END SELECT

!  Clean up memory
      IF (eq_rank .eq. 0) THEN
         recon_rank = 0
#if defined(MPI_OPT)
         CALL MPI_COMM_RANK(context%reconstruction_comm, recon_rank,           &
     &                      error)
#endif

         IF (recon_rank .eq. 0) THEN
            CALL second0(time_end)

            CALL v3fit_context_write(context)

            WRITE (context%runlog_iou,1000) time_end - time_start
            WRITE (context%recout_iou,1000) time_end - time_start
            WRITE (*,1000) time_end - time_start

            CALL profiler_write(context%recout_iou)

         END IF

         CALL v3fit_context_close_files(context)
      END IF
      CALL profiler_destruct

      CALL cleanup_parallelism(context)
      CALL v3fit_context_destruct(context)

#if defined(MPI_OPT)
      CALL MPI_FINALIZE(error)
#endif

1000  FORMAT('  *** V3FIT run time (s): ',f10.2)

      END PROGRAM

!*******************************************************************************
!  TASK SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Solves the equilibrium.
!>
!>  Task to just solve the equilibrium. This task is choosen by setting
!>  @ref v3fit_input::my_task to 'equilibrium'.
!>
!>  @param[inout] context An instance of a @ref v3fit_context object.
!-------------------------------------------------------------------------------
      SUBROUTINE task_equilibrium(context)
      USE v3fit_context

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context

!  local variables
      INTEGER                                   :: eq_steps
      LOGICAL                                   :: converged
      INTEGER                                   :: error
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      eq_steps = 1

      WRITE (*,*) ' *** In task equilibrium'
      WRITE (context%runlog_iou,*) ' *** In task equilibrium'

!  Initialize equilibrium
      CALL init_equilibrium(context)

!  Solve equilibrium
      WRITE (*,*) ' *** Starting initial equilibrium convergence (eq)'
      WRITE (context%runlog_iou,*) ' *** Starting initial ' //                 &
     &                             'equilibrium convergence'

#if defined (MPI_OPT)
      converged = model_converge(context%model, eq_steps,                      &
     &                           context%runlog_iou,                           &
     &                           context%equilibrium_comm, 'All')
      CALL MPI_BCAST(mpi_quit_task, 1, MPI_INTEGER, 0,                         &
     &               context%equilibrium_comm, error)
#else
      converged = model_converge(context%model, eq_steps,                      &
     &                           context%runlog_iou, 0, 'All')
#endif

      CALL v3fit_context_init_data(context, eq_steps)

      CALL profiler_set_stop_time('task_equilibrium', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Solves the equilibrium and calculates the modeled signals.
!>
!>  Task to just compute modeled signals from an equilibrium. This task is
!>  choosen by setting @ref v3fit_input::my_task to 'v3post'.
!>
!>  @param[inout] context An instance of a @ref v3fit_context object.
!-------------------------------------------------------------------------------
      SUBROUTINE task_v3post(context)
      USE v3fit_context

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context

!  local variables
      INTEGER                                   :: eq_steps
      INTEGER                                   :: error
      LOGICAL                                   :: converged
      INTEGER                                   :: i
      INTEGER                                   :: last_para_signal
      REAL (rprec)                              :: g2
      REAL (rprec)                              :: start_time

!  local parameters
      REAL (rprec), DIMENSION(4), PARAMETER     :: dummy_value = 0.0

!  Start of executable code
      start_time = profiler_get_start_time()

      eq_steps = 1

      WRITE (*,*) ' *** In task v3post'
      WRITE (context%runlog_iou,*) ' *** In task v3post'

!  Initialize v3post
      CALL init_equilibrium(context)
      CALL init_signals(context)
      CALL init_gaussian_process(context)

!  Solve equilibrium
      WRITE (*,*) ' *** Starting initial equilibrium convergence (v3p)'
      WRITE (context%runlog_iou,*) ' *** Starting initial ' //                 &
     &                             'equilibrium convergence'
      converged = model_converge(context%model, eq_steps,                      &
     &                           context%runlog_iou,                           &
#if defined(MPI_OPT)
     &                           context%equilibrium_comm, 'All')
      CALL MPI_BCAST(mpi_quit_task, 1, MPI_INTEGER, 0,                         &
     &               context%equilibrium_comm, error)
#else
     &                           0, 'All')
#endif

!  Set the guassian processes
      DO i = 1, SIZE(context%gp)
         CALL gaussp_set_profile(context%gp(i)%p, context%model)
      END DO

!  Calculate and display the signals.
      WRITE (*,*) ' *** Calculating modeled signals'
      WRITE (context%runlog_iou,*) ' *** Calculating modeled signals'

      g2 = 0.0

!  The combination signals cannot be computed in parallel since the depend on
!  other signals previously computed. If combinations are used, the signals need
!  to be split into two loops.
      IF (context%combination_index .eq. -1) THEN
         last_para_signal = SIZE(context%signals)
      ELSE
         last_para_signal = context%combination_index - 1
      END IF

!$OMP PARALLEL DO
!$OMP& SCHEDULE(DYNAMIC)
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i)
!$OMP& REDUCTION(+:g2)
      DO i = 1, last_para_signal
         g2 = g2 + signal_get_g2(context%signals(i)%p,                         &
     &                           context%model, .false., dummy_value)
      END DO
!$OMP END PARALLEL DO

      DO i = last_para_signal + 1, SIZE(context%signals)
         g2 = g2 + context%signals(i)%p%get_g2(context%model, .false.,         &
     &                                         dummy_value)
      END DO

      WRITE (*,1000) g2
      WRITE (context%runlog_iou,1000) g2

      CALL v3fit_context_init_data(context, eq_steps)

      CALL profiler_set_stop_time('task_v3post', start_time)

1000  FORMAT('g^2 = ',es12.5)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Reconstructs the equilibrium.
!>
!>  Task to perform an equilibrium reconstruction. This task is choosen by
!>  setting @ref v3fit_input::my_task to 'reconstruct'.
!>
!>  @param[inout] context An instance of a @ref v3fit_context object.
!-------------------------------------------------------------------------------
      SUBROUTINE task_reconstruct(context)
      USE v3fit_context

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context

!  local variables
      INTEGER                                   :: eq_steps
      INTEGER                                   :: restart_step
      INTEGER                                   :: i, error
      LOGICAL                                   :: eq_converged
      CHARACTER (len=dir_prefix_len)            :: directory
      LOGICAL                                   :: write_input
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (*,*) ' *** In task reconstruct'

!  Initialize the reconstruction.
      write_input = commandline_parser_is_flag_set(context%cl_parser,          &
     &                                             '-out')

      CALL init_equilibrium(context)
      CALL init_signals(context)
      CALL init_gaussian_process(context)
      CALL init_parameters(context)
      CALL init_reconstruction(context)

      IF (commandline_parser_is_flag_set(context%cl_parser,                    &
     &                                   '-restart')) THEN
         WRITE (*,*) ' *** Restarting reconstruction '

         eq_steps = v3fit_context_restart(context, restart_step)
#if defined(MPI_OPT)
!  Sync the number of equilibrium steps the inital convergence took so that the'
!  child processes start in the same place.
         CALL MPI_BCAST(eq_steps, 1, MPI_INTEGER, 0,                           &
     &                  context%reconstruction_comm, error)
#endif
      ELSE
!  Solve equilibrium
         WRITE (*,*) ' *** Starting initial equilibrium ' //                   &
     &               'convergence (rec)'
         WRITE (context%runlog_iou,*) ' *** Starting initial ' //              &
     &                             'equilibrium convergence'

         eq_steps = 1
         eq_converged = reconstruction_eval_e(context%recon,                   &
     &                                        context%signals,                 &
     &                                        context%model, context%gp,       &
     &                                        eq_steps,                        &
     &                                        context%runlog_iou,              &
#if defined(MPI_OPT)
     &                                        context%equilibrium_comm)
#else
     &                                        0)
#endif
         CALL assert(eq_converged,                                             &
     &               'task_reconstruct no inital convergence')
         CALL reconstruction_eval_f(context%recon,                             &
     &                              context%derived_params,                    &
     &                              context%model)

!  Save the signal values from the first 0th reconstruction step.
         DO i = 1, SIZE(context%signals)
            context%recon%last_values(:,i) =                                   &
     &         context%signals(i)%p%modeled
         END DO

#if defined(MPI_OPT)
!  Sync the number of equilibrium steps the inital convergence took so that the'
!  child processes start in the same place.
         CALL MPI_BCAST(eq_steps, 1, MPI_INTEGER, 0,                           &
     &                  context%reconstruction_comm, error)
#endif

         WRITE (*,*)
         WRITE (*,1002) 0
         WRITE (*,1003) reconstruction_get_g2(context%recon)

         WRITE (context%runlog_iou,*)
         WRITE (context%runlog_iou,1002) 0
         WRITE (context%runlog_iou,1003)                                       &
     &      reconstruction_get_g2(context%recon)

         IF (write_input) THEN
            CALL context%model%equilibrium%write_input(0)
         END IF

         CALL v3fit_context_init_data(context, eq_steps)

         restart_step = 1
      END IF

      DO i = restart_step, SIZE(context%recon%e, 2) - 1
         WRITE (*,*)
         WRITE (*,1002) i

         WRITE (context%runlog_iou,*)
         WRITE (context%runlog_iou,1002) i

         IF (reconstruction_step(context%recon, context%signals,               &
     &                           context%derived_params, context%locks,        &
     &                           context%model, context%gp,                    &
     &                           context%params, eq_steps,                     &
     &                           context%runlog_iou,                           &
#if defined(MPI_OPT)
     &                           context%reconstruction_comm,                  &
     &                           context%equilibrium_comm)) THEN
#else
     &                           0, 0)) THEN
#endif

!  Step was successful. Post process the result.
            CALL reconstruction_eval_sem(context%recon,                        &
     &                                   context%params,                       &
     &                                   context%signals,                      &
     &                                   context%derived_params)
            CALL reconstruction_write_step(context%recon,                      &
     &                                     context%runlog_iou)
            CALL v3fit_context_write_step_data(context, .false.,               &
     &                                         eq_steps)

            IF (write_input) THEN
               CALL context%model%equilibrium%write_input(i)
            END IF

!  Test for convergence.
            IF (reconstruction_get_dg2(context%recon) .le.                     &
     &          context%recon_stop .or.                                        &
     &          reconstruction_get_g2(context%recon) .eq. 0.0) THEN
               WRITE (*,*)
               WRITE (*,*) ' *** Equilibrium reconstructed'
               WRITE (*,1000) context%recon_stop,                              &
     &                        reconstruction_get_dg2(context%recon)

               WRITE (context%runlog_iou,*)
               WRITE (context%runlog_iou,*) ' *** Equilibrium ' //             &
     &                                      'reconstructed'
               WRITE (context%runlog_iou,1000) context%recon_stop,             &
     &                        reconstruction_get_dg2(context%recon)
               EXIT
            END IF

            IF (i .eq. SIZE(context%recon%e, 2) - 1) THEN
              WRITE (*,*)
              WRITE (*,1001) i

              WRITE (context%runlog_iou,*)
              WRITE (context%runlog_iou,1001) i
            END IF
         ELSE
            EXIT
         END IF
      END DO

#if defined(MPI_OPT)
      CALL MPI_BCAST(mpi_quit_task, 1, MPI_INTEGER, 0,                         &
     &               context%reconstruction_comm, error)
      CALL MPI_BCAST(mpi_quit_task, 1, MPI_INTEGER, 0,                         &
     &               context%equilibrium_comm, error)
#endif

      CALL profiler_set_stop_time('task_reconstruct', start_time)

1000  FORMAT('dg2_stop = ',es12.5,' change in g^2 = ',es12.5)
1001  FORMAT('nrstep 'i4,' completed. May be reconstructed.')
1002  FORMAT('  *** Reconstruction step ',i4)
1003  FORMAT('Initial g^2 = ',es12.5)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Child task to compute jacobian.
!>
!>  Task to compute the jacobian in a sub process for parallelism.
!>
!>  @param[inout] context An instance of a @ref v3fit_context object.
!-------------------------------------------------------------------------------
      SUBROUTINE task_child_recon(context)
      USE v3fit_context

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context

#if defined(MPI_OPT)
!  local variables
      INTEGER                                   :: i
      INTEGER                                   :: signals_created
      INTEGER                                   :: task
      INTEGER                                   :: error
      INTEGER                                   :: eq_steps
      REAL (rprec)                              :: step_use
      LOGICAL                                   :: temp

!  Start of executable code
      CALL init_equilibrium(context)
      CALL init_signals(context)
      CALL init_gaussian_process(context)
      CALL init_parameters(context)
      CALL init_reconstruction(context)

!  Wait for parent process to finish the initial equilibrium. Sync the number of
!  equilibrium steps so that the child processes match after the inital
!  equilibrium convergence.
      CALL MPI_BCAST(eq_steps, 1, MPI_INTEGER, 0,                              &
     &               context%reconstruction_comm, error)

      DO
         CALL MPI_BCAST(task, 1, MPI_INTEGER, 0,                               &
     &                  context%reconstruction_comm, error)

         SELECT CASE (task)

            CASE (mpi_jacobian_task)
               CALL MPI_BCAST(eq_steps, 1, MPI_INTEGER, 0,                     &
     &                        context%reconstruction_comm, error)
               CALL reconstruction_eval_jacobians(context%recon,               &
     &                 context%signals, context%derived_params,                &
     &                 context%locks, context%model, context%gp,               &
     &                 context%params, eq_steps, context%runlog_iou,           &
     &                 context%reconstruction_comm,                            &
     &                 context%equilibrium_comm)

            CASE (mpi_sync_task)
               CALL reconstruction_sync_svd(context%recon,                     &
     &                                      context%reconstruction_comm)
               DO i = 1, SIZE(context%params)
                  CALL param_sync_delta(context%params(i)%p,                   &
     &                                  context%reconstruction_comm)
               END DO

            CASE (mpi_step_task)
               CALL MPI_BCAST(step_use, 1, MPI_REAL8, 0,                       &
     &                        context%reconstruction_comm, error)
               CALL MPI_BCAST(eq_steps, 1, MPI_INTEGER, 0,                     &
     &                        context%reconstruction_comm, error)
               temp = reconstruction_try_step(context%recon,                   &
     &                   context%signals, context%derived_params,              &
     &                   context%locks, context%model, context%gp,             &
     &                   context%params, eq_steps, step_use,                   &
     &                   context%runlog_iou,                                   &
     &                   context%reconstruction_comm,                          &
     &                   context%equilibrium_comm)

            CASE (mpi_quit_task)
               CALL MPI_BCAST(mpi_quit_task, 1, MPI_INTEGER, 0,                &
     &                        context%equilibrium_comm, error)
               EXIT

            CASE DEFAULT
               WRITE (*,*) 'Unknown MPI task.'
               CALL EXIT(1)

         END SELECT
      END DO
#endif

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Child task to compute equilibrium.
!>
!>  Task to compute the equilibrium in a sub process for parallelism.
!>
!>  @param[inout] context An instance of a @ref v3fit_context object.
!-------------------------------------------------------------------------------
      SUBROUTINE task_child_equilibrium(context)
      USE v3fit_context

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context

#if defined(MPI_OPT)
!  local variables
      INTEGER                                   :: task
      LOGICAL                                   :: result
      INTEGER                                   :: eq_steps
      INTEGER                                   :: error

!  Start of executable code
      eq_steps = 1
      CALL init_equilibrium(context)

      DO
         CALL MPI_BCAST(task, 1, MPI_INTEGER, 0,                               &
     &                  context%equilibrium_comm, error)

         SELECT CASE (task)

            CASE (mpi_equilibrium_task)
               CALL MPI_BCAST(context%model%state_flags, 1, MPI_INTEGER,       &
     &                        0, context%equilibrium_comm, error)
               result = context%model%equilibrium%converge(eq_steps,           &
     &                     context%runlog_iou,                                 &
     &                     context%equilibrium_comm,                           &
     &                     context%model%state_flags)

            CASE (mpi_mgrid_task)
               CALL MPI_BCAST(eq_steps, 1, MPI_INTEGER, 0,                     &
     &                        context%equilibrium_comm, error)
               CALL context%model%equilibrium%read_vac_file(eq_steps,          &
     &                 context%equilibrium_comm)

            CASE (mpi_quit_task)
               EXIT

            CASE DEFAULT
               WRITE (*,*) 'Unknown MPI task.'
               CALL EXIT(1)

         END SELECT
      END DO
#endif
      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Computes the g^2 value on a grid.
!>
!>  Task to map out the chi^2 over a range of reconstruction parameters. This
!>  task is choosen by setting @ref v3fit_input::my_task to 'gsq_on_grid'.
!>
!>  @param[inout] context An instance of a @ref v3fit_context object.
!-------------------------------------------------------------------------------
      SUBROUTINE task_gsq_on_grid(context)
      USE v3fit_context

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context

!  local variables
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (*,*) ' *** In task gsq_on_grid'

!  Initialize the reconstruction.
      CALL init_equilibrium(context)
      CALL init_signals(context)
      CALL init_parameters(context)
      CALL init_reconstruction(context)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Run unit tests
!>
!>  This task is choosen by setting @ref v3fit_input::my_task to 'unit_test'. Or
!>  by using the -test command line flag.
!-------------------------------------------------------------------------------
      SUBROUTINE task_unit_tests
      USE line_segment
      USE coordinate_utilities
      USE functions
      USE integration_path
      USE fourier

      IMPLICIT NONE

!  local variables
      LOGICAL :: tests_failed

!  Start of executable code
      WRITE (*,*) ' *** In task unit_test'
      tests_failed = .false.

! Line segment tests
      IF (line_seg_test()) THEN
         WRITE (*,*) 'Line Segment Test Passed'
      ELSE
         tests_failed = .true.
      END IF

! Coordinate tests
      IF (cood_utils_test()) THEN
         WRITE (*,*) 'Coord Utilites Test Passed'
      ELSE
         tests_failed = .true.
      END IF

! Profile function tests
      IF (function_test()) THEN
         WRITE (*,*) 'Functions Test Passed'
      ELSE
         tests_failed = .true.
      END IF

! Integration path tests
      IF (path_test()) THEN
         WRITE (*,*) 'Integration Path Test Passed'
      ELSE
         tests_failed = .true.
      END IF

!  SIESTA fourier unit test.
      IF (test_fourier()) THEN
         WRITE (*,*) 'SIESTA Fourier Test Passed'
      ELSE
         tests_failed = .true.
      END IF

!  The CTest unit test frame work chooses a pass fail based on the exit code of
!  the program.
      IF (tests_failed) THEN
         CALL EXIT(1)
      END IF

      END SUBROUTINE

!*******************************************************************************
!  INITIALIZE SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Initialize an equilibrium.
!>
!>  This constructs and initializes an equilibrium. Currently only
!>  @ref vmec_equilibium, @ref vacuum_equilibium and @ref siesta_equilibium are
!>  supported.
!>
!>  @param[inout] context An instance of a @ref v3fit_context object.
!-------------------------------------------------------------------------------
      SUBROUTINE init_equilibrium(context)
      USE v3fit_context

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context

!  local variables
      LOGICAL                                   :: force_solve
      INTEGER                                   :: eq_rank
      INTEGER                                   :: error
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      eq_rank = 0
#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(context%equilibrium_comm, eq_rank, error)
#endif

      WRITE (*,*) ' *** Constructing equilibrium model'
      IF (eq_rank .eq. 0) THEN
         WRITE (context%runlog_iou,*) ' *** Constructing ' //                  &
     &                                'equilibrium model'
      END IF

      force_solve =                                                            &
     &   commandline_parser_is_flag_set(context%cl_parser, '-force')

      SELECT CASE (model_eq_type)

         CASE ('vmec')
            CALL init_vmec_equilibrium(context, force_solve)

         CASE ('vacuum')
            CALL init_vacuum_equilibrium(context, force_solve)

         CASE ('siesta')
            CALL init_siesta_equilibrium(context, force_solve)

         CASE DEFAULT
           WRITE (*,*) 'Equilibrium not initialized. Please ' //               &
     &                 'specify a vaild equilibrium type.'
           CALL EXIT(1)

      END SELECT

      CALL profiler_set_stop_time('init_equilibrium', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Initialize a vmec equilibrium.
!>
!>  This constructs and initializes a @ref vmec_equilibrium.
!>
!>  @param[inout] context     An instance of a @ref v3fit_context object.
!>  @param[in]    force_solve If true, forces the equilbirum to resolve every
!>                            time.
!-------------------------------------------------------------------------------
      SUBROUTINE init_vmec_equilibrium(context, force_solve)
      USE v3fit_context
      USE integration_path

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout)      :: context
      LOGICAL, INTENT(in)                            :: force_solve

!  local variables
      TYPE (pprofile_pointer), DIMENSION(:), POINTER :: sxr => null()
      INTEGER                                        :: i
      LOGICAL                                        :: not_converged
      REAL (rprec)                                   :: start_time
      TYPE (emission_class), POINTER :: emission_func => null()
      TYPE (vmec_class), POINTER                     :: vmec => null()
      INTEGER                                        :: state_flags

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(sxr(num_sxrem_p))
      DO i = 1, num_sxrem_p
         sxr(i)%p => pprofile_construct(TRIM(pp_sxrem_ptype_a(i)),             &
     &                                  pp_sxrem_b_a(i,:),                     &
     &                                  pp_sxrem_as_a(i,:),                    &
     &                                  pp_sxrem_af_a(i,:))
      END DO

!  Create an emission object if an emission file was provided.
      IF (TRIM(emission_file) .ne. '') THEN
         emission_func => emission_construct(emission_file)
      END IF

      state_flags = model_state_all_off
      vmec => vmec_construct(vmec_nli_filename, vmec_wout_input,               &
     &                       pprofile_construct(TRIM(pp_ne_ptype),             &
     &                                          pp_ne_b, pp_ne_as,             &
     &                                          pp_ne_af),                     &
     &                       pprofile_construct(TRIM(pp_te_ptype),             &
     &                                          pp_te_b, pp_te_as,             &
     &                                          pp_te_af),                     &
     &                       pprofile_construct(TRIM(pp_ti_ptype),             &
     &                                          pp_ti_b, pp_ti_as,             &
     &                                          pp_ti_af),                     &
     &                       pprofile_construct(TRIM(pp_ze_ptype),             &
     &                                          pp_ze_b, pp_ze_as,             &
     &                                          pp_ze_af), sxr,                &
     &                       phi_offset, z_offset, pol_rad_ratio,              &
     &                       context%runlog_iou,                               &
#if defined(MPI_OPT)
     &                       context%equilibrium_comm,                         &
     &                       context%reconstruction_comm, state_flags)
#else
     &                       0, 0, state_flags)
#endif

      context%model => model_construct(model_ne_type,                          &
     &   model_sxrem_type_a(1:num_sxrem_p), model_te_type,                     &
     &   model_ti_type, model_ze_type, ne_pp_unit, ne_min, te_min,             &
     &   ti_min, ze_min, sxrem_min(1:num_sxrem_p), e_pressure_fraction,        &
     &   emission_func, equilibrium_construct(vmec, force_solve),              &
     &   sxrem_te_a, sxrem_ratio_a, ece_resonance_range, coosig_wgts,          &
     &   state_flags,                                                          &
     &   sfactor_spec_fac(:MINLOC(sfactor_spec_imin, 1) - 1),                  &
     &   soffset_spec_fac(:MINLOC(soffset_spec_imin, 1) - 1),                  &
     &   path_construct(int_method, int_num_points, int_size))

      CALL profiler_set_stop_time('init_vmec_equilibrium', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Initialize a vacuum equilibrium.
!>
!>  This constructs and initializes a @ref vacuum_equilibrium.
!>
!>  @param[inout] context     An instance of a @ref v3fit_context object.
!>  @param[in]    force_solve If true, forces the equilbirum to resolve every
!>                            time.
!-------------------------------------------------------------------------------
      SUBROUTINE init_vacuum_equilibrium(context, force_solve)
      USE v3fit_context
      USE integration_path

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context
      LOGICAL, INTENT(in)                       :: force_solve

!  local variables
      REAL (rprec)                              :: start_time
      TYPE (emission_class), POINTER :: emission_func => null()

!  Start of executable code
      start_time = profiler_get_start_time()

!  Create a emission object if an emission file was provided.
      IF (TRIM(emission_file) .ne. '') THEN
         emission_func => emission_construct(emission_file)
      END IF

      context%model => model_construct(model_ne_type,                          &
     &   model_sxrem_type_a(1:num_sxrem_p), model_te_type,                     &
     &   model_ti_type, model_ze_type, ne_pp_unit, ne_min, te_min,             &
     &   ti_min, ze_min, sxrem_min(1:num_sxrem_p), e_pressure_fraction,        &
     &   emission_func,                                                        &
     &   equilibrium_construct(vacuum_construct(vacuum_nli_filename,           &
     &                                          context%runlog_iou),           &
     &                         force_solve),                                   &
     &   sxrem_te_a, sxrem_ratio_a, ece_resonance_range, coosig_wgts,          &
     &   model_state_all_off,                                                  &
     &   sfactor_spec_fac(:MINLOC(sfactor_spec_imin, 1) - 1),                  &
     &   soffset_spec_fac(:MINLOC(soffset_spec_imin, 1) - 1),                  &
     &   path_construct(int_method, int_num_points, int_size))

      CALL profiler_set_stop_time('init_vacuum_equilibrium', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Initialize a siests equilibrium.
!>
!>  This constructs and initializes a @ref siesta_equilibrium.
!>
!>  @param[inout] context     An instance of a @ref v3fit_context object.
!>  @param[in]    force_solve If true, forces the equilbirum to resolve every
!>                            time.
!-------------------------------------------------------------------------------
      SUBROUTINE init_siesta_equilibrium(context, force_solve)
      USE v3fit_context
      USE integration_path

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout)      :: context
      LOGICAL, INTENT(in)                            :: force_solve

!  local variables
      REAL (rprec)                                   :: start_time
      TYPE (pprofile_pointer), DIMENSION(:), POINTER :: sxr => null()
      INTEGER                                        :: i
      LOGICAL                                        :: not_converged
      TYPE (emission_class), POINTER :: emission_func => null()
      TYPE (vmec_class), POINTER                     :: vmec => null()
      TYPE (siesta_class), POINTER                   :: siesta => null()
      INTEGER                                        :: state_flags

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(sxr(num_sxrem_p))
      DO i = 1, num_sxrem_p
         sxr(i)%p => pprofile_construct(TRIM(pp_sxrem_ptype_a(i)),             &
     &                                  pp_sxrem_b_a(i,:),                     &
     &                                  pp_sxrem_as_a(i,:),                    &
     &                                  pp_sxrem_af_a(i,:))
      END DO

!  Create a emission object if an emission file was provided.
      IF (TRIM(emission_file) .ne. '') THEN
         emission_func => emission_construct(emission_file)
      END IF

      state_flags = model_state_all_off
      vmec => vmec_construct(vmec_nli_filename, vmec_wout_input,               &
     &                       context%runlog_iou,                               &
#if defined(MPI_OPT)
     &                       context%equilibrium_comm,                         &
     &                       context%reconstruction_comm,                      &
#else
     &                       0, 0,
#endif
     &                       state_flags)

      siesta => siesta_construct(siesta_nli_filename,                          &
     &                           siesta_restart_filename,                      &
     &                           pprofile_construct(TRIM(pp_ne_ptype),         &
     &                                              pp_ne_b, pp_ne_as,         &
     &                                              pp_ne_af),                 &
     &                           pprofile_construct(TRIM(pp_te_ptype),         &
     &                                              pp_te_b, pp_te_as,         &
     &                                              pp_te_af),                 &
     &                           pprofile_construct(TRIM(pp_ti_ptype),         &
     &                                              pp_ti_b, pp_ti_as,         &
     &                                              pp_ti_af),                 &
     &                           sxr, phi_offset, z_offset,                    &
     &                           pol_rad_ratio, context%runlog_iou,            &
#if defined(MPI_OPT)
     &                           context%equilibrium_comm,                     &
     &                           context%reconstruction_comm,                  &
#else
     &                           0, 0,                                         &
#endif
     &                           vmec, state_flags,                            &
     &                           vmec_nli_filename, vmec_wout_input)

!  If the woutfile in the v3fit namelist input file is specifed, assume that the
!  equilibrium is converged.
      not_converged = TRIM(siesta_restart_filename) .eq. ''

      context%model => model_construct(model_ne_type,                          &
     &   model_sxrem_type_a(1:num_sxrem_p), model_te_type,                     &
     &   model_ti_type, model_ze_type, ne_pp_unit, ne_min, te_min,             &
     &   ti_min, ze_min, sxrem_min(1:num_sxrem_p), e_pressure_fraction,        &
     &   emission_func, equilibrium_construct_siesta(siesta,                   &
     &                                               force_solve),             &
     &   sxrem_te_a, sxrem_ratio_a, ece_resonance_range, coosig_wgts,          &
     &   state_flags,                                                          &
     &   sfactor_spec_fac(:MINLOC(sfactor_spec_imin, 1) - 1),                  &
     &   soffset_spec_fac(:MINLOC(soffset_spec_imin, 1) - 1),                  &
     &   path_construct(int_method, int_num_points, int_size))

      CALL profiler_set_stop_time('init_siesta_equilibrium', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Constructs the @ref signal array.
!>
!>  This constructs all @ref v3fit_context::v3fit_context_class::signals.
!>
!>  @param[inout] context An instance of a @ref v3fit_context object.
!-------------------------------------------------------------------------------
      SUBROUTINE init_signals(context)
      USE v3fit_context

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context

!  local variables
      INTEGER                                   :: signals_created = 0
      INTEGER                                   :: i
      INTEGER                                   :: j
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (na_s_desc .eq. 0) THEN
         CALL err_warn('init_signals: no signals will be created. ' //         &
     &                 'na_s_desc = 0')
         RETURN
      END IF

      WRITE (*,*) ' *** Constructing signals'
      WRITE (context%runlog_iou,*) ' *** Constructing signals'

      ALLOCATE(context%signals(na_s_desc))

!  If a mdsig filename is given, construct the diagnostics contained inside it.
      IF (mdsig_list_filename .ne. '') THEN
         CALL init_magnetic_signals(context, signals_created)
      END IF

!  If a sxrem filename is given, construct the diagnostics contained inside it.
      IF (sxrch_dot_filename .ne. '') THEN
         CALL init_sxrem_signals(context, signals_created)
      END IF

!  If a intpol filename is given, construct the diagnostics contained inside it.
      IF (ipch_dot_filename .ne. '') THEN
         CALL init_intpol_signals(context, signals_created)
      END IF

!  If a thscte filename is given, construct the diagnostics contained inside it.
      IF (thscte_dot_filename .ne. '') THEN
         CALL init_thomson_signals(context, signals_created)
      END IF

!  If the radial extcur z position isn't negative, construct an extcurz signal.
      IF (extcurz_s0 .ge. 0.0) THEN
         CALL init_extcurz_signals(context, signals_created)
      END IF

!  If a mse filename is given, construct the diagnostics contained inside it.
      IF (mse_dot_filename .ne. '') THEN
         CALL init_mse_signals(context, signals_created)
      END IF

!  If an ece filename is given, construct the diagnostics contained inside it.
      IF (ece_dot_filename .ne. '') THEN
         CALL init_ece_signals(context, signals_created)
      END IF

      CALL init_limiter_signals(context, signals_created)

      CALL init_prior_signals(context, signals_created)

      IF (sxrem_ratio_dot_filename .ne. '') THEN
         CALL init_sxrem_ratio_signals(context, signals_created)
      END IF

      CALL init_combination_signals(context, signals_created)

      CALL v3fit_context_resize(context)

      CALL profiler_set_stop_time('init_signals', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Constructs @ref magnetic signals.
!>
!>  This constructs all @ref magnetic signals from the specified
!>  @ref v3fit_input::mdsig_list_filename.
!>
!>  @param[inout] context         An instance of a @ref v3fit_context object.
!>  @param[inout] signals_created Count of the current number of signals
!>                                constructed.
!-------------------------------------------------------------------------------
      SUBROUTINE init_magnetic_signals(context, signals_created)
      USE v3fit_context
      USE safe_open_mod
      USE v3_utilities
      USE ezcdf
      USE file_opts
      USE magnetic

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context
      INTEGER, INTENT(inout)                    :: signals_created

!  local variables
      INTEGER                                   :: plasma_index
      LOGICAL                                   :: use_point
      INTEGER                                   :: mdsig_list_iou
      INTEGER                                   :: mdsig_iou
      INTEGER                                   :: status
      INTEGER                                   :: mdsig_index
      CHARACTER (len=path_length)               :: mdsig_path
      CHARACTER (len=path_length)               :: mdsig_filename
      INTEGER                                   :: mpi_rank
      CLASS (magnetic_class), POINTER           :: magnetic_object
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      status = 0
      mdsig_list_iou = 0

      mpi_rank = 0
#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(context%reconstruction_comm, mpi_rank, status)
#endif
      IF (mpi_rank .gt. 0) THEN
         IF (.not.is_absolute_path(mdsig_list_filename)) THEN
!  Child jacobian processes run in a sub directory.
            mdsig_list_filename = build_path('..', mdsig_list_filename)
         END IF
      END IF

      WRITE (*,*) ' *** Constructing magnetic signals from ' //                &
     &            TRIM(mdsig_list_filename)
      WRITE (context%runlog_iou,*) ' *** Constructing magnetic ' //            &
     &                             'signals from ' //                          &
     &                             TRIM(mdsig_list_filename)

      CALL safe_open(mdsig_list_iou, status, TRIM(mdsig_list_filename),        &
     &               'old', 'formatted')
      CALL assert_eq(0, status, 'init_magnetic_signals: ' //                   &
     &               'failed to open ' // TRIM(mdsig_list_filename))

!  Get the directory of the mdsig path.
      mdsig_path = get_path_of_file(mdsig_list_filename)

!  Need to find an index of a magnetic resonse that uses the plasma response
!  function. Also need to check for the use of a point.
      plasma_index = 0
      use_point = .false.

      cut_comp_svd =                                                           &
     &   commandline_parser_get_real(context%cl_parser, '-c',                  &
     &                               cut_comp_svd)

!  Loop over each file in the list file
      DO
         READ(mdsig_list_iou, *, iostat=status)                                &
     &      mdsig_index, mdsig_filename

!  When the end of the file is reached, the read will return an error. Break out
!  of the loop.
         IF (status .ne. 0) EXIT

!  Check if there are two many signals.
         IF (signals_created + 1 .gt. v3fit_max_diagnostics) THEN
            CALL err_fatal('init_magnetic_signals: created signals' //         &
     &                     ' exceeds v3fit_max_diagnostics')
         END IF

!  Open the netcdf file for the mdsig.
         CALL cdf_open(mdsig_iou, TRIM(build_path(mdsig_path,                  &
     &                                            mdsig_filename)),            &
     &                 'r', status)
         CALL assert_eq(0, status, 'init_magnetic_signals: ' //                &
     &                  'failed to open ', TRIM(mdsig_path) //                 &
     &                  TRIM(mdsig_filename))

!  Construct a magnetic diagnostic signal using from the mdsig netcdf file.
         signals_created = signals_created + 1
         magnetic_object =>                                                    &
     &      magnetic_class(mdsig_iou, mag_a(signals_created),                  &
     &                     mag_force_coil, mag_3D_a(signals_created),          &
     &                     cut_comp_svd)

         CALL signal_construct(magnetic_object, mdsig_iou,                     &
     &           sdo_data_a(signals_created),                                  &
     &           sdo_sigma_a(signals_created),                                 &
     &           sdo_weight_a(signals_created),                                &
     &           v3fit_input_find_scale_index(signals_created),                &
     &           v3fit_input_find_offset_index(signals_created))

         CALL cdf_close(mdsig_iou)

         context%signals(signals_created)%p => magnetic_object

         IF (plasma_index .eq. 0 .and.                                         &
     &       magnetic_response_use_plasma(                                     &
     &          magnetic_object%response)) THEN
            plasma_index = signals_created
            CALL context%model%equilibrium%set_magnetic_cache(                 &
     &              magnetic_object%response, context%model%state_flags)
         END IF

         IF (.not.use_point .and.                                              &
     &       magnetic_response_is_point(magnetic_object%response)) THEN
            use_point = .true.
            CALL  context%model%equilibrium%set_magnetic_cache(                               &
     &              ANY(mag_3D_a), context%model%state_flags)
         END IF

!  At lease one magnetic signal was made. Mark the index of it in the context.
!  This should only be run once.
         IF (context%magnetic_index .eq. -1) THEN
            context%magnetic_index = signals_created
         END IF
      END DO

      CLOSE(mdsig_list_iou)

      CALL profiler_set_stop_time('init_magnetic_signals', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Constructs @ref sxrem signals.
!>
!>  This constructs all @ref sxrem signals from the specified
!>  @ref v3fit_input::sxrch_dot_filename.
!>
!>  @param[inout] context         An instance of a @ref v3fit_context object.
!>  @param[inout] signals_created Count of the current number of signals
!>                                constructed.
!-------------------------------------------------------------------------------
      SUBROUTINE init_sxrem_signals(context, signals_created)
      USE v3fit_context
      USE sxrch_dot
      USE file_opts

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context
      INTEGER, INTENT(inout)                    :: signals_created

!  local variables
      INTEGER                                   :: mpi_rank
      INTEGER                                   :: error
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      mpi_rank = 0
#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(context%reconstruction_comm, mpi_rank, error)
#endif
      IF (mpi_rank .gt. 0) THEN
         IF (.not.is_absolute_path(sxrch_dot_filename)) THEN
!  Child jacobian processes run in a sub directory.
            sxrch_dot_filename = build_path('..', sxrch_dot_filename)
         END IF
      END IF

      WRITE (*,*) ' *** Constructing sxrem signals from ' //                   &
     &            TRIM(sxrch_dot_filename)
      WRITE (context%runlog_iou,*) ' *** Constructing sxrem ' //               &
     &                             'signals from ' //                          &
     &                             TRIM(sxrch_dot_filename)

!  The setting of the sxrem_index for sxrem happens inside sxrch_dot_parse_chord
      CALL sxrch_dot_read(sxrch_dot_filename, context%signals,                 &
     &                    signals_created, sdo_data_a, sdo_sigma_a,            &
     &                    sdo_weight_a, context%sxrem_index)

      CALL profiler_set_stop_time('init_sxrem_signals', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Constructs @ref intpol signals.
!>
!>  This constructs all @ref intpol signals from the specified
!>  @ref v3fit_input::ipch_dot_filename.
!>
!>  @param[inout] context         An instance of a @ref v3fit_context object.
!>  @param[inout] signals_created Count of the current number of signals
!>                                constructed.
!-------------------------------------------------------------------------------
      SUBROUTINE init_intpol_signals(context, signals_created)
      USE v3fit_context
      USE ipch_dot
      USE file_opts

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context
      INTEGER, INTENT(inout)                    :: signals_created

!  local variables
      LOGICAL                                   :: use_polarimetry
      INTEGER                                   :: mpi_rank
      INTEGER                                   :: error
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      mpi_rank = 0
#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(context%reconstruction_comm, mpi_rank, error)
#endif
      IF (mpi_rank .gt. 0) THEN
         IF (.not.is_absolute_path(ipch_dot_filename)) THEN
!  Child jacobian processes run in a sub directory.
            ipch_dot_filename = build_path('..', ipch_dot_filename)
         END IF
      END IF

      WRITE (*,*) ' *** Constructing intpol signals from ' //                  &
     &            TRIM(ipch_dot_filename)
      WRITE (context%runlog_iou,*) ' *** Constructing intpol ' //              &
     &                             'signals from ' //                          &
     &                             TRIM(ipch_dot_filename)

!  The setting of the intpol_index for sxrem happens inside ipch_dot_parse_chord
      CALL ipch_dot_read(ipch_dot_filename, context%signals,                   &
     &                   signals_created, sdo_data_a, sdo_sigma_a,             &
     &                   sdo_weight_a, context%intpol_index,                   &
     &                   use_polarimetry)

      IF (use_polarimetry .and.                                                &
     &    .not.equilibrium_is_using_point(                                     &
     &            context%model%equilibrium)) THEN
         CALL context%model%equilibrium%set_magnetic_cache(                    &
     &           .false., context%model%state_flags)
      END IF

      CALL profiler_set_stop_time('init_intpol_signals', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Constructs @ref thomson signals.
!>
!>  This constructs all @ref thomson signals from the specified
!>  @ref v3fit_input::thscte_dot_filename.
!>
!>  @param[inout] context         An instance of a @ref v3fit_context object.
!>  @param[inout] signals_created Count of the current number of signals
!>                                constructed.
!-------------------------------------------------------------------------------
      SUBROUTINE init_thomson_signals(context, signals_created)
      USE v3fit_context
      USE thscte_dot
      USE file_opts

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context
      INTEGER, INTENT(inout)                    :: signals_created

!  local variables
      INTEGER                                   :: mpi_rank
      INTEGER                                   :: error
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      mpi_rank = 0
#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(context%reconstruction_comm, mpi_rank, error)
#endif
      IF (mpi_rank .gt. 0) THEN
         IF (.not.is_absolute_path(thscte_dot_filename)) THEN
!  Child jacobian processes run in a sub directory.
            thscte_dot_filename = build_path('..', thscte_dot_filename)
         END IF
      END IF

      WRITE (*,*) ' *** Constructing thomson signals from ' //                 &
     &            TRIM(thscte_dot_filename)
      WRITE (context%runlog_iou,*) ' *** Constructing thomson ' //             &
     &                             'signals from ' //                          &
     &                             TRIM(thscte_dot_filename)

!  The setting of the thomson_index for sxrem happens inside
!  thscte_dot_parse_chord
      CALL thscte_dot_read(thscte_dot_filename, context%signals,               &
     &                     signals_created, sdo_data_a, sdo_sigma_a,           &
     &                     sdo_weight_a, context%thomson_index)

      CALL profiler_set_stop_time('init_thomson_signals', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Constructs the @ref extcurz signal.
!>
!>  This constructs the extcurz signal from @ref v3fit_input::extcurz_s0 and
!>  @ref v3fit_input::extcurz_u0.
!>
!>  @param[inout] context         An instance of a @ref v3fit_context object.
!>  @param[inout] signals_created Count of the current number of signals
!>                                constructed.
!-------------------------------------------------------------------------------
      SUBROUTINE init_extcurz_signals(context, signals_created)
      USE v3fit_context
      USE extcurz

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context
      INTEGER, INTENT(inout)                    :: signals_created

!  local variables
      INTEGER                                   :: mpi_rank
      INTEGER                                   :: error
      CLASS (signal_class), POINTER             :: extcurz_object
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (*,*) ' *** Constructing extcurz signals'
      WRITE (context%runlog_iou,*) ' *** Constructing extcurz signals'

!  Check if there are two many signals.
      IF (signals_created + 1 .gt. v3fit_max_diagnostics) THEN
         CALL err_fatal('init_extcurz_signals: created signals' //             &
     &                  ' exceeds v3fit_max_diagnostics')
      END IF

      extcurz_object => extcurz_class(extcurz_s0, extcurz_u0)

      CALL signal_construct(extcurz_object, 'extcurz',                         &
     &       'EXTernal CURrent along Z', 'A',                                  &
     &       sdo_data_a(signals_created + 1),                                  &
     &       sdo_sigma_a(signals_created + 1),                                 &
     &       sdo_weight_a(signals_created + 1),                                &
     &       v3fit_input_find_scale_index(signals_created + 1),                &
     &       v3fit_input_find_offset_index(signals_created + 1))

      context%signals(signals_created + 1)%p => extcurz_object

      signals_created = signals_created + 1

!  At lease one extcurz signal was made. Mark the index of it in the context.
!  This should only be run once.
      IF (context%extcurz_index .eq. -1) THEN
         context%extcurz_index = signals_created
      END IF

      CALL profiler_set_stop_time('init_extcurz_signals', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Constructs @ref mse signals.
!>
!>  This constructs all @ref mse signals from the specified
!>  @ref v3fit_input::mse_dot_filename.
!>
!>  @param[inout] context         An instance of a @ref v3fit_context object.
!>  @param[inout] signals_created Count of the current number of signals
!>                                constructed.
!-------------------------------------------------------------------------------
      SUBROUTINE init_mse_signals(context, signals_created)
      USE v3fit_context
      USE mse_dot
      USE file_opts

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context
      INTEGER, INTENT(inout)                    :: signals_created

!  local variables
      INTEGER                                   :: mpi_rank
      INTEGER                                   :: error
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      mpi_rank = 0
#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(context%reconstruction_comm, mpi_rank, error)
#endif
      IF (mpi_rank .gt. 0) THEN
         IF (.not.is_absolute_path(mse_dot_filename)) THEN
!  Child jacobian processes run in a sub directory.
            mse_dot_filename = build_path('..', mse_dot_filename)
         END IF
      END IF

      WRITE (*,*) ' *** Constructing mse signals from ' //                     &
     &            TRIM(mse_dot_filename)
      WRITE (context%runlog_iou,*) ' *** Constructing mse ' //                 &
     &                             'signals from ' //                          &
     &                             TRIM(mse_dot_filename)

!  The setting of the mse_index for mse happens inside mse_dot_parse_chord
      CALL mse_dot_read(mse_dot_filename, context%signals,                     &
     &                  signals_created, sdo_data_a, sdo_sigma_a,              &
     &                  sdo_weight_a, context%mse_index)

      IF (context%mse_index .gt. 0 .and.                                       &
     &    .not.context%model%equilibrium%is_using_point()) THEN
         CALL context%model%equilibrium%set_magnetic_cache(                    &
     &           .false., context%model%state_flags)
      END IF

      CALL profiler_set_stop_time('init_mse_signals', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Constructs @ref ece signals.
!>
!>  This constructs all @ref ece signals from the specified
!>  @ref v3fit_input::ece_dot_filename.
!>
!>  @param[inout] context         An instance of a @ref v3fit_context object.
!>  @param[inout] signals_created Count of the current number of signals
!>                                constructed.
!-------------------------------------------------------------------------------
      SUBROUTINE init_ece_signals(context, signals_created)
      USE v3fit_context
      USE ece_dot
      USE file_opts

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context
      INTEGER, INTENT(inout)                    :: signals_created

!  local variables
      INTEGER                                   :: mpi_rank
      INTEGER                                   :: error
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      mpi_rank = 0
#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(context%reconstruction_comm, mpi_rank, error)
#endif
      IF (mpi_rank .gt. 0) THEN
         IF (.not.is_absolute_path(ece_dot_filename)) THEN
!  Child jacobian processes run in a sub directory.
            ece_dot_filename = build_path('..', ece_dot_filename)
         END IF
      END IF

      WRITE (*,*) ' *** Constructing ece signals from ' //                     &
     &            TRIM(ece_dot_filename)
      WRITE (context%runlog_iou,*) ' *** Constructing ece ' //                 &
     &                             'signals from ' //                          &
     &                             TRIM(ece_dot_filename)

!  The setting of the ece_index for ece happens inside ece_dot_parse_chord
      CALL ece_dot_read(ece_dot_filename, context%signals,                     &
     &                  signals_created, sdo_data_a, sdo_sigma_a,              &
     &                  sdo_weight_a, context%ece_index)

      IF (context%ece_index .gt. 0 .and.                                       &
     &    .not.context%model%equilibrium%is_using_point()) THEN
         CALL context%model%equilibrium%set_magnetic_cache(                    &
     &           .false., context%model%state_flags)
      END IF

      CALL profiler_set_stop_time('init_ece_signals', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Constructs @ref limiter signals.
!>
!>  Constructs all iso and grid based limiters.
!>
!>  @param[inout] context         An instance of a @ref v3fit_context object.
!>  @param[inout] signals_created Count of the current number of signals
!>                                constructed.
!-------------------------------------------------------------------------------
      SUBROUTINE init_limiter_signals(context, signals_created)
      USE v3fit_context
      USE stel_constants, only: degree
      USE limiter_iso_T
      USE limiter_grid

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context
      INTEGER, INTENT(inout)                    :: signals_created

!  local variables
      INTEGER                                   :: i
      CHARACTER (len=data_short_name_length)    :: s_name = ''
      CHARACTER (len=data_name_length)          :: l_name = ''
      INTEGER                                   :: num_phi
      CLASS (signal_class), POINTER             :: limiter_obj
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (*,*) ' *** Constructing limiter signals'
      WRITE (context%runlog_iou,*) ' *** Constructing limiter signals'

!  For now, only limiter functions can be created.
      DO i = 1, n_lif
!  Check if there are two many signals
         IF (signals_created + 1 .gt. v3fit_max_signals) THEN
            CALL err_fatal('init_limiter_signals: created signals' //          &
     &                     ' exceeds v3fit_max_signals')
         END IF

!  Check if there are two many limiter signals.
         IF (i .gt. v3fit_max_limiters) THEN
            CALL err_fatal('init_limiter_signals: created signals' //          &
     &                     ' exceeds v3fit_max_limits')
         END IF

         s_name = TRIM(signal_make_short_name('edge_lim_', i))
         WRITE (l_name,'(a,i4)') 'edge limiter iso function ', i

         num_phi = MAX(MIN(v3fit_max_lif_size, n_phi_lif(i)), 1)

!  Check if the sdo_sigma is zero and replace it lif_sigma instead.
         IF (sdo_sigma_a(signals_created + 1) .eq. 0.0) THEN
            sdo_sigma_a(signals_created + 1) = lif_sigma(i)
         END IF

         limiter_obj =>                                                        &
     &      limiter_iso_class(lif_arz(i,:,:), lif_rc(i), lif_zc(i), 20,        &
     &                        lif_phi_degree(i,1:num_phi)*degree,              &
     &                        lif_on_edge(i))

         CALL signal_construct(limiter_obj, s_name, l_name, 'm',              &
     &           sdo_data_a(signals_created + 1),                              &
     &           sdo_sigma_a(signals_created + 1),                             &
     &           sdo_weight_a(signals_created + 1),                            &
     &           v3fit_input_find_scale_index(signals_created + 1),            &
     &           v3fit_input_find_offset_index(signals_created + 1))

         context%signals(signals_created + 1)%p => limiter_obj

         signals_created = signals_created + 1

!  At lease one limiter signal was made. Mark the index of it in the context.
!  This should only be run once.
         IF (context%limiter_index .eq. -1) THEN
            context%limiter_index = signals_created
         END IF

      END DO

!  Add grid limiters to the end.
      IF (limiter_grid_file .ne. '') THEN
         s_name = TRIM(signal_make_short_name('edge_lim_', i))
         WRITE (l_name,'(a,i4)') 'edge limiter polygon ', i

         limiter_obj => limiter_grid_class(limiter_grid_file,                  &
     &                                     lif_on_edge(n_lif + 1))

         CALL signal_construct( limiter_obj, s_name, l_name, 'm',              &
     &           sdo_data_a(signals_created + 1),                              &
     &           sdo_sigma_a(signals_created + 1),                             &
     &           sdo_weight_a(signals_created + 1),                            &
     &           v3fit_input_find_scale_index(signals_created + 1),            &
     &           v3fit_input_find_offset_index(signals_created + 1))

         context%signals(signals_created + 1)%p => limiter_obj

         signals_created = signals_created + 1

!  At lease one limiter signal was made. Mark the index of it in the context.
!  This should only be run once.
         IF (context%limiter_index .eq. -1) THEN
            context%limiter_index = signals_created
         END IF

      END IF

      CALL profiler_set_stop_time('init_limiter_signals', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Constructs @ref prior signals.
!>
!>  For now only @ref prior_gaussian signals are constructed.
!>
!>  @param[inout] context         An instance of a @ref v3fit_context object.
!>  @param[inout] signals_created Count of the current number of signals
!>                                constructed.
!-------------------------------------------------------------------------------
      SUBROUTINE init_prior_signals(context, signals_created)
      USE v3fit_context
      USE prior_gaussian

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context
      INTEGER, INTENT(inout)                    :: signals_created

!  local variables
      CLASS (signal_class), POINTER             :: prior_obj
      INTEGER                                   :: i
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (*,*) ' *** Constructing prior signals'
      WRITE (context%runlog_iou,*) ' *** Constructing prior signals'

!  For now, only limiter functions can be created.
      DO i = 1, n_prior
!  Check if there are two many signals
         IF (signals_created + 1 .gt. v3fit_max_signals) THEN
            CALL err_fatal('init_prior_signals: created signals' //            &
     &                     ' exceeds v3fit_max_signals')
         END IF

!  Check if there are two many prior signals.
         IF (i .gt. v3fit_max_priors) THEN
            CALL err_fatal('init_prior_signals: created signals' //            &
     &                     ' exceeds v3fit_max_priors')
         END IF

         prior_obj => prior_gaussian_class(context%model,                      &
     &                                     prior_param_name(i),                &
     &                                     prior_indices(i,:))

         CALL signal_construct(prior_obj,                                      &
     &           TRIM(signal_make_short_name('prior_', i)),                    &
     &           prior_name(i), prior_units(i),                                &
     &           sdo_data_a(signals_created + 1),                              &
     &           sdo_sigma_a(signals_created + 1),                             &
     &           sdo_weight_a(signals_created + 1),                            &
     &           v3fit_input_find_scale_index(signals_created + 1),            &
     &           v3fit_input_find_offset_index(signals_created + 1))

         context%signals(signals_created + 1)%p => prior_obj

         signals_created = signals_created + 1

!  At lease one prior signal was made. Mark the index of it in the context.
!  This should only be run once.
         IF (context%prior_gaussian_index .eq. -1) THEN
            context%prior_gaussian_index = signals_created
         END IF

      END DO

      CALL profiler_set_stop_time('init_prior_signals', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Constructs @ref sxrem_ratio signals.
!>
!>  This constructs all @ref sxrem_ratio signals from the specified
!>  @ref v3fit_input::sxrem_ratio_dot_filename.
!>
!>  @param[inout] context         An instance of a @ref v3fit_context object.
!>  @param[inout] signals_created Count of the current number of signals
!>                                constructed.
!-------------------------------------------------------------------------------
      SUBROUTINE init_sxrem_ratio_signals(context, signals_created)
      USE v3fit_context
      USE sxrem_ratio_dot
      USE file_opts

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context
      INTEGER, INTENT(inout)                    :: signals_created

!  local variables
      INTEGER                                   :: mpi_rank
      INTEGER                                   :: error
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      mpi_rank = 0
#if defined(MPI_OPT)
      CALL MPI_COMM_RANK(context%reconstruction_comm, mpi_rank, error)
#endif
      IF (mpi_rank .gt. 0) THEN
         IF (.not.is_absolute_path(sxrem_ratio_dot_filename)) THEN
!  Child jacobian processes run in a sub directory.
            sxrem_ratio_dot_filename =                                         &
     &         build_path('..', sxrem_ratio_dot_filename)
         END IF
      END IF

      WRITE (*,*) ' *** Constructing sxrem ratio signals from ' //             &
     &            TRIM(sxrem_ratio_dot_filename)
      WRITE (context%runlog_iou,*) ' *** Constructing intpol ' //              &
     &                             'signals from ' //                          &
     &                             TRIM(sxrem_ratio_dot_filename)

!  The setting of the intpol_index for sxrem happens inside ipch_dot_parse_chord
      CALL sxrem_ratio_dot_read(sxrem_ratio_dot_filename,                      &
     &                          context%signals, signals_created,              &
     &                          sdo_data_a, sdo_sigma_a, sdo_weight_a,         &
     &                          context%sxrem_ratio_index)

      CALL profiler_set_stop_time('init_sxrem_ratio_signals',                  &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Constructs @ref signal::combination_class signals.
!>
!>  @param[inout] context         An instance of a @ref v3fit_context object.
!>  @param[inout] signals_created Count of the current number of signals
!>                                constructed.
!-------------------------------------------------------------------------------
      SUBROUTINE init_combination_signals(context, signals_created)
      USE v3fit_context

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context
      INTEGER, INTENT(inout)                    :: signals_created

!  local variables
      INTEGER                                   :: i
      INTEGER                                   :: j
      CLASS (combination_class), POINTER        :: combination_signal
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (*,*) ' *** Constructing combination signals'
      WRITE (context%runlog_iou,*) ' *** Constructing ' //                     &
     &                             'combination signals'

      DO i = 1, n_coosig
!  Check if there are two many signals
         IF (signals_created + 1 .gt. v3fit_max_signals) THEN
            CALL err_fatal('init_combination_signals: created ' //             &
     &                     'signals exceeds v3fit_max_signals')
         END IF

!  Check if there are two many combination signals.
         IF (i .gt. v3fit_max_combinations) THEN
            CALL err_fatal('init_combination_signals: created ' //             &
     &                     'signals exceeds v3fit_max_combinations')
         END IF

!  Search for the last index.
         combination_signal =>                                                 &
     &      combination_construct(n_sig_coosig(i), coosig_type(i),             &
     &                            coosig_wgts_id(i))
         DO j = 1, n_sig_coosig(i)
            IF (coosig_indices(i,j) .gt. signals_created) THEN
               CALL err_fatal('init_combination_signals: cannot ' //           &
     &                        'create combination signal with ' //             &
     &                        'uninitalized signal')
            END IF

            CALL combination_set_signal(combination_signal,                    &
     &              context%signals(coosig_indices(i,j))%p,                    &
     &              coosig_coeff(i,j), j)
         END DO

         CALL signal_construct(combination_signal, coosig_name(i),             &
     &           coosig_name(i), coosig_units(i),                              &
     &           sdo_data_a(signals_created + 1),                              &
     &           sdo_sigma_a(signals_created + 1),                             &
     &           sdo_weight_a(signals_created + 1),                            &
     &           v3fit_input_find_scale_index(signals_created + 1),            &
     &           v3fit_input_find_offset_index(signals_created + 1))

         context%signals(signals_created + 1)%p => combination_signal

         signals_created = signals_created + 1

!  At lease one combination signal was made. Mark the index of it in the
!  context. This should only be run once.
         IF (context%combination_index .eq. -1) THEN
            context%combination_index = signals_created
         END IF

      END DO

      CALL profiler_set_stop_time('init_combination_signals',                  &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Constructs the @ref v3fit_context::v3fit_context_class::params array.
!>
!>  This constructs all reconstruction @ref v3fit_params::param_class and
!>  derived @ref v3fit_params::param_class objects.
!>
!>  @param[inout] context An instance of a @ref v3fit_context object.
!-------------------------------------------------------------------------------
      SUBROUTINE init_parameters(context)
      USE v3fit_context

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context

!  local variables
      INTEGER                                   :: i, j
      INTEGER, DIMENSION(:,:), ALLOCATABLE      :: indices
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (*,*) ' *** Constructing reconstruction parameters'
      WRITE (context%runlog_iou,*) ' *** Constructing ' //                     &
     &                             'reconstruction parameters'

!  Initialize the reconstruction parameters.
      ALLOCATE(context%params(n_rp))
      DO i = 1, n_rp
         context%params(i)%p => param_construct(context%model,                 &
     &                                          rp_type(i),                    &
     &                                          (/ rp_index(i),                &
     &                                             rp_index2(i) /),            &
     &                                          rp_vrnc(i),                    &
     &                                          rp_range_type(i,:),            &
     &                                          rp_range_index(i,:,:),         &
     &                                          rp_range_value(i,:),           &
     &                                          SIZE(context%signals),         &
     &                                          n_rp)
      END DO

!  Initialize the derived parameters.
      ALLOCATE(context%derived_params(n_dp))
      DO i = 1, n_dp
         context%derived_params(i)%p => param_construct(context%model,         &
     &                                                  dp_type(i),            &
     &                                                  dp_index(i,:),         &
     &                                                  n_dp)

      END DO

!  Initialize the locking parameters.
      ALLOCATE(context%locks(n_lp))

      DO i = 1, n_lp
!  Find the number of parameters to lock to.
         DO j = 1, SIZE(lp_sets,2)
            IF (lp_sets(i,j) .eq. '') THEN
               EXIT
            END IF
         END DO
         j = MIN(j - 1, SIZE(lp_sets,2))

         CALL assert(j .ne. 0, 'Locking parameters require at ' //             &
     &                         'least one set.')

         ALLOCATE(indices(j, data_max_indices))
         indices(:,1) = lp_sets_index(i,1:j)
         indices(:,2) = lp_sets_index2(i,1:j)

         context%locks(i)%p =>                                                 &
     &      param_construct(context%model, lp_type(i),                         &
     &                      (/ lp_index(i), lp_index2(i) /),                   &
     &                      lp_sets(i,1:j), indices,                           &
     &                      lp_sets_coeff(i,1:j),                              &
     &                      context%equilibrium_comm)

         DEALLOCATE(indices)
      END DO

      CALL profiler_set_stop_time('init_parameters', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Constructs the @ref v3fit_context::v3fit_context_class::gp array.
!>
!>  This constructs all reconstruction @ref guassian_process::gaussp_class
!>  objects.
!>
!>  @param[inout] context An instance of a @ref v3fit_context object.
!-------------------------------------------------------------------------------
      SUBROUTINE init_gaussian_process(context)
      USE v3fit_context

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context

!  local variables
      INTEGER                                   :: i
      INTEGER                                   :: j
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(context%gp(n_gp))

      DO i = 1, n_gp
         context%gp(i)%p => gaussp_construct(context%model,                    &
     &                                       n_gp_signal(i),                   &
     &                                       gp_model_type(i),                 &
     &                                       gp_model_index(i),                &
     &                                       gp_param_vrnc(i,:),               &
     &                                       gp_tolerance(i),                  &
     &                                       gp_cholesky_fact(i))

         DO j = 1, n_gp_signal(i)
            CALL gaussp_set_signal(context%gp(i)%p,                            &
     &              context%signals(gp_signal_indices(i,j))%p, j)
         END DO
      END DO

      CALL profiler_set_stop_time('init_gaussian_process', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Initialize Reconstruction
!>
!>  @param[inout] context An instance of a @ref v3fit_context object.
!-------------------------------------------------------------------------------
      SUBROUTINE init_reconstruction(context)
      USE v3fit_context

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context

!  local variables
      INTEGER                                   :: last_para_signal
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  The combination signals cannot be computed in parallel since the depend on
!  other signals previously computed. If combinations are used, the signals need
!  to be split into two loops.
      IF (context%combination_index .eq. -1) THEN
         last_para_signal = SIZE(context%signals)
      ELSE
         last_para_signal = context%combination_index - 1
      END IF

!  Override from command line if set.
      use_central_diff = use_central_diff .or.                                 &
     &   commandline_parser_is_flag_set(context%cl_parser, '-c_diff')

      context%recon_stop = dg2_stop
      context%recon => reconstruction_construct(nrstep,                        &
     &                    SIZE(context%signals),                               &
     &                    SIZE(context%derived_params),                        &
     &                    SIZE(context%params),                                &
     &                    step_type, astep_max, cut_svd, cut_eff,              &
     &                    cut_marg_eff, cut_delta_a, cut_dg2,                  &
     &                    last_para_signal, cut_inv_svd,                       &
     &                    use_central_diff)

      CALL profiler_set_stop_time('init_reconstruction', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Configure Parallelism
!>
!>  When using OpenMP, this sets the maximum number of threads to use. Threads
!>  operate within a process and share memory with the main thread. With in
!>  parallel constructs, it's important to ensure that all code is thread safe
!>  to avoid unexpected behavior and race conditions. The maximum number of
!>  threads is controled using the @fixed_width{-para} command line argument. An
!>  argument of a negative values specifies to use the default behavior.
!>
!>  When using MPI, this sets and configures the processes. A unique directory
!>  needs to be generated for each process to avoid overwriting output files.
!>
!>  The default way is to create and distribute the threads externally. In this
!>  case, the number of processes is fixed. Each process will need to be
!>  reassigned a working directory. One process will be used like a parent
!>  process. The remaining processed will be retasked as child processes.
!>
!>  The second way is to spawn sub processes from a single parent process. In
!>  this case, the parent process will spawn subprocesses to operate the child
!>  task in a sub directory. The number of sub processes can be configured
!>  automatically or manually. @see cl_parsing_sec
!>
!>  @param[inout] context An instance of a @ref v3fit_context object.
!-------------------------------------------------------------------------------
!  When using openmp the !$ allows conditional computation based on if openmp is
!  used or not.
      SUBROUTINE config_parallelism(context)
      USE v3fit_context
      USE file_opts
!$    USE omp_lib
#if defined(MPI_OPT)
      USE data_parameters
#endif

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context

!  local variables
      INTEGER                                   :: num_threads
#if defined(MPI_OPT)
      INTEGER                                   :: error
      INTEGER                                   :: num_recon_processes
      INTEGER                                   :: num_eq_processes
      INTEGER                                   :: i
      INTEGER                                   :: mpi_rank
      INTEGER                                   :: mpi_size
      INTEGER                                   :: eq_rank
      INTEGER                                   :: eq_size
      INTEGER                                   :: recon_rank
      CHARACTER (len=dir_prefix_len)            :: directory_name
      CHARACTER (len=path_length)               :: temp_string
#endif
      REAL (rprec)                              :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      WRITE (*,*) ' *** Configuring parallelism.'

!-- OpenMP ---------------------------------------------------------------------

!  Configure the number of threeds to use.
!$    num_threads = commandline_parser_get_integer(context%cl_parser,          &
!$   &                                             '-para', 1)

!$    IF (num_threads .gt. 0) THEN
!$       CALL OMP_SET_NUM_THREADS(num_threads)
!$    END IF
!$OMP PARALLEL
!$    num_threads = OMP_GET_MAX_THREADS()
!$OMP END PARALLEL
!$    WRITE (*,1000) num_threads

!-- MPI ------------------------------------------------------------------------

#if defined (MPI_OPT)
!  Configure num processes to use for MPI
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, mpi_size, error)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, mpi_rank, error)

!  Initialize the intra communicator to the world communicator. This is done so
!  global_comm has is value even when num_processes is 1 or the processes were
!  created externally.
      context%global_comm = MPI_COMM_WORLD

!  MPI parallelization is only implemented for specific tasks. If the root task
!  is not a reconstruction task stop the child processes.
      SELECT CASE (my_task)

         CASE ('reconstruct', 'reconstruct_a1')
            IF (commandline_parser_is_flag_set(context%cl_parser,              &
     &                                         '-serial')) THEN
               num_recon_processes = 1
               num_eq_processes = mpi_size
            ELSE IF (mpi_size .le. n_rp) THEN
               num_recon_processes = mpi_size
               num_eq_processes = 1
            ELSE
               num_recon_processes = n_rp
               num_eq_processes = mpi_size/n_rp                                &
     &                          + MOD(mpi_size, n_rp)
            END IF

         CASE ('v3post', 'vmec_v3post', 'equilibrium')
            num_recon_processes = 1
            num_eq_processes = mpi_size

         CASE DEFAULT
            num_recon_processes = 1
            IF (mpi_rank .gt. 0) THEN
               STOP 'MPI is only supported for the reconstruct task.'
            END IF

      END SELECT

!  Create sub working directories for the processes. When running in default
!  mode one process can use the main directory. Otherwise create one directory
!  for each sub process and spawn a child process in it. If this point is
!  reached and the size is greater than 1, we can assume that default was used.
      temp_string = commandline_parser_get_string(context%cl_parser,           &
     &                                            '-file')

      IF (mpi_rank .eq. 0) THEN
         DO i = 2, num_recon_processes
!  Create the directory and copy the namelist input files over.
            CALL create_directory(process_dir(i), error)

            IF (.not.is_absolute_path(temp_string)) THEN
               CALL copy_file(temp_string,                                     &
     &                        build_path(process_dir(i), temp_string),         &
     &                        error)
            END IF

!  Copy the equilibrium inputs.
            IF (.not.is_absolute_path(vmec_nli_filename)) THEN
               CALL copy_file(vmec_nli_filename,                               &
     &                        build_path(process_dir(i),                       &
     &                                   vmec_nli_filename), error)
            END IF
            IF (.not.is_absolute_path(vmec_wout_input)) THEN
               CALL copy_file(vmec_wout_input,                                 &
     &                        build_path(process_dir(i),                       &
     &                                   vmec_wout_input), error)
            END IF
            IF (.not.is_absolute_path(vacuum_nli_filename)) THEN
               CALL copy_file(vacuum_nli_filename,                             &
     &                        build_path(process_dir(i),                       &
     &                                   vacuum_nli_filename), error)
            END IF
            IF (.not.is_absolute_path(siesta_nli_filename)) THEN
               CALL copy_file(siesta_nli_filename,                             &
     &                        build_path(process_dir(i),                       &
     &                                   siesta_nli_filename), error)
            END IF
            IF (.not.is_absolute_path(siesta_restart_filename)) THEN
               CALL copy_file(siesta_restart_filename,                         &
     &                        build_path(process_dir(i),                       &
     &                                   siesta_restart_filename),             &
     &                        error)
            END IF
         END DO
      END IF

!  Wait for all processes to reach this point.
      CALL MPI_BARRIER(context%global_comm, error)

!  Partition the equilibrium communicators.
      recon_rank = MOD(mpi_rank, MAX(1, num_recon_processes))
      CALL MPI_COMM_SPLIT(context%global_comm, recon_rank, 0,                  &
     &                    context%equilibrium_comm, error)
      CALL MPI_COMM_RANK(context%equilibrium_comm, eq_rank, error)

!  Take the root process of each equilibrium communicator and partition them
!  from global to the reconstuction communicator.
      IF (eq_rank .gt. 0) THEN
         CALL MPI_COMM_SPLIT(context%global_comm, MPI_UNDEFINED, 0,            &
     &                       context%reconstruction_comm, error)
      ELSE
         CALL MPI_COMM_SPLIT(context%global_comm, 0, recon_rank,               &
     &                       context%reconstruction_comm, error)
      END IF

!  Change the current directory to the reconstruction directory.
      IF (recon_rank .gt. 0) THEN
         CALL change_directory(process_dir(recon_rank + 1), error)
      END IF

      IF (mpi_rank .gt. 0) THEN
         IF (eq_rank .gt. 0) THEN
            my_task = 'child_equilibrium'
         ELSE
            my_task = 'child_recon'
         END IF
      END IF

      WRITE (*,1001) mpi_rank + 1, recon_rank + 1, eq_rank + 1,                &
     &               TRIM(my_task)

      CALL MPI_BARRIER(context%global_comm, error)
#endif

      CALL profiler_set_stop_time('config_parallelism', start_time)

1000  FORMAT('Using ',i4,' threads.')
1001  FORMAT('Configured global process ',i5,', recon process ',i5,            &
     &       ', eq process ',i5,' task ',a)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Cleanup Parallelism
!>
!>  Cleans up resources needed for parallelism.
!>
!>  @param[inout] context An instance of a @ref v3fit_context object.
!-------------------------------------------------------------------------------
      SUBROUTINE cleanup_parallelism(context)
      USE v3fit_context
      USE file_opts
#if defined(MPI_OPT)
      USE data_parameters
#endif

      IMPLICIT NONE

!  Declare Arguments
      TYPE (v3fit_context_class), INTENT(inout) :: context

!  local variables
#if defined(MPI_OPT)
      INTEGER                                   :: error
      INTEGER                                   :: mpi_rank
      INTEGER                                   :: mpi_size
      INTEGER                                   :: recon_size
      INTEGER                                   :: i

!  Start of executable code
      IF (context%reconstruction_comm .eq. MPI_COMM_NULL) THEN
         RETURN
      END IF

      CALL MPI_COMM_RANK(context%global_comm, mpi_rank, error)

      CALL MPI_COMM_SIZE(context%reconstruction_comm, recon_size,              &
     &                   error)
      CALL MPI_BARRIER(context%reconstruction_comm, error)


      IF (mpi_rank .eq. 0) THEN
!  Clean up sub working directories for the processes. Parent directory should
!  be ignored.
         DO i = 2, recon_size
            CALL delete_directory(process_dir(i), error)
         END DO
      END IF
#endif

      END SUBROUTINE

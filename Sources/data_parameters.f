!*******************************************************************************
!>  @file data_parameters.f
!>  @brief Contains module @ref data_parameters.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  This modules contains parameters used by equilibrium models.
!*******************************************************************************
      MODULE data_parameters
      USE file_opts, only: path_length

      IMPLICIT NONE

!*******************************************************************************
!  module parameters
!*******************************************************************************
!>  Maximum length of short data strings.
      INTEGER, PARAMETER           :: data_short_name_length = 30
!>  Maximum length of data strings.
      INTEGER, PARAMETER           :: data_name_length = 80

!>  Default parameter id specifiying no id.
!>  @see model
!>  @see equilibrium
      INTEGER, PARAMETER           :: data_no_id = -1

!>  Max number of parameter indicies.
      INTEGER, PARAMETER           :: data_max_indices = 2

!  MPI task parameters.
!>  Compute the jacobian task.
      INTEGER, PARAMETER           :: mpi_jacobian_task    = 0
!>  Run the equilibrium task.
      INTEGER, PARAMETER           :: mpi_equilibrium_task = 1
!>  Run the mgrid task.
      INTEGER, PARAMETER           :: mpi_mgrid_task       = 2
!>  Run the sync task.
      INTEGER, PARAMETER           :: mpi_sync_task        = 3
!>  Run the step task.
      INTEGER, PARAMETER           :: mpi_step_task        = 4
!>  Quit child process task.
      INTEGER, PARAMETER           :: mpi_quit_task        = 5

!>  Directory prefix name.
      CHARACTER (len=*), PARAMETER :: dir_prefix = 'process_dir'
!>  Directory string length.
      INTEGER, PARAMETER           :: dir_prefix_len = 14

!>  Maximum sxrem profiles. Warning do not change this value before  reading
!>  @ref model_state::model_state_sxrem_flag.
      INTEGER, PARAMETER :: max_sxrem_profiles = 10
!>  Maximum number of Gaussian Process signals. Total number of auxillary
!>  profiles.
      INTEGER, PARAMETER :: max_gaussprocess = 2 + max_sxrem_profiles

      CONTAINS
!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Generates a base filename.
!>
!>  Parses the filename to generate a base name. Looks for a v3fit. and trims
!>  off the remainder.
!>
!>  @param[in] filename The file name of the input file.
!>  @returns The base filename.
!-------------------------------------------------------------------------------
      FUNCTION filename_base(filename)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=path_length)   :: filename_base
      CHARACTER (len=*), INTENT(in) :: filename

!  Start of executable code
      IF ((filename(1:4) .eq. 'v3fi') .and.                                    &
     &    (filename(6:6) .eq. '.')) THEN
         filename_base = TRIM(filename(7:LEN_TRIM(filename)))
      ELSE
         filename_base = TRIM(filename)
      END IF

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Generates a directory name.
!>
!>  Generates a directory name string.
!>
!>  @param[in] rank Index of the directory.
!>  @returns The directory name for a given rank.
!-------------------------------------------------------------------------------
      FUNCTION process_dir(rank)

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=dir_prefix_len) :: process_dir
      INTEGER, INTENT(in)            :: rank

!  Start of executable code
      IF (rank .lt. 10) THEN
         WRITE (process_dir, 1000) dir_prefix, rank
      ELSE
         WRITE (process_dir, 1001) dir_prefix, rank
      END IF

!  Support up to 99 directories. Need to add leading zeros to avoid spaces.
1000  FORMAT(a11,'_0',i1)
1001  FORMAT(a11,'_',i2)

      END FUNCTION

      END MODULE

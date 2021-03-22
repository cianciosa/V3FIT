!*******************************************************************************
!>  @file limiter_grid.f
!>  @brief Contains module @ref limiter
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref limiter_grid_class.
!>  @par Super Class:
!>  @ref geometric
!*******************************************************************************

      MODULE limiter_grid
      USE stel_kinds
      USE mpi_inc
      USE profiler
      USE limiter

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) limiter_grid base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a limiter signal.
!>  @par Super class:
!>  @ref geometric
!-------------------------------------------------------------------------------
      TYPE, EXTENDS(limiter_class) :: limiter_grid_class
!>  Limiter iso grid.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: grid => null()
!>  Limiter iso rgird.
         REAL (rprec), DIMENSION(:), POINTER     :: rgrid => null()
!>  Limiter iso zgrid.
         REAL (rprec), DIMENSION(:), POINTER     :: zgrid => null()
      CONTAINS
         PROCEDURE :: get_max_fval => limiter_grid_get_max_fval
         PROCEDURE :: get_type => limiter_grid_get_type
         FINAL     :: limiter_grid_destruct
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for grid limiter constructor.
!-------------------------------------------------------------------------------
      INTERFACE limiter_grid_class
         MODULE PROCEDURE limiter_grid_construct
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a limiter_grid_class.
!>
!>  Allocates memory and initializes a @ref limiter_grid_class object from a
!>  netcdf file.
!>
!>  @param[in] lgrid_file Filename for the limiter grid netcdf file.
!>  @param[in] on_edge    Specifies if the edge should touch the limiter or
!>                        not. If true, the reconstruction tries to have the
!>                        limiter touch the limiter. If false, the edge will
!>                        only just fall inside the limiter.
!>  @returns A pointer to a constructed @ref limiter_grid_class object.
!-------------------------------------------------------------------------------
      FUNCTION limiter_grid_construct(lgrid_file, on_edge)
      USE ezcdf
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      CLASS (limiter_grid_class), POINTER :: limiter_grid_construct
      CHARACTER (len=*), INTENT(in)       :: lgrid_file
      LOGICAL, INTENT(in)                 :: on_edge

!  local variables
      REAL (rprec)                        :: r0
      REAL (rprec)                        :: dr
      REAL (rprec)                        :: z0
      REAL (rprec)                        :: dz
      INTEGER                             :: lgrid_iou
      INTEGER                             :: varid
      INTEGER                             :: status
      INTEGER, DIMENSION(3)               :: dim_lengths
      INTEGER                             :: i
      REAL (rprec)                        :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(limiter_grid_construct)

      limiter_grid_construct%on_edge = on_edge

      status = nf90_open(TRIM(lgrid_file), NF90_NOWRITE, lgrid_iou)
      CALL assert_eq(0, status, 'limiter_grid_construct: ' //                  &
     &               'failed to open ', TRIM(lgrid_file))

      status = nf90_inq_varid(lgrid_iou, 'r0', varid)
      status = nf90_get_var(lgrid_iou, varid, r0)
      status = nf90_inq_varid(lgrid_iou, 'dr', varid)
      status = nf90_get_var(lgrid_iou, varid, dr)
      status = nf90_inq_varid(lgrid_iou, 'z0', varid)
      status = nf90_get_var(lgrid_iou, varid, z0)
      status = nf90_inq_varid(lgrid_iou, 'dz', varid)
      status = nf90_get_var(lgrid_iou, varid, dz)

      status = nf90_inq_varid(lgrid_iou, 'phi_angles', varid)
      status = nf90_inquire_variable(lgrid_iou, varid,                         &
     &                               dimids=dim_lengths(1:1))
      status = nf90_inquire_dimension(lgrid_iou, dim_lengths(1),               &
     &                                len=dim_lengths(1))
      ALLOCATE(limiter_grid_construct%phi(dim_lengths(1)))
      status = nf90_get_var(lgrid_iou, varid,                                  &
     &                      limiter_grid_construct%phi)

      status = nf90_inq_varid(lgrid_iou, 'iso_grids', varid)
      status = nf90_inquire_variable(lgrid_iou, varid,                         &
     &                               dimids=dim_lengths)
      status = nf90_inquire_dimension(lgrid_iou, dim_lengths(1),               &
     &                                len=dim_lengths(1))
      status = nf90_inquire_dimension(lgrid_iou, dim_lengths(2),               &
     &                                len=dim_lengths(2))
      status = nf90_inquire_dimension(lgrid_iou, dim_lengths(3),               &
     &                                len=dim_lengths(3))
      ALLOCATE(limiter_grid_construct%grid(dim_lengths(1),                     &
     &                                     dim_lengths(2),                     &
     &                                     dim_lengths(3)))
      status = nf90_get_var(lgrid_iou, varid,                                  &
     &                      limiter_grid_construct%grid)

      status = nf90_close(lgrid_iou)

      ALLOCATE(limiter_grid_construct%rgrid(dim_lengths(1)))
      DO i = 1, dim_lengths(1)
         limiter_grid_construct%rgrid(i) = (i - 1)*dr + r0
      END DO
      ALLOCATE(limiter_grid_construct%zgrid(dim_lengths(2)))
      DO i = 1, dim_lengths(2)
         limiter_grid_construct%zgrid(i) = (i - 1)*dz + z0
      END DO

      CALL profiler_set_stop_time('limiter_grid_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref limiter_grid_class object.
!>
!>  Deallocates memory and uninitializes a @ref limiter_grid_class object.
!>
!>  @param[inout] this A @ref limiter_grid_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE limiter_grid_destruct(this)

!  Declare Arguments
      TYPE (limiter_grid_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%phi)) THEN
         DEALLOCATE(this%phi)
         this%phi => null()
      END IF

      IF (ASSOCIATED(this%grid)) THEN
         DEALLOCATE(this%grid)
         this%grid => null()
      END IF

      IF (ASSOCIATED(this%rgrid)) THEN
         DEALLOCATE(this%rgrid)
         this%rgrid => null()
      END IF

      IF (ASSOCIATED(this%zgrid)) THEN
         DEALLOCATE(this%zgrid)
         this%zgrid => null()
      END IF

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Calculates the maximum value of the grid function.
!>
!>  @param[in]  this      A @ref limiter_class instance.
!>  @param[in]  num_theta Number of points in the theta direction.
!>  @param[in]  phi_index Current phi index.
!>  @param[in]  r         R positions of the last closed flux surface.
!>  @param[in]  z         Z positions of the last closed flux surface.
!>  @param[out] rphiz_at_max R, Phi, Z position of the maximum function.
!>  @returns The maximum value of the iso function.
!-------------------------------------------------------------------------------
      FUNCTION limiter_grid_get_max_fval(this, num_theta,  phi_index,          &
     &                                   r, z, rphiz_at_max)
      USE bivariate

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec) :: limiter_grid_get_max_fval
      CLASS (limiter_grid_class), INTENT(in)  :: this
      INTEGER, INTENT(in)                     :: num_theta
      INTEGER, INTENT(in)                     :: phi_index
      REAL (rprec), DIMENSION(:), INTENT(in)  :: r
      REAL (rprec), DIMENSION(:), INTENT(in)  :: z
      REAL (rprec), DIMENSION(3), INTENT(out) :: rphiz_at_max

!  local variables
      CLASS (bivariate_class), POINTER        :: bivariate_obj
      INTEGER                                 :: index
      REAL (rprec), DIMENSION(:), ALLOCATABLE :: fval
      REAL (rprec)                            :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(fval(num_theta))
      bivariate_obj => bivariate_class(1, num_theta)

      CALL bivariate_obj%set_grids(r, z, this%rgrid, this%zgrid)
      CALL bivariate_obj%get_4pt(this%grid(:,:,phi_index), fval)

      index = MAXLOC(fval, 1)
      limiter_grid_get_max_fval = fval(index)
      rphiz_at_max = (/ r(index), this%phi(phi_index),  z(index) /)

      DEALLOCATE(bivariate_obj)
      DEALLOCATE(fval)

      CALL profiler_set_stop_time('limiter_grid_get_max_fval',                 &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Gets a discription of the limiter grid type.
!>
!>  Returns a description of the limiter type for use when writting output files.
!>
!>  @param[in] this A @ref limiter_grid_class instance.
!>  @returns A string describing the limiter type.
!-------------------------------------------------------------------------------
      FUNCTION limiter_grid_get_type(this)
      USE data_parameters

      IMPLICIT NONE

!  Declare Arguments
      CHARACTER (len=data_name_length) :: limiter_grid_get_type
      CLASS (limiter_grid_class), INTENT(in) :: this

!  local variables
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      limiter_grid_get_type =                                                  &
     &   TRIM(this%limiter_class%get_type()) // 'grid'

      CALL profiler_set_stop_time('limiter_grid_get_type',                     &
     &                            start_time)

      END FUNCTION

      END MODULE

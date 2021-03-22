!-------------------------------------------------------------------------------
!  The @header, @table_section, @table_subsection, @item and @end_table commands
!  are custom defined commands in Doxygen.in. They are defined under ALIASES.
!  For the page created here, the 80 column limit is exceeded. Arguments of
!  aliases are separated by ','. If you intended ',' to be a string you must use
!  an escaped comma '\,'.
!
!>  @page emission_file_sec Soft X-Ray Emission File Manual
!>
!>  @tableofcontents
!>  @section emission_file_intro_sec Introduction
!>  This page documents the NetCDF file format for the Soft X-Ray emission file.
!>  This file contains all the information to compute the Soft X-Ray emission
!>  from physical parameters. To allow for mutiple cameras and  multi-color
!>  systems, transmission and absorption functions need to be defined for each
!>  camera.
!>
!>  @section emission_file_contents_sec File Contents
!>  @header{Variable, Description, Dimension}
!>  @begin_table
!>     @item{te_start,   Initial temperature.,          Scalar}
!>     @item{te_step,    Temperature step.,             Scalar}
!>     @item{emissivity, Soft X-Ray emission function., (Number Cameras\, Number Energies)}
!>  @end_table
!>
!*******************************************************************************
!>  @file emission.f
!>  @brief Contains module @ref emission.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref emission_class. This contains the
!>  X-Ray emission as function of temperature and energy. This needs to still be
!>  multiplied by ne^2.
!*******************************************************************************

      MODULE emission
      USE stel_kinds
      USE profiler
      USE mpi_inc

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) emission class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing the soft x-ray emission function.
!-------------------------------------------------------------------------------
      TYPE emission_class
!>  Initial temperature.
         REAL (rprec)                          :: te_start = 0.0
!>  Temperature step.
         REAL (rprec)                          :: te_step = 0.0

!>  X-Ray Emission function.
         REAL (rprec), DIMENSION(:,:), POINTER :: emissivity => null()
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
      INTERFACE emission_construct
         MODULE PROCEDURE emission_construct_netcdf
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref emission_class object.
!>
!>  Allocates memory and initialized a @ref emission_class object from a netcdf
!>  file.
!>
!>  @param[in] filename File path to the emissivity netcdf file.
!>  @returns A pointer to a constructed @ref emission_class object.
!-------------------------------------------------------------------------------
      FUNCTION emission_construct_netcdf(filename)
      USE ezcdf
      USE v3_utilities

      IMPLICIT NONE

!  Declare Arguments
      TYPE (emission_class), POINTER :: emission_construct_netcdf
      CHARACTER (len=*), INTENT(in)  :: filename

!  local variables
      REAL (rprec)                   :: start_time
      INTEGER                        :: iou
      INTEGER                        :: varid
      INTEGER                        :: status
      INTEGER, DIMENSION(2)          :: dim_lengths

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(emission_construct_netcdf)

      status = nf90_open(TRIM(filename), NF90_NOWRITE, iou)
      CALL assert_eq(0, status, 'failed to open emission file')

      status = nf90_inq_varid(iou, 'te_start', varid)
      status = nf90_get_var(iou, varid,                                        &
     &                      emission_construct_netcdf%te_start)
      status = nf90_inq_varid(iou, 'te_step', varid)
      status = nf90_get_var(iou, varid,                                        &
     &                      emission_construct_netcdf%te_step)

!  Start the dimids in the dim_lengths then overwrite them. It is hard coded
!  that emissivity is two dimensional.
      status = nf90_inq_varid(iou, 'emissivity', varid)
      status = nf90_inquire_variable(iou, varid, dimids=dim_lengths)
      status = nf90_inquire_dimension(iou, dim_lengths(1),                     &
     &                                len=dim_lengths(1))
      status = nf90_inquire_dimension(iou, dim_lengths(2),                     &
     &                                len=dim_lengths(2))
      ALLOCATE(emission_construct_netcdf%emissivity(dim_lengths(1),            &
     &                                              dim_lengths(2)))
      status = nf90_get_var(iou, varid,                                        &
     &                      emission_construct_netcdf%emissivity)

      status = nf90_close(iou)

      CALL profiler_set_stop_time('emission_construct_netcdf',                 &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref emission_class object.
!>
!>  Deallocates memory and uninitializes a @ref emission_class object.
!>
!>  @param[inout] this A @ref emission_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE emission_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (emission_class), POINTER :: this

!  Start of executable code
      IF (ASSOCIATED(this%emissivity)) THEN
         DEALLOCATE(this%emissivity)
         this%emissivity => null()
      END IF

      DEALLOCATE(this)

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Gets the emission as a function of energy for a fixed temperature.
!>
!>  The emission for a fixed temperature is interpolated from a the temperature
!>  grid points. The emission array columns are the temperature and rows are the
!>  energy. Transmission and absorption function convert to the signal
!>
!>  @param[in] this  A @ref emission_class instance.
!>  @param[in] te    The electron temperature.
!>  @param[in] ne    The electron density.
!>  @param[in] index Filter index of the camera array.
!>  @returns The emissivity function for a fixed temperature.
!-------------------------------------------------------------------------------
      FUNCTION emission_get_emission(this, te, ne, index)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                      :: emission_get_emission
      TYPE (emission_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)          :: te
      REAL (rprec), INTENT(in)          :: ne
      INTEGER, INTENT(in)               :: index

!  local variables
      INTEGER                           :: i_low, i_high
      REAL (rprec)                      :: w_low, w_high
      INTEGER                           :: num_temp
      REAL (rprec)                      :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

!  Avoid a divid by zero error.
      IF (te .eq. 0.0) THEN
         emission_get_emission = 0.0
         CALL profiler_set_stop_time('emission_get_emission',                  &
     &                               start_time)
         RETURN
      END IF

!  Determine the high and low indices for interpolating. If the temperature is
!  beyond the first or last temperature, linearly interpolate from the last two
!  temperatures.
      num_temp = SIZE(this%emissivity, 2)

      IF (te .lt. this%te_start) THEN
!  Assume no emission at zero eV. Interpolate between zero and the first
!  temperature level.
         i_low = 1
         i_high = 2

         w_high = 0.0
         w_low = te/this%te_start
      ELSE IF (te .gt. this%te_start + (num_temp - 1)*this%te_step) THEN
         i_high = num_temp
         i_low = i_high - 1

         w_high = 1.0
         w_low = 0.0
      ELSE
         i_low = (te - this%te_start)/this%te_step + 1
         i_high = i_low + 1

         w_high = (te - this%te_start)/this%te_step + 1.0 - i_low
         w_low = 1.0 - w_high
      END IF

      emission_get_emission = w_high*this%emissivity(index,i_high)             &
     &                      + w_low*this%emissivity(index,i_low)

      emission_get_emission = ne*ne/SQRT(te)*MAX(emission_get_emission,        &
     &                                           0.0)

      CALL profiler_set_stop_time('emission_get_emission', start_time)

      END FUNCTION

      END MODULE

!*******************************************************************************
!>  @file bivariate.f
!>  @brief Contains module @ref bivariate.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  This modules contains routines for interpolating points inside a grid. This
!>  was originally written by S. P. Hirshman. It has been modified for thread
!>  safety converting it to be object oriented by M. Cianciosa.
!*******************************************************************************
      MODULE bivariate

      USE stel_kinds
      USE profiler

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) bivariate_type
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  An object containing persistent data for the bivariate interpolation.
!-------------------------------------------------------------------------------
      TYPE :: bivariate_class
!>  Number of interpolation points.
         INTEGER                            :: nsu
!>  Number of interpolation points.
         INTEGER                            :: nrz
!>  Lower left interpolation point.
         INTEGER, DIMENSION(:), POINTER     :: irz11_bi => null()
!>  Lower right interpolation point.
         INTEGER, DIMENSION(:), POINTER     :: irz12_bi => null()
!>  Upper left interpolation point.
         INTEGER, DIMENSION(:), POINTER     :: irz21_bi => null()
!>  Upper right interpolation point.
         INTEGER, DIMENSION(:), POINTER     :: irz22_bi => null()
!>  Lower left interpolation weight.
         REAL(rprec), DIMENSION(:), POINTER :: w11_bi => null()
!>  Lower right interpolation weight.
         REAL(rprec), DIMENSION(:), POINTER :: w12_bi => null()
!>  Upper left interpolation weight.
         REAL(rprec), DIMENSION(:), POINTER :: w21_bi => null()
!>  Upper right interpolation weight.
         REAL(rprec), DIMENSION(:), POINTER :: w22_bi => null()
      CONTAINS
         PROCEDURE :: set_grids_1d => bivariate_set_grids_1d
         PROCEDURE :: set_grids_2d => bivariate_set_grids_2d
         GENERIC   :: set_grids => set_grids_1d, set_grids_2d
         PROCEDURE :: get_4pt => bivariate_get_4pt
         FINAL     :: bivariate_destruct
      END TYPE

*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for bivariate constructor.
!-------------------------------------------------------------------------------
      INTERFACE bivariate_class
         MODULE PROCEDURE bivariate_construct
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref bivariate_type object.
!>
!>  Allocates memory and initializes a @ref bivariate_type object.
!>
!>  @param[in] ns Number of radial grid points.
!>  @param[in] nu Number of poloidal grid points.
!>  @returns A pointer to a constructed @ref bivariate_type object.
!-------------------------------------------------------------------------------
      FUNCTION bivariate_construct(ns, nu)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bivariate_class), POINTER :: bivariate_construct
      INTEGER, INTENT(in)              :: ns
      INTEGER, INTENT(in)              :: nu

!  local variables
      REAL (rprec)                     :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(bivariate_construct)

      bivariate_construct%nsu = ns*nu

      ALLOCATE (bivariate_construct%irz11_bi(bivariate_construct%nsu))
      ALLOCATE (bivariate_construct%irz12_bi(bivariate_construct%nsu))
      ALLOCATE (bivariate_construct%irz21_bi(bivariate_construct%nsu))
      ALLOCATE (bivariate_construct%irz22_bi(bivariate_construct%nsu))
      ALLOCATE (bivariate_construct%w11_bi(bivariate_construct%nsu))
      ALLOCATE (bivariate_construct%w12_bi(bivariate_construct%nsu))
      ALLOCATE (bivariate_construct%w21_bi(bivariate_construct%nsu))
      ALLOCATE (bivariate_construct%w22_bi(bivariate_construct%nsu))

      CALL profiler_set_stop_time('bivariate_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref bivariate_type object.
!>
!>  Deallocates memory and uninitializes a @ref bivariate_type object.
!>
!>  @param[inout] this A @ref bivariate_type instance.
!-------------------------------------------------------------------------------
      SUBROUTINE bivariate_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (bivariate_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%irz11_bi)) THEN
         DEALLOCATE (this%irz11_bi)
         this%irz11_bi => null()
      END IF

      IF (ASSOCIATED(this%irz12_bi)) THEN
         DEALLOCATE (this%irz12_bi)
         this%irz12_bi => null()
      END IF

      IF (ASSOCIATED(this%irz21_bi)) THEN
         DEALLOCATE (this%irz21_bi)
         this%irz21_bi => null()
      END IF

      IF (ASSOCIATED(this%irz22_bi)) THEN
         DEALLOCATE (this%irz22_bi)
         this%irz22_bi => null()
      END IF

      IF (ASSOCIATED(this%w11_bi)) THEN
         DEALLOCATE (this%w11_bi)
         this%w11_bi => null()
      END IF

      IF (ASSOCIATED(this%w12_bi)) THEN
         DEALLOCATE (this%w12_bi)
         this%w12_bi => null()
      END IF

      IF (ASSOCIATED(this%w21_bi)) THEN
         DEALLOCATE (this%w21_bi)
         this%w21_bi => null()
      END IF

      IF (ASSOCIATED(this%w22_bi)) THEN
         DEALLOCATE (this%w22_bi)
         this%w22_bi => null()
      END IF

      END SUBROUTINE

!*******************************************************************************
!  SETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Set up the interpolation grid for a phi plane.
!>
!>  Compute the indices (ir_bi, jz_bi) and weight factors (wij_bi) for
!>  performing bivariate (4 pt) interpolation FROM a rectangular (R X Z) grid TO
!>  a general (non-orthogonal) grid (s, u). R and Z grids may have different
!>  mesh sizes.
!>
!>  @param[inout] this  A @ref bivariate_type instance.
!>  @param[in]    rsu   r coordinate at s,u
!>  @param[in]    zsu   z coordinate at s,u
!>  @param[in]    rgrid R coordinate on a fixed, equally spaced grid
!>  @param[in]    zgrid Z coordinate on a fixed, equally spaced grid
!-------------------------------------------------------------------------------
      SUBROUTINE bivariate_set_grids_2d(this, rsu, zsu, rgrid, zgrid)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bivariate_class), INTENT(inout)   :: this
      REAL (rprec), DIMENSION(:,:), INTENT(in) :: rsu
      REAL (rprec), DIMENSION(:,:), INTENT(in) :: zsu
      REAL (rprec), DIMENSION(:), INTENT(in)   :: rgrid
      REAL (rprec), DIMENSION(:), INTENT(in)   :: zgrid

!  local variables
      INTEGER                                  :: ku
      INTEGER                                  :: js
      INTEGER                                  :: ns
      INTEGER                                  :: nu
      INTEGER                                  :: nr
      INTEGER                                  :: nz
      INTEGER                                  :: index1d
      INTEGER                                  :: ir
      INTEGER                                  :: jz
      INTEGER                                  :: ir1
      INTEGER                                  :: jz1
      REAL (rprec)                             :: rad0
      REAL (rprec)                             :: zee0
      REAL (rprec)                             :: ri
      REAL (rprec)                             :: zj
      REAL (rprec)                             :: pr
      REAL (rprec)                             :: qz
      REAL (rprec)                             :: temp
      REAL (rprec)                             :: delr
      REAL (rprec)                             :: delz
      REAL (rprec)                             :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      nr = SIZE(rgrid, 1)
      nz = SIZE(zgrid, 1)
      this%nrz = nr*nz
      ns = SIZE(rsu, 1)
      nu = SIZE(rsu, 2)

      delr = rgrid(2) - rgrid(1)
      delz = zgrid(2) - zgrid(1)

      index1d = 0

      DO ku = 1, nu
         DO js = 1, ns

            index1d = index1d + 1

!  Check that boundary points are inside grid. Stop if not.
            rad0 = rsu(js, ku)
            zee0 = zsu(js, ku)

            IF (rad0.lt.rgrid(1) .or. rad0.gt.rgrid(nr) .or.                   &
     &          zee0.lt.zgrid(1) .or. zee0.gt.zgrid(nz)) THEN
               STOP 'Plasma point outside response function grid!'
            END IF

!  Detremine integer indices (ir, jz) for the lower left R, Z corner grid point.
            ir = INT((rad0 - rgrid(1))/delr) + 1
            jz = INT((zee0 - zgrid(1))/delz) + 1
            ir1 = MIN(nr, ir + 1)
            jz1 = MIN(nz, jz + 1)

!  Store indices in 1d attays.
            this%irz11_bi(index1d) = ir  + nr*(jz - 1)
            this%irz22_bi(index1d) = ir1 + nr*(jz1 - 1)
            this%irz12_bi(index1d) = ir  + nr*(jz1 - 1)
            this%irz21_bi(index1d) = ir1 + nr*(jz - 1)

!  Compute (ri, zj) and (pr, qz) at grid point (ir, jz) also compute weights
!  for the 4 corner grid points.
            ri = rgrid(ir)
            zj = zgrid(jz)
            pr = (rad0 - ri)/delr
            qz = (zee0 - zj)/delz
            temp = pr*qz
            this%w22_bi(index1d) = temp                   !p*q
            this%w21_bi(index1d) = pr - temp              !p*(1-q)
            this%w12_bi(index1d) = qz - temp              !q*(1-p)
            this%w11_bi(index1d) = 1 + temp - (pr + qz)   !(1-p)*(1-q)

         END DO
      END DO

      CALL profiler_set_stop_time('bivariate_set_grids_2d', start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Set up the interpolation grid for a phi plane.
!>
!>  Compute the indices (ir_bi, jz_bi) and weight factors (wij_bi) for
!>  performing bivariate (4 pt) interpolation FROM a rectangular (R X Z) grid TO
!>  a general (non-orthogonal) grid (s, u). R and Z grids may have different
!>  mesh sizes.
!>
!>  @param[inout] this  A @ref bivariate_type instance.
!>  @param[in]    rsu   r coordinate at s,u
!>  @param[in]    zsu   z coordinate at s,u
!>  @param[in]    rgrid R coordinate on a fixed, equally spaced grid
!>  @param[in]    zgrid Z coordinate on a fixed, equally spaced grid
!-------------------------------------------------------------------------------
      SUBROUTINE bivariate_set_grids_1d(this, rsu, zsu, rgrid, zgrid)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bivariate_class), INTENT(inout) :: this
      REAL (rprec), DIMENSION(:), INTENT(in) :: rsu
      REAL (rprec), DIMENSION(:), INTENT(in) :: zsu
      REAL (rprec), DIMENSION(:), INTENT(in) :: rgrid
      REAL (rprec), DIMENSION(:), INTENT(in) :: zgrid

!  local variables
      INTEGER                                :: ku
      INTEGER                                :: ns
      INTEGER                                :: nu
      INTEGER                                :: nr
      INTEGER                                :: nz
      INTEGER                                :: index1d
      INTEGER                                :: ir
      INTEGER                                :: jz
      INTEGER                                :: ir1
      INTEGER                                :: jz1
      REAL (rprec)                           :: rad0
      REAL (rprec)                           :: zee0
      REAL (rprec)                           :: ri
      REAL (rprec)                           :: zj
      REAL (rprec)                           :: pr
      REAL (rprec)                           :: qz
      REAL (rprec)                           :: temp
      REAL (rprec)                           :: delr
      REAL (rprec)                           :: delz
      REAL (rprec)                           :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      nr = SIZE(rgrid, 1)
      nz = SIZE(zgrid, 1)
      this%nrz = nr*nz
      ns = 1
      nu = SIZE(rsu)

      delr = rgrid(2) - rgrid(1)
      delz = zgrid(2) - zgrid(1)

      index1d = 0

      DO ku = 1, nu
         index1d = index1d + 1
!
!       CHECK THAT BOUNDARY POINTS ARE INSIDE GRID.  IF NOT, STOP!
!
         rad0 = rsu(ku)
         zee0 = zsu(ku)

         IF (rad0.lt.rgrid(1) .or. rad0.gt.rgrid(nr) .or.                      &
     &       zee0.lt.zgrid(1) .or. zee0.gt.zgrid(nz)) THEN
            STOP 'Plasma point outside response function grid!'
         END IF
!
!       DETERMINE INTEGER INDICES (IR,JZ) FOR LOWER LEFT R, Z CORNER GRID POINT
!
         ir = INT((rad0 - rgrid(1))/delr) + 1
         jz = INT((zee0 - zgrid(1))/delz) + 1
         ir1 = MIN(nr, ir + 1)
         jz1 = MIN(nz, jz + 1)

!
!        STORE INDICES IN 1D ARRAYS
!
         this%irz11_bi(index1d) = ir  + nr*(jz - 1)
         this%irz22_bi(index1d) = ir1 + nr*(jz1 - 1)
         this%irz12_bi(index1d) = ir  + nr*(jz1 - 1)
         this%irz21_bi(index1d) = ir1 + nr*(jz - 1)
!
!       COMPUTE RI, ZJ AND PR , QZ AT GRID POINT (IR , JZ)
!       ALSO, COMPUTE WEIGHTS WIJ FOR 4 CORNER GRID POINTS
!
         ri = rgrid(ir)
         zj = zgrid(jz)
         pr = (rad0 - ri)/delr
         qz = (zee0 - zj)/delz
         temp = pr*qz
         this%w22_bi(index1d) = temp                   !p*q
         this%w21_bi(index1d) = pr - temp              !p*(1-q)
         this%w12_bi(index1d) = qz - temp              !q*(1-p)
         this%w11_bi(index1d) = 1 + temp - (pr + qz)   !(1-p)*(1-q)
      END DO

      CALL profiler_set_stop_time('bivariate_set_grids_2d', start_time)

      END SUBROUTINE

!*******************************************************************************
!  GETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Interpolate points on to responce function grid.
!>
!>  Determin response function on the s,u grid by 4 point bivariate
!>  interpolation of response function on the r,z mesh at a fixed toroidal
!>  plane. This uses a 2-D interpolation based on the four point formula in
!>  Abramowitz and Stegun, Eq. 25.2.66. Note that the arrays need an explicit
!>  size in dimension to allow the 2D arrays to be passed to the 1D arguments.
!>
!>  @param[inout] this    A @ref bivariate_type instance.
!>  @param[in]    resp_rz Response function on the rz grid.
!>  @param[out]   resp_su Response function on the su grid.
!-------------------------------------------------------------------------------
      SUBROUTINE bivariate_get_4pt(this, resp_rz, resp_su)
      IMPLICIT NONE
      CLASS (bivariate_class), INTENT(inout)         :: this
      REAL (rprec), DIMENSION(this%nrz), INTENT(in)  :: resp_rz
      REAL (rprec), DIMENSION(this%nsu), INTENT(out) :: resp_su

!  local variables
      INTEGER                                        :: jsu
      REAL (rprec)                                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      DO jsu = 1, this%nsu

!  Compute repsonse at R, Phi (fixed kin), Z by 4 point interpolation.
         resp_su(jsu) = this%w11_bi(jsu)*resp_rz(this%irz11_bi(jsu))           &
     &                + this%w22_bi(jsu)*resp_rz(this%irz22_bi(jsu))           &
     &                + this%w21_bi(jsu)*resp_rz(this%irz21_bi(jsu))           &
     &                + this%w12_bi(jsu)*resp_rz(this%irz12_bi(jsu))

      END DO

      CALL profiler_set_stop_time('bivariate_get_4pt', start_time)

      END SUBROUTINE

      END MODULE

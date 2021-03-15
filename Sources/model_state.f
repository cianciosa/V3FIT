!*******************************************************************************
!>  @file model_state.f
!>  @brief Contains module @ref model_state.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Contains parameters defining the bit positions for flags that mark changes
!>  in different parts of the model.
!*******************************************************************************
      MODULE model_state
      USE data_parameters, ONLY: max_gaussprocess

      IMPLICIT NONE

!*******************************************************************************
!  module parameters
!*******************************************************************************
!>  Set all flags off.
      INTEGER, PARAMETER :: model_state_all_off = 0
!>  Set all flags on.
      INTEGER, PARAMETER ::                                                    &
     &   model_state_all_on = NOT(model_state_all_off)

!>  VMEC Equilibrium changed bit position.
      INTEGER, PARAMETER :: model_state_vmec_flag   = 0
!>  SIESTA Equilibrium changed bit position.
      INTEGER, PARAMETER :: model_state_siesta_flag = 1
!>  Denisty profile changed bit position.
      INTEGER, PARAMETER :: model_state_ne_flag     = 2
!>  Temperature profile changed bit position.
      INTEGER, PARAMETER :: model_state_te_flag     = 3
!>  Ion profile changed bit position.
      INTEGER, PARAMETER :: model_state_ti_flag     = 4
!>  Shift parameter changed bit position.
      INTEGER, PARAMETER :: model_state_shift_flag  = 5
!>  Effective charge profile changed bit position.
      INTEGER, PARAMETER :: model_state_ze_flag     = 6
!>  Model state factor changed bit position.
      INTEGER, PARAMETER :: model_state_signal_flag = 7
!>  Soft x-ray emissivity profile changed bit position. This flag needs to
!>  always be the last flag an can be no larger than 31 minus the maximum number
!>  of possible sxrem profiles @ref data_parameters::max_sxrem_profiles. If the
!>  number of model states exceeds 31, then the size of
!>  @ref model::model_class%state_flags needs to be increased.
      INTEGER, PARAMETER :: model_state_sxrem_flag  = 8

      END MODULE

MODULE sac_data_mod
  ! This module provides global access only to the auxiliary/summation arrays that
  ! are NOT passed as explicit arguments in the calling chain (EXSAC -> SAC1 -> FROST/FGFR).
  IMPLICIT NONE
  
  PRIVATE
  
  ! Define Double Precision (dp) kind parameter
  INTEGER, PARAMETER, PUBLIC :: dp = SELECTED_REAL_KIND(15, 307)
  
  ! ------------------------------------------------------------------
  ! FPMFG1, FSMCO1, FSUMS1 Declarations (REMAIN GLOBAL)
  ! ------------------------------------------------------------------
  
  ! FPMFG1, FSMCO1 Arrays and Scalars
  PUBLIC :: FGPM, FGCO, RSUM, PPE, PSC, PTA, PWE
  DOUBLE PRECISION, DIMENSION(10), SAVE :: FGPM
  DOUBLE PRECISION, DIMENSION(6), SAVE :: FGCO
  DOUBLE PRECISION, DIMENSION(7), SAVE :: RSUM
  DOUBLE PRECISION, SAVE :: PPE, PSC, PTA, PWE
  
  ! FSUMS1 Summation Variables
  PUBLIC :: SROT, SIMPVT, SRODT, SROST, SINTFT, SGWFP, SGWFS, SRECHT
  PUBLIC :: SETT, SE1, SE2, SE3, SE4, SE5
  DOUBLE PRECISION, SAVE :: SROT, SIMPVT, SRODT, SROST, SINTFT, SGWFP, SGWFS, SRECHT
  DOUBLE PRECISION, SAVE :: SETT, SE1, SE2, SE3, SE4, SE5
  
  ! ------------------------------------------------------------------
  ! SACPARM1, SACSTAT1 Variables (EXCLUDED)
  ! ------------------------------------------------------------------
  
  ! The following variables (UZTWC, LZTWM, etc.) are NOT defined here 
  ! because they are passed as explicit arguments through the entire call stack 
  ! (EXSAC -> SAC1 -> FROST/FGFR).
  ! If they were declared here, they would cause an 'ambiguous reference' error.
  
END MODULE sac_data_mod

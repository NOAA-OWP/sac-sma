SUBROUTINE SAC1(DT, PXV, EP, TCI, ROIMP, SDRO, SSUR, SIF, BFS, BFP, TET, BFNCC, &
                IFRZE, TA, LWE, WE, ISC, AESC, &
                UZTWM, UZFWM, UZK, PCTIM, ADIMP, RIVA, ZPERC, &
                REXP, LZTWM, LZFSM, LZFPM, LZSK, LZPK, PFREE, SIDE, RSERV, &
                UZTWC, UZFWC, LZTWC, LZFSC, LZFPC, ADIMC)
  
  ! Use the module ONLY for global/summation arrays (FGCO, RSUM, FSUMS1, etc.)
  USE sac_data_mod, ONLY: dp, FGPM, FGCO, RSUM, PPE, PSC, PTA, PWE, &
                          SROT, SIMPVT, SRODT, SROST, SINTFT, SGWFP, SGWFS, SRECHT, &
                          SETT, SE1, SE2, SE3, SE4, SE5
  
IMPLICIT NONE
  
  ! ----- DUMMY ARGUMENTS (Passed In/Out) -----
  DOUBLE PRECISION, INTENT(IN)    :: DT, PXV, EP
  DOUBLE PRECISION, INTENT(IN)    :: TA, LWE, WE, AESC
  INTEGER, INTENT(IN)     :: IFRZE, ISC
  DOUBLE PRECISION, INTENT(INOUT) :: TCI, ROIMP, SDRO, SSUR, SIF, BFS, BFP, TET, BFNCC

  ! SAC Parameters (IN)
  DOUBLE PRECISION, INTENT(IN)    :: UZTWM, UZFWM, UZK, PCTIM, ADIMP, RIVA, ZPERC, &
                             REXP, LZTWM, LZFSM, LZFPM, LZSK, LZPK, PFREE, &
                             SIDE, RSERV
                             
  ! SAC State Variables (INOUT - Their values change here)
  DOUBLE PRECISION, INTENT(INOUT) :: UZTWC, UZFWC, LZTWC, LZFSC, LZFPC, ADIMC
  
  ! ----- LOCAL VARIABLES (All converted to DOUBLE PRECISION) -----
  DOUBLE PRECISION :: EDMND, E1, RED, E2, UZRAT, E3, RATLZT, SAVED, RATLZ, DEL, E5
  DOUBLE PRECISION :: TWX, SPERC, DINC, PINC, DUZ, DLZP, DLZS, PAREA, ADSUR, RATIO, ADDRO
  DOUBLE PRECISION :: BF, SBF, SPBF, PERCM, PERC, DEFR, FR, FI, UZDEFR, CHECK, PERCT, PERCF
  DOUBLE PRECISION :: HPL, RATLP, RATLS, FRACP, PERCP, PERCS, EXCESS, SUR, EUSED, TBF, BFCC, E4
  DOUBLE PRECISION :: FRACP_DENOM
  INTEGER  :: I, NINC
  LOGICAL  :: bypass_ratio_check = .FALSE. ! <-- NEW FLAG TO FIX GOTO equivalents

  ! Check ADIMC >= UZTWC
  IF (ADIMC .LT. UZTWC) ADIMC = UZTWC

  ! -------------------------------------------------------------------
  ! START ET CALCULATION
  ! -------------------------------------------------------------------
  EDMND = EP
  E1 = EDMND * (UZTWC / UZTWM)
  RED = EDMND - E1
  UZTWC = UZTWC - E1
  E2 = 0.0_dp
  
  IF (UZTWC .LT. 0.0_dp) THEN
    E1 = E1 + UZTWC
    UZTWC = 0.0_dp
    RED = EDMND - E1
    
    ! Block to handle E2 from UZFWC (Original IF(UZFWC.GE.RED) GO TO 221)
    IF (UZFWC .GE. RED) THEN
      ! Original Label 221 logic
      E2 = RED
      UZFWC = UZFWC - E2
      RED = 0.0_dp
      ! Code falls through to 220 comparison (we don't set bypass)
    ELSE
      ! Original logic (UZFWC < RED)
      E2 = UZFWC
      UZFWC = 0.0_dp
      RED = RED - E2
      bypass_ratio_check = .TRUE. ! <-- SET FLAG to emulate GO TO 225
    END IF
  END IF
  
  ! --- FIX FOR LOGICAL BYPASS (Original Label 220) ---
  IF (.NOT. bypass_ratio_check) THEN
    ! This block is only executed if we did NOT hit the UZFWC < RED (bypass) condition
    IF ((UZTWC / UZTWM) .LT. (UZFWC / UZFWM)) THEN
      UZRAT = (UZTWC + UZFWC) / (UZTWM + UZFWM)
      UZTWC = UZTWM * UZRAT
      UZFWC = UZFWM * UZRAT
    END IF
  END IF
  ! --- END LOGIC FIX ---

  ! Compute ET from the lower zone (Original Label 225 onwards)
  E3 = RED * (LZTWC / (UZTWM + LZTWM))
  LZTWC = LZTWC - E3
  
  IF (LZTWC .LT. 0.0_dp) THEN
    E3 = E3 + LZTWC
    LZTWC = 0.0_dp
  END IF
  
  RATLZT = LZTWC / LZTWM
  SAVED = RSERV * (LZFPM + LZFSM)
  RATLZ = (LZTWC + LZFPC + LZFSC - SAVED) / (LZTWM + LZFPM + LZFSM - SAVED)

  IF (RATLZT .LT. RATLZ) THEN
    DEL = (RATLZ - RATLZT) * LZTWM
    LZTWC = LZTWC + DEL
    LZFSC = LZFSC - DEL
    
    IF (LZFSC .LT. 0.0_dp) THEN
      LZFPC = LZFPC + LZFSC
      LZFSC = 0.0_dp
    END IF
  END IF
  
  E5 = E1 + (RED + E2) * ((ADIMC - E1 - UZTWC) / (UZTWM + LZTWM))
  ADIMC = ADIMC - E5
  
  IF (ADIMC .LT. 0.0_dp) THEN
    E5 = E5 + ADIMC
    ADIMC = 0.0_dp
  END IF
  E5 = E5 * ADIMP
  
  ! -------------------------------------------------------------------
  ! COMPUTE PERCOLATION AND RUNOFF AMOUNTS.
  ! -------------------------------------------------------------------
  TWX = PXV + UZTWC - UZTWM
  
  IF (TWX .LT. 0.0_dp) THEN
    UZTWC = UZTWC + PXV
    TWX = 0.0_dp
  ELSE
    UZTWC = UZTWM
  END IF
  
  ADIMC = ADIMC + PXV - TWX
  ROIMP = PXV * PCTIM
  SIMPVT = SIMPVT + ROIMP
  
  SBF=0.0_dp; SSUR=0.0_dp; SIF=0.0_dp; SPERC=0.0_dp; SDRO=0.0_dp; SPBF=0.0_dp

  NINC = INT(1.0_dp + 0.2_dp * (UZFWC + TWX))
  IF (NINC .LT. 1) NINC = 1
  
  DINC = (1.0_dp / REAL(NINC, dp)) * DT
  PINC = TWX / REAL(NINC, dp)
  
  DUZ = 1.0_dp - ((1.0_dp - UZK) ** DINC)
  DLZP = 1.0_dp - ((1.0_dp - LZPK) ** DINC)
  DLZS = 1.0_dp - ((1.0_dp - LZSK) ** DINC)
  PAREA = 1.0_dp - ADIMP - PCTIM
  
  ! -------------------------------------------------------------------
  ! START INCREMENTAL LOOP
  ! -------------------------------------------------------------------
  DO I = 1, NINC
    
    ADSUR = 0.0_dp
    RATIO = (ADIMC - UZTWC) / LZTWM
    IF (RATIO .LT. 0.0_dp) RATIO = 0.0_dp
    ADDRO = PINC * (RATIO ** 2)
    
    ! Baseflow (BF)
    BF = LZFPC * DLZP
    LZFPC = LZFPC - BF
    IF (LZFPC .LT. 0.0_dp) LZFPC = 0.0_dp ! <-- ADDED ZERO CLAMP
    IF (LZFPC .LT. 0.0001_dp) THEN
      BF = BF + LZFPC
      LZFPC = 0.0_dp
    END IF
    SBF = SBF + BF
    SPBF = SPBF + BF
    
    BF = LZFSC * DLZS
    LZFSC = LZFSC - BF
    IF (LZFSC .LT. 0.0_dp) LZFSC = 0.0_dp ! <-- ADDED ZERO CLAMP
    IF (LZFSC .LT. 0.0001_dp) THEN
      BF = BF + LZFSC
      LZFSC = 0.0_dp
    END IF
    SBF = SBF + BF
    
    ! Percolation (PERC)
    IF ((PINC + UZFWC) .LT. 0.01_dp) THEN
      UZFWC = UZFWC + PINC
    ELSE
      PERCM = LZFPM * DLZP + LZFSM * DLZS
      PERC = PERCM * (UZFWC / UZFWM)
      DEFR = 1.0_dp - ((LZTWC + LZFPC + LZFSC) / (LZTWM + LZFPM + LZFSM))
      
      FR = 1.0_dp
      FI = 1.0_dp
      
      ! Frozen Ground Adjustment
      IF (IFRZE .NE. 0) THEN
        UZDEFR = 1.0_dp - ((UZTWC + UZFWC) / (UZTWM + UZFWM))
        CALL FGFR1(DEFR, FR, FI, LZTWC, LZFSC, LZFPC, LZTWM, LZFPM, LZFSM)
      END IF
      
      PERC = PERC * (1.0_dp + ZPERC * (DEFR ** REXP)) * FR
      
      IF (PERC .GE. UZFWC) THEN
        PERC = UZFWC
      END IF 
      
      UZFWC = UZFWC - PERC
      
      CHECK = LZTWC + LZFPC + LZFSC + PERC - LZTWM - LZFPM - LZFSM
      IF (CHECK .GT. 0.0_dp) THEN
        PERC = PERC - CHECK
        UZFWC = UZFWC + CHECK
      END IF
      
      SPERC = SPERC + PERC
      
      ! Interflow (DEL)
      DEL = UZFWC * DUZ * FI
      SIF = SIF + DEL
      UZFWC = UZFWC - DEL
      
      ! Distribute Percolation to LZ
      PERCT = PERC * (1.0_dp - PFREE)
      
      IF ((PERCT + LZTWC) .GT. LZTWM) THEN
        PERCF = PERCT + LZTWC - LZTWM
        LZTWC = LZTWM
      ELSE
        LZTWC = LZTWC + PERCT
        PERCF = 0.0_dp
      END IF 
      
      PERCF = PERCF + PERC * PFREE
      
      IF (PERCF .NE. 0.0_dp) THEN
        ! --- NAN FIX: Added checks for zero capacity ---
        IF (LZFPM .LT. TINY(LZFPM) .OR. LZFSM .LT. TINY(LZFSM)) THEN
           ! If capacity is near zero, use a safe default or simpler calculation.
           ! Assuming both are tiny, this entire PERCF distribution is safely skipped,
           ! or the fraction is simply 1.0 (all to LZFPM if LZFPM is non-zero).
           HPL = LZFPM / (LZFPM + LZFSM)
           IF (ABS(LZFPM + LZFSM) .LT. TINY(LZFPM + LZFSM)) THEN
               HPL = 0.5_dp ! Default to 50/50 if total capacity is zero
           END IF
           
           RATLP = LZFPC / LZFPM
           RATLS = LZFSC / LZFSM
           
           IF (LZFPM .LT. TINY(LZFPM)) RATLP = 0.0_dp
           IF (LZFSM .LT. TINY(LZFSM)) RATLS = 0.0_dp
           
        ELSE
           HPL = LZFPM / (LZFPM + LZFSM)
           RATLP = LZFPC / LZFPM
           RATLS = LZFSC / LZFSM
        END IF
        
        FRACP_DENOM = (1.0_dp - RATLP) + (1.0_dp - RATLS)
        
        IF (ABS(FRACP_DENOM) .LT. TINY(FRACP_DENOM)) THEN
           FRACP = 1.0_dp 
        ELSE
           FRACP = (HPL * 2.0_dp * (1.0_dp - RATLP)) / FRACP_DENOM
        END IF
        ! --- END NAN FIX ---

        IF (FRACP .GT. 1.0_dp) FRACP = 1.0_dp
        
        PERCP = PERCF * FRACP
        PERCS = PERCF - PERCP
        
        LZFSC = LZFSC + PERCS
        IF (LZFSC .GT. LZFSM) THEN
          PERCS = PERCS - LZFSC + LZFSM
          LZFSC = LZFSM
        END IF 
        IF (LZFSC .LT. 0.0_dp) LZFSC = 0.0_dp ! Final Zero Clamp
        
        LZFPC = LZFPC + (PERCF - PERCS)
        
        IF (LZFPC .GT. LZFPM) THEN
          EXCESS = LZFPC - LZFPM
          LZTWC = LZTWC + EXCESS
          LZFPC = LZFPM
        END IF
        IF (LZFPC .LT. 0.0_dp) LZFPC = 0.0_dp ! Final Zero Clamp
        
      END IF
      
      ! Distribute PINC (Available Moisture)
      IF (PINC .NE. 0.0_dp) THEN
        
        IF ((PINC + UZFWC) .GT. UZFWM) THEN
          SUR = PINC + UZFWC - UZFWM
          UZFWC = UZFWM
          SSUR = SSUR + SUR * PAREA
          ADSUR = SUR * (1.0_dp - ADDRO / PINC)
          SSUR = SSUR + ADSUR * ADIMP
        ELSE
          UZFWC = UZFWC + PINC
        END IF
      END IF
      
      ! ADIMP Area Water Balance
      ADIMC = ADIMC + PINC - ADDRO - ADSUR
      
      IF (ADIMC .GT. (UZTWM + LZTWM)) THEN
        ADDRO = ADDRO + ADIMC - (UZTWM + LZTWM)
        ADIMC = UZTWM + LZTWM
      END IF 
      
      SDRO = SDRO + ADDRO * ADIMP
      
    END IF
    
  END DO
  ! -------------------------------------------------------------------

  ! Compute Sums and Outputs
  EUSED = E1 + E2 + E3
  SIF = SIF * PAREA

  TBF = SBF * PAREA
  BFCC = TBF * (1.0_dp / (1.0_dp + SIDE))
  BFP = SPBF * PAREA / (1.0_dp + SIDE)
  BFS = BFCC - BFP
  IF (BFS .LT. 0.0_dp) BFS = 0.0_dp
  BFNCC = TBF - BFCC
  
  ! FSUMS1 Updates
  SINTFT = SINTFT + SIF
  SGWFP = SGWFP + BFP
  SGWFS = SGWFS + BFS
  SRECHT = SRECHT + BFNCC
  SROST = SROST + SSUR
  SRODT = SRODT + SDRO
  
  TCI = ROIMP + SDRO + SSUR + SIF + BFCC
  
  E4 = (EDMND - EUSED) * RIVA
  
  TCI = TCI - E4
  IF (TCI .LT. 0.0_dp) THEN
    E4 = E4 + TCI
    TCI = 0.0_dp
  END IF
  SROT = SROT + TCI
  
  EUSED = EUSED * PAREA
  TET = EUSED + E5 + E4
  SETT = SETT + TET
  SE1 = SE1 + E1 * PAREA
  SE3 = SE3 + E3 * PAREA
  SE4 = SE4 + E4
  SE5 = SE5 + E5

  IF (ADIMC .LT. UZTWC) ADIMC = UZTWC 

  ! Call FROST1 Subroutine
  IF (IFRZE .GT. 0) CALL FROST1(PXV, SSUR, SDRO, TA, LWE, WE, ISC, AESC, DT, &
                               UZTWM, UZFWM, LZTWM, LZFSM, LZFPM, LZSK, LZPK, &
                               UZTWC, UZFWC, LZTWC, LZFSC, LZFPC)
  
  ! Update RSUM
  RSUM(1) = RSUM(1) + TCI
  RSUM(2) = RSUM(2) + ROIMP
  RSUM(3) = RSUM(3) + SDRO
  RSUM(4) = RSUM(4) + SSUR
  RSUM(5) = RSUM(5) + SIF
  RSUM(6) = RSUM(6) + BFS
  RSUM(7) = RSUM(7) + BFP
  
END SUBROUTINE SAC1



! ====================================================================
! SUBROUTINE FGFR1 - FROZEN GROUND ADJUSTMENTS
! ====================================================================

SUBROUTINE FGFR1(LZDEFR, FR, FI, LZTWC, LZFSC, LZFPC, LZTWM, LZFPM, LZFSM)

  USE sac_data_mod, ONLY: dp, FGCO, FGPM

  IMPLICIT NONE

  ! ----- DUMMY ARGUMENTS -----
  DOUBLE PRECISION, INTENT(IN)    :: LZDEFR
  DOUBLE PRECISION, INTENT(INOUT) :: FR, FI
  DOUBLE PRECISION, INTENT(IN)    :: LZTWC, LZFSC, LZFPC, LZTWM, LZFPM, LZFSM

  ! ----- LOCAL VARIABLES -----
  DOUBLE PRECISION :: FINDX, FRTEMP, SATR, FREXP, EXP, FSAT, FDRY

  ! INITIAL VALUES (FGCO, FGPM are global)
  FINDX = FGCO(1)
  FRTEMP = FGPM(5)
  SATR = FGPM(6)
  FREXP = FGPM(7)

  ! LOGIC
  IF (FINDX .LT. FRTEMP) THEN
    EXP = FRTEMP - FINDX
    FSAT = (1.0_dp - SATR) ** EXP
    FDRY = 1.0_dp

    IF (LZDEFR .GT. 0.0_dp) THEN
      FR = FSAT + (FDRY - FSAT) * (LZDEFR ** FREXP)
      FI = FR
    ELSE
      FR = FSAT
      FI = FR
    END IF
  END IF

END SUBROUTINE FGFR1


! ====================================================================
! SUBROUTINE FROST1 - FROZEN GROUND INDEX UPDATE
! ====================================================================

SUBROUTINE FROST1(PX, SUR, DIR, TA, LWE, WE, ISC, AESC, DT, &
                  UZTWM, UZFWM, LZTWM, LZFSM, LZFPM, LZSK, LZPK, &
                  UZTWC, UZFWC, LZTWC, LZFSC, LZFPC)

  USE sac_data_mod, ONLY: dp, FGPM, FGCO

  IMPLICIT NONE

  ! ----- DUMMY ARGUMENTS -----
  DOUBLE PRECISION, INTENT(IN)    :: PX, SUR, DIR, TA, LWE, WE, AESC, DT
  INTEGER, INTENT(IN)     :: ISC

  ! SAC Parameters (IN)
  DOUBLE PRECISION, INTENT(IN)    :: UZTWM, UZFWM, LZTWM, LZFSM, LZFPM, LZSK, LZPK

  ! SAC State Variables (INOUT)
  DOUBLE PRECISION, INTENT(INOUT) :: UZTWC, UZFWC, LZTWC, LZFSC, LZFPC

  ! ----- LOCAL VARIABLES -----
  DOUBLE PRECISION :: FINDX, FINDX1, CSOIL, CSNOW, GHC, RTHAW, WATER, COVER, TWE, C, CFI

  ! INITIAL VALUES (FGCO, FGPM are global)
  FINDX = FGCO(1)
  FINDX1 = FINDX

  CSOIL = 4.0_dp * DT * FGPM(1)
  CSNOW = FGPM(2)
  GHC = FGPM(3) * DT
  RTHAW = FGPM(4)

  ! LOGIC
  IF (FINDX .LT. 0.0_dp) THEN
    WATER = PX - SUR - DIR
    IF (WATER .GT. 0.0_dp) THEN
      FINDX = FINDX + RTHAW * WATER
      IF (FINDX .GT. 0.0_dp) FINDX = 0.0_dp
    END IF
  END IF

  IF (.NOT. (FINDX .GE. 0.0_dp .AND. TA .GE. 0.0_dp)) THEN

    IF (LWE .EQ. 0.0_dp .OR. WE .EQ. 0.0_dp .OR. ISC .GT. 0 .AND. AESC .EQ. 0.0_dp) THEN
      C = CSOIL
    ELSE
      IF (ISC .GT. 0) THEN
        COVER = AESC
      ELSE
        COVER = 1.0_dp
      END IF

      TWE = WE / COVER
      C = CSOIL * (1.0_dp - COVER) + CSOIL * ((1.0_dp - CSNOW) ** TWE) * COVER
    END IF

    IF (TA .LT. 0.0_dp) THEN
      CFI = -C * SQRT(TA * TA + FINDX * FINDX) - C * FINDX + GHC
      FINDX = FINDX + CFI
    ELSE
      FINDX = FINDX + C * TA + GHC
    END IF

    IF (FINDX .GT. 0.0_dp) FINDX = 0.0_dp
  END IF

  FGCO(1) = FINDX

END SUBROUTINE FROST1

SUBROUTINE EXSAC(NSOLD, DTM, PCP, TMP, ETP, &
                 UZTWM, UZFWM, UZK, PCTIM, ADIMP, RIVA, ZPERC, &
                 REXP, LZTWM, LZFSM, LZFPM, LZSK, LZPK, PFREE, &
                 SIDE, RSERV, UZTWC, UZFWC, LZTWC, LZFSC, LZFPC, ADIMC, &
                 QS, QG, Q, ETA, ROIMP, SDRO, SSUR, SIF, BFS, BFP, BFNCC)
  
  ! Use the module only for global variables not passed as arguments (FGCO, FSUMS1)
  USE sac_data_mod, ONLY: dp, FGCO, RSUM, PPE, PSC, PTA, PWE, &
                          SROT, SIMPVT, SRODT, SROST, SINTFT, SGWFP, SGWFS, SRECHT, &
                          SETT, SE1, SE2, SE3, SE4, SE5
  
  IMPLICIT NONE
  
  ! ----- DUMMY ARGUMENTS (Retained from original list) -----
  INTEGER, INTENT(IN)    :: NSOLD
  DOUBLE PRECISION, INTENT(IN)    :: DTM, PCP, TMP, ETP
  
  ! SAC Parameters (IN)
  DOUBLE PRECISION, INTENT(IN)    :: UZTWM, UZFWM, UZK, PCTIM, ADIMP, RIVA, ZPERC, &
                             REXP, LZTWM, LZFSM, LZFPM, LZSK, LZPK, PFREE, &
                             SIDE, RSERV
                             
  ! SAC State Variables (INOUT - Updated by SAC1)
  DOUBLE PRECISION, INTENT(INOUT) :: UZTWC, UZFWC, LZTWC, LZFSC, LZFPC, ADIMC 
                             
  ! SAC Outputs (OUT)
  DOUBLE PRECISION, INTENT(OUT)   :: QS, QG, Q, ETA, ROIMP, SDRO, SSUR, SIF, BFS, BFP, BFNCC

  ! ----- LOCAL VARIABLES (Converted to DOUBLE PRECISION) -----
  DOUBLE PRECISION :: TOTAL_S1, TOTAL_S2, TOTAL_S1_1, TOTAL_S2_1
  DOUBLE PRECISION :: DT, DS, DS_1, BAL, BAL_1, EP1, P1, TCI
  DOUBLE PRECISION :: TA, LWE, WE, ISC, AESC  ! Dummy frozen ground arguments
  INTEGER  :: IFRZE
  
  ! ... (Code to declare local variables LZTWM, LZFSM, etc. as DOUBLE PRECISION is removed, as they are arguments) ...

  ! Compute total initial storage (uses arguments)
  TOTAL_S1 = UZTWC + UZFWC + LZTWC + LZFSC + LZFPC + ADIMC
  TOTAL_S1_1 = UZTWC + UZFWC + LZTWC + LZFSC + LZFPC
  
  ! Compute surface moisture fluxes
  DT = DTM / 86400.0_dp
  EP1 = ETP
  P1 = PCP

  ! Dynamically set the IFRZE flag based on the air temperature
  ! reaching the freezing point, which will then turn on frozen
  ! ground processes
  IF (TMP .LE. 0.0_dp) THEN
    IFRZE = 1
  ELSE
    IFRZE = 0
  ENDIF
  
  ! CALL SAC1: Argument list must pass ALL state and parameter variables.
  CALL SAC1(DT, P1, EP1, TCI, ROIMP, SDRO, SSUR, SIF, BFS, BFP, ETA, BFNCC, &
            IFRZE, TA, LWE, WE, ISC, AESC, &
            UZTWM, UZFWM, UZK, PCTIM, ADIMP, RIVA, ZPERC, &
            REXP, LZTWM, LZFSM, LZFPM, LZSK, LZPK, PFREE, SIDE, RSERV, &
            UZTWC, UZFWC, LZTWC, LZFSC, LZFPC, ADIMC)
            
  ! Compute final total storage and water balance
  QS = ROIMP + SDRO + SSUR + SIF
  QG = BFS + BFP
  Q  = TCI
  
  TOTAL_S2 = UZTWC + UZFWC + LZTWC + LZFSC + LZFPC + ADIMC
  DS = TOTAL_S2 - TOTAL_S1
  
  TOTAL_S2_1 = UZTWC + UZFWC + LZTWC + LZFSC + LZFPC
  DS_1 = TOTAL_S1_1 - TOTAL_S2_1
  BAL_1 = P1 - ETA - TCI - DS
  BAL = P1 - ETA - QS - QG - DS
  
END SUBROUTINE EXSAC

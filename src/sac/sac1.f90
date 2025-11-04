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
  
  ! ----- INPUT ARGUMENTS -----
  DOUBLE PRECISION, INTENT(IN)    :: DT
!    ---- Timestep ------
!    DT      Computational time interval for sac-sma
  DOUBLE PRECISION, INTENT(IN)    :: PXV, EP
!    ----- FORCINGS -----
!    PXV      Input moisture (e.g. precip, precip+melt)
!    ET       Potential evapotranspiration  
  DOUBLE PRECISION, INTENT(IN)    :: TA, LWE, WE, AESC
  INTEGER, INTENT(IN)     :: IFRZE, ISC
  DOUBLE PRECISION, INTENT(INOUT) :: TCI, ROIMP, SDRO, SSUR, SIF, BFS, BFP, TET, BFNCC

  ! SAC Parameters (IN)
  DOUBLE PRECISION, INTENT(IN)    :: UZTWM, UZFWM, UZK, PCTIM, ADIMP, RIVA, ZPERC, &
                             REXP, LZTWM, LZFSM, LZFPM, LZSK, LZPK, PFREE, &
                             SIDE, RSERV
!    ----- PARAMETERS -----
!    UZTWM    Maximum upper zone tension water
!    UZFWM    Maximum upper zone free water
!    LZTWM    Maximum lower zone tension water
!    LZFSM    Maximum lower zone free water, secondary (aka
!                 supplemental)
!    LZFPM    Maximum lower zone free water, primary
!    ADIMP    Additional "impervious" area due to saturation.  Also has
!                 been defined as the fraction of area that can produce
!                 direct runoff - this is the maximum value it can be
!    UZK      Upper zone recession coefficient
!    LZPK     Lower zone recession coefficient, primary
!    LZSK     Lower zone recession coefficient, secondary (supplemental)
!    ZPERC    Minimum percolation rate coefficient
!    REXP     Percolation equation exponent
!    PCTIM    Minimum percent impervious area.  this area is always
!                 impervious (e.g. roads)
!    PFREE    Percent percolating directly to lower zone free water
!    RIVA     Percent riparian area
!    SIDE     Portion of baseflow which does *NOT* go to the stream
!    RSERV    Percent of lower zone free water not transferable to the
!                 lower zone tension water                            
                             
  ! SAC State Variables
  DOUBLE PRECISION, INTENT(INOUT) :: UZTWC, UZFWC, LZTWC, LZFSC, LZFPC, ADIMC
  
  ! ----- LOCAL VARIABLES (All converted to DOUBLE PRECISION) -----
  DOUBLE PRECISION :: EDMND, E1, RED, E2, UZRAT, E3, RATLZT, SAVED, RATLZ, DEL, E5
  DOUBLE PRECISION :: TWX, SPERC, DINC, PINC, DUZ, DLZP, DLZS, PAREA, ADSUR, RATIO, ADDRO
  DOUBLE PRECISION :: BF, SBF, SPBF, PERCM, PERC, DEFR, FR, FI, UZDEFR, CHECK, PERCT, PERCF
  DOUBLE PRECISION :: HPL, RATLP, RATLS, FRACP, PERCP, PERCS, EXCESS, SUR, EUSED, TBF, BFCC, E4
  DOUBLE PRECISION :: FRACP_DENOM
  INTEGER  :: I, NINC
  LOGICAL  :: bypass_ratio_check = .FALSE. ! <-- FLAG for GOTO equivalents

!    ----- VARIABLES -----
!    IFRZE    Frozen ground module switch.  0 = No frozen ground module,
!                 1 = Use frozen ground module
!    EDMND    ET demand for the time interval
!    E1       ET from the upper zone tension water content (UZTWC)
!    RED      Residual ET demand
!    E2       ET from upper zone free water content (UZFWC)
!    UZRAT    Upper zone ratio used to transfer water from free to
!                 tension water store
!    E3       ET from the lower zone tension water content (LZTWC)
!    RATLZT   Ratio of the lower zone tension water content to the
!                 maximum tension water.  AKA: percent saturation of 
!                 the lower zone tension water
!    DEL      Used for multiple calculations in the code:
!                 1. Amount of water moved from lower zone free water
!                    content to the tension water content
!                 2. Incremental interflow
!    E5       ET from ADIMP area
!    TWX      Time interval available moisture in excess of UZTW
!                 requirements
!    SIMPVT   Sum of ROIMP
!    SPERC    Sum of incremental percolation
!    SPBF     Sum of the incremental LZ primary baseflow component only
!    NINC     Number of time sub-increments that the time interval is
!                 diveded into for further soil moisture accounting
!    DINC     Length of each sub-increment (calculated by NINC) in days
!    PINC     Amount of available moisture for each time sub-increment
!    DUZ      Depletion in the upper zone
!    DLZP     Depletion in the lower zone, primary
!    DLZS     Depletion in the lower zone, secondary
!    PAREA    Pervious area
!    I        Loop counter
!    ADSUR    Surface runoff from portion of ADIMP not currently
!             generating direct runoff (ADDRO)
!    RATIO    Ratio of excess water in the upper zone from ADIMC to the
!                 maximum lower zone tension water. Used to calculate
!                 ADDRO
!    ADDRO    Additional "impervious" direct runoff from ADIMP.
!                 Essentially saturation excess runoff from ADIMP area
!    BF       Used for multiple baseflow calculations in the code
!                 1. Incremental baseflow, lower zone primary
!                 2. Incremental baseflow, lower zone secondary
!    SBF      Sum of the incremental baseflow components (LZ primary,
!                 secondary).
!    PERCM    Limiting percolation value (aka maximum percolation). In
!                 some documentation it is referred to as PBASE
!    PERC     Percolation
!    DEFR     Lower zone moisture deficiency ratio
!    FR       Change in percolation withdrawal due to frozen ground
!    FI       Change in interflow withdrawal due to frozen ground
!    UZDEFR   Calculated, but not used. RECOMMEND removing
!    CHECK    A check to see if percolation exceeds the lower zone
!                 deficiency
!    SPERC    Sum of interval percolation
!    PERCT    Percolation to tension water
!    PERCF    Percolation to free water
!    HPL      Relative size of the lower zone max free water, primary
!                 storage to the lower zone total max free water storage
!    RATLP    Content capacity ratio (LZ, primary) (i.e. relative
!                 fullness)
!    RATLS    Content capacity ratio (LZ, secondary) (i.e. relative
!                 fullness)
!    FRACP    Fraction going to primary store during each interval
!    PERCP    Amount of excess percolation going to the LZ primary store
!    PERCS    Amount of excess percolation going to the LZ secondary
!                 store
!    EXCESS   LZ free water in excess of the maximum to be removed from
!                 LZFPC and added to LZTWC
!    SUR      Incremental surface runoff.  Not multiplied by PAREA until
!                 added to the sum (SSUR)
!    EUSED    Total ET from the pervious area (PAREA) = E1+E2+E3
!    TBF      Total baseflow
!    BFCC     Baseflow channel component (reduces TBF by fraction SIDE)
!    SINTFT   Monthly sum of SIF (NOT USED)
!    SGWFP    Monthly sum of BFP (NOT USED)
!    SGWFS    Monthly sum of BFS (NOT USED)
!    SRECHT   Monthly sum of BFNCC (NOT USED)
!    SROST    Monthly sum of SSUR (NOT USED)
!    SRODT    Monthly sum of SDRO (NOT USED)
!    E4       ET from riparian vegetation using RIVA
!    SROT     Assuming this is the monthly sum of TCI (NOT USED)
!    TET      Total evapotranspiration
!    SETT     Assuming this is the monthly sum of TET (NOT USED)
!    SE1      Assuming this is the monthly sum of E1 (NOT USED)
!    SE2      Assuming this is the monthly sum of E2 (NOT USED)
!    SE3      Assuming this is the monthly sum of E3 (NOT USED)
!    SE4      Assuming this is the monthly sum of E4 (NOT USED)
!    SE5      Assuming this is the monthly sum of E5 (NOT USED)
!    RSUM(7)  Sums of (1) TCI, (2) ROIMP, (3) SDRO, (4) SSUR, (5) SIF,
!                 (6) BFS, (7) BFP. (NOT USED)



  ! COMPUTE ET FROM UPPER ZONE.
  EDMND = EP
  E1 = EDMND * (UZTWC / UZTWM)
  ! RED IS RESIDUAL EVAP DEMAND
  RED = EDMND - E1
  UZTWC = UZTWC - E1
  E2 = 0.0_dp

  ! E1 CAN NOT EXCEED UZTWC
  IF (UZTWC .LT. 0.0_dp) THEN
    E1 = E1 + UZTWC
    UZTWC = 0.0_dp
    RED = EDMND - E1
    
    ! E2 IS RESIDUAL EVAP DEMAND
    IF (UZFWC .GE. RED) THEN
      E2 = RED
      UZFWC = UZFWC - E2
      RED = 0.0_dp
    ! E2 IS EVAP FROM UZFWC.
    ELSE
      E2 = UZFWC
      UZFWC = 0.0_dp
      RED = RED - E2
      bypass_ratio_check = .TRUE.
    END IF
  END IF

  IF (.NOT. bypass_ratio_check) THEN
    ! UPPER ZONE FREE WATER RATIO EXCEEDS UPPER ZONE
    ! TENSION WATER RATIO, THUS TRANSFER FREE WATER TO TENSION
    IF ((UZTWC / UZTWM) .LT. (UZFWC / UZFWM)) THEN
      UZRAT = (UZTWC + UZFWC) / (UZTWM + UZFWM)
      UZTWC = UZTWM * UZRAT
      UZFWC = UZFWM * UZRAT
    END IF
  END IF
  

  ! COMPUTE ET FROM THE LOWER ZONE.
  ! COMPUTE ET FROM LZTWC (E3)
  E3 = RED * (LZTWC / (UZTWM + LZTWM))
  LZTWC = LZTWC - E3

  ! E3 CAN NOT EXCEED LZTWC
  IF (LZTWC .LT. 0.0_dp) THEN
    E3 = E3 + LZTWC
    LZTWC = 0.0_dp
  END IF
  
  RATLZT = LZTWC / LZTWM
  
  ! INFERRED PARAMETER (ADDED BY Q DUAN ON 3/6/95)
  SAVED = RSERV * (LZFPM + LZFSM)
  RATLZ = (LZTWC + LZFPC + LZFSC - SAVED) / (LZTWM + LZFPM + LZFSM - SAVED)

  IF (RATLZT .LT. RATLZ) THEN
    ! RESUPPLY LOWER ZONE TENSION WATER FROM LOWER
    ! ZONE FREE WATER IF MORE WATER AVAILABLE THERE.
    DEL = (RATLZ - RATLZT) * LZTWM
    ! TRANSFER FROM LZFSC TO LZTWC
    LZTWC = LZTWC + DEL
    LZFSC = LZFSC - DEL
    
    IF (LZFSC .LT. 0.0_dp) THEN
      ! IF TRANSFER EXCEEDS LZFSC THEN REMAINDER COMES FROM LZFPC
      LZFPC = LZFPC + LZFSC
      LZFSC = 0.0_dp
    END IF
  END IF

  ! COMPUTE ET FROM ADIMP AREA.-E5
  E5 = E1 + (RED + E2) * ((ADIMC - E1 - UZTWC) / (UZTWM + LZTWM))
  ! ADJUST ADIMC,ADDITIONAL IMPERVIOUS AREA STORAGE, FOR EVAPORATION.
  ADIMC = ADIMC - E5
  
  IF (ADIMC .LT. 0.0_dp) THEN
    ! E5 CAN NOT EXCEED ADIMC.
    E5 = E5 + ADIMC
    ADIMC = 0.0_dp
  END IF
  ! E5 IS ET FROM THE AREA ADIMP.
  E5 = E5 * ADIMP

  ! COMPUTE PERCOLATION AND RUNOFF AMOUNTS.

  ! TWX IS THE TIME INTERVAL AVAILABLE MOISTURE IN EXCESS
  ! OF UZTW REQUIREMENTS.
  TWX = PXV + UZTWC - UZTWM
  
  IF (TWX .LT. 0.0_dp) THEN
    ! ALL MOISTURE HELD IN UZTW--NO EXCESS.
    UZTWC = UZTWC + PXV
    TWX = 0.0_dp
  ELSE
    ! MOISTURE AVAILABLE IN EXCESS OF UZTW STORAGE.
    UZTWC = UZTWM
  END IF
  
  ADIMC = ADIMC + PXV - TWX

  ! COMPUTE IMPERVIOUS AREA RUNOFF.

  ! ROIMP IS RUNOFF FROM THE MINIMUM IMPERVIOUS AREA.
  ROIMP = PXV * PCTIM
  SIMPVT = SIMPVT + ROIMP

  ! INITIALIZE TIME INTERVAL SUMS.
  SBF=0.0_dp; SSUR=0.0_dp; SIF=0.0_dp; SPERC=0.0_dp; SDRO=0.0_dp; SPBF=0.0_dp

  ! DETERMINE COMPUTATIONAL TIME INCREMENTS FOR THE BASIC TIME INTERVAL

  ! NINC=NUMBER OF TIME INCREMENTS THAT THE TIME INTERVAL
  ! IS DIVIDED INTO FOR FURTHER
  ! SOIL-MOISTURE ACCOUNTING.  NO ONE INCREMENT
  ! WILL EXCEED 5.0 MILLIMETERS OF UZFWC+PAV  
  NINC = INT(1.0_dp + 0.2_dp * (UZFWC + TWX))

  ! DINC=LENGTH OF EACH INCREMENT IN DAYS.
  DINC = (1.0_dp / REAL(NINC, dp)) * DT

  ! PINC=AMOUNT OF AVAILABLE MOISTURE FOR EACH INCREMENT.
  PINC = TWX / REAL(NINC, dp)

  ! COMPUTE FREE WATER DEPLETION FRACTIONS FOR
  ! THE TIME INCREMENT BEING USED-BASIC DEPLETIONS
  ! ARE FOR ONE DAY
  DUZ = 1.0_dp - ((1.0_dp - UZK) ** DINC)
  DLZP = 1.0_dp - ((1.0_dp - LZPK) ** DINC)
  DLZS = 1.0_dp - ((1.0_dp - LZSK) ** DINC)

  ! INFERRED PARAMETER (ADDED BY Q DUAN ON 3/6/95)
  PAREA = 1.0_dp - ADIMP - PCTIM
  
  ! -------------------------------------------------------------------
  ! START INCREMENTAL LOOP FOR THE TIME INTERVAL
  ! -------------------------------------------------------------------
  DO I = 1, NINC
    
    ADSUR = 0.0_dp
    ! COMPUTE DIRECT RUNOFF (FROM ADIMP AREA)
    RATIO = (ADIMC - UZTWC) / LZTWM
    IF (RATIO .LT. 0.0_dp) RATIO = 0.0_dp
    ! ADDRO IS THE AMOUNT OF DIRECT RUNOFF FROM THE AREA ADIMP.
    ADDRO = PINC * (RATIO ** 2)
    
    ! COMPUTE BASEFLOW AND KEEP TRACK OF TIME INTERVAL SUM.
    BF = LZFPC * DLZP
    LZFPC = LZFPC - BF
    IF (LZFPC .LT. 0.0001_dp) THEN
      BF = BF + LZFPC
      LZFPC = 0.0_dp
    END IF
    SBF = SBF + BF
    SPBF = SPBF + BF
    
    BF = LZFSC * DLZS
    LZFSC = LZFSC - BF
    IF (LZFSC .LT. 0.0001_dp) THEN
      BF = BF + LZFSC
      LZFSC = 0.0_dp
    END IF
    SBF = SBF + BF
    
    ! COMPUTE PERCOLATION-IF NO WATER AVAILABLE THEN SKIP
    IF ((PINC + UZFWC) .LE. 0.01_dp) THEN
      UZFWC = UZFWC + PINC
      ADIMC=ADIMC+PINC-ADDRO-ADSUR
      IF (ADIMC.GT.(UZTWM+LZTWM)) THEN
        ADDRO=ADDRO+ADIMC-(UZTWM+LZTWM)
        ADIMC=UZTWM+LZTWM
      ENDIF
      SDRO=SDRO+ADDRO*ADIMP
      CONTINUE
    ENDIF
    PERCM = LZFPM * DLZP + LZFSM * DLZS
    PERC = PERCM * (UZFWC / UZFWM)
    
    ! DEFR IS THE LOWER ZONE MOISTURE DEFICIENCY RATIO
    DEFR = 1.0_dp - ((LZTWC + LZFPC + LZFSC) / (LZTWM + LZFPM + LZFSM))

    ! FR IS THE CHANGE IN PERCOLATION WITHDRAWAL DUE TO FROZEN GROUND
    FR = 1.0_dp
    ! FI IS THE CHANGE IN INTERFLOW WITHDRAWAL DUE TO FROZEN GROUND.
    FI = 1.0_dp
      
    ! Frozen Ground Adjustment
    IF (IFRZE .NE. 0) THEN
      UZDEFR = 1.0_dp - ((UZTWC + UZFWC) / (UZTWM + UZFWM))
      CALL FGFR1(DEFR, FR, FI, LZTWC, LZFSC, LZFPC, LZTWM, LZFPM, LZFSM)
    END IF

    ! NOTE...PERCOLATION OCCURS FROM UZFWC BEFORE PAV IS ADDED
    PERC = PERC * (1.0_dp + ZPERC * (DEFR ** REXP)) * FR
      
    IF (PERC .GE. UZFWC) THEN
      ! PERCOLATION RATE EXCEEDS UZFWC.
      PERC = UZFWC
    END IF 
    ! PERCOLATION RATE IS LESS THAN UZFWC.  
    UZFWC = UZFWC - PERC

    ! CHECK TO SEE IF PERCOLATION EXCEEDS LOWER ZONE DEFICIENCY
    CHECK = LZTWC + LZFPC + LZFSC + PERC - LZTWM - LZFPM - LZFSM
    IF (CHECK .GT. 0.0_dp) THEN
      PERC = PERC - CHECK
      UZFWC = UZFWC + CHECK
    END IF

    ! SPERC IS THE TIME INTERVAL SUMMATION OF PERC
    SPERC = SPERC + PERC
      
    ! COMPUTE INTERFLOW AND KEEP TRACK OF TIME INTERVAL SUM.
    ! NOTE...PINC HAS NOT YET BEEN ADDED
    DEL = UZFWC * DUZ * FI
    SIF = SIF + DEL
    UZFWC = UZFWC - DEL

    ! DISTRIBE PERCOLATED WATER INTO THE LOWER ZONES
    ! TENSION WATER MUST BE FILLED FIRST EXCEPT FOR THE PFREE AREA

    ! PERCT IS PERCOLATION TO TENSION WATER AND PERCF IS PERCOLATION
    ! GOING TO FREE WATER.
    PERCT = PERC * (1.0_dp - PFREE)
      
    IF ((PERCT + LZTWC) .LE. LZTWM) THEN
      LZTWC = LZTWC + PERCT
      PERCF = 0.0_dp    
    ELSE
      PERCF = PERCT + LZTWC - LZTWM
      LZTWC = LZTWM    
    END IF 

    ! DISTRIBUTE PERCOLATION IN EXCESS OF TENSION
    ! REQUIREMENTS AMONG THE FREE WATER STORAGES.
    PERCF = PERCF + PERC * PFREE
      
      IF (PERCF .NE. 0.0_dp) THEN
           ! HPL IS THE RELATIVE SIZE OF THE PRIMARY STORAGE
           ! AS COMPARED WITH TOTAL LOWER ZONE FREE WATER STORAGE.
           HPL = LZFPM / (LZFPM + LZFSM)
           
           ! RATLP AND RATLS ARE CONTENT TO CAPACITY RATIOS, OR
           ! IN OTHER WORDS, THE RELATIVE FULLNESS OF EACH STORAGE
           RATLP = LZFPC / LZFPM
           RATLS = LZFSC / LZFSM

           ! FRACP IS THE FRACTION GOING TO PRIMARY.
           FRACP=(HPL*2.0*(1.0-RATLP))/((1.0-RATLP)+(1.0-RATLS))
           IF (FRACP .GT. 1.0_dp) FRACP = 1.0_dp
           ! PERCP AND PERCS ARE THE AMOUNT OF THE EXCESS
           ! PERCOLATION GOING TO PRIMARY AND SUPPLEMENTAL
           ! STORGES,RESPECTIVELY.
           PERCP = PERCF * FRACP
           PERCS = PERCF - PERCP  
           LZFSC=LZFSC+PERCS
           IF(LZFSC.GT.LZFSM) THEN
             PERCS=PERCS-LZFSC+LZFSM
             LZFSC=LZFSM
           ENDIF
           LZFPC=LZFPC+(PERCF-PERCS)
           ! CHECK TO MAKE SURE LZFPC DOES NOT EXCEED LZFPM
           IF (LZFPC.GT.LZFPM) THEN
             EXCESS=LZFPC-LZFPM
             LZTWC=LZTWC+EXCESS
             LZFPC=LZFPM
           ENDIF
        END IF
      
      ! DISTRIBUTE PINC BETWEEN UZFWC AND SURFACE RUNOFF.
      IF (PINC .NE. 0.0_dp) THEN
        ! CHECK IF PINC EXCEEDS UZFWM
        IF ((PINC + UZFWC) .GT. UZFWM) THEN
          ! COMPUTE SURFACE RUNOFF (SUR) AND KEEP TRACK OF
          ! TIME INTERVAL SUM.
          SUR = PINC + UZFWC - UZFWM
          UZFWC = UZFWM
          SSUR = SSUR + SUR * PAREA
          ! ADSUR IS THE AMOUNT OF SURFACE RUNOFF WHICH COMES
          ! FROM THAT PORTION OF ADIMP WHICH IS NOT
          ! CURRENTLY GENERATING DIRECT RUNOFF.  ADDRO/PINC
          ! IS THE FRACTION OF ADIMP CURRENTLY GENERATING
          ! DIRECT RUNOFF.
          ADSUR = SUR * (1.0_dp - ADDRO / PINC)
          SSUR = SSUR + ADSUR * ADIMP
        ELSE
          ! NO SURFACE RUNOFF
          UZFWC = UZFWC + PINC
        END IF
      END IF
      
      !  ADIMP AREA WATER BALANCE -- SDRO IS THE 
      !  6 HR SUM OF DIRECT RUNOFF
      ADIMC = ADIMC + PINC - ADDRO - ADSUR
      
      IF (ADIMC .GT. (UZTWM + LZTWM)) THEN
        ADDRO = ADDRO + ADIMC - (UZTWM + LZTWM)
        ADIMC = UZTWM + LZTWM
      END IF 
      
      SDRO = SDRO + ADDRO * ADIMP
      
  ! END OF INCREMENTAL DO LOOP. 
  END DO

  ! COMPUTE SUMS AND ADJUST RUNOFF AMOUNTS BY THE AREA OVER
  ! WHICH THEY ARE GENERATED.
  
  ! EUSED IS THE ET FROM PAREA WHICH IS 1.0-ADIMP-PCTIM
  EUSED = E1 + E2 + E3

  ! SEPARATE CHANNEL COMPONENT OF BASEFLOW
  ! FROM THE NON-CHANNEL COMPONENT
  SIF = SIF * PAREA
  ! TBF IS TOTAL BASEFLOW
  TBF = SBF * PAREA

  ! BFCC IS BASEFLOW, CHANNEL COMPONENT
  BFCC = TBF * (1.0_dp / (1.0_dp + SIDE))
  BFP = SPBF * PAREA / (1.0_dp + SIDE)
  BFS = BFCC - BFP
  IF (BFS .LT. 0.0_dp) BFS = 0.0_dp
  ! BFNCC IS BASEFLOW,NON-CHANNEL COMPONENT
  BFNCC = TBF - BFCC
  
  ! ADD TO MONTHLY SUMS.
  SINTFT = SINTFT + SIF
  SGWFP = SGWFP + BFP
  SGWFS = SGWFS + BFS
  SRECHT = SRECHT + BFNCC
  SROST = SROST + SSUR
  SRODT = SRODT + SDRO

  ! COMPUTE TOTAL CHANNEL INFLOW FOR THE TIME INTERVAL
  TCI = ROIMP + SDRO + SSUR + SIF + BFCC

  ! COMPUTE E4-ET FROM RIPARIAN VEGETATION.
  E4 = (EDMND - EUSED) * RIVA

  ! SUBTRACT E4 FROM CHANNEL INFLOW
  TCI = TCI - E4
  !IF (TCI .LT. 0.0_dp) THEN
  !  E4 = E4 + TCI
  !  TCI = 0.0_dp
  !END IF
  SROT = SROT + TCI

  ! COMPUTE TOTAL EVAPOTRANSPIRATION-TET
  EUSED = EUSED * PAREA
  TET = EUSED + E5 + E4
  SETT = SETT + TET
  SE1 = SE1 + E1 * PAREA
  SE3 = SE3 + E3 * PAREA
  SE4 = SE4 + E4
  SE5 = SE5 + E5

  ! CHECK THAT ADIMC.GE.UZTWC
  IF (ADIMC .LT. UZTWC) ADIMC = UZTWC 

  ! COMPUTE NEW FROST INDEX AND MOISTURE TRANSFER.
  IF (IFRZE .GT. 0) CALL FROST1(PXV, SSUR, SDRO, TA, LWE, WE, ISC, AESC, DT, &
                               UZTWM, UZFWM, LZTWM, LZFSM, LZFPM, LZSK, LZPK, &
                               UZTWC, UZFWC, LZTWC, LZFSC, LZFPC)
  
  ! ADD TO SUMS OF RUNOFF COMPONENTS.
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
  ! THIS SUBROUTINE COMPUTES THE CHANGE IN THE PERCOLATION AND
  ! INTERFLOW WITHDRAWAL RATES DUE TO FROZEN GROUND.

  ! WRITTEN BY -- ERIC ANDERSON - HRL   JUNE 1980
  
  USE sac_data_mod, ONLY: dp, FGCO, FGPM

  IMPLICIT NONE

  ! ----- DUMMY ARGUMENTS -----
  DOUBLE PRECISION, INTENT(IN)    :: LZDEFR
  DOUBLE PRECISION, INTENT(INOUT) :: FR, FI
  DOUBLE PRECISION, INTENT(IN)    :: LZTWC, LZFSC, LZFPC, LZTWM, LZFPM, LZFSM

  ! ----- LOCAL VARIABLES -----
  DOUBLE PRECISION :: FINDX, FRTEMP, SATR, FREXP, EXP, FSAT, FDRY

  ! INITIAL VALUES
  FINDX = FGCO(1)
  FRTEMP = FGPM(5)
  SATR = FGPM(6)
  FREXP = FGPM(7)

  ! DETERMINE IF FROZEN GROUND EFFECT EXISTS.
  IF (FINDX .LT. FRTEMP) THEN
    ! COMPUTE SATURATED REDUCTION.
    EXP = FRTEMP - FINDX
    RETURN
  ELSE
    FSAT = (1.0_dp - SATR) ** EXP
    ! CHANGE AT DRY CONDITIONS
    FDRY = 1.0_dp
  ENDIF
  ! COMPUTE ACTUAL CHANGE
  IF (LZDEFR .GT. 0.0_dp) THEN
    FR = FSAT + (FDRY - FSAT) * (LZDEFR ** FREXP)
    FI = FR
    RETURN
  ELSE
    FR = FSAT
    FI = FR
    RETURN
  END IF

END SUBROUTINE FGFR1


! ====================================================================
! SUBROUTINE FROST1 - FROZEN GROUND INDEX UPDATE
! ====================================================================

SUBROUTINE FROST1(PX, SUR, DIR, TA, LWE, WE, ISC, AESC, DT, &
                  UZTWM, UZFWM, LZTWM, LZFSM, LZFPM, LZSK, LZPK, &
                  UZTWC, UZFWC, LZTWC, LZFSC, LZFPC)
  ! THIS SUBROUTINE COMPUTES THE CHANGE IN THE FROZEN GROUND
  ! INDEX AND MOISTURE MOVEMENT DUE TO TEMPERATURE GRADIENTS.

  ! WRITTEN BY ERIC ANDERSON - HRL   JUNE 1980
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

  ! INITIAL VALUES
  FINDX = FGCO(1)
  FINDX1 = FINDX
  CSOIL = 4.0_dp * DT * FGPM(1)
  CSNOW = FGPM(2)
  GHC = FGPM(3) * DT
  RTHAW = FGPM(4)

  ! COMPUTE MOISTURE MOVEMENT
  ! EQUATIONS NOT READY YET.

  ! COMPUTE CHANGE IN FROZEN GROUND INDEX.
  ! CHANGE DUE TO WATER FREZING IN THE SOIL.
  IF (FINDX .LT. 0.0_dp) THEN
    WATER = PX - SUR - DIR
    IF (WATER .GT. 0.0_dp) THEN
      FINDX = FINDX + RTHAW * WATER
      IF (FINDX .GT. 0.0_dp) FINDX = 0.0_dp
    END IF
  END IF

  ! CHANGE DUE TO TEMPERATURE.
  IF ((FINDX .GE. 0.0_dp) .AND. (TA .GE. 0.0_dp)) THEN
    IF (FINDX.LT.0.0) THEN
      CONTINUE
    ELSE
      FGCO(1) = FINDX
      RETURN
    ENDIF
  ENDIF

  ! COMPUTE TRANSFER COEFFIENT.
  IF ((LWE .EQ. 0.0_dp) .OR. (WE.EQ.0.0_dp)) THEN
    C = CSOIL
    ! COMPUTE CHANGE IN FROST INDEX.
    IF (TA.GE.0.0) THEN
      FINDX=FINDX+C*TA+GHC
      ! CHECK FROST INDEX
      IF (FINDX.LT.0.0) THEN
        CONTINUE
      ELSE
        FINDX = 0.0_dp
        ! SAVE NEW FROST INDEX
        FGCO(1)=FINDX
        RETURN
      ENDIF
    ELSE
      CFI=-C*SQRT(TA*TA+FINDX*FINDX)-C*FINDX+GHC
      FINDX=FINDX+CFI
      IF (FINDX.LT.0.0) THEN
        CONTINUE
      ELSE
        FINDX = 0.0_dp
        FGCO(1)=FINDX
        RETURN
      ENDIF
    ENDIF
  ELSE IF (ISC.GT.0) THEN
    COVER=AESC
    IF (COVER.EQ.0.0) THEN
      C=CSOIL
      IF (TA.GE.0.0) THEN
        FINDX=FINDX+C*TA+GHC
        IF (FINDX.LT.0.0) THEN
          CONTINUE
        ELSE
          FINDX = 0.0_dp
          FGCO(1)=FINDX
          RETURN
        ENDIF
      ELSE
        CFI=-C*SQRT(TA*TA+FINDX*FINDX)-C*FINDX+GHC
        FINDX=FINDX+CFI
        IF (FINDX.LT.0.0) THEN
          CONTINUE
        ELSE
          FINDX = 0.0_dp
          FGCO(1)=FINDX
          RETURN
        ENDIF
      ENDIF
    ELSE
      TWE=WE/COVER
      C=CSOIL*(1.0-COVER)+CSOIL*((1.0-CSNOW)**TWE)*COVER
      IF (TA.GE.0.0) THEN
        FINDX=FINDX+C*TA+GHC
        IF (FINDX.LT.0.0) THEN
          CONTINUE
        ELSE
          FINDX = 0.0_dp
          FGCO(1)=FINDX
          RETURN
        ENDIF
      ELSE
        CFI=-C*SQRT(TA*TA+FINDX*FINDX)-C*FINDX+GHC
        FINDX=FINDX+CFI
        IF (FINDX.LT.0.0) THEN
          CONTINUE
        ELSE
          FINDX = 0.0_dp
          FGCO(1)=FINDX
          RETURN
        ENDIF
      ENDIF        
    ENDIF
      
  ELSE
    COVER=1.0
    TWE=WE/COVER
    C=CSOIL*(1.0-COVER)+CSOIL*((1.0-CSNOW)**TWE)*COVER
    IF (TA.GE.0.0) THEN
      FINDX=FINDX+C*TA+GHC
      IF (FINDX.LT.0.0) THEN
        CONTINUE
      ELSE
        FINDX = 0.0_dp
        FGCO(1)=FINDX
        RETURN
      ENDIF
    ELSE
      CFI=-C*SQRT(TA*TA+FINDX*FINDX)-C*FINDX+GHC
      FINDX=FINDX+CFI
      IF (FINDX.LT.0.0) THEN
        CONTINUE
      ELSE
        FINDX = 0.0_dp
        FGCO(1)=FINDX
        RETURN
      ENDIF
    ENDIF
  ENDIF
  RETURN

END SUBROUTINE FROST1

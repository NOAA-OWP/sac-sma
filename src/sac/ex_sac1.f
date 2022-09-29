      SUBROUTINE EXSAC(NSOLD,DTM,PCP,TMP,ETP,
C     SAC PARAMETERS
     &                 UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC,
     &                 REXP,LZTWM,LZFSM,LZFPM,LZSK,LZPK,PFREE,
     &                 SIDE,RSERV,
C     SAC State variables
     &                 UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC,
C     SAC OUTPUTS
     &                 QS,QG,Q,ETA,
     &                 ROIMP,SDRO,SSUR,SIF,BFS,BFP)

C      IMPLICIT NONE

C     RCS Id string, for version control
c      CHARACTER*60 RCSID
C      DATA RCSID/"$Id: ex_sac1.f,v 1.1 2006/09/01 21:59:43 vicadmin Exp $"/

C     ...THIS SUBROUTINE IS AN EXECUTION ROUTINE FOR SMFLX MODEL...
C
      INTEGER NSOLD
      REAL    DTM                 ! timestep in seconds
      REAL    PCP
      REAL    TMP
      REAL    ETP
      REAL    QS
      REAL    QG
      REAL    Q
      REAL    ETA
      REAL    DT         ! timestep in fractions of a day
      REAL    LZTWM,LZFSM,LZFPM,UZTWM,UZFWM,ADIMP
      REAL    LZSK,LZPK    ! missing other params
      REAL    LZTWC,LZFSC,LZFPC,UZTWC,UZFWC,ADIMC

      ! mass balance variables
      REAL    PAREA
      REAL    STATES_PRV_0, STATES_PRV, ADIMC_0, BAL    ! for water balance calcs
     
      ! these blocks contain some variables that are used in SAC1 and should be initialized
      !   (in other versions of ex1.f they are) to store run variable sums and other info
      COMMON/FSMCO1/FGCO(6),RSUM(7),PPE,PSC,PTA,PWE
      COMMON/FSUMS1/SROT,SIMPVT,SRODT,SROST,SINTFT,SGWFP,SGWFS,SRECHT,
     &              SETT,SE1,SE3,SE4,SE5
     
      ! --- record start of timestep state variables
      STATES_PRV_0 = UZTWC + UZFWC + LZTWC + LZFSC + LZFPC
      ADIMC_0      = ADIMC
!      print*, '---------------------------'
!      write (*,'(A,6F10.4)') 'ADIMC,UZTWC,UZFWC,LZTWC,LZFSC,LZFPC', 
!     &                     ADIMC,UZTWC,UZFWC,LZTWC,LZFSC,LZFPC
!      print*, '-----'
     
     
C     TURN OFF FROZEN GROUND PROCESS
      IFRZE = 0

C     COMPUTE SURFACE MOISTURE FLUXES
      DT = DTM/86400.0       ! timestep in days (can be fractional)
      EP1 = ETP
      P1 = PCP
      CALL SAC1(DT,
C     SAC FORCINGS
     &          P1,EP1,
C     SAC OUTPUT FLUX VARIABLES
     &          TCI,ROIMP,SDRO,SSUR,SIF,BFS,BFP,ETA,
C     SAC FROZEN GROUND VARIABLES
     &          IFRZE,TA,LWE,WE,ISC,AESC,
C     SAC PARAMETERS
     &          UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC,
     &          REXP,LZTWM,LZFSM,LZFPM,LZSK,LZPK,PFREE,
     &          SIDE,RSERV,
C     SAC State variables  ',
     &          UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC)

C     COMPUTE FINAL TOTAL STORAGE AND WATER BALANCE
C     sdro:  direct runoff
C     ROIMP: impervious area runoff
C     SSUR:  surface runoff (from upper zone free water)
C     SIF:   interflow (from uppper zone free water)
C     BFS:   non-channel baseflow (suggested: baseflow from LZ free supplemental)
C     BFP:   some kind of baseflow... (suggested: baseflow from LZ free primary)
C     TCI:   Total channel inflow

      QS = ROIMP + SDRO + SSUR + SIF
      QG = BFS + BFP
      PAREA = 1 - ADIMC - PCTIM
C
      ! ---- water balance info
      STATES_PRV = UZTWC + UZFWC + LZTWC + LZFSC + LZFPC
      BAL = P1 - ETA - (STATES_PRV-STATES_PRV_0)*(1-ADIMP-PCTIM)
     &         - (ADIMC - ADIMC_0)*ADIMP
     &         - TCI    

!      write (*,'(A,6F10.4)') 'ADIMC,UZTWC,UZFWC,LZTWC,LZFSC,LZFPC', 
!     &                     ADIMC,UZTWC,UZFWC,LZTWC,LZFSC,LZFPC
!      write (*,'(A,8F10.4)') 'PXV,EP,E1,E2,E3,E4,E5,AET', 
!     &                     PXV,EP,E1,E2,E3,E4,E5,TET
!      write (*,'(A,7F10.4)') 'ROIMP,SDRO,SSUR,SIF,BFS,BFP,TCI', 
!     &                     ROIMP,SDRO,SSUR,SIF,BFS,BFP,TCI
      write (*,'(A,F10.4)') '== BALANCE ==:',BAL
      print*, '---------------------------'



      RETURN
      END

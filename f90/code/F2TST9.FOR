C     Last change:  CA   13 Jul 2004    8:14 pm
C **********************************************************************

      MODULE ARRAY_PAR

      INTEGER MXSIM, MXAGE, I1YR, I1CYR, I1SYR, ICEND, IENDYR, IENDY2

C : extended gray whale trial values
c      PARAMETER (MXSIM=5000, MXAGE=40, I1YR=-411, I1CYR=-411, I1SYR=-44,
c     +           ICEND=299,IENDYR=300,IENDY2=1100)

C : standard gray whale trial values
      PARAMETER (MXSIM=100, MXAGE=40, I1YR=-411, I1CYR=-411, I1SYR=-44,
     +           ICEND=99,IENDYR=100,IENDY2=1100)

C     MXSIM  Maximum number of simulations (=max value of NTRIAL)
C     MXAGE  Maximum number of age classes (=max value of MAXAGE)
C     I1YR   first year of population arrays
C     I1CYR  first year of catch arrays given to SLA
C     I1SYR  first year of survey arrays given to SLA
C     ICEND  last year of catch & survey arrays given to SLA
C     IENDYR last year of management (=max value of NYEAR)
C     IENDY2 last year of extra population simulations run when conditioning
C            For most runs IENDYR=IENDY2=100 and ICEND=IENDYR-1.

      END MODULE ARRAY_PAR

C --------------------------------------------------------------------

      MODULE DECL_STKVRS

      USE ARRAY_PAR
      COMMON /STKVRS/ PMATF(2,I1YR:IENDY2),PSURV(2,I1YR:IENDY2),
     +   FMATUR(2,0:MXAGE),RECF(2,0:MXAGE),SUR(2,0:MXAGE),
     +   UNRM(2,0:MXAGE),UNRF(2,0:MXAGE),RM(2,0:MXAGE),RF(2,0:MXAGE),
     +   Z(2),A1(2),FEC(2),F0(2),
     +   SUMSUR(2),PROBE(2,-3:IENDY2),MAXAGE,OPTDD,IYRPRJ,INITYR,NY,
     +   ILAG,OPTM,DOCON,OPTEE,ISPass,IYRMANPAss,EXTRAS,
     +   PSURVObs(2,I1YR:IENDY2),
     +   IMMIM(I1YR:IENDY2,0:MXAGE),IMMIF(I1YR:IENDY2,0:MXAGE),
     +   TOTALIMMI(I1YR:IENDY2),STORESUR(2,0:MXAGE),SSTILDA
      DOUBLE PRECISION  PMATF,PSURV,RM,RF,UNRM,UNRF,FMATUR,RECF,SUR,
     +       Z,A1,FEC,F0,SUMSUR,EXTRAS,PSURVObs,IMMIM,IMMIF,TOTALIMMI,
     +       STORESUR,SSTILDA
      INTEGER PROBE,MAXAGE,OPTDD,IYRPRJ,INITYR,NY,ILAG,OPTM(2),
     +       DOCON,OPTEE,ISPass,IYRMANPAss

      COMMON /STKVR2/ CATCHM,CATCHF,KDD,A,
     +       NEXP,PCALF,AVEINCCAT,AEVINCCATF,INCCAT
      DOUBLE PRECISION CATCHM(2,I1YR:IENDYR),CATCHF(2,I1YR:IENDYR),
     +       KDD(2,I1YR:IENDY2),A(2,I1YR:IENDY2),
     +       NEXP(2,I1YR:IENDY2),
     +       PCALF(2,I1YR:IENDY2),AVEINCCAT(4),AVEINCCATF(4),
     +       INCCAT(2,I1YR:IENDY2)
C      
      END MODULE DECL_STKVRS

C --------------------------------------------------------------------

      MODULE DECL_SETUP

      USE ARRAY_PAR
      COMMON /SETUP/   MSYL,MSYR,RREC,MAT50,AREC,PREGTG,OPTF,OPMSYL,
     +                 OPTK,OPTA,ITENT,IYRTAR,R1PLUS,IMMIREF,OPTIMMI,
     +                 IMMIPOW
      DOUBLE PRECISION MSYL(2),MSYR(2),RREC(2),MAT50(2),AREC,PREGTG(2),
     +   R1PLUS(2),IMMIREF,IMMIPOW
      INTEGER OPTF,OPMSYL,OPTK(2),OPTA,ITENT,IYRTAR,OPTIMMI

      COMMON /INPUTS/ BIAS(2,I1SYR:IENDYR),NEED(2,0:IENDYR+3),NEEDF(2),
     +        CRATIO(2),
     +        ABOMUL,BIASC,ISIGHT(2,-1:IENDYR),NTRIAL,OPTB,
     +        IFREQ(2),
     +        ICAT1,ICATB, NEVENT,EVENTS,ISUR1,OPTHC
      DOUBLE PRECISION BIAS,NEED,NEEDF,CRATIO,BIASC,ABOMUL
      INTEGER ISIGHT,NTRIAL,OPTB,IFREQ,ICAT1,ICATB,NEVENT,EVENTS,
     +        ISUR1,OPTHC
C
      END MODULE DECL_SETUP

C --------------------------------------------------------------------

      MODULE DECL_CONVRS

      COMMON /CONVRS/ PTARGT,ADULTS,JUVS,K1,TRANSA
      DOUBLE PRECISION PTARGT(2),ADULTS,JUVS(2),K1(2)
      INTEGER TRANSA

      COMMON /PRIORS/ MSYLP1,MSYRP1,MSYLP2,MSYRP2,APARP,JMORTP,AMORTP,
     +                PREGNP,TPARP,
     +                TARGP1,TARGP2,CVADP1,CVADP2,CARRYP1,CARRYP2,
     +                BIASCP,RHOP,ILAGP,CVCADP,EXTRASP
      DOUBLE PRECISION MSYLP1(4),MSYRP1(4),MSYLP2(4),MSYRP2(4),
     +       APARP(4),JMORTP(4),AMORTP(4),
     +       PREGNP(4),TPARP(4),
     +       TARGP1(4),TARGP2(4),CVADP1(4),CVADP2(4),CARRYP1(4),
     +       CARRYP2(4),BIASCP(4),RHOP(4),ILAGP(4),CVCADP(4),
     +       EXTRASP(4)
      COMMON /PRIORC/ MSYLT1,MSYRT1,MSYLT2,MSYRT2,APART,JMORTT,AMORTT,
     +            PREGNT,TPART,
     +            TARGT1,TARGT2,CVADT1,CVADT2,CARRYT1,CARRYT2,BIASCT,
     +            RHOT,ILAGT,CVCADT,EXTRAST
      CHARACTER*1 MSYLT1,MSYRT1,MSYLT2,MSYRT2,APART,JMORTT,AMORTT,
     +            PREGNT,TPART,
     +            TARGT1,TARGT2,CVADT1,CVADT2,CARRYT1,CARRYT2,BIASCT,
     +            RHOT,ILAGT,CVCADT,EXTRAST
      COMMON / PRIORO / CORRELATEDN
      INTEGER CORRELATEDN


      END MODULE DECL_CONVRS

C --------------------------------------------------------------------

      MODULE DECL_DATVRS

      COMMON /DATVRS/ CV1EST,DOF,OPTDET,CVADD2,
     +                ISEED1,ISEED2,BIASB,SDB
      DOUBLE PRECISION CV1EST(2),DOF,CVADD2(2),BIASB(2),SDB(2)
      INTEGER OPTDET,ISEED1,ISEED2

      END MODULE DECL_DATVRS

C --------------------------------------------------------------------

      MODULE DECL_PCFG
      COMMON / PCFG / PHIHISTPCFG,PHIHISTSOUTH,PHIFUTPCFG,ABL,
     +      ProbFalseNeg,ProbFalsePos1,ProbFalsePos2,
     +      FracObs,ProbStrk,Reason,
     +      MonthHarvest,Landed,Strikes,StruckLost,PCFGDesignated,
     +      DeltaHistPCFG,DeltaHistSOUTH,DeltaFutPCFG
      DOUBLE PRECISION PHIHISTPCFG,PHIHISTSOUTH,PHIFUTPCFG, 
     +      ProbFalseNeg,ProbFalsePos1,ProbFalsePos2,
     +      FracObs,ProbStrk, PropPCFG
     +      DeltaHistPCFG,DeltaHistSOUTH,DeltaFutPCFG
      INTEGER Reason(-100:99),MonthHarvest, ABL(-100:99),
     +      Landed(-100:100),Strikes(-100:100),StruckLost(-100:100),
     +      PCFGDesignated(-100:100)
!      CATPCFG  
      END MODULE DECL_PCFG

C --------------------------------------------------------------------

      MODULE ALT_PCFG
! Variables introduced for modeling an alternating season Makah hunt 
! Note: Carryover option not currently implemented, but have 
!  place-holder variables here for developing code. 
! Added by John Brandon, Winter 2016
        real, parameter :: HCM = 0.4d0  ! Human caused mortality: ABL = PBR - HCM
        
      	LOGICAL winter_season, pcfg_strike
      	INTEGER mod_yr  ! modulus(x = 2) of year number: summer v winter season
      	real(kind = 8) :: stock_landings(1:2, -100:100) 
      	real(kind = 8) :: stock_strikes(1:2, -100:100)  	
        real(kind = 8) :: abl_d0(-100:99), pbr_d0(-100:99)  ! initialize floating point variables for limits
        real(kind = 8) :: PCFGDesignated_d0(-100:100)
        real(kind = 8) :: carryover_winter, carryover_summer 
        real(kind = 8) :: carryover_all  ! carryover not currently implemented
      END MODULE ALT_PCFG

C --------------------------------------------------------------------
C --------------------------------------------------------------------
      
      PROGRAM F2TST
C                                                     28 APRIL 2004
C   F2-TST Version 7: Control program for testing potential aboriginal
C                     subsistence management procedures, Fishery type 2.
C
C     Recent updates:                                        date
C     Automated conditioning            Jun 2000; completed 5Jan 01
C     Stochastic model corrected                            1Nov 00
C     Stochastic RY calculation rewritten                    Feb 01
C     Time lag added                                        5Jan 01
C     Print pregnancy data if DOCON=-1                       Feb 01
C     Random number 0.0 disallowed + SolveB corrected        Feb 01
C     Inertial model added                                   Feb 01
C     1st year of population projection, epidemics,
C       time varying & integrated parameters                 Mar 01
C     Correct integer selection from uniform distribution   5Jun 01
C     Additional stochastic parameters                       Jun 01
C     Gray whale conditioning                                Oct 01
C     Correction setting SUR(0) when M changes [BR10d&11]    Oct 01
C     Gray whale CVs                                         Jan 02
C     Alternative episodic event settings                    Sep 03
C     Correction to inertia trial conditioning               Apr 04
C     Modified to include PCFG                               May 11
C
C     The program generates the 'true' population using a series of
C     catches set by the SLC.  'Measurements' or estimates of the stock
C     size are generated, incorporating random noise, for input to the SLC.
C     
C     The management procedure (SLC) or Subsistence Limit Calculation is
C     a self-contained module called to set catch limits using abundance
C     estimates, past catches, need level and calf and mature ratios.
C
C     Notes:
C     The equation numbers refer to SC/53/4 Annex E Appendix 3B & C.
C     Define year 0 as the first year of management.  
C     Maturity is taken to mean age of first parturition throughout.
C     Time dependence in K must start AFTER 1st year of pop projection IYRPRJ
C
C PARAMETERS -----------------------------------------------------------
C
C  Population parameters
C     PMATF(I,IYR)  True mature population size for stock I at start of year IYR
C                   (prior to CATCHF(IYR) being removed).  Set in STKUPA
C     PSURV(I,IYR)  Size of surveyed population I in IYR
C                   = 1+ population (males+females).  Set in STKUPA
C     KDD(I,IYR)    Carrying capacity for density dependent component of
C                   population.  Set in SETKA (up to year 0) & SETVRS
C     CATCHM(I,IYR) & CATCHF(I,IYR)  True catch of males/females of stock I in year IYR
C     CRATIO(J)     Sex ratio of future catches (area J)
C     CATM(J,IYR) & CATF(J,IYR) Catch of males/females in area J in IYR, as passed to SLC.
C                   Setup in INIT.  Historic values in error if BIASC.ne.1
C     BIASC         If BIASC.NE.1 the SLC is not given the true premanagement catch
C     NYEAR         Number of years of management.  Read in. 1-100
C     PhiHISTPCFG   Historical probability of taking a PCFG whales in Dec - May (Default 0.203)
C     PhiHistSouth  Historical probability of taking a PCFG whales in South (Defauly 0.001)
C     IYRPRJ        Start of population projection on scale when management begins
C                   in year 0.  May be before 1st catch (stochastic model)
C                   or after 1st catch (Factor P)
C     INITYR        Year when population pristine. May be earlier (stochastic model)
C     OPTDD         Density dependence on:   0: 1+ population  1: mature population.
C     OPTF          Fishing pattern defining MSYR is uniform selectivity on: 0:1+ 1:mat.pop
C     OPMSYL        MSYL is defined on   0: 1+ population   1: mature population.
C     MSYL(I)       MSY Level (stock I). Read in. See OPMSYL for definition.
C     MSYR(I)       MSY rate (stock I).  Read in. See OPTF for definition.
C     Z(I)          Density dependent exponent (stock I), calculated from MSYL (in MSYPAR).
C     A1(I)         Resilience parameter (stock I) (set in MSYPAR).
C     A(I,IYR)      Resilience parameter for stock I in IYR. Set in SETKA (up to year 0) & SETVRS
C     MAT50(I)      Age of maturity (knife edge) for stock I. Set in RESET or conditioning
C                   (Actually age of 1st parturition = age of maturity+1)
C     SUR(I,A)      Survival rate of animals of stock I and age A. Sex independent. Set in RESET
C     FEC(I)        Fecundity for stock I. See full definition in MSYPAR
C     RREC   Ratio of pristine recruited population to number of age 0
C     NY            minimum interval between calvings (1:bowhead 0:gray)
C     PROBE(I,IYR)  % reduction in population if an epidemic occurs in IYR. Set in RESET.
C     EVENTS        % reduction in population if an epidemic occurs in future. Read in
C     NEVENT        No. of events (i.e. epidemics) occurring between yrs 1-50
C                   If NEVENT=3 3 events occur in yrs 1-75 with at least 2 in yrs 1-50.
C                   If NEVENT=5 10% reduction every 5 years (population 2 only)
C     OPTEE         Defines epidemic type. 0:Uniform thru all ages  1:Youngest 1st. Read in
C     OPTHC         If OPTHC=1 change historic catch ratio 1600-1964 to be 50:50.
C                           =2 apply catch bias to total catch from 1940 to CATB GR08a
C     ILAG          Time lag in density dependence. Read in.
C     OPTK,OPTA,OPTM: options defining time dependence in K, A &/or M
C                     over manangement period only (except tent model)
C                    0: none  1: doubles  2: halves  3: sinusoidal  4: steps  5: tent
C     PREGTG(I)     Target maximum pregnancy rate for stock I (for conditioning) & in inertia model
C
C Survey estimate parameters:
C     SIGHT(J,IYR)  Absolute abundance estimate for area J in IYR  eg sightings.
C                   Set to -1 if no data available that year. Passed to SLC.
C     CVX(J,IYR)    Estimated CV of the sightings estimate
C                   Set to -1 if no data available.  Passed to SLC.
C     BIAS(J,IYR)   Bias of absolute abundance estimates in IYR. Set in INIT
C     OPTB          BIAS changes linearly (or is constant) unless OPTB = 1:sinusoidal
C     ISIGHT(J,IYR) =1 in years when sightings surveys to be run. Set in INIT
C                   1st survey made in year -1. If IFREQ=5 the 2nd is in year 4
C     CV1EST(J) = CV(est), the expectation value of CVX when
C                   P=ref.level (=0.6K for bowhead, 1968 value for gray). Read in.
C     OPTDET        Set on deterministic run  0: stochastic   1:deterministic
C     ISUR1          Year of first sightings survey.  Passed to SLC
C
C Need Parameters (in terms of number of whales)
C     NEED(I,IYR) Total need for years IYR to IYR+IQUOTA(I)-1 where a new strike
C                 limit is due in IYR. Set = -1 in other years.  Set in INIT.
C     IQUOTA(I)   No of years of quota = period for which strike limit is set for area I
C                  Strike limit is split equally to give annual value
C
C General
C     IYR    Current year
C     N      Current trial number  (1,NTRIAL)
C     NTRIAL Number of trials.  Read in.  1-400
C     IOUT   Output file
C     IN,IN4 Input files
C     REF    Reference number of run
C     DOCON  =0 if parameters read in; =-1 checking/printing nos. pregnant etc
C            =1 on conditioning run;   =2 on conditioning with extra printing;
C     ISEED1-4 Seeds for random number generators:
C            1: Set SIGHT   2: Set CVX   3 & 4: Set calf/mature ratios  5 PCFG
C
C DEFINITIONS ----------------------------------------------------------
C
      USE ARRAY_PAR
      USE DECL_STKVRS
      USE DECL_SETUP
      USE DECL_CONVRS
      USE DECL_PCFG
      USE DECL_DATVRS
      USE ALT_PCFG  ! Added by JRB. See comments in module.
C
      COMMON /SIMVRS/MSYLN(2,MXSIM),MSYRN(2,MXSIM),MAT50N(MXSIM),
     +               SURJN(2,MXSIM),SURN(MXSIM),CVADD(2,MXSIM),
     +               ARN(2,MXSIM),PREGTR(2,MXSIM),
     +               GAMLN(2,MXSIM),AGEJN(MXSIM),
     +               ISEEDS(MXSIM,5),ISEEN(MXSIM),EXTRAN(MXSIM)
      DOUBLE PRECISION MSYLN,MSYRN,MAT50N,SURJN,SURN,CVADD,ARN,
     +                PREGTR,GAMLN,EXTRAN
      INTEGER AGEJN,ISEEDS,ISEEN

      DOUBLE PRECISION CATCHQ,CATM(4,I1CYR:ICEND),CATF(4,I1CYR:ICEND),
     +       SIGHT(2,I1SYR:ICEND),CVX(2,I1SYR:ICEND),
     +       NEEDI,NVAL,PHIT(2),NEWQ,NEWQ2,
     +       MSYL1(2),MSYLM(2),MSYR1(2),MSYRM(2),MSYLE(2),MSYRE(2), 
     +       KN(2,MXSIM),PN(2,MXSIM),PTARGN(2,MXSIM),
     +       CCONST1,CCONST2,TST0,TST1,TST2,ABLV,LASTPCFGNEED,
     +       KNObs(2),
     +       TOT1,TOT2,ProbFalseNegUsed,ProbFalsePosUsed
      INTEGER NYEAR,IYR,I,N,IOUT,IN,IN2,IN3,IN4,IYRQ,IS,
     +        IYRMAN,NSIM,OK,IQUOTA(2),ISEED3,ISEED4,ISEED5,
     +        PCFGCASE,BLOCKSTART
      CHARACTER REF*10,DESC*80,PROG*9,PARFIL*12,MATFIL*12,MANAGE*1
      LOGICAL StruckAndLost
      EXTERNAL RAN1
      DOUBLE PRECISION RAN1,MA5(55)
      SAVE MA5,INEXT5,INEXTP5

      DATA IOUT/8/,IN/7/,IN2/10/,IN3/11/,IN4/12/
      OPEN (IN,FILE='COPY.DAT',STATUS='OLD')
C
C INITIALISATION -------------------------------------------------------
C
      PROG = 'F2TST: V9'

C 	Print filler lines before next trial (if running a batch)
			print *, "-----------------------------------------"
			print *, "--------------------"
			print *, "------------"                
			print *, "------"                
			print *, "--"

C 	Read input file header.       
      READ (IN,'(T37,A /A/2(/45X,A))') REF,DESC,PARFIL,MATFIL

C      
C     Read management type & whether conditioning. Allocate output file.
C     If MANAGE=Z all catches set to zero & results output to AW-RESZ
C     If MANAGE=I a zero strike limit is set for both areas & results output to AW-RESI {There are incidental catches}
C     If MANAGE=C constant strike limit is set = CCONST
      OPEN (16,FILE='MANAGE.DAT',STATUS='OLD')
      READ (16,'(A1/I1/F4.0/F4.0/I4/I4)') MANAGE, DOCON, 
     +    CCONST1, CCONST2, PCFGCASE
      CLOSE(16) 

C 	DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     print *, "About to open AW-RES file"
			print *, ""
			print *, "CATCH TREATMENT OPTION (MANAGE):  ", MANAGE
			
C     print *, "DOCON", DOCON
C     print *, "CCONST1", CCONST1
C     print *, "CCONST2", CCONST2                  
C     print *, "PCFGCASE", PCFGCASE      
         	
      OPEN (IOUT,FILE='AW-RES'// MANAGE,STATUS='UNKNOWN')
      WRITE (IOUT,'(/4A/1X,A/)')' CASE: ',REF,'     PROGRAM: ',PROG,DESC
C
C     Open Catch file (CATCHB.DAT or CATCHG.DAT)
      OPEN (IN3,FILE='CATCH'//REF(1:1)//'.DAT',STATUS='OLD')
			
C     Call INIT to read in / set up parameters.
C 		Including MonthHarvest
      CALL INIT (CATM,CATF,SIGHT,CVX,NYEAR,IYRMAN,IQUOTA)

C     Adjust for month -----------------------------------------------------
! DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Note:
!  The "MonthHarvest" (i.e. summer or winter / spring) will ultimately depend
!   on the year for the alternating season option. 
!  Have left PCFGCASE = 1 for input files, but revised code that sets MonthHarvest.
!  Revisions include a revised set of conditional if/else statements 
!    inside the loop over years (IYR). The revised code also allows NEED 
!    to alternate between even / odd years.
!  With respect to MonthHarvest: this variable is used to allocate struck and lost 
!    whales to stock, based on assumption of availability.
!  Also note, PCFGCASE corresponds to the SLA variants in 2012 -- 
!    see JCRM 2013 v14 (Suppl.) Annex E Appendix 2 Table 5 (p 161).
! PCFGCASE:
!		1, 4, or 7] December to April (winter hunt); MonthHarvest = 4
!		2, 5, or 8] May (summer hunt); MonthHarvest = 5 ; PhiFutPCFG = Trial Specified
!		21 - 26] MonthHarvest = 99; used for splitting takes between winter and May during 2012 - deprecated.
!		3, 6, 9, 11] MonthHarvest = 5; PhiFutPCFG = 1.0; DeltaFutPCFG = (200.0/PhiFutPCFG  - 200.0)/20000.0 
      IF (PCFGCASE.EQ.1.OR.PCFGCASE.EQ.4.OR.PCFGCASE.EQ.7.
     +    OR.PCFGCASE.EQ.10) MonthHarvest = 4
      IF (PCFGCASE.EQ.2.OR.PCFGCASE.EQ.5.OR.PCFGCASE.EQ.8) 
     +    MonthHarvest = 5
      IF (PCFGCASE.GE.21.AND.PCFGCASE.LE.26) 
     +    MonthHarvest = 99
! These SLA variants assume 100% PCFG availability (PhiFutPCFG) in May     
      IF (PCFGCASE.EQ.3.OR.PCFGCASE.EQ.6.OR.PCFGCASE.EQ.9.
     +    OR.PCFGCASE.EQ.11) THEN
       MonthHarvest = 5
       PhiFutPCFG = 1.000
       DeltaFutPCFG   = (200.0/PhiFutPCFG  - 200.0)/20000.0  ! Why not just DeltaFutPCFG = 0.d0 ?
! Noting DeltaFutPCFG "...is the relative probability of harvesting a PCFG vs north animal
! had the sizes of the two populations been the same." Eqn A3.1 and A3.2 (p 156 Appendix 2 JCRM 2013 Suppl)
      ENDIF

C
C     Reset the future reference incidental catch if MANAGE = I
      AVEINCCATF = AVEINCCAT
      IF (MANAGE.EQ."Z") AVEINCCATF = 0
			
C     Call PNTOUT to copy to output
      CALL PNTOUT (IOUT,NYEAR,IQUOTA,PARFIL,IYRMAN)
C
C     Set up extra output files if printing extra info
      IF (DOCON.EQ.-1) THEN
        OPEN (30,FILE='PREG.OUT',STATUS='UNKNOWN')
        WRITE (30,*) REF,MANAGE
        OPEN (31,FILE='TIMEPAR.OUT',STATUS='UNKNOWN')
        WRITE (31,*) REF,MANAGE
      ENDIF

C     Read in random number seeds for all trials : store in ISEEDS
      OPEN (IN2,FILE='RANDOM.NUM',STATUS='OLD')
      READ (IN2,'((5I8))') ((ISEEDS(N,I),I=1,5),N=1,NTRIAL)
      CLOSE (IN2)
C
C
C CONDITIONING -------------------------------------------------------
C
C     Do Conditioning - it is run separately from trials.
C                       MATFIL=Conditioning data file		
      IF (DOCON.GE.1) THEN
        WRITE(*,*) "CONDITIONING"
        OPEN(UNIT=14,FILE=MATFIL,STATUS='OLD')
        OPEN(UNIT=13,FILE='CONDIT.TMP',STATUS='UNKNOWN')
        IF (REF(1:1).EQ.'G') THEN
          CALL CONDG(IYRMAN,REF,PARFIL)
        ELSE
          STOP ' **** ERROR: Unknown REFerence type'
        END IF
        WRITE(*,*) "DONE"
        STOP
      ENDIF  
C
C READ CONDITIONING RESULTS --------------------------------------------
C
C     Otherwise not a conditioning run: read parameter file name & parameters
			
C DEBUGGING JRB
			print *, ""
			print *, "PAR FILE = ", PARFIL
			print *, ""			
			
      OPEN (IN4,FILE=PARFIL,STATUS='OLD')
      READ (IN4,*)
      DO 5 N=1,NTRIAL
        READ(IN4,*) AGEJN(N),MAT50N(N),SURN(N),
     +   (MSYLN(IS,N),MSYRN(IS,N),IS=1,2),
     +   (SURJN(IS,N),IS=1,2),
     +   (KN(IS,N),PN(IS,N),PTARGN(IS,N),IS=1,2),
     +   ISEEN(N),(PREGTR(IS,N),IS=1,2),(CVADD(IS,N),IS=1,2),
     +   TST0,TST1,TST2,EXTRAN(N)
5     CONTINUE
      CLOSE (IN4)
C
C     Call ISLC to initialise management routine 
      CALL ISLC(CATM, CATF, SIGHT, CVX,INITYR)
      
C
C
C TRIALS BEGIN ---------------------------------------------------------
C

! DEBUGGING JRB
      print *, "Starting Trials"
      print *, ""
			
      DO 200 N = 1,NTRIAL

C Display progress bar for simulations
        call progress_bar(N, NTRIAL)     						
				
C
C       Reset parameters and reseed the random number generators
        CALL RESET(N,NYEAR,SIGHT,CVX,CATM,CATF,ISEED3,ISEED4,
     +      ISEED5)
C
C       If wanted, print out info on pregnancy rates and calves
        IF (DOCON.EQ.-1) THEN
          CLOSE (30)
          OPEN (30,FILE='PREG.OUT',STATUS='OLD',ACCESS='APPEND')
          WRITE (30,'(A4,5A9,A4,I4)')
     +      'Yr','Calves','MatFem','AvailF','DetPreg', 'StocPrg','N=',N
          CLOSE (31)
          OPEN (31,FILE='TIMEPAR.OUT',STATUS='OLD',ACCESS='APPEND')
          WRITE (31,'(A4,5A9,I4)')'Yr','K','A','M(Ad)','Bias','PR  N=',N
        ENDIF
C
C       Age structured model.  
C       MSYPAR sets A1 & Z, SETKA sets PMATF & PSURV for IYR=IYRPRJ,-1
C       MSYP2 sets MSYL and MSYR in terms of other population components
        DO 201 IS = 1, 2
          CALL MSYPAR(OK,IS)
          CALL MSYP2 (MSYL1,MSYLM,MSYLE,MSYR1,MSYRM,MSYRE,IS)
201     CONTINUE    
     
        DO 1011 IS = 1, 2
          CALL SETKA (KN(IS,N),PN(IS,N),PHIT(IS),OK,IS,IYRMAN)
1011    CONTINUE         
C
C       Compute the expected survey estimates
        CALL ComputePSURVObs(INITYR,FracObs)
        DO IYR = IYRPRJ, -1
          CALL ComputePSURVObs(IYR,FracObs)
        ENDDO

        KNObs(1) = PSurvOBS(1,INITYR)
        KNObs(2) = PSurvOBS(2,INITYR)
C
C       Set up time dependent parameters
        CALL SETVRS(NYEAR,ISEED3)
C
C       Special feature if there are future episodic events
        IF (NEVENT.NE.0) CALL FINDEXTRAS(IYRMAN)
C
C       Update to year 0
        IYR=-1
C       Call DATGEN to reset parameters and, if ISIGHT(-1)=1 to generate
C       the last premanagement survey estimate, SIGHT(-1), & its CV
        DO IS = 1, 2
          CALL DATGEN (SIGHT(IS,IYR),CVX(IS,IYR),1,
     +       PSURVObs(IS,IYR),KNObs(IS),BIAS(IS,IYR),ISIGHT(IS,IYR),
     +       PHIT(IS),IS, N, IYR)
        ENDDO     

C       Advance population to IYR=0
        DO IS = 1, 2
          CALL STKUPA (IYR,CATCHM(IS,IYR),CATCHF(IS,IYR),OK,IS,IYRMAN)
        ENDDO     
    
        CALL ComputePSURVObs(0,FracObs)
				
C       Check that target was achieved and print out new parameters
        IF (ABS(PHIT(1)-PTARGN(1,N)).GT.8.D0) THEN
          PRINT *, ""        
        	PRINT *, "Trial Number: ", N
          WRITE(*,*) ' Target popln 1 not hit',PHIT(1),PTARGN(1,N)
        ENDIF	
        
        IF (ABS(PHIT(2)-PTARGN(2,N)).GT.8.D0) THEN
          PRINT *, ""        
	        PRINT *, "Trial Number: ", N
          WRITE(*,*) ' Target popln 2 not hit',PHIT(2),PTARGN(2,N)
        ENDIF
				     
        WRITE (IOUT,'(/''Trial:'',I4,A10,4I8 / 12X,
     +   2(''MSY'',3(A,2F8.5,4X)))') N,'Seeds:',
     +      (ISEEDS(N,I),I=1,4),'L 1+:',MSYL1,'Mat:',
     +      MSYLM,'Exp:',MSYLE,'R 1+:',MSYR1,'Mat:',MSYRM,'Exp:',MSYRE
        WRITE (IOUT,'(2(2(A,I2),6(1X,A,F6.4)))')  
     +        (' AM:',NINT(MAT50N(N)),
     +        ' AJ:',AGEJN(N),  'SJ:',SURJN(IS,N),'SA:',SURN(N),
     +        'FMax:',PREGTR(IS,N),
     +        'CVAdd:',CVADD(IS,N),'FEC:',FEC(IS),'F0:',F0(IS),
     +        IS = 1,2)
				
C       Reinitialise the management routine (if required)
        NSIM = N
        CALL RSLC (ICAT1,ISUR1,NSIM)
C					
C Loop over years for this trial ----------------------------------------------------------					
        DO 100 IYR = 0,NYEAR-1		
          ! Initialize PCFG limits at start of year
          ABLV = 0.d0
          ABL(IYR) = 0.d0
          abl_d0(IYR) = 0.d0			
          		
! NORTH AREA QUOTA					
C ----------------------------------------------------------------------------------------					
C         Test if a new quota is needed for the northern area, NEWQ, is to be set 
C         this year for use in IYRQ (if NEED(1,IYR+3)>0, IYR=0 or after strategic surveys) 					
          IF (NEED(1,IYR+3).GT.0.D0 .OR. IYR.EQ.0) THEN
            IYRQ = 0
            
            IF (NEED(1,IYR+3).GT.0.D0) IYRQ = IYR+3
            
            NEEDI = NEED(1,IYRQ)
C
C           Call SLC (subsistence limit calculation) to set the block quota.
            IF (MANAGE.EQ.' ') THEN
              NVAL = NEEDI / DBLE(IQUOTA(1))
              CALL SLCG (CATM,CATF,SIGHT,CVX,IYRQ,ICAT1,ISUR1,
     +               NEEDI, NEWQ,NSIM,IQUOTA(1))
             NEWQ = NEWQ / DBLE(IQUOTA(1))
C            Ensure 0 <= NEWQ <= NEED
             NEWQ = MAX(0.D0, MIN(NEWQ,NVAL))
            ELSE IF (MANAGE.EQ.'Z') THEN
              NEWQ = 0.D0
            ELSE IF (MANAGE.EQ.'I') THEN
              NEWQ = 0.D0
            ELSE IF (MANAGE.EQ.'C') THEN
              NEWQ = CCONST1
            ELSE
              STOP 'Unknown management option - not blank, Z or H'
            ENDIF
          ENDIF

!  Determine if this is a winter/spring or summer hunt. Set variables accordingly. 
!		Determine if this year is a winter or summer hunt season (alternating between years).
! 	+  Set NEED equal to max strikes per season (2 for summer, 3 for winter)
!   +  During the 2012 IWC IR, the proposed hunting season had some overlap between 
!        winter / summer. The original code for that evaluation, assumed PCFG
!        availability, and whether a struck whale was PCFG, etc., changed
!        on the 1st of May (i.e. MonthHarvest = 5).

!    Determine if even or odd numbered year. i.e. winter or summer hunt.
          mod_yr = mod(iyr, 2)  ! Take modulus(2) of year. Returns zero or one.			

          if (mod_yr == 0) then     ! 
            winter_season = .TRUE.  ! Winter hunt (even numbered year)
            MonthHarvest = 4        ! Legacy from 2012 computations, but makes life easier below. 
            NEED(2,IYR) = 3         ! PCFG Need = max Winter strikes
!           PCFGCASE = 1            
!           PhiFutFCFG = PhiFutPCFG  ! PCFG availability = Trial specified       
!           DeltaFutPCFG = DeltaFutPCFG
          else 
            winter_season = .FALSE.  ! Summer hunt
            MonthHarvest = 5         ! MonthHarvest is used to allocate strikes to stock
            NEED(2,IYR) = 2          ! PCFG Need = max Summer strikes
!           PCFGCASE = 3             
!           PhiFutPCFG = 1.d0        ! PCFG availability = 100%         
!           DeltaFutPCFG = (200.d0 / PhiFutPCFG - 200.d0) / 20000.d0
          end if
					
C ----------------------------------------------------------------------------------------				
! 	PCFG QUOTA	
C         Test if a new quota is needed for the PFCG area, NEWQ2, is to be set 
C         this year for use in IYRQ (if NEED(2,IYR)>0, IYR=0) 
          IF (NEED(2,IYR).GT.0.D0 .OR. IYR.EQ.0) THEN
          
            IF (NEED(2,IYR).GT.0.D0) IYRQ = IYR
            
            NEEDI = NEED(2,IYRQ)

C
C           Call SLC (subsistence limit calculation) to set the block quota.		
            IF (MANAGE.EQ.' ') THEN            ! Base case 
              NVAL = NEEDI / DBLE(IQUOTA(2))   ! At present, block iquota (= 1) set every year for PCFG

! Strike Limit Control (SLC) returns variable ABLV for ABL value 
! ABL equal to PBR at this stage. 
! ABL is modified again further below (e.g. HCM subtracted) 							 
              CALL SLCPFCG (CATM,CATF,SIGHT,CVX,IYRQ,ICAT1,ISUR1,
     +               NEEDI, NEWQ2,NSIM,IQUOTA(1),ABLV)

!   Subtract incidental human caused mortality from PBR for PCFG
              ABLV = ABLV - HCM
              
C             Ensure 0 <= NEWQ <= NEED 
             
! The NOAA algorithm treats ABL differently than the Makah Tribe's proposed  SLA 
! There is a non-binary component in NOAA's SLA:
! During winter/spring whaling, if a whale is struck AND
!   lost, a value of 0.3d0 is counted towards the ABL.
! In the IWC algorithm, 0.3d0 -> p in a bernoulli trial.
! 
              abl_d0(IYR) = ABLV  ! Stash decimal value before rounded down in next line
              ABLV = INT(MAX(0.D0, MIN(ABLV,NVAL)))
	            
            ELSE IF (MANAGE.EQ.'Z') THEN
              ABLV = 0.D0
	      abl_d0(IYR) = 0.d0
            ELSE IF (MANAGE.EQ.'I') THEN
              ABLV = 0.D0
	      abl_d0(IYR) = 0.d0
            ELSE IF (MANAGE.EQ.'C') THEN
              ABLV = CCONST2
            ELSE
              STOP 'Unknown management option - not blank, Z or H'
            ENDIF
          ENDIF					
C
C         DATGEN sets abundance estimate and CV in IYR if ISIGHT=1: Section B1 
! Where Section B1 refers to the operating model description in the IWC AWMP reports.
! Data generation has been updated for the NOAA analysis. The difference being, the IWC IR
!   used abundance estimates generated from the OR-SVI survey sub-area, whereas NOAA
!   bases their algorithm on the larger putative feeding area between NCA-SEAK.
! The *.DDD input files with individual trial specifications have 
!   been updated to make this change by editing the "BIAS" parameter. 
! The update was implemented by the ./R/DatGen_NPCFG.R script, which contains
!   code to batch the reproducible editing process for all *.DDD trial files.
!   i.e. bias is set to zero for all trial files, such that estimates are based on 
!   100% of PCFG abundance. 
          DO IS = 1, 2
            CALL DATGEN (SIGHT(IS,IYR),CVX(IS,IYR),0,
     +        PSURVObs(IS,IYR),KNObs(IS),BIAS(IS,IYR),ISIGHT(IS,IYR),
     +        PHIT(IS),IS, N, IYR)
          END DO     

! NORTH: Allocate quota over time block 					
C         If NEED>0 the new quota will be applied this year for IQUOTA years
          IF (NEED(1,IYR).GT.0D0) THEN
            CATCHQ = NEWQ
C          Store values for next IQUOTA(1) years
            DO I = IYR,MIN(IYR+IQUOTA(1)-1,NYEAR-1)
              CATM(1,I) = CATCHQ * CRATIO(1)
              CATF(1,I) = CATCHQ - CATM(1,I)
              CATCHM(1,I) = CATM(1,I)
              CATCHF(1,I) = CATF(1,I)
            END DO
          ENDIF

! PCFG: Allocate quota over time block					
C         If NEED>0 the new quota will be applied this year for IQUOTA(2) years
          IF (NEED(2,IYR).GT.0D0) THEN
            LASTPCFGNEED = NINT(NEED(2,IYR)/FLOAT(IQUOTA(2)))  ! NINT Rounds to nearest number 
C          Store values for next IQUOTA(2) years
            DO I=IYR,MIN(IYR+IQUOTA(2)-1,NYEAR-1)
              ABL(I) = ABLV  ! ABLV has been rounded down to an integer at this point
            END DO
          ENDIF
C
C         Specify the probability of false negatives. If surveys only occur every
C         five years we need to adjust this probability
          ProbFalseNegUsed = ProbFalseNeg
          IF (IFREQ(2).NE.1) THEN
            TOT1 = 0.0001
            TOT2 = 0.0001
            DO L = 1,MaxAge
               TOT1 = TOT1 + RF(2,L)+RM(2,L)+UNRM(2,L)+UNRF(2,L)
               IF (L.GE.IFREQ(2)) THEN
                 TOT2 = TOT2 + RF(2,L)+RM(2,L)+UNRM(2,L)+UNRF(2,L)
               ENDIF
            END DO           
            ProbFalseNegUsed = 1.0 - TOT2 / TOT1
          ENDIF
C
C         Specify the probability of false positives.
          ProbFalsePosUsed = ProbFalsePos1 + 
     +     IYR*(ProbFalsePos2-ProbFalsePos1)/99.0d0
C
C         Special feature implement the ABL
! NEXP looks approximately equal to 1+ stock abundance (see file = AW-RES)
! What is this? Catching PCFG extinctions? -JRB
          IF (NEXP(2,IYR) + DeltaFutPCFG * NEXP(1,IYR).GT.0) THEN
            PropPCFG = NEXP(2,IYR) / 
     +        (NEXP(2,IYR) + DeltaFutPCFG * NEXP(1,IYR))
          ELSE
            propPCFG = 0
            GOTO 555  ! Exit looping over strikes
          ENDIF 
C
C         Set the various counters to zero
          CATPCFG = 0
          CATNORTH = 0
          Landed(IYR) = 0
          Strikes(IYR) = 0
          StruckLost(IYR) = 0
          PCFGDesignated(IYR) = 0   
          PCFGDesignated_d0(IYR) = 0.d0  ! Keep track of fractional 0.3 PCFG strikes for NOAA   
          stock_landings(1, IYR) = 0.d0
          stock_landings(2, IYR) = 0.d0 
          stock_strikes(1, IYR) = 0.d0                     
          stock_strikes(2, IYR) = 0.d0           
          
556       CONTINUE      ! -----------------------------------------------------

!  Check limits before a strike is allowed 
!
!  During 2012 IWC IR there was a limit to struck and 
!  lost in the hunt plan. This is not true of the NOAA Alternating Season
!  SLA; the limit is on the number of strikes (irregardless of lost or landed). 
!  Moreover, the max strike limit differs between seasonal hunts: winter/spring (3 strikes)
!  vs summer (2 strikes). The max strike limit for the current year is set above (see line ~594),
!  and stored in the NEED matrix. 
!  JRB

! REASON 0 --------------------------------------------------------------------
! Enforce minimum lower bound that ABL >= 1.0 before hunt can proceed
!	  if(MANAGE.EQ.' ') then
	    if(ABLV. LT. 1.d0) then
              Reason(IYR) = 0  ! If ABL is less than 1.0, no hunt allowed this year
              GOTO 555
            end if
!	  end if

! Error test
          if (Strikes(IYR) .GT. NEED(2, IYR)) then
            print *, "Strikes are greater than strike limit: Check code"
            call debug(N, IYR, "Should not be here")
            stop !
          end if

! REASONS 1 AND 2 ---------------------------------------------------------------	  
! Strike limit reached. 
! NOTE: Because NEED has been set equal to the max strike limit (3 for winter, 2 for summer), 
!       This condition effectively implements the NOAA strike limit -JRB 

          if (Strikes(IYR) .EQ. NEED(2, IYR)) then
            if (winter_season .neqv. .TRUE.) then  ! Check which season 
              Reason(IYR) = 1  ! Hit limit of 2 strikes (PCFG) during summer.  
              ! call debug(N, IYR, "Summer strikes equal to need.")  ! note debug() function -JRB
              GOTO 555         ! Proceed directly to start of next year's hunt.
            else
              Reason(IYR) = 2  ! Hit limit of 3 total strikes during winter
              GOTO 555
            end if
          end if

! REASON 3 -----------------------------------------------------------------------    
! If summer hunt, and (a) one strike has already been taken; and (b) ABL < 2.0, stop hunt
! A second strike can not be taken without total strikes being greater than the ABL
          if (winter_season .neqv. .true.) then
            if (Landed(IYR) .eq. 0) then
              if (PCFGDesignated_d0(IYR) .eq. 1.d0) then  ! For summer, PCFGDesignated(IYR) should equal Strikes(IYR)
                if (abl_d0(IYR) .lt. 2.d0) then 
                  Reason(IYR) = 3
                  GOTO 555  ! Proceed directly to start of next year's hunt.
                end if
              end if
            end if
         end if 

! REASON 4 -----------------------------------------------------------------------
! If winter/spring hunt, and strikes are within less than 1.d0 of ABL, don't allow more strikes.
! Next strike could, if PCFG struck and landed, count as 1.0 strike against ABL.
! Have made the assumption this is how it will be managed, following logic that first
!   strike not allowed if ABL < 1.0. 
         if (winter_season .eqv. .true.) then
           if (abl_d0(IYR) - PCFGDesignated_d0(IYR) .lt. 1.d0) then
             Reason(IYR) = 4
             GOTO 555
           end if
         end if
	
!!!!!!!!!!!!!!!!!
! TODO: Think can remove the conditional statement below, and
!   add a new conditional that catches "Reason 5", which indicates 1.0 PCFG landed in summer
!!!!!!!!!!!!!!!!!
	      
C
C         Stop harvest: PCFG Mortality Limit (ABL) has been reached
!  PCFGDesignated is set further below in the code, with an if/then statement conditional on StruckAndLost
!          IF (PCFGDesignated(IYR).GE.ABL(IYR)) THEN  ! PCFGDesignated set below
!            Reason(IYR) = 3  ! PCFG strikes hit ABL = PRB - HCM 
! Struck and lost assumed to be PCFG in summer hunts (2017)
! Struck and lost assumed 0.3 of a PCFG whale in winter hunts (2017) -- Need to check PCFGDesignated calculations  
           ! print *, "PCFGDesignated .EQ. ABL(IYR)" 
           ! call debug(N, IYR, "Check delta btwn seasonal availability")
!            GOTO 555  ! Exit looping over strikes
!          ENDIF 

C
C         Landing Block quota (TotalLands) exceeded?
!   Don't believe this is needed for 2017 alternating season plan, which is not limited 
!   by numbers of landed whales (only ABL and max strikes). Would still be good to keep
!   the tally of landed whales, but don't need the control statements for stopping
!   the hunt or keeping track of how many times hunt was stopped after landing XX whales. 
          BLOCKSTART = MAX(0,INT(IYR/5)*5)  ! Blockstart = IYR
          TotalLands = 0                    ! Re-initialize landings for this year of the 5yr block quota to zero.

! DEBUGGING ----------------------------------------------
C         if (IYR .LT. 7 .AND. N .LT. 2) then
C           print *, "IYR", IYR
C           print *, "BLOCKSTART", BLOCKSTART
C           print *, "TotalLands", TotalLands
C         endif
! DEBUGGING ----------------------------------------------

          DO IYR2 = BlockStart, IYR         ! Increment total numbers landed for this quota block 
            TotalLands = TotalLands + Landed(IYR2)  ! TotalLands for 2017 seasonal hunt should be annual sum
          END DO

! Error test
          IF (TotalLands.GT.20) THEN          ! 
            Reason(IYR) = 20                  ! Should not get here for 2017 alternating season plan
            call debug(N, IYR, "TotalLands > 20")   ! Catch error for 2017
            print *, "Error TotalLands > 20"
            stop
          ENDIF 


! LIMITS HAVE BEEN CHECKED -----------------------------------------------
!   Hunting may proceed - strike a whale and decide if it PCFG
          RAND = RAN1(ISEED5,MA5,INEXT5,INEXTP5)
          
C        Assign true catch to stock given PropPCFG (availability = ProbPCFG, i.e. ~0.30). 
C        Update probability of identifying as PCFG (and counting against ABL), 
C         given false negative and false positive ID probabilities.
          IF (RAND. LT. PropPCFG) THEN
            pcfg_strike = .TRUE.
            CATPCFG = CATPCFG + 1 
            stock_strikes(2, IYR) = stock_strikes(2, IYR) + 1.d0
            ProbDetect = (1-ProbFalseNegUsed)
          ELSE
            pcfg_strike = .FALSE.          
            CATNORTH = CATNORTH + 1 
            stock_strikes(1, IYR) = stock_strikes(1, IYR) + 1.d0            
            ProbDetect = ProbFalsePosUsed
          ENDIF

          Strikes(IYR) = Strikes(IYR) + 1  ! INCREMENT STRIKES FOR THIS YEAR

C
C         Decide if the whale was struck and lost									
C
          RAND = RAN1(ISEED5,MA5,INEXT5,INEXTP5)
					          
          IF (RAND. LT. ProbStrk) THEN
            StruckAndLost = .TRUE.
            StruckLost(IYR) = StruckLost(IYR) + 1
          ELSE
            StruckAndLost = .FALSE.
          ENDIF
          
! Tally landed whales by stock ---------------------------------------------------
          if (pcfg_strike .eqv. .false. .and.
     +        StruckAndLost .eqv. .false.) then 
              stock_landings(1, IYR) = stock_landings(1, IYR) + 1.d0  ! Non-PCFG strike
          endif  

          if (pcfg_strike .eqv. .true. .and.
     +        StruckAndLost .eqv. .false.) then
              stock_landings(2, IYR) = stock_landings(2, IYR) + 1.d0  ! PCFG strike
          endif                     
!
! Rules differs depend on whether the animals was struck and lost ----------------
!
          IF (StruckAndLost) THEN
!           Struck & Lost in Winter / Spring hunt: 
!             This treatment differs from 2012 Hunt Plan, which assumed
!           that struck and lost whales in winter / spring were only from the 
!           northern stock (in practice, the resulting north mortality rate ignored).
!             For the alternating season hunt plan, struck and lost whales during
!           winter/spring count against (PCFG Mortality Limit) ABL in proportion to PCFG availability,
!           PhiFutPCFG = 0.3 (base case). 
            if (winter_season .eqv. .TRUE.) then
               PCFGDesignated_d0(IYR) = PCFGDesignated_d0(IYR) + 0.3d0 
!              RAND = RAN1(ISEED5, MA5, INEXT5, INEXTP5)  ! No longer a Bernoulli trial, just set to 0.3 in line above -JRB
!              IF (RAND .LT. PhiFutPCFG) THEN
!                PCFGDesignated(IYR) = PCFGDesignated(IYR) + 1
!              ENDIF
            else
!           Struck & Lost in Summer hunt [each whale counts as one PCFG]
              PCFGDesignated(IYR) = PCFGDesignated(IYR) + 1
              PCFGDesignated_d0(IYR) = PCFGDesignated_d0(IYR) + 1.d0
            end if
C
C           Special case - ACCOUNTING FOR THE APRIL / MAY SPLIT (2012). CAN GET RID OF THIS FOR 2017 I THINK -JRB
!            IF (MonthHarvest.GE.99) THEN
!              call debug(N, IYR, "Error: MonthHarvest.GE.99")
!              NumBeforeMay = PCFGCASE-20
C             IF (StruckLost(Iyr).GT.NumBeforeMay) THEN
!              IF (Strikes(IYR). GT. NumBeforeMay) THEN
!                PCFGDesignated(IYR) = PCFGDesignated(IYR) + 1
!              ENDIF
!            ENDIF
C
          ELSE
C           Landed - now match against catalogue ---------------------------------
             Landed(IYR) = Landed(IYR) + 1

! Error Test
             if (Landed(IYR) .GT. NEED(2, IYR)) then
               print *, "---------"
               print *, "Landed greater than strike limit"
               print *, "IYR", IYR
               print *, "NEED(2, IYR)", NEED(2, IYR)
               print *, "Landed(IYR)", Landed(IYR)
               print *, "winter_season: ", winter_season
               print *, "---------"
               call debug(N, IYR, "line: 955")
            endif
               
C
C           Is Detected?
            if (winter_season .eqv. .TRUE.) then
            	RAND = RAN1(ISEED5,MA5,INEXT5,INEXTP5)
            	IF (RAND. LT. ProbDetect) THEN
               PCFGDesignated(IYR) = PCFGDesignated(IYR) + 1
               PCFGDesignated_d0(IYR) = PCFGDesignated_d0(IYR) + 1.d0
            	END IF
						else
							PCFGDesignated(IYR) = PCFGDesignated(IYR) + 1
							PCFGDesignated_d0(IYR) = PCFGDesignated_d0(IYR) + 1.d0
						end if
          ENDIF       
C          
C          
! REASONS 5 AND 6 ---------------------------------------------------------------	
C         Stop SUMMER harvest if any single whale is landed
C         This chunk replaced the 2012 code of five whales landed 
					IF (winter_season .NEQV. .TRUE.) THEN  ! Summer / Spring hunt year
            if (Landed(IYR) .eq. 1 .and. Strikes(IYR) .eq. 1) then 
              Reason(IYR) = 5
							GOTO 555
						end if
						if (Landed(IYR) .eq. 1 .and. Strikes(IYR) .eq. 2) then
							Reason(IYR) = 6 
							GOTO 555
						end if
          END IF
          
          GOTO 556  ! Strike taken, loop to next potential strike 

! DO WHILE (limit_reached .NEQV. .TRUE.)  ! GOTO statements act like DO WHILE
555       CONTINUE  ! Limit reached, proceed with accounting etc.        
	
C
C        Record total catches by sex that are taken in PCFG Area 
          CATM(2,IYR) = (CATPCFG+CATNORTH) * CRATIO(2)  ! CRATIO = Sex ratio (males)  of future catches (area J)
          CATF(2,IYR) = (CATPCFG+CATNORTH) * (1-CRATIO(2))
C
C        Add to the catches from the north stock
          CATCHM(1,IYR) = CATCHM(1,IYR) + CATNORTH * CRATIO(2)
          CATCHF(1,IYR) = CATCHF(1,IYR) + CATNORTH * (1-CRATIO(2))				          
C
C        Add to the catches from the PCFG stock
          CATCHM(2,IYR) = CATPCFG * CRATIO(2)
          CATCHF(2,IYR) = CATPCFG * (1-CRATIO(2))

C        Advance stock to next year, ie remove catch & set PMATF(IYR+1).
C        If quota CATCHQ > recruited population then it is reset in STKUPA
          DO IS = 1,2
            CALL STKUPA (IYR,CATCHM(IS,IYR),CATCHF(IS,IYR),OK,IS,IYRMAN)
          ENDDO                   


          CALL ComputePSURVObs(IYR+1,FracObs)         
C
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
          ! DEBUGGING JRB
!          print *,"IYR", IYR, "winter_season", winter_season, 
!     +           "Strikes(IYR)", Strikes(IYR),
!     +           "abl_d0(IYR)", abl_d0(IYR),
!     +           "PCFGDesignated_d0(IYR)", PCFGDesignated_d0(IYR),
!     +           "PCFGDesignated(IYR)", PCFGDesignated(IYR),
!     +           "ABL(IYR)", ABL(IYR),
!     +           "Landed(IYR)", Landed(IYR),
!     +           "Reason(IYR)", Reason(IYR)
					
100     CONTINUE  ! Loop to next year (IYR) for this trial ---------------------------					
!					print *, ""
!					print *, " -------------------- "					
!					print *, "DEBUGGING !!"
!					print *, " -------------------- "										
!					stop
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
								
C      Call REPORT to print out PMATF & CATCHM in year IYR
        CALL REPORT (IOUT,NYEAR,SIGHT,CVX,ISUR1,CATM,CATF,IYRMAN)
				
  200 CONTINUE  ! Loop to next trial (N) ---------------------------					

C      Print filler lines before next trial (more readable progress if running a batch)
			print *, ""
			print *, ""
			print *, "--"
			print *, "------"
			print *, "------------"
			print *, "--------------------"
			print *, "-----------------------------------------"

      CLOSE (IOUT)
      CLOSE (1)
      CLOSE (30)

      STOP
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
		subroutine progress_bar(iteration, maximum)
!
! Prints progress bar.
!
! Args: 
!     iteration - iteration number
!     maximum - total iterations
!
! (gfortran needs compilation parameter: -fbackslash)
!
! Maciej Żok, 2010 MIT License
! https://github.com/macie/fortran-libs
! 
! Modified by John Brandon in attempt to be more flexible, and extend progress
!  bar across more columns on screen
!
        implicit none
        integer :: iteration, maximum
        integer :: counter
        integer :: step, done
! introduced variables to make algorithm more flexible (e.g. previously hard-coded div = 10.0) JRB        
        integer :: idiv, ibin
        real :: div, bin  

		div = 5.d0
		idiv = nint(div)				
		bin = 20.d0
		ibin = nint(bin)
				
        step = nint(iteration * 100 / (1.0 * maximum))
        done = floor(step / div)  ! mark every 5% (done ranges from 1-20)

        do counter = 1, 100                    ! clear whole line - 100 chars
            write(6,'(a)',advance='no') '\b'  ! (\b - backslash)
        end do

        write(6,'(a)',advance='no') ' -> In progress... ['

        if (done .LE. 0) then
            do counter = 1, ibin
                write(6,'(a)',advance='no') '='
            end do
        else if ((done .GT. 0) .and. (done .LT. ibin)) then
            do counter = 1, done
                write(6,'(a)',advance='no') '#'
            end do
            do counter = done+1, ibin
                write(6,'(a)',advance='no') '='
            end do 
        else
            do counter = 1, ibin
                write(6,'(a)',advance='no') '#'
            end do
        end if
        write(6,'(a)',advance='no') '] '
        write(6,'(I3.1)',advance='no') step        
        write(6,'(a)',advance='no') '%'
	  end subroutine progress_bar

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE INIT (CATM,CATF,SIGHT,CVX,NYEAR,IYRMAN,IQUOTA)

C     INIT reads parameter file and initialises variables
C
      USE DECL_STKVRS
      USE DECL_SETUP
      USE DECL_CONVRS
      USE DECL_PCFG
      USE DECL_DATVRS
C
      DOUBLE PRECISION CATM(4,I1CYR:ICEND),CATF(4,I1CYR:ICEND),
     +        SIGHT(2,I1SYR:ICEND),CVX(2,I1SYR:ICEND),
     +        NINC,BINC,BIASH1(2),BIASH2(2),BIASF(2),
     +        CABOM1,CABOF1,CABOM2,CABOF2,CABOM3,CABOF3,CABOM4,CABOF4,
     +        CM1,CF1,CM2,CF2,CM3,CF3,CM4,CF4
      INTEGER NYEAR,IYR,I,IN,IN3,ISTART,IYRMAN,IQUOTA(2),IGSURV,NSURV,IS

      DATA IN/7/,IN3/11/
C
C     Read in model parameters/options, checking they are within allowed range
      READ (IN,'((44X,I4))') NTRIAL,NYEAR,ISTART,IYRPRJ,IYRMAN,
     +   IQUOTA(1),IQUOTA(2),
     +   MAXAGE,OPTF,OPMSYL,OPTDD,OPTK(1),OPTK(2),OPTA,
     +   OPTM(1),OPTM(2),OPTIMMI,
     +   ILAG,NEVENT,EVENTS,OPTEE,OPTHC,NY,IYRTAR
C
C     The model begins in actual year ISTART. IYRMAN = 1st year of management.
C     Re-scale to define year 0 as first year of management. 
C     IYRPRJ 1st year of population projection. May be before 1st catch
C         (stochastic model) or after 1st catch (Section Dx + Gray basecase)
      IYRPRJ = IYRPRJ - IYRMAN
      INITYR = ISTART - IYRMAN
      IYRTAR = IYRTAR - IYRMAN
      IF (MAXAGE.GE.MXAGE .OR. MAXAGE.LE.2) STOP 'MAXAGE out of range'
      IF (NYEAR .LT.0 .OR. NYEAR.GT.IENDYR) STOP 'NYEAR out of range'
      IF (INITYR.GT.IYRPRJ) STOP 'INITYR out of range'
      IF (-ILAG.LT.IYRPRJ)  STOP 'ILAG out of range'
      ITENT = -2
C     For tent K or A set ITENT = -51 = year tent shape change begins
      IF (OPTK(1).EQ.5 .OR. OPTA.EQ.5) ITENT = -51
      IF (IYRPRJ.LT.I1YR.OR.IYRPRJ.GE.ITENT)STOP 'IYRPRJ out of range'
C
      READ (IN,'((45X,F12.8))') CRATIO(1),CRATIO(2),BIASC,IMMIREF,
     + IMMIPOW
      READ (IN,'((44X,I4))') ICAT1,ICATB
      ICAT1 = ICAT1 - IYRMAN
      ICATB = ICATB - IYRMAN
      IF (ICAT1.LT.I1CYR .OR. ICAT1.GE.0) STOP 'ICAT1 out of range'
      READ (IN,'((45X,F9.2))') ABOMUL,AREC, NEED(1,0),NEEDF(1), 
     + NEED(2,0),NEEDF(2)
      READ (IN,'((45X,F9.5))') PhiHistPCFG,PhiHistSOUTH,PhiFutPCFG,
     +     ProbFalseNeg,ProbFalsePos1,ProbFalsePos2,FracObs,ProbStrk
      DeltaHistPCFG  = (200.0/PhiHistPCFG - 200.0)/20000.0
      DeltaHistSOUTH = (200.0/PhiHistSOUTH - 200.0)/20000.0
      DeltaFutPCFG   = (200.0/PhiFutPCFG  - 200.0)/20000.0
      READ (IN,'((44X,I4))') MonthHarvest
C     
      READ (IN,'(/(45X,F9.2))') (AVEINCCAT(I),I=1,4)

C     Read in abundance data parameters
      READ(IN,'(/(44X,I4))') ISUR1,IGSURV,OPTDET,IFREQ(1),IFREQ(2),OPTB
      ISUR1 = ISUR1-IYRMAN
      IGSURV = IGSURV-IYRMAN
      IF (ISUR1.GE.0.OR.ISUR1.LT.I1SYR) STOP 'ERROR IN SURVEY YEAR'
      IF (IGSURV.LT.-1) STOP 'ERROR IN DATE OF 1ST GENERATED SURVEY'
      READ (IN,'((45X,F9.2))') 
     +     BIASH1(1),BIASH2(1),BIAS(1,25),BIASF(1),
     +     BIASH1(2),BIASH2(2),BIAS(2,25),BIASF(2)
      READ (IN,'((45X,F9.4))') (BIASB(I),SDB(I),I=1,2)     
      READ (IN,'((45X,F9.2))') DOF,CV1EST(1),CV1EST(2)
      READ (IN,*)
C
C *** PRIORS ***
C     Read in the formal priors (used for checking when not conditioning)
C     (Some may not be used)
      READ (IN,'((44X,I4))') CORRELATEDN
      CALL RPRIOR (MSYLT1,MSYLP1,IN)
      CALL RPRIOR (MSYLT2,MSYLP2,IN)
      CALL RPRIOR (MSYRT1,MSYRP1,IN)
      CALL RPRIOR (MSYRT2,MSYRP2,IN)
      CALL RPRIOR (APART,APARP,IN)
      CALL RPRIOR (JMORTT,JMORTP,IN)
      CALL RPRIOR (AMORTT,AMORTP,IN)
      CALL RPRIOR (PREGNT,PREGNP,IN)
      CALL RPRIOR (TPART,TPARP,IN)
      CALL RPRIOR (TARGT1,TARGP1,IN)
      CALL RPRIOR (TARGT2,TARGP2,IN)
      CALL RPRIOR (CVADT1,CVADP1,IN)
      CALL RPRIOR (CVADT2,CVADP2,IN)
      CALL RPRIOR (CARRYT1,CARRYP1,IN)
      CALL RPRIOR (CARRYT2,CARRYP2,IN)
      CALL RPRIOR (CVCADT,CVCADP,IN)
      CALL RPRIOR (EXTRAST,EXTRASP,IN)
      CLOSE (IN)
C
C *** CATCHES ***
      DO 20 IS = 1,4
       DO 20 IYR = I1CYR,ICAT1-1
        CATM(IS,IYR) = 0.D0
        CATF(IS,IYR) = 0.D0
   20 CONTINUE
      DO 21 IS = 1,2
       DO 21 IYR = I1YR,ICAT1-1
        CATCHM(IS,IYR) = 0.D0
        CATCHF(IS,IYR) = 0.D0
   21 CONTINUE
C     Read catches by sex into arrays given to the SLC [this ignores incidental catches]
      READ (IN3,*)
      DO 22 IYR = ICAT1,-1
C       1=North;2=PCFG (DEC-MAY); 3=PCFG*JUN-DEC);4=LAGOONS
        READ (IN3,*) I,CM1,CF1,CABOM1,CABOF1,CM2,CF2,CABOM2,CABOF2,
     +                 CM3,CF3,CABOM3,CABOF3,CM4,CF4,CABOM4,CABOF4
        CATM(1,IYR) = CM1 + CABOM1
        CATF(1,IYR) = CF1 + CABOF1
        CATM(2,IYR) = CM2 + CABOM2
        CATF(2,IYR) = CF2 + CABOF2
        CATM(3,IYR) = CM3 + CABOM3
        CATF(3,IYR) = CF3 + CABOF3
        CATM(4,IYR) = CM4 + CABOM4
        CATF(4,IYR) = CF4 + CABOF4
        IF (I-IYRMAN.NE.IYR) STOP 'ERROR ON READING CATCH FILE'

C       Historic catches up to year ICATB may be biased (unknown to SLC)
        IF (BIASC.NE.1.D0 .AND. IYR.LE.ICATB .AND. OPTHC.LE.1) THEN
          CM1 = CM1 * BIASC
          CF1 = CF1 * BIASC
          CM2 = CM2 * BIASC
          CF2 = CF2 * BIASC
          CM3 = CM3 * BIASC
          CF3 = CF3 * BIASC
          CM4 = CM4 * BIASC
          CF4 = CF4 * BIASC
        ENDIF

C       Copy to catch array for population projection
C       Any catches before IYRPRJ (=start of projection) are ignored
C       Apply aboriginal multiplier (not given to SLC)
C       North catches are: all North + some Dec-May + some South
        CATCHM(1,IYR) = (CM1 + ABOMUL*CABOM1)
     +                  + (1-PhiHistPCFG)*(CM2 + ABOMUL*CABOM2)
     +                  + (1-PhiHistSOUTH)*(CM4 + ABOMUL*CABOM4)
        CATCHF(1,IYR) = (CF1 + ABOMUL*CABOF1)
     +                  + (1-PhiHistPCFG)*(CF2 + ABOMUL*CABOF2)
     +                  + (1-PhiHistSOUTH)*(CF4 + ABOMUL*CABOF4)
C
C       South catches are: some Dec-May + all Jun-Nov + some South
        CATCHM(2,IYR) = PhiHistPCFG*(CM2 + ABOMUL*CABOM2)
     +                  +  (CM3 + ABOMUL*CABOM3)
     +                  +  PhiHistSOUTH*(CM4 + ABOMUL*CABOM4)
        CATCHF(2,IYR) = PhiHistPCFG*(CF2 + ABOMUL*CABOF2) 
     +                  +  (CF3 + ABOMUL*CABOF3)   
     +                  +  PhiHistSOUTH*(CF4 + ABOMUL*CABOF4)   
   22 CONTINUE
C
C     If OPTHC=1 change historic catch ratio 1600-1964 to 50:50 (unknown to SLC)
      IF (OPTHC.EQ.1)  THEN
        DO 24 IS = 1,2
         DO 24 IYR = ICAT1,(1964-IYRMAN)
          CATCHM(IS,IYR) = (CATCHM(IS,IYR)+CATCHF(IS,IYR)) * 0.5D0
          CATCHF(IS,IYR) = CATCHM(IS,IYR)
   24   CONTINUE
      ELSE IF (OPTHC.EQ.2) THEN
        DO 25 IS = 1,2
         DO 25 IYR = (1940-IYRMAN),ICATB
          CATCHM(IS,IYR) = CATCHM(IS,IYR) * BIASC
          CATCHF(IS,IYR) = CATCHF(IS,IYR) * BIASC
   25   CONTINUE
      ENDIF
C
C     Set historic bias array (BIAS): change linearly from BIASH1 to BIASH2
      DO 35 IS = 1,2
       BINC = (BIASH2(IS)-BIASH1(IS)) / DBLE(-ISUR1-1)
       BIAS(IS,ISUR1) = BIASH1(IS)
       DO 351 IYR = ISUR1+1,-1
        BIAS(IS,IYR) = BIAS(IS,IYR-1) + BINC
  351  CONTINUE
   35 CONTINUE 
C
C     The rest is not needed for conditioning run
      IF (DOCON.GE.1) RETURN
C
C *** ABUNDANCE ESTIMATES ***
C     Initialise SIGHT & CVX. 
      DO 40 IS = 1,2
       DO 40 IYR = I1SYR,-1
        SIGHT(IS,IYR) = -1.D0
        CVX(IS,IYR) = -1.D0
   40 CONTINUE

      READ (IN3,'(// I2 //)') NSURV
      
      DO 45 IYR = ISUR1,ISUR1+NSURV-1
        READ (IN3,*) I,SIGHT(1,IYR),CVX(1,IYR)
        IF (I-IYRMAN.NE.IYR) STOP 'ERROR(2) READING SURVEY YEAR'
   45 CONTINUE
   
C DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
C Commented out this write statement. Doesn't seem to add anything. 
C      WRITE(*,*) I,IGSURV+IYRMAN
      
      IF (I.GE.IGSURV+IYRMAN) STOP 'ERROR(4a) IN SURVEY YEAR'
      READ (IN3,'(// I2 //)') NSURV
      DO 46 IYR = ISUR1,ISUR1+NSURV-1
        READ (IN3,*) I,SIGHT(2,IYR),CVX(2,IYR)
        IF (I-IYRMAN.NE.IYR) STOP 'ERROR(2) READING SURVEY YEAR'
   46 CONTINUE
      CLOSE(IN3)
      
C DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
C Commented out this write statement. Doesn't seem to add anything. 
C      WRITE(*,*) I,IGSURV
      
      IF (I.GE.IGSURV+IYRMAN) STOP 'ERROR(4b) IN SURVEY YEAR'
C
C *** Initialise other variables:   ------------------------------------
C
C     Set ISIGHT=1 in survey years. IGSURV=1st generated survey
C     Survey every IFREQ yrs beginning 4yrs prior to a new strike limit
      DO 50 IS = 1,2
       DO 501 IYR=-1,IENDYR
        ISIGHT(IS,IYR) = 0
  501  CONTINUE
       ISIGHT(IS,IGSURV) = 1
       DO 502 IYR=IFREQ(IS)-4,NYEAR-2,IFREQ(IS)
        IF (IYR.GE.0) ISIGHT(IS,IYR) = 1
  502  CONTINUE
  50  CONTINUE  

C     Set annual NEED array: Linear
      DO 61 IS = 1,2
       NINC = (NEEDF(IS) - NEED(IS,0)) / DBLE(NYEAR)
       DO 60 IYR = 1,NYEAR-1
        NEED(IS,IYR)  = NEED(IS,IYR-1) + NINC
  60   CONTINUE
C
C      Now reset need array to total value for each IQUOTA year period.
C      NEED(IYR)=0 in other years (when there is no new strike limit)
       DO 70 IYR = 0,NYEAR-1,IQUOTA(IS)
        DO 71 I=IYR+1,IYR+IQUOTA(IS)-1
         NEED(IS,IYR) = NEED(IS,IYR)+NEED(IS,I)
         NEED(IS,I) = 0.D0
  71    CONTINUE         
  70   CONTINUE
C      Extend array so program doesn't fall over when testing NEED(IYR+4) in main
       IF (IS.EQ.1) THEN
        DO 72 I=0,3
         NEED(IS,NYEAR+I) = 0.D0
  72    CONTINUE
       ENDIF
  61  CONTINUE   

C     Set bias array (BIAS): changes linearly unless OPTB = 1:sinusoidal
C     BIAS in year 25 was read in
      DO 80 IS = 1,2
       BIAS(IS,0) = BIASH2(IS)
       IF (OPTB.EQ.0 .AND. BIASH2(IS).NE.BIAS(IS,25)) THEN
C       Avoid discontinuity at year 0: change linearly from BIASH2 to
C       BIAS(25) in yr 25.  Constant from year 25. See Div
        BINC = (BIAS(IS,25)-BIASH2(IS)) / DBLE(25)
        DO 801 IYR = 1,25
          BIAS(IS,IYR) = BIAS(IS,IYR-1) + BINC
 801    CONTINUE
        BINC = (BIASF(IS)-BIAS(IS,25)) / DBLE(NYEAR-26)
        DO 802 IYR = 26,NYEAR-1
          BIAS(IS,IYR) = BIAS(IS,IYR-1) + BINC
 802    CONTINUE
       ELSE IF (OPTB.EQ.0) THEN
        BINC = (BIASF(IS)-BIAS(IS,0)) / DBLE(NYEAR-1)
        DO 803 IYR = 1,NYEAR-1
          BIAS(IS,IYR) = BIAS(IS,IYR-1) + BINC
 803     CONTINUE
       ELSE
C       Cyclic bias.
        CALL TIMDEP(BIAS,BIAS(IS,0),0.d0,I1SYR,ICEND,0,NYEAR-1,3,IS)
       ENDIF

C DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
C       PRINT '('' Bias: '',F7.3 / 10(10F7.3 /) )',(BIAS(IS,I),I=-1,100)

  80  CONTINUE
C
      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE RESET (N,NYEAR,SIGHT,CVX,CATM,CATF,ISEED3,ISEED4,
     +   ISEED5)
C
C     RESET values for new trial

      USE DECL_STKVRS
      USE DECL_SETUP
      USE DECL_CONVRS
      USE DECL_DATVRS

      COMMON /SIMVRS/MSYLN(2,MXSIM),MSYRN(2,MXSIM),MAT50N(MXSIM),
     +               SURJN(2,MXSIM),SURN(MXSIM),CVADD(2,MXSIM),
     +               ARN(2,MXSIM),PREGTR(2,MXSIM),
     +               GAMLN(2,MXSIM),AGEJN(MXSIM),
     +               ISEEDS(MXSIM,5),ISEEN(MXSIM),EXTRAN(MXSIM)
      DOUBLE PRECISION MSYLN,MSYRN,MAT50N,SURJN,SURN,CVADD,ARN,
     +                PREGTR,GAMLN,EXTRAN
      INTEGER AGEJN,ISEEDS,ISEEN,ISEED3,ISEED4

      DOUBLE PRECISION SIGHT(2,I1SYR:ICEND),CVX(2,I1SYR:ICEND),
     +       SN(2),SJN(2),
     +       CATM(4,I1CYR:ICEND),CATF(4,I1CYR:ICEND)
      INTEGER N,NYEAR,IYR,AGEJ(2),J,IS,ISEED5

C     Set new negative values for the random number generator seeds -
C     the generator is reset whenever a negative seed is used.
      ISEED1 = ISEEDS(N,1)
      ISEED2 = ISEEDS(N,2)
      ISEED3 = ISEEDS(N,3)
      ISEED4 = ISEEDS(N,4)
      ISEED5 = ISEEDS(N,5)
C
C     Extract parameters and assign them to stock
      DO 1010 IS = 1,2
       CVADD2(IS) = CVADD(IS,N)*CVADD(IS,N)
       MSYL(IS) = MSYLN(IS,N)
       MSYR(IS) = MSYRN(IS,N)
       MAT50(IS) = MAT50N(N)
       SJN(IS) = SURJN(IS,N)
       SN(IS)  = SURN(N)
       AGEJ(IS)  = AGEJN(N)
       PREGTG(IS) = PREGTR(IS,N)
       IF (AGEJ(IS).GT.MAXAGE .OR. MAT50(IS).GT.MAXAGE) 
     +              STOP 'ERROR 1 in RESET'
C
C      Set survival rates {Eqn A1.2}
       DO 5 J = 0,AGEJ(IS)
        SUR(IS,J) = SJN(IS)
   5   CONTINUE
       DO 6 J = AGEJ(IS)+1,MAXAGE
        SUR(IS,J) = SN(IS)
   6   CONTINUE
       IF (AGEJ(IS).GT.0) SUR(IS,0) = SJN(IS)*SN(IS)
       STORESUR = SUR
C
C      Extract the extra mortality
       EXTRAS = EXTRAN(N)

C      If OPTM>0 set up increment for future changes to SUR array
       IF (OPTM(IS).GT.0) CALL SETSUR(0,AGEJ(IS),0,IS)
C   
1010  CONTINUE   
C
C      Reinitialise survey arrays which are passed to the SLC.
       DO 10 IYR=0,NYEAR-1
        DO 10 IS = 1,2
         SIGHT(IS,IYR) = -1.D0
         CVX(IS,IYR)   = -1.D0
   10  CONTINUE
C
C      Reinitialise catch arrays which are passed to the SLC.
       DO 11 IYR=0,NYEAR-1
        DO 11 J = 1,4
         CATM(J,IYR) = 0.D0
         CATF(J,IYR) = 0.D0
   11  CONTINUE
C
      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C
      SUBROUTINE SETVRS (NYEAR,ISEED3)
C
C     Reset parameters which are time dependent from year 0  (Dvi-ix)
C     (except the varying mortality rate which is done in STKUPA)

      USE DECL_STKVRS
      USE DECL_SETUP

      DOUBLE PRECISION MA(55),RAN1,A2(2)
      INTEGER NYEAR,IYR,INEXT,INXTP,ISEED3,ISEED,I,IS
      EXTERNAL RAN1

      DO 1010 IS = 1,2
       A2(IS) = A1(IS)/2.5d0
1010  CONTINUE

      DO 16 IYR = 0,NYEAR
       DO 16 IS = 1,2
        PROBE(IS,IYR) = 0
        KDD(IS,IYR) = KDD(IS,IYRPRJ)
        A(IS,IYR)   = A1(IS)
   16 CONTINUE

C     Update time dependent arrays
      DO 17 IS = 1,2 
       IF(OPTK(IS).NE.0) CALL TIMDEP(KDD,KDD(IS,0),  0.d0,I1YR,IENDY2,
     +                                   0,NYEAR,OPTK(IS),IS)
       IF(OPTA.NE.0) CALL TIMDEP(  A,  A(IS,0),A2(IS),I1YR,IENDY2,
     +                                   0,NYEAR,OPTA,IS)
17    CONTINUE      

C     Set PROBE: NEVENT years are generated in which an event occurs that reduces
C     the population by EVENTS%. Ensure different years are generated.
C     Events occur in years 1-50 except if NEVENT=3  2 occur from 1-50 & 1 from 1-75.
      ISEED = ISEED3
      IF (NEVENT.EQ.3) THEN
       DO 18 IS = 1,2
        NMORE = NEVENT
        IF (NEVENT.EQ.3) THEN
         IYR = MAX(1, NINT(RAN1(ISEED,MA,INEXT,INXTP)*75.D0+0.49999D0))
         IF (IS.EQ.1) PROBE(IS,IYR) = MAX(0,EVENTS)
         IF (IS.EQ.2) PROBE(IS,IYR) = EVENTS
         NMORE = 2
        END IF
        DO 21 I=1,NMORE
   20    IYR = MAX(1, NINT(RAN1(ISEED,MA,INEXT,INXTP)*50.D0+0.49999D0))
         IF (PROBE(IS,IYR).GT.0) GO TO 20
         IF (IS.EQ.1) PROBE(IS,IYR) = MAX(0,EVENTS)
         IF (IS.EQ.2) PROBE(IS,IYR) = EVENTS
   21   CONTINUE
        DO 22 IYR = NYEAR+1,IENDY2,25
         IF (IS.EQ.1) PROBE(IS,IYR) = MAX(0,EVENTS)
         IF (IS.EQ.2) PROBE(IS,IYR) = EVENTS
22      CONTINUE        
   18  CONTINUE      
      ENDIF
      IF (NEVENT.EQ.-3) THEN
       NMORE = NEVENT
       IYR = MAX(1, NINT(RAN1(ISEED,MA,INEXT,INXTP)*75.D0+0.49999D0))
       PROBE(1,IYR) = -1*EVENTS
       PROBE(2,IYR) = EVENTS
       NMORE = 2
       DO 31 I=1,NMORE
   30   IYR = MAX(1, NINT(RAN1(ISEED,MA,INEXT,INXTP)*50.D0+0.49999D0))
        IF (PROBE(1,IYR).NE.0) GO TO 30
        PROBE(1,IYR) = -1*EVENTS
        PROBE(2,IYR) = EVENTS
   31  CONTINUE
       DO 32 IYR = NYEAR+1,IENDY2,25
        PROBE(1,IYR) = -1*EVENTS
        PROBE(2,IYR) = EVENTS
32     CONTINUE        
      ENDIF
      IF (NEVENT.EQ.5) THEN
       DO 25 IYR = 5,IENDY2,5
        PROBE(2,IYR) = EVENTS
25     CONTINUE        
      ENDIF

      RETURN
      END

C ----------------------------------------------------------------------

      SUBROUTINE TIMDEP (X,X1,X2,ID1,ID2,I1,I2,OPT,IS)

      INTEGER ID1,ID2,I1,I2,OPT,IEND,IYR
      DOUBLE PRECISION X(2,ID1:ID2),X1,X2,XINC,PIFACT

C     Set time dependent array X as defined by OPT
C       1: doubles  2: halves   3: sinusoidal  4: steps  5: tent (2nd part)
C
C     X1=starting value; X2=step value if OPT=4; I1 & I2=start & end years

      IF (OPT.EQ.3) THEN
C       Cyclic parameter: Fig 1a. Mean in year 0, max (=*1.5) in year 40
        PIFACT = 3.141593D0 / 80.D0
        DO 10 IYR = I1+1,I2
          X(IS,IYR) = X1 * (1.0D0 + 0.5D0*SIN(PIFACT * IYR))
   10   CONTINUE

      ELSE IF (OPT.EQ.4) THEN
C       Steps
        DO 20 IYR = 33,65
          X(IS,IYR) = X2
   20   CONTINUE

      ELSE
C       OPT=1 (X doubles) or 2 (X halves) or 5 (tent: Fig1b)
        IEND = I2
        IF (OPT.EQ.5) THEN
C        Tent part 2: Fig 1b. X returns from *2 to original value over 50yrs
         IEND = I2/2
         X(IS,I1) = 2*X1
        ENDIF
        XINC = X1 / DBLE(IEND-I1)
        IF (OPT.NE.1) XINC = -XINC*0.5d0
        DO 30 IYR = I1+1,IEND
          X(IS,IYR) = X(IS,IYR-1) + XINC
   30   CONTINUE
      END IF

      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE ComputePSURVObs(IYR,FracObs)

C     COMPUTEURVOBS calculates the observations pertinent to surveys based on the
C     1+ estimates of biomass. 

      USE DECL_STKVRS
      
      DOUBLE PRECISION FracObs
C
C     Total abundance is observed in the survey in CA
      PSURVOBS(1,IYR) = PSURV(1,IYR) + PSURV(2,IYR)
C
C     The PFCG survey capture only a fraction of the northern stock
      PSURVOBS(2,IYR) = PSURV(2,IYR) + FracObs*PSURV(1,IYR)
C      
      RETURN
      END 

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE DATGEN(SIGHT,CVX,SETDAT,PSUR,KSUR,BIASI,ISGHT,
     +    REFP,IS, N, IYR)

      USE DECL_DATVRS

C     DATGEN sets absolute abundance estimate SIGHT(IYR) adding 
C     random noise to the surveyed (1+) population PSUR (set in STKUPA)
C     REFP = reference population level = Population in target year (PHIT).

      EXTERNAL RAN1,GAMMAZ,XNORM,POISSN
      DOUBLE PRECISION PSUR,KSUR,BIASI,RAN1,GAMMAZ,XNORM,SIGHT,CVX,
     +       POISSN,RANNO,EPS,W,CHISQ,CVESQ,BKCON(2),SIG(2),SIG2,
     +       ACON(2),BCON(2),
     +       THTASQ,ALPHSQ,BETASQ,ASQ,BSQ,MA1(55),MA2(55),ECV2,CV2,
     +       REFP,EPS2,BIASY,ETA
      INTEGER SETDAT,ISEED,INEXT1,INXTP1,INEXT2,INXTP2,ISGHT,IS
      INTEGER N, IYR      
C     ETA           Process error parameter.  If ETA = 0 there is no process error.

      SAVE BKCON,SIG,ACON,BCON,MA1,INEXT1,INXTP1,MA2,INEXT2,INXTP2
      PARAMETER (ASQ=0.02D0,BSQ=0.012D0)
      
C     On 1st call to routine or if CV or K change, set the constants
      IF (SETDAT.EQ.1) THEN   
       CVESQ = CV1EST(IS)*CV1EST(IS)

C      ETA = {CV(true)**2 - CV(est)**2}/(.1+.013/.6)                    {Eqn B1.8}
C      Use CV(est)**2 = THTASQ (ASQ + BSQ K/P) to give THTASQ           {Eqn B1.7}
       ETA = CVADD2(IS) / (0.1d0 + 0.013d0*KSUR/(MAX(REFP,1.0d0)))      {Eqn B1.8b}
       THTASQ = CVESQ / (ASQ + BSQ*KSUR/MAX(REFP,1.0d0))                {Eqn B1.7}

C      CV(true)**2= ALPHSQ +BETASQ K/P                                  {Eqn B1.5}
C      where ALPHSQ =THTASQ*ASQ +.1*ETA & BETASQ =THTASQ*BSQ +.013ETA   {Eqn B1.9}
       ALPHSQ = THTASQ * ASQ + ETA*0.1D0
       BETASQ = THTASQ * BSQ + ETA*0.013D0

       BKCON(IS) = BETASQ * KSUR
       ACON(IS) = THTASQ * ASQ
       BCON(IS) = THTASQ * BSQ / BETASQ
C      SIG = sd of lognormal component, used to set EPS                 {Eqn B1.3}
       SIG(IS) = SQRT(LOG(1.D0 + ALPHSQ))
 
      ENDIF

C     Set EPS, BIASY, W & SEEDS to update random generators even if no survey

C     XNORM(SIG,MEAN,ISEEDi,...) produces a random number from a normal
C     distribution N [MEAN,SIG**2] using the ith random generator.
C          (if MEAN = 0,  XNORM (SIG,0) = SIG * XNORM (1,0) )
      EPS = SIG(IS) * XNORM (1.D0,0.D0,ISEED1,MA1,INEXT1,INXTP1)

! DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
C     IF(N.EQ.60.AND.IYR.GE.96)THEN
C     	PRINT *,""      
C			PRINT *, "ABOUT TO CALL FIRST RAN1()", N, IYR
C     END IF            
      
      RANNO = RAN1(ISEED1,MA1,INEXT1,INXTP1)
      
! DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!            
C     IF(N.EQ.60.AND.IYR.GE.96)THEN
C     	PRINT *, "FINISHED FIRST RAN1() CALL"
C     	PRINT *, "SETDAT", SETDAT
C     	PRINT *, "ISEED1", ISEED1
C     	PRINT *, "INEXT1", INEXT1
C     	PRINT *, "INXTP1", INXTP1      
C     END IF            	
      
      EPS2 = SDB(IS) * XNORM (1.D0,0.D0,ISEED1,MA1,INEXT1,INXTP1)
      BIASY = BIASB(IS)*EXP(EPS2-SDB(IS)**2.0/2.0)
      
! DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
C     IF(N.EQ.60.AND.IYR.GE.96)THEN
C     	PRINT *,""
C			PRINT *, "ABOUT TO CALL SECOND RAN1()", N, IYR
C     END IF                  
      
      ISEED  = INT(-RAN1(ISEED2,MA2,INEXT2,INXTP2)*100000.D0)
      
! DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!            
C     IF(N.EQ.60.AND.IYR.GE.96)THEN
C     	PRINT *, "FINISHED SECOND RAN1() CALL"
C     	PRINT *, "SETDAT", SETDAT
C     	PRINT *, "ISEED1", ISEED1
C     	PRINT *, "INEXT1", INEXT1
C     	PRINT *, "INXTP1", INXTP1      
C     END IF                  

C
C     Set abundance estimate SIGHT & CVX if a survey is done this year.
C     Note: if ISGHT=0 then SIGHT & CVX were set =-1 in RESET
C           except for historic values which were read in.

      IF (ISGHT.EQ.0) RETURN

C      1st set W to its expectation value
       W = PSUR/BKCON(IS)

       IF (OPTDET.EQ.0) THEN
C       Stochastic trial: Set SIGHT = Bias * K * BETASQ * EXP(EPS) * W
C           where W is the Poisson component.                           {Eqn B1.1}
        IF (W .LT. 70.D0) W = POISSN (W, RANNO)
 
        SIGHT = BIASY * BIASI * BKCON(IS) * EXP(EPS) * W

C       Set CVX, the estimate of the CV                                 {Eqn B1.6}:
        IF (SIGHT.GT.0.D0) THEN
C         CHISQ= random no from chi square distribution (DOF=deg.of freedom)
C         CVEST**2 = THTASQ(ASQ + BSQ/(W*BETASQ))                       {Eqn B1.7}

! DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
C				IF(N.EQ.60.AND.IYR.GE.96)THEN
C					PRINT *, "ABOUT TO CALL GAMMAZ"
C				END IF 
      		
   10     CHISQ = GAMMAZ (DOF, SQRT(DOF+DOF), ISEED, N, IYR) ! DOF = mean; sqrt(2*DOF) = sd
   
! DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
C				IF(N.EQ.60.AND.IYR.GE.96)THEN
C					PRINT *, "EXITING GAMMAZ"
C				END IF    
      		
          ECV2 = ACON(IS) + BCON(IS)/W                                  {Eqn B1.7}
          SIG2 = LOG(1.D0 + ECV2)                                       {Eqn B1.6}
          CV2  = SIG2 * CHISQ/DOF                                       {Eqn B1.6}
          CVX = MIN(SQRT(CV2),10.d0)
        ELSE
C         Zero estimate. Store Z(i), the Poisson multiplier, in CVX
          CVX = MIN(BIASI * BKCON(IS) * EXP(EPS),10.0d0)
        ENDIF
      ELSE
C       Deterministic trial                                             {Eqns B1.1, .6 & .7}
        SIGHT = BIASI * PSUR
        CVX = SQRT( LOG(1.D0 + ACON(IS) + BCON(IS)/W))
      ENDIF
      
! DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
C     IF(N.EQ.60.AND.IYR.GE.96)THEN
C 			PRINT *, "EXITING DATGEN"
C     END IF 
            
      RETURN
      END


C *********************************************************************
C *********************************************************************

      SUBROUTINE SETKA (K,PPRJ,PHIT,OK,IS,IYRMAN)

C  Set up the age-structure for this K & PPRJ
C  K=pristine 1+ population; PPRJ=1+ population size at start of projection

      USE DECL_STKVRS
      USE DECL_SETUP

      DOUBLE PRECISION K,PPRJ,KREC,RINIT(0:MXAGE),UINIT(0:MXAGE),PA,
     +       CM,CF,AINC,KINC,PHIT,CALF,MATF,RELREC,REL1PLS,TEMP,KZERO
      INTEGER OK,L,IYR,IS,IYRMAN

      DO 108 L = 0,MAXAGE
       SUR(2,L) = STORESUR(2,L) 
108   CONTINUE       
C
C     Set up recruitment ogive RECF: knife edge at age 1  {Eqn A4.1}
      CALL SETO (RECF,0.d0,AREC,MAXAGE,0,IS)

C     Scale the zero age class so the relative recruited population is unity.
      PA  = 1.D0/RREC(IS)
C     Set up relative recruited & unrecruited age vectors and 1+ population
      UINIT(0) = PA*(1.D0-RECF(IS,0))
      RINIT(0) = PA*RECF(IS,0)
      RELREC = PA*RECF(IS,0)
      REL1PLS = 0
      DO 20 L = 1,MAXAGE
        PA = PA*SUR(IS,L-1)
        UINIT(L) = PA*(1.D0 - RECF(IS,L))
        RINIT(L) = PA*RECF(IS,L)
        RELREC = RELREC + RINIT(L)
        REL1PLS = REL1PLS + RINIT(L) + UINIT(L)
   20 CONTINUE
      RELREC = RELREC - RINIT(MAXAGE)
      REL1PLS = REL1PLS - (RINIT(MAXAGE) + UINIT(MAXAGE))
      RINIT(MAXAGE) = RINIT(MAXAGE)/(1.D0 - SUR(IS,MAXAGE))
      RELREC = RELREC + RINIT(MAXAGE)
      REL1PLS = REL1PLS + (RINIT(MAXAGE) + UINIT(MAXAGE))
      KZERO = (1+REL1PLS/PA) / (REL1PLS/PA)

C     Reset recruitment ogive to transition form for use by STKUPA {Eqn A4.2}
      CALL TRFORM (RECF,MAXAGE,IS)

C     Set up the age-structure for this K (K=pristine 1+ population)
C     Note: in this model the maturity, recruitment and survival ogives
C          are the same for males as for females (hence the 0.5 factor).
      KREC = 0.5d0 * K*RELREC/REL1PLS
      MATF = 0.d0
      TST1 =0
      TST2 = 0
      DO 105 L = 0,MAXAGE
        UNRM(IS,L) = KREC*UINIT(L)
        RM(IS,L)   = KREC*RINIT(L)
        UNRF(IS,L) = UNRM(IS,L)
        RF(IS,L)   = RM(IS,L)
        MATF    = MATF +(RF(IS,L)+UNRF(IS,L))*FMATUR(IS,L)              {Eqn A2.2}
        TST1 = TST1 + RM(IS,L)
        IF (L.GT.0) TST2 = TST2 + UNRM(IS,L)+RM(IS,L)
  105 CONTINUE
C     NB K is constant from INITYR to IYRPRJ
      CALF = RM(IS,0)+RF(IS,0)+UNRM(IS,0)+UNRF(IS,0)
      KDD(IS,IYRPRJ) = K
      IF (OPTDD.EQ.1) KDD(IS,IYRPRJ) = MATF
      A(IS,IYRPRJ)   = A1(IS)

      OK = 1

C     Store (use for future reference if IYRPRJ.NE.INITYR)
      PMATF(IS,INITYR) = MATF
      PSURV(IS,INITYR) = K
      NEXP(IS,INITYR) = KREC * 2.0d0

      IF (IYRPRJ.NE.INITYR) THEN
C       Population not pristine when projection begins: Gray base case + Dx
C       Set up age structure for 1+ population=PPRJ in IYRPRJ
        CALL INITP(PPRJ,OK,IS)
C
        IF (OK.EQ.0) RETURN
C
C       Set maturity & 1+ population sizes                              {Eqn A2.2,A2.6}
        PSURV(IS,IYRPRJ) = 0.d0
        PMATF(IS,IYRPRJ) = 0.d0
        DO 106 L = 1,MAXAGE
          PMATF(IS,IYRPRJ) = PMATF(IS,IYRPRJ) +
     +        (RF(IS,L)+UNRF(IS,L))*FMATUR(IS,L)
          PSURV(IS,IYRPRJ) = PSURV(IS,IYRPRJ) + 
     +        RM(IS,L)+UNRM(IS,L)+RF(IS,L)+UNRF(IS,L)
  106   CONTINUE
      END IF
      
      IF (IS.EQ.2) THEN
       TEMP = IMMIREF*PSURV(1,INITYR)/20000
       TEMP = 1.0d0-TEMP/(PSURV(2,INITYR)*KZERO)
       SSTILDA = TEMP
       DO 107 L = 0,MAXAGE
        SUR(2,L) = STORESUR(2,L) * TEMP
107    CONTINUE       
      ENDIF

C     Forward project the population, reseting CM & CF in case they
C             were reduced in STKUPA
C     ITENT=-2 unless OPTK or OPTA=5 when ITENT+1=year of first increase
      DO 110 IYR = IYRPRJ,ITENT
        CM = CATCHM(IS,IYR)
        CF = CATCHF(IS,IYR)
        KDD(IS,IYR+1) = KDD(IS,IYRPRJ)
        A(IS,IYR+1)   = A1(IS)
        CALL STKUPA (IYR,CM,CF,OK,IS,IYRMAN)
  110 CONTINUE

      IF (OPTK(IS).EQ.5 .OR. OPTA.EQ.5) THEN
C       K &/or A vary as Tent from yr -50 {see Dvi & Dvii}
        KINC = 0.d0
        IF (OPTK(IS).EQ.5) KINC = KDD(IS,IYRPRJ)/50.d0
        AINC = 0.d0
        IF (OPTA.EQ.5) AINC = A1(IS)/50.d0
        DO 120 IYR = ITENT+1,-2
          CM = CATCHM(IS,IYR)
          CF = CATCHF(IS,IYR)
          KDD(IS,IYR+1) = KDD(IS,IYR)+KINC
          A(IS,IYR+1)   = A(IS,IYR)+AINC
          CALL STKUPA (IYR,CM,CF,OK,IS,IYRMAN)
  120   CONTINUE
      END IF

C     Store population in target year
      PHIT = PSURV(IS,IYRTAR)

      RETURN
      END

C *********************************************************************
C
      SUBROUTINE INITP(PPRJ,OK,IS)
C
C    Called from SETKA if IYRPRJ.NE.INITYR (ie projection does not start @ K)
C    Base case for gray whales 
C    INITP sets up the initial age-distribution in IYRPRJ by
C    solving the population equations to "hit" PPRJ = 1+ pop.size
C
      USE DECL_STKVRS
C
      DOUBLE PRECISION PPRJ,P1PL,ROI,FMIN,FMAX,B
      INTEGER OK,II,J,IS
C
      ROI = 0.d0
      CALL FUN2(ROI,P1PL,B,IS)
      IF (ABS(P1PL-PPRJ).LT.0.0000001d0*PPRJ) GOTO 1001

C     Use a Bisection approach to find the equilibrium age-structure
      FMIN =  0.d0
      FMAX =  1.d0
      DO 1000 II = 1,50
C
C      Bisect F    
       ROI = (FMIN + FMAX) * 0.5d0
       Call FUN2(ROI,P1PL,B,IS)
C
C      Check for convergence
       IF (ABS(P1PL-PPRJ).LT.0.0000001d0*PPRJ) GOTO 1001
       IF (P1PL.GT.PPRJ) THEN
         FMIN = ROI
       ELSE
         FMAX = ROI
       ENDIF
C
1000  CONTINUE
c     GOTO 1001  =  AEP trick !!
      IF (DOCON.NE.1) GOTO 1001
      OK = 0
      RETURN

1001  CONTINUE 

C     Rescale the numbers at age to actual values:
C     (FUN2 set RF & UNRF to sizes scaled by B)
      DO 2000 J = 0,MAXAGE
        UNRM(IS,J) = UNRF(IS,J) * B
        RM(IS,J) =   RF(IS,J) * B
        UNRF(IS,J) = UNRM(IS,J)
        RF(IS,J) =   RM(IS,J)
2000  CONTINUE      
C
      RETURN
      END
C
C *********************************************************************
C
      SUBROUTINE FUN2(ROI,P1PL,B,IS)
C
C     FUN2 sets the 1+ population for the given value of ROI (rate of inc.)
C     Called by INITP (only called if IYRPRJ.NE.INITYR)
C
      USE DECL_STKVRS
C
      DOUBLE PRECISION ROI,P1PL,PMAT,B,B2
      INTEGER J,IS
C
C     Calculate the numbers at age relative to the number of 0 yr olds {A6.3}
C     NB Population is in equilibrium so males=females as SUR is sex independent
      UNRF(IS,0) = 0.5d0
      RF(IS,0) = 0.d0
      DO 70 J = 0,MAXAGE-1
       UNRF(IS,J+1) = SUR(IS,J)*UNRF(IS,J)*(1.d0-RECF(IS,J+1))
       RF(IS,J+1) = SUR(IS,J) * 
     +                  (RF(IS,J)*(1.d0-ROI)+UNRF(IS,J)*RECF(IS,J+1))
 70   CONTINUE
C   
C     Adjust for last age class being pooled (and fully recruited)
      RF(IS,MAXAGE) = RF(IS,MAXAGE) / (1.d0-SUR(IS,MAXAGE)*(1.d0-ROI))

C     Add 1+ & mature female totals
      P1PL = 0.d0
      PMAT = 0.d0
      DO 90 J = 1,MAXAGE
       P1PL = P1PL + (UNRF(IS,J) + RF(IS,J))
       PMAT = PMAT + (UNRF(IS,J) + RF(IS,J))*FMATUR(IS,J)
 90   CONTINUE
      P1PL = P1PL*2.d0
C
C     Find no. of births B {A6.4} and hence 1+ population (P1PL)
      B = 0.d0
      IF (PMAT.GT.SUMSUR(IS)) THEN
        B2 = 1.d0 - (1.d0/(FEC(IS)*(PMAT-SUMSUR(IS)))-1.d0)/A1(IS)
        IF (B2.GT. 0.d0) THEN                                     
          B2 = KDD(IS,IYRPRJ) * B2**(1.d0/Z(IS))
          IF (OPTDD.EQ.1) THEN
C           Density dependence on mature FEMALES
            B = B2 / PMAT
          ELSE
            B = B2 / P1PL
          ENDIF
        ENDIF
      ENDIF
      P1PL = P1PL*B
C
      RETURN
      END


C *********************************************************************

      SUBROUTINE GETINCIDENTAL (IYR,IS,IYRMAN)

C     This routine determines the annual incidental catches
C     I assume a line increase from 1930 to 1999 and constant after 1999

      USE DECL_STKVRS  ! Include these variables in the scope of this procedure
      USE DECL_PCFG
      
      DOUBLE PRECISION ALPHA,BETA,FRACT(4),INCCATTEMP,Fract1,Fract2
      DOUBLE PRECISION TOP(4)
      INTEGER IYR,IS,IYRMAN
C
C     Increase in the incidental catch over time (Eqn A3.3)
      IF (IYR.LT.1999-IYRMAN) THEN
       BETA = 0.5 / (1999-1930)
       ALPHA = 1.0 - BETA*(1999-IYRMAN)
       FRACT = ALPHA + IYR*BETA
      ELSEIF (IYR.LT.0) THEN
       FRACT = 1.0
      ELSE
       TOP = 0
       DO 1000 IY = 2000-IYRMAN,2009-IYRMAN
        TOP(1) = TOP(1) + PSURV(1,IY)
        TOP(2) = TOP(2) + DeltaFutPCFG*PSURV(1,IY)+PSURV(2,IY)
        TOP(3) = TOP(3) + PSURV(2,IY)
        TOP(4) = TOP(4) + DeltaHistSOUTH*PSURV(1,IY)+PSURV(2,IY)
1000   CONTINUE        
       TOP(1) = TOP(1) / 10.0
       TOP(2) = TOP(2) / 10.0
       TOP(3) = TOP(3) / 10.0
       TOP(4) = TOP(4) / 10.0
       FRACT(1) = PSURV(1,IYR) / TOP(1)
       FRACT(2) = (DeltaFutPCFG*PSURV(1,IYR)+PSURV(2,IYR))/TOP(2)
       FRACT(3) = PSURV(2,IYR) / TOP(3)
       FRACT(4) = (DeltaHistSOUTH*PSURV(1,IYR)+PSURV(2,IYR))/TOP(4)
      ENDIF 
C
C     There are four strata (north, PCFG [Dec-May], PCFG [Jun - Nov], south)
      INCCATTEMP = 0
C
C     Before year 1 incidental catches are deterministic
      IF (IYR.LT.0) THEN
       IF (IS.EQ.1) THEN
        INCCATTEMP = INCCATTEMP + 1.0 * AVEINCCAT(1)
        INCCATTEMP = INCCATTEMP + (1-PhiHistPCFG) * AVEINCCAT(2)
        INCCATTEMP = INCCATTEMP + 0.0 * AVEINCCAT(3)
        INCCATTEMP = INCCATTEMP + (1-PhiHistSOUTH) * AVEINCCAT(4)
       ENDIF
       IF (IS.EQ.2) THEN
        INCCATTEMP = INCCATTEMP + 0.0 * AVEINCCAT(1)
        INCCATTEMP = INCCATTEMP + PhiHistPCFG * AVEINCCAT(2)
        INCCATTEMP = INCCATTEMP + 1.0 * AVEINCCAT(3)
        INCCATTEMP = INCCATTEMP + PhiHistSOUTH * AVEINCCAT(4)
       ENDIF
       INCCATTEMP = INCCATTEMP * FRACT(1)
      ENDIF 
C
C     After year 1 incidental catches are allocated proportional to pop size (Eqn A3.2) 
      IF (IYR.GE.0) THEN
       ONEP1 = PSURV(1,IYR)+0.001
       ONEP2 = PSURV(2,IYR)+0.001
       Fract1 = ONEP2/(DeltaFutPCFG*ONEP1+ONEP2)
       Fract2 = ONEP2/(DeltaHistSOUTH*ONEP1+ONEP2)
       IF (IS.EQ.1) THEN
        INCCATTEMP = INCCATTEMP + FRACT(1)*1.0 * AVEINCCATF(1)
        INCCATTEMP = INCCATTEMP + FRACT(2)*(1-Fract1) * AVEINCCATF(2)
        INCCATTEMP = INCCATTEMP + FRACT(3)*0.0 * AVEINCCATF(3)
        INCCATTEMP = INCCATTEMP + FRACT(4)*(1-Fract2) * AVEINCCATF(4)
       ENDIF
       IF (IS.EQ.2) THEN
        INCCATTEMP = INCCATTEMP + FRACT(1)*0.0 * AVEINCCATF(1)
        INCCATTEMP = INCCATTEMP + FRACT(2)*Fract1 * AVEINCCATF(2)
        INCCATTEMP = INCCATTEMP + FRACT(3)*1.0 * AVEINCCATF(3)
        INCCATTEMP = INCCATTEMP + FRACT(4)*Fract2 * AVEINCCATF(4)
       ENDIF
      ENDIF
C
C     Store for later use (no incidental catches after year 100)
      IF (IYR.LE.IENDYR) THEN
       INCCAT(IS,IYR) = INCCATTEMP
      ELSE
       INCCAT(IS,IYR) = 0
      ENDIF 
C
      RETURN
      END

C *********************************************************************

      SUBROUTINE GETIMMIGRATION(IYR,IS,IYRMAN)

      USE DECL_STKVRS
      USE DECL_SETUP
            
      INTEGER IYR,IS,L
C
C     What to depends on which stock it is (only need to compute IS=1)
C
C     North stock (emigration)
      IF (IS.EQ.1) THEN
C
C      How much emigration is there?
       IF (OPTIMMI.EQ.0) THEN
        TOTALIMMI(IYR) = IMMIREF*(PSURV(IS,IYR)/20000.0)**IMMIPOW
       ELSEIF (OPTIMMI.EQ.1) THEN
        IF (IYR+IYRMAN.EQ.1999.OR.IYR+IYRMAN.EQ.2000) THEN
         TOTALIMMI(IYR) = 20.0
        ELSE 
         TOTALIMMI(IYR) = IMMIREF*(PSURV(IS,IYR)/20000.0)**IMMIPOW
        ENDIF 
       ELSEIF (OPTIMMI.EQ.2) THEN
        IF (IYR+IYRMAN.EQ.1999.OR.IYR+IYRMAN.EQ.2000) THEN
         TOTALIMMI(IYR) = 30.0
        ELSE 
         TOTALIMMI(IYR) = IMMIREF*(PSURV(IS,IYR)/20000.0)**IMMIPOW
        ENDIF 
       ELSEIF (OPTIMMI.EQ.3) THEN
        IF (IYR+IYRMAN.EQ.1999.OR.IYR+IYRMAN.EQ.2000) THEN
         TOTALIMMI(IYR) = 10.0
        ELSE 
         TOTALIMMI(IYR) = IMMIREF*(PSURV(IS,IYR)/20000.0)**IMMIPOW
        ENDIF 
       ELSE
        WRITE(*,*) "Error in Immigration"
        STOP
       ENDIF
C
C      Allocate to ages {Eqn A1.3}
       DO 1000 L = 1,MAXAGE
        IMMIM(IYR,L) = TOTALIMMI(IYR)*(RM(IS,L)+UNRM(IS,L))/
     +   (PSURV(IS,IYR)+0.0001)
        IMMIF(IYR,L) = TOTALIMMI(IYR)*(RF(IS,L)+UNRF(IS,L))/
     +   (PSURV(IS,IYR)+0.0001)
 1000  CONTINUE
C 
      ENDIF
C
      RETURN
      END

C *********************************************************************

      SUBROUTINE STKUPA (IYR,CM,CF,OK,IS,IYRMAN)

C     STKUPA updates the stock size at the start of IYR+1, i.e. it
C     updates the RM & UNRM arrays and sets PMATF(IYR+1) & PSURV(IYR+1)

      USE DECL_STKVRS
      USE DECL_SETUP

      DOUBLE PRECISION CM,CF,PEXPM,PEXPF,URM,URF,DDFUN,
     +     BIRTHS,PMAT,P1PLUS,PDD,PROBB,NAVMAT,FMALES,MALES,
     +     RX,PDD2,REMOVE,AGEL,CM2,CF2,RFTMP(0:MXAGE),
     +     RMTMP(0:MXAGE),INCIDENTALCATCH
      INTEGER IYR,L,OK,I,IX,IL,IYRMAN
C
C     Find the Incidental catch
! DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!			if(IYR.eq.69) print *, "CALLING GETINCIDENTAL"

      CALL GETINCIDENTAL(IYR,IS,IYRMAN)
C
C     Find the immigration
      CALL GETIMMIGRATION(IYR,IS,IYRMAN)
      INCIDENTALCATCH = INCCAT(IS,IYR)*0.5
C      
C     Adjust for incidental catces
      CM2 = CM + INCIDENTALCATCH
      CF2 = CF + INCIDENTALCATCH
C
C     Define Numbers AFTER immigration and compute the numbers of recruited whales
      PEXPM = 0.0
      PEXPF = 0.0
      DO 10 L = 0,MAXAGE
       IF (IS.EQ.1) RMTMP(L) = RM(IS,L) - IMMIM(IYR,L)
       IF (IS.EQ.1) RFTMP(L) = RF(IS,L) - IMMIF(IYR,L)
       IF (IS.EQ.2) RMTMP(L) = RM(IS,L) + IMMIM(IYR,L)
       IF (IS.EQ.2) RFTMP(L) = RF(IS,L) + IMMIF(IYR,L)
       PEXPM  = PEXPM + RMTMP(L)
       PEXPF  = PEXPF + RFTMP(L)
10    CONTINUE       
      NEXP(IS,IYR) = PEXPM + PEXPF
      
C     Set URM and URF = average survivorship after catching.  {Eqn A3.1}
      IF (PEXPM.GT.CM2) THEN
        URM = 1.D0 - CM2/PEXPM
      ELSE
        URM = 0.D0
        CM = MAX(0.0d0,PEXPM - INCIDENTALCATCH)
        OK = 0
      ENDIF
      IF (PEXPF.GT.CF2) THEN
        URF = 1.D0 - CF2/PEXPF
      ELSE
        URF = 0.D0
        CF = MAX(0.0d0,PEXPF - INCIDENTALCATCH)
        OK = 0
      ENDIF

C *** Population projection
C     First advance the pooled age class (gets different treatment) {Eqn A1.1}
      RX = URM*RMTMP(MAXAGE)*SUR(IS,MAXAGE)
      RM(IS,MAXAGE) = URM*RMTMP(MAXAGE-1)*SUR(IS,MAXAGE-1) + RX
      RX = URF*RFTMP(MAXAGE)*SUR(IS,MAXAGE)
      RF(IS,MAXAGE) = URF*RFTMP(MAXAGE-1)*SUR(IS,MAXAGE-1) + RX

C     Loop advances remaining age classes, working from oldest down.{Eqn A1.1}
C     Add nos. of mature females (PMATF) and 1+ (PSURV) {Eqns A2.2 & 6}
      PMAT  =  RF(IS,MAXAGE)
      P1PLUS = RM(IS,MAXAGE) + RF(IS,MAXAGE)
      DO 40 L = MAXAGE-1,1,-1
        UNRM(IS,L) = UNRM(IS,L-1)*(1.D0 - RECF(IS,L))*SUR(IS,L-1)
        UNRF(IS,L) = UNRF(IS,L-1)*(1.D0 - RECF(IS,L))*SUR(IS,L-1)
        RM(IS,L)   = (RMTMP(L-1)*URM + UNRM(IS,L-1)*RECF(IS,L)) * 
     +   SUR(IS,L-1)
        RF(IS,L)   = (RFTMP(L-1)*URF + UNRF(IS,L-1)*RECF(IS,L)) * 
     +   SUR(IS,L-1)
        PMAT = PMAT + (RF(IS,L)+UNRF(IS,L))*FMATUR(IS,L)
        P1PLUS = P1PLUS + RM(IS,L) + UNRM(IS,L) + RF(IS,L) + UNRF(IS,L)
   40 CONTINUE
      IX = IYR+1
      PMATF(IS,IX) = PMAT
      PSURV(IS,IX) = P1PLUS

C     Set PDD = ratio in Eqn A2.3. Reset if Z < 0 (can occur in search for K)
C     Incorporate time lag ILAG (if any)
      IL = MAX(IX-ILAG,IYRPRJ)
      IF (OPTDD.EQ.0) THEN
        PDD = PSURV(IS,IL)/(KDD(IS,IL)+0.00001)
      ELSE 
        PDD = PMATF(IS,IL)/(KDD(IS,IL)+0.00001)
      ENDIF

C     Set PDD2 = PDD**Z, checking log value to ensure no overflow error
      IF (PDD.GT.0.D0) THEN
        PDD2 = Z(IS)*LOG(PDD)
        IF (PDD2.GT.600.D0) THEN
          PDD2=600.D0
        ENDIF
        PDD2 = EXP(PDD2)
      ELSE
        PDD2 = 0.D0
      ENDIF

C     Add new births to zero age-class
C     1st set b(t+1) = probability of births/available female {Eqn A2.3}
      DDFUN = MAX(0.d0,1.D0 + A(IS,IX)*(1.D0 - PDD2))
      ProbB = MAX(0.000001D0, FEC(IS)*DDFUN)
C
C     Set no.of available females & hence births {A2.1,gen.} NB SUMSUR=0 if NY=0
C     (This assumes that all mature have survival rate=SUR(MAXAGE))
      NAvMat = PMATF(IS,IX)
      DO 50 L=1,NY
        I = MAX(IX-L,IYRPRJ)
        NAvMat = NAVMAT - PCALF(IS,I)*SUR(IS,MAXAGE)**L
   50 CONTINUE
      BIRTHS = MAX(NAvMat,0.d0)*ProbB
C
C     Allocate births to sex {A2.4 & 5}
      MALES = BIRTHS *0.5d0
      FMALES = BIRTHS - MALES
      RM(IS,0) = MALES * RECF(IS,0)
      UNRM(IS,0) = MALES - RM(IS,0)
      RF(IS,0) = FMALES * RECF(IS,0)
      UNRF(IS,0) = FMALES - RF(IS,0)

C     Update the survival array if OPTM(IS)>0 (See Dviii)
C     (It only changes from year 0 so not used in conditioning)
      IF (DOCON.LT.1) THEN
        IF (OPTM(IS).GT.0 .AND. IX.GE.0)CALL SETSUR(1,0,IX+1,IS)
      ENDIF
C
C     Special mortality event (Northern stock only)
      IF ((IS.EQ.1).AND.(IX+IYRMAN.EQ.1999.OR.IX+IYRMAN.EQ.2000)) THEN
       DO 95 L=0,MAXAGE
        UNRM(IS,L) = UNRM(IS,L) * EXTRAS
        UNRF(IS,L) = UNRF(IS,L) * EXTRAS
        RM(IS,L) = RM(IS,L) * EXTRAS
        RF(IS,L) = RF(IS,L) * EXTRAS
   95  CONTINUE
      ENDIF
C
      IF (IX.LT.-1) GOTO 9989

C     Check PROBE to see if an epidemic occurs
C     Reduce population by PROBE(IX)%
      IF (PROBE(IS,IX).NE.0) THEN
       IF (OPTEE.EQ.0) THEN
C       Deaths occur uniformly thru all age classes
        RATIO = 1.D0 - (PROBE(IS,IX)/100.D0)
        DO 90 L=0,MAXAGE
          UNRM(IS,L) = UNRM(IS,L) * RATIO
          UNRF(IS,L) = UNRF(IS,L) * RATIO
          RM(IS,L) = RM(IS,L) * RATIO
          RF(IS,L) = RF(IS,L) * RATIO
   90   CONTINUE
       ELSE
C       Reduce population by PROBE(IS,IX)%, youngest first.
        REMOVE = (PSURV(IS,IX)+PCALF(IS,IX))*(PROBE(IS,IX)/100.D0)
        DO 100 L=0,MAXAGE
          AGEL = UNRM(IS,L)+UNRF(IS,L)+RM(IS,L)+RF(IS,L)
          IF (AGEL.LE.REMOVE) THEN
            REMOVE = REMOVE - AGEL
            UNRM(IS,L) = 0.D0
            UNRF(IS,L) = 0.D0
            RM(IS,L) = 0.D0
            RF(IS,L) = 0.D0
          ELSE
C           Only part of this age group die
            RATIO   = REMOVE/AGEL
            UNRM(IS,L) = UNRM(IS,L) * RATIO
            UNRF(IS,L) = UNRF(IS,L) * RATIO
            RM(IS,L) = RM(IS,L) * RATIO
            RF(IS,L) = RF(IS,L) * RATIO
            GO TO 9989
          END IF
  100   CONTINUE
       ENDIF
      ENDIF
C
C     Recalculated totals
9989  CONTINUE
      PCALF(IS,IX) = UNRM(IS,0) + UNRF(IS,0) + RM(IS,0) + RF(IS,0)
      PMATF(IS,IX) = 0.d0
      PSURV(IS,IX) = 0.d0
      PEXPM = RM(IS,0)
      PEXPF = RF(IS,0)
      DO 96 L = 1,MAXAGE
        PEXPM  = PEXPM + RM(IS,L)
        PEXPF  = PEXPF + RF(IS,L)
        PMATF(IS,IX) = PMATF(IS,IX)+(RF(IS,L)+UNRF(IS,L))*FMATUR(IS,L)
        PSURV(IS,IX) = PSURV(IS,IX) +  
     +      RM(IS,L)+UNRM(IS,L)+RF(IS,L)+UNRF(IS,L)
   96 CONTINUE
      NEXP(IS,IX) = PEXPM + PEXPF
C
      RETURN
      END
C
C *********************************************************************

      SUBROUTINE SETSUR(I1,IA,IYR,IS)

C     Update survival array SUR if it is time dependent (if OPTM > 0, see Dviii)
C     OPTM= 0: none  1: doubles  2: halves  4: steps (halve then double)
C     I1 = 0 for initialisation.  IA is used to pass value of AGEJ.

      USE DECL_STKVRS

      DOUBLE PRECISION MORTJ(2),MORT(2),MJINC(2),MINC(2)
      INTEGER I1,IA,AGEJ(2),J,I,IYR,IS
      SAVE MORTJ,MORT,MJINC,MINC,AGEJ

      IF (I1.EQ.0) THEN
C       Initialise variables used to update SUR array (called by RESET)
        AGEJ(IS) = IA
        MORTJ(IS) = -LOG(SUR(IS,AGEJ(IS)))
        MORT(IS)  = -LOG(SUR(IS,MAXAGE))
C       Set increments for M doubling or halving
        IF (OPTM(IS).EQ.1) THEN
          MJINC(IS) = MORTJ(IS) / 100.d0
          MINC(IS)  = MORT(IS) / 100.d0
        ELSE IF (OPTM(IS).EQ.2) THEN
          MJINC(IS) = -MORTJ(IS) / 200.d0
          MINC(IS)  = -MORT(IS) / 200.d0
        ELSE IF (OPTM(IS).EQ.5) THEN
          MJINC(IS) = MORTJ(IS) / 200.d0
          MINC(IS)  = MORT(IS) / 200.d0
        ELSE IF (OPTM(IS).EQ.6) THEN
          MJINC(IS) = -MORTJ(IS) / 300.d0
          MINC(IS)  = -MORT(IS) / 300.d0
        ENDIF

        RETURN

C     Update survival rate {Eqn A1.2}   (called by STKUPA)
      ELSE IF (OPTM(IS).NE.4) THEN
        MORTJ(IS) = MORTJ(IS) + MJINC(IS)
        MORT(IS) = MORT(IS) + MINC(IS)
      ELSE IF (IYR.EQ.33) THEN
C       33 yr step change in M
        MORTJ(IS) = MORTJ(IS) * 0.5d0
        MORT(IS) = MORT(IS) * 0.5d0
      ELSE IF (IYR.EQ.66) THEN
        MORTJ(IS) = MORTJ(IS)+MORTJ(IS)
        MORT(IS)  = MORT(IS) +MORT(IS)
      ELSE
C       SUR doesn't change this year
        RETURN
      ENDIF

C     Corrected Oct01: previously used 1,AGEJ
      DO 5 J = 0,AGEJ(IS)
       SUR(IS,J) = EXP(-MORTJ(IS))
   5  CONTINUE
      DO 6 J = AGEJ(IS)+1,MAXAGE
       SUR(IS,J) = EXP(-MORT(IS))
   6  CONTINUE
      IF (AGEJ(IS).GT.0) SUR(IS,0) = SUR(IS,0)*SUR(IS,MAXAGE)

C     Reset SUMSUR = SUR + SUR**2 + ..... + SUR**NY.
      SUMSUR(IS) = 0.D0
      DO 10 I = 1,NY
        SUMSUR(IS) = SUMSUR(IS) + SUR(IS,MAXAGE)**I
  10  CONTINUE

      RETURN
      END

C ======================================================================

      SUBROUTINE CONDG (IYRMAN,REF,PARFIL)

C     GRAY WHALES: Generate from the prior distributions,
C     calculate the likelihood function and save the results to disk.

      USE DECL_STKVRS
      USE DECL_SETUP
      USE DECL_CONVRS
      USE DECL_DATVRS
C
      INTEGER ISEEDC,REPLI,IERR,IYRMAN,I,J,NALL,ITest,
     +        ISEEDZ,NDRAW,MAXDUP,NABUND(2),ABSYR(2,30),IYR,Iunique
      INTEGER FAILS(2,6),FAILS2(2)
      LOGICAL GOOD,LPRJ
      DOUBLE PRECISION DIF,P1,P2,F1,F2,PARS(10),TOL,
     +       LIKEL,PINIT(2),ABSOBS(2,30),CVOBS2(2,30),HISTBIAS(2,30),
     +       HITPYR,HITPEG,MXLIKE,TOTLKE,C1,ECVA2,
     +       CALFO(20),CALFCV(20),CVADDC,CALFSE,LIKEA,TEMP,
     +       VARCOA(2,100,100),VARC2(100,100),IVAR(100,100),
     +       RESID(100),THEDET,PREDICTED
      INTEGER NCALFE,CALFY(20)
      CHARACTER REF*10, PARFIL*12
      EXTERNAL HITPYR,HITPEG
C
C     Seed assignment 
      ISEEDC = -45645
      LPRJ = IYRPRJ.NE.INITYR
      TOL = 0.00001d0
C
C     Read in the abundance estimates + CVs. See table 3 & A8diii
C     Store log of abundance in ABSOBS      
      DO 1700 IS = 1,2
       READ(14,*) NABUND(IS)
       DO 800 I = 1,NABUND(IS)
        READ(14,*) ABSYR(IS,I),ABSOBS(IS,I),CVOBS2(IS,I),HISTBIAS(IS,I)
        ABSOBS(IS,I) = LOG(ABSOBS(IS,I))
        ABSYR(IS,I) = ABSYR(IS,I) - IYRMAN
        CVOBS2(IS,I) = CVOBS2(IS,I) * CVOBS2(IS,I)
800    CONTINUE
       READ(14,*)
       READ(14,*)
       DO 900 I = 1,NABUND(IS)
        READ(14,*) TEMP,(VARCOA(IS,I,J),J=1,NABUND(IS))
900    CONTINUE
       READ(14,*)
       
1700  CONTINUE       
C
C     Calf counts
      READ(14,*) NCALFE
      DO 801 I = 1,NCALFE
       READ(14,*) CALFY(I),CALFO(I),CALFSE
       CALFY(I) = CALFY(I)-IYRMAN
       CALFCV(I) = CALFSE/CALFO(I)
801   CONTINUE      
C
      NALL = 0
      TOTLKE = 0.D0
      MXLIKE = 0.D0
      NDRAW = 100000
      ITEST = 200000
      
      IYRMANPass = IYRMAN

C     Move through all the replicates
  950 CONTINUE
      OPEN(UNIT=13,FILE='CONDIT.TMP',ACCESS='APPEND')
      DO 1000 REPLI = 1,NDRAW
C       WRITE(*,'(1x,30(I9,1x))') REPLI,((FAILS(IS,II),II=1,6),IS=1,2),
C     +  FAILS2
C
C      Generate ISEEDZ=seed for this replicate + 
C      parameters from the priors using ISEEDC (modify for new cases). Step A8a
       CALL GENPAR(SUR,MAXAGE,ISEEDC,CVADD2,BIAS(1,IYRTAR),
     +    BIAS(2,IYRTAR),ISEEDZ,LPRJ,CVADDC,EXTRAS)
C
C      Find the juvenile S corresponding to adult M, pregnancy rate. Step A8b
       DO 1100 IS = 1,2
        ISpass = IS
        P1 = 5.d0
        P2 = 100.d0
        CALL ZBRAC2(HITPEG,PARS,1,P1,P2,F1,F2,GOOD)
        IF (.NOT.GOOD) THEN
         FAILS(IS,1) = FAILS(IS,1) + 1
         GOTO 9999
        ENDIF 
        CALL ZBRNT2(HITPEG,PARS,1,P1,P2,F1,F2,0.0001d0,IERR)
        DIF = HITPEG(PARS,IERR)
        IF (ADULTS.LT.JUVS(IS) .OR. ABS(DIF).GT.0.001d0) THEN
         FAILS(IS,2) = FAILS(IS,2) + 1
         GOTO 9999
        ENDIF 
        IF ((A1(IS)*Z(IS).LT.0.D0 .OR. Z(IS).LT.-100.D0  .OR.
     +       Z(IS).GT.1000.D0 .OR. ABS(A1(IS)).GT.100.D0)) THEN
         FAILS(IS,3) = FAILS(IS,3) + 1
         GOTO 9999
        ENDIF 
1100   CONTINUE       
       STORESUR = SUR
C
C      Find 1+ population in IYRPRJ giving target (PTARGT) in IYRTAR (1968). Step A8c
       DO 1200 IS = 1,2
        ISpass = IS
        IF (ISPASS.EQ.1) THEN
         IF (LPRJ)  THEN
          P1 = 1000.d0
          P2 = K1(IS)
         ELSE
          P1 =  5000.D0
          P2 = 10000.D0
         ENDIF
        ELSE
         IF (LPRJ)  THEN
          P1 = 0.1*K1(IS)
          P2 = K1(IS)
         ELSE
          P1 =  50.D0
          P2 = 1000.D0
         ENDIF
        ENDIF
        CALL ZBRAC2(HITPYR,PARS,1,P1,P2,F1,F2,GOOD)
        IF (.NOT.GOOD) THEN
         FAILS(IS,4) = FAILS(IS,4) + 1
         GOTO 1000
        ENDIF 
        CALL ZBRNT2(HITPYR,PARS,1,P1,P2,F1,F2,TOL,IERR)
        DIF = HITPYR(PARS,IERR)
        IF (ABS(DIF).GT.10.D0) THEN
         FAILS(IS,5) = FAILS(IS,5) + 1
         GOTO 9999
        ENDIF 
        PINIT(IS) = PARS(1)
        IF (.NOT.LPRJ) K1(IS) = PINIT(IS)
1200   CONTINUE        
C
C      Calculate likelihood (seriously case-specific). Step A8d
       LIKEL = 0.D0
       DO 1300 IS = 1,2
C
C       With varco matrix
        LIKEA = 0
        C1 = CVADD2(IS) / (0.1D0 + (0.013D0*K1(IS))/PSURVObs(IS,IYRTAR))
        DO 1310 I = 1,NABUND(IS)
         IYR = ABSYR(IS,I)
         IF (PSURVOBS(Is,IYR).LT.0) THEN
          RESID(I) = 10000
          ECVA2 = 0
         ELSE 
          ECVA2 = C1 * (0.1D0 + (0.013D0*K1(IS))/PSURVObs(IS,IYR))       {Eqn A8.2}
          PREDICTED = 0.1+PSURVObs(IS,IYR)*BIAS(IS,IYR)*HISTBIAS(IS,I)
          RESID(I) = ABSOBS(IS,I)-LOG(PREDICTED)
         ENDIF 
         DO 1310 J = 1,NABUND(IS)
          VARC2(I,J) = VARCOA(IS,I,J)
          IF (I.EQ.J) VARC2(I,J) = VARC2(I,J) + ECVA2
1310    CONTINUE          
C
C       Find the matrix inversion and the determinant
        CALL INVM2(VARC2, IVAR, 100, NABUND(IS), THEDET)
C
C       Negative log-likelihood
        IF (THEDET.GT.0) THEN
         LIKEA = 0.5*log(THEDET)
         DO 1320 I = 1,NABUND(IS)
          DO 1320 J = 1,NABUND(IS)
           LIKEA = LIKEA + 0.5*RESID(I)*IVAR(I,J)*RESID(J)
1320     CONTINUE        
         IF (LIKEA.GT.1000) LIKEA = 1000
        ELSE
         LIKEA = 1000
         FAILS(IS,6) = FAILS(IS,6) + 1
         GOTO 9999
        ENDIF  
C
        LIKEL = LIKEL + LIKEA
1300   CONTINUE 
       
C      If likelihood v. large don't store
       IF (LIKEL.GT.-30d0) THEN
        FAILS2(1) = FAILS2(1) + 1
        GO TO 9999
       ENDIF 
C
       WRITE(13,600) LikeL,TRANSA,NINT(MAT50(1)),ADULTS,MSYL(1),MSYR(1),
     +      MSYL(2),MSYR(2),JUVS(1),JUVS(2),K1(1),PINIT(1),PTARGT(1),
     +      K1(2),PINIT(2),PTARGT(2),ISEEDZ,PregTG(1),PregTG(2),
     +      SQRT(CVADD2(1)),SQRT(CVADD2(2)),CVADDC,PSURV(1,-1),
     +      PSURV(2,-1),EXTRAS,SSTILDA
       FAILS2(2) = FAILS2(2) + 1

C      Find MXLIKE = maximum value of likelihood and test probability of
C      having sufficient replicates
       LIKEL = EXP(-LIKEL+40.D0)
       TOTLKE = TOTLKE + LIKEL
       IF (LIKEL.GT.MXLIKE) MXLIKE = LIKEL
       NALL = NALL + 1
       IF (MOD(NALL,1000).EQ.0) PRINT '(2A,I10,F10.5,I9)',' CONDGX  ',
     +             REF,NALL,MXLIKE/TOTLKE,ITEST

9999  CONTINUE
1000  CONTINUE
      CLOSE(13)
C
1001  IF (NALL.GT.0 .AND. MXSIM.GT.100) THEN
C      AEP gray whale runs & extended inertia trials
       CALL COND2(100,NALL,MXLIKE,TOTLKE,MAXDUP,IUnique,'P100.PAR ',
     +   FAILS,FAILS2,PARFIL)
       PRINT '(A,I6,I8,I6/)',' CONDGA  ',MAXDUP,ITEST,Iunique
       CALL COND2(MXSIM,NALL,MXLIKE,TOTLKE,MAXDUP,IUnique,'PARMS.PAR',
     +   FAILS,FAILS2,PARFIL)
       PRINT '(/A,I6,I8,I6)',' CONDGB  ',MAXDUP,ITEST,Iunique
       IF (MAXDUP.LE.MAX(1.0d0,0.004d0*MXSIM) .OR. ITEST.LE.0) RETURN
      ELSE IF (NALL.GT.0) THEN
C      AW trials
       CALL COND2(NTRIAL,NALL,MXLIKE,TOTLKE,MAXDUP,IUnique,'P100.PAR ',
     +   FAILS,FAILS2,PARFIL)
       PRINT '(A,I6,I8,I6/)',' CONDGY ',MAXDUP,ITEST,Iunique
       IF (MAXDUP.EQ.1 .OR. ITEST.LE.0) RETURN
      ENDIF
C      IF (ITEST.EQ.199999) STOP
      ITEST = ITEST - 1
      GO TO 950
 600  FORMAT(F11.5,2I3,7F16.13,6F16.9,I11,5F16.12,2F10.3,2F16.9)
      END

C
C **********************************************************************

      SUBROUTINE COND2(NREPL,NALL,MXLIKE,TOTLKE,MaxDup,IUnique,OFILE,
     +  FAILS,FAILS2,PARFIL)

C     COND2 reads in CONDIT.TMP and selects NREPL sets of parameters
C     with replacement (computes the major weight and the number of
C     duplicate replicates (subroutine based on SUMMARY.FOR)

C     Global variables
      INTEGER NREPL,NALL
      DOUBLE PRECISION TOTLKE,MXLIKE  
      INTEGER FAILS(2,6),FAILS2(2)
      CHARACTER OFILE*9, PARFIL*12
C
C     Local variables
      DOUBLE PRECISION LikeL,MA(55),CUMDO(10000),Cumtot,RAN1,RNLogL
      INTEGER ISEED,I,II,INEXT,INEXTP,IQQ,IDupl,MaxDup,Iunique,IS
      CHARACTER*357 STRING
      EXTERNAL RAN1
C
C     Generate a wad of uniform random numbers
      ISEED = -8989
      DO 2000 I = 1,NREPL
       CUMDO(I) = RAN1(ISEED,MA,INEXT,INEXTP)
2000  CONTINUE
      CALL SORT(CUMDO,NREPL)

      OPEN(UNIT=13,FILE='CONDIT.TMP',STATUS='OLD')
      OPEN(UNIT=15,FILE=OFILE,STATUS='UNKNOWN')
      WRITE(15,'(1X,2A3,6A16,A11,A10,5A16,4A10)')  'Aa','Am','MSYL',
     +   'MSYR','MORTJ','MORTA','K1+','PInit','ISEED','PTarget','CVADD',
     +   'AR','PREG-MAX','GAMMA','P0','SSTILDA','LIKEL'
      CumTot = 0.d0
      IQQ = 1
      Iunique = 0
      MaxDup = 0
      DO 3000 II = 1,NALL
       READ(13,'(F11.5,A357)') LikeL,STRING
       RNLogL = Likel
       LikeL = EXP(-LikeL+40.d0)
       CumTot = CumTot + LikeL
       Idupl = 0
3001   IF (IQQ.LE.NREPL .AND. CumTot/TOTLKE.GT.CumDO(IQQ)) THEN
         WRITE(15,'(1X,A,F11.5)') STRING,RNLogL
         IF (Idupl.EQ.0) Iunique = Iunique + 1
         Idupl = Idupl + 1
         IF (Idupl.GT.MaxDup) MaxDup = IDupl
         IQQ = IQQ + 1
         GOTO 3001
       ENDIF
3000  CONTINUE
      CLOSE(13)
      WRITE (15,*)
      WRITE (15,*) ' COND2',MaxDup,NALL,TOTLKE,MXLIKE,MXLIKE/TOTLKE,
     +             Iunique
      WRITE(15,'(1x,30(I9,1x))') NALL,((FAILS(IS,II),II=1,6),IS=1,2),
     +  FAILS2
      WRITE(15,602) PARFIL
      CLOSE(15)
C
      RETURN
602   FORMAT(1x,"PARAMETER FILE ",A12)      
      END


C **********************************************************************
C
      SUBROUTINE GENPAR(SUR,MAXAGE,ISEED,CVADD2,BIAST1,BIAST2,
     +                 ISEEDZ,LPRJ,CVADDC,EXTRAS)
C
C     Generate parameters from prior distributions.  Step A8a
C
      USE DECL_SETUP
      USE DECL_CONVRS
C
C     Global variables
      DOUBLE PRECISION SUR(2,0:MXAGE),CVADD2(2),CVADDC,EXTRAS
      INTEGER ISEED,MAXAGE,ISEEDZ,IS
      LOGICAL LPRJ
C
C     Local variables
      DOUBLE PRECISION FECMAX,PTEMP(4),BIAST1,BIAST2,PRIOR
      INTEGER AGE

      EXTERNAL PRIOR
C
C     Seed generation
      PTEMP(1) = -1000000.d0
      PTEMP(2) = -1.d0
      ISEEDZ = NINT(PRIOR('U',PTEMP,ISEED))
C
      MSYL(1) = PRIOR(MSYLT1,MSYLP1,ISEED)
      MSYR(1) = PRIOR(MSYRT1,MSYRP1,ISEED)
      MSYL(2) = PRIOR(MSYLT2,MSYLP2,ISEED)
      MSYR(2) = PRIOR(MSYRT2,MSYRP2,ISEED)
C
C     Generate the juvenile / adult transition age
      TRANSA =  PRIOR(APART,APARP,ISEED)
C
C     Generate the juvenile natural survival rate  NOT USED HERE
1100  JUVS(1) = PRIOR(JMORTT,JMORTP,ISEED)
      JUVS(2) = JUVS(1)
      IF (JUVS(1).LE.0.d0.OR.JUVS(1).GE.1.d0) GOTO 1100
C     Note: these juvenile values are not used - value is reset in HITPEG
C
C     Generate the adult natural survival rate (note constraints)
1200  ADULTS =  PRIOR(AMORTT,AMORTP,ISEED)
      IF (ADULTS.LE.0.d0.OR.ADULTS.GE.1.d0) GOTO 1200
C
C     Set up the SUR array = survival rate - do juveniles later {Eqn A1.2}
      DO 6 IS = 1,2
       DO 6 AGE = TRANSA+1,MAXAGE
        SUR(IS,AGE) = ADULTS
   6  CONTINUE
C
C     Generate the target pregnancy rate
      FecMax = PRIOR(PREGNT,PREGNP,ISEED)
      PREGTG(1) = 1.d0 / FecMax
      PREGTG(2) = 1.d0 / FecMax
C
C     Generate an age-at-maturity (and convert to first parturition)
      MAT50 = PRIOR(TPART,TPARP,ISEED)
      MAT50 = MAT50 + 1
C
C     Generate a target population size (stock 1)
      CVADD2(1) = PRIOR(CVADT1,CVADP1,ISEED)
      CVADD2(1) = CVADD2(1)**2.0
      PTARGT(1) = PRIOR(TARGT1,TARGP1,ISEED)*1000.d0/BIAST1
C
C     Generate a target population size (stock 2)
      CVADD2(2) = PRIOR(CVADT2,CVADP2,ISEED)
      CVADD2(2) = CVADD2(2)**2.0
      PTARGT(2) = PRIOR(TARGT2,TARGP2,ISEED)*1000.d0/BIAST2
C
C     Special case (correlated P68)
      IF (CORRELATEDN.EQ.1) THEN
       PROPN = (PTARGT(1)/1000.0d0*BIAST1-TARGP1(1))
     +     / (TARGP1(2)-TARGP1(1))
       PTARGT(2) = (TARGP2(1) + PROPN*(TARGP2(2)-TARGP2(1))) * 
     +     1000.0d0/BIAST2
      ENDIF

C     Generate 1+ carrying capacity for trials starting after catching begun
      IF (LPRJ) K1(1) = PRIOR(CARRYT1,CARRYP1,ISEED)*1000.0
      IF (LPRJ) K1(2) = PRIOR(CARRYT2,CARRYP2,ISEED)*1000.0
C
C     Generate an additional CV for the calf data
      CVADDC = PRIOR(CVCADT,CVCADP,ISEED)
C
C     Generate an extra mortality
      EXTRAS = PRIOR(EXTRAST,EXTRASP,ISEED)
C
      RETURN
      END
C
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION HITPYR (PARS, IERR)
C
C  Step A8c
C  HITPYR calculates the difference between the target hit and the
C  target sought (=PTARGT in IYRTAR) for the population shooting routine
C
C  PARS(1) = Initial 1+ population size to be used
C
      USE DECL_STKVRS
      USE DECL_CONVRS
      USE DECL_PCFG

      DOUBLE PRECISION PARS(1),PINIT,PHIT
      INTEGER IERR,OK

C     Project the population ahead given PARS
      IERR = 0
      
      PINIT = PARS(1)
      IF (INITYR.EQ.IYRPRJ) THEN
C       Start model when population pristine. PARS(1) = Test value for K
        PINIT = MAX(7000.D0, MIN(60000.D0,PINIT))
        CALL SETKA(PINIT,PINIT,PHIT,OK,ISPass,IYRMANPass)
      ELSE
C       Start model in more recent year.
C       PARS(1) = Test value of P at start of projection. K is fixed =generated parameter
        PINIT = MIN(K1(ISPass),PINIT)
        CALL SETKA(K1(ISPass),PINIT,PHIT,OK,ISPass,IYRMANPass)
      ENDIF
      CALL ComputePSURVObs(INITYR,FracObs)
      DO 1000 IYR = IYRPRJ,-1
       CALL ComputePSURVObs(IYR,FracObs)
1000  CONTINUE         

C     Population went -ve if OK=0
      IF (OK.EQ.0) THEN
        HITPYR = -PTARGT(IsPass)
      ELSE
C       Find the difference between the target and projected values
        HITPYR = PHIT - PTARGT(ISpass)
      ENDIF
C      IF (ISpass.EQ.2) WRITE(*,'(1x,3F9.2,1x,I2,2F9.2)') 
C     +   PARS(1),PINIT,HITPYR,OK,PHIT,PTARGT(ISpass)
C
      RETURN
      END
C
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION HITPEG (PARS, IERR)
C
C  Subroutine calculates the difference between the target maximum pregnancy
C  rate and the model prediction
C
      USE DECL_STKVRS
      USE DECL_CONVRS
      USE DECL_SETUP

      DOUBLE PRECISION PARS(1)
      INTEGER IERR
C
C     Local variables
      INTEGER Age,OK
      DOUBLE PRECISION INVFEC
C
C     Check for out of bounds
      IERR = 0
      IF (PARS(1).LE.   0.d0) PARS(1) = 0.000001d0
      IF (PARS(1).GE.100000.d0) PARS(1) = 100000.d0
C
C     Set up juv. SURvival array (adult values set in GENPAR)
      JUVS(ISPass) = EXP(-PARS(1)/1000.d0)
      DO 1010 Age = 0,TRANSA
        SUR(ISPass,Age) = JUVS(ISPass)
1010  CONTINUE
      IF (TRANSA.GT.0) SUR(ISPass,0) = JUVS(ISpass)*ADULTS
C
C     Find the difference between the target and projected value  {Eqn A7.4}
      OK = 1
      CALL MSYPAR (OK,ISPass)
      IF (OK.EQ.1) THEN
        INVFEC = FEC(ISPass)*(1.0d0 + A1(ISpass))/
     +         (1.d0+FEC(ISpass)*(1.d0+A1(ISpass))*SUMSUR(ISPass))
        HITPEG = (PREGTG(ISPass) - INVFEC)*10000.d0
      ELSE
        HITPEG = 100000.D0
      ENDIF

      RETURN
      END
      
C **********************************************************************
C
      SUBROUTINE FINDEXTRAS(IYRMAN)
C
C This subroutine applies bisection to find the extra S which keeps the populations stable
C
      USE DECL_STKVRS
      USE DECL_CONVRS

      INTEGER IYRMAN

      DOUBLE PRECISION SUNRM(2,0:MXAGE),SUNRF(2,0:MXAGE),
     +    SRM(2,0:MXAGE),SRF(2,0:MXAGE),SPSURV(2),CM,CF,
     +    SMIN,SMAX,DEPL(2)
      INTEGER IS,L,Istep,OK,AGE
C
C     Save the age structure
      DO 10000 IS = 1,2
       SPSURV(IS) = PSURV(IS,-1)
       DO 10100 L = 0,MXAGE
        SRM(IS,L) = RM(IS,L)
        SRF(IS,L) = RF(IS,L)
        SUNRM(IS,L) = UNRM(IS,L)
        SUNRF(IS,L) = UNRF(IS,L)
10100  CONTINUE        
10000 CONTINUE 
C
C     Reference level of surviva;    
      SMIN = SUR(1,MAXAGE)
      SMAX = 1.000

      DO 20000 Istep = 1,30
       ADULTS = (SMIN+SMAX)/2.0
C
C      Specify survival
       DO 21000 IS = 1,2
        DO 21000 AGE = TRANSA+1,MAXAGE
         SUR(IS,AGE) = ADULTS
21000  CONTINUE         
C
C      Compute the projected result
       DEPL = 0
       DO 22000 IS = 1,2
        DO 22100 IYR = -1,IENDY2-1 
         CM = 0.0d0
         CF = 0.0d0
         CALL STKUPA (IYR,CM,CF,OK,IS,IYRMAN)
          IF (IYR.GE.IENDY2-50) DEPL(IS) = DEPL(IS) + PSURV(IS,IYR)
22100   CONTINUE         
22000  CONTINUE      
       DEPL(1) = DEPL(1)/PSURV(1,INITYR)/50.0
       DEPL(2) = DEPL(2)/PSURV(2,INITYR)/50.0
C
C      Bisect
       IF (DEPL(1)+DEPL(2).LT.2) THEN
        SMIN = ADULTS
       ELSE
        SMAX = ADULTS
       ENDIF
C
C      Restore the age structure
       DO 23000 IS = 1,2
        PSURV(IS,-1) = SPSURV(IS)
        DO 23100 L = 0,MXAGE
         RM(IS,L) = SRM(IS,L)
         RF(IS,L) = SRF(IS,L)
         UNRM(IS,L) = SUNRM(IS,L)
         UNRF(IS,L) = SUNRF(IS,L)
23100   CONTINUE        
23000  CONTINUE 
C
20000 CONTINUE
C
      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE PNTOUT (IOUT,NYEAR,IQUOTA,PARFIL,IYRMAN)

C     Print parameter list & check values are within range.

      USE DECL_STKVRS
      USE DECL_SETUP
      USE DECL_CONVRS
      USE DECL_PCFG
      USE DECL_DATVRS

      INTEGER IOUT,NYEAR,I,IQUOTA(2),IYRMAN
      CHARACTER OPT(-3:3)*50,PARFIL*12
      LOGICAL ERR

      ERR = .FALSE.
      OPT(-1) = '**** ERROR: UNKNOWN VALUE'

      OPT(0) = '1+ population'
      OPT(1) = 'Mature population'
      I = OPTF
      IF (I.LT.0 .OR. I.GT.1) CALL SETERR(ERR,I)
      WRITE (IOUT,95)'Option specifying MSYR',OPTF,OPT(I)
      I = OPMSYL
      IF (I.LT.0 .OR. I.GT.1) CALL SETERR(ERR,I)
      WRITE (IOUT,95)'Option specifying MSYL',OPMSYL,OPT(I)
      I = OPTDD
      IF (I.LT.0 .OR. I.GT.1) CALL SETERR(ERR,I)
      WRITE (IOUT,95)'Option specifying density dependence',OPTDD,OPT(I)

      OPT(0) = 'Linear bias'
      OPT(1) = 'Sinusoidal bias'
      I = OPTB
      IF (OPTB.LT.0 .OR. OPTB.GT.1) CALL SETERR(ERR,I)
      WRITE (IOUT,95) 'Variable bias option',OPTB,OPT(I)

      OPT(0) = 'Stochastic'
      OPT(1) = 'Deterministic'
      I = OPTDET
      IF (OPTDET.LT.0 .OR. OPTDET.GT.1) CALL SETERR(ERR,I)
      WRITE (IOUT,95) 'Data generation (abundance & age)',OPTDET,OPT(I)
      OPT(-2) = 'Gamma density dependence'
      OPT(0) = 'Deterministic PellaTomlinson model'
      OPT(1) = 'Stochastic, no correlation'
      OPT(2) = 'Stochastic, + correlation'

      WRITE (IOUT,95) 'Number of trials',NTRIAL
      WRITE (IOUT,95) 'Number of years in simulation',NYEAR
      WRITE (IOUT,95) 'Year projection begins', IYRPRJ
      WRITE (IOUT,95) 'Year 0 for calculations',IYRMAN
      WRITE (IOUT,96) 'MSYL (North)',' ',MSYLP1(1)
      WRITE (IOUT,96) 'MSYL (PCFG)',' ',MSYLP1(2)
      IF (MSYRP1(1).EQ.MSYRP1(2)) THEN
        WRITE (IOUT,96) 'MSY rate (North)', ' ',MSYRP1(1)
      ELSE
        WRITE (IOUT,96) 'MSY rate (North)', ' ',MSYRP1(1),
     +   ' to',MSYRP1(2)
      ENDIF
      IF (MSYRP2(1).EQ.MSYRP2(2)) THEN
        WRITE (IOUT,96) 'MSY rate (PCFG)', ' ',MSYRP2(1)
      ELSE
        WRITE (IOUT,96) 'MSY rate (PCFG)', ' ',MSYRP2(1),
     +   ' to',MSYRP2(2)
      ENDIF
      WRITE (IOUT,96) 'Immigration rate',' ',IMMIREF
      WRITE (IOUT,96) 'Prob (Dec-May)=PCFG',' ',PhiHistPCFG
      WRITE (IOUT,96) 'Prob (South)=PCFG',' ',PhiHistSOUTH
      WRITE (IOUT,95) 'Maximum age class',MAXAGE
      WRITE (IOUT,96) 'Future catch ratio (North)','CRATIO',CRATIO(1)
      WRITE (IOUT,96) 'Future catch ratio (PCFG)','CRATIO',CRATIO(2)
      WRITE (IOUT,96) 'Historic catch bias','BIASC',BIASC
      WRITE (IOUT,95) 'Density dependent time lag',ILAG
      WRITE (IOUT,95) 'Number of epidemics',NEVENT
      WRITE (IOUT,96) 'Level of need (North)','Final',NEEDF(1)
      WRITE (IOUT,96) 'Level of need (PCFG)','Final',NEEDF(2)
      WRITE (IOUT,96) 'Bias in abundance ests (North)','Initial:',
     +                 BIAS(1,-1),'Final',BIAS(1,NYEAR-1)
      WRITE (IOUT,96) 'Bias in abundance ests (PCFG)','Initial:',
     +                 BIAS(2,-1),'Final',BIAS(2,NYEAR-1)
      WRITE (IOUT,96) 'Prob (PCFG=North stock)',' ',ProbFalseNeg
      WRITE (IOUT,96) 'Prob (North=PCFG stock) (yr 0)',' ',
     + ProbFalsePos1
      WRITE (IOUT,96) 'Prob (North=PCFG stock) (yr 99)',' ',
     + ProbFalsePos2
      WRITE (IOUT,96) 'Prob (struck & lost)',' ',ProbStrk
      WRITE (IOUT,96)'Expected value of CV est (North)','CV(est)',
     +   CV1EST(1)
      WRITE (IOUT,96)'Expected value of CV est (PCFG)','CV(est)',
     +   CV1EST(2)
      WRITE (IOUT,95) 'Frequency of abundance ests (North)',IFREQ(1)
      WRITE (IOUT,95) 'Frequency of abundance ests (PCFF)',IFREQ(1)
      WRITE (IOUT,95) 'Length of quota period (North)',IQUOTA(1)
      WRITE (IOUT,95) 'Length of quota period (PCFG)',IQUOTA(2)
      WRITE (IOUT,95) 'Minimum calving interval',NY
      WRITE (IOUT,95) 'Target year',IYRTAR
      WRITE (IOUT,96) 'Aboriginal catch multiplier','ABOMUL',ABOMUL
      WRITE (IOUT,97) 'Parameter file',PARFIL
      WRITE (IOUT,97) 'Fishery 2: Bowheads;  format',
     +  '(I4,1x,I4,1x,6F8.0,12F8.2,2(F8.1,F8.4),2F8.0,2F8.1,2F8.0,
     +   6(I8),3(F8.2))'
      WRITE (IOUT,'(/A,(T8,10F8.2))')' Need:',(NEED(1,I),I=0,NYEAR-1,
     +                                         IQUOTA(1))
      WRITE (IOUT,'(/A,(T8,10F8.2))')' Need:',(NEED(2,I),I=0,NYEAR-1,
     +                                         IQUOTA(2))
      IF (ERR) STOP

   95 FORMAT (1X,A,T41,I6,4X,A)
   96 FORMAT (1X,A,T35,A8,2(F8.3,2X,A,:': '),I5)
   97 FORMAT (1X,A,T41,A)
   98 FORMAT (1X,A,T35,A8,F8.3,2X,A,F12.9)

      RETURN
      END

      SUBROUTINE SETERR(ERR,I)
      INTEGER I
      LOGICAL ERR
      ERR = .TRUE.
      I = -1
      RETURN
      END

C *********************************************************************

      SUBROUTINE REPORT (IOUT,NYEAR,SIGHT,CVX,ISUR1,CATM,CATF,IYRMAN)

C     REPORT prints out population size & catch arrays from year 0

      USE DECL_STKVRS
      USE DECL_PCFG
      USE ALT_PCFG  ! for stock_strikes(2,-100:100), and stock_landings(2,-100:100)

      DOUBLE PRECISION SIGHT(2,I1SYR:ICEND),
     +                 CVX(2,I1SYR:ICEND),
     +                 CATM(4,I1CYR:ICEND),CATF(4,I1CYR:ICEND)
      INTEGER IOUT,NYEAR,IYR,ISUR1,IS,IYRMAN
      !INTEGER stock_strikes(2,-100:100) 
      !INTEGER stock_landings(2,-100:100)


      !print *, "HELLO FROM REPORT"
      !PRINT *, "I1CYR", I1CYR
      !PRINT *, "ICEND", ICEND
      !STOP  !


      WRITE (IOUT,'(A4,1x,A4,1x,40(A8))') 'Year','Year',
     +   ('MtFem','1+Pop','SurvP',I=1,2),
     +   '1-CM','1-CF','2-CM','2-CF',
     +   'A-CM','A-CF','B-CM','B-CF','C-CM','C-CF','D-CM','D-CF',
     +   'SIGHT-A','CV-A','SIGHT-B','CV-B',
     +   'CALF-1','CALF-2','RY-1','RY-2','NEXP-1','NEXP-2',
     +   'ABL','Strike','Landed','S&L','NA','Reason',
     +   'INC-1','INC-2','Immigr', 
     +   'strik_1','landd_1'
! CATNORTH, CATPCFG, stock_landings(1, IYR), stock_strikes(1, IYR)                       
      IYR= INITYR  ! Appears variable INITYR has been declared with global scope and set above
      ! Debugging JRB
      !print *, ""
      !print *, "Hello from procedure: REPORT"
      !print *, "IYR, e.g. INITYR", IYR
      !print *, "IYRMAN", IYRMAN
      !print *, "IYR+IYRMAN", IYR+IYRMAN
      !print *, "IYRPRJ", IYRPRJ
      !print *, "ISUR1", ISUR1
      !print *, "NYEAR", NYEAR
      !stop

      WRITE(IOUT,99) IYR,IYR+IYRMAN,
     +               (NINT(PMATF(IS,IYR)),NINT(PSURV(IS,IYR)),
     +                NINT(PSURVOBS(IS,IYR)),IS=1,2),
     +               -1.0,-1.0,-1.0,-1.0,
     +               -1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,-1.0,
     +               -1.0d0,-1.0d0,-1.0d0,-1.0d0,
     +                -1,-1,-1.0,-1.0,
     +               (NINT(NEXP(IS,IYR)),IS=1,2),-1,-1,-2,-2,-2,-2,
     +                -1.0,-1.0,-1.0, -1.0, -1.0
      DO IYR = IYRPRJ,ISUR1-1
       WRITE(IOUT,99) IYR,IYR+IYRMAN,
     +               (NINT(PMATF(IS,IYR)),NINT(PSURV(IS,IYR)),
     +                NINT(PSURVOBS(IS,IYR)),IS=1,2),
     +               (CATCHM(IS,IYR),CATCHF(IS,IYR),IS=1,2),
     +               (CATM(IS,IYR),CATF(IS,IYR),IS=1,4),
     +               -1.0d0,-1.0d0,-1.0d0,-1.0d0,
     +               (NINT(PCALF(IS,IYR)),IS=1,2),
     +               -1.0,-1.0,
     +               (NINT(NEXP(IS,IYR)),IS=1,2),-1,-1,-2,-2,-2,-2,
     +               (INCCAT(IS,IYR),IS=1,2),TOTALIMMI(IYR),
     +               stock_strikes(1,IYR), stock_landings(1,IYR)
      END DO
      DO IYR = ISUR1,NYEAR-1
       WRITE(IOUT,99) IYR,IYR+IYRMAN,
     +               (NINT(PMATF(IS,IYR)),NINT(PSURV(IS,IYR)),
     +                NINT(PSURVOBS(IS,IYR)),IS=1,2),
     +               (CATCHM(IS,IYR),CATCHF(IS,IYR),IS=1,2),
     +               (CATM(IS,IYR),CATF(IS,IYR),IS=1,4),
     +               (SIGHT(IS,IYR),CVX(IS,IYR),IS=1,2),
     +               (NINT(PCALF(IS,IYR)),IS=1,2),
     +               -1.0,-1.0,
     +               (NINT(NEXP(IS,IYR)),IS=1,2),ABL(IYR),Strikes(IYR),
     +               Landed(IYR),StruckLost(IYR),
     +               PCFGDesignated(IYR),Reason(IYR),
     +               (INCCAT(IS,IYR),IS=1,2),TOTALIMMI(IYR),
     +               stock_strikes(1,IYR), stock_landings(1,IYR)
      END DO
      WRITE (IOUT,99) NYEAR,NYEAR+IYRMAN, 
     +  (NINT(PMATF(IS,NYEAR)),NINT(PSURV(IS,NYEAR)),
     +   NINT(PSURVOBS(IS,IYR)),IS=1,2)
   99 FORMAT (I4,1x,I4,1x,6I8,12F8.2,2(F8.1,F8.4),2I8,2F8.1,2I8,
     +   6(I8),55555(F8.2))
      RETURN
      END

      
C *********************************************************************

      SUBROUTINE debug(trial, year, msg)
      ! Print output for debugging and stop execution
      ! The argument 'msg' should correspond to the nine characters "line ####"
      ! John R Brandon
        use DECL_PCFG  ! Variables from 2012
        use ALT_PCFG   ! "            " 2017

        implicit none

        integer(kind = 4) :: trial, year
        character(9) :: msg

        print *, ""
        print *, "Inside debug()"
        print *, msg
        print *, "Trial: ", trial
        print *, "IYR  : ", year
        print *, ""
        stop  

        RETURN
      END

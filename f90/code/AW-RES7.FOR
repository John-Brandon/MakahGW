C     Last change:  CA    1 May 2002   11:53 am
      PROGRAM AWRES7
C
C     AW-RES Version 7                             6 March 2002
C
C  The program reads in the results from the aboriginal management trials
C  and calculates the following required summary statistics.
C  (Reference: SC/51/4 Annex E Appendix 3A & B)
C
C  This version contains an option (ISRT) for the data to be sorted    or
C  to be output by simulation number (i.e. unsorted)
C
C  PZ = P* = Population size under scenario of zero strikes from t=0
C  Results calculated for mature females (F) & 1+ (1) components of population
C
C  Risk:  (D: depletion)
C   D1  Final depletion P(T)/K  (DEPLF & DEPL1)
C   D2  Lowest depletion reached in each trial (DFMIN & D1MIN)
C   D6  Stock sizes (P and P*) for simulations 1 and 2 from t=0 (PFN & P1N)
C   D7  List of the Xth %ile of the distributions of P(t) and P*(t)
C       where X=5 & 50   (PX5 & PX50)
C   D8  Scaled Final population size = P(T)/P*(T)  (PFINF & PFIN1)
C   D9  Minimum population size (PMINF & PMIN1)
C   D10 Relative increase P(T)/P(0)  (D10F & D101)
C
C  Need: (N)
C   N1  Total catch in each trial / total need (CTOTAL)
C   N2  - max. number of consecutive years in which C(t)<Need(t)/T  (-CYRS/T)
C   N3  Fraction of years in which C(t) >=  Need(t)/2  (CN3)
C   N4  Fraction of years in which C(t) >=  Need(t)    (CN4)
C   N7  List of the Xth %ile of the distribution of V(t) = (Ct-Needt)/Needt
C   N8  List of V(t) for simulations 1 and 2. (VN)
C   N9  Average need satisfaction (CN9)
C   N10 Average annual variation in catch (CN10)
C   N11 Anti-curvature catch variation statistic (CN11)
C   N12 Mean downstep (or modified AAV) (CN12)
C   N13 Average number of animals landed (CN13)
C   N14 Average number of animals struck and lost (CN14)
C   N15 Average number of animals killed by stock (CN15)

C  Recovery: (R)
C   R1  Relative recovery (RR1) = stock level in yr IZMSY when P* passes MSYL.
C   R2  Final recovery = #[P(T)>=MSYL] / #[P*(T)>=MSYL]    (FINR/FINRZ)
C   R3  Time frequency in recovered state                  (RECOVF & RECOV1)
C       = # of yrs after i for which P>0.9MSYL/(T-i+1)  where i=1st year P>=MSYL
C   R4  Relative time to recovery (RTRF & RTR1)
C
C  Tuning:
C   T   sum of squares of differences between 'perfect' and actual strike limits
C
C INPUT AND OUTPUT FILES ------------------------------------------------------
C   Input:  AW-RES   SLC results = output from F2.FOR
C           AW-RESZ  Zero management catch results = F2Z.FOR output
C
C   Output:
C   AW-SUM.OUT Summary of input parameters + output statistics 
C   AW-1-2.OUT Trajectories for first 2 simulations  [D6 & N8]
C           +  Xth %ile of P(t),PZ(t) & V(t) distributions [D7 & N7]
C           +  list of initial, final & minimum depletions by trial: for information
C   RSK-RCOV.100 Complete set of risk (depletion) stats for T=100 [D1,2]
C              + complete set of recovery statistics [R1,3 & 4]
C   NEED.20   Complete set of need statistics for 20yr time period [N1-N4,N9]
C   NEED.100  Complete set of need statistics for 100yr time period [N1-N4,N9-11]
C
C   AW-ALL.OUT Summary results can be appended to this file if required.  
C              Otherwise these lines can be commented out.
C
C PARAMETERS ------------------------------------------------------------------

C   PF       Number of mature females at start of year IYR (before taking catch)
C   P1       Number of 1+ whales at start of current year
C   C        Catch in current year (as set by management procedure)
C   NEED(IYR)Holds the total need for IQUOTA years in the last of the
C            years to which it applies (and =0 for other years)
C   PFZ(N,IYR)The number of mature females in IYR in trial NTRIAL
C             under a zero catch scenario.  It is read from file AW-RESZ
C   P1Z(N,IYR)No.in 1+ component of population in IYR under 0 catch scenario.
C   PFI(N,IYR)The number of mature females in IYR in trial NTRIAL
C             under a zero strike scenario.  It is read from file AW-RESI
C   P1I(N,IYR)No.in 1+ component of population in IYR under 0 strike scenario.
C   NTRIAL   Number of trials (or replicates) = NT.  Read in.  1-400
C   NYEAR    Number of years of management in simulation.  Read in. 1-100
C   KF       Number of mature females in pristine population
C   K1       Number of 1+ whales in pristine population
C   FISHRY   = 2 if population parameters change for every trial
C            i.e. they have different trajectories in the zero strike case
C   IYR      Current year
C   N        Current trial number (1,NTRIAL)
C   REF      Reference number of run
C   NTIME    No. of time periods
C   ISTART   Starting year of population trajectory (in input file)


C DEFINITIONS -----------------------------------------------------------------

      INTEGER IEND,NPRD
      PARAMETER (IEND=100, NPRD=100)

C      PARAMETER (IEND=300, NPRD=120)                                     ANDRE'S VERSION

      REAL NT,CM(2),CF(2),C(2),PF(2),P1(2),DF(2),D1(2),KF(2),K1(2),
     +   P1Z(2,100,0:IEND),PFZ(2,100,0:IEND),
     +   P1I(2,100,0:IEND),PFI(2,100,0:IEND),
     +   P1Z2(2,100,-500:IEND),PFZ2(2,100,-500:IEND),
     +   P1I2(2,100,-500:IEND),PFI2(2,100,-500:IEND),
     +   DEPLF(2,100,2),DEPL1(2,100,2),
     +   PMATF(2,100,0:IEND), PSURV(2,100,0:IEND),
     +   PSURV2(2,100,-500:IEND),PMATF2(2,100,-500:IEND),
     +   DFMINA(2),DFMIN(2,100,2),D1MINA(2),D1MIN(2,100,2),
     +   PMINFA(2),PMIN1A(2),
     +   PFINF(2,100),PFIN1(2,100),PFINFI(2,100),PFIN1I(2,100),
     +   PMINF(2,100),PMIN1(2,100),
     +   D10F(2,100),D101(2,100),DFINIT(2,100),D1INIT(2,100),
     +   X5,X50,MED1,MED2,MED2A,MED3,
     +   NEED(2,0:IEND-1),NTOT(2,2),CTOT(2),CTOTAL(2,100,2),CGTOT(2),
     +   CYRS(2,100,2),       
     +   CN3A(2),CN3(2,100,2),CN4A(2),CN4(2,100,2),
     +   CN9(2,100,2),CN10(2,100,2),
     +   CFRAC,
     +   CN11(2,100,2),CN12(2,100,2),CN10A(2),CN11A(2),CN12A(2),
     +   C1(2),C2(2),CPREM(2),CN15A(2),
     +   V(2,100,1:NPRD),VN(2,2,1:NPRD),VX5(2,1:NPRD),VX50(2,1:NPRD),
     +   SATIS(2),
     +   MSYLF(2),MSYL1(2),PFMSYL(2),P1MSYL(2),
     +   RRF(2,100,2),RR1(2,100,2),                   
     +   RECOVF(2,100,2),RECOV1(2,100,2),RTRF(2,100,2),RTR1(2,100,2),
     +   Q(2,2),ERR(100),
     +   STRUCK,LANDED,CN13(2,100,2),CN14(2,100,2),CN15(2,100,2),
     +   CSTOR(2,100,0:IEND),CMS(2),CFS(2)
      INTEGER ISRT,I,IYR,NYEAR,IT,ITYEAR(2),IZ,NTRIAL,N,NZ,NREAD,FISHRY,
     +   IN,IN2,IOUT,IOUT2,IX5,IX50,IXZ5,IXZ50,NTIME,IQUOTA(2),
     +   NQ(2,2),ISTART,
     +   PFN(2,2,0:IEND),PFZN(2,2,0:IEND),                                  [D]
     +   P1N(2,2,0:IEND),P1ZN(2,2,0:IEND),
     +   PFX50(2,0:IEND),PFZX50(2,0:IEND),
     +   P1X50(2,0:IEND),P1ZX50(2,0:IEND),
     +   PFX5(2,0:IEND),PFZX5(2,0:IEND),
     +   P1X5(2,0:IEND),P1ZX5(2,0:IEND),   IC(2),ICY(2),IP(2),              [N]
     +   IZFMSY(2),IZ1MSY(2),FINRF(2),FINR1(2),FINRZF(2),FINRZ1(2),
     +   IFMSYL(2),I1MSYL(2),                                               [R]
     +   NFMSYL(2),N1MSYL(2),R11(2),R1F(2),  I5,I50,I96,
     +   ICATPCFG(6),REASON(5),LANDS(100,0:IEND),BLOCKSTART
      CHARACTER REF*10, REFZ*10, REFI*10, DESC*50, PROG*21, LINE*399,
     +   OUTDR(2)*12, OUTN(2)*12, TITLE*1, FMT*105
      EXTERNAL MED1,MED2,MED2A,MED3
      PARAMETER (CFRAC=0.5)                                                [N3]
      PARAMETER (NTIME=2)
      PARAMETER (X5=5., X50=50.)                                          [D7,N7]
      DATA OUTDR(2)/'RSK-RCOV.100'/, OUTN/'NEED.20','NEED.100'/
      DATA IN/10/,IN2/11/,IN3/120/,IOUT/12/,IOUT2/16/
      OPEN (IN,  FILE='AW-RES')
      OPEN (IN2, FILE='AW-RESZ')
      OPEN (IN3, FILE='AW-RESI')
      OPEN (IOUT, FILE='AW-SUM.OUT')
      OPEN (IOUT2, FILE='AW-1-2.OUT')

C     Set ISRT=1 for sorted output,  ISRT=0 for unsorted output
      ISRT = 1

C INITIALISATION --------------------------------------------------------------


C     Read in required parameters and copy lines defining run parameters
C          to output.  Skip the same lines in the zero catch file
      READ (IN,'(/7X,A10,A/A/)') REF,PROG,DESC
      WRITE (IOUT,'(/A,A10,A/A/)') ' Ref: ',REF,PROG,DESC
      READ (IN2,'(/7X,A10//)') REFZ
      READ (IN3,'(/7X,A10//)') REFI
      IF (REF.NE.REFZ) STOP 'File reference numbers do not match'
      IF (REF.NE.REFI) STOP 'File reference numbers do not match'
      DO 5 I=1,200
        READ (IN, '(A)') LINE
        READ (IN2,'()') 
        READ (IN3,'()') 
        WRITE (IOUT,'(1X,A)') LINE
        IF (LINE(2:8) .EQ.'Fishery') GO TO 6
        IF (LINE(2:16).EQ.'Number of years') READ (LINE,99) NYEAR
        IF (LINE(2:16).EQ.'Number of trial') READ (LINE,99) NTRIAL
        IF (LINE(2:31).EQ.'Length of quota period (North)') 
     +               READ (LINE,99) IQUOTA(1)
        IF (LINE(2:31).EQ.'Length of quota period (PCFG) ') 
     +               READ (LINE,99) IQUOTA(2)
   99 FORMAT (T41,I6)
    5 CONTINUE
    6 READ (LINE,'(8X,I2,T41,A)') FISHRY,FMT

C     Set periods for which to set statistics (i.e. set T)
      ITYEAR(1) = NYEAR / 5
      ITYEAR(2) = NYEAR

C     Initialise NEED array
C     NEED holds the total need for IQUOTA years in the last of the
C        years to which it applies (and =0 for other years) - this is
C        is different from that in F2-TST.FOR:
C     NB Requires year of last catch to coincide with last year of quota
      DO 30 IS = 1,2
       DO 30 IYR = 0,NYEAR-1
        NEED(IS,IYR)  = 0.D0
   30 CONTINUE
      READ (IN,'(/ (7X,10F8.2))')(NEED(1,IYR),
     +                      IYR=IQUOTA(1)-1,NYEAR-1,IQUOTA(1))
      READ (IN,'(/ (7X,10F8.2))')(NEED(2,IYR),
     +                      IYR=IQUOTA(2)-1,NYEAR-1,IQUOTA(2))
C     Skip same no. of lines of IN2
      READ (IN2,'(/ (A1))') (LINE,I=IQUOTA(1)-1,NYEAR-1,IQUOTA(1)*10)
      READ (IN2,'(/ (A1))') (LINE,I=IQUOTA(2)-1,NYEAR-1,IQUOTA(2)*10)
C     Skip same no. of lines of IN3
      READ (IN3,'(/ (A1))') (LINE,I=IQUOTA(1)-1,NYEAR-1,IQUOTA(1)*10)
      READ (IN3,'(/ (A1))') (LINE,I=IQUOTA(2)-1,NYEAR-1,IQUOTA(2)*10)
C     Sum total need over each time period
      DO 40 IS = 1,2
       DO 40 IT = 1,NTIME
        NTOT(IS,IT) = 0.0
        DO 40 IYR = IQUOTA(IS)-1,ITYEAR(IT)-1,IQUOTA(IS)
          NTOT(IS,IT) = NTOT(IS,IT) + NEED(IS,IYR)
   40 CONTINUE

      NT = REAL(NTRIAL)
      R11 = 0                                                              [R1]
      R1F = 0                                                              [R1]
      FINRF = 0                                                            [R2]
      FINR1 = 0                                                             . 
      FINRZF = 0                                                            . 
      FINRZ1 = 0                                                           [R2]
      C = 0.0                                                               [N]
      CGTOT = 0.0                                                         [N1tot]
      REASON = 0


C READ DATA AND INCREMENT STORED VARIABLES ------------------------------------

      DO 200 N = 1,NTRIAL

C       Read trial heading line & check that trial number is correct
        READ (IN, '(/ 6X,I4//)') NREAD
        IF (NREAD.NE.N) THEN
          WRITE (IOUT,*) ' **** ERROR READING TRIAL NUMBER', N,NREAD
          STOP ' **** ERROR IN READING TRIAL NUMBER'
        ENDIF
C
C       Read zero catch trajectories. Set NZ = pointer to current 0 trajectory
        IF (N.EQ.1 .OR. FISHRY.EQ.2) THEN
          READ (IN2,'(//12X,2(8X,F8.5,F8.5) //)') MSYL1(1),MSYL1(2),
     +        MSYLF(1),MSYLF(2)
          READ (IN2,FMT) IZ,II,KF(1),K1(1),P1A,KF(2),K1(2)
          READ (IN2,FMT) ISTART
          DO 50 IYR=ISTART+1,-MAX(IQUOTA(1),IQUOTA(2))-1
            READ (IN2,*)
   50     CONTINUE
C         Sum catch in last IQUOTA years prior to management
          CPREM(1) = 0.0
          CPREM(2) = 0.0
          DO 51 IYR=-MAX(IQUOTA(1),IQUOTA(2)),-1
            READ (IN2,FMT) I,II,PF(1),P1(1),P1A,PF(2),P1(2),P1A,
     +           CM(1),CF(1),CM(2),CF(2)
            IF (IYR.GE.-IQUOTA(1)) CPREM(1) = CPREM(1) + CM(1)+CF(1)
            IF (IYR.GE.-IQUOTA(2)) CPREM(2) = CPREM(2) + CM(2)+CF(2)
   51     CONTINUE
          DO 52 IYR=0,NYEAR
            READ (IN2,FMT) I,II,PFZ(1,N,IYR),P1Z(1,N,IYR),P1A,
     +                          PFZ(2,N,IYR),P1Z(2,N,IYR),P1A
   52     CONTINUE
          NZ = N

C         Set IZFMSY & IZ1MSY = yr in which zero trajectory passes MSYL    [R1]
C         Use final year if zero trajectory never reaches MSYL
C         Also count number of trajectories with P0 > MSYL.
          DO 67 IS = 1,2
           P1MSYL(IS) = K1(IS) * MSYL1(IS)                                 [R1-3]
           IF (P1Z(IS,N,0) .GT. P1MSYL(IS)) R11(IS) = R11(IS) + 1
           DO 60 II=0,NYEAR-1
            IF (P1Z(IS,N,II) .GE. P1MSYL(IS)) GO TO 62                     [R1]
   60      CONTINUE
   62      CONTINUE
           IZ1MSY(IS) = II
           PFMSYL(IS) = KF(IS) * MSYLF(IS)                                 [R1-3]
           IF (PFZ(IS,N,0) .GT. PFMSYL(IS)) R1F(IS) = R1F(IS) + 1
           DO 64 II=0,NYEAR-1
            IF (PFZ(IS,N,II) .GE. PFMSYL(IS)) GO TO 65                     [R1]
   64      CONTINUE
   65      CONTINUE   
           IZFMSY(IS) = II
   67     CONTINUE         
   66   ENDIF

C       Read zero strike trajectories. 
        IF (N.EQ.1 .OR. FISHRY.EQ.2) THEN
          READ (IN3,'(//12X,2(8X,F8.5,F8.5) //)') MSYL1(1),MSYL1(2),
     +        MSYLF(1),MSYLF(2)
          READ (IN3,*) IZ,II,KF(1),K1(1),P1A,KF(2),K1(2)
          READ (IN3,FMT) ISTART
          DO 501 IYR=ISTART+1,-1
            READ (IN3,*)
  501     CONTINUE
          DO 502 IYR=0,NYEAR
            READ (IN3,*) I,II,PFI(1,N,IYR),P1I(1,N,IYR),P1A,
     +                        PFI(2,N,IYR),P1I(2,N,IYR),P1A
  502     CONTINUE
        ENDIF

C       Read pristine population levels and check the 2 files match
        READ (IN, *) 
        READ (IN, *) IYR, II,PF(1),P1(1),P1A,PF(2),P1(2),P1A,
     +           CM(1),CF(1),CM(2),CF(2)
        IF (IYR.NE.IZ .OR. IYR.GE.0 .OR. KF(1).NE.PF(1) .OR. 
     +         K1(1).NE.P1(1) .OR. KF(2).NE.PF(2) .OR. K1(2).NE.P1(2))
     +     STOP '**** ERROR: files do not match in initial year'
C       Skip any lines before management begins
        DO 68 IYR=ISTART,-1
          READ (IN,*) I,II,(PMATF2(IS,N,IYR),PSURV2(IS,N,IYR),
     +     P1A,IS=1,2)
   68   CONTINUE

C       Initialise trial parameters
        IT = 1
        DFMINA = 1.E8                                                      [D2]
        D1MINA = 1.E8                                                      [D2]
        PMINFA = 1.E8                                                      [D9]
        PMIN1A = 1.E8                                                      [D9]
        CTOT = 0.0                                                         [N1]
        IC = 0                                                             [N2]
        ICY = 0                                                            [N2]
        CN3A = 0.0                                                         [N3]
        CN4A = 0.0                                                         [N4]
C AEP UPDATE        
        NQ(1,1) = ITYEAR(1)/IQUOTA(1)                                      [N7-11]
        NQ(1,2) = ITYEAR(2)/IQUOTA(1)                                      [N7-11]
        NQ(2,1) = ITYEAR(1)/IQUOTA(2)                                      [N7-11]
        NQ(2,2) = ITYEAR(2)/IQUOTA(2)                                      [N7-11]
        SATIS = 0.0                                                        [N9]
        C1 = CPREM                                                       [N10,11]
        C2 = -1.0                                                        [N10,11]
        CN10A = 0.0                                                       [N10]
        CN11A = 0.0                                                       [N11]
        CN12A = 0.0                                                       [N12]
        CN15A = 0.0                                                       [N15]
        LANDED = 0                                                        [N13]
        STRUCK = 0                                                        [N14]
        IFMSYL = 1000                                                      [R3]
        I1MSYL = 1000                                                       .
        NFMSYL = 0                                                         [R3]
        N1MSYL = 0                                                         [R3]
        IP = 0

        DO 100 IYR = 0,NYEAR

C         Read population size & catch in IYR; store & sum variables.
C         Check zero catch file matches in year 0
          READ(IN,FMT) I,II,(PF(IS),P1(IS),P1A,IS=1,2),
     +                  (CMS(IS),CFS(IS),IS=1,2),
     +                  (CM(IS),CF(IS),IS=1,2),(RR,J=1,14),
     +                  (ICATPCFG(J),J=1,6)
          IF (IYR.EQ.0.AND.
     +          (PF(1).NE.PFZ(1,NZ,0).OR.PF(2).NE.PFZ(2,NZ,0))) THEN
                  WRITE(*,*)' * ERROR: file mismatch in year 0',N,
     +                   PF(1),PFZ(1,NZ,0),PF(2),PFZ(2,NZ,0)             
C                  STOP
          ENDIF
          IF (I.NE.IYR) STOP ' * ERROR: file mismatch'

C Risk (depletion) statistics: ------------------------------------------------
          DO 1000 IS = 1,2
           DF(IS) = PF(IS) / KF(IS)
           D1(IS) = P1(IS) / K1(IS)
           IF (DF(IS).LT.DFMINA(IS)) DFMINA(IS) = DF(IS)                   [D2]
           IF (D1(IS).LT.D1MINA(IS)) D1MINA(IS) = D1(IS)                   [D2]
C          Store 1st 2 population trajectories
           IF (N.LE.2) THEN
             PFN(IS,N,IYR) = NINT(PF(IS))                                  [D6]
             P1N(IS,N,IYR) = NINT(P1(IS))                                    .
             PFZN(IS,N,IYR) = NINT(PFZ(IS,NZ,IYR))                           .
             P1ZN(IS,N,IYR) = NINT(P1Z(IS,NZ,IYR))                         [D6]
           ENDIF
           PMATF(IS,N,IYR) = PF(IS)                                        [D7]
           PMATF2(IS,N,IYR) = PF(IS)                                       [ap]
           PSURV(IS,N,IYR) = P1(IS)                                        [D7]
           PSURV2(IS,N,IYR) = P1(IS)                                       [ap]
           PFZ2(IS,N,IYR) =  PFZ(IS,N,IYR)
           PFI2(IS,N,IYR) =  PFI(IS,N,IYR)
           P1Z2(IS,N,IYR) =  P1Z(IS,N,IYR)
           P1I2(IS,N,IYR) =  P1I(IS,N,IYR)
           IF (PF(IS).LT.PMINFA(IS)) PMINFA(IS) = PF(IS)                   [D9]
           IF (P1(IS).LT.PMIN1A(IS)) PMIN1A(IS) = P1(IS)                   [D9]

C Recovery statistics: --------------------------------------------------------
C          I1MSYL = 1000 (& hence IYR < I1MSYL) until P1=P1MSYL.  
           IF (I1MSYL(IS).EQ.1000 .AND. P1(IS).GE.P1MSYL(IS))    
     +                                      I1MSYL(IS) = IYR               [R3]
           IF (IFMSYL(IS).EQ.1000 .AND. PF(IS).GE.PFMSYL(IS)) 
     +                                      IFMSYL(IS) = IYR               [R3]
           IF (IYR.GE.I1MSYL(IS) .AND. P1(IS).GT.0.9*P1MSYL(IS)) 
     +                                      N1MSYL(IS) = N1MSYL(IS) + 1    [R3]
           IF (IYR.GE.IFMSYL(IS) .AND. PF(IS).GT.0.9*PFMSYL(IS)) 
     +                                      NFMSYL(IS) = NFMSYL(IS) + 1    [R3]
 
C *****    Store final statistics for T = ITYEAR. (NB: include stock sizes up to
C *****    year T + catches up to T-1; increment annual catch statistics later)
           IF (IYR.EQ.ITYEAR(IT)) THEN
            DEPLF(IS,N,IT)  = DF(IS)                                       [D1]
            DEPL1(IS,N,IT)  = D1(IS)                                       [D1]
            DFMIN(IS,N,IT)  = DFMINA(IS)                                   [D2]
            D1MIN(IS,N,IT)  = D1MINA(IS)                                   [D2]
C           Store population in year IZ1MSY when P* passes MSYL (or else in IYR)
            IF (PFZ(IS,N,0) .GT. PFMSYL(IS)) THEN
              RRF(IS,N,IT) = MIN (1.0, PF(IS)/PFMSYL(IS))                  [R1]
            ELSE
             RRF(IS,N,IT) = PMATF(IS,N,MIN(IZFMSY(IS),IYR))/
     +                      PFZ(IS,NZ,MIN(IZFMSY(IS),IYR))                 [R1]
            ENDIF
            IF (P1Z(IS,N,0) .GT. P1MSYL(IS)) THEN
              RR1(IS,N,IT) = MIN (1.0, P1(IS)/P1MSYL(IS))                  [R1]
            ELSE
              RR1(IS,N,IT) = PSURV(IS,N,MIN(IZ1MSY(IS),IYR))/
     +                       P1Z(IS,NZ,MIN(IZ1MSY(IS),IYR))                [R1]
            ENDIF
            IF (IFMSYL(IS).EQ.1000) THEN
C             Population has not reached MSYL
              RECOVF(IS,N,IT) = 1.0                                        [R3]
              DO 70 I=0,IYR
C               Set I = first year in which P* >= P(T)
                IF (PFZ(IS,NZ,I).GE.PF(IS)) GO TO 72                       [R4]
   70         CONTINUE
            ELSE
              RECOVF(IS,N,IT) = REAL(NFMSYL(IS))/REAL(IYR-IFMSYL(IS)+1)    [R3]
              I = IYR - IFMSYL(IS) + IZFMSY(IS)                            [R4]
            ENDIF
   72       RTRF(IS,N,IT) = REAL(I) / REAL(IYR)                            [R4]
            IF (I1MSYL(IS).EQ.1000) THEN
              RECOV1(IS,N,IT) = 1.0                                        [R3]
              DO 74 I=0,IYR
                IF (P1Z(IS,NZ,I).GE.P1(IS)) GO TO 76                       [R4]
   74         CONTINUE
            ELSE
              RECOV1(IS,N,IT) = REAL(N1MSYL(IS))/REAL(IYR-I1MSYL(IS)+1)    [R3]
              I = IYR - I1MSYL(IS) + IZ1MSY(IS)                            [R4]
            ENDIF
   76       RTR1(IS,N,IT) = REAL(I) / REAL(IYR)                            [R4]

            CTOTAL(IS,N,IT)= CTOT(IS)/NTOT(IS,IT)                          [N1]
            CYRS(IS,N,IT)  = -REAL(ICY(IS))                                [N2]
            CN3(IS,N,IT)   = CN3A(IS) / REAL(IYR/IQUOTA(IS))               [N3]
            CN4(IS,N,IT)   = CN4A(IS) / REAL(IYR/IQUOTA(IS))               [N4]
            CN9(IS,N,IT)   = SATIS(IS) / REAL(IYR/IQUOTA(IS))              [N9]
            CN10(IS,N,IT) = CN10A(IS) / (CTOT(IS)+CPREM(IS)-C1(IS))        [N10]
            CN12(IS,N,IT) = CN12A(IS) / (CTOT(IS)+CPREM(IS)-C1(IS))        [N12]
            CN11(IS,N,IT) = CN11A(IS) / REAL(NQ(IS,IT)-1)                  [N11]
            IF (IS.EQ.2) THEN
             CN13(IS,N,IT) = LANDED / REAL(IYR)                            [N13]
             CN14(IS,N,IT) = STRUCK / REAL(IYR)                            [N14]
            ENDIF 
            CN15(IS,N,IT) = CN15A(IS) / REAL(IYR)                          [N15]

            IF (IYR.EQ.NYEAR) GO TO 1000
            IF (IS.EQ.2) IT = IT +1 
           ENDIF

C -----------------------------------------------------------------------------
C Catch Statistics: (no value is set in NYEAR)  -------------------------------
C
C          Sum catch over each block quota period
           C(IS) = C(IS) + CM(IS) + CF(IS)
           CSTOR(IS,N,IYR) = CM(IS) + CF(IS)
           CN15A(IS) = CN15A(IS) + CMS(IS)+CFS(IS)                         [N15]
           
           BLOCKSTART = MAX(0,INT(IYR/5))
           IF (IS.EQ.2) LANDS(N,BLOCKSTART) = LANDS(N,BLOCKSTART) + 
     +        ICATPCFG(3)
C
C          Only continue if this the start of block
           IF (NEED(IS,IYR).EQ.0.0) GO TO 1000

C          Need>0 so IYR = last year of a block quota period
C          Note: NEED = the maximum allowed value of C
           IF (C(IS).GT.NEED(IS,IYR)) C(IS) = NEED(IS,IYR)
C          Need statistics:
           CTOT(IS) = CTOT(IS) + C(IS)                                     [N1]
C          IC = current # of consecutive periods for which C=NEED and
C          ICY = largest value of IC in current trial
           IF (C(IS).LT.NEED(IS,IYR)-.02) THEN                             [N2]
            IC(IS) = IC(IS) + 1                                                     .
            IF (IC(IS).GT.ICY(IS)) ICY(IS) = IC(IS)                                         .
           ELSE
C           the Catch quota = NEED (within accuracy of printout)
            IC(IS) = 0                                                     [N2]
            CN4A(IS) = CN4A(IS)+1.0                                        [N4]
           ENDIF
           IF (C(IS).GE.NEED(IS,IYR)*CFRAC) CN3A(IS) = CN3A(IS)+1.0        [N3]
C          Increment NSAT if C >= Need

           IP(IS) = IP(IS)+1
           V(IS,N,IP(IS)) = C(IS) / NEED(IS,IYR)                           [N7]
           IF (N.LE.2) VN(IS,N,IP(IS)) = V(IS,N,IP(IS))                    [N8]
           SATIS(IS) = SATIS(IS) + C(IS)/NEED(IS,IYR)                      [N9]

           CN10A(IS) = CN10A(IS) + ABS(C(IS)-C1(IS))                       [N10]
           IF (C(IS).LT.C1(IS)) CN12A(IS) = CN12A(IS)+ABS(C(IS)-C1(IS))    [N12]
C          C2=-1 on first time thru this loop for each replicate
           IF (C2(IS).GT.-1.) THEN 
            CM(IS) = (C(IS)+C2(IS))*0.5                                    [N11]
            CN11A(IS) = CN11A(IS)+ABS( (C1(IS)-CM(IS)))/MAX(10.0,CM(IS))   [N11]
           ENDIF
C          Store previous 2 quotas
           C2(IS) = C1(IS)                                                 [N11]
           C1(IS) = C(IS)                                                  [N10,11]

C          Reset C for next quota period
           C(IS) = 0.0
C
C          Special for PCFG
           IF (IS.EQ.2.AND.IYR.NE.NYEAR.AND.ICATPCFG(6).NE.0) THEN
            REASON(ICATPCFG(6)) = REASON(ICATPCFG(6)) + 1
            STRUCK = STRUCK + ICATPCFG(4)
            LANDED = LANDED + ICATPCFG(3)
           ENDIF
C           
1000     CONTINUE          

  100   CONTINUE

C End of trial N. -------------------------------------------------------------
        DO 1010 IS = 1,2
C        Store initial depletion levels
         DFINIT(IS,N) = PMATF(IS,N,0)/KF(IS)
         D1INIT(IS,N) = PSURV(IS,N,0)/K1(IS)
C        Store final depletion levels
         PFINF(IS,N) = -1.0                                                 [D8]
         PFIN1(IS,N) = -1.0                                                 [D8]
         IF (PFZ(IS,N,NYEAR).GT.0.0) 
     +      PFINF(IS,N) = PF(IS)/PFZ(IS,N,NYEAR)                            [D8]
         IF (P1Z(IS,N,NYEAR).GT.0.0) 
     +      PFIN1(IS,N) = P1(IS)/P1Z(IS,N,NYEAR)                            [D8]
         PFINFI(IS,N) = -1.0                                                [D8]
         PFIN1I(IS,N) = -1.0                                                [D8]
         IF (PFI(IS,N,NYEAR).GT.0.0) 
     +      PFINFI(IS,N) = PF(IS)/PFI(IS,N,NYEAR)                           [D8]
         IF (P1I(IS,N,NYEAR).GT.0.0) 
     +      PFIN1I(IS,N) = P1(IS)/P1I(IS,N,NYEAR)                           [D8]

         PMINF(IS,N)  = PMINFA(IS)                                          [D9]
         PMIN1(IS,N)  = PMIN1A(IS)                                          [D9]
         D10F(IS,N) = PF(IS)/PMATF(IS,N,0)                                  [D10]
         D101(IS,N) = P1(IS)/PSURV(IS,N,0)                                  [D10]
C        Store total catch (to calculate total need satisfaction [N1 total])
         CGTOT(IS) = CGTOT(IS) + CTOT(IS)                                   [N1tot]
         IF (PF(IS).GE.PFMSYL(IS))  FINRF(IS) = FINRF(IS) + 1               [R2]
         IF (P1(IS).GE.P1MSYL(IS))  FINR1(IS) = FINR1(IS) + 1               
         IF (PFZ(IS,NZ,NYEAR).GE.PFMSYL(IS)) FINRZF(IS) = FINRZF(IS) + 1                    .
         IF (P1Z(IS,NZ,NYEAR).GE.P1MSYL(IS)) FINRZ1(IS) = FINRZ1(IS) + 1    [R2]
1010    CONTINUE         

  200 CONTINUE
C
C     All data now read
      CLOSE (IN)
      CLOSE (IN2)


C CALCULATE SUMMARY STATISTICS AND PRINT RESULTS ------------------------------

C     Set D7: sort PMATF & PSURV to give a sorted set of population sizes
C       for every year.  Then pick the 5th and 50th percentiles
C     Set Xth percentiles: (If NTRIAL=100, IX5=5 & IX50=50)
      IX5  = NINT (NT * X5/100.)                                           [D+N7]
      IX50 = NINT (NT * X50/100.)                                          [D+N7]
      CALL SHSORT3 (PMATF,2,NTRIAL,NYEAR+1,2,100,IEND+1)                   [D7]
      CALL SHSORT3 (PSURV,2,NTRIAL,NYEAR+1,2,100,IEND+1)                   [D7]
      IF (FISHRY.EQ.2) THEN
C       Sort zero trajectory sizes in every year
        CALL SHSORT3 (PFZ,2,NTRIAL,NYEAR+1,2,100,IEND+1)                   [D7]
        CALL SHSORT3 (P1Z,2,NTRIAL,NYEAR+1,2,100,IEND+1)                   [D7]
        IXZ5 = IX5 
        IXZ50 = IX50
      ELSE
C       The zero trajectory is the same in every trial - no need to sort
        IXZ5 = 1
        IXZ50 = 1
      ENDIF
      DO 300 IS = 1,2
      DO 300 IYR = 0,NYEAR
        PFX5(IS,IYR)  = NINT(PMATF(IS,IX5,IYR))                            [D7]
        P1X5(IS,IYR)  = NINT(PSURV(IS,IX5,IYR))                              .
        PFX50(IS,IYR) = NINT(PMATF(IS,IX50,IYR))                             .
        P1X50(IS,IYR) = NINT(PSURV(IS,IX50,IYR))                             .
        PFZX5(IS,IYR)  = NINT(PFZ(IS,IXZ5,IYR))                              .
        P1ZX5(IS,IYR)  = NINT(P1Z(IS,IXZ5,IYR))                              .
        PFZX50(IS,IYR) = NINT(PFZ(IS,IXZ50,IYR))                             .
        P1ZX50(IS,IYR) = NINT(P1Z(IS,IXZ50,IYR))                           [D7]
  300 CONTINUE
C      Print [D6] (=1st 2 stochastic trials) & [D7] statistics
       WRITE (IOUT2,'(4X, 8(3X,''<'',8(''-''),A,8(''-''),''>'') /A4,
     +     32A7)') 
     +    'D6:Sim1','D6:Sim2','D6:Sim1','D6:Sim2',
     +    '--D7:5%','-D7:50%','--D7:5%','-D7:50%',
     +    'Yr',('Fmat','Fmat*','P1+','P1+*',I=1,8)
       WRITE (IOUT2,'(I4,32I7)')(IYR,((PFN(IS,N,IYR),PFZN(IS,N,IYR),
     +   P1N(IS,N,IYR),P1ZN(IS,N,IYR),N=1,2),IS=1,2),
     +   (PFX5(IS,IYR),PFZX5(IS,IYR),P1X5(IS,IYR),P1ZX5(IS,IYR),
     +   PFX50(IS,IYR),PFZX50(IS,IYR),P1X50(IS,IYR),P1ZX50(IS,IYR),
     +   IS=1,2),IYR=0,NYEAR)

      CALL SHSORT3 (V,2,NTRIAL,100,2,100,NPRD)                             [N7]
      DO 301 IS = 1,2
       DO 301 I=1,NQ(IS,2)
        VX5(IS,I)   = V(IS,IX5,I)                                          [N7]
        VX50(IS,I)  = V(IS,IX50,I)                                         [N7]
  301 CONTINUE
C      Print [N8] + [N7] statistics
       WRITE (IOUT2,'(/4X,4A8,4x,4A8 / 2(A4,4A8) / 2(I4,4F8.3) )') 
     +      'N8:Sim1','N8:Sim2','N7','N7',
     +      'N8:Sim1','N8:Sim2','N7','N7',
     +      'Yr','V(1)','V(2)','V(5%)','V(50%)',
     +      'Yr','V(1)','V(2)','V(5%)','V(50%)',
     +      (((I-1)*IQUOTA(IS),
     +       VN(IS,1,I),VN(IS,2,I),VX5(IS,I),VX50(IS,I),IS=1,2), 
     +      I=1,NQ(2,2))

C      Add extra information: initial, final & minimum depletion in each trial
C      WRITE(IOUT2,
C     +   '(//A6,2(2(A6,''Depletion''),2A8,2x)/1(4X,12A8), 
C     +        110(/I4,12F8.3))'),

       WRITE(IOUT2,
     +   '(//A6,2(2(A6,''Depletion''),2A8,2x)/1(4X,12A8), 
     +        110(/I4,12F8.3)')
     +       'Trial','Init.','Fin.','RPL','RPL',
     +               'Init.','Fin.','RPL','RPL',
     +       ('Fmat','1+',I=1,3),('Fmat','1+',I=1,3),
     +       (N,(DFINIT(IS,N),D1INIT(IS,N),DEPLF(IS,N,NTIME),
     +       DEPL1(IS,N,NTIME),
     +       DFMIN(IS,N,NTIME),D1MIN(IS,N,NTIME),IS=1,2),N=1,NTRIAL)
      CLOSE (IOUT2)

C     Sort arrays
      IF (ISRT.EQ.1) THEN
        CALL SHSORT3 (DEPLF,2,NTRIAL,NTIME,2,100,2)                        [D1]
        CALL SHSORT3 (DEPL1,2,NTRIAL,NTIME,2,100,2)                        [D1]
        CALL SHSORT3 (DFMIN,2,NTRIAL,NTIME,2,100,2)                        [D2]
        CALL SHSORT3 (D1MIN,2,NTRIAL,NTIME,2,100,2)                        [D2]
        CALL SHSORT3 (PFINF,2,NTRIAL,1,2,100,1)                            [D8]
        CALL SHSORT3 (PFINFI,2,NTRIAL,1,2,100,1)                           [D8]
        CALL SHSORT3 (PFIN1,2,NTRIAL,1,2,100,1)                            [D8]
        CALL SHSORT3 (PFIN1I,2,NTRIAL,1,2,100,1)                           [D8]
        CALL SHSORT3 (PMINF,2,NTRIAL,1,2,100,1)                            [D9]
        CALL SHSORT3 (PMIN1,2,NTRIAL,1,2,100,1)                            [D9]
        CALL SHSORT3 (D10F,2,NTRIAL,1,2,100,1)                             [D9]
        CALL SHSORT3 (D101,2,NTRIAL,1,2,100,1)                             [D9]
        CALL SHSORT3 (CTOTAL,2,NTRIAL,NTIME,2,100,2)                       [N1]
        CALL SHSORT3 (CYRS,2,NTRIAL,NTIME,2,100,2)                         [N2]
        CALL SHSORT3 (CN3,2,NTRIAL,NTIME,2,100,2)                          [N3]
        CALL SHSORT3 (CN4,2,NTRIAL,NTIME,2,100,2)                          [N4]
        CALL SHSORT3 (CN9,2,NTRIAL,NTIME,2,100,2)                          [N9]
        CALL SHSORT3 (CN10,2,NTRIAL,NTIME,2,100,2)                         [N10]
        CALL SHSORT3 (CN11,2,NTRIAL,NTIME,2,100,2)                         [N11]
        CALL SHSORT3 (CN12,2,NTRIAL,NTIME,2,100,2)                         [N12]
        CALL SHSORT3 (CN13,2,NTRIAL,NTIME,2,100,2)                         [N13]
        CALL SHSORT3 (CN14,2,NTRIAL,NTIME,2,100,2)                         [N14]
        CALL SHSORT3 (CN15,2,NTRIAL,NTIME,2,100,2)                         [N15]
        CALL SHSORT3 (RRF,2,NTRIAL,NTIME,2,100,2)                          [R1]
        CALL SHSORT3 (RR1,2,NTRIAL,NTIME,2,100,2)                          [R1]
        CALL SHSORT3 (RECOVF,2,NTRIAL,NTIME,2,100,2)                       [R3]
        CALL SHSORT3 (RECOV1,2,NTRIAL,NTIME,2,100,2)                       [R3]
        CALL SHSORT3 (RTRF,2,NTRIAL,NTIME,2,100,2)                         [R4]
        CALL SHSORT3 (RTR1,2,NTRIAL,NTIME,2,100,2)                         [R4]
        TITLE = '%'
      ELSE
        TITLE = 'N'
      ENDIF

C     Print sorted sets of performance statistics
      DO 400 IT = 1,NTIME
        OPEN (IOUT2, FILE=OUTN(IT))
        WRITE (IOUT2,'(A4,20A8)')TITLE,
     +         'N1','N2','N4','N9','N10','N11','N12','N13','N14','N15',
     +         'N1','N2','N4','N9','N10','N11','N12','N13','N14','N15',
     +          ' ',
     +         'C/Q','Sh.fall','C>=N','AvSat','AAV','ACurv','DnStep',
     +         'Landed','S&L','Catch',
     +         'C/Q','Sh.fall','C>=N','AvSat','AAV','ACurv','DnStep',
     +         'Landed','S&L','Catch'
        Q(IT,1) = REAL(ITYEAR(IT)/IQUOTA(1))
        Q(IT,2) = REAL(ITYEAR(IT)/IQUOTA(2))
        WRITE (IOUT2,'(I4,20F8.3)') (N,(CTOTAL(IS,N,IT),
     +         CYRS(IS,N,IT)/Q(IT,IS),CN4(IS,N,IT),CN9(IS,N,IT),
     +         CN10(IS,N,IT),CN11(IS,N,IT),CN12(IS,N,IT),
     +         CN13(IS,N,IT),CN14(IS,N,IT),CN15(IS,N,IT),IS=1,2),
     +         N=1,NTRIAL)
        CLOSE (IOUT2)
  400 CONTINUE
      IT = NTIME
      OPEN (IOUT2, FILE=OUTDR(IT))
      WRITE (IOUT2,'(A4,2(9A8,4X,6A8))') 
     +        TITLE,'D1:fem','D1:1+','D2:fem',
     +       'D8:fem','D8:1+','D9:fem','D9:1+','D10:fem','D10:1+',
     +       'R1:fem','R1:1+','R3:fem','R3:1+','R4:fem','R4:1+',
     +       'D1:fem','D1:1+','D2:fem',
     +       'D8:fem','D8:1+','D9:fem','D9:1+','D10:fem','D10:1+',
     +       'R1:fem','R1:1+','R3:fem','R3:1+','R4:fem','R4:1+'
      WRITE (IOUT2,'(4x,2(3A8,100X))') 
     +       ('FinDepl',I=1,2), 'MinDepl',
     +       ('FinDepl',I=1,2), 'MinDepl'
      DO 520 N = 1,NTRIAL
        WRITE (IOUT2,'(I4,2(5F8.3,2F8.1,2F8.3,4X,6F8.3))') 
     +        N,(DEPLF(IS,N,IT),
     +        DEPL1(IS,N,IT),DFMIN(IS,N,IT),PFINF(IS,N),
     +        PFIN1(IS,N),PMINF(IS,N),PMIN1(IS,N),
     +        D10F(IS,N),D101(IS,N),RRF(IS,N,IT),
     +        RR1(IS,N,IT),RECOVF(IS,N,IT),
     +        RECOV1(IS,N,IT),RTRF(IS,N,IT),RTR1(IS,N,IT),IS=1,2)
  520 CONTINUE
      CLOSE (IOUT2)

C     Print summary results 
      I5 = NTRIAL / 20
      I50 = NTRIAL / 2
      I96 = (NTRIAL - I5) + 1
      IF (ISRT.NE.1) THEN
        CALL SHSORT3 (DEPL1,2,NTRIAL,NTIME,2,100,2)                        [D1]
        CALL SHSORT3 (DEPLF,2,NTRIAL,NTIME,2,100,2)                        [D1]
        CALL SHSORT3 (DFMIN,2,NTRIAL,NTIME,2,100,2)                        [D2]
        CALL SHSORT3 (D101,2,NTRIAL,1,2,100,1)                             [D8]
        CALL SHSORT3 (PFIN1,2,NTRIAL,1,2,100,1)                            [D9]
        CALL SHSORT3 (PFINF,2,NTRIAL,1,2,100,1)                            [D9]
        CALL SHSORT3 (CN9,2,NTRIAL,NTIME,2,100,2)                          [N9]
        CALL SHSORT3 (CN12,2,NTRIAL,NTIME,2,100,2)                         [N10]
        CALL SHSORT3 (RRF,2,NTRIAL,NTIME,2,100,2)                          [R1]
        CALL SHSORT3 (RR1,2,NTRIAL,NTIME,2,100,2)                          [R1]
      ENDIF

C opt 1: summary line used for SC Hammersmith outputs
      WRITE (IOUT,'(/A,7x,7(A18,1X),10(A18,1X),A25 / 
     +              11X,2(10(3A6,1X)))') 
     +     ' Ref',
     +     'D1: Final 1+ Depln','D1: FinalFemDepln',
     +     'D8: ReSc 1+ depln',' D8: ReSc 1+ depln(I)',
     +     'D9: MinFemSizeF',
     +     'N9: Need Satfn(20)','N9: Need Satfn(100)',
     +     'D1: Final 1+ Depln','D1: FinalFemDepln',
     +     'D8: ReSc 1+ depln',' D8: ReSc 1+ depln(I)',
     +     'D9: MinFemSizeF',
     +     'N9: Need Satfn(20)','N9: Need Satfn(100)',
     +     'N13: Landed','N14: Str&lost','N15: Catch',
     +     'Reasons',
     +     ('5%','Med','96%',I=1,7),
     +     ('5%','Med','96%',I=1,10)
      WRITE (LINE,'(2(4(3F6.3,1X),1(3I6,1X),2(3F6.2,1X)
     +      ),3(3F6.2,1X),5(I6))')
     +   (DEPL1(IS,I5,2),MED3(DEPL1,IS,I50,2),DEPL1(IS,I96,2),
     +   DEPLF(IS,I5,2),MED3(DEPLF,IS,I50,2),DEPLF(IS,I96,2),
     +   PFIN1(IS,I5),MED2A(PFIN1,IS,I50),PFIN1(IS,I96),
     +   PFIN1I(IS,I5),MED2A(PFIN1I,IS,I50),PFIN1I(IS,I96),
     +   NINT(PMINF(IS,I5)),NINT(MED2A(PMINF,IS,I50)),
     +   NINT(PMINF(IS,I96)),
     +   CN9(IS,I5,1),MED3(CN9,IS,I50,1),CN9(IS,I96,1),
     +   CN9(IS,I5,2),MED3(CN9,IS,I50,2),CN9(IS,I96,2),
     +   IS=1,2),
     +   CN13(2,I5,2),MED3(CN13,2,I50,2),CN13(2,I96,2),
     +   CN14(2,I5,2),MED3(CN14,2,I50,2),CN14(2,I96,2),
     +   CN15(2,I5,2),MED3(CN15,2,I50,2),CN15(2,I96,2),
     +   (REASON(I),I=1,5)
      WRITE (IOUT,'(1X,2A)') REF,LINE
      CLOSE (IOUT)

C     Add summary stats line to AW-ALL.OUT
      OPEN (IOUT,FILE='AW-ALL.OUT',ACCESS='APPEND')
      WRITE(IOUT,'(1X,2A)') REF,LINE
      CLOSE(IOUT)

C     Add summary stats line to AW-ALL2.OUT
      OPEN (IOUT,FILE='AW-ALL2.OUT',ACCESS='APPEND')
      WRITE(IOUT,'(1X,2A)') REF,LINE
      CLOSE(IOUT)

C     Print all statistics (for 'BOOK')
      IF (ISRT.EQ.1) THEN
        OPEN (IOUT,FILE='AW-TABLE',STATUS='UNKNOWN')
        WRITE(IOUT,'(1X,13X,2(35A6))') 'D1:1+','D1:f','D2:f',
     +   'D8:1+','D8:f','D9:1+','D9:f','D10:1+','D10:f',
     +   'C1:s','C2:s','C4:s','C9:s','C10:s','C11:s','C12:s',
     +   'C1:T','C2:T','C4:T','C9:T','C10:T','C11:T','C12:T',
     +   'R1:1+','R1:f','R3:1+','R3:f','R4:1+','R4:f',
     +   'D1:1+','D1:f','D2:f',
     +   'D8:1+','D8:f','D9:1+','D9:f','D10:1+','D10:f',
     +   'C1:s','C2:s','C4:s','C9:s','C10:s','C11:s','C12:s',
     +   'C1:T','C2:T','C4:T','C9:T','C10:T','C11:T','C12:T',
     +   'R1:1+','R1:f','R3:1+','R3:f','R4:1+','R4:f'
       I=I5
        WRITE(IOUT,999) REF,I,
     +  (DEPL1(IS,I,2),DEPLF(IS,I,2),DFMIN(IS,I,2),PFIN1(IS,I),
     +   PFINF(IS,I),NINT(PMIN1(IS,I)),NINT(PMINF(IS,I)),
     +   D101(IS,I),D10F(IS,I),
     +   CTOTAL(IS,I,1),CYRS(IS,I,1)/Q(1,IS),CN4(IS,I,1),
     +   CN9(IS,I,1),CN10(IS,I,1),CN11(IS,I,1),CN12(IS,I,1),
     +   CTOTAL(IS,I,2),CYRS(IS,I,2)/Q(2,IS),CN4(IS,I,2),
     +   CN9(IS,I,2),CN10(IS,I,2),CN11(IS,I,2),CN12(IS,I,2),
     +   RR1(IS,I,2),RRF(IS,I,2),RECOV1(IS,I,2),RECOVF(IS,I,2),
     +   RTR1(IS,I,2),RTRF(IS,I,2),IS=1,2)
        I=I50
        WRITE(IOUT,999) REF,I,
     +  (MED3(DEPL1,IS,I,2),MED3(DEPLF,IS,I,2),
     +   MED3(DFMIN,IS,I,2),MED2A(PFIN1,IS,I),
     +   MED2A(PFINF,IS,I),NINT(MED2A(PMIN1,IS,I)),
     +   NINT(MED2A(PMINF,IS,I)),
     +   MED2A(D101,IS,I),MED2A(D10F,IS,I),
     +   MED3(CTOTAL,IS,I,1),MED3(CYRS,IS,I,1)/Q(1,IS),
     +   MED3(CN4,IS,I,1),MED3(CN9,IS,I,1),MED3(CN10,IS,I,1),
     +   MED3(CN11,IS,I,1),MED3(CN12,IS,I,1), 
     +   MED3(CTOTAL,IS,I,2),MED3(CYRS,IS,I,2)/Q(2,IS),
     +   MED3(CN4,IS,I,2),MED3(CN9,IS,I,2),MED3(CN10,IS,I,2),
     +   MED3(CN11,IS,I,2),MED3(CN12,IS,I,2), 
     +   MED3(RR1,IS,I,2),  MED3(RRF,IS,I,2),MED3(RECOV1,IS,I,2),
     +   MED3(RECOVF,IS,I,2),MED3(RTR1,IS,I,2),MED3(RTRF,IS,I,2),
     +   IS=1,2)
        I=I96
        WRITE(IOUT,999) REF,I,
     +  (DEPL1(IS,I,2),DEPLF(IS,I,2),DFMIN(IS,I,2),PFIN1(IS,I),
     +   PFINF(IS,I),NINT(PMIN1(IS,I)),NINT(PMINF(IS,I)),
     +   D101(IS,I),D10F(IS,I),
     +   CTOTAL(IS,I,1),CYRS(IS,I,1)/Q(1,IS),CN4(IS,I,1),
     +   CN9(IS,I,1),CN10(IS,I,1),CN11(IS,I,1),CN12(IS,I,1),
     +   CTOTAL(IS,I,2),CYRS(IS,I,2)/Q(2,IS),CN4(IS,I,2),
     +   CN9(IS,I,2),CN10(IS,I,2),CN11(IS,I,2),CN12(IS,I,2),
     +   RR1(IS,I,2),RRF(IS,I,2),RECOV1(IS,I,2),RECOVF(IS,I,2),
     +   RTR1(IS,I,2),RTRF(IS,I,2),IS=1,2)
        CLOSE(IOUT)

C       Print mandatory statistics (for 'Zeh' plots)
        OPEN (IOUT,FILE='AW-MSTAT',STATUS='UNKNOWN')
        WRITE(IOUT,'(1X,A)') Ref
        WRITE(IOUT,'(5X,(18A13))') 'D1:FinDepl1+','D1:FinDeplF',
     +       'D8:ResFDep1+','D10:RelInc1+','R1:RelRec1+','R1:RelRecF',
     +       'N9:AvSat20','N9:AvSat100','N12:MnDnStep',
     +       'D1:FinDepl1+','D1:FinDeplF',
     +       'D8:ResFDep1+','D10:RelInc1+','R1:RelRec1+','R1:RelRecF',
     +       'N9:AvSat20','N9:AvSat100','N12:MnDnStep'
        I=1
        WRITE(IOUT,998) I,(DEPL1(IS,I,2),DEPLF(IS,I,2),
     +    PFIN1(IS,I),D101(IS,I),RR1(IS,I,2),RRF(IS,I,2),
     +    CN9(IS,I,1),CN9(IS,I,2),CN12(IS,I,2),IS=1,2)
        I=5
        WRITE(IOUT,998) I,(DEPL1(IS,I,2),DEPLF(IS,I,2),
     +    PFIN1(IS,I),D101(IS,I),RR1(IS,I,2),RRF(IS,I,2),
     +    CN9(IS,I,1),CN9(IS,I,2),CN12(IS,I,2),IS=1,2)
        I=25
        WRITE(IOUT,998) I,(DEPL1(IS,I,2),DEPLF(IS,I,2),
     +    PFIN1(IS,I),D101(IS,I),RR1(IS,I,2),RRF(IS,I,2),
     +    CN9(IS,I,1),CN9(IS,I,2),CN12(IS,I,2),IS=1,2)
        I=I50
        WRITE(IOUT,998) 50,(MED3(DEPL1,IS,I,2),MED3(DEPLF,IS,I,2),
     +    MED2A(PFIN1,IS,I),MED2A(D101,IS,I), 
     +    MED3(RR1,IS,I,2),MED3(RRF,IS,I,2),
     +    MED3(CN9,IS,I,1),MED3(CN9,IS,I,2),MED3(CN12,IS,I,2),
     +    IS=1,2)
        I=76
        WRITE(IOUT,998) 75,(DEPL1(IS,I,2),DEPLF(IS,I,2),
     +    PFIN1(IS,I),D101(IS,I),RR1(IS,I,2),RRF(IS,I,2),
     +    CN9(IS,I,1),CN9(IS,I,2),CN12(IS,I,2),IS=1,2)
        I=96
        WRITE(IOUT,998) 95,(DEPL1(IS,I,2),DEPLF(IS,I,2),
     +    PFIN1(IS,I),D101(IS,I),RR1(IS,I,2),RRF(IS,I,2),
     +    CN9(IS,I,1),CN9(IS,I,2),CN12(IS,I,2),IS=1,2)
        I=100
        WRITE(IOUT,998) 99,(DEPL1(IS,I,2),DEPLF(IS,I,2),
     +    PFIN1(IS,I),D101(IS,I),RR1(IS,I,2),RRF(IS,I,2),
     +    CN9(IS,I,1),CN9(IS,I,2),CN12(IS,I,2),IS=1,2)
        CLOSE(IOUT)
      ENDIF
  998 FORMAT(1X,I4,18F13.5)
  999 FORMAT(1X,A,I2,'%',2(5F6.3,2I6,3F6.3,F6.2,6F6.3,F6.2,11F6.3))

C ?? not sure what you are doing with the psurv2 array - my compiler falls
C    over at the next line with ARR(51,1) undefined
C      go TO 97977

      DO 97000 IS = 1,2
       DO 97000 IYR = ISTART,-1
        DO 97000 N = 1,NTRIAL
         P1Z2(IS,N,IYR) = PSURV2(IS,N,IYR)
         P1I2(IS,N,IYR) = PSURV2(IS,N,IYR)
         PFZ2(IS,N,IYR) = PMATF2(IS,N,IYR)
         PFI2(IS,N,IYR) = PMATF2(IS,N,IYR)
97000 CONTINUE

      OPEN(IOUT,FILE='AW-TRAJ.OUT')
      OPEN(IOUT2,FILE='AW-CONF.OUT')
      WRITE(IOUT,*) ISTART,NYEAR,2011
      WRITE(IOUT2,*) ISTART,NYEAR,2011

      DO 97100 IS = 1,2
       DO 97110 IYR = ISTART,NYEAR
        WRITE(IOUT,'(102I6)') 1,IYR,(NINT(PSURV2(IS,N,IYR)),N=1,NTRIAL)
        DO 97111 N = 1,NTRIAL
         ERR(N) = PSURV2(IS,N,IYR)
97111   CONTINUE                      
        CALL SHSORT(ERR,NTRIAL,1,100,1)
        WRITE(IOUT2,'(102I6)') 1,IYR,NINT(ERR(I5)),
     +     NINT((ERR(I50)+ERR(I50+1))/2.0),NINT(ERR(I96))
97110  CONTINUE
97100 CONTINUE
      DO 97200 IS = 1,2
       DO 97210 IYR = ISTART,NYEAR
        WRITE(IOUT,'(102I6)') 1,IYR,(NINT(PMATF2(IS,N,IYR)),N=1,NTRIAL)
        DO 97211 N = 1,NTRIAL
         ERR(N) = PMATF2(IS,N,IYR)
97211   CONTINUE                      
        CALL SHSORT(ERR,NTRIAL,1,100,1)
        WRITE(IOUT2,'(102I6)') 1,IYR,NINT(ERR(I5)),
     +     NINT((ERR(I50)+ERR(I50+1))/2.0),NINT(ERR(I96))
97210  CONTINUE
97200 CONTINUE
      DO 97300 IS = 1,2
       DO 97310 IYR = ISTART,NYEAR
        WRITE(IOUT,'(102I6)') 2,IYR,(NINT(P1Z2(IS,N,IYR)),N=1,NTRIAL)
        DO 97311 N = 1,NTRIAL
         ERR(N) = P1Z2(IS,N,IYR)
97311   CONTINUE                      
        CALL SHSORT(ERR,NTRIAL,1,100,1)
        WRITE(IOUT2,'(102I6)') 2,IYR,NINT(ERR(I5)),
     +     NINT((ERR(I50)+ERR(I50+1))/2.0),NINT(ERR(I96))
97310  CONTINUE
97300 CONTINUE
      DO 97400 IS = 1,2
       DO 97410 IYR = ISTART,NYEAR
        WRITE(IOUT,'(102I6)') 2,IYR,(NINT(PFZ2(IS,N,IYR)),N=1,NTRIAL)
        DO 97411 N = 1,NTRIAL
         ERR(N) = PFZ2(IS,N,IYR)
97411   CONTINUE                      
        CALL SHSORT(ERR,NTRIAL,1,100,1)
        WRITE(IOUT2,'(102I6)') 2,IYR,NINT(ERR(I5)),
     +     NINT((ERR(I50)+ERR(I50+1))/2.0),NINT(ERR(I96))
97410  CONTINUE
97400 CONTINUE
      DO 97500 IS = 1,2
       DO 97510 IYR = ISTART,NYEAR
        WRITE(IOUT,'(102I6)') 3,IYR,(NINT(P1I2(IS,N,IYR)),N=1,NTRIAL)
        DO 97511 N = 1,NTRIAL
         ERR(N) = P1I2(IS,N,IYR)
97511   CONTINUE                      
        CALL SHSORT(ERR,NTRIAL,1,100,1)
        WRITE(IOUT2,'(102I6)') 3,IYR,NINT(ERR(I5)),
     +     NINT((ERR(I50)+ERR(I50+1))/2.0),NINT(ERR(I96))
97510  CONTINUE
97500 CONTINUE
      DO 97600 IS = 1,2
       DO 97610 IYR = ISTART,NYEAR
        WRITE(IOUT,'(102I6)') 3,IYR,(NINT(PFI2(IS,N,IYR)),N=1,NTRIAL)
        DO 97611 N = 1,NTRIAL
         ERR(N) = PFI2(IS,N,IYR)
97611   CONTINUE                      
        CALL SHSORT(ERR,NTRIAL,1,100,1)
        WRITE(IOUT2,'(102I6)') 3,IYR,NINT(ERR(I5)),
     +     NINT((ERR(I50)+ERR(I50+1))/2.0),NINT(ERR(I96))
97610  CONTINUE
97600 CONTINUE
      DO 98100 IS = 1,2
       DO 98110 IYR = 0,NYEAR-1
        WRITE(IOUT,'(I6,102F8.1)') IYR,(CSTOR(IS,N,IYR),N=1,NTRIAL)
        DO 98111 N = 1,NTRIAL
         ERR(N) = CSTOR(IS,N,IYR)
98111   CONTINUE                      
        CALL SHSORT(ERR,NTRIAL,1,100,1)
        WRITE(IOUT2,'(I6,102F8.1)') IYR,ERR(I5),
     +     (ERR(I50)+ERR(I50+1)/2.0),ERR(I96)
98110  CONTINUE
98100 CONTINUE
      DO 98200 IYR = 0,19
       WRITE(IOUT,'(I6,102I8)') IYR,(LANDS(N,IYR),N=1,NTRIAL)
98200 CONTINUE
C
      CLOSE(IOUT)
      CLOSE(IOUT2)   
      STOP
      END

C -----------------------------------------------------------------------------

      FUNCTION MED1(ARR,I1)
C     Returns median of integer array
      REAL ARR(100),MED1
      INTEGER I1
      MED1 = (ARR(I1)+ARR(I1+1))*0.5
      END

      FUNCTION MED2(ARR,I1,I2)
      REAL ARR(100,2),MED2
      INTEGER I1,I2
      MED2 = (ARR(I1,I2)+ARR(I1+1,I2))*0.5
      END
      
      FUNCTION MED2A(ARR,I1,I2)
      REAL ARR(2,100),MED2A
      INTEGER I1,I2
      MED2A = (ARR(I1,I2)+ARR(I1,I2+1))*0.5
      END

      FUNCTION MED3(ARR,I1,I2,I3)
      REAL ARR(2,100,2),MED3
      INTEGER I1,I2
      MED3 = (ARR(I1,I2,I3)+ARR(I1,I2+1,I3))*0.5
      END

C -----------------------------------------------------------------------------
C -----------------------------------------------------------------------------

      SUBROUTINE SHSORT(ARR,NTRIAL,NSORT,IDIM1,IDIM2)

C     Subroutine sorts columns of array ARR into ascending order, by the
C     Shell-Mezgar algorithm (diminishing increment sort).
C     (Reference: Press,Flannery,Teukolsky & Vetterling: Numerical Recipes,
C     CUP, Cambridge 1986 (Page 229)
C     ARR is replaced on output by its sorted rearrangement.
C     ARR has IDIM1 rows X IDIM2 columns.
C     NTRIAL is the number of rows and NSORT the number of columns to be sorted
C
      INTEGER NTRIAL,NSORT,JE,M,K,NN,LOGNB2,I,J,L,IDIM1,IDIM2
      REAL ARR(IDIM1,IDIM2),ALN2I,TINY,T
      PARAMETER (ALN2I=1./0.69314718, TINY=1.E-5)

      LOGNB2 = INT(LOG(REAL(NTRIAL))*ALN2I+TINY)
      DO 20 JE=1,NSORT
        M = NTRIAL
        DO 12 NN = 1,LOGNB2
          M = M/2
          K = NTRIAL-M
          DO 11 J=1,K
            I = J
    3       CONTINUE
            L = I+M
            IF (ARR(L,JE).LT.ARR(I,JE)) THEN
              T = ARR(I,JE)
              ARR(I,JE) = ARR(L,JE)
              ARR(L,JE) = T
              I = I-M
              IF (I.GE.1) GO TO 3
            ENDIF
   11     CONTINUE
   12   CONTINUE
   20 CONTINUE
      RETURN
      END                                                                                     

C -----------------------------------------------------------------------------
C -----------------------------------------------------------------------------

      SUBROUTINE SHSORT3(ARR,NOPT,NTRIAL,NSORT,IDIM1,IDIM2,IDIM3)

C     Subroutine sorts columns of array ARR into ascending order, by the
C     Shell-Mezgar algorithm (diminishing increment sort).
C     (Reference: Press,Flannery,Teukolsky & Vetterling: Numerical Recipes,
C     CUP, Cambridge 1986 (Page 229)
C     ARR is replaced on output by its sorted rearrangement.
C     ARR has IDIM1 rows X IDIM2 columns.
C     NTRIAL is the number of rows and NSORT the number of columns to be sorted
C
      INTEGER NTRIAL,NSORT,JE,M,K,NN,LOGNB2,I,J,L,IDIM1,IDIM2
      REAL ARR(IDIM1,IDIM2,IDIM3),ALN2I,TINY,T
      PARAMETER (ALN2I=1./0.69314718, TINY=1.E-5)

      LOGNB2 = INT(LOG(REAL(NTRIAL))*ALN2I+TINY)
      DO 10 JF=1,NOPT
       DO 20 JE=1,NSORT
        M = NTRIAL
        DO 12 NN = 1,LOGNB2
          M = M/2
          K = NTRIAL-M
          DO 11 J=1,K
            I = J
    3       CONTINUE
            L = I+M
            IF (ARR(JF,L,JE).LT.ARR(JF,I,JE)) THEN
              T = ARR(JF,I,JE)
              ARR(JF,I,JE) = ARR(JF,L,JE)
              ARR(JF,L,JE) = T
              I = I-M
              IF (I.GE.1) GO TO 3
            ENDIF
   11     CONTINUE
   12   CONTINUE
   20  CONTINUE
   10 CONTINUE    
      RETURN
      END                                                                                     

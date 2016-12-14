      CHARACTER*1 Types(4)
      INTEGER II
      

      Types(1) = "P"
      Types(2) = "B"
      Types(3) = "I"

      DO 1000 II = 1,3
       CALL CREATE(Types(II),"01","A",1)
       CALL CREATE(Types(II),"01","B",1)
       CALL CREATE(Types(II),"01","C",1)
       CALL CREATE(Types(II),"01","D",1)
       CALL CREATE(Types(II),"02","A",2)
       CALL CREATE(Types(II),"02","B",2)
       CALL CREATE(Types(II),"02","C",2)
       CALL CREATE(Types(II),"02","D",2)
       CALL CREATE(Types(II),"03","A",3)
       CALL CREATE(Types(II),"03","B",3)
       CALL CREATE(Types(II),"04","A",4)
       CALL CREATE(Types(II),"04","B",4)
       CALL CREATE(Types(II),"05","A",5)
       CALL CREATE(Types(II),"05","B",5)
1000  CONTINUE
      DO 2000 II = 1,2
       CALL CREATE(Types(II),"06","A",1)
       CALL CREATE(Types(II),"06","B",1)
       CALL CREATE(Types(II),"07","A",1)
       CALL CREATE(Types(II),"07","B",1)
       CALL CREATE(Types(II),"08","A",1)
       CALL CREATE(Types(II),"08","B",1)
       CALL CREATE(Types(II),"09","A",1)
       CALL CREATE(Types(II),"09","B",1)
       CALL CREATE(Types(II),"10","A",1)
       CALL CREATE(Types(II),"10","B",1)
       CALL CREATE(Types(II),"11","A",1)
       CALL CREATE(Types(II),"11","B",1)
       CALL CREATE(Types(II),"12","A",1)
       CALL CREATE(Types(II),"12","B",1)
       CALL CREATE(Types(II),"13","A",13)
       CALL CREATE(Types(II),"13","B",13)
       CALL CREATE(Types(II),"13","C",13)
       CALL CREATE(Types(II),"14","A",14)
       CALL CREATE(Types(II),"14","B",14)
       CALL CREATE(Types(II),"51","A",1)
       CALL CREATE(Types(II),"51","B",1)
       CALL CREATE(Types(II),"52","A",1)
       CALL CREATE(Types(II),"52","B",1)
       CALL CREATE(Types(II),"53","A",1)
       CALL CREATE(Types(II),"53","B",1)
       CALL CREATE(Types(II),"54","A",1)
       CALL CREATE(Types(II),"54","B",1)
       CALL CREATE(Types(II),"55","A",1)
       CALL CREATE(Types(II),"55","B",1)
       CALL CREATE(Types(II),"56","A",1)
       CALL CREATE(Types(II),"56","B",1)
       CALL CREATE(Types(II),"57","A",1)
       CALL CREATE(Types(II),"57","B",1)
       CALL CREATE(Types(II),"62","A",1)
       CALL CREATE(Types(II),"62","B",1)
       CALL CREATE(Types(II),"63","A",1)
       CALL CREATE(Types(II),"63","B",1)

       CALL CREATE(Types(II),"58","B",58)
       CALL CREATE(Types(II),"59","B",59)
       CALL CREATE(Types(II),"60","B",60)
       CALL CREATE(Types(II),"61","B",61)

2000  CONTINUE       



C       
      STOP
      END
C
C =================================================================================
C
      SUBROUTINE CREATE(Model,Trial,MSYR,Base)
C
C  This creates the input files
C
      IMPLICIT NONE
C
C     Global variables
      CHARACTER Model*1,Trial*2,MSYR*1
      INTEGER BASE
C
C     Local variables
      CHARACTER FILEN*9
      CHARACTER CONDF*2
      CHARACTER PartA*10,PartB*11,PartC*10,PartD*32
      INTEGER IQUOTA1,IQUOTA2,OPTIMMI
      INTEGER IOPTK1,IOPTK2,IOPTM1,IOPTM2,NEVENT,EVENTS
      INTEGER MonthHarvest,IFREQ2,CORRN
      REAL*8 IMMIRRF,CRATIO2,NEEDF,PHIPCFG,SURVB1,IMMIPOW
      REAL*8 MSYR1L,MSYR1H,MSYR2L,MSYR2H
      REAL*8 ProbInitial,ProbFinal,ProbStruck,CVEST
      REAL*8 ProbFalseNeg,PCFGBias
      REAL*8 INCC1,INCC2,INCC3,INCC4
      
      PartA = ""
      PartB = "FNeed=340/7"                            
      PartD = "; Immigration = 2"
      PartC = ""
   
      IF (Model.EQ."P") THEN
       PartC = "; Pulse Immigration"
       OPTIMMI = 1
      ENDIF 
      IF (Model.EQ."B") THEN
       PartC = "; Survey Bias"
       OPTIMMI = 0
      ENDIF 
      IF (Model.EQ."I") THEN
       PartC = "; Intermediate"
       OPTIMMI = 3
      ENDIF 
       
      IF (MSYR.EQ."A") THEN
       PartA = "4.5%/4.5%;"
       MSYR1L = 0.045
       MSYR1H = 0.045
       MSYR2L = 0.045
       MSYR2H = 0.045
       SurvB1 = 1
      ENDIF 
      IF (MSYR.EQ."B") THEN
       PartA = "4.5%/2.0%;"
       MSYR1L = 0.045
       MSYR1H = 0.045
       MSYR2L = 0.020
       MSYR2H = 0.020
       SurvB1 = 1
      ENDIF 
      IF (MSYR.EQ."C") THEN
       PartA = "4.5%/1.0%;"
       MSYR1L = 0.0450
       MSYR1H = 0.0450
       MSYR2L = 0.010
       MSYR2H = 0.010
       SurvB1 = 1
      ENDIF 
      IF (MSYR.EQ."D") THEN
       PartA = "2.0%/2.0%;"
       MSYR1L = 0.020
       MSYR1H = 0.020
       MSYR2L = 0.020
       MSYR2H = 0.020
       SurvB1 = 0.5
      ENDIF 
C
C     Default para,eyers
      IMMIRRF = 2
       
       
      
C
C     Base linkages
      IF (Base.EQ.1) THEN
       CONDF = "01"
      ENDIF 
      IF (Base.EQ.2) THEN
       CONDF = "02"
       IMMIRRF = 0
       PartD = "; Immigration = 0"
      ENDIF 
      IF (Base.EQ.3) THEN
       CONDF = "03"
       IMMIRRF = 1
       PartD = "; Immigration = 1"
      ENDIF 
      IF (Base.EQ.4) THEN
       CONDF = "04"
       IMMIRRF = 4
       PartD = "; Immigration = 4"
      ENDIF 
      IF (Base.EQ.5) THEN
       CONDF = "05"
       IMMIRRF = 6
       PartD = "; Immigration = 6"
      ENDIF 
      IF (Base.EQ.13) THEN
       CONDF = "13"
       PartD = "; High 1999-2000 pulse"
      ENDIF 
      IF (Base.EQ.14) THEN
       CONDF = "14"
       PartD = "; Low 1999-2000 pulse"
      ENDIF 
      IF (Base.EQ.58) CONDF = "58"
      IF (Base.EQ.59) CONDF = "59"
      IF (Base.EQ.60) CONDF = "60"
      IF (Base.EQ.61) CONDF = "61"
           
      
      IQUOTA1 = 5
      IQUOTA2 = 1
      IOPTK1 = 0
      IOPTK2 = 0
      IOPTM1 = 0
      IOPTM2 = 0
      NEVENT = 0
      EVENTS = 0
      CRATIO2 = 0.5
      NEEDF = 340
      PHIPCFG =  0.3
      ProbFalseNeg = 0
      ProbInitial = 0.01
      ProbFinal = 0.01
      ProbStruck = 0.5
      MonthHarvest = 4
      CVEST = 0.075
      IFREQ2 = 1
      PCFGBias = 1
      CORRN = 0
      IMMIPOW = 1

      INCC1 = 0
      INCC2 = 2
      INCC3 = 1.4
      INCC4 = 3.40
      
      IF (Trial.EQ."06") THEN
       NEEDF = 530
       PartB = "FNeed=530/7"
      ENDIF 
      IF (Trial.EQ."07") THEN
       NEVENT = 3
       EVENTS = 20
       PartD = "; 3 Episodic events"
      ENDIF
      IF (Trial.EQ."08") THEN
       NEVENT = 5
       EVENTS = 10
       PartD = "; 10% Episodic event every 5 yrs"
      ENDIF
      IF (Trial.EQ."09") THEN
       NEVENT = -3
       EVENTS = -20
       PartD = "; 3 Unepisodic events"
      ENDIF
      IF (Trial.EQ."10") THEN
       PhiPCFG = 0.6
       PartD = "; Phi(PCFG) = 0.600"
      ENDIF 
      IF (Trial.EQ."11") THEN
       ProbStruck = 0.25
       PartD = "; Stuck & lost = 0.25"
      ENDIF 
      IF (Trial.EQ."12") THEN
       ProbStruck = 0.75
       PartD = "; Stuck & lost = 0.75"
      ENDIF 
      IF (Trial.EQ."13") THEN
       OPTIMMI = 2
      ENDIF 
      IF (Trial.EQ."14") THEN
       OPTIMMI = 3
      ENDIF 
      IF (Trial.EQ."51") THEN
       IFREQ2 = 6
       PartD = "6-year surveys"
      ENDIF
      IF (Trial.EQ."52") THEN
       IOPTK1 = 2
       IOPTK2 = 2
       PartD = "; K declines (all)"
      ENDIF 
      IF (Trial.EQ."53") THEN
       IOPTK2 = 2
       PartD = "; K declines (PCFG)"
      ENDIF 
      IF (Trial.EQ."54") THEN
       IOPTM1 = 1
       IOPTM2 = 1
       PartD = "; M doubles (all)"
      ENDIF 
      IF (Trial.EQ."55") THEN
       IOPTM2 = 1
       PartD = "; M doubles (PCFG)"
      ENDIF 
      IF (Trial.EQ."56") THEN
       ProbFinal = 0.05
       PartD = "; Prob (false pos) -> 0.05"
      ENDIF
      IF (Trial.EQ."57") THEN
       ProbFalseNeg = 0.5
       PartD = "; Prob (false neg) = 0.5"
      ENDIF 
      IF (Trial.EQ."58") THEN
       ProbFalseNeg = 0.5
       PCFGBias = 0.5
       PartD = "; PCFG(Bias) = 0.5"
      ENDIF
      IF (Trial.EQ."59") THEN
       CORRN = 1
       PartD = "; Correlation in priors"
      ENDIF
      IF (Trial.EQ."60") THEN
       INCC1 = INCC1 * 2
       INCC2 = INCC2 * 2
       INCC3 = INCC3 * 2
       INCC4 = INCC4 * 2
       PartD = "; Double incidental catches"
      ENDIF  
      IF (Trial.EQ."61") THEN
       INCC1 = INCC1 / 2
       INCC2 = INCC2 / 2
       INCC3 = INCC3 / 2
       INCC4 = INCC4 / 2
       PartD = "; Halve incidental catches"
      ENDIF  
      IF (Trial.EQ."62") THEN
       CRATIO2 = 0.20
       PartD = "; 80:20 F:M Sex ratio"
      ENDIF
      IF (Trial.EQ."63") THEN
       PhiPCFG = 1
       PartD = "; Phi(PCFG) = 1.000"
      ENDIF  

      WRITE(FILEN,601) Model,Trial,MSYR 
      WRITE(*,*) FILEN
      OPEN(UNIT=14,FILE=FILEN)
      WRITE(14,701) Model,Trial,MSYR
      WRITE(14,702) PartA,PartB,PartC,PartD
      WRITE(14,*)
      WRITE(14,703) Model,CONDF,MSYR
      WRITE(14,704) Model
      WRITE(14,705)
      WRITE(14,706)
      WRITE(14,707)
      WRITE(14,708)
      WRITE(14,709)
      WRITE(14,711) IQUOTA1
      WRITE(14,712) IQUOTA2
      WRITE(14,713)
      WRITE(14,714)
      WRITE(14,715)
      WRITE(14,716)
      
      WRITE(14,721) IOPTK1
      WRITE(14,722) IOPTK2
      WRITE(14,723) 
      WRITE(14,724) IOPTM1
      WRITE(14,725) IOPTM2
      
      WRITE(14,731) OPTIMMI
      WRITE(14,732) 
      WRITE(14,733) NEVENT
      WRITE(14,734) EVENTS
      WRITE(14,735) 
      WRITE(14,736) 
      WRITE(14,737) 
      WRITE(14,738)
      
      WRITE(14,741)
      WRITE(14,742) CRATIO2
      WRITE(14,743)
      WRITE(14,744) IMMIRRF
      WRITE(14,7445) IMMIPOW
      WRITE(14,745)
      WRITE(14,746)
      WRITE(14,747)
      WRITE(14,748)


      WRITE(14,751)
      WRITE(14,752) NEEDF
      WRITE(14,753)
      WRITE(14,754)
      WRITE(14,755) 
      WRITE(14,756)
      WRITE(14,757) PHIPCFG
      WRITE(14,758) ProbFalseNeg
      WRITE(14,759) ProbInitial
      WRITE(14,760) ProbFinal
      WRITE(14,761)
      WRITE(14,762) ProbStruck
      WRITE(14,763) MonthHarvest
      WRITE(14,764) 

      WRITE(14,771) INCC1
      WRITE(14,772) INCC2
      WRITE(14,773) INCC3
      WRITE(14,774) INCC4

      WRITE(14,781)
      WRITE(14,782)
      WRITE(14,783)
      WRITE(14,784)
      WRITE(14,785)
      WRITE(14,786) IFREQ2
      WRITE(14,787)
      WRITE(14,788) SurvB1
      WRITE(14,789)
      WRITE(14,790)
      WRITE(14,791)
      WRITE(14,792) PCFGBias
      WRITE(14,793) PCFGBias
      WRITE(14,794) PCFGBias
      WRITE(14,795) PCFGBias
      WRITE(14,796)
      WRITE(14,797)
      WRITE(14,798)
      WRITE(14,799)
      WRITE(14,800)
      WRITE(14,801) CVEST
      WRITE(14,802)

      WRITE(14,811)
      WRITE(14,812) CORRN

      WRITE(14,911)
      WRITE(14,912)
      WRITE(14,913)
      WRITE(14,914)
      WRITE(14,915)
      WRITE(14,916)
      WRITE(14,917)
      WRITE(14,918) MSYR1L
      WRITE(14,919) MSYR1H
      WRITE(14,1001)
      WRITE(14,1002) MSYR2L
      WRITE(14,1003) MSYR2H
      WRITE(14,1004)
      WRITE(14,1005)
      WRITE(14,1006)
      WRITE(14,1007)
      WRITE(14,1008) 
      WRITE(14,1009) 
      WRITE(14,1010) 
      WRITE(14,1011)
      WRITE(14,1012) 
      WRITE(14,1013)
      WRITE(14,1014)
      WRITE(14,1015)
      WRITE(14,1016)
      WRITE(14,1017)
      WRITE(14,1018) 
      WRITE(14,1019) 
      WRITE(14,1020) 
      WRITE(14,1021)
      WRITE(14,1022) 
      WRITE(14,1023)
      WRITE(14,1024)
      WRITE(14,1025)
      WRITE(14,1026)
      WRITE(14,1027)
      WRITE(14,1028) 
      WRITE(14,1029) 
      WRITE(14,1030) 
      WRITE(14,1031)
      WRITE(14,1032) 
      WRITE(14,1033)
      WRITE(14,1034)
      WRITE(14,1035)
      WRITE(14,1036)
      WRITE(14,1037)
      WRITE(14,1038) 
      WRITE(14,1039) 
      WRITE(14,1040) 
      WRITE(14,1041) 
      WRITE(14,1042) 



      CLOSE(14)
      
      
      RETURN
601   FORMAT("G",A1,A2,A1,".DDD")      
701   FORMAT("MANAGEMENT PARAMETERS         CASE  G",A1,A2,A1)      
702   FORMAT("BASE CASE: ",A10," ",A11,A10,A32)
703   FORMAT("NAME OF PARAMETER FILE               FILNAM  G",A1,A2,A1,
     +       ".PAR")
704   FORMAT("NAME OF CONDITIONING DATA FILE       MATNAM  MATRIX",
     +       A1,".DAT")     
705   FORMAT("No. OF TRIALS                        NTRIAL  100")
706   FORMAT("No. OF YEARS IN SIMULATION           NYEAR   100")
707   FORMAT("YEAR MODEL BEGINS                    ISTART 1600")
708   FORMAT("YEAR TO BEGIN POPULATION PROJECTION  IYRPRJ 1930")
709   FORMAT("YEAR MANAGEMENT BEGINS               IYRMAN 2011")
711   FORMAT("NUMBER OF YEARS OF QUOTA (1)         IQUOTA ",I4)
712   FORMAT("NUMBER OF YEARS OF QUOTA (2)         IQUOTA ",I4)
713   FORMAT("MAXIMUM AGE                          MAXAGE   15")
714   FORMAT("MSYR COMPONENT      0=1+, 1=Mat      OPTF      0")
715   FORMAT("MSYL COMPONENT      0=1+, 1=Mat      OPMSYL    0")
716   FORMAT("DENSITY DEPENDENCE: 0=1+, 1=Mat      OPTDD     0")
721   FORMAT("TIME DEPENDENCE IN K(1) 0:Const      OPTK   ",I4)
722   FORMAT("TIME DEPENDENCE IN K(2) 0:Const      OPTK   ",I4)
723   FORMAT("TIME DEPENDENCE IN A 0:Const         OPTA      0")
724   FORMAT("TIME DEPENDENCE IN M(1) 0:Const      OPTM   ",I4)
725   FORMAT("TIME DEPENDENCE IN M(2) 0:Const      OPTM   ",I4)
731   FORMAT("IMMIGRATION 0: Default; 1:Pulse      OPTIMMI",I4)
732   FORMAT("LAG IN DENSITY DEPENDENCE            ILAG      2")
733   FORMAT("NUMBER OF EPISODIC EVENTS            NEVENT ",I4)
734   FORMAT("EVENT POPULATION % REDUCTION         EVENTS ",I4)
735   FORMAT("EVENT TYPE 0:Uniform 1: Juvs 1st     OPTEE     0")
736   FORMAT("HISTORIC CATCH OPTION 1=50:50 catch  OPTHC     0")
737   FORMAT("MINIMUM CALVING INTERVAL             NY        0")
738   FORMAT("TARGET YEAR                          IYRTAR 1968")
741   FORMAT("FUTURE CATCH SEX RATIO (NORTH)       CRATIO  0.5")
742   FORMAT("FUTURE CATCH SEX RATIO (PCFG)        CRATIO  ",F4.2)
743   FORMAT("HISTORIC CATCH BIAS                  BIASC   1.0")
744   FORMAT("IMMIGRATION SCALAR                   IMMIRRF ",F4.2)
7445  FORMAT("IMMIGRATION POWER                    IMMIPOW ",F4.2)
745   FORMAT("YEAR OF FIRST CATCH                  ICAT1  1600")
746   FORMAT("CATCH BIAS APPLIES UP TO             ICATB  1914")
747   FORMAT("MULTIPLIER OF ABORIGINAL CATCH       ABOMUL  1.0")
748   FORMAT("AGE OF RECRUITMENT                   AREC    1.0")
751   FORMAT("INITIAL NEED LEVEL (Area 1           NEED0   150.0")
752   FORMAT("FINAL NEED LEVEL (Area 1)            NEEDF   ",F5.1)
753   FORMAT("INITIAL NEED LEVEL (Area 2)          NEED0     7.0")
754   FORMAT("FINAL NEED LEVEL (Area 2)            NEEDF     7.0")
755   FORMAT("HISTPROP (PCFG; JUN-NOV)             HPCFG1  0.300")
756   FORMAT("HISTPROP (SOUTH)                     HPCFG2  0.010")
757   FORMAT("FUT PROP (DELTA)                     FNORTH  ",F5.3)
758   FORMAT("PROB PCFG NOT DETECTED AS PCFG       PFNeg   ",F4.2)
759   FORMAT("PROB NORTH DETECTED AS PCFG (Yr1)    PFPos   ",F4.2)
760   FORMAT("PROB NORTH DETECTED AS PCFG (Yr99)   PFPos   ",F4.2)
761   FORMAT("FRACTION OF NORTH IN PCFG SURVEY     FracObs 0.00")
762   FORMAT("PROBABILITY STRUCK                   PrStrk  ",F4.2)
763   FORMAT("Month of harvest                     MonHa   ",I3)
764   FORMAT("    Incidental catches")
771   FORMAT("NORTH AREA                           INCC(1) ",F4.2)
772   FORMAT("PCFG [DEC - MAY]                     INCC(2) ",F4.2)
773   FORMAT("PCFG [JUN - NOV]                     INCC(3) ",F4.2)
774   FORMAT("SOUTH                                INCC(4) ",F4.2)
781   FORMAT("    Abundance data:")
782   FORMAT("YEAR OF 1ST HISTORIC SURVEY          ISUR1  1968")
783   FORMAT("YEAR OF 1ST GENERATED SURVEY         IGSURV 2012")
784   FORMAT("DETERMINISTIC ABUNDANCE  0=No 1=Yes  OPTDET    0")
785   FORMAT("FREQUENCY OF ABUNDANCE ESTIMATES (1) IFREQ    10")
786   FORMAT("FREQUENCY OF ABUNDANCE ESTIMATES (2) IFREQ   ",I3)
787   FORMAT("FUTURE BIAS FORM 0=linear, 1=cyclic  OPTB      0")
788   FORMAT("FIRST HISTORIC SURVEY BIAS (1)       BIASH1  ",F4.2)
789   FORMAT("FINAL HISTORIC SURVEY BIAS (1)       BIASH2  1.00")
790   FORMAT("BIAS IN YEAR 25 (1)                  BIAS    1.00")
791   FORMAT("BIAS IN FINAL YEAR (1)               BIASF   1.00")
792   FORMAT("FIRST HISTORIC SURVEY BIAS (2)       BIASH1  ",F4.2)
793   FORMAT("FINAL HISTORIC SURVEY BIAS (2)       BIASH2  ",F4.2)
794   FORMAT("BIAS IN YEAR 25 (2)                  BIAS    ",F4.2)
795   FORMAT("BIAS IN FINAL YEAR (2)               BIASF   ",F4.2)
796   FORMAT("MEAN BIAS (NORTH)                    BIASB1  1.000")
797   FORMAT("SD BIAS (NORTH)                      SDB1    0.000")
798   FORMAT("MEAN BIAS (PCFG)                     BIASB2  0.744")
799   FORMAT("SD BIAS (PCFG)                       SDB2    0.110")
800   FORMAT("NO. OF DEGREES OF FREEDOM            DOF     19.00")
801   FORMAT("CV ESTIMATE: EXPECTATION VALUE (1st) CV1EST  ",F5.3)
802   FORMAT("CV ESTIMATE: EXPECTATION VALUE (1st) CV1EST  0.097")
811   FORMAT("    Priors:")
812   FORMAT("CORRELATED P68 (1=Yes; 2=No)                 ",I3)

911   FORMAT("PRIOR FOR MSYL1                              UNIFORM")
912   FORMAT("MINIMUM VALUE                                0.6000")
913   FORMAT("MAXIMUM VALUE                                0.6000")
914   FORMAT("PRIOR FOR MSYL2                              UNIFORM")
915   FORMAT("MINIMUM VALUE                                0.6000")
916   FORMAT("MAXIMUM VALUE                                0.6000")
917   FORMAT("PRIOR FOR MSYR1                              UNIFORM")
918   FORMAT("MINIMUM VALUE                                ",F6.4)
919   FORMAT("MAXIMUM VALUE                                ",F6.4)
1001   FORMAT("PRIOR FOR MSYR2                              UNIFORM")
1002   FORMAT("MINIMUM VALUE                                ",F6.4)
1003   FORMAT("MAXIMUM VALUE                                ",F6.4)
1004   FORMAT("PRIOR FOR TRANSITION AGE (TRANSA)            DUNIFORM")
1005   FORMAT("MINIMUM VALUE                                0.0000")
1006   FORMAT("MAXIMUM VALUE                                0.0000")
1007   FORMAT("PRIOR FOR JUVENILE SURVIVAL (NOT USED)       UNIFORM")
1008   FORMAT("MINIMUM VALUE                                0.9000")
1009   FORMAT("MAXIMUM VALUE                                0.9000")
1010   FORMAT("PRIOR FOR ADULT SURVIVAL (ADULTS)            UNIFORM")
1011   FORMAT("MINIMUM VALUE                                0.9500")
1012   FORMAT("MAXIMUM VALUE                                0.9900")
1013   FORMAT("PRIOR FOR 1/MAX PREGNANCY RATE (1/PREGTG)    UNIFORM")
1014   FORMAT("MINIMUM VALUE                                1.6667")
1015   FORMAT("MAXIMUM VALUE                                3.3333")
1016   FORMAT("PRIOR FOR AGE AT MATURITY (+1 = AGEM)        DUNIFORM")
1017   FORMAT("MINIMUM VALUE                                 5.000")
1018   FORMAT("MAXIMUM VALUE                                11.000")
1019   FORMAT("PRIOR FOR VALUE TO BE HIT1 ('000s)           UNIFORM")
1020   FORMAT("MINIMUM VALUE                                 8.000")
1021   FORMAT("MAXIMUM VALUE                                16.000")
1022   FORMAT("PRIOR FOR VALUE TO BE HIT2 ('000s)           UNIFORM")
1023   FORMAT("MINIMUM VALUE                                 0.020")
1024   FORMAT("MAXIMUM VALUE                                 0.150")
1025   FORMAT("PRIOR FOR ADDITIONAL VARIANCE (CVadd1)       UNIFORM")
1026   FORMAT("MINIMUM VALUE                                 0.100")
1027   FORMAT("MAXIMUM VALUE                                 0.300")
1028   FORMAT("PRIOR FOR ADDITIONAL VARIANCE (CVadd2)       UNIFORM")
1029   FORMAT("MINIMUM VALUE                                 0.050")
1030   FORMAT("MAXIMUM VALUE                                 0.300")
1031   FORMAT("PRIOR FOR CARRYING CAPACITY1 (in 1000s)      UNIFORM")
1032   FORMAT("MINIMUM VALUE                                16.000")
1033   FORMAT("MAXIMUM VALUE                                70.000")
1034   FORMAT("PRIOR FOR CARRYING CAPACITY2 (in 1000s)      UNIFORM")
1035   FORMAT("MINIMUM VALUE                                 0.100")
1036   FORMAT("MAXIMUM VALUE                                 0.500")
1037   FORMAT("PRIOR FOR CALF ADDITIONAL VARIANCE (CVaddC)  UNIFORM")
1038   FORMAT("MINIMUM VALUE                                 0.200")
1039   FORMAT("MAXIMUM VALUE                                 0.600")
1040   FORMAT("PRIOR FOR EXTRA SURVIVAL (ExtraS)            UNIFORM")
1041   FORMAT("MINIMUM VALUE                                 0.500")
1042   FORMAT("MAXIMUM VALUE                                 1.000")





      END
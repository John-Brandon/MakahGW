C     Last change:  CA   27 Jun 2004   10:25 am
C The following code is an outline for developers to modify.
C The only data available is that passed into these routines
C ***********************************************************************
C ***********************************************************************

      SUBROUTINE ISLC(CATM, CATF, SIGHT, CVX, INITYR)
      DOUBLE PRECISION CATM(4,-410:99),CATF(4,-410:99),SIGHT(2,-43:99),
     +       CVX(2,-43:99)
      INTEGER INITYR
      INITYR = INITYR
      CALL JBISLC
      
!      print *, "Hello from ISLC in GUP2.FOR" ! DEBUGGING JRB
      CALL DMISLC(SIGHT, CVX,CATM, CATF)

      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE RSLC (INITYR,ISURV,NSIM)
C
C     Reset the SLC at the start of each new replicate, if required
C
C     All years are measured on a scale such that management begins in year 0
C     INITYR Year of first premanagement catch (-409 to -1)
C     ISURV  Year of first sightings survey (-43 to -1).
C     NSIM   Simulation number (i.e. replicate number)

      INTEGER INITYR,ISURV,NSIM

      CALL JBRSLC (INITYR,ISURV,NSIM)
      CALL DMRSLC (INITYR,ISURV,NSIM)

      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE SLC (CATM, CATF, SIGHT, CVX, IYR, INITYR, ISURV,
     +                NEED, NCALF, NMAT, NSAMPI, CATCHQ, NSIM, IQUOTA)

C     BOWHEADS ONLY :Subroutine sets strike quota CATCHQ

      DOUBLE PRECISION CATM(-165:99),CATF(-165:99),SIGHT(-43:99),
     +       CVX(-43:99),NEED,CATCHQ
      INTEGER IYR,INITYR,ISURV,NCALF(-43:99),NMAT(-43:99),
     +        NSAMPI(-43:99), NSIM, IQUOTA

C
C     Dummies
      IYR = IYR
      NSIM = NSIM
      IQUOTA = IQUOTA
      ISURV = ISURV
      INITYR = INITYR
      NSAMPI(1) = NSAMPI(1)
      CATM(1) = CATM(1)
      CATF(1) = CATF(1)
      CVX(1) = CVX(1)
      NCALF(1) = NCALF(1)
      NMAT(1) = NMAT(1)
      SIGHT(1) = SIGHT(1)
C      
      CATCHQ = NEED
C      
      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE SLCG (CATMI, CATFI, SIGHTI, CVXI, IYR, INITYR, ISURV,
     +                NEED, CATCHQ, NSIM, IQUOTA)

C     Subroutine sets strike quota CATCHQ for GRAY WHALES

C     All years are measured on a scale such that management begins in year 0
C     CATM(I) Catch of males in year I where I = INITYR, IYR-1
C     CATF(I) Catch of females in year I
C     SIGHT(I)Absolute abundance estimate, in year I  eg sightings. 
C             Set to -1 if no data available that year.
C     CVX(I)  Estimated CV of the sightings estimate. Set to -1 if no data.
C     IYR     Current year on scale with 0 = 1st year of management
C     INITYR  Year of first premanagement catch (-410 to 0)
C     ISURV   Year of first sightings survey (-43 to -1).
C     NEED    Current need level (as total no. of whales over next IQUOTA yrs).
C     IQUOTA  Number of years for which the quota is set
C             (annual catch = CATCHQ/IQUOTA)
C     CATCHQ  Total strike limit for IYR to IYR+IQUOTA-1, which is 
C             set by this subroutine
C     NSIM    Simulation number (i.e. replicate number)

      DOUBLE PRECISION CATMI(4,-410:99),CATFI(4,-410:99),
     +       SIGHTI(2,-43:99),CVXI(2,-43:99), NEED,CATCHQ
      DOUBLE PRECISION CATM(-410:99),CATF(-410:99),SIGHT(-43:99),
     +       CVX(-43:99)
      INTEGER INITYR,ISURV,NSIM, IQUOTA
      DOUBLE PRECISION JBCATCHQ,DMCATCHQ

      DO 10110 IY = -410,99
       CATM(IY) = CATMI(1,IY)+CATMI(2,IY)+CATMI(3,IY)+CATMI(4,IY)
       CATF(IY) = CATFI(1,IY)+CATFI(2,IY)+CATFI(3,IY)+CATFI(4,IY)
10110 CONTINUE
      DO 10120 IY = -43,99
       SIGHT(IY) = SIGHTI(1,IY)
       CVX(IY) = CVXI(1,IY)
10120 CONTINUE       


      CALL JBSLCG (CATM, CATF, SIGHT, CVX, IYR, INITYR, ISURV,
     +                NEED, JBCATCHQ, NSIM, IQUOTA)

      CALL DMSLCG (CATM, CATF, SIGHT, CVX, IYR, INITYR, ISURV,
     +                NEED, DMCATCHQ, NSIM, IQUOTA)

      CATCHQ = (JBCATCHQ+DMCATCHQ)/2.D0

      IF (CATCHQ .ge. NEED*0.95D0) CATCHQ = NEED
C      WRITE(*,*) IYR,NEED,CATCHQ
C
      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE SLCPFCG (CATMI, CATFI, SIGHTI, CVXI, IYR, INITYR, 
     +                ISURV,NEED, CATCHQ, NSIM, IQUOTA, ABL)

C     Subroutine sets strike quota CATCHQ for GRAY WHALES

C     All years are measured on a scale such that management begins in year 0
C     CATM(I) Catch of males in year I where I = INITYR, IYR-1
C     CATF(I) Catch of females in year I
C     SIGHT(I)Absolute abundance estimate, in year I  eg sightings. 
C             Set to -1 if no data available that year.
C     CVX(I)  Estimated CV of the sightings estimate. Set to -1 if no data.
C     IYR     Current year on scale with 0 = 1st year of management
C     INITYR  Year of first premanagement catch (-410 to 0)
C     ISURV   Year of first sightings survey (-43 to -1).
C     NEED    Current need level (as total no. of whales over next IQUOTA yrs).
C     IQUOTA  Number of years for which the quota is set
C             (annual catch = CATCHQ/IQUOTA)
C     CATCHQ  Total strike limit for IYR to IYR+IQUOTA-1, which is 
C             set by this subroutine
C     NSIM    Simulation number (i.e. replicate number)

      DOUBLE PRECISION CATMI(3,-410:99),CATFI(3,-410:99),
     +       SIGHTI(2,-43:99),CVXI(2,-43:99),NEED,CATCHQ
      DOUBLE PRECISION CATM(-410:99),CATF(-410:99),SIGHT(-43:99),
     +       CVX(-43:99)
      INTEGER INITYR,NSIM,ISURV,IQUOTA
      DOUBLE PRECISION NMIN,ABL
      DOUBLE PRECISION F_R, R_MAX  ! Added by JRB
      
C     Initialize 2017 values
      F_R = 0.5d0     ! PCFG specific value (NMFS) for depletion unknown
      R_MAX = 0.062d0  ! This is for ENP gray whales in general

C
C     Dummy Code
      CATCHQ = CATCHQ
      NSIM = NSIM
      INITYR = INITYR
      NEED = NEED
      ISURV = ISURV
      IQUOTA = IQUOTA
C
      DO 10110 IY = -410,99
       CATM(IY) = CATMI(3,IY)
       CATF(IY) = CATFI(3,IY)
10110 CONTINUE
      DO 10120 IY = -43,99
       SIGHT(IY) = SIGHTI(2,IY)
       CVX(IY) = CVXI(2,IY)
10120 CONTINUE       
C
C     Three year delay between a survey be conducted and being used
      DO 20000 IY = IYR-3,-43,-1
       IF (SIGHT(IY).GT.0) GOTO 20100
20000 CONTINUE       
20100 CONTINUE
C      WRITE(*,*) 2010+IYR,2010+IY,SIGHT(IY)
      NMIN = SIGHT(IY)/EXP(0.842*SQRT(LOG(1+CVX(IY)**2.0)))
      ABL = NMIN * 0.5 * R_MAX * F_R
      ! ABL = MAX(0.0d0,MIN(NEED,ABL))

      ! DEBUGGING JRB
      ! print *, "ABL = MAX(0.0d0,MIN(NEED,ABL))", ABL      
C
      RETURN
      END



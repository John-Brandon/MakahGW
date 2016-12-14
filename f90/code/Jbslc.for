C     Last change:  CA   27 Jun 2004    2:09 pm
C ***********************************************************************
C ***********************************************************************
C
C    SLC by Johnston and Butterworth                            Dec 2000
C     for Gray whales
C
C ***********************************************************************
C
      SUBROUTINE JBISLC
C
C     Initialise the SLC  (subsistence limit calculation)
C
      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE JBRSLC (INITYR,ISURV,NSIM)
C
C     All years are measured on a scale such that management begins in year 0
C     INITYR Year of first premanagement catch (-165 to -1)
C     ISURV  Year of first sightings survey (-16 to -1). 
C     NSIM   Simulation number (i.e. replicate number)

      INTEGER INITYR,ISURV,NSIM
C
C     Dummy variables
      INITYR = INITYR
      ISURV = ISURV
      NSIM = NSIM
C
      RETURN
      END


C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C*****************************************************************************


c**************************************************************

      SUBROUTINE JBSLCG (CATM, CATF, SIGHT, CVX, IYR, INITYR, ISURV,
     +                NEED, CATCHQ, NSIM, IQUOTA)

C     All years are measured on a scale such that management begins in year 0
C     CATM(I)  Catch of males in year I where I = INITYR, IYR-1
C     CATF(I)  Catch of females in year I
C     SIGHT(I) Absolute abundance estimate, in year I  eg sightings. 
C              Set to -1 if no data available that year.
C     CVX(I)   Estimated CV of the sightings estimate
C              Set to -1 if no data available.  
C     IYR      Current year on scale with 0 = 1st year of management
C     INITYR   Year of first premanagement catch (-411 to 0)
C     ISURV    Year of first sightings survey (-44 to -1).
C     NEED     Current need level (as number of whales).(annual catch=catchq/iquota
C     CATCHQ   Catch limit for IYR which is set by this subroutine
C     NSIM     Simulation number (i.e. replicate number)

      DOUBLE PRECISION CATM(-411:99),CATF(-411:99),SIGHT(-44:99),
     +       CVX(-44:99),CVXX(-44:99),NEED,CATCHQ
      INTEGER IYR,INITYR,ISURV,NSIM,IQUOTA,DW

      DOUBLE PRECISION X(1:2),F,lnL,K,r, N(-411:120),finr
      DOUBLE PRECISION C(-411:120),EST(-44:99),fink,value,xalpha
      DOUBLE PRECISION Maxc(-411:120),RSTAR,SIGMAR,XX,UCC,LCC
      INTEGER y,first,end,surv

      DOUBLE PRECISION  Z1,Z2,Cmin,Cmax,Catch,lam,WT(-44:99)
      CHARACTER*1    Gmin,Gmax,Gmid,G
      INTEGER   Tol, Nmax, j,jiyr,jnsim
      
      COMMON / Catch / C
      COMMON / years / first, end
      COMMON / survey / surv, EST, CVxx, WT
      COMMON / Ns / N
      COMMON / Xs / finK,finr
      COMMON / VAR / RSTAR,SIGMAR,XX,UCC,LCC
      common / new / jnsim,jiyr

      open(unit=15,file='out.res',action='write')
      open(unit=16,file='cout.res',action='write')
      open(unit=17,file='DW.res',action='write')

      jnsim=nsim
      jiyr=iyr
!   ****** DEFINING VARIABLES TO BE PASSED TO F

C********************************************************************
C       THESE ARE THE FIVE VARIABLES THAT CAN BE CHANGES TO PRODUCE
c           DIFFERENT VERSIONS OF THE SLA

c ********* downweight of earlier data ? **********
c      DW=1  !no downweight
      DW=2  !downweight earlier sighting data in likelihood

      lam=0.1
c      lam=0.01

c*******************************************
c      RSTAR = 0.01d0
      RSTAR = 0.03d0

c      SIGMAR = 0.01d0
c       sigmar=0.005d0
      sigmar=0.001d0
      XX = 1.2d0

      UCC = 1000.d0   !UPPER INTER-5-YEAR CATCH VARIATION CONSTRAINT (30%)
      LCC = 0.15d0     !lOWER INTER-5-YEAR CATCH VARIATION CONSTRAINT (15%)



C**********************************************************************

      DO y = INITYR, IYR-1
       C(y) = CATM(y) + CATF(y)
      END DO

c      first = INITYR
      first=-73
      end = IYR

      DO y = ISURV, IYR
       EST(y) = SIGHT(y)  !ABSOLUTE ABUNDANCE SURVEY ESTIMATES
       CVXX(y) = CVX(y)    !CVs OF ABUNDACNE SURVEY ESTIMATES
      END DO


      if(DW.eq.2)then
      do y=ISURV,IYR
      WT(y)=exp(-lam*(IYR-y))
      enddo
      endif


      if(DW.eq.1)then
      do y=ISURV,IYR
      WT(y)=1.0
      enddo
      endif

      surv = ISURV

!   ****** DETERMINING A REASONABLE STARTING VALUE FOR K



      K=100000
      r = 0.03d0              !starting value for r

      X(1) = K 
      X(2) = r

      CALL FIT(X,lnL,2)  !CALLS ROUTINE TO FIT R AND K





      lnL = F(X)



      finr=x(2)   !final best estimate of r
      finK=X(1)   !final best estimate of K


 
C********** WE NOW HAVE A VALUE OF k AND R TO PROJECT FORWARDS ****

      DO y = IYR, IYR+20
       C(y) = NEED/5.d0
       value=N(y)/x(1)
       if(value.le.0)value=0.00001
       N(y+1) = N(y) + X(2) * N(y) * (1 - value**2.39d0) - C(y)
      END DO


       Z1 = N(IYR+20) / X(1)
       Z2 = N(IYR+20) / N(IYR)


      IF ((Z1 .GE. 0.6d0) .OR. (Z2 .GE. XX)) THEN

      CATCHQ = NEED  !catchq = 5 yr total catch

      ELSE

C***** WORK OUT THE MAXIMUM CATCH SO THAT EITHER CONDITION IS MET ***

        Cmin = 0.d0
        Cmax = NEED/5.d0
        Tol = 1.d0


        DO 452 J=1,300

         catch = Cmax

        Gmin = G(Cmin)
        Gmax = G(Cmax)

        if(abs(Cmax-Cmin).lt.tol) catch = Cmax
        if(abs(Cmax-Cmin).lt.tol) goto 453

        if(Gmin.eq.'n') catch = 0.d0

        if(Gmax.eq.'n') Cmax = Cmax - (Cmax-Cmin)/2.d0

        if(Gmax.eq.'y') Cmax = Cmax + (Cmax-Cmin)/2.d0

        if(Gmax.eq.'y') Cmin = catch

 452  continue
 453  continue

      CATCHQ = CATCH*5.d0   ! CATCHQ=5 YEAR QUOTA



      ENDIF

c**** inter-annual catch constraint **********

      MaxC(iyr)=catchq

      MAXC(iyr-5)=(catm(iyr-5)+Catf(iyr-5))*5.d0

      if(maxc(iyr).lt.((1.d0-LCC)*maxc(iyr-5)))maxc(iyr)=
     &maxc(iyr-5)*(1.d0-LCC)

      if(maxc(iyr).gt.((1.d0+UCC)*maxc(iyr-5)))maxc(iyr)=
     &maxc(iyr-5)*(1.d0+UCC)

      catchq=maxc(iyr)



4675  continue



c      CATCHQ=NEED


      write(16,*)nsim,iyr,catchq,need,fink

      RETURN
      END
                   
c************************************************************

      DOUBLE PRECISION FUNCTION F(X)

C     All years are measured on a scale such that management begins in year 0
C     CATM(I)  Catch of males in year I where I = INITYR, IYR-1
C     CATF(I)  Catch of females in year I
C     SIGHT(I) Absolute abundance estimate, in year I  eg sightings. 
C              Set to -1 if no data available that year.
C     CVX(I)   Estimated CV of the sightings estimate
C              Set to -1 if no data available.  
C     IYR      Current year on scale with 0 = 1st year of management
C     INITYR   Year of first premanagement catch (-165 to 0)
C     ISURV    Year of first sightings survey (-16 to -1).
C     NEED     Current need level (as number of whales).
C     CATCHQ   Catch limit for IYR which is set by this subroutine
C     NSIM     Simulation number (i.e. replicate number)

!      DOUBLE PRECISION CATM(-411:99),CATF(-411:99),SIGHT(-44:99),
!     +       CVX(-44:99),NEED,CATCHQ
      INTEGER IYR,INITYR,ISURV,NSIM

      DOUBLE PRECISION rstar,sigmar,N(-411:120),C(-411:120),
     +        EST(-44:99),CVXX(-44:99),X(1:2),lnL,LNL1,LNL2,
     +        XX,UCC,LCC,WT(-44:99)
      INTEGER y,first,end,surv,jnsim,jiyr
      
      COMMON / Catch / C
      COMMON / years / first, end
      COMMON / survey / surv, EST, CVXX, WT
      COMMON / Ns / N
      COMMON / VAR / RSTAR,SIGMAR,XX,UCC,LCC
      common / new / jnsim,jiyr


      open(unit=20,file='fit.res',action='write')

      if(x(1).le.0.d0 .or. x(2).le.0.d0)then
      F=1d19
      return
      endif

      if(x(1).gt.50000)then
       x(1)=50000
      endif

      xalpha=0.255

      N(first) = xalpha*X(1)


!   ****** CALCULATING NUMBER OF WHALES
      DO y = first, end-1
      value=N(y)/x(1)
      if(value.le.0)value=0.000001
   
       N(y+1) = N(y) + X(2) * N(y) * (1 - value**2.39d0) - C(y)

       IF (N(y+1) .LT. 0) THEN
        F = 1d19
        N(y+1) = 0.1
        RETURN
       END IF


      ENDDO



      lnL = 0.d0


!   *** CALCULATING lnL
      DO y = SURV, end
       IF (EST(y) .NE. -1.d0) THEN
       if(est(y).LE.0)est(y)=1.0
         lnL = lnL +WT(y) * ((LOG(EST(y)) - LOG(N(y))) **2 )
     &    /(2.d0 * CVXX(y)**2)

       END IF

      END DO


      LNL1=LNL

      lnL2=  ( (X(2) - rstar)**2 ) / (2.d0 * sigmar**2)




      LNL=LNL1+LNL2

      F=lnL



      RETURN
      END

! *****************************************************************

      CHARACTER*1 FUNCTION G(Cpass)

      DOUBLE PRECISION N(-411:120),finr,fink,value
      INTEGER y,first,end
      DOUBLE PRECISION Cpass, Z1, Z2, RSTAR,SIGMAR,XX,UCC,LCC

      COMMON / years / first, end
      COMMON / Xs / finK,finr
      COMMON / Ns / N
      COMMON / VAR / RSTAR,SIGMAR,XX,UCC,LCC

      DO y = end, end+20
       value=N(y)/finK
       if(value.le.0)value=0.000001
       N(y+1) = N(y) + finr * N(y)*(1-value**2.39d0) - Cpass
      END DO
      Z1 = N(end+20) / finK
      Z2 = N(end+20) / N(end)


      IF ((Z1 .GE. 0.6d0) .OR. (Z2 .GE. XX)) THEN
       G = 'y'
      ELSE 
       G = 'n'
      END IF
      
      RETURN
      END

                   
!  *****************************************************************
      SUBROUTINE FIT(X,SS,NDIM)

!     Set up the parameters for a fit
                   
      IMPLICIT NONE

      DOUBLE PRECISION P(3,2),Y(3),X(2),SS,F,TOL,GRD  
      INTEGER NDIM,I,J,ITER

!     SET UP TOLERANCES AND GRIDDING

      
!      OPEN(7, FILE = 'plow')
!      REWIND(7)
      TOL = 0.00002d0
      

      GRD = 1.2d0

      DO I=1,NDIM+1
        DO J=1,NDIM
          P(I,J)=X(J)
          IF((I-1).EQ.J) P(I,J) = GRD*P(I,J)
        END DO
      END DO

      DO I=1,NDIM+1
        DO J=1,NDIM
          X(J)=P(I,J)
        END DO
        Y(I)=F(X)             !the ss for P with this I
      END DO

!      CALL AMOEBA(P,Y,51,50,NDIM,TOL,ITER)
      CALL AMOEBA(P,Y,3,2,NDIM,TOL,ITER)

      DO J = 1,NDIM
         X(J) = P(1,J)
      END DO
      SS = Y(1)

!      CLOSE(7)

      END
                                                                   
! ***************************************************************** 
                                                                   
      SUBROUTINE AMOEBA(P,Y,MP,NP,NDIM,FTOL,ITER)

!     MULTIDIMENSIONAL MINIMISATION OF THE FUNCTION FUNK(X) WHERE X IS
!     AN NDIM-DIMENSIONAL VECTOR, BY THE DOWNHILL SIMPLEX METHOD OF
!     NELDER AND MEAD. INPUT IS A MATRIX P WHOSE NDIM+1 ROWS ARE THE
!     NDIM-DIMENSIONAL VECTORS WHICH ARE THE VERTICES OF THE STARTING
!     SIMPLEX. [LOGICAL DIMENSIONS OF P ARE P(NDIM+1,NDIM); PHYSICAL
!     DIMENSIONS ARE INPUT AS P(MP,NP).] ALSO INPUT IS THE VECTOR Y
!     OF LENGTH NDIM+1, WHOSE COMPONENTS MUST BE PRE-INITIALISED TO
!     THE VALUES OF FUNK EVALUATED AT THE NDIM+1 VERTICES (ROWS) OF P;
!     AND FTOL IS THE FRACTIONAL CONVERGENCE TOLERANCE TO BE ACHIEVED
!     IN THE FUNCTION VALUE (N.B.!). ON OUTPUT, P AND Y WILL HAVE BEEN
!     RESET TO NDIM+1 NEW POINTS ALL WITHIN FTOL OF A MINIMUM FUNCTION
!     VALUE, AND ITER GIVES THE NUMBER OF ITERATIONS TAKEN.

!     FROM: NUMERICAL RECIPES - THE ART OF SCIENTIFIC COMPUTING
!           BY W. H. PRESS ET AL, CAMBRIDGE UNIVERSITY PRESS
!           ISBN 0-251-30811-9

!     ********************************************************************


!     SPECIFY THE MAXIMUM NUMBER OF DIMENSIONS, THREE PARAMETERS WHICH
!     DEFINE THE EXPANSIONS AND CONTRACTIONS, AND THE MAXIMUM NUMBER OF
!     ITERATIONS ALLOWED

      IMPLICIT NONE
      DOUBLE PRECISION ALPHA,BETA,GAMMA
      INTEGER*4 ITMAX
      PARAMETER (ALPHA=1.0,BETA=0.5,GAMMA=2.0,ITMAX=300)!600)
                
!     Global Data
      INTEGER*4 MP,NP,NDIM,ITER
!      REAL*4 P(MP,NP),Y(MP),PR(50),PRR(50),PBAR(50),FTOL,F
      DOUBLE PRECISION P(MP,NP),Y(MP),PR(2),PRR(2),PBAR(2),FTOL,F

!     Local Data 
      DOUBLE PRECISION RTOL,YPR,YPRR, swop
      INTEGER*4 I,ILO,IHI,INHI,J,MPTS

!     NOTE THAT MP IS THE PHYSICAL DIMENSION CORRESPONDING TO THE LOGICAL
!     DIMENSION MPTS, NP TO NDIM.

      MPTS = NDIM+1
      ITER=0

!     FIRST WE MUST DETERMINE WHICH POINT IS THE HIGHEST (WORST), NEXT
!     HIGHEST, AND LOWEST (BEST).

1     ILO=1
      IF(Y(1).GT.Y(2)) THEN
        IHI=1
        INHI=2
       ELSE
        IHI=2
        INHI=1
      END IF
      DO I=1,MPTS
         IF(Y(I).LT.Y(ILO)) ILO=I
         IF(Y(I).GT.Y(IHI)) THEN
           INHI=IHI
           IHI=I
          ELSE IF(Y(I).GT.Y(INHI)) THEN
                 IF(I.NE.IHI) INHI=I
         ENDIF
      END DO


!     COMPUTE THE FRACTIONAL RANGE FROM THE HIGHEST TO THE LOWEST AND
!     RETURN IF SATISFACTORY

      IF (ABS(Y(IHI))+ABS(Y(ILO)).EQ.0.0) THEN
        swop = Y(1)
        Y(1) = Y(ILO)           !set Y(1) = smallest (best)
        Y(ILO) = swop
        DO J = 1, NDIM
          swop = P(1,J)
          P(1,J) = P(ILO,J)     !set P(1,J) = smallest
          P(ILO,J) = swop
        END DO
        PRINT*, 'abs = 0'
        RETURN
      END IF
      RTOL=2.*ABS(Y(IHI)-Y(ILO))/(ABS(Y(IHI))+ABS(Y(ILO)))
      IF(RTOL.LT.FTOL) THEN
        swop = Y(1)             !set Y(1) = smallest (best)
        Y(1) = Y(ILO)
        Y(ILO) = swop
        DO J = 1, NDIM
          swop = P(1,J)         
          P(1,J) = P(ILO,J)     !set P(1,J) = smallest
          P(ILO,J) = swop
        END DO
!        PRINT*, '< ftol'
        RETURN
      END IF
      IF(ITER.EQ.ITMAX) THEN
!        WRITE(*,200)
200     FORMAT(1H ,'AMOEBA EXCEEDING MAXIMUM ITERATIONS')
        swop = Y(1)
        Y(1) = Y(ILO)           !set Y(1) = smallest (best)
        Y(ILO) = swop
        DO J = 1, NDIM
          swop = P(1,J)
          P(1,J) = P(ILO,J)     !set P(1,j) = smallest
          P(ILO,J) = swop
        END DO
        RETURN
      END IF
      ITER=ITER+1

      DO J=1,NDIM
         PBAR(J)=0.d0
      END DO

!     BEGIN A NEW ITERATION. COMPUTE THE VECTOR AVERAGE OF ALL POINTS
!     EXCEPT THE HIGHEST, I.E. THE CENTRE OF THE "FACE" OF THE SIMPLEX
!     ACROSS FROM THE HIGH POINT. WE WILL SUBSEQUENTLY EXPLORE ALONG
!     THE RAY FROM THE HIGH POINT THROUGH THE CENTRE.

      DO I=1,MPTS
         IF(I.NE.IHI) THEN
           DO J=1,NDIM
             PBAR(J)=PBAR(J)+P(I,J)
           END DO
         END IF
      END DO

!     EXTRAPOLATE BY A FACTOR ALPHA THROUGH THE FACE, I.E. REFLECT THE
!     SIMPLEX FROM THE HIGH POINT

      DO J=1,NDIM
         PBAR(J)=PBAR(J)/NDIM
         PR(J)=(1.d0+ALPHA)*PBAR(J)-ALPHA*P(IHI,J)
      END DO
       
!     EVALUATE THE FUNCTION AT THE REFLECTED POINT

      YPR=F(PR)

!     GIVES A BETTER RESULT THAN THE BEST POINT, SO TRY AN ADDITIONAL
!     EXTRAPOLATION BY A FACTOR GAMMA

      IF(YPR.LE.Y(ILO)) THEN

!       WRITE(7,*) pr(1),pr(2),ypr
!       WRITE(7,*) 'two'

        DO J=1,NDIM
          PRR(J)=GAMMA*PR(J)+(1.d0-GAMMA)*PBAR(J)
        END DO

!        CHECK THE FUNCTION THERE

        YPRR=F(PRR)

!        THE ADDITIONAL EXTRAPOLATION SUCCEEDED, AND REPLACES THE
!        HIGHEST POINT

        IF(YPRR.LT.Y(ILO)) THEN

!          WRITE(7,*) prr(1), prr(2), prr(3), yprr
!          WRITE(7,*) 'three'

          DO J=1,NDIM
            P(IHI,J)=PRR(J)
          END DO
          Y(IHI)=YPRR
         ELSE

!        THE ADDITIONAL EXTRAPOLATION FAILED, BUT WE CAN STILL USE THE
!        REFLECTED POINT

          DO J=1,NDIM
            P(IHI,J)=PR(J)
          END DO
          Y(IHI)=YPR
        ENDIF

!     THE REFLECTED POINT IS WORSE THAN THE SECOND HIGHEST

       ELSE IF(YPR.GE.Y(INHI)) THEN

!        IF IT'S BETTER THAN THE HIGHEST, THEN REPLACE THE HIGHEST

              IF(YPR.LT.Y(IHI)) THEN
                DO J=1,NDIM
                  P(IHI,J)=PR(J)
                END DO
                Y(IHI)=YPR
              END IF

!        BUT LOOK FOR AN INTERMEDIATE LOWER POINT; IN OTHER WORDS
!        PERFORM A CONTRACTION OF THE SIMPLEX ALONG ONE DIMENSION
!        AND THEN EVALUATE THE FUNCTION

              DO J=1,NDIM
                PRR(J)=BETA*P(IHI,J)+(1.d0-BETA)*PBAR(J)
              END DO
              YPRR=F(PRR)

!        CONTRACTION GIVES AN IMPROVEMENT, SO ACCEPT IT

              IF(YPRR.LT.Y(IHI)) THEN
                DO J=1,NDIM
                  P(IHI,J)=PRR(J)
                END DO
                Y(IHI)=YPRR
               ELSE

!           CAN'T SEEM TO GET RID OF THAT HIGH POINT. BETTER CONTRACT
!           AROUND THE LOWEST (BEST) POINT

                DO I=1,MPTS
                  IF(I.NE.ILO) THEN
                    DO J=1,NDIM
                      PR(J)=0.5d0*(P(I,J)+P(ILO,J))
                      P(I,J)=PR(J)
                    END DO
                    Y(I)=F(PR)
                  END IF
                END DO
              END IF

       ELSE

!        WE ARRIVE HERE IF THE ORIGINAL REFLECTION GIVES A MIDDLING
!        POINT. REPLACE THE OLD HIGH POINT AND CONTINUE

        DO J=1,NDIM
          P(IHI,J)=PR(J)
        END DO 
        Y(IHI)=YPR

      ENDIF

      GO TO 1

      END


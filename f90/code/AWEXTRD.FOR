C     ------------------------------------------------------------------
C     ------------------------------------------------------------------

      SUBROUTINE MSYPAR (OK,IS)

C    MSYPAR finds the resilience A1 & density dependent exponent Z
C    giving the required MSY at the specified MSYL.  It equates the
C    slope in the balancing per capita birthrates at two levels of
C    fishing mortality just above and below MSY, with the derivative of
C    per capita birth rate at MSYL. A root finding routine using Brent's
C    method is used to solve for the density dependent exponent.

      USE DECL_STKVRS
      USE DECL_SETUP

      EXTERNAL ZCALC
      DOUBLE PRECISION P,UF(2),PARS(4),ZCALC,FMSY2,PA,RMAT,DF,TOL,
     +       ZLO,ZHI,F1,F2,U1PLUS(2),UMAT(2),PADJST,UM,U1,
     +       MSYLD,TERM,TERM1,TERM2,TERM3
      INTEGER I,IERR,L,OK,IS
      LOGICAL GOOD
C
C     Set up tolerances
      DF  = 0.00001D0
      TOL = 0.00001D0

C     Note SUR array set in RESET or in conditioning (in GENPAR & HITPEG)
C     Set up maturity ogive: FMATUR =proportion of age A which are mature {EqnA5}
C     Knife edge at age MAT50 in this version
      CALL SETO (FMATUR,0.d0,MAT50(IS),MAXAGE,0,IS)
C     Set up recruitment ogive RECF: knife edge at age AREC (=1bowheads)  {Eqn A4.1}
      CALL SETO (RECF,0.d0,AREC,MAXAGE,0,IS)
C
C     Set SUMSUR = SUR + SUR**2 + ..... + SUR**NY. Note SUMSUR=0 if NY=0
C     SUMSUR is used to set birth rate where NY =the minimum calving interval
      SUMSUR(IS) = 0.D0
      DO 7 I = 1,NY
        SUMSUR(IS) = SUMSUR(IS) + SUR(IS,MAXAGE)**I
   7  CONTINUE

C *** Add the relative mature & recruited pristine population sizes starting
C     with unity in the zero age class (L=0)  (PA=relative no.in Lth age class)
      RMAT = FMATUR(IS,0)
      RREC(IS) = RECF(IS,0)
      R1PLUS(IS) = 0.D0
      PA   = SUR(IS,0)
      DO 9 L = 1,MAXAGE-1
        RMAT = RMAT + PA*FMATUR(IS,L)
        RREC(IS) = RREC(IS) + PA*RECF(IS,L)
        R1PLUS(IS) = R1PLUS(IS) + PA
        PA   = PA*SUR(IS,L)
    9 CONTINUE

C     Adjust for last age class being pooled (and fully recruited / mature)
      PA   = PA/(1.D0 - SUR(IS,MAXAGE))
      RMAT = RMAT + PA
      RREC(IS) = RREC(IS) + PA
      R1PLUS(IS) = R1PLUS(IS) + PA

C     Set the birth rates FEC & F0 so as to give balance at equilibrium
C     F0  = # of age 0 / # mature (M+F) in pristine population 
C     FEC = # of age 0 / # of available mature females (pristine) {Eqn A2.1}
C     Note: FEC = 2F0 if NY=0 = Standard Hitter-Fitter
C           SUMSUR = SUR + SUR**2 + ... + SUR**NY. SUMSUR=0 if NY=0
      FEC(IS) = 1.d0 / (RMAT/2.d0 - SUMSUR(IS))
      F0(IS)  = 1.d0 / RMAT
 
C     Set up the recruitment ogive in transition form:
C     RECF(A+1) = fraction of unrecruited animals of age A which recruit
C     at age A+1, except RECF(0) = fraction recruited of age 0  {Eqn A4.2}
      CALL TRFORM(RECF,MAXAGE,IS)

C *** Estimate birth rate at levels of survival which bound MSYR
C     Set two levels of survival after fishing to closely straddle MSY
      UF(1) = 1.D0 - MSYR(IS) + DF*.5D0
      UF(2) = UF(1) - DF

      DO 100 I = 1,2

C       Add numbers of mature & 1+, relative to # of age 0
C       P = number of age A relative to # of age 0, starting with age 1
        P = SUR(IS,0)
        UMAT(I) = P*FMATUR(IS,1)
        U1PLUS(I) = P

        IF (OPTF.EQ.0) THEN
C         Fishing pattern is uniform on 1+ population.
          DO 10 L = 2,MAXAGE
            P = P * SUR(IS,L-1) * UF(I)
            UMAT(I) = UMAT(I) + P*FMATUR(IS,L)
            U1PLUS(I) = U1PLUS(I) + P
   10     CONTINUE
        ELSE
C         Fishing pattern is uniform on mature population
          DO 20 L = 2,MAXAGE
            P = SUR(IS,L-1) * P * (1.D0 - (1.D0-UF(I))*FMATUR(IS,L-1))
            UMAT(I) = UMAT(I) + P*FMATUR(IS,L)
            U1PLUS(I) = U1PLUS(I) + P
   20     CONTINUE
        ENDIF
C       Adjust for last age class for pooling
        PADJST = 1.D0/(1.D0 - SUR(IS,MAXAGE)*UF(I))
        UMAT(I) = UMAT(I) - P*FMATUR(IS,MAXAGE) * (1.D0 - PADJST)
        U1PLUS(I) = U1PLUS(I) - P*(1.D0 - PADJST)
C
C       Save birth rate (=births/mature female)
        UF(I) = 2.D0/UMAT(I)
  100 CONTINUE

      UM   =  (UMAT(1) + UMAT(2)) * 0.5D0
      U1   =  (U1PLUS(1) + U1PLUS(2)) * 0.5D0

C     Setup MSYLD (JCRM 1:270 eqn 20)
      IF (OPTDD.EQ.OPMSYL) THEN
C       MSYL defined on density dependent component of population
        MSYLD = MSYL(IS)
      ELSE IF (OPMSYL.EQ.0 .AND. OPTDD.EQ.1) THEN
C       MSYL on 1+;  DD on mature
        MSYLD = MSYL(IS) * R1PLUS(IS) * UM * F0(IS) / U1
      ELSE IF (OPMSYL.EQ.1 .AND. OPTDD.EQ.0) THEN
        MSYLD = MSYL(IS) * U1 / (R1PLUS(IS) * UM * F0(IS))
      ENDIF

C     TERM (1st 2 terms in {} in eqn A7.3)
      IF (OPTDD.EQ.OPTF) THEN
C       MSYR defined on density dependent component of population
        TERM = 0.D0
      ELSE
        TERM = ((U1PLUS(2)-U1PLUS(1))/U1 - (UMAT(2)-UMAT(1))/UM) / DF
        IF (OPTF.EQ.1 .AND. OPTDD.EQ.0) TERM = -TERM
      ENDIF

C     Place the values to be passed into PARS to solve eqn A7.3
C     Note FMSY2 = 2*f(FMSY) = births / mature female  &  term1 = 2 * df/dF
      PARS(1) = 1.D0 + TERM*MSYR(IS)
      FMSY2 = (UF(1) + UF(2)) * .5D0
      TERM1 = (UF(2) - UF(1)) / DF
      TERM2 = 1.d0 - FMSY2*SUMSUR(IS)
      TERM3 = FMSY2 * (1.d0+FEC(IS)*SUMSUR(IS)) - FEC(IS)
      PARS(2) = MSYR(IS) * TERM1 / (TERM2 * TERM3)
      PARS(3) = MSYLD

C     Find density dependent exponent Z
      ZLO =  -5.D0
      ZHI =   5.D0
      CALL ZBRAC (ZCALC, PARS, 4, ZLO, ZHI, F1, F2, GOOD)
      IF (.NOT.GOOD) GO TO 1000
      CALL ZBRENT (ZCALC, PARS, 4, ZLO, ZHI, F1, F2, TOL, IERR)
      Z(IS) = PARS(4)
      IF (Z(IS)*LOG(MSYLD).GT.20.D0) GO TO 1000

C     Compute the resilience parameter A {Eqn A7.2}
      A1(IS) = (FMSY2*(1.D0+FEC(IS)*SUMSUR(IS))-FEC(IS)) /
     +     (FEC(IS)*(1.D0-FMSY2*SUMSUR(IS))*(1.D0 - MSYLD**Z(IS)))
      OK = 1
      RETURN

C *** Error exit
 1000 OK = 0
      RETURN
      END

C     -----------------------------------------------------------------

      FUNCTION ZCALC (PARS,IERR)

C     The root of this function = density dependent exponent z {Eqn A7.3}
C     PARS(1) = 1 + F (dP/dF/P - dPD/dF/PD)         
C     PARS(2) = F * df(Fmsy)/dF / (1-2Fmax*SUMSUR) * (2Fmax(1+FEC*SUMSUR)-FEC)
C     PARS(3) = MSYLD
C     PARS(4) = z

      DOUBLE PRECISION ZCALC,PARS(4),P4
      INTEGER IERR

      P4 = PARS(4)
      IF (P4.GT.1000.D0) P4 = 1000.D0
      IERR = 0
      IF (ABS(P4) .LE. 0.001D0) THEN
        ZCALC = PARS(1) - PARS(2)*LOG(PARS(3))
      ELSE
        ZCALC = PARS(1) - PARS(2)*(PARS(3)**(-P4)-1.D0) / P4
      ENDIF
      END

C     -----------------------------------------------------------------
C     -----------------------------------------------------------------

      SUBROUTINE MSYP2 (MSYL1,MSYLM,MSYLE,MSYR1,MSYRM,MSYRE,IS)

C     Set MSYL & MSYR in terms of the 1+ and mature components of population
C     (For information and use by RES program)

      USE DECL_SETUP
      USE DECL_STKVRS

      EXTERNAL MSYM,MSY1,MSYE
      DOUBLE PRECISION MSYL1(2),MSYLM(2),MSYR1(2),MSYRM(2),
     +       MSYRE(2),MSYLE(2),MSYM,
     +       MSY1,MSYE,PARS(4),TOL,ZLO,ZHI,F1,F2
      INTEGER IERR,IS
      LOGICAL GOOD
      TOL = 0.00001D0
C
C     Set the ISPASS variables
      ISPass = IS
C
C     1st set PARS(1) = pristine R1+; PARS(4) = pristine RREC
      PARS(1) = R1PLUS(IS)
      PARS(4) = RREC(IS)
      ZLO =  MSYR(IS) * 0.5D0
      ZHI =  MSYR(IS) * 1.5D0

C     Find mature values
      IF (OPTF.EQ.1.AND.OPMSYL.EQ.1) THEN
        MSYRM(IS) = MSYR(IS)
        MSYLM(IS) = MSYL(IS)
      ELSE
        CALL ZBRAC (MSYM, PARS, 3, ZLO, ZHI, F1, F2, GOOD)
        IF (.NOT.GOOD) STOP '*****  ERROR:  FAILURE TO SOLVE FOR MSYR'
        CALL ZBRENT (MSYM, PARS, 3, ZLO, ZHI, F1, F2, TOL, IERR)
        MSYRM(IS) = PARS(3)
        MSYLM(IS) = PARS(2)
      ENDIF

C     Find 1+ values
      IF (OPTF.EQ.0.AND.OPMSYL.EQ.0) THEN
        MSYR1(IS) = MSYR(IS)
        MSYL1(IS) = MSYL(IS)
      ELSE
        CALL ZBRAC (MSY1, PARS, 3, ZLO, ZHI, F1, F2, GOOD)
        IF (.NOT.GOOD) STOP '*****  ERROR:  FAILURE TO SOLVE FOR MSYR'
        CALL ZBRENT (MSY1, PARS, 3, ZLO, ZHI, F1, F2, TOL, IERR)
        MSYR1(IS) = PARS(3)
        MSYL1(IS) = PARS(2)
      ENDIF
C
C     Find Exploitable values
      CALL ZBRAC (MSYE, PARS, 3, ZLO, ZHI, F1, F2, GOOD)
      IF (.NOT.GOOD) STOP '*****  ERROR:  FAILURE TO SOLVE FOR MSYRe'
      CALL ZBRENT (MSYE, PARS, 3, ZLO, ZHI, F1, F2, TOL, IERR)
      MSYRE(IS) = PARS(3)
      MSYLE(IS) = PARS(2)
      

      RETURN
      END

C     -----------------------------------------------------------------

      FUNCTION MSY1 (PARS,IERR)

C     Subroutine used to find MSYR and MSYL for the 1+ population
C     using Eqn A7.3

      USE DECL_STKVRS

      DOUBLE PRECISION MSY1,PARS(4),U1PLUS(2),UMAT(2),FMSY2,P,PADJST,
     +       MSYLDZ,F,UF(2),U1,UM,TERM,DF,TEMP,TERM1,TERM2,TERM3,DfDF
      INTEGER IERR,L,I

C     The function is 0 when PARS(3) = MSYR for the 1+ population
C     R1PLUS=PARS(1),  MSYR(1+) = PARS(3),  MSYL(1+) = PARS(2)

      IERR = 0
C *** Estimate birth rate at levels of survival which bound MSYR
      F = PARS(3)
      DF  = 0.00001D0
      UF(1) = 1.D0 - F + DF*.5D0
      UF(2) = UF(1) - DF

      DO 100 I = 1,2

C       Add numbers of mature & 1+, relative to # of age 0
C       P = number of age A relative to # of age 0, starting with age 1
        P = SUR(ISPass,0)
        UMAT(I) = P*FMATUR(ISPass,1)
        U1PLUS(I) = P

C       Fishing pattern is uniform on 1+ population
        DO 10 L = 2,MAXAGE
          P = P * SUR(ISPass,L-1) * UF(I)
          UMAT(I) = UMAT(I) + P*FMATUR(ISPass,L)
          U1PLUS(I) = U1PLUS(I) + P
   10   CONTINUE
C       Adjust for last age class for pooling
        PADJST = 1.D0/(1.D0 - SUR(ISPass,MAXAGE)*UF(I))
        UMAT(I) = UMAT(I) - P*FMATUR(ISPass,MAXAGE) * (1.D0 - PADJST)
        U1PLUS(I) = U1PLUS(I) - P*(1.D0 - PADJST)
C
C       Save birth rate
        UF(I) = 2.D0/UMAT(I)
  100 CONTINUE

C     Set MSYLD**Z using {Eqn A7.1} with 2f=FMSY2 and b(eq)=FEC
      FMSY2 = (UF(1) + UF(2)) * 0.5D0
      TEMP  = FMSY2 / (FEC(ISPass)*(1.d0-FMSY2*SUMSUR(ISPass)))
      MSYLDZ = MAX (0.0001D0, 1.D0 - (TEMP-1.D0)/A1(ISPass))
C     Now set MSYL(1+) {JCRM 1:270 eqn 20} and solve eqn A7.3 to find MSYR1
      IF (OPTDD.EQ.0) THEN
C       Density dependence on 1+
        TERM = 0.D0
        PARS(2) = MSYLDZ**(1.D0/Z(ISPass))
      ELSE 
C       Density dependence on mature
        UM   = (UMAT(1) + UMAT(2)) * 0.5D0
        U1   = (U1PLUS(1) + U1PLUS(2)) * 0.5D0
        TERM = ((U1PLUS(2)-U1PLUS(1))/U1 - (UMAT(2)-UMAT(1))/UM) *F / DF
        PARS(2) = MSYLDZ**(1.D0/Z(ISPass)) * U1 
     +          / (UM * F0(ISPASS) * PARS(1)) 
      ENDIF
      DfDF = (UF(2)-UF(1))/DF
      TERM1 = (1.D0/MSYLDZ)-1.D0
      TERM2 = (1.d0 - FMSY2*SUMSUR(ISPass))
      TERM3 = FMSY2*(1.d0+FEC(ISPass)*SUMSUR(ISPass)) - FEC(ISPass)

      MSY1 = 1.D0 + TERM - F/Z(ISPass) * DfDF*TERM1/(TERM2*TERM3)

      RETURN
      END

C     -----------------------------------------------------------------

      FUNCTION MSYE (PARS,IERR)

C     Subroutine used to find MSYR and MSYL for the exploitable population
C     using Eqn A7.3

      USE DECL_STKVRS

      DOUBLE PRECISION MSYE,PARS(4),U1PLUS(2),UMAT(2),FMSY2,PADJST,
     +       MSYLDZ,F,UF(2),U1,UM,TERM,DF,TEMP,TERM1,TERM2,TERM3,DfDF,
     +       UE,UEXP(2),UP,RP
      INTEGER IERR,L,I

C     The function is 0 when PARS(3) = MSYR for the exploitable population
C     R1PLUS=PARS(1),  MSYR(1+) = PARS(3),  MSYL(1+) = PARS(2)

      IERR = 0
C *** Estimate birth rate at levels of survival which bound MSYR
      F = PARS(3)
      DF  = 0.00001D0
      UF(1) = 1.D0 - F + DF*.5D0
      UF(2) = UF(1) - DF

      DO 100 I = 1,2

C       Add numbers of mature & 1+, relative to # of age 0
C       P = number of age A relative to # of age 0, starting with age 1
        RP = 0.D0
        UP = 1.D0
        UMAT(I) = 0.D0
        U1PLUS(I) = 0.D0
        UEXP(I) = 0.D0

C       Fishing pattern is uniform on 1+ population
        DO 10 L = 1,MAXAGE
          RP = RP * SUR(ISPass,L-1) * UF(I)+
     +                        UP*SUR(ISPass,L-1)*RECF(ISPass,L)
          UP = UP * SUR(ISPass,L-1) * (1.0d0 - RECF(ISPass,L))
          UMAT(I) = UMAT(I) + (UP+RP)*FMATUR(ISPass,L)
          U1PLUS(I) = U1PLUS(I) + (UP+RP)
          UEXP(I) = UEXP(I) + RP
   10   CONTINUE
C       Adjust for last age class for pooling
        PADJST = 1.D0/(1.D0 - SUR(ISPass,MAXAGE)*UF(I))
        UMAT(I) = UMAT(I) - RP*FMATUR(ISPass,MAXAGE) * (1.D0 - PADJST)
        U1PLUS(I) = U1PLUS(I) - RP*(1.D0 - PADJST)
        UEXP(I) = UEXP(I) - RP*(1.D0 - PADJST)
C
C       Save birth rate
        UF(I) = 2.D0/UMAT(I)
  100 CONTINUE

C     Set MSYLD**Z using {Eqn A7.1} with 2f=FMSY2 and b(eq)=FEC
      FMSY2 = (UF(1) + UF(2)) * 0.5D0
      TEMP  = FMSY2 / (FEC(ISPass)*(1.d0-FMSY2*SUMSUR(ISPass)))
      MSYLDZ = MAX (0.0001D0, 1.D0 - (TEMP-1.D0)/A1(ISPass))

C     Now set MSYL(1+) {JCRM 1:270 eqn 20} and solve eqn A7.3 to find MSYR1
      UM   = (UMAT(1) + UMAT(2)) * 0.5D0
      U1   = (U1PLUS(1) + U1PLUS(2)) * 0.5D0
      UE   = (UEXP(1) + UEXP(2)) * 0.5d0
      IF (OPTDD.EQ.0) THEN
C       Density dependence on 1+
        TERM = 0.D0
        TERM = ((UEXP(2)-UEXP(1))/UE - (U1PLUS(2)-U1PLUS(1))/U1) *F/DF
        PARS(2) = MSYLDZ**(1.D0/Z(ISPass)) * UE * PARS(1)/(U1*PARS(4))
      ELSE 
C       Density dependence on mature
        UM   = (UMAT(1) + UMAT(2)) * 0.5D0
        U1   = (U1PLUS(1) + U1PLUS(2)) * 0.5D0
        TERM = ((UEXP(2)-UEXP(1))/UE - (UMAT(2)-UMAT(1))/UM) *F / DF
        PARS(2) = MSYLDZ**(1.D0/Z(ISPass)) * UE 
     +   / (UM * F0(ISPASS) * PARS(4)) 
      ENDIF
      DfDF = (UF(2)-UF(1))/DF
      TERM1 = (1.D0/MSYLDZ)-1.D0
      TERM2 = (1.d0 - FMSY2*SUMSUR(ISPass))
      TERM3 = FMSY2*(1.d0+FEC(ISPass)*SUMSUR(ISPass)) - FEC(ISPass)

      MSYE = 1.D0 + TERM - F/Z(ISPass) * DfDF*TERM1/(TERM2*TERM3)

      RETURN
      END

C     -----------------------------------------------------------------
C     -----------------------------------------------------------------

      FUNCTION MSYM (PARS,IERR)

      USE DECL_STKVRS

      DOUBLE PRECISION MSYM,PARS(4),U1PLUS(2),UMAT(2),FMSY2,P,PADJST,
     +       MSYLDZ,F,UF(2),U1,UM,TERM,DF,TEMP,TERM1,TERM2,TERM3,DfDF
      INTEGER IERR,L,I

C     The root of this function is MSYR for the mature population
C     R1PLUS=PARS(1),  MSYR(mat) = PARS(3),  MSYL(mat) = PARS(2)

      IERR = 0
C *** Estimate birth rate at levels of survival which bound MSYR
      F = PARS(3)
      DF  = 0.00001D0
      UF(1) = 1.D0 - F + DF*.5D0
      UF(2) = UF(1) - DF

      DO 100 I = 1,2

C       Add numbers of mature & 1+, relative to # of age 0
C       P = number of age A relative to # of age 0, starting with age 1
        P = SUR(ISPass,0)
        UMAT(I) = P*FMATUR(ISPass,1)
        U1PLUS(I) = P

C       Fishing pattern is uniform on mature population 
        DO 20 L = 2,MAXAGE
          P = SUR(ISPass,L-1) * P * 
     +                        (1.D0 - (1.D0-UF(I))*FMATUR(ISPass,L-1))
          UMAT(I) = UMAT(I) + P*FMATUR(ISPass,L)
          U1PLUS(I) = U1PLUS(I) + P
   20   CONTINUE
C       Adjust for last age class for pooling
        PADJST = 1.D0/(1.D0 - SUR(ISPass,MAXAGE)*UF(I))
        UMAT(I) = UMAT(I) - P*FMATUR(ISPass,MAXAGE) * (1.D0 - PADJST)
        U1PLUS(I) = U1PLUS(I) - P*(1.D0 - PADJST)
C
C       Save birth rate
        UF(I) = 2.D0/UMAT(I)
  100 CONTINUE

      FMSY2 = (UF(1) + UF(2)) * 0.5D0
      Temp = FMSY2/(FEC(ISPass)*(1.d0-FMSY2*SUMSUR(ISPass)))
      MSYLDZ = MAX (0.0001d0, 1.d0 - (Temp-1.d0)/A1(ISPass))
      IF (OPTDD.EQ.0) THEN
C       Density dependence on 1+
        UM   = (UMAT(1) + UMAT(2)) * 0.5D0
        U1   = (U1PLUS(1) + U1PLUS(2)) * 0.5D0
        TERM = ((UMAT(2)-UMAT(1))/UM - (U1PLUS(2)-U1PLUS(1))/U1) *F / DF
        PARS(2) = MSYLDZ**(1.D0/Z(ISPass)) * UM * 
     +       F0(ISPASS) * PARS(1) / U1
      ELSE 
C       Density dependence on mature
        TERM = 0.D0
        PARS(2) = MSYLDZ**(1.D0/Z(ISPass))
      ENDIF
      DfDF = (UF(2)-UF(1))/DF
      TERM1 = (1.D0/MSYLDZ)-1.d0
      TERM2 = (1.d0 - FMSY2*SUMSUR(ISPass))
      TERM3 = FMSY2*(1.D0+FEC(ISPass)*SUMSUR(ISPass)) - FEC(ISPass)

      MSYM = 1.D0 + TERM - F/Z(ISPass) * DfDF*TERM1/(TERM2*TERM3)

      RETURN
      END

C     ------------------------------------------------------------------

      SUBROUTINE RPRIOR(DISTRI,PARS,IN)

C     Read in a prior distribution

      INTEGER IN,II
      CHARACTER*1 DISTRI
      DOUBLE PRECISION PARS(4)
C
C     Initial the output values
      DO 10 II = 1,4
       PARS(II) = 0.D0
 10   CONTINUE
      READ(IN,'(45X,A1)') DISTRI 
C
C     Select a distribution
      IF    (DISTRI.EQ.'U'.OR.DISTRI.EQ.'u'
     +  .OR. DISTRI.EQ.'L'.OR.DISTRI.EQ.'l'
     +  .OR. DISTRI.EQ.'D'.OR.DISTRI.EQ.'d') THEN
        READ(IN,'(45X,F10.0)') PARS(1)
        READ(IN,'(45X,F10.0)') PARS(2)

      ELSE IF (DISTRI.EQ.'N'.OR.DISTRI.EQ.'n'
     +  .OR.   DISTRI.EQ.'I'.OR.DISTRI.EQ.'i') THEN
        READ(IN,'(45X,F10.0)') PARS(1)
        READ(IN,'(45X,F10.0)') PARS(2)
        READ(IN,'(45X,F10.0)') PARS(3)
        READ(IN,'(45X,F10.0)') PARS(4)

      ELSE IF  (DISTRI.EQ.'G'.OR.DISTRI.EQ.'g') THEN
        READ(IN,'(45X,F10.0)') PARS(1)
        READ(IN,'(45X,F10.0)') PARS(2)
        READ(IN,'(45X,F10.0)') PARS(3)
      ELSE
        PRINT *,DISTRI,PARS
        STOP 'Error: Unknown distribution'
      ENDIF

      RETURN
      END
C
C **********************************************************************
C
      DOUBLE PRECISION FUNCTION PRIOR(DISTRI,PAR,ISEED)
C
C     Generate random numbers from priors
C
C     Global variables
      CHARACTER*1 DISTRI
      DOUBLE PRECISION PAR(4)
      INTEGER ISEED
C
C     Local variables 
      INTEGER ISEEDA,INEXT,INEXTP
      DOUBLE PRECISION MA(55),RAN1,XNORM,GAMMAZ
      EXTERNAL RAN1,XNORM,GAMMAZ
      SAVE INEXT,INEXTP,MA

      IF (DISTRI.EQ.'U'.OR.DISTRI.EQ.'u') THEN
C       Uniform between par(1) and (2)
        PRIOR = RAN1(ISEED,MA,INEXT,INEXTP)*(PAR(2)-PAR(1))+PAR(1)

      ELSE IF (DISTRI.EQ.'L'.OR.DISTRI.EQ.'l') THEN
        PRIOR = RAN1(ISEED,MA,INEXT,INEXTP)*
     +   (LOG(PAR(2))-LOG(PAR(1)))+LOG(PAR(1))
        PRIOR = EXP(PRIOR)

      ELSE IF (DISTRI.EQ.'D'.OR.DISTRI.EQ.'d') THEN
C       Uniform integer between par(1) & (2)
        PRIOR = RAN1(ISEED,MA,INEXT,INEXTP)*(PAR(2)-PAR(1)+1)+PAR(1)
        PRIOR = INT(PRIOR)

      ELSE IF (DISTRI.EQ.'N'.OR.DISTRI.EQ.'n') THEN
C       Normal, mean par(1), sd par(2), between par(3) & (4)
8988    PRIOR = XNORM(PAR(2),PAR(1),ISEED,MA,INEXT,INEXTP)
        IF (PRIOR.LE.PAR(3).OR.PRIOR.GE.PAR(4)) GOTO 8988

      ELSE IF (DISTRI.EQ.'I'.OR.DISTRI.EQ.'i') THEN
C       Normal, mean par(1), sd par(2), nearest integer between par(3) & (4)
8989    PRIOR = XNORM(PAR(2),PAR(1),ISEED,MA,INEXT,INEXTP)
        PRIOR = NINT(PRIOR)
        IF (PRIOR.LT.PAR(3).OR.PRIOR.GT.PAR(4)) GOTO 8989

      ELSE IF (DISTRI.EQ.'G'.OR.DISTRI.EQ.'g') THEN
        ISEEDA = -(RAN1(ISEED,MA,INEXT,INEXTP)*1000000.d0+1.d0)
        PRIOR = GAMMAZ(PAR(1),PAR(2),ISEEDA)+PAR(3)
      ENDIF

      RETURN
      END
C
C     ------------------------------------------------------------------

      SUBROUTINE SETO (V,SIG,A50,MAXAGE,AMIN,IS)

C     SETO calculates the proportion of each age class mature or
C     recruited, returning the ogive in the vector V
C  NB this routine gives the same results as BALEEN II if 
C     T95 = SIG * 2.94444  - T50  and  AMIN(recruited) = 0
C
      DOUBLE PRECISION V(2,0:40),SIG,A50,X
      INTEGER MAXAGE,AMIN,L

C     If SIG = 0 ogive is knife edge at age A50.
      IF (SIG.EQ.0.D0) THEN
        DO 10 L = 0,NINT(A50)-1
          V(IS,L) = 0.D0
   10   CONTINUE
        DO 15 L = NINT(A50),MAXAGE-2
          V(IS,L) = 1.D0
   15   CONTINUE

      ELSE

C       Loop over all ages
C       AMIN  = first age at which animals  are recruited or mature
        DO 20 L = 0, AMIN-1
          V(IS,L) = 0.D0
   20   CONTINUE

C       A50   = age at which 50% of animals are recruited or mature
C       SIG   = width of ogive
        DO 25 L = AMIN, MAXAGE-2
          X = (A50 - DBLE(L)) / SIG
          IF (X.LT.-50.D0) THEN
            V(IS,L) = 1.D0
          ELSE IF (X.GT.50.D0) THEN
           V(IS,L) = 0.D0
          ELSE
            V(IS,L) = 1.D0/(1.D0 + EXP(X))
          ENDIF
   25   CONTINUE
C
      ENDIF
C
C  Set the terminal age classes (forced to be fully recruited or mature)
      V(IS,MAXAGE-1) = 1.D0
      V(IS,MAXAGE) = 1.D0
      RETURN
      END

C     ------------------------------------------------------------------

      SUBROUTINE TRFORM (V,MAXAGE,IS)

C     Adjust an ogive to transition form, that is so that 
C     V(L) = the proportion of animals in a given class at age A-1 
C            which make the transition to a different class age A

      DOUBLE PRECISION V(2,0:40),RM,D
      INTEGER MAXAGE,L,IS

      RM   = V(IS,0)
      DO 10 L = 1,MAXAGE
        IF (RM .LT. 1.D0) THEN
          D = RM
          RM = V(IS,L)
          V(IS,L) = (RM - D)/(1.D0 - D)
        ELSE
          RM = V(IS,L)
          V(IS,L) = 1.D0
        ENDIF
   10 CONTINUE

      RETURN
      END

C ***********************************************************************

      SUBROUTINE SORT(X,M)

C     Use A QUICK-SORT TO SORT ALL THE DATA
C     (C)opyright A.E. Punt (1987)

      DOUBLE PRECISION X(10000),ST1(10000),MID
      INTEGER M,LEFT(10000),RIGHT(10000),STKLEN,LEFTS,RIGHTS,LS,RS,IC

C     Check for Daft call
      IF (M.LT.2) RETURN

C     Set up initial conditions
      LEFT(1) = 1
      RIGHT(1) = M
      STKLEN = 1

99    IF (STKLEN.EQ.0) GOTO 100

C     Set up the Pointers for this run
      MID = x(LEFT(STKLEN))
      LEFTS = LEFT(STKLEN)
      RIGHTS = RIGHT(STKLEN)
      LS = LEFT(STKLEN)
      RS = RIGHT(STKLEN)

C     Do a one-level sort
      DO 10 IC = LEFT(STKLEN)+1,RIGHT(STKLEN)

C      Check whether the current is less than the middle
       IF (X(IC).GT.MID) THEN
         ST1(RIGHTS) = X(IC)
         RIGHTS = RIGHTS - 1
       ELSE
         ST1(LEFTS) = X(IC)
         LEFTS = LEFTS + 1
       ENDIF
10    CONTINUE

C     Store the middle value
      ST1(LEFTS) = x(LEFT(STKLEN))

C     Replace the data
      DO 11 IC = LEFT(STKLEN),RIGHT(STKLEN)
       x(IC) = ST1(IC)
11    CONTINUE
      STKLEN = STKLEN - 1

C     update right pointer
      IF ((LEFTS-LS).GT.1) THEN
        STKLEN = STKLEN + 1
        LEFT(STKLEN) = LS
        RIGHT(STKLEN) = LEFTS - 1
      ENDIF

C     update left pointer
      IF ((RS-RIGHTS).GT.1) THEN
        STKLEN = STKLEN + 1
        LEFT(STKLEN) = RIGHTS + 1
        RIGHT(STKLEN) = RS
      ENDIF

      GOTO 99
100   CONTINUE

      RETURN
      END

C ------------------------------------------------------------------------------
C ------------------------------------------------------------------------------

      FUNCTION RAN1 (ISEED,MA,INEXT,INEXTP)

C     Copyright Numerical Recipes Software
C     Reference: Press, Flannery, Teukolsky & Vetterling: Numerical Recipes,
C     CUP, Cambridge 1986 (Page 199).
C
C     Function returns a uniform random deviate between 0.0 & 1.0.
C     Set ISEED to any negative value to reinitialize the sequence.
C     Note: ISEED MUST be -ve to initialise routine. It is reset to 1 on exit.
C
      DOUBLE PRECISION MA(55),MJ,MK,MBIG,MSEED,MZ,FAC,RAN1
      INTEGER ISEED,INEXT,INEXTP,I,K,II
!      INTEGER NN,IIYR
      PARAMETER (MBIG=4000000.D0,MSEED=1618033.D0,MZ=0.D0,FAC=1.D0/MBIG)

! DEBUGGING JRB     
!			if (NN==60) then 
!				print *, "NN", NN
!			end if
				
!	      print *, "HELLO FROM RAN1 INSIDE AWEXTRD.FOR"
!	      print *, "NN", NN
!  	    print *, "SIZE(MA)", SIZE(MA)  
!    	  print *, "SHAPE(MA)", SHAPE(MA)        
!      	print *, "ISEED", ISEED
!      	print *, "INEXT", INEXT
!      	print *, "INEXTP", INEXTP 
!      end if  
      
      IF (ISEED.LT.0.D0) THEN
! DEBUGGING JRB     
!				print *, "NEGATIVE ISEED FROM RAN1", ISEED
				     
        MJ = MSEED-IABS(ISEED)
        MJ = MOD(MJ,MBIG)        
        MA(55) = MJ
        MK = 1
        DO 11 I = 1,54
          II = MOD(21*I,55)
          MA(II) = MK
          MK = MJ-MK
          IF (MK.LT.MZ) MK = MK+MBIG
          MJ = MA(II)
   11   CONTINUE
        DO 12 K = 1,4
        	DO 12 I = 1,55
          	MA(I) = MA(I)-MA(1+MOD(I+30,55))
          	IF (MA(I).LT.MZ) MA(I) = MA(I)+MBIG
   12   CONTINUE
        INEXT = 0
        INEXTP = 31
        ISEED = 1
      ENDIF
      
      INEXT = INEXT+1
      IF (INEXT.EQ.56) INEXT = 1
      INEXTP = INEXTP+1
      IF (INEXTP.EQ.56) INEXTP = 1
      
! DEBUGGING JRB     
			if (INEXT.le.0.or.INEXT.gt.55) then
	      print *, "HELLO FROM AWEXTRD.FOR RAN1 for inext check"			
	      print *, "ISEED", ISEED
				print *, "INEXT", INEXT
				print *, "INEXTP", INEXTP	
				inext = 1			
				print *, "MA(INEXT)", MA(INEXT)
			end if
			if (INEXTP.le.0.or.INEXTP.gt.55) then 
	      print *, "HELLO FROM AWEXTRD.FOR RAN1 for inextp check"			
	      print *, "ISEED", ISEED	      
				print *, "INEXTP", INEXTP
				print *, "INEXT", INEXT	
				inextp = 32			
				print *, "MA(INEXTP)", MA(INEXTP)
			end if
			
      MJ = MA(INEXT)-MA(INEXTP)  ! This line is causing bug with MA() array bounds 
      
      IF (MJ.LT.MZ) MJ = MJ+MBIG
      MA(INEXT) = MJ
      RAN1 = MJ*FAC
      END


C ------------------------------------------------------------------------------
C ------------------------------------------------------------------------------

      FUNCTION XNORM (SIGG,MEAN,ISEED,MA,INEXT,INEXTP)

C     Uniform to Normal conversion routine

      DOUBLE PRECISION XNORM, Z1, Z2, MEAN, SIGG, RAN1, MA(55)
      INTEGER ISEED, INEXT,INEXTP
      EXTERNAL RAN1

      Z1 = RAN1(ISEED,MA,INEXT,INEXTP)
    1 Z2 = RAN1(ISEED,MA,INEXT,INEXTP)
      IF (Z2.LE.0.D0) GO TO 1

      XNORM = SIN(6.238319D0*Z1) * SQRT(-2.D0*LOG(Z2)) * SIGG+MEAN

      RETURN
      END

C ------------------------------------------------------------------------------
C ------------------------------------------------------------------------------

      FUNCTION POISSN (XM,RANNO)

      DOUBLE PRECISION F,SS,R,XM,RANNO,POISSN
      INTEGER I

      R = RANNO
      F = EXP(-XM)
      SS = F
      I = 0
   99 IF (SS.GE.R) GOTO 100
      I = I + 1
      F = F*XM/DBLE(I)
      SS = SS + F
      GOTO 99
  100 POISSN = DBLE(I)

      END

C --------------------------------------------------------------------------
C --------------------------------------------------------------------------
C
      FUNCTION GAMMAZ (PARX,PARS,ISEED, N, IYR)
C
C     Approximate random gamma generator: PARX = mean; PARS= sd
C
      EXTERNAL RAN1
      DOUBLE PRECISION PARX,PARS,X,S,RAN1,MA(55),P,F,FN,GAMMAZ
      INTEGER ISEED,INEXT,INEXTP,I
      INTEGER N, IYR  ! DEBUGGING JRB
C
      X = PARX
      S = PARS
C
C     Convert to gamma parameters
      s = s*s / x
C
      P = 0.D0
      IF (S.GT.0.D0) THEN
        X = X/S
        I = INT(X)
        FN = AINT(X)
        IF (I.GT.0) THEN
C
C         Count down from i to zero
100       IF (I.NE.0) THEN

! DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
C					IF(N.EQ.59.AND.IYR.GE.96)THEN
C						PRINT *, ""
C						PRINT *, "1. ABOUT TO CALL RAN1 FROM GAMMAZ"
C						PRINT *, "ISEED", ISEED
C						!PRINT *, "MA", MA
C						PRINT *, "INEXT", INEXT
C						PRINT *, "INEXTP", INEXTP
C					END IF 
      		
            F = RAN1(ISEED,MA,INEXT,INEXTP)

! DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
C					IF(N.EQ.59.AND.IYR.GE.96)THEN
C						PRINT *, "1. FINISHED CALL RAN1 FROM GAMMAZ"
C					END IF 
      		            
            IF (F.LE.0.D0) THEN
              WRITE(*,*) 'WARNING F=0 in GAMMAZ',F
              F = 0.00001D0
            ENDIF
            P = P + LOG(F)
            I = I - 1
            GOTO 100
          ENDIF
        ENDIF
C
C       Usually this is the case (unless x is an integer)
        IF (X.GT.FN) THEN
! DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
C	      IF(N.EQ.60.AND.IYR.GE.96)THEN
C	      	PRINT *, ""
C					PRINT *, "2. ABOUT TO CALL RAN1 FROM GAMMAZ"
C     		END IF 
      		
          F = RAN1(ISEED,MA,INEXT,INEXTP)
          
! DEBUGGING JRB !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
C	      IF(N.EQ.60.AND.IYR.GE.96)THEN
C					PRINT *, "2. FINISHED CALL RAN1 FROM GAMMAZ"
C     		END IF 
      		          
          IF (F.LE.0.D0) THEN
            WRITE(*,*) 'WARNING F=0 in GAMMAZ',F
            F = 0.00001D0
          ENDIF
          GAMMAZ = -S*( (X-FN)*LOG(F) + P )
        ELSE
          GAMMAZ = -S*P
        ENDIF

      ELSE
C
        GAMMAZ = X
C
      ENDIF
C
      RETURN
      END
C
C--------------------------------------------------------------------------
C
      FUNCTION GENBET(AA,BB,ISEED)

C                              LEGALITIES
C
C Code that appeared in an ACM publication is subject to their
C algorithms policy:
C
C     Submittal of an algorithm for publication in one of the ACM
C     Transactions implies that unrestricted use of the algorithm within a
C     computer is permissible . General permission to copy and distribute
C     the algorithm without fee is granted provided that the copies are not
C     made or distributed for direct commercial advantage.  The ACM
C     copyright notice and the title of the publication and its date appear,
C     and notice is given that copying is by permission of the Association
C     for Computing Machinery. To copy otherwise, or to republish, requires
C     a fee and/or specific permission.
C
C     Krogh, F. Algorithms Policy. ACM Tran. Math. Softw. 13(1987), 183-186.
C
C We place the Ranlib code that we have written in the public domain.  
C
C
C                                 NO WARRANTY
C     
C     WE PROVIDE ABSOLUTELY  NO WARRANTY  OF ANY  KIND  EITHER  EXPRESSED OR
C     IMPLIED,  INCLUDING BUT   NOT LIMITED TO,  THE  IMPLIED  WARRANTIES OF
C     MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK
C     AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS  WITH YOU.  SHOULD
C     THIS PROGRAM PROVE  DEFECTIVE, YOU ASSUME  THE COST  OF  ALL NECESSARY
C     SERVICING, REPAIR OR CORRECTION.
C     
C     IN NO  EVENT  SHALL THE UNIVERSITY  OF TEXAS OR  ANY  OF ITS COMPONENT
C     INSTITUTIONS INCLUDING M. D.   ANDERSON HOSPITAL BE LIABLE  TO YOU FOR
C     DAMAGES, INCLUDING ANY  LOST PROFITS, LOST MONIES,   OR OTHER SPECIAL,
C     INCIDENTAL   OR  CONSEQUENTIAL DAMAGES   ARISING   OUT  OF  THE USE OR
C     INABILITY TO USE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA OR
C     ITS ANALYSIS BEING  RENDERED INACCURATE OR  LOSSES SUSTAINED  BY THIRD
C     PARTIES) THE PROGRAM.
C     
C**********************************************************************
C
C     REAL FUNCTION GENBET( A, B )
C               GeNerate BETa random deviate
C
C                              Function
C
C     Returns a single random deviate from the beta distribution with
C     parameters A and B.  The density of the beta is
C               x^(a-1) * (1-x)^(b-1) / B(a,b) for 0 < x < 1
C
C                              Method
C     R. C. H. Cheng
C     Generating Beta Variatew with Nonintegral Shape Parameters
C     Communications of the ACM, 21:317-322  (1978)
C     (Algorithms BB and BC)
C
C**********************************************************************
C     .. Parameters ..
C     EXPMAX  Close to the largest number that can be exponentiated
C     INFNTY  Close to the largest representable single precision number
      DOUBLE PRECISION GENBET, EXPMAX, INFNTY
      PARAMETER (EXPMAX=89.0D0, INFNTY=1.0E38)
C     ..
C     .. Scalar Arguments ..
      DOUBLE PRECISION AA,BB
      INTEGER ISEED
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION A,ALPHA,B,BETA,DELTA,GAMMA,K1,K2,OLDA,OLDB,R,S,T,
     +       U1,U2,V,W,Y,Z
      LOGICAL QSAME
C     ..
C     .. External & Intrinsic Functions ..
      DOUBLE PRECISION RAN1, MA(55)
      EXTERNAL RAN1
      INTEGER INEXT,INEXTP
      INTRINSIC EXP,LOG,MAX,MIN,SQRT
C     ..
C     .. Save statement ..
      SAVE OLDA,OLDB,ALPHA,BETA,GAMMA,K1,K2
C     ..
C     .. Data statements ..
      DATA OLDA,OLDB/-1,-1/
C     ..
C     .. Executable Statements ..
      QSAME = (OLDA.EQ.AA) .AND. (OLDB.EQ.BB)
      IF (QSAME) GO TO 20
      IF (.NOT. (AA.LE.0.0.OR.BB.LE.0.0)) GO TO 10
      WRITE (*,*) ' AA OR BB <= 0 IN GENBET - ABORT!'
      WRITE (*,*) ' AA: ',AA,' BB ',BB
      STOP ' AA OR BB <= 0 IN GENBET - ABORT!'

   10 OLDA = AA
      OLDB = BB
   20 IF (.NOT. (MIN(AA,BB).GT.1.0)) GO TO 100


C     Alborithm BB

C     Initialize
C
      IF (QSAME) GO TO 30
      A = MIN(AA,BB)
      B = MAX(AA,BB)
      ALPHA = A + B
      BETA = SQRT((ALPHA-2.0)/ (2.0*A*B-ALPHA))
      GAMMA = A + 1.0/BETA
   30 CONTINUE
   40 U1 = RAN1(ISEED,MA,INEXT,INEXTP)
C
C     Step 1
C
      U2 = RAN1(ISEED,MA,INEXT,INEXTP)
      V = BETA*LOG(U1/ (1.0-U1))
      IF (.NOT. (V.GT.EXPMAX)) GO TO 50
      W = INFNTY
      GO TO 60

   50 W = A*EXP(V)
   60 Z = U1**2*U2
      R = GAMMA*V - 1.3862944
      S = A + R - W
C
C     Step 2
C
      IF ((S+2.609438).GE. (5.0*Z)) GO TO 70
C
C     Step 3
C
      T = LOG(Z)
      IF (S.GT.T) GO TO 70
C
C     Step 4
C
      IF ((R+ALPHA*LOG(ALPHA/ (B+W))).LT.T) GO TO 40
C
C     Step 5
C
   70 IF (.NOT. (AA.EQ.A)) GO TO 80
      GENBET = W/ (B+W)
      GO TO 90

   80 GENBET = B/ (B+W)
   90 GO TO 230


C     Algorithm BC
C
C     Initialize
C
  100 IF (QSAME) GO TO 110
      A = MAX(AA,BB)
      B = MIN(AA,BB)
      ALPHA = A + B
      BETA = 1.0/B
      DELTA = 1.0 + A - B
      K1 = DELTA* (0.0138889+0.0416667*B)/ (A*BETA-0.777778)
      K2 = 0.25 + (0.5+0.25/DELTA)*B
  110 CONTINUE
  120 U1 = RAN1(ISEED,MA,INEXT,INEXTP)
C
C     Step 1
C
      U2 = RAN1(ISEED,MA,INEXT,INEXTP)
      IF (U1.GE.0.5) GO TO 130
C
C     Step 2
C
      Y = U1*U2
      Z = U1*Y
      IF ((0.25*U2+Z-Y).GE.K1) GO TO 120
      GO TO 170
C
C     Step 3
C
  130 Z = U1**2*U2
      IF (.NOT. (Z.LE.0.25)) GO TO 160
      V = BETA*LOG(U1/ (1.0-U1))
      IF (.NOT. (V.GT.EXPMAX)) GO TO 140
      W = INFNTY
      GO TO 150

  140 W = A*EXP(V)
  150 GO TO 200

  160 IF (Z.GE.K2) GO TO 120
C
C     Step 4
C
C
C     Step 5
C
  170 V = BETA*LOG(U1/ (1.0-U1))
      IF (.NOT. (V.GT.EXPMAX)) GO TO 180
      W = INFNTY
      GO TO 190

  180 W = A*EXP(V)
  190 IF ((ALPHA* (LOG(ALPHA/ (B+W))+V)-1.3862944).LT.LOG(Z)) GO TO 120
C
C     Step 6
C
  200 IF (.NOT. (A.EQ.AA)) GO TO 210
      GENBET = W/ (B+W)
      GO TO 220

  210 GENBET = B/ (B+W)
  220 CONTINUE
  230 RETURN

      END
      
C -----------------------------------------------------------------------------

      SUBROUTINE ZBRAC(FUNC,PARS,M,X1,X2,F1,F2,SUCCES)
C
C  This subroutine is based on the Numerical Recipes Software Library
C  (Refer, W.H.Press, B.P.Flannery, S.A.Teukolsky & W.T.Vetterling 1986.  
C  Numerical Recipes: The Art of Scientific Computing.  CUP, Cambridge. 818pp.
C
C  Given a function FUNC and an initial guessed range X1 to X2, the
C  routine expands the range geometrically until a root is bracketted
C  by the return values X1 and X2 (in which case SUCCES returns as
C  .TRUE.) or until the range becomes unacceptably large (in which
C  case SUCCES returns as .FALSE.).  The parameters to FUNC are passed
C  through the vector PARS.  X1 and X2 are passed to FUNC through
C  PARS(M).  The corresponding function values F1 and F2 are returned.
C  Success is guaranteed for a function which has opposite sign for
C  sufficiently large and small arguments.
C
      EXTERNAL FUNC
      DOUBLE PRECISION FUNC,PARS(4),FACTOR,X1,X2,F1,F2
      INTEGER NTRY,M,IERR,J
      LOGICAL SUCCES
      PARAMETER (FACTOR=1.6D0,NTRY=50)
C
      IF(X1.EQ.X2)PAUSE 'You have to guess an initial range'
      PARS(M) = X1
      F1 = FUNC(PARS,IERR)
      IF (IERR .NE. 0) THEN
        SUCCES = .FALSE.
        RETURN
      ENDIF
      PARS(M) = X2
      F2 = FUNC(PARS,IERR)
      IF (IERR .NE. 0) THEN
        SUCCES = .FALSE.
        RETURN
      ENDIF
      SUCCES=.TRUE.
      DO 11 J=1,NTRY
        IF(F1*F2.LT.0.D0)RETURN
        IF(ABS(F1).LT.ABS(F2))THEN
          X1=X1+FACTOR*(X1-X2)
          PARS(M) = X1
          F1 = FUNC(PARS,IERR)
          IF (IERR .NE. 0) THEN
            SUCCES = .FALSE.
            RETURN
          ENDIF
        ELSE
          X2=X2+FACTOR*(X2-X1)
          PARS(M) = X2
          F2 = FUNC(PARS,IERR)
          IF (IERR .NE. 0) THEN
            SUCCES = .FALSE.
            RETURN
          ENDIF
        ENDIF
11    CONTINUE
      SUCCES=.FALSE.
      RETURN
      END
C
C  ***********************************************************************
C
      SUBROUTINE ZBRENT(FUNC,PARS,M,X1,X2,FX1,FX2,TOL,IERR)

C  This subroutine is based on the Numerical Recipes Software Library
C  (Refer, W.H.Press, B.P.Flannery, S.A.Teukolsky & W.T.Vetterling 1986.  
C  Numerical Recipes: The Art of Scientific Computing.  CUP, Cambridge. 818pp.
C
C  Using Brent's method, find the root of a function FUNC known to
C  lie between X1 and X2 (with function values FX1 and FX2).  The
C  parameters to FUNC are passed through the vector PARS.  The root
C  is returned through PARS(M)
C
      EXTERNAL FUNC
      DOUBLE PRECISION FUNC,PARS(4),X1,X2,FX1,FX2,TOL,EPS,A,FA,B,FB,C,
     +       D,E,TOL1,XM,S,P,Q,R,FC
      INTEGER M,ITMAX,ITER,IERR
      PARAMETER (ITMAX=100,EPS=3.E-5)
C
      A=X1
      FA = FX1
      B=X2
      FB = FX2
C
      IF(FB*FA.GT.0.D0) PAUSE 'Root must be bracketed for ZBRENT.'
      FC=FB
      DO 11 ITER=1,ITMAX
C
        IF(FB*FC.GT.0.D0) THEN
C  Rename A, B, C and adjust bounding interval D
          C=A
          FC=FA
          D=B-A
          E=D
        ENDIF
C
C  Check for convergence
        IF(ABS(FC).LT.ABS(FB)) THEN
          A=B
          B=C
          C=A
          FA=FB
          FB=FC
          FC=FA
        ENDIF
        TOL1=2.D0*EPS*ABS(B)+0.5D0*TOL
        XM=.5D0*(C-B)
        IF(ABS(XM).LE.TOL1 .OR. FB.EQ.0.D0)THEN
          PARS(M)=B
          RETURN
        ENDIF
        IF(ABS(E).GE.TOL1 .AND. ABS(FA).GT.ABS(FB)) THEN
C  Attempt inverse quadratic interpolation
          S=FB/FA
          IF(A.EQ.C) THEN
            P=2.D0*XM*S
            Q=1.D0-S
          ELSE
            Q=FA/FC
            R=FB/FC
            P=S*(2.D0*XM*Q*(Q-R)-(B-A)*(R-1.D0))
            Q=(Q-1.D0)*(R-1.D0)*(S-1.D0)
          ENDIF
C
C  Check whether in bounds
          IF(P.GT.0.D0) Q=-Q
          P=ABS(P)
          IF(2.D0*P .LT. MIN(3.D0*XM*Q-ABS(TOL1*Q),ABS(E*Q))) THEN
C  Accept interpolation
            E=D
            D=P/Q
          ELSE
C  Interpolation failed, use bisection
            D=XM
            E=D
          ENDIF
        ELSE
C  Bounds decreasing too slowly, use bisection
          D=XM
          E=D
        ENDIF
C  Save latest best guess
        A=B
        FA=FB
        IF(ABS(D) .GT. TOL1) THEN
          B=B+D
        ELSE
          B=B+SIGN(TOL1,XM)
        ENDIF
C  Evaluate new trial root
        PARS(M) = B
        FB = FUNC(PARS,IERR)
        IF (IERR .NE. 0) RETURN
11    CONTINUE
C
      PAUSE 'ZBRENT exceeding maximum iterations.'
      PARS(M)=B
      RETURN
      END

C -----------------------------------------------------------------------------

      SUBROUTINE ZBRAC2(FUNC,PARS,M,X1,X2,F1,F2,SUCCES)
C
C  This subroutine is based on the Numerical Recipes Software Library
C  (Refer, W.H.Press, B.P.Flannery, S.A.Teukolsky & W.T.Vetterling 1986.  
C  Numerical Recipes: The Art of Scientific Computing.  CUP, Cambridge. 818pp.
C
C  Given a function FUNC and an initial guessed range X1 to X2, the
C  routine expands the range geometrically until a root is bracketted
C  by the return values X1 and X2 (in which case SUCCES returns as
C  .TRUE.) or until the range becomes unacceptably large (in which
C  case SUCCES returns as .FALSE.).  The parameters to FUNC are passed
C  through the vector PARS.  X1 and X2 are passed to FUNC through
C  PARS(M).  The corresponding function values F1 and F2 are returned.
C  Success is guaranteed for a function which has opposite sign for
C  sufficiently large and small arguments.
C
      EXTERNAL FUNC
      DOUBLE PRECISION FUNC,PARS(4),FACTOR,X1,X2,F1,F2
      INTEGER NTRY,M,IERR,J
      LOGICAL SUCCES
      PARAMETER (FACTOR=1.6D0,NTRY=50)
C
      IF(X1.EQ.X2)PAUSE 'You have to guess an initial range'
      PARS(M) = X1
      F1 = FUNC(PARS,IERR)
      IF (IERR .NE. 0) THEN
        SUCCES = .FALSE.
        RETURN
      ENDIF
      PARS(M) = X2
      F2 = FUNC(PARS,IERR)
      IF (IERR .NE. 0) THEN
        SUCCES = .FALSE.
        RETURN
      ENDIF
      SUCCES=.TRUE.
      DO 11 J=1,NTRY
        IF(F1*F2.LT.0.D0)RETURN
        IF(ABS(F1).LT.ABS(F2))THEN
          X1=X1+FACTOR*(X1-X2)
          PARS(M) = X1
          F1 = FUNC(PARS,IERR)
          IF (IERR .NE. 0) THEN
            SUCCES = .FALSE.
            RETURN
          ENDIF
        ELSE
          X2=X2+FACTOR*(X2-X1)
          PARS(M) = X2
          F2 = FUNC(PARS,IERR)
          IF (IERR .NE. 0) THEN
            SUCCES = .FALSE.
            RETURN
          ENDIF
        ENDIF
11    CONTINUE
      SUCCES=.FALSE.
      RETURN
      END
C
C  ***********************************************************************
C
      SUBROUTINE ZBRNT2(FUNC,PARS,M,X1,X2,FX1,FX2,TOL,IERR)

C  This subroutine is based on the Numerical Recipes Software Library
C  (Refer, W.H.Press, B.P.Flannery, S.A.Teukolsky & W.T.Vetterling 1986.  
C  Numerical Recipes: The Art of Scientific Computing.  CUP, Cambridge. 818pp.
C
C  Using Brent's method, find the root of a function FUNC known to
C  lie between X1 and X2 (with function values FX1 and FX2).  The
C  parameters to FUNC are passed through the vector PARS.  The root
C  is returned through PARS(M)
C
      EXTERNAL FUNC
      DOUBLE PRECISION FUNC,PARS(4),X1,X2,FX1,FX2,TOL,EPS,A,FA,B,FB,C,
     +       D,E,TOL1,XM,S,P,Q,R,FC
      INTEGER M,ITMAX,ITER,IERR
      PARAMETER (ITMAX=200,EPS=3.E-10)
C
      A=X1
      FA = FX1
      B=X2
      FB = FX2
C
      IF(FB*FA.GT.0.D0) PAUSE 'Root must be bracketed for ZBRNT2'
      FC=FB
      DO 11 ITER=1,ITMAX
C
        IF(FB*FC.GT.0.D0) THEN
C  Rename A, B, C and adjust bounding interval D
          C=A
          FC=FA
          D=B-A
          E=D
        ENDIF
C
C  Check for convergence
        IF(ABS(FC).LT.ABS(FB)) THEN
          A=B
          B=C
          C=A
          FA=FB
          FB=FC
          FC=FA
        ENDIF
        TOL1=2.D0*EPS*ABS(B)+0.5D0*TOL
        XM=.5D0*(C-B)
        IF(ABS(XM).LE.TOL1 .OR. FB.EQ.0.D0)THEN
          PARS(M)=B
          RETURN
        ENDIF
        IF(ABS(E).GE.TOL1 .AND. ABS(FA).GT.ABS(FB)) THEN
C  Attempt inverse quadratic interpolation
          S=FB/FA
          IF(A.EQ.C) THEN
            P=2.D0*XM*S
            Q=1.D0-S
          ELSE
            Q=FA/FC
            R=FB/FC
            P=S*(2.D0*XM*Q*(Q-R)-(B-A)*(R-1.D0))
            Q=(Q-1.D0)*(R-1.D0)*(S-1.D0)
          ENDIF
C
C  Check whether in bounds
          IF(P.GT.0.D0) Q=-Q
          P=ABS(P)
          IF(2.D0*P .LT. MIN(3.D0*XM*Q-ABS(TOL1*Q),ABS(E*Q))) THEN
C  Accept interpolation
            E=D
            D=P/Q
          ELSE
C  Interpolation failed, use bisection
            D=XM
            E=D
          ENDIF
        ELSE
C  Bounds decreasing too slowly, use bisection
          D=XM
          E=D
        ENDIF
C  Save latest best guess
        A=B
        FA=FB
        IF(ABS(D) .GT. TOL1) THEN
          B=B+D
        ELSE
          B=B+SIGN(TOL1,XM)
        ENDIF
C  Evaluate new trial root
        PARS(M) = B
        FB = FUNC(PARS,IERR)
        IF (IERR .NE. 0) RETURN
11    CONTINUE
C
      PAUSE 'ZBRNT2 exceeding maximum iterations.'
      PARS(M)=B
      RETURN
      END

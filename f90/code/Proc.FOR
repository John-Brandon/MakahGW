      IMPLICIT NONE

      REAL*8 Total1P(100,2,-82:100)
      REAL*8 Depl1P(100,2,-82:100)
      REAL*8 T1,RESU(4,3),ERROR(1000)
      INTEGER II,IYR1,IYR2,ISIM,IYR,I5,I50,I95,I,J
      
      OPEN(UNIT=13,FILE="AW-RES")
      DO 1000 II = 1,59
       READ(13,*)
1000  CONTINUE
      DO 10000 ISIM = 1,100
       READ(13,*)
       READ(13,*)
       READ(13,*)
       READ(13,*)
       READ(13,*)
       DO 11000 IYR = -82,100
        READ(13,*)  IYR1,IYR2,T1,Total1P(ISIM,1,IYR),T1,T1,
     +   Total1P(ISIM,2,IYR)
        Depl1P(ISIM,1,IYR) = Total1P(ISIM,1,IYR)/Total1P(ISIM,1,-82)*100
        Depl1P(ISIM,2,IYR) = Total1P(ISIM,2,IYR)/Total1P(ISIM,2,-82)*100
11000  CONTINUE        
10000 CONTINUE      
      CLOSE(13)
      
      I5 = 5
      I95 = 96
      I50 = 50
      DO 20000 IYR = -81,100
       DO 21000 ISIM = 1,100
        ERROR(ISIM) = Total1P(ISIM,1,IYR)
21000  CONTINUE
       CALL SORT(ERROR,100)
       RESU(1,1) = ERROR(I5)
       RESU(1,2) = (ERROR(I50)+ERROR(I50+1))/2.0
       RESU(1,3) = ERROR(I95)
       DO 22000 ISIM = 1,100
        ERROR(ISIM) = Total1P(ISIM,2,IYR)
22000  CONTINUE
       CALL SORT(ERROR,100)
       RESU(2,1) = ERROR(I5)
       RESU(2,2) = (ERROR(I50)+ERROR(I50+1))/2.0
       RESU(2,3) = ERROR(I95)
       DO 23000 ISIM = 1,100
        ERROR(ISIM) = Depl1P(ISIM,1,IYR)
23000  CONTINUE
       CALL SORT(ERROR,100)
       RESU(3,1) = ERROR(I5)
       RESU(3,2) = (ERROR(I50)+ERROR(I50+1))/2.0
       RESU(3,3) = ERROR(I95)
       DO 24000 ISIM = 1,100
        ERROR(ISIM) = Depl1P(ISIM,2,IYR)
24000  CONTINUE
       CALL SORT(ERROR,100)
       RESU(4,1) = ERROR(I5)
       RESU(4,2) = (ERROR(I50)+ERROR(I50+1))/2.0
       RESU(4,3) = ERROR(I95)
       WRITE(*,600) IYR,((RESU(I,J),J=1,3),I=1,4)
20000 CONTINUE       
C      
      STOP
600   FORMAT(1x,I4,1x,12(F10.1))      
      END
C
C -------------------------------------------------------------------------
C
      SUBROUTINE SORT(X,M)

C     USE A QUICK-SORT TO SORT ALL THE DATA

      IMPLICIT NONE
      REAL*8 X(21000),ST1(21000),MID
      INTEGER*4 M,LEFT(21000),RIGHT(21000),STKLEN
      INTEGER*4 LEFTS,RIGHTS,LS,RS,IC

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
      

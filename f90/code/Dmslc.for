C     Last change:  CA   27 Jun 2004    2:07 pm
C The following code is an outline for developers to modify.
C The only data available is that passed into these routines
C
C ***********************************************************************
C ***********************************************************************
C
C    SLC                                        14 June 2004
C
C ***********************************************************************
C
      SUBROUTINE DMISLC(SIGHTI,CVXI,CATFI,CATMI)
C
C     Initialise the SLC (subsistence limit calculation) if required
C
C     Gamma  tuning parameter
C     MSYR   Six values of MSYR
C     z      Degree of compensation
C     S      Annual survival rate
C     K      Carrying capacity
C     V      Catch for each year
C     Q      The Kalman filter variance
C     A      Resilence parameter
C     Y      Year for which abundance estimates exist
C     ABEST  Abundance estimates
C     CVHIST Estimated CV for historical abundance data

C     Additional definitions for COMMON/STOCK:
C     NHIST  Stock size (for years -41 to -1, 917 filters)
C     P0HIST Estimated variance (historical)
C     DF     Differential of P-T logfunction (Calculated in subroutine diff)
C     PRIOR  Prior distribution p(alpha(i))
C     POST   Posterior distribution p(alpha(i)|Zt)
C     CONDHIST   Conditional distribution p(Zt|alpha(i)) (historical)


C     Definitions for COMMON/ MODEL/
      DOUBLE PRECISION :: gamma,S,z
      DOUBLE PRECISION,DIMENSION(6) :: MSYR
      DOUBLE PRECISION,DIMENSION(6) :: A
      DOUBLE PRECISION,DIMENSION(174) :: MSY
      DOUBLE PRECISION :: Q,QHIST,cvadd,alfa,temp

C     Definitions forCOMMON/ INITIALIZE/
      DOUBLE PRECISION,DIMENSION(-79:-6,1044) :: NHIST
      DOUBLE PRECISION,DIMENSION(1044) :: P0HIST,CONDHIST
C
C     Data passed through
      DOUBLE PRECISION CATMI(3,-410:99),CATFI(3,-410:99),
     +       SIGHTI(2,-43:99),CVXI(2,-43:99)
C
C     Definitions for historical data variables
      DOUBLE PRECISION ABEST(-43:99),CVHIST(-43:99)
      INTEGER Y(-43:99),YCATCH(-410:99) 
      DOUBLE PRECISION CATCHF(-410:99),CATCHM(-410:99)

      DOUBLE PRECISION,DIMENSION(1044) :: POST

C     Definitions for the Kalman filter
      DOUBLE PRECISION :: CVXX,POW,X0,DF,KAL,X,VARSUM
      DOUBLE PRECISION,DIMENSION(1044) :: BIAS
      DOUBLE PRECISION :: pi=3.1415926535897932D0

C     local variables (counters)
      INTEGER :: i,j,l,vis

C     dimensions of vectors K and A for do-loops
      INTEGER :: UA,UK

      COMMON /INITIALIZE/ NHIST,P0HIST,CONDHIST
!
      COMMON /MODEL/ gamma,MSYR,MSY,z,S,A,Q,QHIST,cvadd
C
C     Extract the historical data
      DO 10110 IY = -410,99
       CATCHM(IY) = CATMI(1,IY)+CATMI(2,IY)+CATMI(3,IY)
       CATCHF(IY) = CATFI(1,IY)+CATFI(2,IY)+CATFI(3,IY)
       YCATCH(IY) = IY+2009
10110 CONTINUE
      DO 10120 IY = -43,99
       ABEST(IY) = SIGHTI(1,IY)
       CVHIST(IY) = CVXI(1,IY)
       Y(IY) = IY+2009
10120 CONTINUE       
C
     
      OPEN(UNIT=666,FILE='GUP2/tuning.txt',STATUS='unknown')
      READ(666,*) Q
      READ(666,*) QHIST
      READ(666,*) CVADD
      READ(666,*) ALFA
      READ(666,*) GAMMA
      READ(666,*) temp
      
! Debugging JRB      
!      print *, "Q", Q      
!      print *, "QHIST", QHIST      
!      print *, "CVADD", CVADD      
!      print *, "ALFA", ALFA      
!      print *, "GAMMA", GAMMA                              
!      print *, "temp", temp      
!      
      CLOSE(666)

C     Set values of P-T parameters and tuning parameter gamma.
      S = 0.97D0
      z = 2.39D0

C     Find dimensions of vectors MSY/0.6 and A for do-loops
      UA=UBOUND(A,dim=1)
      UK=UBOUND(MSY,dim=1)

C     Generate values of MSYR from 1% to 6%
      MSYR(1) = 0.01D0
      DO i=2,UA
        MSYR(i)=MSYR(i-1)+0.01D0
      END DO

C     Generate values of MSY/0.6 from 100 to 2176 in increments of 12.
C     Calculate A for values of MSYR.
      MSY=(/(i,i=100,2176,12)/)
      DO i=1,UA
        A(i)=MSYR(i)*S/(1-S)*(z+1)/z
      END DO

      BIAS=1.00D0

C     Initialize stock size at year 1968 (NOTE -79 is 1930)
      DO j=1,UA
        DO l=1,UK
            vis=l+(j-1)*UK
            NHIST(-79,vis)=alfa*MSY(l)/MSYR(j)
        END DO
      END DO

      P0HIST=temp

      CONDHIST=(1.D0)/(UA*UK)

C     Calculate stock size for all 1057 filters from year 1968 to 1997
C     1968 is the year of the first abundance estimate	
      DO i=-78,-6
        DO j=1,UA
          DO l=1,UK
            vis=l+(j-1)*UK

C           Check for negative stock size
            IF (NHIST(i-1,vis)<0.D0) THEN
              NHIST(i-1,vis)=0.D0
            END IF

            NHIST(i,vis)=(1.D0+A(j)*(1.D0-(NHIST(i-1,vis)/ 
     &                  MSY(l)*MSYR(j))**z))*NHIST(i-1,vis)*(1.D0-S)
            NHIST(i,vis)=NHIST(i,vis)+S*(NHIST(i-1,vis)- 
     &                   CATCHF(i-1)-CATCHM(i-1))
!
            IF (NHIST(i-1,vis)>1.D0) THEN
              CALL diff(NHIST(i-1,vis),MSY(l)/MSYR(j),A(j),z,S,DF,
     &                  DBLE(CATCHF(i-1)+CATCHM(i-1)))
            ELSE
              DF=0.D0
            END IF
            P0HIST(vis)=DF*P0HIST(vis)*DF+QHIST
          END DO
        END DO

C       Update probability distribution and stock estimates when abundance
C       estimates are obtained.
				!IF (i.GE.-43.AND.ABEST(i).NE.-1) THEN
        IF (i.GE.-43) THEN		! Changed line about to two subsequent conditionals here JRB
        	IF (ABEST(i).NE.-1) THEN  ! Was getting error with gfortran compiler re: ABEST(i) out of bounds

!     	    Kalman Gain and conditional distribution calculations
    	      CVXX=LOG(1 + CVHIST(i)**2 + cvadd**2)
!
						DO j=1,UA
							DO l=1,UK
								vis=l+(j-1)*UK
								IF (NHIST(i,vis) > 1.D0) THEN
									X0=LOG(NHIST(i,vis))
								ELSE
									X0=0.D0
								END IF

								VARSUM=P0HIST(vis)+CVXX
								KAL=P0HIST(vis)/VARSUM
								X=X0+KAL*(LOG(ABEST(i))-LOG(BIAS(vis))-X0)
								P0HIST(vis)=P0HIST(vis)-KAL*P0HIST(vis)
								NHIST(i,vis)=EXP(X)
								POW=-0.5D0*((LOG(ABEST(i))-LOG(BIAS(vis))- 
     &                   X0)**2)/VARSUM
								if (POW < -300.D0) THEN
									 CONDHIST(vis)=0.D0
								ELSE
									 CONDHIST(vis)=CONDHIST(vis)/(SQRT(2.0*pi*VARSUM)) 
     &                   *EXP(POW)
								END IF
								
							end do
						END DO

					END IF
				END if
					
				POST=CONDHIST/sum(CONDHIST)
			END DO


      RETURN
      END

C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE DMRSLC (INITYR,ISURV,NSIM)
C
C     Reset the SLC at the start of each new replicate, if required
C
C     All years are measured on a scale such that management begins in year 0
C     INITYR Year of first premanagement catch (-165 to -1)
C     ISURV  Year of first sightings survey (-27 to -1). 
C     NSIM   Simulation number (i.e. replicate number)

      INTEGER INITYR,ISURV,NSIM

C     Definitions for COMMON/ INITIALIZE/
      DOUBLE PRECISION,DIMENSION(-79:-6,1044) :: NHIST
      DOUBLE PRECISION,DIMENSION(1044) :: P0HIST,CONDHIST

C     Definitions for COMMON/ STOCK/
      DOUBLE PRECISION,DIMENSION(-6:99,1044) :: N
      DOUBLE PRECISION,DIMENSION(1044) :: P0,COND

      COMMON /INITIALIZE/ NHIST,P0HIST,CONDHIST
      COMMON /STOCK/ N,P0,COND

C     Initialize stock size at year 1997
C     Initialize conditional distribution at year 1997
C     Initialize variance PO at year 1997
C
C     Dummy variables
      ISURV = ISURV
      INITYR = INITYR
      NSIM = NSIM
C      
      N(-6,:)=NHIST(-6,:)
      COND=CONDHIST
      P0=P0HIST

      RETURN
      END


C ----------------------------------------------------------------------
C ----------------------------------------------------------------------

      SUBROUTINE DMSLCG (CATM, CATF, SIGHT, CVX, IYR, INITYR, ISURV,
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

      DOUBLE PRECISION CATM(-410:99),CATF(-410:99),SIGHT(-43:99),
     +       CVX(-43:99), NEED,CATCHQ
      INTEGER IYR,INITYR,ISURV,NSIM, IQUOTA

C     Counters
      INTEGER :: r,j,vis,l

C     dimensions of vectors K and A for do-loops
      INTEGER :: UA,UK

C     Definitions for COMMON/ MODEL/
      DOUBLE PRECISION :: gamma,S,z
      DOUBLE PRECISION,DIMENSION(6) :: MSYR,A
      DOUBLE PRECISION,DIMENSION(174) :: MSY
      DOUBLE PRECISION :: Q,SNAP,QHIST,cvadd

C     Definitions for COMMON/ STOCK/
      DOUBLE PRECISION,DIMENSION(-6:99,1044) :: N
      DOUBLE PRECISION,DIMENSION(1044) :: P0,COND

C     Definitions for the Kalman filter
      DOUBLE PRECISION :: CATCHSUM,CVXX,RECRUIT,POW,X0,KAL,X,VARSUM,DF
      DOUBLE PRECISION,DIMENSION(1044) :: POST
      DOUBLE PRECISION,DIMENSION(1044) :: BIAS
      DOUBLE PRECISION :: V_temp,V_tempsum
      DOUBLE PRECISION :: pi=3.1415926535897932D0
!
      COMMON /MODEL/ gamma,MSYR,MSY,z,S,A,Q,QHIST,cvadd
      COMMON /STOCK/ N,P0,COND

      BIAS=1.00D0

C
C     Dummy variab;es
      INITYR = INITYR
      ISURV = ISURV

      CATCHSUM=0.D0
      if (SUM(cond)==0) then
         WRITE(*,*) 'sum(cond)=0'
      else
         POST=COND/sum(COND)
      end if

C     Find dimensions of vectors MSY and A for do-loops
      UA=UBOUND(A,dim=1)
      UK=UBOUND(MSY,dim=1)

      IF (IQUOTA/=5) THEN
         WRITE(*,*) 'error, iquota not equal to 5 years'
         stop
      END IF

      DO r=IYR-IQUOTA,IYR-1
        DO j=1,UA
          do l=1,UK

            vis=l+(j-1)*UK
C           Check for negative stock size
            IF (N(r-1,vis)<0.D0) THEN
              N(r-1,vis)=0.D0
            END IF

            RECRUIT=(1.D0+A(j)*(1.D0-(N(r-1,vis)/MSY(l)*MSYR(j))**z))*
     &            N(r-1,vis)
            N(r,vis)=S*(N(r-1,vis)-(CATM(r-1)+CATF(r-1)))+
     &            (1.D0-S)*RECRUIT
            IF (N(r-1,vis)>1.D0) THEN
              CALL diff(N(r-1,vis),MSY(l)/MSYR(j),A(j),z,S,DF,
     &                   CATM(r-1)+CATF(r-1))
            ELSE
              DF=0.D0
            END IF
            if (r<0) then
              P0(vis)=DF*P0(vis)*DF+QHIST
            else
              P0(vis)=DF*P0(vis)*DF+Q
            end if
          end do
        END DO

C       Update probability distribution and stock estimates when abundance
C       estimates are obtained.

        IF (SIGHT(r)/=-1.D0) THEN

C         Kalman Gain and conditional distribution calculations

           CVXX=LOG(1+CVX(r)**2+cvadd**2)


          DO j=1,UA
            do l=1,UK

              vis=l+(j-1)*UK
              IF (N(r,vis) > 1.D0) THEN
                X0=LOG(N(r,vis))
              ELSE
                X0=0.D0
              END IF

              VARSUM=P0(vis)+CVXX
              KAL=P0(vis)/VARSUM
              X=X0+KAL*(LOG(SIGHT(r)+1.0E-20)-LOG(BIAS(vis))-X0)
              P0(vis)=P0(vis)-KAL*P0(vis)
              N(r,vis)=EXP(X)
              POW=-0.5D0*((LOG(SIGHT(r)+1.0e-20)
     +            -LOG(BIAS(vis))-X0)**2)/VARSUM
              if (POW < -300.D0) THEN
                 COND(vis)=0.D0
              ELSE
                 COND(vis)=COND(vis)/(SQRT(2.0*pi*VARSUM))*EXP(POW)
              END IF
            end do
          END DO

        END IF
        CATCHSUM=CATCHSUM+CATM(r)+CATF(r)
C     Calculate posterior distribution
        IF (sum(COND).GT.0) POST=COND/sum(COND)
      END DO

C     Forcast for the next 5 years and determine catch limit

C     Initialize V_temp and V_tempsum
      V_temp=CATM(IYR-1)+CATF(IYR-1)
      V_tempsum=0.D0

      DO r=IYR,IYR+IQUOTA-1
        DO j=1,UA
          DO l=1,UK

            vis=l+(j-1)*UK
            RECRUIT=0.D0
            IF (N(r-1,vis)/MSY(l)*MSYR(j) .GE.0.D0)
     &        RECRUIT=(1.D0+A(j)*(1.D0-(N(r-1,vis)/MSY(l)*MSYR(j))**z))*
     &            N(r-1,vis)
            N(r,vis)=S*(N(r-1,vis)-V_temp)+(1.D0-S)*RECRUIT
            IF (N(r,vis)<1.D0) N(r,vis)=0.D0

          END DO
        END DO
        CALL catchrule(N(r,:),POST,V_temp,NSIM,IYR)
        V_tempsum=V_tempsum+V_temp
      END DO

      if (IYR.le.20) then
       SNAP = 1.0D0
      else
       SNAP = 1.0D0
      end if

      IF (V_tempsum > SNAP*NEED) THEN
        CATCHQ = NEED
      ELSEIF (V_tempsum < 0.8*CATCHSUM) THEN
        CATCHQ = CATCHSUM*0.8
      ELSEIF (V_tempsum > 1.2*CATCHSUM) THEN
        CATCHQ = CATCHSUM*1.2
      ELSE
        CATCHQ = V_tempsum
      END IF

      RETURN
      END

C  -----------------------------------------------------------------------------

      subroutine selx(n,arr,post,indx,gamma,catch)

C     Subroutine gives the index sorting of the vector arr

      INTEGER n,indx(n)
      DOUBLE PRECISION,DIMENSION(n) :: arr,post
      INTEGER i,indxt,ir,itemp,j,l,mid
      DOUBLE PRECISION ::  a,gamma,catch

      DO j=1,n
        indx(j)=j
      ENDDO
      l=1
      ir=n
    1 IF (ir-l .le. 10) THEN

         if (ir-l .eq. 1) then
            if (arr(indx(ir)) .lt. arr(indx(l))) then
               itemp=indx(l)
               indx(l)=indx(ir)
               indx(ir)=itemp
            end if
            if (SUM(post(indx(1:l)))>gamma) then
               a=arr(indx(l-1))
               do j=1,l-2
                  if (arr(indx(j)) .gt. a) then
                     a=arr(indx(j))
                     indxt=indx(l-1)
                     indx(l-1)=indx(j)
                     indx(j)=indxt
                  end if
               end do
               catch=arr(indx(l-1))+(arr(indx(l))-
     &              arr(indx(l-1)))*(gamma-SUM(post(indx(1:l-1))))/
     &              (SUM(post(indx(1:l)))-SUM(post(indx(1:l-1))))
            else

               catch=arr(indx(ir-1))+(arr(indx(ir))-arr(indx(ir-1)))
     &           *(gamma-SUM(post(indx(1:ir-1))))/
     &           (SUM(post(indx(1:ir)))-SUM(post(indx(1:ir-1))))
            END if

         ELSEIF (ir-l .eq. 0) then
            if (SUM(post(indx(1:ir))) .gt. gamma) then
               a=arr(indx(ir-1))
               do j=1,ir-2
                 if (arr(indx(j)) .gt. a) then
                    a=arr(indx(j))
                    indxt=indx(ir-1)
                    indx(ir-1)=indx(j)
                    indx(j)=indxt
                 end if
               end do
               catch=arr(indx(ir-1))+(arr(indx(ir))-arr(indx(ir-1)))
     &          *(gamma-SUM(post(indx(1:ir-1))))/
     &          (SUM(post(indx(1:ir)))-SUM(post(indx(1:ir-1))))
            else
               a=arr(indx(ir+1))
               do j=ir+2,n
                 if (arr(indx(j)) .lt. a) then
                    a=arr(indx(j))
                    indxt=indx(ir+1)
                    indx(ir+1)=indx(j)
                    indx(j)=indxt
                 end if
               end do
               catch=arr(indx(ir))+(arr(indx(ir+1))-arr(indx(ir)))
     &          *(gamma-SUM(post(indx(1:ir))))/
     &          (SUM(post(indx(1:ir+1)))-SUM(post(indx(1:ir))))
            end if
         else
            call shell(ir-l+1,arr(indx(l:ir)),indx(l:ir))
            do i=l,ir
              if (SUM(post(indx(1:i)))>gamma) then
                 if (i==l) then
                   a=arr(indx(l-1))
                   do j=1,l-2
                     if (arr(indx(j)) .gt. a) then
                        a=arr(indx(j))
                        indxt=indx(l-1)
                        indx(l-1)=indx(j)
                        indx(j)=indxt
                     end if
                   end do
                   catch=arr(indx(l-1))+(arr(indx(l))-
     &              arr(indx(l-1)))*(gamma-SUM(post(indx(1:l-1))))/
     &              (SUM(post(indx(1:l)))-SUM(post(indx(1:l-1))))

                 else
                    catch=arr(indx(i-1))+(arr(indx(i))-
     &               arr(indx(i-1)))*(gamma-SUM(post(indx(1:i-1))))/
     &               (SUM(post(indx(1:i)))-SUM(post(indx(1:i-1))))
                 end if
                 exit
              end if
            end do
         end if

         return
      ELSE
        mid=(l+ir)/2
        itemp=indx(mid)
        indx(mid)=indx(l+1)
        indx(l+1)=itemp
        IF(arr(indx(l)).gt.arr(indx(ir)))THEN
          itemp=indx(l)
          indx(l)=indx(ir)
          indx(ir)=itemp
        ENDIF
        IF(arr(indx(l+1)).gt.arr(indx(ir)))THEN
          itemp=indx(l+1)
          indx(l+1)=indx(ir)
          indx(ir)=itemp
        ENDIF
        IF(arr(indx(l)).gt.arr(indx(l+1)))THEN
          itemp=indx(l)
          indx(l)=indx(l+1)
          indx(l+1)=itemp
        ENDIF
        i=l+1
        j=ir
        indxt=indx(l+1)
        a=arr(indxt)
    3   CONTINUE
          i=i+1
        IF(arr(indx(i)).lt.a)GOTO 3
    4   CONTINUE
          j=j-1
        IF(arr(indx(j)).gt.a)GOTO 4
        IF(j.lt.i)GOTO 5
        itemp=indx(i)
        indx(i)=indx(j)
        indx(j)=itemp
        GOTO 3
    5   indx(l+1)=indx(j)
        indx(j)=indxt
        IF(SUM(post(indx(1:j))) .GT. gamma) ir=j
        IF(SUM(post(indx(1:j))) .LT. gamma) l=j+1
      ENDIF
      GOTO 1
      END

!-------------------------------------------------------------------

      subroutine shell(n,a,indx)

      INTEGER n,i,j,inc,indx(n),indxv
      DOUBLE PRECISION,DIMENSION(n) :: a
      DOUBLE PRECISION :: v

      inc=1
    1 inc=3*inc+1
      IF(inc.le.n)goto 1
    2 continue
       inc=inc/3
       do i=inc+1,n
          v=a(i)
          indxv=indx(i)
          j=i
    3     if (a(j-inc).gt.v) then
             a(j)=a(j-inc)
             indx(j)=indx(j-inc)
             j=j-inc
             IF(j.le.inc)GOTO 4
          GOTO 3
          endif
    4     a(j)=v
          indx(j)=indxv
       enddo
      IF(inc.gt.1)GOTO 2
      return

      end subroutine
!-------------------------------------------------------------------

      SUBROUTINE sel(n,ra,rb,rbr,gamma,catch)

C     Subrouting sorts ra in increasing order. rbr is the vector rb
C     sorted in the same way as ra.

      INTEGER n,iwksp(n)
      DOUBLE PRECISION,DIMENSION(n) :: ra,rb,wksp,rbr
      DOUBLE PRECISION gamma,catch
      CALL selx(n,ra,rb,iwksp,gamma,catch)

      wksp=ra
      ra=wksp(iwksp)
      wksp=rb
      rbr=wksp(iwksp)

      RETURN
      END 
!---------------------------------------------------------------------------
    
      SUBROUTINE diff(X,K,A,z,S,DF,V)

C     Subroutine which differentiates the logarithm of the P-T model

      DOUBLE PRECISION :: K,A,S,z,V,DIV,BRACKET
      DOUBLE PRECISION :: X,DF

      BRACKET=A*(1.D0-(X/K)**z)
      DIV=S*(X-V)+(1.D0-S)*(1.D0+BRACKET)*X
      DF=1/DIV*(S*X+(1.D0-S)*X*(1.D0+BRACKET-A*z*(X/K)**z))

      END
    
C-----------------------------------------------------------------------------

      SUBROUTINE catchrule(N,POST,C,NSIM,IYR)


C     The subroutine determine the catchlimit for one year, given the estimated
C     stock size (N), the posterior distribution of the Kalman filters and the
C     P-T parameters.

C     SUMRATIO  The cumulative sum of the posterior probabilities after sorting
C     POSTSORT  Sorted posterior probabilities (sorted according to increasing catch)
C     V         Estimated catch limit for each filter
C     MINIMUM   The stock protection limit
C     C         Estimated catch limit
C     MSY       Maximum sustainable yield
C     POINT     Index for the largest sumratio less than gamma
  
      DOUBLE PRECISION :: gamma,z,S
      DOUBLE PRECISION,DIMENSION(1044) :: N,POST
      DOUBLE PRECISION,DIMENSION(6) :: A,MSYR
      DOUBLE PRECISION,DIMENSION(174) :: MSY
      DOUBLE PRECISION :: Q,QHIST,cvadd

C     Local variables
      DOUBLE PRECISION,DIMENSION(1044) :: POSTSORT,V
      DOUBLE PRECISION :: C,minimum,beta,K

C     Counters
      INTEGER :: j,l,vis,IYR

C     dimensions of vectors K and A for do-loops
      INTEGER :: UA,UK

      COMMON /MODEL/ gamma,MSYR,MSY,z,S,A,Q,QHIST,cvadd


C     Find dimensions of vectors K and A for do-loops
      UA=UBOUND(A,dim=1)
      UK=UBOUND(MSY,dim=1)

      minimum=2000
      beta=0.7d0
      V=0.D0
      DO j=1,UA
        do l=1,UK

          vis=l+(j-1)*UK
          K=MSY(l)/MSYR(j)
          IF (N(vis) > minimum) THEN
           IF (N(vis) < 0.5*0.6*K) THEN
               V(vis)=beta*(N(vis)-minimum)/(0.5*0.6*K-minimum)*
     &          ((1.D0-S)/S*A(j)*(1.D0-(N(vis)/K)**z)*N(vis))
           ELSE
            IF (N(vis) < 0.9*0.6*K) THEN
              V(vis)=(beta+(0.8d0-beta)*(N(vis)-0.5*K*0.6)/(0.4*0.6*
     &          K))*((1.D0-S)/S*A(j)*(1.D0-(N(vis)/K)**z)*N(vis))
            ELSE
             IF (N(vis) < 0.6*K) THEN
                 V(vis)=(0.8d0+(N(vis)-0.9*K*0.6)/(0.6*K))
     &               *((1.D0-S)/S*A(j)*(1.D0-(N(vis)/K)**z)*N(vis))
             END IF
            END IF
           END IF
          END IF
          IF (N(vis) > 0.6*K) V(vis)=0.9*MSY(l)*0.6

        end do
      END DO

      CALL sel(UBOUND(V,dim=1),V,POST,POSTSORT,gamma,C)

      END


      SUBROUTINE SEVERE(ILEN,JLEN,LVLS,MLCAPE, &
                        &TARR,QARR,PARR,HARR,  &
                        &SFCARR,CAPE3,LSI)
           
           
           
           
      !----------------------------------------------------------------------------
      !Routine built to calculate some severe weather parameters and wrap to python
      !By JF Massicotte
      !2017-11-05
      !----------------------------------------------------------------------------


      implicit none

      !Declare variables
      integer, intent(in) :: ILEN,JLEN,LVLS
      !integer, dimension(LVLS,ILEN,JLEN),intent(in) :: 
      real,    dimension(LVLS,ILEN,JLEN),intent(in) :: TARR,QARR,PARR,HARR  !arrays from args
      real,    dimension(ILEN,JLEN),intent(in) :: MLCAPE,SFCARR
      real,    dimension(ILEN,JLEN),intent(inout) :: CAPE3,LSI    !output arrays
      real,    dimension(ILEN,JLEN) :: HLCL,MXLTMP    !LCL Prs,LCL tmp, LCL hgt, mltmp
      real,    dimension(LVLS,ILEN,JLEN) :: TPAR,TKPAR  ! Parcel temp, Parc mx tmp
      real,    dimension(50,318)  ::  TBLLCL
      real,    dimension(100,500) ::  TBLLR
      real :: STPRS,MXTMP,MXQ,MR,Y,DJ,DI,I1,I2,PLCL,TLCL,P1, &
              &P2,T1,T2,CAPET,LSIT,LFCH,ELH,LSIR,TK,ES,WS,TV,  &
              &LAT,PA,PB,DENS,TLR,TD,PR
      real,    parameter  ::  A=0.38,B=17.2693882,C=35.86,TFR=273.15
      integer    :: MXLVLS,LCL00,LCL10,LCL01,LCL11, &
                     &I,J,L,LR00,LR01,LR10,LR11


      !Init calculated arrays
!$omp  parallel do private (L,I,J)
      DO I=1, ILEN
        DO J=1,JLEN
          CAPE3(I,J)   = 0
          LSI(I,J)     = 0
          HLCL(I,J)    = 0
	  MXLTMP(I,J)  = 0
          DO L=1,LVLS
            TPAR(L,I,J) = 0
            
          END DO
        END DO
      END DO

      !Build lookup tables
	  
	  
	  
      !LCL table
!$omp  parallel do private (i,j,mr,y,td)	  
	  DO I=1, 50
	    DO J=233,318
		  MR = REAL(I)/(1-REAL(I))
		  Y = LOG(MR/A)
		  TD = (TFR-(C/B)*Y)/(1-Y/B)   !TD in Deg C
		  TBLLCL(I,J) = 1 / ((1/(REAL(J)-329.15))+(LOG((REAL(J)-273.15)/TD)/800)) + 56
		  
		  
		  
		END DO
	  END DO
	  
	  
	  
	!Saturated lapse rate table
!$omp  parallel do private (i,j,tk,es,ws,tv,lat,pa,pb,dens)
	  DO I=-50, 50
	    DO J=500,1000
	      TK=REAL(I)+273.15
	      ES=6.112*EXP(17.67*REAL(I)/(REAL(I)+243.5))
	      WS=0.62197*ES/(REAL(J)-ES)         !MIXING RATIO HERE IN G/G
	      TV=TK*(1.0+0.6*WS)              
	      LAT=2502.2-2.43089*REAL(I)
	      PA=1.0+LAT*WS/(287*TK)
	      PB=1.0+0.62197*LAT*LAT*WS/(1005*287*TK*TK)
	      DENS=REAL(J)*100/(287*TV)
	      TBLLR(I,J)=(PA/PB)/(1005*DENS)
	    END DO
	  END DO

	  
        
	  
	  




      !Calculate mixed layer parcel T and Q
      
	  
!$omp  parallel do private (i,j,l,stprs,mxtmp,mxq,mxlvls,lcl00,lcl10,lcl01,lcl11,di, &
!$omp& dj,i1,i2,tlcl,plcl,p1,p2,t1,t2,lr00,lr01,lr10,lr11,capet,lsit,lfch,elh,lsir,tlr,pr)

      DO I = 1, ILEN
        DO J = 1, JLEN
          IF (MLCAPE(I,J) .LT. 5) THEN
            CYCLE               !There is no 3kmCAPE at GP if there is no CAPE at all
          ENDIF
          STPRS = PARR(1,I,J) - 90000
          MXTMP = 0
          MXQ = 0
          MXLVLS = 0
          DO L=1,LVLS
            IF (PARR(L,I,J) .LT. STPRS) THEN
              EXIT             !No need to compute above 90mb
            ELSE
              MXLVLS = MXLVLS + 1
              MXTMP = MXTMP + TARR(L,I,J)/((PARR(L,I,J)/100000)**0.286)
              MXQ = MXQ + QARR(L,I,J)
            ENDIF
          END DO
          MXTMP = MXTMP / MXLVLS
          MXQ = MXQ / MXLVLS
	  
	  
	  !Now lookup table for LCL
          LCL00 = TBLLCL(FLOOR(MXQ),FLOOR(MXTMP))
          LCL10 = TBLLCL(FLOOR(MXQ)+1,FLOOR(MXTMP))
	  LCL01 = TBLLCL(FLOOR(MXQ),FLOOR(MXTMP)+1)
	  LCL11 = TBLLCL(FLOOR(MXQ)+1,FLOOR(MXTMP)+1)
	  DI = MXQ-REAL(FLOOR(MXQ))
	  DJ = MXTMP-REAL(FLOOR(MXTMP))
	  I1 = LCL00+(LCL10-LCL00)*DI
	  I2 = LCL01+(LCL11-LCL01)*DI
	  TLCL = I1+(I2-I1)*DJ
	  PLCL = 100000 * ((TLCL/MXTMP)**3.48)
	  
	  
	  !Elevate parcel moist adiabatically from LCL
	  CAPET=0
	  LSIT=0
	  DO L=2,LVLS
	    P1=PLCL   !init P and T at LCL
	    T1=TLCL
	    IF (HARR(L,I,J)-SFCARR(I,J) > 3000) THEN  !We stop looping after 3000m
	      EXIT
	    END IF
	    IF (PARR(L,I,J)>PLCL) THEN  !We wont elevate below LCL
	      CYCLE
	    ELSE
	      P2=PARR(L,I,J)
	      PR=(P1+P2)/2
	      !Lookup table for sat LR
	      LR00=TBLLR(FLOOR(T1),FLOOR(PR))
	      LR10=TBLLR(FLOOR(T1)+1,FLOOR(PR))
	      LR01=TBLLR(FLOOR(T1),FLOOR(PR)+1)
	      LR11=TBLLR(FLOOR(T1)+1,FLOOR(PR)+1)
	      DI=T1-REAL(FLOOR(T1))
	      DJ=P1-REAL(FLOOR(P1))
	      I1=LR00+(LR10-LR00)*DI
	      I2=LR01+(LR11-LR01)*DI
	      TLR=I1+(I2-I1)*DJ
	      
	      ! New parcel temperature
	      T2=T1-(P2-P1)*TLR
	      
              IF (T2 > TARR(L,I,J) .AND. T1 > TARR(L-1,I,J)) THEN  !whole layer is unstable
	      
                CAPET=CAPET+((T1-TARR(L-1,I,J)/T1+(T2-TARR(L,I,J))/T2)/2*9.81*(HARR(L,I,J)-HARR(L-1,I,J)))  !Calculate CAPE
		
	      ELSE IF (T2 > TARR(L,I,J)) THEN             !LFC is whitin layer,
	      
		LFCH=(TARR(L-1,I,J)-T1)/((TARR(L-1,I,J)-T1)+(T2-TARR(L,I,J)))*(HARR(L,I,J)-HARR(L-1,I,J))+HARR(L-1,I,J)   ! Get LFC height
		CAPET=CAPET+((T2-TARR(L,I,J))/TARR(L,I,J)/2*9.81*(HARR(L,I,J)-LFCH))! Then calculate CAPE
		
		IF(LSIT<(TARR(L-1,I,J)-T1)) THEN    ! Calculate LSI
		  LSIT=TARR(L-1,I,J)-T1
		END IF
		
	      ELSE IF (T2 < TARR(L,I,J) .AND. T1 > TARR(L-1,I,J)) THEN   ! not expected but catched equilibrium level
	        
		ELH=(T1-TARR(L-1,I,J))/((T1-TARR(L-1,I,J))+(TARR(L,I,J)-T2))*(HARR(L,I,J)-HARR(L-1,I,J))+HARR(L-1,I,J)  ! Get EL height
		CAPET=CAPET+((T1-TARR(L-1,I,J))/TARR(L-1,I,J)/2*9.81*(ELH-HARR(L,I,J)))   ! Then calculate CAPE
		
		IF(LSIT<(TARR(L,I,J)-T2)) THEN   ! Then LSI
		  LSIT=TARR(L,I,J)-T2
		END IF
		
	      ELSE         !Only possibility here is layer is fully stable
	        
		LSIR=MIN((TARR(L-1,I,J)-T1),(TARR(L,I,J)-T2))      !Only need to calculate LSI
		IF(LSIT<LSIR) THEN
		  LSIT=LSIR
		END IF
	      
	      
	      END IF    
	      
	    END IF
	    
	    
	    
	    P1=P2
	  END DO
	  
	  CAPE3(I,J)=CAPET
	  LSI(I,J)=LSIT
	  
	  
	  
        END DO
      END DO
      
      
      END SUBROUTINE





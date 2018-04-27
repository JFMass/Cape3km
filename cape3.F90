MODULE CAPE3

      CONTAINS
      
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
      real*8,    dimension(ILEN,JLEN),intent(out) :: CAPE3,LSI    !output arrays
      real,    dimension(ILEN,JLEN) :: HLCL,MXLTMP    !LCL Prs,LCL tmp, LCL hgt, mltmp
      real,    dimension(LVLS,ILEN,JLEN) :: TPAR,TKPAR  ! Parcel temp, Parc mx tmp
      real,    dimension(50,318)  ::  TBLLCL
      real,    dimension(-50:50,500:1000) ::  TBLLR
      real :: STPRS,MXTMP,MXQ,MR,Y,DJ,DI,I1,I2,PLCL,TLCL,P1, &
              &P2,T1,T2,CAPET,LSIT,LFCH,ELH,LSIR,TK,ES,WS,TV,  &
              &LAT,PA,PB,DENS,TLR,TD,PR,LR00,LR01,LR10,LR11, POTT, WBPOT
      real,    parameter  ::  A=0.0038,B=17.2693882,C=35.86,TFR=273.15
      integer    :: MXLVLS,LCL00,LCL10,LCL01,LCL11, &
                     &I,J,L


      !Init calculated arrays
      write(6,*) 'initiating arrays!'
!$omp  parallel do private (L,I,J)
      DO I=1,ILEN
        DO J=1,JLEN
          CAPE3(I,J)   = 0
          LSI(I,J)     = 5
          HLCL(I,J)    = 0
	  MXLTMP(I,J)  = 0
          DO L=1,LVLS
            TPAR(L,I,J) = 0
            
          END DO
        END DO
      END DO

      !Build lookup tables
	  
	  
	  
      !LCL table from specific humidity and temperature
        write(6,*) 'Building LCL table!'
!$omp  parallel do private (i,j,mr,y,td)	
        DO I=1, 50
          DO J=233,318
	    !MR = REAL(I)/(1-REAL(I))
	    !Y = LOG(MR/A)
            Y = LOG(REAL(I)*0.001/A)
	    TD = (TFR-(C/B)*Y)/(1-Y/B)  !TD in Deg C
	    !TBLLCL(I,J) = 1 / ((1/(REAL(J)-329.15))+(LOG((REAL(J)-273.15)/TD)/800)) + 56
            TBLLCL(I,J) = ((1/(1/(TD-56)+LOG(REAL(J)/TD)/800))+56)
            !write(*,*) I, J, Y, TD, TBLLCL(I,J)
		  
		  
	  END DO
	END DO
	  
	  
	  
	!Saturated lapse rate table
        write(6,*) 'Building LR table!'
!$omp  parallel do private (i,j,tk,es,ws,tv,lat,pa,pb,dens)
        DO I=-50, 50
          DO J=500,1000
            TK=REAL(I)+273.15
            ES=6.112*EXP(17.67*REAL(I)/(REAL(I)+243.5))   !Satvap in mb
            WS=0.62197*ES/((REAL(J))-ES)         !MIXING RATIO HERE IN G/G
            TV=TK*(1.0+0.6*WS)              
            LAT=2502.2-2.43089*REAL(I)*1000
            PA=1.0+LAT*WS/(287*TK)
            PB=1.0+0.62197*LAT*LAT*WS/(1005*287*TK*TK)
            DENS=REAL(J)*100/(287*TV)
            TBLLR(I,J)=(PA/PB)/(1005*DENS)
            !write(*,*) I, J, ES, WS, TV, LAT, PA, PB, DENS, TBLLR(I,J)
          END DO
        END DO

	  
        
	  
	  




      !Calculate mixed layer parcel T and Q
      
      write(6,*) 'Iterating grid'
!$omp  parallel do private (i,j,l,stprs,mxtmp,mxq,mxlvls,lcl00,lcl10,lcl01,lcl11,di,y,td, &
!$omp& dj,i1,i2,tlcl,plcl,p1,p2,t1,t2,lr00,lr01,lr10,lr11,capet,lsit, &
!$omp& lfch,elh,lsir,tlr,pr,pott, wbpot)

      DO I = 1, ILEN   !replace with ILEN once testing is done
        DO J = 1, JLEN    !replace with JLEN once testing is done
          IF (MLCAPE(I,J) .LT. 5) THEN
            CYCLE               !There is no 3kmCAPE at GP if there is no CAPE at all
          ENDIF
          STPRS = PARR(1,I,J) - 9000
          MXTMP = 0
          MXQ = 0
          MXLVLS = 0
          DO L=1,LVLS
            IF (PARR(L,I,J) .LT. STPRS) THEN
              EXIT             !No need to compute above 90mbBus error (core dumped)
            ELSE
              MXLVLS = MXLVLS + 1
              MXTMP = MXTMP + TARR(L,I,J)/((PARR(L,I,J)/100000)**0.286)
              MXQ = MXQ + QARR(L,I,J)
              !write(*,*) L, PARR(L,I,J), MXQ, QARR(L,I,J), TARR(L,I,J)
            ENDIF
          END DO
          MXTMP = MXTMP / MXLVLS
          MXQ = MXQ / MXLVLS
          !write(*,*) MXQ, MXTMP, MXLVLS	  
	  
          
          Y = LOG(MXQ/A)
	  TD = (TFR-(C/B)*Y)/(1-Y/B)  !TD in K
          !write(*,*) I, J, TD-TFR, MXTMP-TFR
          IF (TD > MXTMP) THEN
            TD = MXTMP
          END IF
          TLCL = ((1/(1/(TD-56)+LOG(MXTMP/TD)/800))+56)
	  PLCL = 100000 * ((TLCL/MXTMP)**3.48)
          !write(*,*) I, J, TD-TFR, TLCL, PLCL  


	  !Elevate parcel moist adiabatically from LCL
	  CAPET=0
	  LSIT=0
	  POTT=OS(TLCL-273.15,PLCL*0.01)
	  WBPOT=TSA(POTT,1000.0)
          P1=PLCL     !init P and T at LCL
          T1=TLCL
          !write(6,*) 'start iterating levels' 
	  DO L=2,LVLS
            !write(*,*) L, P1, PLCL, T1, TARR(L,I,J)
	    IF (HARR(L,I,J)-SFCARR(I,J) > 3000) THEN  !We stop looping after 3000m
	      EXIT
	    END IF
	    IF (PARR(L,I,J)>PLCL) THEN  !We wont elevate below LCL
              P1=PARR(L,I,J)
	      CYCLE
	    ELSE
	      P2=PARR(L,I,J)
	      PR=(P1+P2)/2
	      !Lookup table for sat LR
              !write(*,*) L,  
	      !LR00=TBLLR(FLOOR(T1-TFR),FLOOR(PR*0.01))
	      !LR10=TBLLR(FLOOR(T1-TFR)+1,FLOOR(PR*0.01))
	      !LR01=TBLLR(FLOOR(T1-TFR),FLOOR(PR*0.01)+1)
	      !LR11=TBLLR(FLOOR(T1-TFR)+1,FLOOR(PR*0.01)+1)
	      !DI=(T1-TFR)-REAL(FLOOR(T1-TFR))
	      !DJ=P1*0.01-REAL(FLOOR(P1*0.01))
	      !I1=LR00+(LR10-LR00)*DI
	      !I2=LR01+(LR11-LR01)*DI
	      !TLR=I1+(I2-I1)*DJ
              !T2=T1-(P1-P2)*0.01*TLR
              T2=SATLFT(WBPOT,P2*0.01)+TFR
	      !write(*,*) L, WBPOT, TARR(L,I,J), T2
	      ! New parcel temperature
 
              IF (MLCAPE(I,J) > 1000) THEN
               !write(*,*) I, J, L, LR00, LR11, TLR, T1, T2, TARR(L-1,I,J), TARR(L,I,J)
	      END IF

              IF (T2 > TARR(L,I,J) .AND. T1 > TARR(L-1,I,J)) THEN  !whole layer is unstable
	      
                CAPET=CAPET+(((T1-TARR(L-1,I,J))/T1+(T2-TARR(L,I,J))/T2)/2*9.81*(HARR(L,I,J)-HARR(L-1,I,J)))  !Calculate CAPE
		!write(*,*)L, 'Totally unstable layer, CAPE so far:', CAPET
	      ELSE IF (T2 > TARR(L,I,J)) THEN             !LFC is whitin layer,
	      
		LFCH=(TARR(L-1,I,J)-T1)/((TARR(L-1,I,J)-T1)+(T2-TARR(L,I,J)))*(HARR(L,I,J)-HARR(L-1,I,J))+HARR(L-1,I,J)   ! Get LFC height
		CAPET=CAPET+((T2-TARR(L,I,J))/TARR(L,I,J)/2*9.81*(HARR(L,I,J)-LFCH))! Then calculate CAPE
		!write(*,*)L, 'LFC within layer, CAPE so far:', CAPET
		IF(LSIT<(TARR(L-1,I,J)-T1)) THEN    ! Calculate LSI
		  LSIT=TARR(L-1,I,J)-T1
		END IF
		
	      ELSE IF (T2 < TARR(L,I,J) .AND. T1 > TARR(L-1,I,J)) THEN   ! not expected but catched equilibrium level
	        
		ELH=(T1-TARR(L-1,I,J))/((T1-TARR(L-1,I,J))+(TARR(L,I,J)-T2))*(HARR(L,I,J)-HARR(L-1,I,J))+HARR(L-1,I,J)  ! Get EL height
		CAPET=CAPET+((T1-TARR(L-1,I,J))/TARR(L-1,I,J)/2*9.81*(ELH-HARR(L,I,J)))   ! Then calculate CAPE
		!write(*,*)L, 'El within layer, CAPE so far:', CAPET
		IF(LSIT<(TARR(L,I,J)-T2)) THEN   ! Then LSI
		  LSIT=TARR(L,I,J)-T2
		END IF
		
	      ELSE         !Only possibility here is layer is fully stable
	        !write(*,*)L, 'Totally stable layer, CAPE so far:', CAPET
		LSIR=MIN((TARR(L-1,I,J)-T1),(TARR(L,I,J)-T2))      !Only need to calculate LSI
		IF(LSIT<LSIR) THEN
		  LSIT=LSIR
		END IF
	      
	      
	      END IF    
	      
	    END IF
	    
	    
	    T1=T2
	    P1=P2
	  END DO
	  
          
	  CAPE3(I,J)=CAPET
	  LSI(I,J)=LSIT
          IF (MLCAPE(I,J) > 500) THEN
            !write(*,*) I, J, MLCAPE(I,J), CAPE3(I,J), LSI(I,J)
	  END IF
	  
	  
        END DO
      END DO
      
      write(6,*) 'Done Iterating grid'
      
      
      END SUBROUTINE



      FUNCTION satlft(thw,p)
      !
      !    baker, schlatter  17-may-1982     original version.
      !
      !   input:  thw = wet-bulb potential temperature (celsius).
      !         thw defines a moist adiabat.
      !        p = pressure (millibars)
      !   output: satlft = temperature (celsius) where the moist adiabat
      !         crosses p
         DATA cta,akap/273.15,0.28541/
       !   cta = difference between kelvin and celsius temperatures
      !   akap = (gas constant for dry air) / (specific heat at constant
      !        pressure for dry air)

      !     the algorithm below can best be understood by referring to a
      !   skew-t/log p chart.  it was devised by herman wobus, a mathemati-
      !   cian formerly at the navy weather research facility but now retired.
      !   the value returned by satlft can be checked by referring to table
      !   78, pp.319-322, smithsonian meteorological tables, by roland list
      !   (6th revised edition).
      !

        IF (p /= 1000.) GO TO 5
        satlft = thw
        RETURN
        5   CONTINUE

        !   compute tone, the temperature where the dry adiabat with value thw
        !   (celsius) crosses p.

        pwrp = (p/1000.)**akap
        tone = (thw+cta)*pwrp-cta

      !   consider the moist adiabat ew1 through tone at p.  using the defini-
      !   tion of the wobus function (see documentation on wobf), it can be
      !   shown that eone = ew1-thw.

        eone = wobf(tone)-wobf(thw)
        rate = 1.
        GO TO 15

      !   in the loop below, the estimate of satlft is iteratively improved.
        10   CONTINUE

      !   rate is the ratio of a change in t to the corresponding change in
      !   e.  its initial value was set to 1 above.

        rate = (ttwo-tone)/(etwo-eone)
        tone = ttwo
        eone = etwo
        15   CONTINUE

      !   ttwo is an improved estimate of satlft.

        ttwo = tone-eone*rate

      !   pt is the potential temperature (celsius) corresponding to ttwo at p

        pt = (ttwo+cta)/pwrp-cta

      !   consider the moist adiabat ew2 through ttwo at p. using the defini-
      !   tion of the wobus function, it can be shown that etwo = ew2-thw.
         etwo = pt+wobf(ttwo)-wobf(pt)-thw
      !   dlt is the correction to be subtracted from ttwo.

        dlt = etwo*rate
        IF (ABS(dlt) > 0.1) GO TO 10
        satlft = ttwo-dlt
        RETURN
        END FUNCTION satlft
          
          
        FUNCTION wobf(t)

      !   this function calculates the difference of the wet-bulb potential
      !   temperatures for saturated and dry air given the temperature.
      !
      !    baker, schlatter  17-may-1982     original version.
      !
      !     let wbpts = wet-bulb potential temperature for saturated
      !   air at temperature t (celsius). let wbptd = wet-bulb potential
      !   temperature for completely dry air at the same temperature t.
      !   the wobus function wobf (in degrees celsius) is defined by
      !                   wobf(t) = wbpts-wbptd.
      !   although wbpts and wbptd are functions of both pressure and
      !   temperature, their difference is a function of temperature only.
      !     to understand why, consider a parcel of dry air at tempera-
      !   ture t and pressure p. the thermodynamic state of the parcel is
      !   represented by a point on a pseudoadiabatic chart. the wet-bulb
      !   potential temperature curve (moist adiabat) passing through this
      !   point is wbpts. now t is the equivalent temperature for another
      !   parcel saturated at some lower temperature tw, but at the same
      !   pressure p.  to find tw, ascend along the dry adiabat through
      !   (t,p). at a great height, the dry adiabat and some moist
      !   adiabat will nearly coincide. descend along this moist adiabat
      !   back to p. the parcel temperature is now tw. the wet-bulb
      !   potential temperature curve (moist adiabat) through (tw,p) is wbptd.
      !   the difference (wbpts-wbptd) is proportional to the heat imparted
      !   to a parcel saturated at temperature tw if all its water vapor
      !   were condensed. since the amount of water vapor a parcel can
      !   hold depends upon temperature alone, (wbptd-wbpts) must depend
      !   on temperature alone.

      !     the wobus function is useful for evaluating several thermo-
      !   dynamic quantities.  by definition:
      !           wobf(t) = wbpts-wbptd.               (1)
      !   if t is at 1000 mb, then t is a potential temperature pt and
      !   wbpts = pt. thus
      !           wobf(pt) = pt-wbptd.                 (2)
      !   if t is at the condensation level, then t is the condensation
      !   temperature tc and wbpts is the wet-bulb potential temperature
      !   wbpt. thus
      !           wobf(tc) = wbpt-wbptd.               (3)
      !   if wbptd is eliminated from (2) and (3), there results
      !           wbpt = pt-wobf(pt)+wobf(tc).
      !   if wbptd is eliminated from (1) and (2), there results
      !           wbpts = pt-wobf(pt)+wobf(t).

      !     if t is an equivalent potential temperature ept (implying
      !   that the air at 1000 mb is completely dry), then wbpts = ept
      !   and wbptd = wbpt. thus
      !           wobf(ept) = ept-wbpt.
      !   this form is the basis for a polynomial approximation to wobf.
      !   in table 78 on pp.319-322 of the smithsonian meteorological
      !   tables by roland list (6th revised edition), one finds wet-bulb
      !   potential temperatures and the corresponding equivalent potential
      !   temperatures listed together. herman wobus, a mathematician for-
      !   merly at the navy weather research facility, norfolk, virginia,
      !   and now retired, computed the coefficients for the polynomial
      !   approximation from numbers in this table.
      !
      !                                 notes by t.w. schlatter
      !                                 noaa/erl/profs program office
      !                                 august 1981

        x = t-20.
        IF (x > 0.) GO TO 10
        pol = 1.                 +x*(-8.8416605E-03                           &
              +x*( 1.4714143E-04  +x*(-9.6719890E-07                          &
             +x*(-3.2607217E-08  +x*(-3.8598073E-10)))))
        wobf = 15.130/pol**4
        RETURN
        10   CONTINUE
        pol = 1.                 +x*( 3.6182989E-03                           &
             +x*(-1.3603273E-05  +x*( 4.9618922E-07                           &
             +x*(-6.1059365E-09  +x*( 3.9401551E-11                           &
             +x*(-1.2588129E-13  +x*( 1.6688280E-16)))))))
        wobf = 29.930/pol**4+0.96*x-14.8
        RETURN
        END FUNCTION wobf
        
        FUNCTION os(t,p)
!
!    g.s. stipanuk     1973          original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns the equivalent potential temperature os
!   (celsius) for a parcel of air saturated at temperature t (celsius)
!   and pressure p (millibars).
        DATA b/2.6518986/
!   b is an empirical constant approximately equal to the latent heat
!   of vaporization for water divided by the specific heat at constant
!   pressure for dry air.

        tk = t+273.15
        osk= tk*((1000./p)**.286)*(EXP(b*w(t,p)/tk))
        os= osk-273.15
        RETURN
        END FUNCTION os
        
         FUNCTION w(t,p)
!
!    g.s. stipanuk     1973              original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982
!
!  this function returns the mixing ratio (grams of water vapor per
!  kilogram of dry air) given the dew point (celsius) and pressure
!  (millibars). if the temperture  is input instead of the
!  dew point, then saturation mixing ratio (same units) is returned.
!  the formula is found in most meteorological texts.

        x= esat(t)
        w= 622.*x/(p-x)
        RETURN
        END FUNCTION w
        
        
        FUNCTION esat(t)
!
!    g.s. stipanuk     1973           original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982
!
!   this function returns the saturation vapor pressure over
!   water (mb) given the temperature (celsius).
!   the algorithm is due to nordquist, w.s.,1973: "numerical approxima-
!   tions of selected meteorlolgical parameters for cloud physics prob-
!   lems," ecom-5475, atmospheric sciences laboratory, u.s. army
!   electronics command, white sands missile range, new mexico 88002.

        tk = t+273.15
        p1 = 11.344-0.0303998*tk
        p2 = 3.49149-1302.8844/tk
        c1 = 23.832241-5.02808*ALOG10(tk)
        esat = 10.**(c1-1.3816E-7*10.**p1+8.1328E-3*10.**p2-2949.076/tk)
        RETURN
        END FUNCTION esat
        
        
         FUNCTION ow(t,td,p)
!
!    g.s. stipanuk     1973          original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns the wet-bulb potential temperature ow
!   (celsius) given the temperature t (celsius), dew point td
!   (celsius), and pressure p (millibars).  the calculation for ow is
!   very similar to that for wet bulb temperature. see p.13 stipanuk (1973).
!   find the wet bulb temperature of the parcel

        atw = tw(t,td,p)

!   find the equivalent potential temperature of the parcel.

        aos= os(atw,p)

!   find the wet-bulb potential temperature.

        ow= tsa(aos,1000.)
        RETURN
        END FUNCTION ow
        
        
        FUNCTION tsa(os,p)
!
!    g.s. stipanuk     1973           original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns the temperature tsa (celsius) on a saturation
!   adiabat at pressure p (millibars). os is the equivalent potential
!   temperature of the parcel (celsius). sign(a,b) replaces the
!   algebraic sign of a with that of b.
!   b is an empirical constant approximately equal to 0.001 of the latent
!   heat of vaporization for water divided by the specific heat at constant
!   pressure for dry air.

         DATA b/2.6518986/
         a= os+273.15

!   tq is the first guess for tsa.

         tq= 253.15

!   d is an initial value used in the iteration below.

         d= 120.

!   iterate to obtain sufficient accuracy....see table 1, p.8
!   of stipanuk (1973) for equation used in iteration.

         DO i= 1,12
          tqk= tq-273.15
          d= d/2.
           x= a*EXP(-b*w(tqk,p)/tq)-tq*((1000./p)**.286)
           IF (ABS(x) < 1E-7) GOTO 2
           tq= tq+SIGN(d,x)
        END DO
       2 tsa= tq-273.15
        RETURN
        END FUNCTION tsa
        
        
         FUNCTION tw(t,td,p)

!    g.s. stipanuk     1973           original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns the wet-bulb temperature tw (celsius)
!   given the temperature t (celsius), dew point td (celsius)
!   and pressure p (mb).  see p.13 in stipanuk (1973), referenced
!   above, for a description of the technique.
!
!
!   determine the mixing ratio line thru td and p.

         aw = w(td,p)
!
!   determine the dry adiabat thru t and p.

         ao = o(t,p)
         pi = p

!   iterate to locate pressure pi at the intersection of the two
!   curves .  pi has been set to p for the initial guess.

         DO i= 1,10
          x= .02*(tmr(aw,pi)-tda(ao,pi))
          IF (ABS(x) < 0.01) EXIT
           pi= pi*(2.**(x))
        END DO

!   find the temperature on the dry adiabat ao at pressure pi.

        ti= tda(ao,pi)

!   the intersection has been located...now, find a saturation
!   adiabat thru this point. function os returns the equivalent
!   potential temperature (c) of a parcel saturated at temperature
!   ti and pressure pi.

        aos= os(ti,pi)

!   function tsa returns the wet-bulb temperature (c) of a parcel at
!   pressure p whose equivalent potential temperature is aos.

        tw = tsa(aos,p)
         RETURN
        END FUNCTION tw
        
        
        FUNCTION tmr(w,p)
!
!    g.s. stipanuk     1973           original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns the temperature (celsius) on a mixing
!   ratio line w (g/kg) at pressure p (mb). the formula is given in
!   table 1 on page 7 of stipanuk (1973).
!
!   initialize constants

        DATA c1/.0498646455/,c2/2.4082965/,c3/7.07475/
        DATA c4/38.9114/,c5/.0915/,c6/1.2035/

        x= ALOG10(w*p/(622.+w))
        tmrk= 10.**(c1*x+c2)-c3+c4*((10.**(c5*x)-c6)**2.)
        tmr= tmrk-273.15
        RETURN
        END FUNCTION tmr
        
        
        FUNCTION tda(o,p)
!
!    g.s. stipanuk     1973           original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns the temperature tda (celsius) on a dry adiabat
!   at pressure p (millibars). the dry adiabat is given by
!   potential temperature o (celsius). the computation is based on
!   poisson's equation.

        ok= o+273.15
        tdak= ok*((p*.001)**.286)
        tda= tdak-273.15
        RETURN
        END FUNCTION tda
        
        FUNCTION o(t,p)
!
!    g.s. stipanuk     1973          original version.
!    reference stipanuk paper entitled:
!         "algorithms for generating a skew-t, log p
!         diagram and computing selected meteorological
!         quantities."
!         atmospheric sciences laboratory
!         u.s. army electronics command
!         white sands missile range, new mexico 88002
!         33 pages
!    baker, schlatter  17-may-1982

!   this function returns potential temperature (celsius) given
!   temperature t (celsius) and pressure p (mb) by solving the poisson
!   equation.

        tk= t+273.15
        ok= tk*((1000./p)**.286)
        o= ok-273.15
        RETURN
        END FUNCTION o

  
   
  
  

END MODULE


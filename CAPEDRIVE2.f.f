      SUBROUTINE SEVERE(ILEN,JLEN,LVLS,MLCAPE,TARR,QARR,PARR,HARR,CAPE3,LSI)
           
           
           
           
      !----------------------------------------------------------------------------
      !Routine built to calculate some severe weather parameters and wrap to python
      !By JF Massicotte
      !2017-11-05
      !----------------------------------------------------------------------------


      implicit none

      !Declare variables
      integer, intent(in) :: ILEN,JLEN,LVLS
      integer, dimension(LVLS,ILEN,JLEN),intent(in) ::
      real,    dimension(LVLS,ILEN,JLEN),intent(in) :: TARR,QARR,PARR,HARR  !arrays from args
      real,    dimension(ILEN,JLEN),intent(in) :: MLCAPE
      real,    dimension(ILEN,JLEN),intent(inout) :: CAPE3,LSI    !output arrays
      real,    dimension(ILEN,JLEN) :: PLCL,TLCL,HLCL,MXLTMP    !LCL Prs,LCL tmp, LCL hgt, mltmp
      real,    dimension(LVLS,ILEN,JLEN) :: TPAR,TKPAR  ! Parcel temp, Parc mx tmp
	  real,    dimension(50,318)  ::  TBLLCL
      real                :: STPRS,MXTMP,MXQ,MR,Y,DJ,DI     !Stop pressure, Mix tmp add,
	  real,    parameter  ::  A=0.38,B=17.2693882,C=35.86,TFR=273.15
      integer,    :: MXLVLS,LCL00,LCL10,LCL01,LCL11

      !Init calculated arrays
!$omp  parallel do private (i,j,l)
      DO I=1,ILEN
        DO J=1,JLEN
          CAPE3(I,J)   = 0
          LSI(I,J)     = 0
		  TLCL(I,J)    = 0
          HLCL(I,J)    = 0
          PLCL(I,J)    = 0
		  MXLTMP(I,J)  = 0
          DO L=1,LVLS
            TPAR(L,I,J) = 0
            
          END DO
        END DO
      END DO

      !Build lookup tables
	  
!$omp  parallel do private (i,j,mr,y,td)	  
	  DO I=1,50
	    Do J=233,318
		  MR = REAL(I)/(1-REAL(I))
		  Y = LOG(MR/A)
		  TD = (TFR-(C/B)*Y)/(1-Y/B)   !TD in Deg C
		  TBLLCL(I,J) = 1 / ((1/(REAL(J)-329.15))+(LOG((REAL(J)-273.15)/TD)/800)) + 56
		  
		  
		  
		END DO
	  END DO
	  
	  
	  

	  
	  
	  
	  




      !Calculate mixed layer parcel T and Q
      
	  
!$omp  parallel do private (i,j,l,stprs,mxtmp,mxq,mxlvls,lcl00,lcl10,lcl01,clc11,di,dj,i1,i2)
      DO I=1,ILEN
        DO J=1,JLEN
          IF (MLCAPE(I,J) .LT. 5) THEN
            CYCLE               !There is no 3kmCAPE at GP if there is no CAPE at all
          ENDIF
          STPRS = PARR(1,I,J) - 90000
          MXTMP = 0
          MXQ = 0
          MXLVLS
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
		  LCL10 = TBLLCL(FLOOR(MXQ)+1),FLOOR(MXTMP)
		  LCL01 = TBLLCL(FLOOR(MXQ),FLOOR(MXTMP)+1)
		  LCL11 = TBLLCL(FLOOR(MXQ)+1,FLOOR(MXTMP)+1)
		  DI = MXQ-FLOOR(MXQ)
		  DJ = MXTMP-FLOOR(MXTMP)
		  I1 = LCL00+(LCL10-LCL00)*DI
		  I2 = LCL01+(LCL11-LCL01)*DI
		  TLCL(I,J) = I1+(I2-I1)*DJ
		  PLCL(I,J) = 100000 * ((TLCL(I,J)/MXTMP)**3.48)
        END DO
      END DO

      !Calculate LCL atributes

!$omp  parallel do private

      DO I=1,ILEN
        DO J=1,JLEN      




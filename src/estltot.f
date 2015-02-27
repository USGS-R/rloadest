************************************************************************
*
*     Subroutine estltot                               Called by: R
*
*     compute nearly-unbiased AMLE total load estimates
*
************************************************************************
      SUBROUTINE estltot(NPAR,PARMLE,BIAS,CV_IN,SBIAS,SCV_IN,
     &     NPRED,XLPRED,NDAYS,N_DAY,sdin,
     &     LOAD,LOADVAR,LOADLOW,LOADUP,LOADSP,IERR)
*     
*     dimensional parameters and logical devices
*
      INCLUDE 'fmodules.h'
*
*     subroutine arguments
*
      INTEGER*4 NPAR,NPRED,NDAYS,N_DAY,IERR, sdin
      DOUBLE PRECISION PARMLE(*),BIAS(*),CV_IN(NPAR+1, *),SBIAS(*),
     &     SCV_IN(NPAR+1, *),XLPRED(NPRED,*)
      DOUBLE PRECISION LOAD,LOADVAR,LOADLOW,
     $     LOADUP,LOADSP
*
*     local vars
*
      LOGICAL SDEXACT
      INTEGER I,K
      DOUBLE PRECISION MVUE(MAXOBSE),CV(MAXPARMS+1,MAXPARMS+1),
     &   SCV(MAXPARMS+1,MAXPARMS+1),XLSPRED(MAXOBSE,MAXPARMS),S2,
     &   CV_M(MAXPARMS+1,MAXPARMS+1),S3_M
*
*     set initial values and check limits
*
      if(sdin .eq. 1) then
         SDEXACT = .TRUE.
      else
         SDEXACT = .FALSE.
      endif
      IERR=0
      IF(NPAR .gt. MAXPARMS) IERR=1
      IF(NOBSC .gt. MAXOBSC) IERR=2
      IF(NPRED .gt. MAXOBSE) IERR=4
      IF(IERR .ne. 0) RETURN
      S2=PARMLE(NPAR+1)/(1.d0+BIAS(NPAR+1))
      S3_M=PARMLE(NPAR+1)**1.5
*
*     Copy the CVs to internal memory
*
      DO I=1,NPAR+1
         DO K=1,NPAR+1
            CV(I,K)=CV_IN(I,K)
            SCV(I,K)=SCV_IN(I,K)
         ENDDO
      ENDDO
*
*     The logic allows for variable numbers per day, which can
*     deal with 0 values (processed out in R)
*
      DO I=1,NPRED
         DO K=1,NPAR
            XLSPRED(I,K) = XLPRED(I,K)
         ENDDO
      ENDDO
      IF(SDEXACT) THEN
*
*     Compute load
*
         CALL TAC_LOAD(NPAR,PARMLE,BIAS,CV,SBIAS,SCV,NPRED,XLSPRED,
     &         MVUE,LOAD,LOADVAR)
*
      ELSE
*
*     Compute CV_M and then the load
*
         CV_M(NPAR+1,NPAR+1) = PARMLE(NPAR+1)**2 *
     &         CV(NPAR+1,NPAR+1)
         DO I=1,NPAR
            CV_M(I,NPAR+1) = S3_M*CV(I,NPAR+1)
            CV_M(NPAR+1,I) = CV_M(I,NPAR+1)
            DO K=I,NPAR
              CV_M(K,I) = PARMLE(NPAR+1)*CV(K,I)
              CV_M(I,K) = CV_M(K,I)
            ENDDO
         ENDDO
         CALL TACIT_LOADS_NC(NPAR,NPRED,XLSPRED,MVUE,LOAD,
     &                       LOADVAR,PARMLE,BIAS,SBIAS,CV,SCV,CV_M)
      ENDIF
      LOAD = LOAD/NDAYS/N_DAY
*
*     Compute standard error of prediction assuming errors
*     uncorrelated lag 1 (approx correct for mid-size rivers -- see
*     Tim Cohn 1989 notes)
*     
      LOADVAR = LOADVAR/(N_DAY*N_DAY)
      CALL LOADSEP(N_DAY,NPRED,MVUE,S2,LOADVAR,LOADSP)
      LOADVAR = LOADVAR/(NDAYS*NDAYS)
*
*     Compute approx 95% confidence interval around true load
*     assuming a 2-parameter lognormal distribution
*
      CALL LOAD95CI(LOAD,LOADSP,LOADLOW,LOADUP)
      RETURN
      END

************************************************************************
*
*     Subroutine estlday                               Called by: R
*
*     compute nearly-unbiased AMLE daily (or unit) load estimates
*
************************************************************************
      SUBROUTINE estlday(NPAR,PARMLE,BIAS,CV_IN,SBIAS,SCV_IN,
     &     NPRED,XLPRED,NDAYS,KDAY2,
     &     LOAD,LOADVAR,LOADLOW,LOADUP,LOADSP,IERR)
*     
*     dimensional parameters and logical devices
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NPAR,NPRED,KDAY2(*),NDAYS,IERR
      DOUBLE PRECISION PARMLE(*),BIAS(*),CV_IN(NPAR+1, *),SBIAS(*),
     &     SCV_IN(NPAR+1, *),XLPRED(NPRED,*)
      DOUBLE PRECISION LOAD(*),LOADVAR(*),LOADLOW(*),
     $     LOADUP(*),LOADSP(*)
*
*     local vars
*
      INTEGER K1,K2,I,K,NUM
      DOUBLE PRECISION MVUE(MAXOBSE),CV(MAXPARMS+1,MAXPARMS+1),
     &   SCV(MAXPARMS+1,MAXPARMS+1),XLSPRED(MAXOBSE,MAXPARMS),S2
*
*     set initial values and check limits
*
      IERR=0
      IF(NPAR .gt. MAXPARMS) IERR=1
      IF(NOBSC .gt. MAXOBSC) IERR=2
      IF(NPRED .gt. MAXOBSE) IERR=4
      IF(IERR .ne. 0) RETURN
      S2=PARMLE(NPAR+1)/(1.d0+BIAS(NPAR+1))
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
      K1=1
      DO I=1,NDAYS
*
*     Get the unit values for each day
*     The logic allows for variable numbers per day, which can
*     deal with 0 values (processed out in R)
*
         NUM=0
         K2=KDAY2(K1)
         DO WHILE (K2 .eq. KDAY2(K1))
            NUM=NUM+1
            DO K=1,NPAR
               XLSPRED(NUM,K) = XLPRED(K1,K)
            ENDDO
            K1=K1+1
            if(K1 .gt. NPRED) GOTO 10
         ENDDO
 10      CONTINUE
*
*     Compute load
*
         CALL TAC_LOAD(NPAR,PARMLE,BIAS,CV,SBIAS,SCV,NUM,XLSPRED,MVUE,
     &      LOAD(I),LOADVAR(I))
*
         LOAD(I) = LOAD(I)/NUM
*
*     Compute standard error of prediction assuming errors
*     uncorrelated lag 1 (approx correct for mid-size rivers -- see
*     Tim Cohn 1989 notes)
*     
         CALL LOADSEP(NUM,MVUE,S2,LOADVAR(I),LOADSP(I))
         LOADVAR(I) = LOADVAR(I)/(NUM*NUM)
*
*     Compute approx 95% confidence interval around true load
*     assuming a 2-parameter lognormal distribution
*
         CALL LOAD95CI(LOAD(I),LOADSP(I),LOADLOW(I),LOADUP(I))
      ENDDO
      
      RETURN
      END

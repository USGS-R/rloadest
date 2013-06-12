************************************************************************
*
*     Subroutine LOADSEP                            Called by: AMLLOAD2
*
*     compute standard error of prediction (SEP), a measure of how
*     close the estimated sum of loads is to the true (unobserved) sum
*     that we wish to estimate.
*
*     SEP includes 3 components:
*
*        1) variance of the estimated sum (parameter uncertainty)
*        2) variability associated with random error around model
*        3) covariance of random errors between days (this component
*           is missing)
*
*     Notes: This is a crippled version of what could be a much richer
*     approach.  However, fitting a real time series model would
*     require much more frequent sampling than is available.  Thus it
*     is assumed that correlation between errors within days is 100%
*     and between days it is zero.  This is not as far off as it may
*     seem -- see Tim Cohn's working papers [1989].
*
*     If we ever figure out how to estimate RHO, then this routine
*     will need to be updated. (Tim Cohn 01/24/2002). Modified
*     21 JANUARY 2002 by T.Cohn.
*
************************************************************************
      SUBROUTINE LOADSEP(NUMOBSE,PLDAML,S2,XLOADVAR,SEP)
*
*     subroutine args
*
      INTEGER*4 NUMOBSE
      DOUBLE PRECISION S2,XLOADVAR,SEP,PLDAML(*)
*
*     local vars
*
      INTEGER*4 I
*
*     start with the variance of predictions
*  
      SEP  =  XLOADVAR   
*
*     add variability around individual predictions.  See Tim Cohn's
*     ESTIMATOR code (from Jan 2002) for an inner loop that adds
*     correlation of random variability between predictions. This loop
*     is ommitted here as "we lack the data to fit a larger AR model"
*
      DO 10 I=1,NUMOBSE
          SEP = SEP + PLDAML(I)**2 * (EXP(S2)-1.D0)   
 10   CONTINUE
      SEP  = SQRT(SEP)/DBLE(NUMOBSE)

      RETURN
      END 

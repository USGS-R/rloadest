************************************************************************
*
*     Function EXPVAR                               Called by: TAC_LOAD
*
*     Estimate covariance of two EXPON estimators of the form
*     EXPON(SIGMA^2,A,B), assuming that SIGMA^2 is a
*     GAMMA(ALPHA,S2*BETA) random variable (where BETA is a parameter
*     = 2/(N-1) in standard model)
*
*     result is exact aside from machine error
*
*     S2        sample estimate of SIGMA^2
*     ALPHA     parameter of gamma distribution = (N-1)/2 in std.model
*     A1        1st coefficient in front of SIGMA^2
*     B1        1st coefficient in front of SIGMA
*     A2        2nd coefficient in front of SIGMA^2
*     B2        2nd coefficient in front of SIGMA
*
************************************************************************
      DOUBLE PRECISION FUNCTION EXPVAR(S2,ALPHA,A1,B1,A2,B2)
*
*     subroutine args
*
      DOUBLE PRECISION S2,ALPHA,A1,B1,A2,B2
*
*     local vars
*
      INTEGER*4 I,J,N
      DOUBLE PRECISION ARG,A,SIGMA,TOL,TSUM,DELTA
      DOUBLE PRECISION C1(0:1000),C2(0:1000),GAMM(0:1000)
      DATA TOL/1.D-6/
*
*     function declaration
*
      DOUBLE PRECISION DLNGAM
*
*     
*
      SIGMA = SQRT(S2)
  
      ARG = MAX(ABS(A1)*S2+ABS(B1)*SIGMA,ABS(A2)*S2+ABS(B2)*SIGMA)
      A=MAX(ABS(A1),ABS(A2))
      N = MIN(998.D0,
     &        30.D0+ARG*(5.4-0.2*ARG+A
     &        *(8.28-1.11*A+1.98*ARG)+1.33*(MAX(ABS(B1),ABS(B2)))))
  
 10   EXPVAR = 0.D0
      CALL SQUARE(N,A1,B1,C1)
      CALL SQUARE(N,A2,B2,C2)
  
      GAMM(0) = 1.D0
      GAMM(1) = EXP(DLNGAM(ALPHA+0.5D0)-DLNGAM(ALPHA))
      DO 30 J=0,N
         GAMM(J+2) = (ALPHA+0.5D0*J)*GAMM(J)
         TSUM = 0.D0
         DO 20 I=0,J
            TSUM = TSUM+C1(I)*C2(J-I)*GAMM(J)/(GAMM(I)*GAMM(J-I))
 20      CONTINUE
         DELTA = SIGMA**J*TSUM
         EXPVAR = EXPVAR+DELTA
 30   CONTINUE
      IF (DELTA.LE.TOL*EXPVAR) RETURN

 40   N = N+10
      IF (N.LT.998) GOTO 10
*
*     Error: failure to converge in 998 terms
*
      EXPVAR=0.D0

      RETURN
      END

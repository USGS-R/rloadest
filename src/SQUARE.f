************************************************************************
*
*     Subroutine SQUARE                      Called by: EXPON, EXPVAR
*
*     compute the first N coefficients, C(K), such that:
*
*         SUM(C(K)*X**K,K,0,INF)  =  EXP(A*X**2+B*X)
*
*     Result is exact aside from machine error
*
*     A         coefficient in front of SIGMA^2
*     B         coefficient in front of SIGMA
* 
*     warning: C(K) is not set to zero for K>N
*
************************************************************************
      SUBROUTINE SQUARE(N,A,B,C)
*
*     subroutine args
*
      INTEGER*4 N
      DOUBLE PRECISION A,B,C(0:1000)
*
*     local vars
*
      INTEGER*4 K,L
      DOUBLE PRECISION TOL1,TOLMIN,T1,T2,T0(0:1000)
      DATA TOL1/1.D-15/
*
*     
*
      IF (ABS(A).GT.TOL1) THEN
         IF (ABS(B).GT.TOL1) THEN
*
*           Case I:  both A and B are non-zero
*
            T0(0) = 1.D0
            TOLMIN = 0.D0
            T2 = B**2/A
            DO 30 K=0,N
               C(K) = 0.D0
               T1 = T0(K)
               DO 10 L=K/2,0,-1
                  C(K) = C(K)+T1
                  T1 = T1*T2*L/((K-2.D0*L+2.D0)*(K-2.D0*L+1.D0))
                  IF (ABS(T1).LT.TOLMIN) GOTO 20
 10            CONTINUE
 20            IF (MOD(K,2).EQ.0) THEN
                  T0(K+1) = T0(K)*B
                  T0(K+2) = T0(K)*A/(0.5D0*(K+2.D0))
                  IF (K.GT.1) TOLMIN = TOL1*MIN(ABS(C(K)),ABS(C(K-1)))
               ENDIF
 30         CONTINUE
         ELSE
*
*           Case II:  A is non-zero; B is zero
*
            C(0) = 1.D0
            DO 40 K=0,N,2
               C(K+1) = 0.D0
               C(K+2) = C(K)*A/(0.5D0*(K+2.D0))
 40         CONTINUE
         ENDIF
      ELSE
         IF (ABS(B).GT.0.D0) THEN
*
*           Case III:  B is non-zero; A is zero
*
            C(0) = 1.D0
            DO 50 K=0,N
               C(K+1) = C(K)*B/(K+1.D0)
 50         CONTINUE
         ELSE
*
*           Case IV:  both A and B are zero--pretty silly
*
            C(0) = 1.D0
            DO 60 K=1,N
               C(K) = 0.D0
 60         CONTINUE
         ENDIF
      ENDIF
      RETURN
      END

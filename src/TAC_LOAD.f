************************************************************************
*
*     Subroutine TAC_LOAD           Called by: AMLLOAD2, TACIT_LOADS_NC
*
*     compute nearly-unbiased AMLE load estimates
*
*     Notes: The code assumes the parameter values have been estimated
*     from data.  It adjusts the incoming MLE estimates to the AMLE
*     estimates before computing the variances. (If this is used in a
*     Monte Carlo context, the true parameter values should be used
*     uncorrected for bias.)
*
************************************************************************
      SUBROUTINE TAC_LOAD(NPAR,PARMLE,BIAS,CV,SBIAS,SCV,NUM,XLEST,MVUE,
     &                    XLOADSUM,XLOADVAR)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine args
*
      INTEGER*4 NPAR,NUM
      DOUBLE PRECISION XLOADSUM,XLOADVAR
      DOUBLE PRECISION PARMLE(*),BIAS(*),SBIAS(*),MVUE(*)
      DOUBLE PRECISION XLEST(MAXOBSE,*),CV(MAXPARMS+1,*),
     &                 SCV(MAXPARMS+1,*)
*
*     local vars
*
      INTEGER*4 I,I2,K,K2
      DOUBLE PRECISION KAPPA,ALPHA,S2,AT,XCX,XOMEGA,S2_U,XXBSB,XXB,
     &                 COVAR,XXCXX
      DOUBLE PRECISION GAMMA(MAXPARMS),B(MAXPARMS),OMEGA(MAXPARMS),
     &                 XC(MAXPARMS),XXC(MAXPARMS),A1(MAXOBSE),
     &                 B1(MAXOBSE),XX(MAXPARMS)
      DOUBLE PRECISION C(MAXPARMS,MAXPARMS)
*
*     function declarations 
*
      DOUBLE PRECISION EXPON,EXPVAR
*
*     define parameters and OMEGA
*
      S2 = PARMLE(NPAR+1)
      DO 10 I=1,NPAR
         GAMMA(I) = SCV(I,NPAR+1)/SCV(NPAR+1,NPAR+1)
         OMEGA(I) = PARMLE(I)-GAMMA(I)*SQRT(S2)
         B(I) = SBIAS(I)-GAMMA(I)*(1.D0+SBIAS(NPAR+1))
 10   CONTINUE
      DO 30 I=1,NPAR
         DO 20 K=1,NPAR
            C(I,K) = SCV(I,K)-SCV(NPAR+1,NPAR+1)*GAMMA(I)*GAMMA(K)
 20      CONTINUE
 30   CONTINUE
      ALPHA = (1.D0+BIAS(NPAR+1))**2/CV(NPAR+1,NPAR+1)
      KAPPA = (1.D0+BIAS(NPAR+1))/ALPHA
*
*     compute individual loads
*
      DO 90 I=1,NUM
         AT = 0.D0
         DO 40 K=1,NPAR
            AT = AT+XLEST(I,K)*B(K)
 40      CONTINUE
         A1(I) = -AT
         DO 60 I2=1,NPAR
            XC(I2) = 0.D0
            DO 50 K=1,NPAR
               XC(I2) = XC(I2)+XLEST(I,K)*C(K,I2)
 50         CONTINUE
 60      CONTINUE
         XCX = 0.D0
         DO 70 K=1,NPAR
            XCX = XCX+XC(K)*XLEST(I,K)
 70      CONTINUE
         B1(I) = (1.D0-XCX)/2.D0
         XOMEGA = 0.D0
         DO 80 K=1,NPAR
            XOMEGA = XOMEGA+XLEST(I,K)*OMEGA(K)
 80      CONTINUE
         MVUE(I) = EXP(XOMEGA)*EXPON(S2,ALPHA,KAPPA,B1(I),A1(I))
 90   CONTINUE
*
*     compute the sum of loads, and their variance; use AMLE estimate
*     for S^2, in place of true value
*
      S2_U = S2/(1.D0+BIAS(NPAR+1))
      XLOADSUM = 0.D0
      XLOADVAR = 0.D0
      DO 150 I=1,NUM
         XLOADSUM = XLOADSUM+MVUE(I)
         DO 140 K=1,I
            XXBSB = 0.D0
            XXB = 0.D0
            DO 100 I2=1,NPAR
               XX(I2) = XLEST(I,I2)+XLEST(K,I2)
               XXBSB = XXBSB+XX(I2)*(PARMLE(I2)+SQRT(S2_U)*B(I2))
               XXB = XXB+XX(I2)*PARMLE(I2)
 100        CONTINUE
            DO 120 I2=1,NPAR
               XXC(I2) = 0.D0
               DO 110 K2=1,NPAR
                  XXC(I2) = XXC(I2)+XX(K2)*C(K2,I2)
 110           CONTINUE
 120        CONTINUE
            XXCXX = 0.D0
            DO 130 K2=1,NPAR
               XXCXX = XXCXX+XXC(K2)*XX(K2)
 130        CONTINUE
            COVAR = EXP(XXBSB+XXCXX*S2_U/2.D0)
     &             * EXPVAR(S2_U,ALPHA,B1(I),A1(I),B1(K),A1(K))
     &             - EXP(XXB+S2_U)
            IF (I.EQ.K) THEN
               XLOADVAR = XLOADVAR+COVAR
            ELSE
               XLOADVAR = XLOADVAR+2.D0*COVAR
            ENDIF
 140     CONTINUE
 150  CONTINUE
  
       RETURN
       END

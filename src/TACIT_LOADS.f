************************************************************************
*
*     Subroutine TACIT_LOADS_NC                    Called by: AMLLOAD2
*
*     calculate the variance using a linear approximation
*
*     MVUE      estimated loads
*     XLOADSUM  sum of estimated loads
*     XLOADVAR  approximate variance of sum of estimated loads
*
************************************************************************
      SUBROUTINE TACIT_LOADS_NC(NPAR,NUMOBSE,XLEST,MVUE,XLOADSUM,
     &                          XLOADVAR,B_MLE,BIAS,SBIAS,CV,SCV,CV_M)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine args
*
      INTEGER*4 NPAR,NUMOBSE
      DOUBLE PRECISION XLOADSUM,XLOADVAR
      DOUBLE PRECISION MVUE(*),B_MLE(*),BIAS(*),SBIAS(*)
      DOUBLE PRECISION XLEST(MAXOBSE,*),CV(MAXPARMS+1,*),
     &                 SCV(MAXPARMS+1,*),CV_M(MAXPARMS+1,*)
*
*     local vars
*
      INTEGER*4 I,K
      DOUBLE PRECISION XTOT,XVARTOT
      DOUBLE PRECISION XL(MAXPARMS+1),CVXL(MAXPARMS+1)
*
*
*
      XLOADSUM = 0.D0
      DO 10 I=1,NUMOBSE
         CALL TAC_LOAD(NPAR,B_MLE,BIAS,CV,SBIAS,SCV,1,XLEST(I,1),
     &                 MVUE(I),XTOT,XVARTOT)
         XLOADSUM = XLOADSUM + XTOT
 10   CONTINUE

      DO 20 K=1,NPAR+1
         XL(K) = 0.D0
 20   CONTINUE

      DO 40 I=1,NUMOBSE
         XL(NPAR+1) = XL(NPAR+1)+MVUE(I)/2.D0
         DO 30 K=1,NPAR
            XL(K) = XL(K)+XLEST(I,K)*MVUE(I)
 30      CONTINUE
 40   CONTINUE
*
*     CV_M x XL = CVXL
*
      DO 60 I=1,NPAR+1
         CVXL(I) = 0.D0
         DO 50 K=1,NPAR+1
            CVXL(I) = CVXL(I) + CV_M(I,K)*XL(K)
 50      CONTINUE
 60   CONTINUE
*
*     XL*CVXL
*
      XLOADVAR  =  0.D0
      DO 70 K=1,NPAR+1
         XLOADVAR = XLOADVAR + XL(K)*CVXL(K)
 70   CONTINUE

      RETURN
      END

************************************************************************
*
*     Function DLNGAM                          Called by: EXPON, EXPVAR
*
*     compute logarithm of the GAMMA function; valid only for XX>0
*
************************************************************************
      DOUBLE PRECISION FUNCTION DLNGAM(XX)
*
*     subroutine args
*
      DOUBLE PRECISION XX
*
*     local vars
*
      INTEGER*4 J
      DOUBLE PRECISION SER,COF(6)
      
      DATA COF /76.18009173D0,-86.50532033D0,24.01409822D0,
     &          -1.231739516D0,1.20858003D-3,-5.36382D-6/
*
*     
*
      SER = 1.D0
      DO 10 J=1,6
         SER = SER+COF(J)/(XX+DBLE(J-1))
 10   CONTINUE

      DLNGAM = ((XX-0.5D0)*LOG(XX+4.5D0)-(XX+4.5D0))
     &         + LOG(2.50662827465D0*SER)

      RETURN
      END

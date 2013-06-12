************************************************************************
*
*     Subroutine LOAD95CI                           Called by: AMLLOAD2
*
*     compute 95% confidence intervals
*
************************************************************************
      SUBROUTINE LOAD95CI(LOADAML,SEP,LOW95,UP95)
*
*     subroutine args
*
      DOUBLE PRECISION LOADAML,SEP,LOW95,UP95
*
*     local vars
*
      DOUBLE PRECISION A,B
*
*     compute 95% confidence intervals
*
      B = SQRT(LOG(1+(SEP/LOADAML)**2))
      A = LOG(LOADAML)-B**2/2.D0
      LOW95 = EXP(A-1.96*B)
      UP95 = EXP(A+1.96*B)
  
      RETURN
      END

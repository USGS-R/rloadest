# dialog support functions for S-LOADEST.
#    Prepare data (calibration or estimation) for input to FORTRAN routines
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004Oct29 DLLorenz modifications for t*
#    2004Nov10 DLLorenz Modified for quadratic time
#    2004Nov16 DLLorenz Modified for use anomalies
#    2004Nov16          This version.
#
loadestSetXLDat <- function(temp.df, sname, j, Qadj, Tadj, flowtrans, floworder,
                            lineartime=0, seasonal, period,
                            additional.terms = "<None>", diurnal=F) {
  ## set up output
  ## The contents of sname controls how the data are extracted.
  ## If sname[1] is blank, then only the explanatory data are set.
  ## If the length of sname is 1, then extraction is based on the standard
  ## S-LOADEST naming convention. Otherwise, sname contains the names of
  ## the columns for the remarks, load, detection, flow, and dectime
  out <- list()
  if(sname[1] != "") {
    ## extract the data frame to make a predefined model
    ## the response variable:
    if(length(sname) == 1) { # standard S-LOADEST
      out$censflag <- temp.df[[paste("R", sname, sep="")]] == "<"
      out$ylcal <- log(temp.df[[paste("L", sname, sep="")]])
      out$yd <- log(temp.df[[paste("D", sname, sep="")]])
      Flow <- "FLOW"
      Dectime <-  "DECTIME"
      Date <- "DATES"
      Time <- "TIMES"
    } else {
      out$censflag <- temp.df[[sname[1]]] == "<"
      out$ylcal <- log(temp.df[[sname[2]]])
      out$yd <- log(temp.df[[sname[3]]])
      Flow <- sname[4]
      Dectime <- sname[5]
      Date <- sname[6]
      Time <- sname[7]
    }
  } else {  # end of sname[1] found
    ## make the dataframe of the prediction data
    ## the first column is julian date
    if(length(sname) == 1) { # standard S-LOADEST
#       Months <- as.double(months(temp.df$DATES))
#       Days <- as.double(days(temp.df$DATES))
#       out$dectime <- julian(Months, Days, 1960)
      out$dectime <- julian(temp.df$DATES,origin=as.Date("1960-01-01"))
      Flow <- "FLOW"
      Dectime <-  "DECTIME"
      Date <- "DATES"
      Time <- "TIMES"
    } else {
      Date <- sname[4]
#       Months <- as.double(months(temp.df[[Date]]))
#       Days <- as.double(days(temp.df[[Date]]))
#       out$dectime <- julian(Months, Days, 1960)
      out$dectime <- julian(temp.df[[Date]],origin=as.Date("1960-01-01"))
      Flow <- sname[2]
      Dectime <-  sname[3]
      Time <- sname[5]
    }
  } # end of sname[1] not found--prediction data
  if(j > 0) {
    lnQ <- log(temp.df[[Flow]]/Qadj)
    xlcal <- cbind(rep(1.0,length(lnQ)), lnQ)
    NPAR <- 2
    if(is.element(j,(c(2,5,6,8,9)))) { # has second order flow term
      xlcal <- cbind(xlcal, lnQ2 = lnQ^2)
      NPAR <- NPAR + 1
    }
    DECTIME <- temp.df[[Dectime]] - Tadj[1]
    if(is.element(j,(c(3,5,7,8,9)))) { # has linear time term
      xlcal <- cbind(xlcal, DECTIME=DECTIME - Tadj[2])
      NPAR <- NPAR + 1
    }
    if(j == 9) { # has second order time term
      DECTIME2 <- (DECTIME - Tadj[2])^2
      xlcal <- cbind(xlcal, DECTIME2=DECTIME2)
      NPAR <- NPAR + 1
    }
    if(is.element(j,(c(4,6,7,8,9)))) { # has sinusoidal terms
      xlcal <- cbind(xlcal, sin.DECTIME = sin(DECTIME * 2 * pi),
                     cos.DECTIME = cos(DECTIME * 2 * pi))
      NPAR <- NPAR + 2
    }
  } else { # user defined model
    if(floworder != "use anomalies") { # use transformed flow as explanatory variable
      if(flowtrans == "log") {
        lnQ <- log(temp.df[[Flow]]/Qadj)
        xlcal <- cbind(rep(1.,length(lnQ)), lnQ)
        NPAR <- 2
        lFlow <- T
      }
      else { # must be reciprocal
        recipQ <- 1 - Qadj/temp.df[[Flow]]
        xlcal <- cbind(rep(1.,length(recipQ)), recipQ)
        NPAR <- 2
        lFlow <- F
      }
      if(floworder == "quadratic") {
        if(lFlow) {
          xlcal <- cbind(xlcal, lnQ2 = lnQ^2)
          NPAR <- NPAR + 1
        }
        else { # need not worry about anything else because can not be piecewise
          xlcal <- cbind(xlcal, recipQ2 = recipQ^2)
          NPAR <- NPAR + 1
        }
      } # bracket not necessary, but makes the if more readable
      if(floworder == "one breakpoint") {
        if(seasonal == "period")
          guiDisplayMessageBox("Piecewise regression not valid with period.\nUsing linear.",
                               button = "Ok", icon = "warning")
        else {
          if(lFlow)
            Bp <- try(loadestSelBreakpoint(lnQ, out$ylcal))
          else
            Bp <- try(loadestSelBreakpoint(recipQ, out$ylcal))
          if(class(Bp) != "Error") { # we were successful
            if(lFlow)
              BpQ <- ifelse(lnQ > Bp, lnQ - Bp, 0)
            else
              BpQ <- ifelse(recipQ > Bp, recipQ - Bp, 0)
            xlcal <- cbind(xlcal, BpQ = BpQ)
            NPAR <- NPAR + 1
            assign("loadest.breakpoint", Bp, where=1) # save it
          }
          else # breakpoint regression failed.
            guiDisplayMessageBox("Piecewise regression failed.\nUsing linear.",
                                 button = "Ok", icon = "warning")
        } # end of else (period not selected for season
      } # end of single breakpoint regression
      if(floworder == "two breakpoints") {
        if(seasonal == "period")
          guiDisplayMessageBox("Piecewise regression not valid with period.\nUsing linear.",
                               button = "Ok", icon = "warning")
        else {
          if(lFlow)
            Bp <- try(loadestSelBreakpoint(lnQ, out$ylcal, nbreak=2))
          else
            Bp <- try(loadestSelBreakpoint(recipQ, out$ylcal, nbreak=2))
          if(class(Bp) != "Error") { # we were successful
            if(lFlow) {
              BhiQ <- ifelse(lnQ > Bp[1], lnQ - Bp[1], 0)
              BloQ <- ifelse(lnQ < Bp[2], Bp[2] - lnQ, 0)
            }
            else {
              BhiQ <- ifelse(recipQ > Bp[1], recipQ - Bp[1], 0)
              BloQ <- ifelse(recipQ < Bp[2], Bp[2] - recipQ, 0)
            }
            xlcal <- cbind(xlcal, BhiQ = BhiQ, BloQ = BloQ)
            NPAR <- NPAR + 2
            assign("loadest.breakpoint", Bp, where=1) # save it
          }
          else # breakpoint regression failed.
            guiDisplayMessageBox("Piecewise regression failed.\nUsing linear.",
                                 button = "Ok", icon = "warning")
        } # end of else (period not selected for season
      } # end of double breakpoint regression
      if(substring(floworder,1,9) == "piecewise") { # piecewise succeeded
        terms <- unlist(unpaste(floworder,sep=","))
        if(length(terms) == 2) { # single breakpoint
          Bp <- as.double(terms[2])
          if(lFlow)
            BpQ <- ifelse(lnQ > Bp, lnQ - Bp, 0)
          else
            BpQ <- ifelse(recipQ > Bp, recipQ - Bp, 0)
          xlcal <- cbind(xlcal, BpQ = BpQ)
          NPAR <- NPAR + 1
        }
        else { # must be double breakpoint
          BhiQ <- as.double(terms[2])
          BloQ <- as.double(terms[3])
          if(lFlow) {
            BhiQ <- ifelse(lnQ > BhiQ, lnQ - BhiQ, 0)
            BloQ <- ifelse(lnQ < BloQ, BloQ - lnQ, 0)
          }
          else {
            BhiQ <- ifelse(recipQ > BhiQ, recipQ - BhiQ, 0)
            BloQ <- ifelse(recipQ < BloQ, BloQ - recipQ, 0)
          }
          xlcal <- cbind(xlcal, BhiQ = BhiQ, BloQ = BloQ)
          NPAR <- NPAR + 2
        }
      } # done with piecewise
    } # end of if not use anomalies
    else { # must set xlcal
      xlcal <- matrix(1., nrow=nrow(temp.df), ncol=1)
      NPAR = 1
    }
    DECTIME <- temp.df[[Dectime]] - Tadj[1]
    if(lineartime > 0) {
      xlcal <- cbind(xlcal, DECTIME=DECTIME - Tadj[2])
      NPAR <- NPAR + 1
    }
    if(lineartime > 1) {
      xlcal <- cbind(xlcal, DECTIME2=(DECTIME - Tadj[2])^2)
      NPAR <- NPAR + 1
    }
    if(seasonal == "sinusoidal") {
      xlcal <- cbind(xlcal, sin.DECTIME = sin(DECTIME * 2 * pi),
                     cos.DECTIME = cos(DECTIME * 2 * pi))
      NPAR <- NPAR + 2
    }
    if(seasonal == "period") {
      months.target <- unlist(unpaste(period, sep = ","))
      months.sample <- as.character(months(temp.df[[Date]], abb=F))
      period <- is.element(months.sample, months.target)
      if(lFlow) {
        xlcal <- cbind(xlcal, period = period, period.Q = period * lnQ)
        NPAR <- NPAR + 2
      }
      else {
        xlcal <- cbind(xlcal, period = period, period.Q = period * recipQ)
        NPAR <- NPAR + 2
      }
      if(floworder == "quadratic") {
        if(lFlow) {
          xlcal <- cbind(xlcal, period.Q2 = period * lnQ^2)
          NPAR <- NPAR + 2
        }
        else { # need not worry about anything else because can not be piecewise
          xlcal <- cbind(xlcal, period.Q2 = period * recipQ^2)
          NPAR <- NPAR + 2
        }
      } # bracket not necessary, but makes the if more readable
    } # end of period
    if(additional.terms != "<None>") {
      Terms <- unlist(unpaste(additional.terms, sep = ","))
      for(i in Terms) {
        xlcal <- cbind(xlcal, temp.df[[i]])
        NPAR <- NPAR + 1
        dimnames(xlcal)[[2]][NPAR] <- i
      }
    }
    if(diurnal) {
      TIMES <- as.double(substring(temp.df[[Time]],1,2))/24 +
        as.double(substring(temp.df[[Time]],3,4))/1440
      xlcal <- cbind(xlcal, sin.TIMES = sin(TIMES * 2 * pi),
                   cos.TIMES = cos(TIMES * 2 * pi))
      NPAR <- NPAR + 2
    }
  } # end of user defined model
  out$xlcal <- xlcal
  out$NPAR <- NPAR
  return(out)
}

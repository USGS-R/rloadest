#' Model Matrix
#'
#' Internal support function to extract the model matrix for
#'one of the 9 predefined models in LOADEST.
#'
#' @param data the dataset containing dates and flow columns.
#' @param flow character string indicating the name of the 
#'flow column.
#' @param dates character string indicating the name of the 
#'date column.
#' @param Qadj the centering value for flow.
#' @param Tadj the centering value for decimal time.
#' @param model.no the model number, must be in the range 1-9.
#'
#' @return The model matrix corresponding to the selected model
#'number.
#'
setXLDat <- function(data, flow, dates, Qadj, Tadj, model.no) {
  ## Coding history:
  ##    2013May31 DLLorenz Original version based on S+ library
  ##
  ## Error check
  if(model.no < 1 || model.no > 9)
    stop("Model number not in range 1-9")
  lnQ <- log(data[[flow]]/Qadj)
  xlcal <- cbind("(Intercept)"=rep(1.0,length(lnQ)), lnQ=lnQ)
  NPAR <- 2
  if(is.element(model.no,(c(2,5,6,8,9)))) { # Has second order flow term
    xlcal <- cbind(xlcal, lnQ2 = lnQ^2)
    NPAR <- NPAR + 1
  }
  if(class(data[[dates]])[1] == "Date")
    DECTIME <- dectime(data[[dates]] + 0.5) - Tadj[1] # Adjust for noon
  else
    DECTIME <- dectime(data[[dates]]) - Tadj[1]
  if(is.element(model.no,(c(3,5,7,8,9)))) { # Has linear time term
    xlcal <- cbind(xlcal, DECTIME=DECTIME - Tadj[2])
    NPAR <- NPAR + 1
  }
  if(model.no == 9) { # Has second order time term
    DECTIME2 <- (DECTIME - Tadj[2])^2
    xlcal <- cbind(xlcal, DECTIME2=DECTIME2)
    NPAR <- NPAR + 1
  }
  if(is.element(model.no,(c(4,6,7,8,9)))) { # Has sinusoidal terms
    xlcal <- cbind(xlcal, sin.DECTIME = sin(DECTIME * 2 * pi),
      cos.DECTIME = cos(DECTIME * 2 * pi))
    NPAR <- NPAR + 2
  }
  return(xlcal)
}

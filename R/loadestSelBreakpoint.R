# dialog support functions for S-LOADEST.
#    Select one or two breakpoints for piecewise linear regression
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004Jan28          This version.
#
loadestSelBreakpoint <- function(x, y, nbreak=1, plotit=T) {
  if(nbreak == 1) { # single breakpoint
    Bp <- median(x)
    b0 <- median(y)
    b1 <- (b0 - min(y))/(Bp - min(x))
    b2 <- (max(y) - b0)/(max(x) - Bp) - b1
    nls.out <- nls(Y ~ b0 + b1 * X + b2 * (X - Bp)*(pnorm(X-Bp,0,0.01)), 
                   data=data.frame(Y=y,X=x),start=c(Bp=Bp, b0=b0, b1=b1, b2=b2))
    cat(" Selected Breakpoint information:\n")
    print(summary(nls.out)$parameters[1,])
    if(plotit) {
      guiClose("GraphSheet", "Single Breakpoint")
      graphsheet(Name="Single Breakpoint")
      plot(x,y, xlab="transformed FLOW", ylab="log of FLUX")
      x <- sort(x)
      y <- sort(fitted.values(nls.out))
      lines(x, y, col = 3)
    }
    return(nls.out$parameters[1])
  }
  else { #nbreak can only be 1 or 2, so must be 2
    ## need to remove the names because the quantile function attaches names
    ## to its output.
    Bplow <- quantile(x,.33)
    names(Bplow) <- NULL
    Bphi <- quantile(x,.67)
    names(Bphi) <- NULL
    b0 <- median(y)
    names(b0) <- NULL
    b1 <- (quantile(y,.67) - quantile(y,.33))/(Bphi - Bplow)
    names(b1) <- NULL
    b2 <- (max(y) - quantile(y,.67))/(max(x) - Bphi) - b1
    names(b2) <- NULL
    b3 <- b1 - (quantile(y,.33) - min(y))/(Bplow - min(x))
    names(b3) <- NULL
    nls.out <- nls(Y ~ b0 + b1 * X + b2 * (X - Bphi)*(pnorm(X-Bphi,0,0.01)) +
                   b3 * (Bplow - X) * (pnorm(Bplow - X,0,0.01)),
                   data=data.frame(Y=y,X=x),
                   start=c(Bphi=Bphi, Bplow=Bplow, b0=b0, b1=b1, b2=b2, b3=b3))
    cat(" Selected Breakpoint information:\n")
    print(summary(nls.out)$parameters[1:2,])
    if(plotit) {
      guiClose("GraphSheet", "Double Breakpoint")
      graphsheet(Name="Double Breakpoint")
      plot(x,y, xlab="transformed FLOW", ylab="log of FLUX")
      x <- sort(x)
      y <- sort(fitted.values(nls.out))
      lines(x, y, col = 3)
    }
    return(nls.out$parameters[1:2])
  }
}

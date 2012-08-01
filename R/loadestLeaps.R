# dialog support functions for S-LOADEST.
#    Select the 5 "best" candidate models using the additional variables
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004Jan28          This version.
#
loadestLeaps <- function(station, constituentlist)
{
  if(missing(station)) {
    cat(" *** The station must be selected. ***\n")
    return()
  }
  if(!exists("loadest.expvars", where=1)) {
    cat(" *** No additional explanatory variables were selected. ***\n")
    return()
  }
  ## get the data file
  loadest.df <- get("loadest.df", where=1)
  ## get the list of constituents
  constituent.objs <- dimnames(loadest.df)$snames
  ## get the selected constituents, and create a vector of stations to choose
  if(missing(constituentlist))
    constituent.list <- constituent.objs
  else
    constituent.list <- unlist(unpaste(constituentlist,sep=','))
  constituents.sel <- charmatch(constituent.list, constituent.objs)
  ## get the additional explanatory variables
  loadest.expvars <- get ("loadest.expvars", where=1)
  loadest.expvars <- unlist(unpaste(loadest.expvars,sep=','))
  ## OK, now do the leaps regression
  cat("Station: ", station,"\n",sep="")
  for(i in constituents.sel) {
    temp.df <- loadest.df[station,i][[1]]
    sname <- constituent.objs[i]
    cat("  Best 5 candidate models for constituent: ", sname, "\n", sep="")
    constituent.name <- paste("L", sname, sep="")
    y <- log(temp.df[[constituent.name]])
    x <- cbind(poly(log(temp.df$FLOW),2), temp.df[,loadest.expvars])
    ## make sure that no explanatory variables have missing values
    good.x <- apply(x, 1, FUN= function(x) !any(is.na(x)))
    x <- x[good.x,]
    y <- y[good.x]
    leaps.out <- leaps(x,y,names=c("logFLOW", "logFLOW2", loadest.expvars))
    leaps.df <- data.frame(Model=leaps.out$label, Cp=leaps.out$Cp)
    print(leaps.df[order(leaps.df$Cp)[1:5],])
    cat("\n")
  }
  cat("Note, these models do not include time terms.\n")
  cat("Trend, seasonality, and daily terms might also be important.\n")
  cat("The 'best' model may not be any of these candidates.\n\n")
  return()
}

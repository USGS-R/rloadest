# User-defined variables:
## Named vector of USGS station IDs to retrieve, name
## used only for documentation here
Staids <- c("St. Louis River at Scanlon" = "04024000")
## Parameter codes to retrieve for load estimation
Params <- c("00631")
## Stating and ending dates
Start <- "2009-10-01"
End <- "2012-09-30"
## Aggregate Loads by what? and the output data frame suffix
Loads <- "water year"
ld.sfx <- "wy"
## Load Units?
ld.unt <- "kg"
# ** End of user input **

# Set the required libraries
library(dataRetrieval)
library(rloadest)

# Loop for each station
for(sta in Staids) {
  # Set names for R objects for each station
  ## The flow data
  Qdata <- paste("Q", sta, sep="")
  ## The water-quality data
  QWdata <- paste("QW", sta, sep="")
  # Get the flow and water-quality data--use temporary dataset
  # names that can be overwritten to manipulate and then save
  # in the names provided above.
  Qtmp <- renameNWISColumns(readNWISdv(sta, "00060", startDate=Start, endDate=End))
  QWtmp <- importNWISqw(sta, params=Params, begin.date=Start, 
                        end.date=End, use.pnames=TRUE)
  # Make sure that the recorded values are daily averages
  QWtmp <- dailyAg(QWtmp)
  # Change the names of dates and times. Note if the order
  # of the columns changes in the retrieval, then this needs to
  # be revised, use the name in the flow data.
  names(QWtmp)[2:3] <- c("Date", "TIMES")
  # Merge the flow data into the water-quality data
  QWtmp <- mergeQ(QWtmp, FLOW="Flow", DATES="Date", 
                  Qdata=Qtmp, Plot=FALSE)
  # Save the data
  assign(Qdata, Qtmp)
  assign(QWdata, QWtmp)
  # OK now loop through each parameter and build model
  for(prm in Params) {
    # Set name for output object, pdf file base name, and load
    LR <- paste("LR", sta, prm, sep=".")
    LD <- paste("LD", sta, prm, ld.sfx, sep=".")
    # Because regression models expect the names of the
    # objects instead of the objects, one must construct the
    # call and evaluate it. But selBestModel can be executed
    # directly. Note that calls directly to loadReg should
    # use the actual name of the water-quality data rather
    # than the temporary dataset.
    assign(LR, selBestModel(paste("P", prm, sep=""), QWtmp, flow="Flow",
                 dates="Date", load.units=ld.unt,
                 station=sta))
    # Protect against errors
    if(get(LR)$lfit$IERR > 0) {
      cat("Load error:", sta, prm, "\n", sep=" ")
    } else {
      # Create the pdf report
      loadReport(get(LR), LR)
      # Estimate the loads, accept after validating the model
      assign(LD, predLoad(get(LR), Qtmp, by=Loads))
    }
  }
}
# Clean up the temporary datasets
rm(Qtmp, QWtmp)

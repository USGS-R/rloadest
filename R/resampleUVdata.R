#' @title Resample Unit-Value Data
#' 
#' @description Unit-value data can be recorded at any arbitrary intervals. For some 
#'applications, such as load estimates, a uniform series is required. The 
#'\code{resampleUVdata} function resamples the orginal unit-value data to a consistent 
#'time interval.
#' 
#' @param UVdata the dataset containing the unit-values data. Must have one column that
#'represents the time of the observation that is class "POSIXt." Missing values are not
#'permitted in that column.
#' @param time.step the time step of the new data in minutes; must divide an hour
#'exactly evenly. The default value is 15 minutes.
#' @param start.date a character string indicating the first day of the output dataset. The
#'default value ("") indicates use the first day in \code{UVdata}.
#' @param end.date a character string indicating the last day of the output dataset. The
#'default value ("") indicates use the last day in \code{UVdata}.
#' @param max.diff a character string indicating the maximum difference in time to 
#'sucessfully resample the unit-value data. The default is "2 hours" see
#'\code{\link{mergeNearest}} for details.
#' @return A data frame like \code{UVdata} but having a uniform time step.
#' 
#' @export
resampleUVdata <- function(UVdata, time.step=15, start.date="", end.date="",
													 max.diff="2 hours") {
	# Verify that one column of class POSIXt is in UVdata
	DateTime <- names(UVdata)[sapply(UVdata, function(x) inherits(x, "POSIXt"))]
	if(length(DateTime) > 1L)
		stop("Multiple datetime columns in UVdata")
	if(length(DateTime) < 1L)
		stop("No datetime columns in UVdata")
	# Verify that time step divides an hour
	if((60 %% time.step) != 0)
		stop("Invalid time step: ", as.character(time.step))
	# OK make a difftime and get timezone info
	ts.dt <- as.difftime(time.step, unit="mins")
	tz <- attr(UVdata[[DateTime]], "tzone")
	# Set start/end times; requires lubridate
	if(start.date == "") {
		start.date <- floor_date(UVdata[[DateTime]][1L], unit="day")
	} else {
		start.date <- as.POSIXct(start.date, tz=tz)
	}
	if(end.date == "") {
		end.date <- ceiling_date(UVdata[[DateTime]][nrow(UVdata)], unit="day")
	} else {
		end.date <- as.POSIXct(end.date, tz=tz) + days(1) # Must round up
	}
	end.date <- end.date - ts.dt
	## OK, we are ready to go.
	# Create the target sequence, force that same number of observations per day
	targseq <- seq(start.date, end.date, by=ts.dt)
	targdts <- as.Date(as.POSIXlt(targseq))
	targtbl <- table(targdts)
	Nperdy <- as.integer(60/as.numeric(ts.dt)*24 + 1e-9)
	ckdst <- targtbl != Nperdy
	if(any(ckdst)) { # Have transition from dst to st or st to dst
		ckdates <- names(targtbl)[ckdst]
		for(dt in ckdates) {
			start.ck <- as.POSIXct(dt, tz=tz)
			end.ck <- start.ck + days(1) - ts.dt
			seq.ck <- seq(start.ck, end.ck, length.out=Nperdy)
			targseq <- c(targseq[targseq < start.ck], seq.ck, targseq[targseq > end.ck])
		}
	}
	# Now create the data.frame and merge
	retval <- data.frame(X=targseq)
	names(retval) <- DateTime
	retval <- mergeNearest(retval, DateTime, right=UVdata, dates.right=DateTime,
												 max.diff=max.diff)
	names(retval)[1L] <- DateTime
	return(retval)
}
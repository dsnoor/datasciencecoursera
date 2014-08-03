best <- function(state, outcome) 
{
	
	## Read outcome data
	datasource <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

		## Filter and simplify the column names
		datasource <- datasource[c(2, 7, 11, 17, 23)]
		names(datasource)[1] <- "name"
		names(datasource)[2] <- "state"
		names(datasource)[3] <- "heart attack"
		names(datasource)[4] <- "heart failure"
		names(datasource)[5] <- "pneumonia"

	## Check that state and outcome are valid

		## Check that state is valid
		states <- datasource[, 2]
		states <- unique(states)
		if( state %in% states == FALSE ) stop("invalid state")


		## Check that outcome is valid
		outcomes = c("heart attack", "heart failure", "pneumonia")
		if( outcome %in% outcomes == FALSE ) stop("invalid outcome")


		## Filter rows for the requested state	
		datasource <- datasource[datasource$state==state & datasource[outcome] != 'Not Available', ]
		values <- datasource[, outcome]
		rowNum <- which.min(values)

	## Return hospital name in that state with lowest 30-day death rate
    	datasource[rowNum, ]$name

}
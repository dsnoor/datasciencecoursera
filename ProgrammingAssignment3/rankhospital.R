rankhospital <- function(state, outcome, num) 
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

		## check that num value is valid
		if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")


	## Filter rows for the requested state	
	datasource <- datasource[datasource$state==state & datasource[outcome] != 'Not Available', ]


	## sort the datasource in ascending order by name and outcome
	datasource[outcome] <- as.data.frame(sapply(datasource[outcome], as.numeric))
	datasource <- datasource[order(datasource$name, decreasing = FALSE), ]
	datasource <- datasource[order(datasource[outcome], decreasing = FALSE), ]


	## Read values related to outcome
	values <- datasource[, outcome]

	##Process and return row number based on the num argument
	if( num == "best" ) 
	{
         rowNum <- which.min(values)
    	} 
	else if( num == "worst" ) 
		{
	         rowNum <- which.max(values)
		} 
	     else 
		{
 	 	 rowNum <- num
		}

	## Return hospital name in that state with the given rank 30-day death rate
	datasource[rowNum, ]$name
}
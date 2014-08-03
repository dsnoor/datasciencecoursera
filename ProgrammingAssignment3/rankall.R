rankall <- function(outcome, num = "best") 
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

	## Check that outcome and num are valid

		## Check that outcome is valid
		outcomes = c("heart attack", "heart failure", "pneumonia")
		if( outcome %in% outcomes == FALSE ) stop("invalid outcome")

		## check that num value is valid
		if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")


	## Filter rows for the requested outcome	
	datasource <- datasource[datasource[outcome] != 'Not Available', ]


	## sort the datasource in ascending order by name and outcome
	datasource[outcome] <- as.data.frame(sapply(datasource[outcome], as.numeric))
	datasource <- datasource[order(datasource$name, decreasing = FALSE), ]
	datasource <- datasource[order(datasource[outcome], decreasing = FALSE), ]


	## Support function to process and return the hospital based on the num argument
	getHospitalByRank <- function(myds, myst, mynum) 
	{
		myds <- myds[myds$state==myst, ]
		values <- myds[, outcome]
         
		if( mynum == "best" ) 
		{
		 rowNum <- which.min(values)
	        } else if( mynum == "worst" ) 
			{
			 rowNum <- which.max(values)
		        } else 
				{
				 rowNum <- mynum
			        }
	        myds[rowNum, ]$name
	}



	## For each state, find the hospital of the given rank
	states <- datasource[, 2]
	states <- unique(states)
	resultdataframe <- data.frame("hospital"=character(), "state"=character())

	for(currentstate in states) 
	{
        	## retun hospital of the given rank
		rankedhospital <- getHospitalByRank(datasource, currentstate, num)
	        resultdataframe <- rbind(resultdataframe, data.frame(hospital=rankedhospital, state=currentstate))
	}

	## Return a data frame with the hospital names and the (abbreviated) state name

		## sort the result data frame by state
		resultdataframe <- resultdataframe[order(resultdataframe['state'], decreasing = FALSE), ]

		## return the end result
		resultdataframe


}


 
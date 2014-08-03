complete <- function(directory, id = 1:332) 
{
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        
        pollutantcompleteDataset <- data.frame()
        
        ## Reading each file using for loop and append the data to resultDataset 
        ## for final calculation
        for(file in rep(id))
        {
                ## concatenate directory with filename/monitorID
                filepath <-  paste(directory,"/",sprintf("%03d",file),".csv", sep ="")
                
                ## read content from current file/monitor
                pollutantcurrentDataset <-  read.table(filepath, header = TRUE, sep = ",") 
                
                ## Extract complete cases from currentDataset 
                completeCases <- pollutantcurrentDataset[complete.cases(pollutantcurrentDataset),]        
                
                
                ## Append file/monitor Id and count(completecases) of each Dataset to the final result
                pollutantcompleteDataset <- rbind(pollutantcompleteDataset, cbind(file, nrow(completeCases)))            
        }

        ## Adding column names
        colnames(pollutantcompleteDataset) <- c("id","nobs")
        
        ## returning final result
        pollutantcompleteDataset
}
pollutantmean <- function(directory, pollutant, id = 1:332) 
{
        ## 'directory' is a character vecgtor of lenght 1 indicating the 
        ##  location of the CSV file
        
        ## 'pollutant' is a character vector of length 1 indicating the name 
        ## of the pollutant for which we will calculate the mean; either 
        ## "sulphate" or "nitrate".
        
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        pollutantDataset <- data.frame()
        
        ## Reading each file using for loop and append the data to pollutantDataset 
        ## for final calculation
        for(file in rep(id))
        {
                ## concatenate directory with filename
                filepath <-  paste(directory,"/",sprintf("%03d",file),".csv", sep ="")
                
                ## read content from current file
                pollutantCurrentData <-  read.table(filepath, header = TRUE, sep = ",") 
                
                ## Append each current dataset to the final pollutatnDataset
                pollutantDataset <- rbind(pollutantDataset, pollutantCurrentData)
                
        }
        ## The below line of code is to just to check on final dataset content for debugging purpose
        ## pollutantDataset      
        
        
        ## calculating mean for requested pollutant while removing NA values
        mean(pollutantDataset[,pollutant],na.rm=TRUE)
}
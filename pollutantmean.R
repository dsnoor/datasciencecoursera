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
        for(file in rep(id))
        {
                ## directory 
                pollutantCurrentData <-  read.table(paste(directory,"/",sprintf("%03d",file),".csv",sep=""), header = TRUE, sep = ",") 
                pollutantDataset <- rbind(pollutantDataset, pollutantCurrentData)
                
        }
        ##pollutantDataset
        mean(pollutantDataset[,pollutant],na.rm=TRUE)
}

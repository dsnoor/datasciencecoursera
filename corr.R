corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        ##============================================================
        
        # get list of files from directory
        files <- list.files( path = directory )
        
        # function return as avector
        cr <- c() 
        
        # read data from csv files
        for(f in 1:length(files))
        {
                data <- read.csv( paste(directory, "/", files[f], sep="") )
                
                # filter data with complete cases
                data <- data[complete.cases(data),]
                
                # check for total cases against threshold
                if ( nrow(data) > threshold ) 
                {
                        # correlate and append correlations to the vector
                        cr <- c(cr, cor(data$sulfate, data$nitrate) ) 
                }
        }
        
        # convert the vector to numeric and return
        return( as.numeric(cr))
}
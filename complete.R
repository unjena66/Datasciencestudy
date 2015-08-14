complete <- function(directory, id = 1:332){
        prevdir <- getwd()
        
        setwd(directory) # locate a directory where the specdata is
        
        nobs <- numeric()
        
        for(no in id){
                
                # read csv files of id into data.frame type
                filename <- sprintf("%03d.csv",no)
                spec <- read.csv(filename)
                
                # make a numeric vector of number of observed cases(nobs)
                nobs <- c(nobs, nrow(na.omit(spec)))
        }
        
        comp <- data.frame(id, nobs) # make a dataframe with id and nobs
        
        setwd(prevdir)  # back to where you were
        
        comp #return
}
pollutantmean <- function(directory, pollutant, id = 1:332){
        prevdir <- getwd()
      
        setwd(directory) # locate a directory where the specdata is
                  
        allpltnt <- numeric() # make empty numeric to make a list      
                
        for(no in id){
                
                # read csv files of id into data.frame type
                filename <- sprintf("%03d.csv",no)
                spec <- read.csv(filename)
                
                # make a list of pollutant from all data.frame - sulfate or nitrate
                allpltnt <- c(allpltnt, as.numeric(as.matrix(spec[pollutant])))
                
        }

        pltmean <- mean(allpltnt, na.rm=TRUE) # calculate mean of the list
        
        setwd(prevdir)  # back to where you were

        pltmean
}
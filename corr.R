corr <- function(directory, threshold = 0){
        prevdir <- getwd()
        
        setwd(directory) # locate a directory where the specdata is
        
        correlate <- numeric(0) # make an empty vector to load correlation values of each monitors
                
        for(no in 1:332){
                
                # read csv files of id into data.frame type
                filename <- sprintf("%03d.csv",no)
                spec <- read.csv(filename)
                
                # if the number of complete observed cases is greater than threshold
                if(nrow(na.omit(spec))>threshold) {
                        # calculate the correlation between nitrate and sulfate
                        co <- cor(spec$nitrate, spec$sulfate, use = "complete.obs")
                        correlate <- c(correlate, co)
                }
                
        }
        
        setwd(prevdir)  # back to where you were
        
        correlate #return
}
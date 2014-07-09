corr <-
function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    dataset0 <- list.files(directory, pattern="csv")
    mid <- c(1:length(dataset0))
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    source("complete.R")
    completedata <- complete("specdata", mid)
    nobs <- completedata$nobs
    
    mcor <- vector(mode="numeric", length=0)
    c <- 1
    
    for(i in mid)
    {
        path1 <- paste("./",directory,"/",dataset0[i],sep="")
        dataset1 <- read.csv(path1)
        subset1 <- na.omit(dataset1)
        
        if(nobs[i] > threshold)
        {
            nit <- subset1["nitrate"]
            sul <- subset1["sulfate"]
            
            mcor[c] <- cor(nit,sul)
            c <- c+1
        }
    }
    
    ## Return a numeric vector of correlations
    output <- mcor
    return(output)
}

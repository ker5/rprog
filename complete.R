complete <-
function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    dataset0 <- list.files(directory, pattern="csv")
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    mid <- 0
    mnobs <- 0
    c <- 1
    for(i in id)
    {
        path1 <- paste("./",directory,"/",dataset0[i],sep="")
        dataset1 <- read.csv(path1)
        nobs1 <- nrow(na.omit(dataset1))
        if(i==head(id,n=1))
        {
            mid[c] <- i
            mnobs[c] <- nobs1
            c <- c+1
        }
        else
        {
            mid[c] <- i
            mnobs[c] <- nobs1
            c <- c+1
        }
    }
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    output <- data.frame(mid,mnobs)
    colnames(output) <- c("id","nobs")
    return(output)
}

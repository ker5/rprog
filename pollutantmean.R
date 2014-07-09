pollutantmean <-
function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    #### directory <- "./specdata"
    dataset0 <- list.files(directory, pattern="csv")
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    for(i in id)
    {
        path1 <- paste("./",directory,"/",dataset0[i],sep="")
        dataset1 <- read.csv(path1)
        if(i==head(id,n=1))
        {
            dataset2 <- dataset1
        }	
        else
        {	
            dataset2 <- rbind(dataset2,dataset1)
        }	
    }
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    output <- mean(dataset2[, pollutant], na.rm=TRUE)
    return(output)
}

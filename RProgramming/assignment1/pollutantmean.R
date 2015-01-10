pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
    ## initialize a vector for the mean value
  meandata <- c()
  
  ##directory <- ("./specdata")
  
  # find all files in the specdata folder
  dFiles <- as.character( list.files(directory,full.names = TRUE) )
  ## Loop through the files
  for(i in id) {
    ## Read file contents into datafile
    datafile <- read.csv(dFiles[i], header=T, sep=",")
    ## strip empty values
    na_rm <- datafile[!is.na(datafile[, pollutant]), pollutant]
    ##calc mean
    meandata <- c(meandata, na_rm)
    
    ##print(mean(meandata))
    ##flush.console()
  }
   return(round(mean(meandata), 3)) 
    
  
}

##pollutantmean("specdata", "sulfate", 1:10)

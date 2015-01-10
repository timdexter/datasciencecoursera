corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  
  ## Load all the files incl path into dfiles
  dfiles <- as.character( list.files(directory,full.names = TRUE) )
  ## define vector for the coev values
  correlation <- vector()
  ## Loop over all files
  for(i in 1:length(dfiles))
  {
    ## Read the current file and count the number of complete cases
    datafile <- read.csv(dfiles[i])
    cases <- datafile[complete.cases(datafile),]
    ## Test if cases is gtreater than the provided threshold. 
    ## If so, calculate the coeff of the two pollutant variables
    if (nrow(cases) > threshold)
    {
      correlation <- c(correlation,cor(cases$sulfate, cases$nitrate))
      ##print(paste(dfiles[i]," - ",datafile$sulfate," - ", datafile$nitrate))
    }
  }
  return(correlation)
}

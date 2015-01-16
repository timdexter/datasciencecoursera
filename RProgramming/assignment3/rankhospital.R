rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome are valid
  if(!outcome %in% c("heart attack","heart failure","pneumonia"))
  {
    stop("invalid outcome.")
  }
  
  if(!toupper(state) %in% data$State)
  {
    stop("invalid state")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  ## fetch index of appropriate outcome column
  oindex <- ifelse (outcome=="heart attack",11,ifelse (outcome=="heart failure",17,23))
  data[,oindex] <- as.numeric(data[,oindex])
  
  
  ## Filter by state
  statedata <- subset(data, State==toupper(state))
  ## strip out NA data
  statedata <- na.omit(statedata)
  ## sort by out come and hospital
  hosps <- statedata[order(statedata[,oindex],statedata[,2]),2]
  ## get the rannking number based on parameter
  num <- ifelse(num=="best",1,ifelse(num=="worst",length(hosps),num))
  hosps[num]            



}
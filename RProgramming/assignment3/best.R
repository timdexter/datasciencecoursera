best <- function(state, outcome) {
  ##Global turn off warnings
  options(warn=-1)
  
  ## Read outcome data
  
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
    
  ## Check that outcome are valid
  if(!outcome %in% c("heart attack","heart failure","pneumonia"))
  {
    stop("You have entered and invalid outcome.")
  }

  ## Check state is valid
  if(!toupper(state) %in% data$State)
  {
    stop("You have entered and invalid state code. It should be 2 letters.")
  }
  
  ##Find the index of the outcome we are interested in
  ## Values for outcome
  ## "heart attack" - [15] "Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
  ## "heart failure" - [21] "Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
  ## "pneumonia" - [45] "Number.of.Patients...Hospital.30.Day.Readmission.Rates.from.Pneumonia"
  
  oindex <- ifelse (outcome=="heart attack",11,ifelse (outcome=="heart failure",17,23))
  data[,oindex] <- as.numeric(data[,oindex])
  #data <- na.omit(data)
  

  ## Filter by state
  statedata <- subset(data, State==toupper(state))
  
  ## Find lowest death value
  ## Return hospital name in that state with lowest 30-day death
  ## rate  
  
  hosps <- statedata[order(statedata[,oindex],na.last = T),2]
  hosps[1]
}
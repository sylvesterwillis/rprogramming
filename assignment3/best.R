best <- function(state, outcome) {
  ## Read outcome data
  careData <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  stateColumn <- 7
  hospitalNameColumn <- 2
  stateList <- unique(careData[, stateColumn])
  if ( !is.element(state, stateList) ) {
    stop("invalid state")
  }
  
  outcome <- gsub(" ", ".", outcome)
  outcomeColumnNumber <- data.frame("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23) 
  if( !is.element(outcome, colnames(outcomeColumnNumber)) ) {
    stop("invalid outcome")
  }
  
  ## The hospital name and outcome column numbers both come from the ordering of the usedColumns vector.
  outcomeNumber <- 3
  
  usedColumns <- c(hospitalNameColumn, stateColumn, outcomeColumnNumber[,outcome])
  dataNeeded <- careData[, usedColumns]
  dataNeeded <- dataNeeded[dataNeeded$State == state,]
  
  outcomeToNumeric <- as.numeric(as.character(dataNeeded[,colnames(dataNeeded)[outcomeNumber]]))
  numericData <- dataNeeded[dataNeeded[,outcomeNumber] != "Not Available",]
  
  minimum <- min(as.numeric(as.character(numericData[,3])))
  
  ## First column in lowestHospitals should be the name of the hospital.
  lowestHospitals <- numericData[as.numeric(as.character(numericData[, outcomeNumber])) == minimum, 1]
  ## Return hospital name in that state with lowest 30-day death
  sort(as.character(lowestHospitals))
  ## rate
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  directory <- paste(directory, "/", sep = "")
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  results <- c()
  
  for (i in 1:332){
    fileName <- paste(directory, sprintf("%03.0f", i), ".csv", sep = "")
    completeData <- na.omit(read.csv(fileName))
    
    if (nrow(completeData) > threshold){
      corResult <- cor(completeData["sulfate"], completeData["nitrate"])
      results <- c(results, corResult)
    }
  }
  
  results
}
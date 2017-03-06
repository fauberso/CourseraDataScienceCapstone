# Global variables
k <- 5 #"good enough" value above which we don't discount anymore
discProp <- "$discounts" #Name of the Discount table in an environment

# 
# Print a debug message
#
debugLog <- function(...) {
  cat(paste(Sys.time(), paste(...), sep=" - "))
  cat("\n")
}

#
# Load data files into the environment 
# Measure load times with: system.time(load())
#

load <- function(samplePercent=1) {
  projectDirName <- "CourseraDataScienceCapstone"
  
  # Move one directory down if we're still in our project dir
  if (file.exists(projectDirName)){
    setwd(projectDirName)
  }
  originalDir <- getwd()
  
  dataDirName <- paste("model",samplePercent, sep=".")
  dataDir <- paste(getwd(),dataDirName,sep="/")
  
  start.time <- Sys.time()
  debugLog("Loading from ",dataDir, "...")
  assign("pred1", convert(readRDS(file=paste(dataDir, "predictor.1.rds", sep="/")), parent=emptyenv(), size = 15000000), pos=globalenv())
  debugLog("Predictor 1 loaded")
  assign("pred2", convert(readRDS(file=paste(dataDir, "predictor.2.rds", sep="/")), parent=emptyenv(), size = 50000000), pos=globalenv())
  debugLog("Predictor 2 loaded")
  assign("pred3", convert(readRDS(file=paste(dataDir, "predictor.3.rds", sep="/")), parent=emptyenv(), size = 100000000), pos=globalenv())
  debugLog("Predictor 3 loaded")
  assign("resi1", convert(readRDS(file=paste(dataDir, "residual.1.rds", sep="/")), parent=emptyenv(), size = 15000000), pos=globalenv())
  debugLog("Residuals from Predictor 1 loaded")
  assign("resi2", convert(readRDS(file=paste(dataDir, "residual.2.rds", sep="/")), parent=emptyenv(), size = 50000000), pos=globalenv())
  debugLog("Residuals from Predictor 2 loaded")
  assign("resi3", convert(readRDS(file=paste(dataDir, "residual.3.rds", sep="/")), parent=emptyenv(), size = 100000000), pos=globalenv())
  debugLog("Residuals from Predictor 3 loaded")
  end.time <- Sys.time()
  
  debugLog("Load complete in",end.time - start.time,"seconds")
}

convert <- function(data, ...) {
  #list2env(data, ...)
  data
}

getSearchTerm <- function(words, count) {
  if (length(words)-(count-1)<1) return(NULL)
  paste(words[(length(words)-(count-1)):length(words)], collapse="_")
}

getPrediction <- function(searchTerm, pred) {
  if (is.null(searchTerm)) return (c())
  discounts <- pred[[discProp]]
  frequencies <- pred[[searchTerm]]
  total <- sum(frequencies) 
  ifelse(frequencies > k, frequencies, discounts[frequencies]*frequencies)/total
}

predict <- function(string, searchTerms=NULL) {
  words <- strsplit(string, " ")[[1]]

  emptylist <- list(c())
  
  #resultsFromTrigrams <- mget(getSearchTerm(words,3), envir=trigrams, inherits = FALSE, ifnotfound = emptylist)[[1]]
  resultsFromTrigrams  <- getPrediction(getSearchTerm(words,3), pred3)
  redidualFromTrigrams <- 1-sum(resultsFromTrigrams)
  
  #resultsFromBigrams <- mget(getSearchTerm(words,2), envir=bigrams, inherits = FALSE, ifnotfound = emptylist)[[1]]
  resultsFromBigrams   <- getPrediction(getSearchTerm(words,2), pred2)
  redidualFromBigrams  <- 1-sum(resultsFromBigrams)
  resultsFromBigrams   <- resultsFromBigrams * redidualFromTrigrams
  
  #resultsFromUnigrams <- mget(getSearchTerm(words,1), envir=unigrams, inherits = FALSE, ifnotfound = emptylist)[[1]]
  resultsFromUnigrams  <- getPrediction(getSearchTerm(words,1), pred1)
  redidualFromUnigrams <- 1-sum(resultsFromUnigrams)
  resultsFromUnigrams  <- resultsFromUnigrams * redidualFromBigrams
  
  debugLog("Residual probabilities:", redidualFromTrigrams, "from Trigrams,", redidualFromBigrams, "from Bigrams,", redidualFromUnigrams, "probability of non match.")
  
  results <- c(resultsFromTrigrams, resultsFromBigrams, resultsFromUnigrams)
  
  if (!is.null(searchTerms)) {
    results <- results[names(results) %in% searchTerms]
  }
  
  results
}
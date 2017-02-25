# Global variables
k <- 5 #"good enough" value above which we don't discount anymore
discProp <- "$discounts" #Name of the Discount table in an environment

#
# Load data files into the environment 
#
load <- function(samplePercent=1) {
  projectDirName <- "CourseraDataScienceCapstone"
  
  # Move one directory down if we're still in our project dir
  if (file.exists(projectDirName)){
    setwd(projectDirName)
  }
  originalDir <- getwd()
  
  dataDirName <- paste("data",samplePercent, sep=".")
  dataDir <- paste(getwd(),dataDirName,sep="/")
  
  assign("unigrams", readRDS(file=paste(dataDir, "unigrams.rds", sep="/")), pos=globalenv())
  assign("bigrams", readRDS(file=paste(dataDir, "bigrams.rds", sep="/")), pos=globalenv())
  assign("trigrams", readRDS(file=paste(dataDir, "trigrams.rds", sep="/")), pos=globalenv())
}

predict <- function(string, searchTerms=NULL) {
  words <- strsplit(string, " ")[[1]]
  getSearchTerm <- function(words, count) paste(words[(length(words)-(count-1)):length(words)], collapse="_")
  
  emptylist <- list(c())
  
  resultsFromTrigrams <- mget(getSearchTerm(words,3), envir=trigrams, inherits = FALSE, ifnotfound = emptylist)[[1]]
  resultsFromBigrams <- mget(getSearchTerm(words,2), envir=bigrams, inherits = FALSE, ifnotfound = emptylist)[[1]]
  resultsFromUnigrams <- mget(getSearchTerm(words,1), envir=unigrams, inherits = FALSE, ifnotfound = emptylist)[[1]]

  results <- c(resultsFromTrigrams, resultsFromBigrams, resultsFromUnigrams)
  
  if (!is.null(searchTerms)) {
    results <- results[names(results) %in% searchTerms]
  }
  
  results
}
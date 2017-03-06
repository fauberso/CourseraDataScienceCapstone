library(Matrix)
library(quanteda)
library(readtext)
library(parallel)

#install readtext using devtools::install_github("kbenoit/readtext")
#install quanteda using devtools::install_github("kbenoit/quanteda")
#install_version("quanteda", version = "0.9.8.3", repos = "http://cran.us.r-project.org")

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
# Make an n-percent sample
#
makeSample <- function(originalDir, sampledDir, filename, samplePercent) {
  debugLog(paste("Sampling ", filename, "...", sep = ""))
  setwd(originalDir)
  connection <- file(description=filename, open="rb")
  contents <- readLines(connection, skipNul = TRUE, encoding = "UTF-8")
  subsample <- sample(contents, size=length(contents)*(samplePercent/100), replace=FALSE)
  close(connection)
  setwd(sampledDir)
  write(subsample, filename, sep="\n")
}

#
# Make a DFM of N-Grams
#
getDFM <- function(c, ...) {
  debugLog("Generating NGram...")
  dfm(c, stem=FALSE, tolower=TRUE, removeNumbers=TRUE, removePunct=TRUE, removeSymbols=TRUE, removeHyphens=TRUE, removeTwitter=TRUE, removeURL=TRUE, ...)
}

#
# Make a predictor map out of an N-Gram
#
#Usage example: get('and_the_facts', envir=predictor, inherits = FALSE)
#Check whether an ngram-1 exists with: exists('and_the_fact', envir=predictor, inherits = FALSE)
#
toPredictorMap <- function(dfm) {
  #Variant 1: Use an R environment: Effectively a native Hash map implementation 
  predictor <- new.env(hash = TRUE, parent=emptyenv(), size = 5000000)
  #Variant 2: Use a list (More readable, but much slower for large amounts of data)
  #predictor <- list()
    
  debugLog("Summing ngram frequencies...")
  frequencies <- colSums(dfm_sort(dfm, decreasing = FALSE))
  rm(dfm)
  
  emptylist <- list(c())
  # Preparations for Good-Turing estimator
  debugLog("Calculating discounts...")
  fofTable <- table(frequencies)
  discounts <- sapply(1:k, function(i) ( ((i+1)/i) * (fofTable[names(fofTable)==i+1]/fofTable[names(fofTable)==i]) ) )
  discounts <- unname(discounts)
  print(discounts)
  assign(discProp, discounts, pos=predictor)
  
  splitAtLastDash <- function(ngram) {
    lastDash <- regexpr("_([^_]*?)$", ngram)
    previous <- unname(substr(ngram, 0, lastDash-1))
    nextword <- unname(substr(ngram, lastDash+1, lastDash+attr(lastDash, "match.length")-1))
    c(previous,nextword)
  }
  
  #Iterate through ngrams in increasing order of frequency
  debugLog("Splitting NGrams and collating frequencies of ngrams with identical root...")
  pb <- txtProgressBar(min = 1, max = length(frequencies), style = 3)
  
  for (i in 1:length(frequencies)) {
    #split ngram in an ngram-1 and the term after the last dash
    ngram <- names(frequencies)[i]
    lastDash <- regexpr("_([^_]*?)$", ngram)
    previous <- unname(substr(ngram, 0, lastDash-1))
    nextword <- unname(substr(ngram, lastDash+1, lastDash+attr(lastDash, "match.length")-1))
    nextfreq <- unname(frequencies[i])
    names(nextfreq) <- nextword
    
    #The sorting means that the more frequent occurences will overwrite less frequents. The frequencies
    #of the previous occurences will be kept (in descending order) so that we're later able to compute
    #a good-turing estimator.
    # Variant 1
    entry <- mget(previous, envir=predictor, inherits = FALSE, ifnotfound = emptylist)[[1]]
    assign(previous,  c(nextfreq, entry), pos=predictor)
    # Variant 2
    # predictor[[previous]] <- c(nextfreq, predictor[[previous]])
    
    setTxtProgressBar(pb, i)
  }
  
  # Variant 1
  as.list.environment(predictor)
  # Variant 2
  # predictor
}

#
# Given a list of frequencies for classes, and a list of Good-Turing discounts (in which the first element is the
# discount for single occurences, the second is the discount for 2 occurences, etc..), calculate the 
# probability that an item is not in any of the classes, but rather from a missing, unknown class.
# 
getResidualProb <- function(flist, dlist, k) {
  totalFreq<-sum(flist)
  probabilities <- ifelse(flist > k, flist, dlist[flist]*flist)
  1-sum(probabilities/totalFreq)
}


computeResidualProb <- function(predictorMap) {
  cl <- makeCluster(8)
  clusterExport(cl=cl, varlist=c("getResidualProb", "k"))  
  dlist <- predictorMap[[discProp]] #List of good-turing discounts
  residuals <- parLapply(cl, predictorMap, function(x) getResidualProb(x, dlist, k))
  stopCluster(cl)
  
  residuals
}

#
# In an underscore-separated ngram, remove the first word and underscore
#
removeFirstTerm <- function(term) {
  substr(term, regexpr("_", term)+1, nchar(term))
}

#
# Calculate probability for each prediction for an n-gram. Check in corresponding n-1gram whether the same term
# exists. If yes, and if its backed off probability is higher than its probability in the n-gram, then remove
# it: During prediction, we're better off using the backed-off model, since it yields a higher probability
# for the same predicted term.
# Return the entire model, but using probabilities instead of frequencies.
#
filterModel <- function(model, backoff) {
  cl <- makeCluster(8)
  clusterExport(cl=cl, varlist=c("removeFirstTerm", "k", "discProp")) 
  
  modelDisc <- model[[discProp]]
  backoffDisc <- backoff[[discProp]]

  #results <- sapply(1:length(model), function(i) {
  results <- parSapply(cl, 1:length(model), function(i) {

    values <- model[[i]]
    name <- names(model)[i]

    if(name==discProp) {
      retval <- list()
      retval[[name]]<-values
      return(retval)
    }
    
    backoffValues <- backoff[removeFirstTerm(name)][[1]]

    #Remove from the n-1gram any values we already have as part of the ngram
    # backoffValues <- backoffValues[!(names(backoffValues) %in% names(values))]
    
    # Convert the frequencies to probabilities
    values <- ifelse(values > k, values, modelDisc[values]*values)/sum(values)
    backoffValues <- ifelse(backoffValues > k, backoffValues, backoffDisc[backoffValues]*backoffValues)/sum(backoffValues)
    backoffValues <- backoffValues * (1-sum(values))
    
    # If probability from a value is lower than the backed off probability of the same value,
    # then there's no point in keeping the value. We will used the backed off probability in the predictor anyway
    filter <- Vectorize(function(x, name) is.na(backoffValues[name]) || backoffValues[name]<x)
    values <- values[filter(values, names(values))]
    
    # Lastly, remove all values below a certain threshold. This will prevent us from keeping very long list with
    # values that are not very significant
    values <- values[values>0.001]
    
    retval <- list()
    retval[[name]]<-values
    retval
  })
  
  stopCluster(cl)
  results[!sapply(results, function(x) is.null(x) || length(x)==0)] 
}


# Main block:
# Download, Sample, make ngrams, save.
#
computeFiles <- function(samplePercent=1, parallel=TRUE) {
  projectDirName <- "CourseraDataScienceCapstone"
  finalDirName <- "final"
  sampledDirName <- paste("sample",samplePercent, sep=".")
  sampledDir <- paste(getwd(),sampledDirName,sep="/")
  dataDirName <- paste("model",samplePercent, sep=".")
  dataDir <- paste(getwd(),dataDirName,sep="/")
  debugLog(paste("Using sample under:", sampledDirName))
  modelFileName <- paste("model",samplePercent, "rds", sep=".")
  
  # Move one directory down if we're still in our project dir
  if (file.exists(projectDirName)){
    setwd(projectDirName)
  }
  originalDir <- getwd()
  
  # Download original dataset if necessary
  if (!file.exists(finalDirName)){
    URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(url = URL, src = "Coursera-SwiftKey.zip", quiet = TRUE)
    unzip("Coursera-SwiftKey.zip")
    file.remove("Coursera-SwiftKey.zip")
  }
  
  # Sample to x percent of the original size if necessary
  if (!file.exists(sampledDir)){
    if (samplePercent!=100) {
      dir.create(paste(sampledDir, "final", "en_US", sep = "/"), recursive = TRUE)
      makeSample(originalDir, sampledDir, "final/en_US/en_US.blogs.txt", samplePercent)
      makeSample(originalDir, sampledDir, "final/en_US/en_US.news.txt", samplePercent)
      makeSample(originalDir, sampledDir, "final/en_US/en_US.twitter.txt", samplePercent)
    }
  }
  
  # Compute simple data files
  if (!file.exists(paste(dataDir, "predictor.1.rds", sep="/"))){
    setwd(sampledDir)
    texts <- readtext("final/en_US/*.txt", docvarsfrom="filenames", dvsep="\\.", docvarnames=c("lang","name"), encoding="latin1")
    c <- corpus(texts, textField = "text")
    rm(texts)
    setwd(originalDir)
    
    summary(c)
    dir.create(dataDir, recursive = TRUE)

    debugLog("Processing 2-grams into 1-gram and a predicted value...")
    saveRDS(toPredictorMap(getDFM(c, ngrams=2)), file=paste(dataDir, "predictor.1.rds", sep="/"))
    debugLog("Processing 3-grams into 2-gram and a predicted value...")
    saveRDS(toPredictorMap(getDFM(c, ngrams=3)), file=paste(dataDir, "predictor.2.rds", sep="/"))
    debugLog("Processing 4-grams into 3-gram and a predicted value...")
    saveRDS(toPredictorMap(getDFM(c, ngrams=4)), file=paste(dataDir, "predictor.3.rds", sep="/"))
    rm(c)
  }
  
  # Compute rediduals (currently unused)
  if (!file.exists(paste(dataDir, "residual.1.rds", sep="/"))){
    debugLog("Calculating residuals of 1-gram predictor...")
    saveRDS(computeResidualProb(readRDS(file=paste(dataDir, "predictor.1.rds", sep="/"))), 
                                file=paste(dataDir, "residual.1.rds", sep="/"))
    debugLog("Calculating residuals of 2-gram predictor...")
    saveRDS(computeResidualProb(readRDS(file=paste(dataDir, "predictor.2.rds", sep="/"))), 
            file=paste(dataDir, "residual.2.rds", sep="/"))
    debugLog("Calculating residuals of 3-gram predictor...")
    saveRDS(computeResidualProb(readRDS(file=paste(dataDir, "predictor.3.rds", sep="/"))), 
            file=paste(dataDir, "residual.3.rds", sep="/"))
  }
  
  # Filter results and combine into 1 file
  if (!file.exists(modelFileName)){
    print(ls())
    model <- list()
    debugLog("Removing 3-grams less significant than their backed off 2-grams...")
    pred3 <- readRDS(file=paste(dataDir, "predictor.3.rds", sep="/"))
    pred2 <- readRDS(file=paste(dataDir, "predictor.2.rds", sep="/"))
    model[[3]] <- filterModel(pred3,pred2)
    debugLog("Removing 3-grams less significant than their backed off 2-grams...")
    rm(pred3)
    pred1 <- readRDS(file=paste(dataDir, "predictor.1.rds", sep="/"))
    model[[2]] <- filterModel(pred2,pred1)
    debugLog("Removing 1-grams with low significance...")
    rm(pred2)
    model[[1]] <- filterModel(pred1, list());
    rm(pred1)
    saveRDS(model, file=modelFileName)
  }
  
  TRUE
}

batchCompute <- function() {
  computeFiles(1)
  computeFiles(2)
  computeFiles(5)
  computeFiles(10)
  computeFiles(20)
}
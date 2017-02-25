library(Matrix)
library(quanteda)
library(readtext)

#install readtext using devtools::install_github("kbenoit/readtext")
#install quanteda using devtools::install_github("kbenoit/quanteda")
#install_version("quanteda", version = "0.9.8.3", repos = "http://cran.us.r-project.org")

# Global variables
k <- 5 #"good enough" value above which we don't discount anymore
discProp <- "$discounts" #Name of the Discount table in an environment

#
# Make an n-percent sample
#
makeSample <- function(originalDir, sampledDir, filename, samplePercent) {
  print(paste("Sampling ", filename, "...", sep = ""))
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
  print("Generating NGram...")
  dfm(c, stem=FALSE, tolower=TRUE, removeNumbers=TRUE, removePunct=TRUE, removeSymbols=TRUE, removeHyphens=FALSE, removeTwitter=TRUE, removeURL=TRUE, ...)
}

#
# Make a predictor map out of an N-Gram
#
#Usage example: get('and_the_facts', envir=predictor, inherits = FALSE)
#Check whether an ngram-1 exists with: exists('and_the_fact', envir=predictor, inherits = FALSE)
toPredictorMap <- function(dfm) {
  #Use an R environment: Effectively a native Hash map implementation 
  predictor <- new.env(hash = TRUE, parent=emptyenv())
  print("Summing ngram frequencies...")
  frequencies <- colSums(dfm_sort(dfm, decreasing = FALSE))
  rm(dfm)
  
  emptylist <- list(c())
  # Preparations for Good-Turing estimator
  print("Calculating discounts...")
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
  print("Splitting NGrams and collating frequencies of ngrams with identical root...")
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
    entry <- mget(previous, envir=predictor, inherits = FALSE, ifnotfound = emptylist)[[1]]
    assign(previous,  c(nextfreq, entry), pos=predictor)
    
    setTxtProgressBar(pb, i)
  }
  
  return(predictor)
  
  # Each key is now an n-gram, each element is a list, consisting of a predicted value and a list of 
  # frequencies. For each element, replace the list of frequencies with a good-turing estimation of the
  # combined proportion of undetected species. This estimation dictates in the back-off algorithm whether
  # we prefer using the result from a lower-order ngram instead, beacuse we suspect the followin word
  # is most likely to be one we haven't encountered.
  print("Calculating Good-Turing estimation of combined proportion of undetected species...")
  k <- 5 #"good enough" value above which we don't discount anymore
  fofTable <- table(frequencies)
  discMultiplier <- sapply(1:k, function(i) (fofTable[names(fofTable)==i+1]/fofTable[names(fofTable)==i]) )
  
  predictor <- list2env(eapply(predictor, function(e) {
    results <- data.frame(freq=e[[2]])
    rownames(results) <- e[[1]]
    
    results$discounts <- sapply(results$freq, function(freq) {
      if(freq>k) {
        1
      } else {
        ((freq+1)/freq) * discMultiplier[freq]
      }
    }
    )
    
    results
  }), hash = TRUE, parent=emptyenv())
  
  predictor #A hash map, with an ngram-1 as key, and a data frame with (next word, freq, gt discount) as value 
}

# Main block:
# Download, Sample, make ngrams, save.
#
computeFiles <- function(samplePercent=1, parallel=TRUE) {
  projectDirName <- "CourseraDataScienceCapstone"
  finalDirName <- "final"
  sampledDirName <- paste("sample",samplePercent, sep=".")
  sampledDir <- paste(getwd(),sampledDirName,sep="/")
  dataDirName <- paste("data",samplePercent, sep=".")
  dataDir <- paste(getwd(),dataDirName,sep="/")
  print(paste("Using sample under:", sampledDirName))
  
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
  if (TRUE & !file.exists(dataDir)){
    setwd(sampledDir)
    texts <- readtext("final/en_US/*.txt", docvarsfrom="filenames", dvsep="\\.", docvarnames=c("lang","name"), encoding="latin1")
    c <- corpus(texts, textField = "text")
    rm(texts)
    setwd(originalDir)
    
    summary(c)
    dir.create(dataDir, recursive = TRUE)

    print("Processing 2-grams into 1-gram and a predicted value...")
    saveRDS(toPredictorMap(getDFM(c, ngrams=2)), file=paste(dataDir, "unigrams.rds", sep="/"))
    print("Processing 3-grams into 2-gram and a predicted value...")
    saveRDS(toPredictorMap(getDFM(c, ngrams=3)), file=paste(dataDir, "bigrams.rds", sep="/"))
    print("Processing 4-grams into 3-gram and a predicted value...")
    saveRDS(toPredictorMap(getDFM(c, ngrams=4)), file=paste(dataDir, "trigrams.rds", sep="/"))
  }
    
  dataDirName
}

batchCompute <- function() {
  computeFiles(1)
  computeFiles(10)
  computeFiles(20)
}
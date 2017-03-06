library(parallel)

k <- 5 #"good enough" value above which we don't discount anymore
discProp <- "$discounts" #Name of the Discount table in an environment
respProp <- "$res" #Name of the residual probability variable in a table

# getResidualProb(model[[3]][["is_to_make"]], model[[3]][[discProp]])
getResidualProb <- function(flist, dlist) {
  totalFreq<-sum(flist)
  probabilities <- ifelse(flist > 5, 1, dlist[flist]*flist)
  sum(probabilities/totalFreq)
}

load <- function(samplePercent=1) {
  projectDirName <- "CourseraDataScienceCapstone"
  
  # Move one directory down if we're still in our project dir
  if (file.exists(projectDirName)){
    setwd(projectDirName)
  }
  originalDir <- getwd()
  
  dataDirName <- paste("model",samplePercent, sep=".")
  dataDir <- paste(getwd(),dataDirName,sep="/")
  
  model <- list(
    unigrams=readRDS(file=paste(dataDir, "unigrams.rds", sep="/")),
    bigrams=readRDS(file=paste(dataDir, "bigrams.rds", sep="/")), 
    trigrams=readRDS(file=paste(dataDir, "trigrams.rds", sep="/"))
  )
  
  cl <- makeCluster(16)
  clusterExport(cl=cl, varlist=c("getResidualProb", "discProp"))
  residuals <- list(
    unigrams=parLapply(cl, model[[1]], function(x) getResidualProb(x, model[[1]][[discProp]])),
    bigrams=parLapply(cl, model[[2]], function(x) getResidualProb(x, model[[2]][[discProp]])), 
    trigrams=parLapply(cl, model[[3]], function(x) getResidualProb(x, model[[3]][[discProp]]))
  )
  stopCluster(cl)
  
  assign("model", model, pos=globalenv())
  assign("residuals", residuals, pos=globalenv())
}


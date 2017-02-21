library(Matrix)
library(quanteda)
library(readtext)
#install readtext using devtools::install_github("kbenoit/readtext")
#install quanteda using devtools::install_github("kbenoit/quanteda")
#install_version("quanteda", version = "0.9.8.3", repos = "http://cran.us.r-project.org")
samplePercent <- 1 # Percentage of lines to keep. Adjust this value as needed.
originalDir <- getwd()
sampledDir <- paste(getwd(),"sampled",sep="/")

makeSample <- function(originalDir, sampledDir, filename) {
  setwd(originalDir)
  connection <- file(description=filename, open="rb")
  contents <- readLines(connection, skipNul = TRUE, encoding = "UTF-8")
  subsample <- sample(contents, size=length(contents)*(samplePercent/100), replace=FALSE)
  close(connection)
  setwd(sampledDir)
  write(subsample, filename, sep="\n")
}

if (!file.exists("final")){
  URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(url = URL, src = "Coursera-SwiftKey.zip", quiet = TRUE)
  unzip("Coursera-SwiftKey.zip")
  file.remove("Coursera-SwiftKey.zip")
}

if (!file.exists("sampled")){
  if (samplePercent!=100) {
    dir.create(paste(sampledDir, "final", "en_US", sep = "/"), recursive = TRUE)
    makeSample(originalDir, sampledDir, "final/en_US/en_US.blogs.txt")
    makeSample(originalDir, sampledDir, "final/en_US/en_US.news.txt")
    makeSample(originalDir, sampledDir, "final/en_US/en_US.twitter.txt")
  }
}

setwd(sampledDir)
texts <- readtext("final/en_US/*.txt", docvarsfrom="filenames", dvsep="\\.", docvarnames=c("lang","name"), encoding="UTF-8")
setwd(originalDir)

c <- corpus(texts, textField = "text")
summary(c)
saveRDS(c, file="corpus.rds")

getDFM <- function(...) {
  x <- dfm(c, stem=FALSE, tolower=TRUE, removeNumbers=TRUE, removePunct=TRUE, removeSymbols=TRUE, removeHyphens=FALSE, removeTwitter=TRUE, removeURL=TRUE, ...)
  dfm_trim(x, min_count=2)
}

saveRDS(getDFM(ngrams=1), file="unigram.rds")
saveRDS(getDFM(ngrams=2), file="bigram.rds")
saveRDS(getDFM(ngrams=3), file="trigram.rds")
saveRDS(getDFM(ngrams=4), file="quadrigram.rds")


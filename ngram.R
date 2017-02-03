library(ngram)
library(tm)
getngram <- function() { 
  browser()
  
  if (!file.exists("final")){
  URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(url = URL, destfile = "Coursera-SwiftKey.zip", quiet = TRUE)
  unzip("Coursera-SwiftKey.zip")
  file.remove("Coursera-SwiftKey.zip")
}
  

source <- DirSource("small/en_US")
corpus <- VCorpus(source, readerControl = list(language="en_US"))

corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, tolower)
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
#corpus <- tm_map(corpus, stemDocument, language = "english")
corpus <- tm_map(corpus, PlainTextDocument)

#ng <- ngram(concatenate(lapply(corpus, "[", 1)), n=3)

all <- concatenate(lapply(corpus, "[", 1))
for(chunk in split(all, ceiling(seq_along(all)/20))) {
  browser()
}

#ng
}
library(ngram)
library(tm)
getngram <- function() { 
  if (!file.exists("final")){
    URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(url = URL, destfile = "Coursera-SwiftKey.zip", quiet = TRUE)
    unzip("Coursera-SwiftKey.zip")
    file.remove("Coursera-SwiftKey.zip")
  }
  
  map <- new.env()
  updateNgram(file(description="small/en_US/en_US.blogs.txt", open="rb"), map)
  updateNgram(file(description="small/en_US/en_US.news.txt", open="rb"), map)
  updateNgram(file(description="small/en_US/en_US.twitter.txt", open="rb"), map)
  
#source <- DirSource("small/en_US")
#corpus <- VCorpus(source, readerControl = list(language="en_US"))
#corpus <- tm_map(corpus, removeNumbers)
#corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, stripWhitespace)
#corpus <- tm_map(corpus, tolower)
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
#corpus <- tm_map(corpus, stemDocument, language = "english")
#corpus <- tm_map(corpus, PlainTextDocument)

#ng <- ngram(concatenate(lapply(corpus, "[", 1)), n=3)

#all <- concatenate(lapply(corpus, "[", 1))


#ng
}

updateNgram <- function(connection, env) { 
  lines <- readLines(connection)
  for(chunk in split(lines, ceiling(seq_along(lines)/20))) {
    text <- concatenate(chunk)
    #filtering goes here
    for (ngram in get.ngrams(ngram(text, n=3))) {
      browser()      
    }
    
  }
  
}
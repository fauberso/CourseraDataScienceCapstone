library(tm)

get3gram <- function() {
  # see  http://tm.r-forge.r-project.org/faq.html
  
  if (!file.exists("final")){
    URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(url = URL, destfile = "Coursera-SwiftKey.zip", quiet = TRUE)
    unzip("Coursera-SwiftKey.zip")
    file.remove("Coursera-SwiftKey.zip")
  }
  
  source <- DirSource("final/en_US", encoding = "UTF-8", mode="binary")
  corpus <- VCorpus(source, readerControl = list(language="en_US"))
  
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, tolower)
  #corpus <- tm_map(corpus, removeWords, stopwords("english"))
  #corpus <- tm_map(corpus, stemDocument, language = "english")
  corpus <- tm_map(corpus, PlainTextDocument)
  
  BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 3))}
  tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)) 
  saveRDS(tdm, file="tdm.nds")
  
  return(tdm)
}
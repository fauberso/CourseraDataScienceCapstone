library(ngram)

get3gram <- function() {
  lines <- c()
  
  blogs <- file(description="final/en_US/en_US.blogs.txt", open="rb")
  lines <- readLines(blogs, skipNul = TRUE, encoding = "UTF-8")
  
  news <- file(description="final/en_US/en_US.news.txt", open="rb")
  lines <- c(lines,readLines(news, skipNul = TRUE, encoding = "UTF-8"))
  
  twitter <- file(description="final/en_US/en_US.twitter.txt", open="rb", encoding = "UTF-8")
  lines <- c(lines,readLines(twitter, skipNul = TRUE, encoding = "UTF-8"))
  
  ng <- c()
  for (line in lines) {
    #line <- preprocess(line, case ="lower", remove.punct = TRUE, remove.numbers = TRUE, fix.spacing = TRUE)
    line <- gsub(pattern="[^([:alpha:]|[:space:])]", x=line, replacement = "")
    line <- tolower(line)
    ng <- c(ng, get.ngrams(ngram(line, n=2)))
    ng <- c(ng, get.ngrams(ngram(line, n=3)))
  }
  
  
  samplePercent <- 1 # Percentage of lines to keep. Adjust this value as needed.
  lines <- sample(lines, size=length(lines)*(samplePercent/100), replace=FALSE)
  
  saveRDS(ng, file="ngrams.nds")
  
  return(ng)
}


getngram <- function() { 
  if (!file.exists("final")){
    URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
    download.file(url = URL, destfile = "Coursera-SwiftKey.zip", quiet = TRUE)
    unzip("Coursera-SwiftKey.zip")
    file.remove("Coursera-SwiftKey.zip")
  }
  
  map <- new.env()
  map <- updateNgramsTable(file(description="small/en_US/en_US.blogs.txt", open="rb"), map)
  map <- updateNgramsTable(file(description="small/en_US/en_US.news.txt", open="rb"), map)
  map <- updateNgramsTable(file(description="small/en_US/en_US.twitter.txt", open="rb"), map)
  
}

updateNgramsTable <- function(connection, env) { 
  lines <- readLines(connection)
  for(chunk in split(lines, ceiling(seq_along(lines)/100000))) {
    text <- concatenate(chunk)
    text <- gsub(pattern="[^([:alpha:]|[:space:])]", x=text, replacement = "")
    text <- tolower(text)
    
    
  }
  return(env)
}

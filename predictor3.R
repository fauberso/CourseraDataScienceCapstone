library(quanteda)

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
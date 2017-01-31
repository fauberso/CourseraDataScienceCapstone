# install.packages("quanteda")
# devtools::install_github("kbenoit/readtext") 
require(quanteda)
require(readtext)

blogstxt <- corpus(readtext("data/en_US/en_US.blogs.txt"))


blogstxt <- file(description="data/en_US/en_US.blogs.txt", open="r")
newstxt <- file(description="data/en_US/en_US.news.txt", open="r")
twittertxt <- file(description="data/en_US/en_US.twitter.txt", open="r")

countlines <- function(filename, pattern) {
  con <- file(description=paste("data/en_US/en_US.", filename, ".txt", sep = "") , open="rb", blocking = TRUE)
  result <- sum(grepl(pattern, readLines(con, skipNul = TRUE))) 
  close(con)
  result
}

longestline <- function(filename) {
  con <- file(description=paste("data/en_US/en_US.", filename, ".txt", sep = "") , open="rb", blocking = TRUE)
  result <- which.max(lapply( readLines(con, skipNul = TRUE), nchar))  
  close(con)
  result
}



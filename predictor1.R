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
    line <- preprocess(item, case ="lower", remove.punct = TRUE, remove.numbers = TRUE, fix.spacing = TRUE)
    line <- gsub(pattern="[^([:alpha:]|[:space:])]", x=item, replacement = "")
    line <- tolower(line)
    ng <- c(ng, get.ngrams(ngram(line, n=2)))
    ng <- c(ng, get.ngrams(ngram(line, n=3)))
  }
  
  saveRDS(ng, file="ngrams.nds")
  
  return(ng)
}
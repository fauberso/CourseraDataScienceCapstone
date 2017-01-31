library(ngram)

# https://www.r-bloggers.com/text-mining-the-complete-works-of-william-shakespeare/

TEXTFILE = "pg100.txt"
if (!file.exists(TEXTFILE)) {
     download.file("http://www.gutenberg.org/cache/epub/100/pg100.txt", destfile = TEXTFILE)
}
shakespeare = readLines(TEXTFILE)
shakespeare = shakespeare[-(1:173)]
shakespeare = shakespeare[-(124195:length(shakespeare))]
shakespeare = concatenate(shakespeare)

shakespeare = strsplit(shakespeare, "<<[^>]*>>")[[1]]
dramatis.personae <- grep("Dramatis Personae", shakespeare, ignore.case = TRUE)
shakespeare = shakespeare[-dramatis.personae]
shakespeare = concatenate(shakespeare)

shakespeare = strsplit(shakespeare, "<<[^>]*>>")[[1]]

ng = ngram(shakespeare,2)

babble (ng , 200)
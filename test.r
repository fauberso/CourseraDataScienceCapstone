predictor <- readRDS("predictor.rds")
frequencies <- readRDS("frequencies.rds")

#unigram <- readRDS("unigram.1.rds")
#bigram <- readRDS("bigram.1.rds")
#trigram <- readRDS("trigram.1.rds")
k <- 5 #"good enough" value above which we don't discount anymore
fofTable <- table(frequencies)
for(i in 1:k) {
  discount <- fofTable[names(fofTable)==i+1]/fofTable[names(fofTable)==i]
  
}

discMultiplier <- sapply(1:k, function(i) (fofTable[names(fofTable)==i+1]/fofTable[names(fofTable)==i]) )

predictor <- list2env(eapply(predictor, function(e) {
  results <- data.frame(freq=e[[2]])
  rownames(results) <- e[[1]]
  
  results$discounts <- sapply(results$freq, function(freq) {
    if(freq>k) {
      1
    } else {
      ((freq+1)/freq) * discMultiplier[freq]
    }
  }
  )
  
  results
}), hash = TRUE, parent=emptyenv())

#results[grepl("^Sear*", rownames(results)), ]
#get('and_the_facts', envir=trigram, inherits = FALSE)



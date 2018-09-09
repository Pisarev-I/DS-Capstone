fNextWordPrediction <- function(x) {
  numWordCount <- 0
  dfNextWord <- data.frame(freq = numeric(), LastWord = character())
  while (numWordCount < 10) {
    dfNextWordTemp <- dfWordsFreq[dfWordsFreq$PrevWords == x, c(2,4)]
    vcRepeatWords <- as.vector(which(dfNextWordTemp$LastWord %in% dfNextWord$LastWord))
    if (length(vcRepeatWords) > 0) {
      dfNextWordTemp <- dfNextWordTemp[-vcRepeatWords,]
    }
    dfNextWordTemp <- dfNextWordTemp[order(dfNextWordTemp$freq, decreasing = TRUE),]
    dfNextWordTemp <- head(dfNextWordTemp, 10 - numWordCount)
    dfNextWord <- rbind(dfNextWord, dfNextWordTemp)
    numWordCount <- nrow(dfNextWord)
    ifelse(str_count(x, "\\S+") == 1,
           x <- "",
           x <- word(x, start = 2, end = -1))
  }
  return(dfNextWord$LastWord)
}
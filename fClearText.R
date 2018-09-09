fClearText <- function(x) {
  x <- tolower(x)
  #Remove url-s
  x <- gsub("(http(s)?:.*?( |$))|(www\\..*?( |$))", " ", x)
  #Change . or ? or ! to EndOfSentence
  x <- gsub("[.!?]+", " EndOfSentence ", x)
  #Remove unknown Unicode
  x <- gsub("(<U\\+)([0-9A-F]{1,8})([>])", " ", x)
  #Change any digits to NumberOrDigit
  x <- gsub("[0-9]+", " NumberOrDigit ", x)
  #Remove all punctuation
  x <- gsub("[^'[:alpha:]]", " ", x)
  #Remove waterspace
  x <- stripWhitespace(x)
  #Remove obscene words
  urlBadWords <- "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
  vcBadWords <- readLines(urlBadWords)
  x <- removeWords(x, vcBadWords)
  #Remove EndOfSentence and NumberOrDigit in the end of line
  x <- gsub("(([ ]*EndOfSentence[ ]*)|([ ]*NumberOrDigit[ ]*))+$", "", x)
  return(x)
}
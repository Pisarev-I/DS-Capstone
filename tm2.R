library(tm) #Main library
library(stringr) #For str_count
library(RWeka) #For n-grams
library(ggplot2) #For graphs
library(dplyr) #For summarise

#Read the data
setwd("/run/media/galen/WIN_D/Galen/Study/Coursera/10_Capstone/Dataset/final")
con <- file("./en_US/en_US.blogs.txt", "r")
vcBlogs <- readLines(con,-1,encoding = "UTF-8", skipNul = TRUE)
close(con)
con <- file("./en_US/en_US.news.txt", "r")
vcNews <- readLines(con,-1,encoding = "UTF-8", skipNul = TRUE)
close(con)
con <- file("./en_US/en_US.twitter.txt", "r")
vcTwitter <- readLines(con,-1,encoding = "UTF-8", skipNul = TRUE)
close(con)
urlBadWords <- "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
vcBadWords <- readLines(urlBadWords)

#Function for Clear text
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
  x <- removeWords(x, vcBadWords)
  #Remove EndOfSentence and NumberOrDigit in the end of line
  x <- gsub("(([ ]*EndOfSentence[ ]*)|([ ]*NumberOrDigit[ ]*))+$", "", x)
  return(x)
}

#Function for predict 10 words
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

dfWordsFreq <- data.frame(word = character(),
                          freq = numeric(),
                          PrevWords = character(),
                          LastWord = character())

dfTestResults <- data.frame(Iteration = numeric(),
                            SampleSize = numeric(),
                            NGrammsCount = numeric(),
                            SuccessPredictionRate = integer(),
                            MemoryNcells = integer(),
                            MemoryVcells = integer())
for (countSampleSize in 1:50) {
  #Create sample
  #constSampleSize = 5000 + ((countSampleSize-1)%/%5)*1000
  constSampleSize = 1000
  numAllLength <- length(vcBlogs)+length(vcNews)+length(vcTwitter)
  numBlogsSampleSize <- (length(vcBlogs)*constSampleSize) %/% numAllLength
  numNewsSampleSize <- (length(vcNews)*constSampleSize) %/% numAllLength
  numTwitterSampleSize <- constSampleSize - numBlogsSampleSize - numNewsSampleSize

  vcBlogSample <- sample(vcBlogs, size = numBlogsSampleSize, replace = FALSE)
  vcNewsSample <- sample(vcNews, size = numNewsSampleSize, replace = FALSE)
  vcTwitterSample <- sample(vcTwitter, size = numTwitterSampleSize, replace = FALSE)

  vcCorpus <- c(vcBlogSample, vcNewsSample, vcTwitterSample)
  rm(vcBlogSample, vcNewsSample, vcTwitterSample)
  #rm(vcBlogSample, vcNewsSample, vcTwitterSample, vcBlogs, vcNews, vcTwitter)
  gc()

  constTestSize = 100
  cTestIDs <- sample(1:length(vcCorpus), constTestSize)
  vcCorpusTest <- vcCorpus[cTestIDs]
  vcCorpusTrain <- vcCorpus[-cTestIDs]

  #Create and clear Corpus
  vcCorpusTrain <- fClearText(vcCorpusTrain)
  crCorpusTrain <- VCorpus(VectorSource(vcCorpusTrain))

  #Create table with unigrams
  UniGramTkn <- function(x) NGramTokenizer(x,Weka_control(min = 1, max = 1,
                                                        delimiters = " "))
  dtmCorpusTrain1G <- DocumentTermMatrix(crCorpusTrain, control = list(tokenize = UniGramTkn))
  cCorpusTrainFreq1G <- sort(colSums(as.matrix(dtmCorpusTrain1G)), decreasing=TRUE)
  dfCorpusTrainFreq1G   <- data.frame(word = names(cCorpusTrainFreq1G),
                                    freq=cCorpusTrainFreq1G)
  dfCorpusTrainFreq1G$word <- as.character(dfCorpusTrainFreq1G$word)
  dfCorpusTrainFreq1G <- dfCorpusTrainFreq1G[-grep("(endofsentence|numberordigit)",
                                         dfCorpusTrainFreq1G$word),]
  rm(dtmCorpusTrain1G,cCorpusTrainFreq1G)
  gc()

  #Create table with bigrams
  BiGramTkn <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2,
                                                        delimiters = " "))
  dtmCorpusTrain2G <- DocumentTermMatrix(crCorpusTrain, control = list(tokenize = BiGramTkn))
  cCorpusTrainFreq2G <- sort(colSums(as.matrix(dtmCorpusTrain2G)), decreasing=TRUE)
  dfCorpusTrainFreq2G   <- data.frame(word=names(cCorpusTrainFreq2G), freq=cCorpusTrainFreq2G)
  dfCorpusTrainFreq2G$word <- as.character(dfCorpusTrainFreq2G$word)
  dfCorpusTrainFreq2G <- dfCorpusTrainFreq2G[-grep("(endofsentence$|numberordigit$)",
                                         dfCorpusTrainFreq2G$word),]
  rm(dtmCorpusTrain2G,cCorpusTrainFreq2G)
  gc()

  #Create table with trigrams
  TriGramTkn <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3,
                                                        delimiters = " "))
  dtmCorpusTrain3G <- DocumentTermMatrix(crCorpusTrain, control = list(tokenize = TriGramTkn))
  cCorpusTrainFreq3G <- sort(colSums(as.matrix(dtmCorpusTrain3G)), decreasing=TRUE)
  dfCorpusTrainFreq3G   <- data.frame(word=names(cCorpusTrainFreq3G), freq=cCorpusTrainFreq3G)
  dfCorpusTrainFreq3G$word <- as.character(dfCorpusTrainFreq3G$word)
  dfCorpusTrainFreq3G <- dfCorpusTrainFreq3G[-grep("(endofsentence$|numberordigit$)",
                                                 dfCorpusTrainFreq3G$word),]
  rm(dtmCorpusTrain3G,cCorpusTrainFreq3G)
  gc()

  #Create table with fourgrams
  FourGramTkn <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4,
                                                         delimiters = " "))
  dtmCorpusTrain4G <- DocumentTermMatrix(crCorpusTrain, control = list(tokenize = FourGramTkn))
  cCorpusTrainFreq4G <- sort(colSums(as.matrix(dtmCorpusTrain4G)), decreasing=TRUE)
  dfCorpusTrainFreq4G   <- data.frame(word=names(cCorpusTrainFreq4G), freq=cCorpusTrainFreq4G)
  dfCorpusTrainFreq4G$word <- as.character(dfCorpusTrainFreq4G$word)
  dfCorpusTrainFreq4G <- dfCorpusTrainFreq4G[-grep("(endofsentence$|numberordigit$)",
                                                 dfCorpusTrainFreq4G$word),]
  rm(dtmCorpusTrain4G,cCorpusTrainFreq4G)
  gc()

  #Create table with fivegrams
  FiveGramTkn <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5,
                                                          delimiters = " "))
  dtmCorpusTrain5G <- DocumentTermMatrix(crCorpusTrain, control = list(tokenize = FiveGramTkn))
  cCorpusTrainFreq5G <- sort(colSums(as.matrix(dtmCorpusTrain5G)), decreasing=TRUE)
  dfCorpusTrainFreq5G   <- data.frame(word=names(cCorpusTrainFreq5G), freq=cCorpusTrainFreq5G)
  dfCorpusTrainFreq5G$word <- as.character(dfCorpusTrainFreq5G$word)
  dfCorpusTrainFreq5G <- dfCorpusTrainFreq5G[-grep("(endofsentence$|numberordigit$)",
                                                 dfCorpusTrainFreq5G$word),]
  rm(dtmCorpusTrain5G,cCorpusTrainFreq5G)
  gc()

  #Create table with sixgrams
  SixGramTkn <- function(x) NGramTokenizer(x, Weka_control(min = 6, max = 6,
                                                          delimiters = " "))
  dtmCorpusTrain6G <- DocumentTermMatrix(crCorpusTrain, control = list(tokenize = SixGramTkn))
  cCorpusTrainFreq6G <- sort(colSums(as.matrix(dtmCorpusTrain6G)), decreasing=TRUE)
  dfCorpusTrainFreq6G   <- data.frame(word=names(cCorpusTrainFreq6G), freq=cCorpusTrainFreq6G)
  dfCorpusTrainFreq6G$word <- as.character(dfCorpusTrainFreq6G$word)
  dfCorpusTrainFreq6G <- dfCorpusTrainFreq6G[-grep("(endofsentence$|numberordigit$)",
                                                 dfCorpusTrainFreq6G$word),]
  rm(dtmCorpusTrain6G,cCorpusTrainFreq6G)
  gc()

  #Connect all n-gramms tables in one table
  dfCorpusTrainFreq1G$PrevWords <- ""
  dfCorpusTrainFreq1G$LastWord <- dfCorpusTrainFreq1G$word

  dfCorpusTrainFreq <- rbind(dfCorpusTrainFreq2G,
                             dfCorpusTrainFreq3G,
                             dfCorpusTrainFreq4G,
                             dfCorpusTrainFreq5G,
                             dfCorpusTrainFreq6G)

  dfCorpusTrainFreq$PrevWords <- word(dfCorpusTrainFreq$word, start = 1, end = -2)
  dfCorpusTrainFreq$LastWord <- word(dfCorpusTrainFreq$word, start = -1)
  dfCorpusTrainFreq <- rbind(dfCorpusTrainFreq1G, dfCorpusTrainFreq)
  rm(dfCorpusTrainFreq1G, dfCorpusTrainFreq2G,
     dfCorpusTrainFreq3G, dfCorpusTrainFreq4G,
     dfCorpusTrainFreq5G, dfCorpusTrainFreq6G)
  gc()
  
  dfWordsFreq <- rbind(dfWordsFreq, dfCorpusTrainFreq)
  rm(dfCorpusTrainFreq)
  gc()
  
  dfWordsFreq <- dfWordsFreq %>% group_by(word, PrevWords, LastWord) %>%
    summarise(freq = sum(freq))
  dfWordsFreq <- cbind(dfWordsFreq[,1],dfWordsFreq[,4],dfWordsFreq[,2],dfWordsFreq[,3])

  vcCorpusTest <- fClearText(vcCorpusTest)

  numWordsInTest <- 0
  numWordsPredict <- 0
  for (strLine in vcCorpusTest) {
    if (str_count(strLine, "\\S+") > 1) {
      for (wordPos in 2:str_count(strLine, "\\S+")) {
        if (wordPos < 7) {
          strPrevWords <- word(strLine, start = 1, end = wordPos-1)
        } else {
          strPrevWords <- word(strLine, start = wordPos-5, end = wordPos-1)
        }
        numWordsInTest <- numWordsInTest + 1
        strRealWord <- word(strLine, start = wordPos, end = wordPos)
        if (strRealWord %in% fNextWordPrediction(strPrevWords)) {
          numWordsPredict <- numWordsPredict + 1
        }
      }
    }
  }

  dfTestResults[countSampleSize,1] <- countSampleSize
  dfTestResults[countSampleSize,2] <- constSampleSize
  dfTestResults[countSampleSize,3] <- nrow(dfWordsFreq)
  dfTestResults[countSampleSize,4] <- numWordsPredict / numWordsInTest
  dfTestResults[countSampleSize,5] <- gc()[1,2]
  dfTestResults[countSampleSize,6] <- gc()[2,2]
  print(countSampleSize)
}


ggplot(data = dfTestResults, aes(x = Iteration, y = SuccessPredictionRate))+
  geom_point()+
  geom_smooth()

ggplot(data = dfTestResults, aes(x = Iteration, y = MemoryNcells+MemoryVcells))+
  geom_point()+
  geom_smooth()

ggplot(data = dfTestResults, aes(x = Iteration, y = NGrammsCount))+
  geom_point()+
  geom_smooth()

gc(verbose = TRUE)

dfWrTmp <- head(dfWordsFreq[order(dfWordsFreq$freq, decreasing = TRUE),], 10000)
dfWrTmp$PrevWordsCount <- str_count(dfWrTmp$PrevWords, "\\S+")


save(dfWordsFreq, file = "dfWordsFreq.RData")

vcTest <- c("A", "B", "C")
paste(vcTest, collapse = "<br/>")


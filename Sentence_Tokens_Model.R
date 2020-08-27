# We can split into sentences
# Then do the n gram dataframe and keep a list of all sentences that include the n-gram
# Test each sentence for how many words match from the input text
# base prediction on which sentence has the most matches

# return all sentences that contain the input phrase
# get the next 1 - 2 words after the input phrase
# see which is most common
# if there is a tie pick first?

# input will be text
# model will store all the sentences
# search sentences for input text - only up to x number of words
# all matched setences look at next two words
# whichever is most common pick that

# to handle stop words in returning
# search up to next five words
# always take first word - keeping taking words until
# you hit a non-stop word followed by a stop word

library(tm)
library(stringr)
library(R.utils)
library(LaF)
library(tokenizers)
library(data.table)
library(readr)

#Count lines in each text source
con <- unz("/home/andspringer17/Final Capstone/Coursera-SwiftKey.zip","final/en_US/en_US.twitter.txt", open="rb")
twitrLines <- countLines(con)
close(con)
con <- unz("/home/andspringer17/Final Capstone/Coursera-SwiftKey.zip","final/en_US/en_US.blogs.txt", open="rb")
blogLines <- countLines(con)
close(con)
con <- unz("/home/andspringer17/Final Capstone/Coursera-SwiftKey.zip","final/en_US/en_US.news.txt", open="rb")
newsLines <- countLines(con)
close(con)

#Paths to each file
pathT <- "/home/andspringer17/Final Capstone/Final Data/en_US/en_US.twitter.txt"
pathB <- "/home/andspringer17/Final Capstone/Final Data/en_US/en_US.blogs.txt"
pathN <- "/home/andspringer17/Final Capstone/Final Data/en_US/en_US.news.txt"

#Sample 10% lines from each text source (leave last 1000 lines of each for testing)
set.seed(101)
twtrSamp <- sample_lines(pathT,twitrLines*.1,nlines=twitrLines-1000)
blogSamp <- sample_lines(pathB,blogLines*.1,nlines=blogLines-1000)
newsSamp <- sample_lines(pathN,newsLines*.1,nlines=newsLines-1000)

allSamp <- list(twitter = twtrSamp, blog = blogSamp, news = newsSamp)
rm(twtrSamp,blogSamp,newsSamp)

allSamp$twitter <- unlist(tokenize_sentences(allSamp$twitter))
allSamp$blog <- unlist(tokenize_sentences(allSamp$blog))
allSamp$news <- unlist(tokenize_sentences(allSamp$news))
#blogSentences <- str_replace_all(blogSentences, "\\$\\d+(?:(\\.|,)\\d+)*", "<dollaramount>")
#first30 <- blogSentences[str_detect(blogSentences, "<dollaramount>")][1:30]

sentCorp <- VCorpus(VectorSource(allSamp))
rm(allSamp)
names(sentCorp) <- c("Twitter", "Blog", "News")
removeRetweet <- function(x) gsub("^RT :| RT :","",x)
sentCorp <- tm_map(sentCorp, content_transformer(removeRetweet))
sentCorp <- tm_map(sentCorp, content_transformer(tolower))
sentCorp <- tm_map(sentCorp, removePunctuation)
sentCorp <- tm_map(sentCorp, removeNumbers)
sentCorp <- tm_map(sentCorp, stemDocument)
saveRDS(sentCorp, "sentenceCorpus.rds")

sentCorp <- c(sentCorp[[1]][[1]],sentCorp[[2]][[1]],sentCorp[[3]][[1]])
saveRDS(sentCorp,"sentenceCharVector.rds")
getwd()
setwd("Final Capstone/")

removeFirstWord <- function(phrase) {
  words <- unlist(str_split(phrase, " "))
  nWords <- length(words)
  if (nWords == 1) {
    return(NULL)
  }
  subWords <- words[2:nWords]
  return(paste(subWords, collapse = " "))
}

trimPrediction <- function(phrase) {
  predicted <- unlist(str_split(phrase, " "))
  if (length(predicted) < 2) {
    return(phrase)
  } else {
    returnPhrase <- predicted[1]
    for (i in predicted[2:length(predicted)]) {
      if (returnPhrase[length(returnPhrase)] %in% stopwords()) {
        returnPhrase <- c(returnPhrase,i)
      } else if (!i %in% stopwords()) {
        returnPhrase <- c(returnPhrase,i)
      } else {break}
    }
    return(paste(returnPhrase, collapse = " "))
  }
}

length(removeFirstWord("sentence"))

predictNext <- function(inputText) {
  inputCorpus <- VCorpus(VectorSource(inputText))
  inputCorpus <- tm_map(inputCorpus, content_transformer(tolower))
  inputCorpus <- tm_map(inputCorpus, removePunctuation)
  inputCorpus <- tm_map(inputCorpus, removeNumbers)
  inputCorpus <- tm_map(inputCorpus, stemDocument)
  detect <- inputCorpus[[1]][[1]]
  detect <- unlist(str_split(detect, " "))
  nWords <- length(detect)
  if (nWords > 5) {
    detect <- paste(detect[(nWords - 4):nWords], collapse = " ")
  } else {
    detect <- paste(detect, collapse = " ")
  }
  sentenceMatches <- sentCorp[str_detect(sentCorp, paste("\\b",detect,"\\b"))]
  while (length(sentenceMatches) == 0 & length(detect) > 0) {
    detect <- removeFirstWord(detect)
    sentenceMatches <- sentCorp2[str_detect(sentCorp2, paste("\\b",detect,"\\b"))]
  }
  nextWordsRegex <- paste("\\b(",detect,")(\\b\\s*([A-Za-z]\\w+)){0,5}")
  nextWords <- str_extract(sentenceMatches, nextWordsRegex)
  nextWords <- str_split_fixed(nextWords, paste("\\s*",detect), n=2)[,2]
  nextWords <- trimws(nextWords, which = "left")
  nextWords <- nextWords[nextWords != ""]
  nextWords <- sapply(nextWords,trimPrediction)
  tab <- sort(table(nextWords),decreasing = TRUE)
  print(paste("detect:",detect,collapse = " "))
  predicted <-names(tab[1:3])
  return(predicted)
}

predictNext("im going to the")

detect <- "of the"

length(a)

tw <- sentCorp[[1]][[1]][[1]]

a <- sentCorp2[str_detect(sentCorp2, paste("\\b",detect,"\\b"))]
a[5]
length(a)
a[22]
myReg <- paste("\\b(",detect,")(\\b\\s*([A-Za-z]\\w+)){0,2}")
b <- str_extract(a, myReg)
length(b)
b[22]
c <- str_split_fixed(b, paste("\\s*",detect), n=2)[,2]
c <- trimws(c,which = "left")
blanks <- which(c == "")
length(blanks)
blanks[1:10]
tab <- sort(table(c),decreasing = TRUE)
names(tab[1])
length(tab)





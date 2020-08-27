#Model 1: High Memory - How will it work
# 1. Store dictionaries with top 90% of n grams (may change to greater than x count if frequency has too many n-grams)
# 2. When text is entered - search the dictionaries for rows that contain the text
# 3. Use those rows to determine a probability
# 4. Probability for an n-gram will be pct of n-grams with next word out of all n-grams that contain the input text
# 5. Somehow weight the prediction based on length of n-gram

# Need to stem the input text so n-grams will match
# How to determine the probability
# Remove stop words or not? -- Leave stop words in 

library(tm)
library(stringr)
library(R.utils)
library(LaF)
library(tokenizers)
library(data.table)

# #Count lines in each text source
# con <- unz("/home/andspringer17/Final Capstone/Coursera-SwiftKey.zip","final/en_US/en_US.twitter.txt", open="rb")
# twitrLines <- countLines(con)
# close(con)
# con <- unz("/home/andspringer17/Final Capstone/Coursera-SwiftKey.zip","final/en_US/en_US.blogs.txt", open="rb")
# blogLines <- countLines(con)
# close(con)
# con <- unz("/home/andspringer17/Final Capstone/Coursera-SwiftKey.zip","final/en_US/en_US.news.txt", open="rb")
# newsLines <- countLines(con)
# 
# #Paths to each file
# pathT <- "/home/andspringer17/Final Capstone/Final Data/en_US/en_US.twitter.txt"
# pathB <- "/home/andspringer17/Final Capstone/Final Data/en_US/en_US.blogs.txt"
# pathN <- "/home/andspringer17/Final Capstone/Final Data/en_US/en_US.news.txt"
# 
# #Sample 10,000 lines from each text source
# set.seed(101)
# twtrSamp <- sample_lines(pathT,twitrLines*.1,nlines=twitrLines)
# blogSamp <- sample_lines(pathB,blogLines*.1,nlines=blogLines)
# newsSamp <- sample_lines(pathN,newsLines*.1,nlines=newsLines)
# 
# allSamp <- list(twitter = twtrSamp, blog = blogSamp, news = newsSamp)
# rm(twtrSamp,blogSamp,newsSamp)
# 
# #create the corpus
# corp <- VCorpus(VectorSource(allSamp))
# rm(allSamp)
# names(corp) <- c("Twitter", "Blog", "News")
# #remove retweet character from twitter (RT :)
# removeRetweet <- function(x) gsub("^RT :| RT :","",x)
# corp <- tm_map(corp, content_transformer(removeRetweet))
# corp <- tm_map(corp, content_transformer(tolower))
# ###  corp <- tm_map(corp,removeWords, stopwords("english"))  --  Don't remove stop words
# #More text cleaning
# corp <- tm_map(corp, removePunctuation)
# #Removes these characters: ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
# corp <- tm_map(corp, removeNumbers)
# corp <- tm_map(corp, stemDocument)

setwd("Final Capstone/")
#saveRDS(corp,"sampledCorpus.rds")

corp <- readRDS("sentenceCorpus.rds")

corp <- tm_map(corp,removeWords, stopwords("english"))

#Create 2-gram matrices
twtrBigram <- tokenize_ngrams(corp[["Twitter"]][[1]],n=2)
twtrBigram <- data.table(bigrams = unlist(twtrBigram))
blogBigram <- tokenize_ngrams(corp[["Blog"]][[1]],n=2)
blogBigram <- data.table(bigrams = unlist(blogBigram))
newsBigram <- tokenize_ngrams(corp[["News"]][[1]],n=2)
newsBigram <- data.table(bigrams = unlist(newsBigram))
#   Frequency of all 2-grams
allBigram <- rbind(twtrBigram, blogBigram, newsBigram)
rm(twtrBigram, blogBigram, newsBigram)
allBigram <- allBigram[, .N, by = bigrams][order(-N)]
allBigram <- allBigram[!is.na(bigrams),]
countAllBi <- nrow(allBigram)
#   Keep only 2-grams that make up top 90%
#   Changed to 2-grams with greater than 2 occurences to balance volume with predictive power
allBigram <- allBigram[N > 2]

allBigramNoStop <- allBigram
saveRDS(allBigramNoStop, "bigramNoStopDF.rds")
saveRDS(allBigram, "bigramDF.rds")
rm(allBigram)

#Create 3-gram matrices
twtrTrigram <- tokenize_ngrams(corp[["Twitter"]][[1]],n=3)
twtrTrigram <- data.table(trigrams = unlist(twtrTrigram))
blogTrigram <- tokenize_ngrams(corp[["Blog"]][[1]],n=3)
blogTrigram <- data.table(trigrams = unlist(blogTrigram))
newsTrigram <- tokenize_ngrams(corp[["News"]][[1]],n=3)
newsTrigram <- data.table(trigrams = unlist(newsTrigram))
#   Frequency of all 3-grams
allTrigram <- rbind(twtrTrigram, blogTrigram, newsTrigram)
rm(twtrTrigram, blogTrigram, newsTrigram)
allTrigram <- allTrigram[, .N, by = trigrams][order(-N)]
allTrigram <- allTrigram[!is.na(trigrams),]
sum(allTrigram[N > 1,N]) / sum(allTrigram[,N])
#   Keep only 3-grams with greater than 2 occurences
allTrigram <- allTrigram[N > 2]

saveRDS(allTrigram, "trigramDF.rds")
rm(allTrigram)

#Create 4-gram matrices
twtrQuadgram <- tokenize_ngrams(corp[["Twitter"]][[1]],n=4)
twtrQuadgram <- data.table(quadgrams = unlist(twtrQuadgram))
blogQuadgram <- tokenize_ngrams(corp[["Blog"]][[1]],n=4)
blogQuadgram <- data.table(quadgrams = unlist(blogQuadgram))
newsQuadgram <- tokenize_ngrams(corp[["News"]][[1]],n=4)
newsQuadgram <- data.table(quadgrams = unlist(newsQuadgram))
#   Frequency of all 4-grams
allQuadgram <- rbind(twtrQuadgram, blogQuadgram, newsQuadgram)
rm(twtrQuadgram, blogQuadgram, newsQuadgram)
allQuadgram <- allQuadgram[,  .N, by = quadgrams][order(-N)]
allQuadgram <- allQuadgram[!is.na(quadgrams),]
sum(allQuadgram[N > 1,N]) / sum(allQuadgram[,N])
#   Keep only 4-grams with greater than 2 occurences
allQuadgram <- allQuadgram[N > 2]

saveRDS(allQuadgram, "quadgramDF.rds")
rm(allQuadgram)


#saveRDS() and readRDS()

matchNoStopWords <- function(lookFor) {
  ngramMatch <- 
    allBigramNoStop[str_detect(bigrams, paste0("^",lookFor)),bigrams][1]
  return(ngramMatch)
}


matchNgram <- function(lookFor, N) {
  ngramMatch <- 
    if (N == 2) {
      allBigram[str_detect(bigrams, paste0("^",lookFor)),bigrams][1]
    } else if (N == 3) {
      allTrigram[str_detect(trigrams, paste0("^",lookFor)),trigrams][1]
    } else if (N == 4) {
      allQuadgram[str_detect(quadgrams, paste0("^",lookFor)),quadgrams][1]
    } 
  return(ngramMatch)
}

bigrams <- readRDS("bigramDF.rds")
trigrams <- readRDS("trigramDF.rds")
quadgrams <- readRDS("quadgramDF.rds")

predictNext <- function(string) {
  inputCorpus <- VCorpus(VectorSource(string))
  inputCorpus <- tm_map(inputCorpus, content_transformer(tolower))
  inputCorpus <- tm_map(inputCorpus, removePunctuation)
  inputCorpus <- tm_map(inputCorpus, removeNumbers)
  inputCorpus <- tm_map(inputCorpus, stemDocument)
  textTransform <- inputCorpus[[1]][[1]]
  words <- unlist(str_split(textTransform, " "))
  numWords <- length(words)
  if (numWords > 3) {
    words <- c(words[numWords-2],words[numWords-1],words[numWords])
  }
  # take the last x number of words and check n-gram dictionaries
  if (length(words) == 1) {
    firstMatch <- matchNgram(words[1],2)
    if (is.na(firstMatch)) {
      return("I don't know!")
    }
    nextWord <- unlist(str_split(firstMatch, " "))[[2]]
  } else if (length(words) == 2) {
    trigramMatch <- matchNgram(textTransform,3)
    if (!is.na(trigramMatch)) {
      nextWord <- unlist(str_split(trigramMatch, " "))[[3]]
    } else {
      bigramMatch <- matchNgram(words[2],2)
      if (!is.na(bigramMatch)) {
        nextWord <- unlist(str_split(bigramMatch[1], " "))[[2]]
      } else {
        return("I don't know!")
      }
    }
  } else if (length(words) == 3) {
    quadgramMatch <- matchNgram(textTransform,4)
    if (!is.na(quadgramMatch)) {
      nextWord <- unlist(str_split(quadgramMatch, " "))[[4]]
    } else {
      trigramMatch <- matchNgram(paste(words[2],words[3]),3)
      if (!is.na(trigramMatch)) {
        nextWord <- unlist(str_split(trigramMatch, " "))[[3]]
      } else {
        bigramMatch <- matchNgram(words[3],2)
        if (!is.na(bigramMatch)) {
          nextWord <- unlist(str_split(bigramMatch, " "))[[2]]
        } else {
          return("I don't know!")
        }
      }
    }
  }
  return(nextWord)
}

# next thing if stop word predict a second time

completeSentence <- function(string) {
  nextWord <- predictNext(string)
  if (nextWord %in% stopwords("en")) {
    string <- paste(string,nextWord)
    nextWord2 <- predictNext(string)
    pred <- paste(nextWord, nextWord2)
  } else {
    pred <- nextWord
  }
  return(pred)
}

completeSentence("romantic date at the")

about <- corp[["News"]][[1]][str_detect(corp[["News"]][[1]]," a about")]
length(about)
about

biAbout <- allTrigram[str_detect(allTrigram$trigrams, "a about")]
length(biAbout)
biAbout


# _________________________  SENTENCE TOKENS  ____________________

# We can split into sentences
# Then do the n gram dataframe and keep a list of all sentences that include the n-gram
# Test each sentence for how many words match from the input text
# base prediction on which sentence has the most matches

library(tm)
library(stringr)
library(R.utils)
library(LaF)
library(tokenizers)
library(data.table)


blogSentences <- unlist(tokenize_sentences(blogSamp))
blogSentences[str_detect(blogSentences, "\\$")][1:30]
blogSentences <- str_replace_all(blogSentences, "\\$\\d+(?:(\\.|,)\\d+)*", "<dollaramount>")
first30 <- blogSentences[str_detect(blogSentences, "<dollaramount>")][1:30]
bi <- tokenize_ngrams(first30,n=2)
length(bi)
class(bi)
bi[[2]]




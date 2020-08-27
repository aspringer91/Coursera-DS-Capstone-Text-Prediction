# Application map
# The text corpus will be read in as a character vector when application loads
# Model will store the sentence corpus
# User inputs text
# Model runs to return prediction

# https://aspringer.shinyapps.io/TextPredictionAlgorithm/

library(shiny)
library(tm)
library(stringr)
library(R.utils)
library(SnowballC)

shinyServer(function(input, output, session) {
    
    #Read in the corpus of sentences 
    sentCorp <- readRDS("sentenceCharVector.rds")

    #function to remove the first word from a phrase
    removeFirstWord <- function(phrase) {
        words <- unlist(str_split(phrase, " "))
        nWords <- length(words)
        if (nWords == 1) {
            return(NULL)
        }
        subWords <- words[2:nWords]
        return(paste(subWords, collapse = " "))
    }
    
    #function to trim a phrase so that it ends on the last non-stop word in a sequence
    trimPrediction <- function(phrase) {
        predicted <- unlist(str_split(phrase, " "))
        # if only 1 word then return phrase unchanged
        if (length(predicted) < 2) {
            return(phrase)
        } else {
            returnPhrase <- predicted[1]
            # go through phrase one word at a time
            for (i in predicted[2:length(predicted)]) {
                # if the previous word is a stop word then add the next word to the final phrase
                if (returnPhrase[length(returnPhrase)] %in% stopwords()) {
                    returnPhrase <- c(returnPhrase,i)
                # if the previous word is not a stop word then only add next word if its not a stop word
                } else if (!i %in% stopwords()) {
                    returnPhrase <- c(returnPhrase,i)
                } else {break}
            }
            return(paste(returnPhrase, collapse = " "))
        }
    }
    
    predictNext <- function(inputText) {
        # clean and stem the input text

        inputCorpus <- VCorpus(VectorSource(inputText))
        inputCorpus <- tm_map(inputCorpus, content_transformer(tolower))
        inputCorpus <- tm_map(inputCorpus, removePunctuation)
        inputCorpus <- tm_map(inputCorpus, removeNumbers)
        inputCorpus <- tm_map(inputCorpus, stemDocument)
        detect <- inputCorpus[[1]][[1]]
        detect <- unlist(str_split(detect, " "))
        nWords <- length(detect)
        # take only up to last five words that were entered
        if (nWords > 5) {
            detect <- paste(detect[(nWords - 4):nWords], collapse = " ")
        } else {
            detect <- paste(detect, collapse = " ")
        }
        # find all sentences that have a match with the input text
        sentenceMatches <- sentCorp[str_detect(sentCorp, paste("\\b",detect,"\\b"))]
        # if the searched string returns no matches, remove the first word from the string and search again
        while (length(sentenceMatches) == 0 & length(detect) > 0) {
            detect <- removeFirstWord(detect)
            sentenceMatches <- sentCorp[str_detect(sentCorp, paste("\\b",detect,"\\b"))]
        }
        # for each match get the next words in the sentence up to 5 words
        nextWordsRegex <- paste("\\b(",detect,")(\\b\\s*([A-Za-z]\\w+)){0,4}")
        nextWords <- str_extract(sentenceMatches, nextWordsRegex)
        # remove the input part to leave only the predicted words
        nextWords <- str_split_fixed(nextWords, paste("\\s*",detect), n=2)[,2]
        nextWords <- trimws(nextWords, which = "left")
        nextWords <- nextWords[nextWords != ""]
        # use the stop word logic to trim the prediction
        if (length(nextWords) > 1000) {
            set.seed(17)
            nextWords <- sample(nextWords, 1000)
        }
        nextWords <- sapply(nextWords,trimPrediction)
        if (length(nextWords) == 0) {
            return("the")
        } else {
            # count number of times each phrase occurs in the corpus
            tab <- sort(table(nextWords),decreasing = TRUE)
            #print(paste("detect:",detect,collapse = " "))
            # return the most frequent predicted phrase
            predicted <- names(tab[1])
            return(predicted)
        }
    }
    observeEvent(input$run, {
        output$predictedText <- renderText({
            isolate(predictNext(input$textInput))
        })
    })
 })

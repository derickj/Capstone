library(RCurl)
library(RWeka)
library(tm)
library(ggplot2)
library(stringr)
library(gridExtra)
library(reshape2)

source("utils.R")
source("model.R")

#load("data/unigrams.RData")
#load("data/bigrams.RData")
#load("data/trigrams.RData")
#load("data/quadgrams.RData")
#unigrams <- unigrams[unigrams$tf > 3,]
#bigrams <- bigrams[bigrams$tf > 3,]
#trigrams <- trigrams[trigrams$tf > 2,]
#quadgrams <- quadgrams[quadgrams$tf > 2,]
load("data/unigramsSGT.RData")
load("data/bigramsSGT.RData")
load("data/trigramsSGT.RData")
load("data/quadgramsSGT.RData")
load("data/pentgramsSGT.RData")
unigrams <- ngrams1[ngrams1$tf > 5,]
rm(ngrams1)
bigrams <- ngrams2[ngrams2$tf > 3,]
rm(ngrams2)
trigrams <- ngrams3[ngrams3$tf > 1,]
rm(ngrams3)
quadgrams <- ngrams4[ngrams4$tf > 1,]
rm(ngrams4)
pentgrams <- ngrams5[ngrams5$tf > 1,]
rm(ngrams5)


inputStrings <- c("A pound of bacon, a bouquet, and a case of pretzels",
                  "A pound of bacon, a bouquet, and a case of beer",
                  "A pound of bacon, a bouquet, and a case of cheese",
                  "A pound of bacon, a bouquet, and a case of soda")
inputStrings <- c("you follow me  and make  me  the bluest",
                  "you follow me  and make  me  the smelliest",
                  "you follow me  and make  me  the happiest",
                  "you follow me  and make  me  the saddest"
)
inputStrings <- c("offense still struggling but  the players",
                  "offense still struggling but  the crowd",
                  "offense still struggling but  the referees",
                  "offense still struggling but  the defense"
)
inputStrings <- c("Go on a romantic date at the grocery",
                  "Go on a romantic date at the beach",
                  "Go on a romantic date at the movies",
                  "Go on a romantic date at the mall"
)
inputStrings <- c(
    "pound bacon, bouquet, case",
    "follow please? mean",
    "follow make",
    "offense still struggling",
    "Go romantic date",
    "dust",
    "Love that film and haven't seen it in quite some",
    "Louis will push his long, wet hair out of his eyes with his little",
    "grateful for the good times and keep the faith during the",
    "cutest thing you've ever seen, you must be")
inputStrings <- c(
    "a pound of bacon, a bouquet, and a case of",
    "follow me please? It would mean the",
    "you follow me  and make  me  the ",
    "offense still struggling but  the",
    "Go on a romantic date at the",
    "I'll dust them off and be on my",
    "Love that film and haven't seen it in quite some",
    "Louis will push his long, wet hair out of his eyes with his little",
    "grateful for the good times and keep the faith during the",
    "cutest thing you've ever seen, you must be")

inputStrings <- cleanInput(inputStrings)
inputStrings

nstr <- length(inputStrings)
#nstr <- 3
# testmode = TRUE
testmode = FALSE
simpleBackoff <- TRUE
for (i in 1:nstr)
{
    nextwords <- findWords (inputStrings[i],10, testmode, simpleBackoff)
    n <- min(length(nextwords$nextword),50)
    msg <- paste (i, " string processed (Simple Backoff",inputStrings[i])
    print (msg)
    print(nextwords[1:n,])
    nextwords <- findWords (inputStrings[i],10, testmode, FALSE)
    n <- min(length(nextwords$nextword),50)
    msg <- paste (i, " string processed (SGT & interpolation)",inputStrings[i])
    print (msg)
    print(nextwords[1:n,])
}

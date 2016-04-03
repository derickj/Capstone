library(RCurl)
library(RWeka)
library(tm)
library(ggplot2)
library(stringr)
library(gridExtra)
library(reshape2)

source("utils.R")
source("model.R")

load("data/unigramsSGT.RData")
load("data/bigramsSGT.RData")
load("data/trigramsSGT.RData")
load("data/quadgramsSGT.RData")
unigrams <- ngrams1
rm(ngrams1)
bigrams <- ngrams2
rm(ngrams2)
trigrams <- ngrams3
rm(ngrams3)
quadgrams <- ngrams4
rm(ngrams4)

#Quiz 2
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
#Quiz 2
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
# Quiz 3
#inputStrings <- c(
#    "I'll be there for you, I'd live and I'd sleep",
#    "I'll be there for you, I'd live and I'd die",
#    "I'll be there for you, I'd live and I'd eat",
#    "I'll be there for you, I'd live and I'd give"
#)
# inputStrings <- c(
    # "and he started telling me about his horticultural",
    # "and he started telling me about his marital",
    # "and he started telling me about his spiritual",
    # "and he started telling me about his financial"
# )
# inputStrings <- c(
    # "I'd give anything to see arctic monkeys this weekend",
    # "I'd give anything to see arctic monkeys this decade",
    # "I'd give anything to see arctic monkeys this month",
    # "I'd give anything to see arctic monkeys this morning"
# )
# inputStrings <- c(
    # "same effect as a hug and helps reduce your sleepiness",
    # "same effect as a hug and helps reduce your hunger",
    # "same effect as a hug and helps reduce your stress",
    # "same effect as a hug and helps reduce your happiness"
# )
# inputStrings <- c(
    # "inch away from me but you hadn't time to take a minute",
    # "inch away from me but you hadn't time to take a walk",
    # "inch away from me but you hadn't time to take a look",
    # "inch away from me but you hadn't time to take a picture"
# )
# inputStrings <- c(
    # "a presentation of evidence, and a jury to settle the account",
    # "a presentation of evidence, and a jury to settle the case",
    # "a presentation of evidence, and a jury to settle the incident",
    # "a presentation of evidence, and a jury to settle the matter"
# )
# inputStrings <- c(
    # "an uneven number of bags of groceries in each hand",
    # "an uneven number of bags of groceries in each toe",
    # "an uneven number of bags of groceries in each arm",
    # "an uneven number of bags of groceries in each finger"
# )
# inputStrings <- c(
    # "perfect from the bottom to the side",
    # "perfect from the bottom to the center",
    # "perfect from the bottom to the middle",
    # "perfect from the bottom to the top"
# )
# inputStrings <- c(
    # "filled with imagination and bruises from playing daily",
    # "filled with imagination and bruises from playing inside",
    # "filled with imagination and bruises from playing weekly",
    # "filled with imagination and bruises from playing outside"
# )
# inputStrings <- c(
    # "same people are in almost all of Adam Sandler's pictures",
    # "same people are in almost all of Adam Sandler's movies",
    # "same people are in almost all of Adam Sandler's stories",
    # "same people are in almost all of Adam Sandler's films"
# )
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
    "cutest thing you've ever seen, you must be",
    "I'll be there for you, I'd live and I'd",
    "and he started telling me about his",
    "I'd give anything to see arctic monkeys this",
    "same effect as a hug and helps reduce your",
    "inch away from me but you hadn't time to take a",
    "a presentation of evidence, and a jury to settle the",
    "an uneven number of bags of groceries in each",
    "perfect from the bottom to the",
    "filled with imagination and bruises from playing",
    "same people are in almost all of Adam Sandler's")
inputStrings <- cleanInput(inputStrings)
inputStrings

nstr <- length(inputStrings)
simpleBackoff <- TRUE
for (i in 1:nstr)
{
    nextwords <- findWords (inputStrings[i], 10, simpleBackoff)
    n <- min(length(nextwords$nextword),10)
    msg <- paste (i, " string processed (Simple Backoff",inputStrings[i])
    print (msg)
    print(nextwords[1:n,])
    nextwords <- findWords (inputStrings[i],10, FALSE)
    n <- min(length(nextwords$nextword),10)
    msg <- paste (i, " string processed (SGT & interpolation)",inputStrings[i])
    print (msg)
    print(nextwords[1:n,])
}

library(RCurl)
library(RWeka)
library(tm)
library(ggplot2)
library(stringr)
library(gridExtra)
library(reshape2)

load("data/unigramsSGT.RData")
load("data/bigramsSGT.RData")
load("data/trigramsSGT.RData")
load("data/quadgramsSGT.RData")
#load("data/pentgramsSGT.RData")
unigrams <- ngrams1
rm(ngrams1)
bigrams <- ngrams2
rm(ngrams2)
trigrams <- ngrams3
rm(ngrams3)
quadgrams <- ngrams4
rm(ngrams4)
#pentgrams <- ngrams5
#rm(ngrams5)

source("utils.R")
source("model.R")
source("benchmark.R")

benchmark(predict.baseline, 
          # additional parameters to be passed to the prediction function can be inserted here
          sent.list = list('tweets' = tweets, 
                           'blogs' = blogs), 
          ext.output = T)

testfile <- "data/cleantest.txt"
if(!file.exists(testfile)) {
    msg <- paste(testfile, "does not exist", sep=" ")
    stop(msg)
}
textlines <- readLines(testfile)
nlines <- length(textlines)
#nstr <- 3
simpleBackoff <- TRUE
processed <- 0
correct <- 0
#for (i in 1:1)
#{
#    delim <- "[ \r\n\t]+"
#    x <- textlines[i]
#    words <- unlist(strsplit(x, delim))
#    cnt <- length(words)
#    nend <- cnt - 1
#    for (j in 1:nend)
#    {
#        nextw <- words[j + 1]
#        inputphrase <- unlist(paste(unlist(words[1:j]), collapse=" "))
#        nextwords <- findWords (inputphrase,1, FALSE)
#        processed <- processed + 1
#        if (nextwords$nextword[1] == nextw)
#        {
#           correct <- correct + 1
#        }
#        msg <- paste (correct, " Phrase (",i,") of ",processed," - predicted ",nextwords$nextword[1], " for ",nextw)
#        print (msg)
#    }
#    
#    nextwords <- findWords (inputStrings[i], 10, simpleBackoff)
#    n <- min(length(nextwords$nextword),10)
#    msg <- paste (i, " string processed (Simple Backoff",inputStrings[i])
#    print (msg)
#    print(nextwords[1:n,])
#}
msg <- paste (correct, " words predicted out of ",processed)
print (msg)

#
# Load the required libraries
#
library(shiny)
library(RCurl)
library(RWeka)
library(tm)
library(ggplot2)
library(stringr)
library(gridExtra)
library(reshape2)

load("unigramsSGT.RData")
load("bigramsSGT.RData")
load("trigramsSGT.RData")
load("quadgramsSGT.RData")
unigrams <- ngrams1[ngrams1$tf > 5,]
rm(ngrams1)
bigrams <- ngrams2[ngrams2$tf > 2,]
rm(ngrams2)
trigrams <- ngrams3[ngrams3$tf > 1,]
rm(ngrams3)
quadgrams <- ngrams4[ngrams4$tf > 1,]
rm(ngrams4)

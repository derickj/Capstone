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
unigrams <- ngrams1
rm(ngrams1)
bigrams <- ngrams2
rm(ngrams2)
trigrams <- ngrams3
rm(ngrams3)
quadgrams <- ngrams4
rm(ngrams4)

badwords <- readLines("bad-words.txt")


library(knitr)
library(RCurl)
library(RWeka)
library(tm)
library(ggplot2)
library(stringr)
library(gridExtra)
library(reshape2)

cleanfile <- "cleantext.txt"
mycorpus <- Corpus(DirSource("./", pattern = cleanfile),
                   readerControl = list(reader = readPlain,
                                        language = "en_US",
                                        load = TRUE))
summary(mycorpus[[1]])
#tdm <- TermDocumentMatrix(mycorpus)
#unigrams <- data.frame(tdm$v, tdm$dimnames$Terms, stringsAsFactors = FALSE)
#colnames(unigrams) <- c ("tf","term")
#unigrams <- unigrams[order(unigrams$tf, decreasing=TRUE),]
delim <- ' \r\n\t'
BigramTokenizer <- function(x) 
{
    NGramTokenizer(x, Weka_control(min=2, max=2, delimiters=delim))
}
makeTDM2 <- function(x) {
    tdm <- TermDocumentMatrix(x, control=list(tokenize=BigramTokenizer))
    return(tdm)
}
TrigramTokenizer <- function(x) 
{
    NGramTokenizer(x, Weka_control(min=3, max=3, delimiters=delim))
}

makeTDM3 <- function(x) {
    tdm <- TermDocumentMatrix(x, control=list(tokenize=TrigramTokenizer))
    return(tdm)
}
#
# Determine frequency of bigrams (sequences of 2 "words") 
#
tdm2 <- makeTDM2(mycorpus)
bigrams <- data.frame(tdm2$v, tdm2$dimnames$Terms, stringsAsFactors = FALSE)
colnames(bigrams) <- c ("tf", "term")
bigrams <- bigrams[order(bigrams$tf, decreasing=TRUE),]
#
# Determine frequency of trigrams (sequences of 3 "words") 
#
tdm3 <- makeTDM3(mycorpus)
trigrams <- data.frame(tdm3$v, tdm3$dimnames$Terms, stringsAsFactors = FALSE)
colnames(trigrams) <- c ("tf","term")
trigrams <- trigrams[order(trigrams$tf, decreasing=TRUE),]

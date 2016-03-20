library(knitr)
library(RCurl)
library(RWeka)
library(tm)
library(ggplot2)
library(stringr)
library(gridExtra)
library(reshape2)

unkfile <- "unktext.txt"
mycorpus <- Corpus(DirSource("./", pattern = unkfile),
                   readerControl = list(reader = readPlain,
                                        language = "en_US",
                                        load = TRUE))
summary(mycorpus[[1]])
print("Read the file")
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
QuadgramTokenizer <- function(x) 
{
    NGramTokenizer(x, Weka_control(min=4, max=4, delimiters=delim))
}
makeTDM4 <- function(x) {
    tdm <- TermDocumentMatrix(x, control=list(tokenize=QuadgramTokenizer))
    return(tdm)
}
PentgramTokenizer <- function(x) 
{
    NGramTokenizer(x, Weka_control(min=5, max=5, delimiters=delim))
}
makeTDM5 <- function(x) {
    tdm <- TermDocumentMatrix(x, control=list(tokenize=PentgramTokenizer))
    return(tdm)
}
#
# Determine frequency of bigrams (sequences of 2 "words") 
#
tdm2 <- makeTDM2(mycorpus)
bigrams <- data.frame(tdm2$v, tdm2$dimnames$Terms, stringsAsFactors = FALSE)
colnames(bigrams) <- c ("tf", "term")
bigrams <- bigrams[order(bigrams$tf, decreasing=TRUE),]
head(bigrams)
n <- length(bigrams$tf)
save(tdm2,file = "tdmbigrams.RData")
save(bigrams,file = "bigrams.RData")
msg <- paste(n, "bigrams written to files")
print (msg)
#
# Determine frequency of trigrams (sequences of 3 "words") 
#
tdm3 <- makeTDM3(mycorpus)
trigrams <- data.frame(tdm3$v, tdm3$dimnames$Terms, stringsAsFactors = FALSE)
colnames(trigrams) <- c ("tf","term")
trigrams <- trigrams[order(trigrams$tf, decreasing=TRUE),]
head(trigrams)
n <- length(trigrams$tf)
save(tdm3,file = "tdmtrigrams.RData")
save(trigrams,file = "trigrams.RData")
msg <- paste(n, "trigrams written to files")
print (msg)
#
# Determine frequency of quadgrams (sequences of 4 "words") 
#
tdm4 <- makeTDM4(mycorpus)
quadgrams <- data.frame(tdm4$v, tdm4$dimnames$Terms, stringsAsFactors = FALSE)
colnames(quadgrams) <- c ("tf","term")
quadgrams <- quadgrams[order(quadgrams$tf, decreasing=TRUE),]
head(quadgrams)
n <- length(quadgrams$tf)
save(tdm4,file = "tdmquadgrams.RData")
save(quadgrams,file = "quadgrams.RData")
msg <- paste(n, "quadgrams written to files")
print (msg)
#
# Determine frequency of pentgrams (sequences of 5 "words") 
#
tdm5 <- makeTDM5(mycorpus)
pentgrams <- data.frame(tdm5$v, tdm5$dimnames$Terms, stringsAsFactors = FALSE)
colnames(pentgrams) <- c ("tf","term")
pentgrams <- pentgrams[order(pentgrams$tf, decreasing=TRUE),]
head(pentgrams)
n <- length(pentgrams$tf)
save(tdm5,file = "tdmpentgrams.RData")
save(pentgrams,file = "pentgrams.RData")
msg <- paste(n, "pentgrams written to files")
print (msg)

#n2 <- data.frame(tdm2$v, tdm2$dimnames$Terms, stringsAsFactors = FALSE)
#colnames(n2) <- c ("tf","term")
#n2words <- sapply(n2$term, str_split," ",2)
#n2frame <- data.frame(n2words, stringsAsFactors = FALSE)
#row.names(n2frame) <- NULL
#rm(n2words)
#n2a <- cbind(n2[,1],n2frame)
#rm(n2,n2frame)
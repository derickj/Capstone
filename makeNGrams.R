options("java.parameters")
options(java.parameters = "-Xmx1g")
library(RCurl)
library(RWeka)
library(tm)
library(stringr)
library(reshape2)

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
cleanfile <- "cleantext.txt"
mycorpus <- Corpus(DirSource("./data/", pattern = cleanfile),
                   readerControl = list(reader = readPlain,
                                        language = "en_US",
                                        load = TRUE))
#summary(mycorpus[[1]])
print("Read the file")
#
# Determine frequency of words 
#
tdm <- TermDocumentMatrix(mycorpus)
unigrams <- data.frame(tdm$v, tdm$dimnames$Terms, stringsAsFactors = FALSE)
colnames(unigrams) <- c ("tf","term")
unigrams <- unigrams[order(unigrams$tf, decreasing=TRUE),]
n <- length(unigrams$tf)
save (tdm,file = "data/tdmunigrams.RData")
save (unigrams, file = "data/unigrams.RData")
msg <- paste(n, "unigrams (words) written to files")
print (msg)
rm(tdm)

#
# Determine frequency of bigrams (sequences of 2 "words") 
#
tdm2 <- makeTDM2(mycorpus)
bigrams <- data.frame(tdm2$v, tdm2$dimnames$Terms, stringsAsFactors = FALSE)
colnames(bigrams) <- c ("tf", "term")
bigrams <- bigrams[order(bigrams$tf, decreasing=TRUE),]
head(bigrams)
n <- length(bigrams$tf)
save(tdm2,file = "data/tdmbigrams.RData")
save(bigrams,file = "data/bigrams.RData")
msg <- paste(n, "bigrams written to files")
print (msg)
rm(tdm2)
#
# Determine frequency of trigrams (sequences of 3 "words") 
#
tdm3 <- makeTDM3(mycorpus)
trigrams <- data.frame(tdm3$v, tdm3$dimnames$Terms, stringsAsFactors = FALSE)
colnames(trigrams) <- c ("tf","term")
trigrams <- trigrams[order(trigrams$tf, decreasing=TRUE),]
head(trigrams)
n <- length(trigrams$tf)
save(tdm3,file = "data/tdmtrigrams.RData")
save(trigrams,file = "data/trigrams.RData")
msg <- paste(n, "trigrams written to files")
print (msg)
rm(tdm3)
#
# Determine frequency of quadgrams (sequences of 4 "words") 
#
tdm4 <- makeTDM4(mycorpus)
quadgrams <- data.frame(tdm4$v, tdm4$dimnames$Terms, stringsAsFactors = FALSE)
colnames(quadgrams) <- c ("tf","term")
quadgrams <- quadgrams[order(quadgrams$tf, decreasing=TRUE),]
head(quadgrams)
n <- length(quadgrams$tf)
save(tdm4,file = "data/tdmquadgrams.RData")
save(quadgrams,file = "data/quadgrams.RData")
msg <- paste(n, "quadgrams written to files")
print (msg)
rm(tdm4)
#
# Determine frequency of pentgrams (sequences of 5 "words") 
#
tdm5 <- makeTDM5(mycorpus)
pentgrams <- data.frame(tdm5$v, tdm5$dimnames$Terms, stringsAsFactors = FALSE)
colnames(pentgrams) <- c ("tf","term")
pentgrams <- pentgrams[order(pentgrams$tf, decreasing=TRUE),]
head(pentgrams)
n <- length(pentgrams$tf)
save(tdm5,file = "data/tdmpentgrams.RData")
save(pentgrams,file = "data/pentgrams.RData")
msg <- paste(n, "pentgrams written to files")
print (msg)
rm(tdm5)

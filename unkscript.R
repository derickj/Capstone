library(knitr)
library(RCurl)
library(RWeka)
library(tm)
library(ggplot2)
library(stringr)
library(gridExtra)
library(reshape2)

cleanfile <- "cleantext.txt"
if(!file.exists(cleanfile)) {
    msg <- paste(cleanfile, "does not exist", sep=" ")
    stop(msg)
}
mycorpus <- Corpus(DirSource("./", pattern = cleanfile),
                   readerControl = list(reader = readPlain,
                                        language = "en_US",
                                        load = TRUE))
summary(mycorpus[[1]])
tdm <- TermDocumentMatrix(mycorpus)

unigrams <- data.frame(tdm$v, tdm$dimnames$Terms, stringsAsFactors = FALSE)
colnames(unigrams) <- c ("tf","term")
unigrams <- unigrams[order(unigrams$tf, decreasing=TRUE),]
# Assume words only occurring once or twice are the same as <UNK> unknown words in the "to be predicted" vocabulary
unkset <- unigrams[unigrams$tf < 3,]
cleantext <- mycorpus[[1]]$content

#replaceUNKword <- function(x,word) 
#{
#    pattern <- paste("[ \r\n]",word,"[ \r\n]|^",word,"[ \r\n]|[ \r\n]", word, "$", sep ="")
#    gsub(pattern, " UNK ", x)
#}
#n <- length(unkset$term)
# Takes forever to run on large file
#for (i in 1:n)
#{
#    term <- unkset$term[i]
#    cleantext <- replaceUNKword(cleantext,term)
#    msg <- paste("Replaced", term, "we have now done",i,n-i,"left", sep = " ")
#    print(msg)
#}
unkLine <- function(textLine,unkterms) 
{
    unkWords <- function(x, theunks) 
    {
        if (x %in% theunks) 
        {
            x <- "UNK"
        } 
        else 
        {
            x
        }
    }
    words <- unlist(strsplit(textLine, " "))
    words <- lapply(words, unkWords, unkterms)
    unlist(paste(unlist(words), collapse=" "))
}
n <- length(cleantext)
for (i in 1:n)
{
    cleantext[i] <- unkLine (cleantext[i],unkset$term)
    msg <- paste("Means we have now done",i,n-i,"left", sep = " ")
    print(msg)
}
unklst <- grep (" UNK ", cleantext)
msg <- paste (length(unklst), 
              "lines contained rare words which were replaced with UNK", 
              sep = " ")
print (msg)
# [1] "39224 lines contained rare words which were replaced with UNK"
unkfile <- "unktext.txt"
writeLines(cleantext, unkfile, useBytes = TRUE)

# Save data for potential re-use
save (tdm,file = "tdmunigrams.RData")
save (unigrams, file = "unigrams.RData")

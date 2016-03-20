library(knitr)
library(RCurl)
library(RWeka)
library(tm)
library(ggplot2)
library(stringr)
library(gridExtra)
library(reshape2)

load("unigrams.RData")
load("bigrams.RData")
load("trigrams.RData")
load("quadgrams.RData")

removePunctuationAndForeign <- function(x) 
{
    # Keep only letters, numbers and spaces, special treatment for certain apostrophes
    x <- gsub("n't", "nt", x)
    x <- gsub("'s", "s", x)
    x <- gsub("'re", " are", x)
    x <- gsub("'ve", " have", x)
    x <- gsub("i'm", "i am", x)
    x <- gsub("[^[:alnum:][:blank:]]", " ", x)
    x <- gsub("â", "", x)
    gsub("[šžþÃàáâãäåçèéêëìíîïðñòóôõöùúûüý¢]+", " ", x)
}

removeNumbers <- function(x) 
{
    gsub("[0-9]+", "", x)
}

collapseWhiteSpace <- function(x) 
{
    x <- gsub(" +"," ", x)
}

cleanInput <- function (x)
{
    x <- removePunctuationAndForeign(x)
    x <- tolower(x)
    x <- removeNumbers(x)
    collapseWhiteSpace(x)    
}

lastnwords <- function (x,n)
{
    words <- unlist(strsplit(x, " "))
    cnt <- length(words)
    nbeg <- cnt - n + 1
    if ((nbeg > 0) && (nbeg <= cnt))
    {
        words <- words[nbeg:cnt]
        x <- unlist(paste(unlist(words), collapse=" "))
    }
    x
}
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

allstrings <- c(
                  "follow me please? It would mean the",
                  "you follow me  and make  me  the ",
                  "offense still struggling but  the",
                  "Go on a romantic date at the",
                  "I'll dust them off and be on my",
                  "Love that filem and haven't seen it in quite some",
                  "Louis will push his long, wet hair out of his eyes with his little",
                  "grateful for the good times and keep the faith during the",
                  "cutest thing you've ever seen, you must be")
inputStrings <- unlist(lapply(inputStrings,cleanInput))
inputStrings

x <- inputStrings[1]

simpleCalc <- function (x, nrows = 10, testing = FALSE)
{
    xtra = 0
    if (testing == TRUE)
    {
        xtra = 1
    }
    mtchstr <- lastnwords(x,3 + xtra)
    pattern <- paste("^",mtchstr," ", sep="")
    mtchterm4 <- quadgrams[grep(pattern,quadgrams$term),]
    nlen <- length(mtchterm4$tf)
    result <- data.frame(0,"unk","unk",stringsAsFactors = FALSE)
    colnames(result) <- c("relfreq","word","orig")
    if (nlen > 0)
    {
        denom <- sum(mtchterm4$tf)
        mtchterm4$relfreq <- mtchterm4$tf / denom
        mtchterm4$word <- unlist(lapply(mtchterm4$term,lastnwords,1))
        # mtchterm4 <- mtchterm4[mtchterm4$word != "unk",]
        nlen <- length(mtchterm4$tf)
        if (nlen > 0)
        {
            mtchterm4 <- mtchterm4[order(mtchterm4$relfreq,decreasing=TRUE),]
            mtchterm4 <- mtchterm4[1:min(nlen,nrows),]
            result4 <- data.frame(mtchterm4$relfreq,mtchterm4$word,stringsAsFactors = FALSE)
            result4$orig <- "Quad"
            colnames(result4) <- c("relfreq","word","orig")
            result <- rbind(result,result4)
        }
    }

    mtchstr <- lastnwords(x,2 + xtra)
    pattern <- paste("^",mtchstr," ", sep="")
    mtchterm3 <- trigrams[grep(pattern,trigrams$term),]
    nlen <- length(mtchterm3$tf)
    if (nlen > 0)
    {
        denom <- sum(mtchterm3$tf)
        mtchterm3$relfreq <- mtchterm3$tf / denom * 0.4
        mtchterm3$word <- unlist(lapply(mtchterm3$term,lastnwords,1))
        # mtchterm3 <- mtchterm4[mtchterm3$word != "unk",]
        nlen <- length(mtchterm3$tf)
        if (nlen > 0)
        {
            mtchterm3 <- mtchterm3[order(mtchterm3$relfreq,decreasing=TRUE),]
            mtchterm3 <- mtchterm3[1:min(nlen,nrows),]
            result3 <- data.frame(mtchterm3$relfreq,mtchterm3$word,stringsAsFactors = FALSE)
            result3$orig <- "Tri"
            colnames(result3) <- c("relfreq","word","orig")
            result <- rbind(result,result3)
        }
    }
    
    mtchstr <- lastnwords(x,1 + xtra)
    pattern <- paste("^",mtchstr," ", sep="")
    mtchterm2 <- bigrams[grep(pattern,bigrams$term),]
    nlen <- length(mtchterm2$tf)
    if (nlen > 0)
    {
        denom <- sum(mtchterm2$tf)
        mtchterm2$relfreq <- mtchterm2$tf / denom * 0.4
        mtchterm2$word <- unlist(lapply(mtchterm2$term,lastnwords,1))
        # mtchterm2 <- mtchterm2[mtchterm2$word != "unk",]
        nlen <- length(mtchterm2$tf)
        if (nlen > 0)
        {
            mtchterm2 <- mtchterm2[order(mtchterm2$relfreq,decreasing=TRUE),]
            mtchterm2 <- mtchterm2[1:min(nlen,nrows),]
            result2 <- data.frame(mtchterm2$relfreq,mtchterm2$word,stringsAsFactors = FALSE)
            result2$orig <- "Bi"
            colnames(result2) <- c("relfreq","word","orig")
            result <- rbind(result,result2)
        }
    }
    if (testing == TRUE)
    {
        mtchstr <- lastnwords(x,1)
        pattern <- paste("^",mtchstr, "$", sep="")
        mtchterm1 <- unigrams[grep(pattern,unigrams$term),]
        nlen <- length(mtchterm1$tf)
        if (nlen > 0)
        {
            denom <- sum(unigrams$tf)
            mtchterm1$relfreq <- mtchterm1$tf / denom * 0.4
            mtchterm1$word <- mtchstr
            result1 <- data.frame(mtchterm1$relfreq,mtchterm1$word,stringsAsFactors = FALSE)
            result1$orig <- "Uni"
            colnames(result1) <- c("relfreq","word","orig")
            result <- rbind(result,result1)
        }
    }
    result <- result[result$word != "unk",] 
    result[order(result$relfreq,decreasing = TRUE),]
#    head(mtchterm2[order(mtchterm2$relfreq,decreasing=TRUE),])
}

nstr <- length(inputStrings)
testmode = TRUE
for (i in 1:nstr)
{
    nextwords <- simpleCalc(inputStrings[i],10, testmode)
    n <- min(length(nextwords$relfreq),5)
    print(nextwords[1:n,])
}

# Reading the data
#library(RCurl)
#data_url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
#download.file(data_url, "Coursera-SwiftKey.zip", method = "auto")
# extract zip files
#unzip("Coursera-SwiftKey.zip")
library(tm)
# txt <- system.file("texts", "txt", package = "tm")
engcorpus <- Corpus(DirSource("final/en_US"),
                  readerControl = list(reader = readPlain,
                  language = "en_US",
                  load = TRUE))
meta(engcorpus[[1]])
head(engcorpus[[1]]$content)
meta(engcorpus[[2]])
head(engcorpus[[2]]$content)
meta(engcorpus[[3]])
head(engcorpus[[3]]$content)

# Rweka
library("RWeka")
library("tm")

data("crude")
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(crude, control = list(tokenize = BigramTokenizer))
inspect(tdm[340:345,1:10])
plot(tdm, terms = findFreqTerms(tdm, lowfreq = 2)[1:50], corThreshold = 0.5)

crude <- as.VCorpus(crude)
crude <- tm_map(crude, stripWhitespace)
crude <- tm_map(crude, content_transformer(tolower))
crude <- tm_map(crude, removeWords, stopwords("english"))
crude <- tm_map(crude, stemDocument)
# Sets the default number of threads to use
options(mc.cores=1)
tdm <- TermDocumentMatrix(crude, control=list(tokenize = NGramTokenizer))
findFreqTerms(tdm, lowfreq = 10)
# OR
NGramTokenizer(crude[[1]])
# Also see
WOW("NGramTokenizer")

# Examples from tm library
MC_tokenizer(crude[[1]])
scan_tokenizer(crude[[1]])
# Custom tokenizer
strsplit_space_tokenizer <- function(x)
    unlist(strsplit(as.character(x), "[[:space:]]+"))
strsplit_space_tokenizer(crude[[1]])

# Another custom function
library(ngram)
bigramTokenizer <- function(x) {
    x <- as.character(x)
    
    # Find words
    one.list <- c()
    tryCatch({
        one.gram <- ngram::ngram(x, n = 1)
        one.list <- ngram::get.ngrams(one.gram)
    }, 
    error = function(cond) { warning(cond) })
    
    # Find 2-grams
    two.list <- c()
    tryCatch({
        two.gram <- ngram::ngram(x, n = 2)
        two.list <- ngram::get.ngrams(two.gram)
    },
    error = function(cond) { warning(cond) })
    
    res <- unlist(c(one.list, two.list))
    res[res != '']
}
# Then you can test the function with:
dtmTest <- lapply(myCorpus.3, bigramTokenizer)
# And finally:
dtm <- DocumentTermMatrix(myCorpus.3, control = list(tokenize = bigramTokenizer))
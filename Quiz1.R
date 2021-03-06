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
tm_map(engcorpus, FUN = stripWhitespace)
# grep("woman",engcorpus[[3]]$content,ignore.case=TRUE)
# or
# grepl("woman",engcorpus[[3]]$content,ignore.case=TRUE)
# Quiz 1
# Question 1
# Not! object.size(engcorpus[[1]]$content)/(1024*1024)
sum(nchar(engcorpus[[1]]$content))/(1024*1024)
# Question 2
length(engcorpus[[3]]$content)
# Question 3
ndoc <- length(engcorpus)
maxlen <- 0
maxndx <- 0
for (i in 1:ndoc) 
{
  n <- max(nchar(engcorpus[[i]]$content))
  if (n > maxlen) 
  {
    maxlen <- n
    maxndx <- i
  }
}
engcorpus[[maxndx]]$meta$id
maxlen
# Question 4
nlove <- length(grep ("love",engcorpus[[3]]$content,ignore.case=FALSE))
nhate <- length(grep ("hate",engcorpus[[3]]$content,ignore.case=FALSE))
nlove/nhate
# Question 5
engcorpus[[3]]$content[grep("biostats",engcorpus[[3]]$content)]
# Question 6
engcorpus[[3]]$content[grep("^A computer once beat me at chess, but it was no match for me at kickboxing$",engcorpus[[3]]$content)]


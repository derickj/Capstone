library(RCurl)
library(tm)

source("utils.R")
#
# Read the data from the files, clean and write to new files
#
trainfile <- "data/traintext.txt"
if(!file.exists(trainfile)) {
    msg <- paste(trainfile, "does not exist", sep=" ")
    stop(msg)
}
cleantext <- readLines(trainfile)
#
# Function to strip out punctuation and foreign characters, convert to lower case, remove numbers, strip extra whitespace
#
cleantext <- cleanInput(cleantext)
cleanfile <- "data/cleantext.txt"
writeLines(cleantext, cleanfile, useBytes = TRUE)

cleantext <- cleanInput(cleantext, TRUE)
cleanfile <- "data/trainnostop.txt"
writeLines(cleantext, cleanfile, useBytes = TRUE)

testfile <- "data/testtext.txt"
if(!file.exists(testfile)) {
    msg <- paste(testfile, "does not exist", sep=" ")
    stop(msg)
}
cleantest <- readLines(testfile)
#
# Function to strip out punctuation and foreign characters, convert to lower case, remove numbers, strip extra whitespace
#
cleantest <- cleanInput(cleantest)
cleanfile <- "data/cleantest.txt"
writeLines(cleantest, cleanfile, useBytes = TRUE)

valifile <- "data/valitext.txt"
if(!file.exists(valifile)) {
    msg <- paste(valifile, "does not exist", sep=" ")
    stop(msg)
}
cleanvali <- readLines(valifile)
#
# Function to strip out punctuation and foreign characters, convert to lower case, remove numbers, strip extra whitespace
#
cleanvali <- cleanInput(cleanvali)
cleanfile <- "data/cleanvali.txt"
writeLines(cleanvali, cleanfile, useBytes = TRUE)


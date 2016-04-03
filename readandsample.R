library(RCurl)
library(RWeka)
library(tm)
#library(stringr)
#
# Loading the file from the URL
#
#data_url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
zipfile <- "Swiftkey_data.zip"
#download.file(data_url, outfile, method = "auto")
#
# Check whether the load was successful and if so, unzip
#
if(!file.exists(zipfile)) {
    msg <- paste(zipfile, "does not exist", sep=" ")
    stop(msg)
}
#unzip(zipfile)
#
# After investigating what was unpacked from the ZIP file, obtain a vector with all the .txt file names 
# in the en_US locale directory
#
engdir <- "final/en_US"
if(!file.exists(engdir)) {
    msg <- paste(engdir, "does not exist in working directory", sep=" ")
    stop(msg)
}
files <- list.files(engdir, pattern = "*.txt")
if (length(files)<1)
{
    msg <- paste(engdir, "does not contain .txt files", sep=" ")
    stop(msg)
}
#
# Set a seed so that the sampling is reproducible and randomly select 5% of the data strings for inclusion
#
set.seed(12321)
#set.seed(42)
#set.seed(6007)
#set.seed(9115)
samplesize <- 0.05
#
# Cycle through the list of file names, reading each file as binary before converting to ASCII
#
thedata <- NULL
for (i in 1:length(files))
{
    dataname <- paste(engdir,files[i],sep="/")
    con <- file(dataname, "rb")
    textvector <- readLines(con, 
                            #                         n = 20000,  # Comment out for final version
                            encoding="UTF-8")
    close(con)
    iconv(textvector, "UTF-8", "ascii", sub = " ")
    # Sample each file's knowledge to ensure equal coverage across the files
    doclen <- length(textvector)
    samplendx <- sample(doclen, round(samplesize*doclen))
    textvector <- textvector[samplendx]
    thedata <- c(thedata, textvector)
}
#
#  The sample text is split 70-20-10 into a training, test and validation test sets and written to file.
#
trainprob <- 0.7
doclen <- length(thedata)
samplendx <- sample(doclen, round(trainprob*doclen))
traindata <- thedata[samplendx]
tvdata <- thedata[-samplendx]
rm(thedata)   # Done with this, clean memory
doclen <- length(tvdata)
# Keep 20% for test and leave 10% as held-out data for validation of model
samplendx <- sample(doclen, round(0.667*doclen))
testdata <- tvdata[samplendx]
validata <- tvdata[-samplendx]
trainfile <- "data/traintext.txt"
writeLines(traindata,trainfile,useBytes = TRUE)
writeLines(testdata,"data/testtext.txt",useBytes = TRUE)
writeLines(validata,"data/valitext.txt",useBytes = TRUE)
rm(traindata,testdata,validata,tvdata, textvector,samplendx)


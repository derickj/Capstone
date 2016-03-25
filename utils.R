doApostrophes <- function(x) 
{
    x <- gsub("\xe2\x80\x99","\x27",x)
    x <- gsub("\xe2\x80\x9c","",x)
    x <- gsub("\xe2\x80\x9d","",x)
    x <- gsub("\xe2"," ",x)
    x <- gsub("n't", "nt ", x)
    x <- gsub("'s", "s ", x)
    x <- gsub("'re", " are ", x)
    x <- gsub("'ve", " have ", x)
    x <- gsub("i'm", "i am ", x)
    x
}

doURLsAndEmail <- function(x)
{
    x <- gsub("http:[^ ]+ ","http ",x)
    x <- gsub("https:[^ ]+ ","http ",x)
    x <- gsub("[a-zA-Z-]+@[a-zA-Z0-9\x2e]+","emailaddr ",x)
    x <- gsub("@"," at ",x)
}

removePunctuation <- function(x) 
{
    # Keep only letters, numbers and spaces
    x  <- gsub("-","",x)
    gsub("[^[:alnum:][:blank:]]", " ", x)
}

removeNumbers <- function(x) 
{
    gsub("[0-9]+", "", x)
}

collapseWhiteSpace <- function(x) 
{
    gsub(" +"," ", x)
}

cleanInput <- function (x)
{
    x <- tolower(x)
    x <- doURLsAndEmail(x)
    x <- doApostrophes(x)
    x <- gsub("[\x80-\xff]+","",x)
#    x <- gsub("[šžþÃàáâãäâåçèéêëìíîïðñòóôõöùúûüý¢]+", " ", x)
    x <- removePunctuation(x)
    x <- removeNumbers(x)
    collapseWhiteSpace(x)
}

firstnwords <- function (x,n)
{
    delim <- "[ \r\n\t]+"
    words <- unlist(strsplit(x, delim))
    cnt <- length(words)
    nend <- cnt - 1
    if ((nend > 0) && (n >= 1))
    {
        words <- words[1:nend]
        x <- unlist(paste(unlist(words), collapse=" "))
    }
    x
}

lastnwords <- function (x,n)
{
    delim <- "[ \r\n\t]+"
    words <- unlist(strsplit(x, delim))
    cnt <- length(words)
    nbeg <- cnt - n + 1
    if ((nbeg > 0) && (nbeg <= cnt))
    {
        words <- words[nbeg:cnt]
        x <- unlist(paste(unlist(words), collapse=" "))
    }
    x
}
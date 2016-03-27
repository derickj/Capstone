doApostrophes <- function(x) 
{
    x <- gsub("\xe2\x80\x99","\x27",x)
    x <- gsub("\xe2\x80\x9c","",x)
    x <- gsub("\xe2\x80\x9d","",x)
    x <- gsub("\xe2"," ",x)
    x <- gsub("won't", "will not ", x)
    x <- gsub("n't", " not ", x)
    x <- gsub("'d", " would ", x)
    x <- gsub("'ll", " will ", x)
    x <- gsub("it's", " it is ", x)
    x <- gsub("let's", " let us ", x)
    x <- gsub("who's", " who is ", x)
    x <- gsub("she's", " she is ", x)
    x <- gsub("he's", " he is ", x)
    x <- gsub("there's", " there is ", x)
    x <- gsub("where's", " where is ", x)
    x <- gsub("here's", " here is ", x)
    x <- gsub("that's", " that is ", x)
    x <- gsub("what's", " what is ", x)
    x <- gsub("'s", " possesivess ", x)
    x <- gsub("s'", "s ", x)
    x <- gsub("'re", " are ", x)
    x <- gsub("'ve", " have ", x)
    x <- gsub("i'm", "i am ", x)
    x <- gsub("'", " ", x)
    x <- gsub(" possesivess ", " 's ", x)
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
    # Keep only letters, numbers, remaining quotes and spaces
    x  <- gsub("-","",x)
    gsub("[^[:alnum:][:blank:]']", " ", x)
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
    x <- gsub("[\x80-\xff]+","",x)
    x <- tolower(x)
    x <- doURLsAndEmail(x)
    x <- doApostrophes(x)
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
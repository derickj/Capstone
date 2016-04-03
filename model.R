

calcBackoffScore <- function (df, pattern, ngramcount, nrows = 50, lambda = 0.4)
{
    mtchterm <- df[grep(pattern,df$first),]
    nlen <- length(mtchterm$tf)
    result <- NULL
    if (nlen > 0)
    {
        denom <- sum(mtchterm$tf)
        #mtchterm$relfreq <- mtchterm$tf / denom * lambda
        mtchterm$mle <- mtchterm$tf / denom * lambda
        mtchterm$nextword <- mtchterm$lastw
        mtchterm <- mtchterm[order(mtchterm$mle,decreasing=TRUE),]
        mtchterm <- mtchterm[1:min(nlen,nrows),]
        result <- data.frame(mtchterm$mle,mtchterm$nextword,stringsAsFactors = FALSE)
        result$orig <- ngramcount
    }
    rm(mtchterm)
    result
}


calcSGTScore <- function (df, pattern, ngramcount, nrows = 250, lambda = 0.4)
{
    mtchterm <- df[grep(pattern,df$first),]
    nlen <- length(mtchterm$rstar)
    result <- NULL
    if (nlen > 0)
    {
        denom <- sum(mtchterm$tf)
        mtchterm$mle <- mtchterm$rstar / denom
        mtchterm$nextword <- mtchterm$lastw
        mtchterm <- mtchterm[order(mtchterm$mle,decreasing=TRUE),]
        mtchterm <- mtchterm[1:min(nlen,nrows),]
        result <- data.frame(mtchterm$mle,mtchterm$nextword,stringsAsFactors = FALSE)
        result$orig <- ngramcount
        colnames(result) <- c("mle","nextword","orig")
    }
    rm(mtchterm)
    result
}


findWords <- function (x, nrows = 5, useSimple = TRUE)
{
    lambda1 <- 1
    lambda2 <- 1
    lambda3 <- 1
    lambda4 <- 1
    delim <- "[ \r\n\t]+"
    words <- unlist(strsplit(x, delim))
    cnt <- length(words)
    
    result <- data.frame("the",0, 0,stringsAsFactors = FALSE)
    colnames(result) <- c("nextword","mle", "orig")
    result <- rbind(result, c("and",0,0))
    result <- rbind(result, c("that",0,0))
    result <- rbind(result, c("rt",0,0))
    result <- rbind(result, c("you",0,0))
    delim <- "[ \r\n\t]*"
    result4 <- NULL
    if (cnt > 2)
    {
        mtchstr <- lastnwords(x,3)
        pattern <- paste("^",mtchstr, delim, "$", sep="")
        if (useSimple == TRUE)
        {
            result4 <- calcBackoffScore (quadgrams,pattern,4, lambda = 0.8)
        }
        else
        {
            result4 <- calcSGTScore (quadgrams,pattern,4)
        }
    }
    if (!is.null(result4))
    {
        colnames(result4) <- c("mle4","nextword","orig4")
        result <- merge(result,result4, by = "nextword", all = TRUE)
        lambda3 <- 0
        lambda2 <- 0
    }
    else
    {
        result$mle4 <- 0
    }

    result3 <- NULL
    if (cnt > 1)
    {
        mtchstr <- lastnwords(x,2)
        pattern <- paste("^",mtchstr, delim, "$", sep="")
        if (useSimple == TRUE)
        {
            result3 <- calcBackoffScore (trigrams,pattern,3, lambda = 0.4)
        }
        else
        {
            result3 <- calcSGTScore (trigrams,pattern,3)
        }
    }
    if (!is.null(result3))
    {
        colnames(result3) <- c("mle3","nextword","orig3")
        result <- merge(result,result3, by = "nextword", all = TRUE)
        lambda2 <- 0
    }
    else
    {
        result$mle3 <- 0
    }

    result2 <- NULL
    if (cnt > 0)
    {
        mtchstr <- lastnwords(x,1)
        pattern <- paste("^",mtchstr, delim, "$", sep="")
        if (useSimple == TRUE)
        {
            result2 <- calcBackoffScore (bigrams,pattern,2, lambda = 0.2)
        }
        else
        {
            result2 <- calcSGTScore (bigrams,pattern,2)
        }
    }
    if (!is.null(result2))
    {
        colnames(result2) <- c("mle2","nextword","orig2")
        result <- merge(result,result2, by = "nextword", all = TRUE)
    }
    else
    {
        result$mle2 <- 0
    }
    
    p0 <- 0.0277970770431787
    result$mle1 <- p0
    nwords <- length(result$nextword)
    if (nwords == 1)
    {
        result$nextword[1] <- unigrams$lastw[1]
    }
    for (i in 1:nwords)
    {
        theword <- unigrams[unigrams$lastw == result$nextword[i],] 
        if (nrow(theword) > 0)
        {
            result$mle1[i] <- theword$p[1]
            lambda1 <- 1
        }
    }
    result[is.na(result)] <- 0
    if (useSimple == TRUE)
    {
        result$mle <- (result$mle4 * lambda4)
        result$mle <- result$mle + (result$mle3 * lambda3)
        result$mle <- result$mle + (result$mle2 * lambda2)
    }
    else
    {
        # Tested with pentgrams, but no real value added
        lambda1 <- 0.05
        lambda2 <- 0.3
        lambda3 <- 0.3
        lambda4 <- 0.35
        result$mle <- (result$mle4 * lambda4)
        result$mle <- result$mle + (result$mle3 * lambda3)
        result$mle <- result$mle + (result$mle2 * lambda2)
        result$mle <- result$mle + (result$mle1 * lambda1)
    }
   result <- result[result$nextword != "unk",] 
   nrows <- min (nrows, length(result$nextword))
   result <- result[order(result$mle,result$mle4,result$mle3,result$mle2,decreasing = TRUE),]
   result <- result[1:nrows,]
   result
}


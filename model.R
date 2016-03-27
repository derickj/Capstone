
#testing <- TRUE
calcBackoffScore <- function (df, pattern, ngramcount, nrows = 50, lambda = 0.4)
{
#    if(testing == TRUE)
#    {
#        mtchterm <- df[grep(pattern,df$term),]
#        
#    }
#    else
#    {
        mtchterm <- df[grep(pattern,df$first),]
        
#    }
#    mtchterm <- df[grep(pattern,df$term),]
    nlen <- length(mtchterm$tf)
    result <- NULL
    if (nlen > 0)
    {
        denom <- sum(mtchterm$tf)
        #mtchterm$relfreq <- mtchterm$tf / denom * lambda
        mtchterm$mle <- mtchterm$tf / denom * lambda
        #mtchterm$nextword <- unlist(lapply(mtchterm$term, lastnwords, 1))
        mtchterm$nextword <- mtchterm$lastw
        # mtchterm4 <- mtchterm4[mtchterm4$nextword != "unk",]
        #nlen <- length(mtchterm$tf)
        #if (nlen > 0)
        #{
            mtchterm <- mtchterm[order(mtchterm$mle,decreasing=TRUE),]
            mtchterm <- mtchterm[1:min(nlen,nrows),]
            result <- data.frame(mtchterm$mle,mtchterm$nextword,stringsAsFactors = FALSE)
            result$orig <- ngramcount
#            colnames(result) <- c("relfreq","nextword","orig")
        #}
    }
    rm(mtchterm)
    result
}

simpleBackoffCalc <- function (x, nrows = 50, testing = FALSE)
{
    delim <- "[ \r\n\t]+"
    xtra = 0
    if (testing == TRUE)
    {
        xtra = 1
    }
    result <- data.frame(0,"unk",0,stringsAsFactors = FALSE)
    colnames(result) <- c("relfreq","nextword","orig")
    mtchstr <- lastnwords(x,3 + xtra)
    pattern <- paste("^",mtchstr, delim, sep="")
    result4 <- calcBackoffScore (quadgrams,pattern,4,lambda = 1)
    if (!is.null(result4))
    {
        result <- rbind(result,result4)
    }

    mtchstr <- lastnwords(x,2 + xtra)
    pattern <- paste("^",mtchstr, delim, sep="")
    result3 <- calcBackoffScore (trigrams,pattern,3,lambda = 0.4)
    if (!is.null(result3))
    {
        result <- rbind(result,result3)
    }
    
    mtchstr <- lastnwords(x,1 + xtra)
    pattern <- paste("^",mtchstr, delim, sep="")
    result2 <- calcBackoffScore (bigrams,pattern,2,lambda = 0.4)
    if (!is.null(result2))
    {
        result <- rbind(result,result2)
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
            mtchterm1$nextword <- mtchstr
            result1 <- data.frame(mtchterm1$relfreq,mtchterm1$nextword,stringsAsFactors = FALSE)
            result1$orig <- 1
            colnames(result1) <- c("relfreq","nextword","orig")
            result <- rbind(result,result1)
        }
    }
    result <- result[result$nextword != "unk",] 
    result[order(result$relfreq,decreasing = TRUE),]
#    head(mtchterm2[order(mtchterm2$relfreq,decreasing=TRUE),])
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

simpleWithSGT <- function (x, nrows = 250, testing = FALSE)
{
    delim <- "[ \r\n\t]*"
    xtra = 0
    if (testing == TRUE)
    {
        xtra = 1
    }
    result <- data.frame("unk",0, 0,stringsAsFactors = FALSE)
    colnames(result) <- c("nextword","mle", "orig")
    mtchstr <- lastnwords(x,3 + xtra)
    pattern <- paste("^",mtchstr, delim, "$", sep="")
    result4 <- calcSGTScore (quadgrams,pattern,4)
    if (!is.null(result4))
    {
        colnames(result4) <- c("mle4","nextword","orig4")
        result <- merge(result,result4, by = "nextword", all = TRUE)
    }
    else
    {
        result$mle4 <- 0
    }
    
    mtchstr <- lastnwords(x,2 + xtra)
    pattern <- paste("^",mtchstr, delim, "$", sep="")
    result3 <- calcSGTScore (trigrams,pattern,3)
    if (!is.null(result3))
    {
        colnames(result3) <- c("mle3","nextword","orig3")
        result <- merge(result,result3, by = "nextword", all = TRUE)
    }
    else
    {
        result$mle3 <- 0
    }
    

    mtchstr <- lastnwords(x,1 + xtra)
    pattern <- paste("^",mtchstr, delim, "$", sep="")
    result2 <- calcSGTScore (bigrams,pattern,2)
    if (!is.null(result2))
    {
        colnames(result2) <- c("mle2","nextword","orig2")
        result <- merge(result,result2, by = "nextword", all = TRUE)
    }
    else
    {
        result$mle2 <- 0
    }

    result <- result[result$nextword != "unk",] 
    p0 <- 0.030557288
    result$mle1 <- p0
    if (testing == TRUE)
    {
        mtchstr <- lastnwords(x,1)
        mtchterm1 <- unigrams[grep(mtchstr,unigrams$term),]
        nlen <- length(mtchterm1$tf)
        if (nlen > 0)
        {
            denom <- sum(unigrams$tf)
            mtchterm1$mle <- mtchterm1$tf / denom
            mtchterm1$nextword <- mtchstr
            result1 <- data.frame(mtchterm1$mle,mtchterm1$nextword,stringsAsFactors = FALSE)
            result1$orig <- 1
            colnames(result1) <- c("mle","nextword","orig")
            result <- merge(result,result1, by = 2)
        }
    }
    else
    {
        nwords <- length(result$nextword)
        for (i in 1:nwords)
        {
         
            theword <- unigrams[unigrams$lastw == result$nextword[i],] 
            if (nrow(theword) > 0)
            {
                result$mle1[i] <- theword$p[1]
            }
            
        }
    }
    lambda1 <- 0.01
    lambda2 <- 0.1
    lambda3 <- 0.29
    lambda4 <- 0.5
    result[is.na(result)] <- 0
    result$mle <- (result$mle1 * lambda1) + (result$mle2 * lambda2) + (result$mle3 * lambda3) + (result$mle4 * lambda4)
    result[order(result$mle,decreasing = TRUE),]
    #    head(mtchterm2[order(mtchterm2$relfreq,decreasing=TRUE),])
}


findWords <- function (x, nrows = 250, testing = FALSE, useSimple = TRUE)
{
 #browser()
    lambda1 <- 1
    lambda2 <- 1
    lambda3 <- 1
    lambda4 <- 1
   lambda5 <- 1
    delim <- "[ \r\n\t]*"
    xtra = 0
    if (testing == TRUE)
    {
        xtra = 1
    }
    result <- data.frame("unk",0, 0,stringsAsFactors = FALSE)
    colnames(result) <- c("nextword","mle", "orig")
    mtchstr <- lastnwords(x,4 + xtra)
    pattern <- paste("^",mtchstr, delim, "$", sep="")
    if (useSimple == TRUE)
    {
        result5 <- calcBackoffScore (pentgrams,pattern,5, lambda = 0.4)
    }
    else
    {
        result5 <- calcSGTScore (pentgrams,pattern,5)
    }
    if (!is.null(result5))
    {
        colnames(result5) <- c("mle5","nextword","orig5")
        result <- merge(result,result5, by = "nextword", all = TRUE)
        lambda4 <- 0
        lambda3 <- 0
        lambda2 <- 0
    }
    else
    {
        result$mle5 <- 0
    }
    
    mtchstr <- lastnwords(x,3 + xtra)
    pattern <- paste("^",mtchstr, delim, "$", sep="")
    if (useSimple == TRUE)
    {
        result4 <- calcBackoffScore (quadgrams,pattern,4, lambda = 0.3)
    }
    else
    {
        result4 <- calcSGTScore (quadgrams,pattern,4)
    }
    #    browser()
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
    
    mtchstr <- lastnwords(x,2 + xtra)
    pattern <- paste("^",mtchstr, delim, "$", sep="")
    if (useSimple == TRUE)
    {
        result3 <- calcBackoffScore (trigrams,pattern,3, lambda = 0.2)
    }
    else
    {
        result3 <- calcSGTScore (trigrams,pattern,3)
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
    
    
    mtchstr <- lastnwords(x,1 + xtra)
    pattern <- paste("^",mtchstr, delim, "$", sep="")
    if (useSimple == TRUE)
    {
        result2 <- calcBackoffScore (bigrams,pattern,2, lambda = 0.1)
    }
    else
    {
        result2 <- calcSGTScore (bigrams,pattern,2)
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
    
    p0 <- 0.030557288
    result$mle1 <- p0
    result <- result[result$nextword != "unk",] 
    if (testing == TRUE)
    {
        mtchstr <- lastnwords(x,1)
        mtchterm1 <- unigrams[grep(mtchstr,unigrams$lastw),]
        nlen <- length(mtchterm1$tf)
        if (nlen > 0)
        {
            denom <- sum(unigrams$tf)
            mtchterm1$mle <- mtchterm1$rstar / denom
            mtchterm1$nextword <- mtchstr
            result1 <- data.frame(mtchterm1$mle,mtchterm1$nextword,stringsAsFactors = FALSE)
            result1$orig <- 1
            colnames(result1) <- c("mle1","nextword","orig")
            result <- merge(result,result1, by = 2)
        }
    }
    else
    {
        nwords <- length(result$nextword)
        for (i in 1:nwords)
        {
            theword <- unigrams[unigrams$lastw == result$nextword[i],] 
            if (nrow(theword) > 0)
            {
                result$mle1[i] <- theword$p[1]
                lambda1 <- 1
            }
        }
    }
    result[is.na(result)] <- 0
    if (useSimple == TRUE)
    {
        result$mle <- (result$mle5 * lambda5)
        result$mle <- result$mle + (result$mle4 * lambda4)
        result$mle <- result$mle + (result$mle3 * lambda3)
        result$mle <- result$mle + (result$mle2 * lambda2)
    }
    else
    {
# Tested with pentgrams, but no real value added
        lambda1 <- 0.01
        lambda2 <- 0.15
        lambda3 <- 0.2
        lambda4 <- 0.24
        lambda5 <- 0.4
        result$mle <- (result$mle5 * lambda5)
        result$mle <- result$mle + (result$mle4 * lambda4)
#        lambda1 <- 0.01
#        lambda2 <- 0.19
#        lambda3 <- 0.3
#        lambda4 <- 0.4
#       result$mle <- (result$mle4 * lambda4)
        result$mle <- result$mle + (result$mle3 * lambda3)
        result$mle <- result$mle + (result$mle2 * lambda2)
        result$mle <- result$mle + (result$mle1 * lambda1)
    }
    result[order(result$mle,result$mle4,result$mle3,result$mle2,decreasing = TRUE),]
}


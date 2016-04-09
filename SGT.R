#
# This script loads the previously generated N-grams (from the sample training data) and applies a 
# Simple Good Turing smoothing to the data  
# The paper published at http://www.grsampson.net/AGtf1.html was consulted for the implementation methodology
#
library(stringr)
library(reshape2)

source("utils.R")

load("data/unigrams.RData")
load("data/bigrams.RData")
load("data/trigrams.RData")
load("data/quadgrams.RData")
load("data/pentgrams.RData")

calculateSGTprob <- function(ngrams, nterms)
{
    ngrams <- ngrams[order(ngrams$tf),]
    runlengths <- rle (ngrams$tf)
    sgtmat <- data.frame(runlengths$values,runlengths$lengths)
    colnames(sgtmat) <- c ("r","Nr")
    sgtmat$Nc <- sgtmat$r * sgtmat$Nr
    N <- sum(sgtmat$Nc)
    P0 <- sum(sgtmat[sgtmat$r == 1,]$Nr) / N
    if (P0 == 0)
    {
        P0 <- sum(sgtmat[sgtmat$r == 2,]$Nr) / N
    }
    OneMinusP0 <- 1 - P0
    sgtmat$Z <- 0
    i <- 0
    len <- length(sgtmat$r)
    for (j in 1:len)
    {
        if (j == len)
        {
            k <- (2 * sgtmat$r[j]) - i
        }
        else
        {
            k <- sgtmat$r[j + 1]
        }
        sgtmat$Z[j] <- (2 * sgtmat$Nr[j])/(k - i)
        i <- sgtmat$r[j]
    }
	sgtmat$logr <- log10(sgtmat$r)
	sgtmat$logZ <- log10(sgtmat$Z)
	logZ <- sgtmat$logZ
	logr <- sgtmat$logr
	pname <- paste("data/plot",nterms,".jpg",sep="")
	jpeg(pname)
	pname <- paste("Plot of log values for ",nterms,"-grams",sep="")
	plot(logr,logZ, main = pname)
	dev.off()
	
	linmod <- coef(lm(logZ ~ logr))
	coefa <- linmod[[1]]
	coefb <- linmod[[2]]
	sgtmat$rstar <- 0
	contiguous <- TRUE
	Sfit <- function(x,a,b) 
	{ 
		return (10 ^ (a + (b * log10(x)))) 
	}
	for (j in 1:len)
	{
		r <- sgtmat$r[j]
		if (j < len)
		{
			if (sgtmat$r[j + 1] != (r + 1))
			{
				contiguous <- FALSE
			}
		}
		x <- 0
		y <- (r + 1) * (Sfit(r+1,coefa,coefb) / Sfit(r,coefa,coefb))
		if (contiguous)
		{
			fact <- (sgtmat$Nr[j+1]) / (sgtmat$Nr[j])
			x <- (r + 1) * fact
			val <- sqrt(((r + 1)^2) * fact * (1 + fact)) * 1.96
			if (abs(x - y) <= val)
			{
				contiguous = FALSE
			}
		}
		if (contiguous == TRUE)
		{
			sgtmat$rstar[j] <- x
		}
		else
		{
			sgtmat$rstar[j] <- y
		}
	}
	Nquote <- sum(sgtmat$Nr * sgtmat$rstar)
	sgtmat$p <- OneMinusP0 * (sgtmat$rstar / Nquote)
	sgtmat$logp <- log10(sgtmat$p)
	prare1 <- data.frame(0,P0,log10(P0))
	colnames(prare1) <- c ("r","p","logp")
	prare2 <- data.frame(sgtmat$r[1:5],sgtmat$p[1:5],sgtmat$logp[1:5])
	colnames(prare2) <- c ("r","p","logp")
	prare1 <- rbind(prare1,prare2)
	prare1$ngram <- nterms
	pname <- paste("data/rareprob",nterms,".csv",sep="")
	write.csv(prare1, pname, row.names=FALSE)
	colkeep <- c("r", "rstar", "p","logp")
	sgtmat <- sgtmat[,colkeep]
	ngrams <- merge(ngrams,sgtmat,by.x = 1, by.y = 1)
	ngrams <- ngrams[order(ngrams$tf,decreasing = TRUE),]
	ngrams
}

calculateNextWord <- function (ngrams, nterms)
{
    if(nterms == 1)
    {
        ngrams$lastw <- ngrams$term
        ngrams$first <- ngrams$term
    }
    else
    {
        ngrams$lastw <- unlist(lapply(ngrams$term,lastnwords,1))
        ngrams$first <- unlist(lapply(ngrams$term,firstnwords,nterms - 1))
    }
    ngrams
}
keepcolumns <- c("tf","rstar","p","lastw","first")
ngrams1 <- calculateSGTprob(unigrams, 1)
ngrams1 <- calculateNextWord(ngrams1, 1)
ngrams1 <- ngrams1[ngrams1$tf > 5,keepcolumns]
save(ngrams1,file = "data/unigramsSGT.RData")
print("finished 1")
ngrams2 <- calculateSGTprob(bigrams, 2)
ngrams2 <- calculateNextWord(ngrams2, 2)
ngrams2 <- ngrams2[ngrams2$tf > 2,keepcolumns]
save(ngrams2,file = "data/bigramsSGT.RData")
print("finished 2")
ngrams3 <- calculateSGTprob(trigrams, 3)
ngrams3 <- calculateNextWord(ngrams3, 3)
ngrams3 <- ngrams3[ngrams3$tf > 1,keepcolumns]
save(ngrams3,file = "data/trigramsSGT.RData")
print("finished 3")
ngrams4 <- calculateSGTprob(quadgrams, 4)
ngrams4 <- calculateNextWord(ngrams4, 4)
ngrams4 <- ngrams4[ngrams4$tf > 1,keepcolumns]
save(ngrams4,file = "data/quadgramsSGT.RData")
print("finished 4")
ngrams5 <- calculateSGTprob(pentgrams, 5)
ngrams5 <- calculateNextWord(ngrams5, 5)
ngrams5 <- ngrams5[ngrams5$tf > 1,keepcolumns]
save(ngrams5,file = "data/pentgramsSGT.RData")
print("finished 5")

rm(ngrams1, unigrams)
rm(ngrams2, bigrams)
rm(ngrams3, trigrams)
rm(ngrams4, quadgrams)
rm(ngrams5, pentgrams)

viewngrams <- function (n = 10, topcnt = TRUE, mtch = FALSE,  pattern = NULL)
{
    if (mtch == TRUE)
    {
    if (topcnt == TRUE)
    {
        print(head(quadgrams[ndx <- grep (pattern, bigrams$term),],n))
        print(head(trigrams[ndx <- grep (pattern, bigrams$term),],n))
        print(head(bigrams[ndx <- grep (patter, bigrams$term),],n))
        head(unigrams[ndx <- grep (pattern, bigrams$term),],n)        
    }
    else
    {
        print(tail(quadgrams[ndx <- grep (pattern, bigrams$term),],n))
        print(tail(trigrams[ndx <- grep (pattern, bigrams$term),],n))
        print(tail(bigrams[ndx <- grep (pattern, bigrams$term),],n))
        tail(unigrams[ndx <- grep (pattern, bigrams$term),],n)        
        
    }
        
    }
    else
    {
        if (topcnt == TRUE)
        {
            print(head(quadgrams,n))    
            print(head(trigrams,n))    
            print(head(bigrams,n))    
            head(unigrams,n)        
        }
        else
        {
            print(tail(quadgrams,n))    
            print(tail(trigrams, n))    
            print(tail(bigrams,n))    
            tail(unigrams,n)        
            
        }
        
    }
}

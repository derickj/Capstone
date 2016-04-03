#
# server.R:  The server of the shiny app
#

#
# Load the required libraries
#

library (shiny)
library(RCurl)
library(RWeka)
library(tm)
library(ggplot2)
library(stringr)
library(gridExtra)
library(reshape2)

source("utils.R")
source("model.R")

shinyServer(
  function(input, output) {
    dofilter <- eventReactive (input$ifilteron, {
        if (input$ifilteron == TRUE)
        {
            cleanWords <- TRUE
        }
        else
        {
            cleanWords <- FALSE
        }
        cleanWords
    })
    doverbose <- eventReactive (input$iverbose, {
        if (input$iverbose == TRUE)
        {
            verbose <- TRUE
        }
        else
        {
            verbose <- FALSE
        }
        verbose
    })
    dowords <- eventReactive (input$inwords, {
            nwords <- input$inwords
        nwords
    })
    predictWords <- eventReactive(input$go, {
              x <- cleanInput(input$inputphrase)
              nextwords <- findWords (x, dowords(), FALSE)
              if (dofilter() ==  TRUE)
              {
                  if (nrow (nextwords[nextwords$nextword %in% badwords, ]) > 0)
                  {
                      nextwords[nextwords$nextword %in% badwords, ]$nextword <- "%$@!*"
                  }
              }
              if (doverbose() == FALSE)
              {
                  nextwords <- data.frame (nextwords$nextword)
                  colnames(nextwords) <- c("nextword")
              }
              nextwords
    })
    output$oinput <- renderPrint (input$inputphrase)
    output$ofilter <- renderPrint (input$ifilteron)
    output$otable <- renderTable (predictWords())
  }
)

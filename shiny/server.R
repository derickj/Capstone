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
#    msg <- reactive({
#        x <- cleanInput(input$inputphrase)
#        nextwords <- findWords (x , 3, FALSE)
#        msg <- paste(nextwords$nextword[1:3],sep = " ")
#        msg
#    })
    dofilter <- eventReactive (input$filteron, {
        if (input$filteron == TRUE)
        {
            cleanWords <- TRUE
        }
        else
        {
            cleanWords <- FALSE
        }
        cleanWords
    })
    predictWords <- eventReactive(input$go, {
              x <- cleanInput(input$inputphrase)
              nextwords <- findWords (x , 3, FALSE)
              if (dofilter() ==  TRUE)
              {
              }
#              msg <- paste(nextwords$nextword[1:3],sep = " ")
#              msg
              nextwords[1:5,]
    })
    output$oinput <- renderPrint (input$inputphrase)
    output$ofilter <- renderPrint (input$filteron)
#    output$owords <- renderPrint (predictWords())
    output$otable <- renderTable (predictWords())
  }
)

#
# ui.R:  The user interface of the shiny app
#

#
# Load the required libraries
#
library (shiny)
require (markdown)

# Define UI for dataset viewer application
shinyUI(fluidPage(theme = "bootstrap.css", 
    
    # Application title
    titlePanel("Word Prediction"),
    
    sidebarLayout(
        
        # Sidebar with inputs
        sidebarPanel(
            textInput(inputId = "inputphrase",
                      label = "Enter a phrase and click <Predict> to predict next word",
                      value = "",
                      width = '500px'),
            actionButton("go", "Predict"),
            checkboxInput(inputId = "ifilteron", 
                          label = "Profanity filtering on?", 
                          value = TRUE),
            checkboxInput(inputId = "iverbose", 
                          label = "Verbose mode (to see MLEs)?", 
                          value = FALSE),
            sliderInput(inputId = "inwords", 
                        label = "Max number of words to return?", 
                        min=3, max=10, value=5)
        ),
        # Show the output together with the input phrase
        mainPanel(
            tabsetPanel(
                tabPanel("Input phrase",  
                         verbatimTextOutput("oinput"),
                         "The next potential words are:",
                        tableOutput("otable")),
                tabPanel("Instructions", includeMarkdown("README.md"))
            )
        )
    )   
)
)


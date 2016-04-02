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
        
        # Sidebar with a slider input
        sidebarPanel(
            textInput(inputId = "inputphrase",
                      label = "Enter a phrase to predict next word",
                      value = "",
                      width = '500px'),
            checkboxInput(inputId = "filteron", 
                          label = "Profanity filtering on?", 
                          value = TRUE),
            actionButton("go", "Predict")
        ),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Predict",  
                         verbatimTextOutput("oinput"),
                         "Profanity filtering set to ",  
                         verbatimTextOutput("ofilter"),
                         "The next potential words are:",
#                         verbatimTextOutput("owords")),
                        tableOutput("otable")),
                tabPanel("Instructions", includeMarkdown("README.md"))
            )
        )
    )   
)
)


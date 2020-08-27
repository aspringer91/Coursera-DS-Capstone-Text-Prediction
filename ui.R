
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Text Prediction Algorithm"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("textInput", label = h3("Text Input"), value = "I will predict text"),
            actionButton("run", "Predict Text")
        ),
        mainPanel(
            h3("Prediction"),
            verbatimTextOutput("predictedText")
        )
    )
))

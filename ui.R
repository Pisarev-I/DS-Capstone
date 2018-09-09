library(shiny)

shinyUI(fluidPage(
  titlePanel("Next Word Prediction"),
  sidebarLayout(
    sidebarPanel(
       textAreaInput("strInputText",
                 "Type your text, and press Submit button for predict next word:",
                 value = "Example: For the first",
                 width = "100%",
                 height = "100%"),
       submitButton("Submit")
    ),
    mainPanel(
      h4('Last 5 words of the text, used for prediction:'),
      verbatimTextOutput("strLine"),
      
      h4('10 predicted words:'),
      htmlOutput("strNextWords")
    )
  )
))
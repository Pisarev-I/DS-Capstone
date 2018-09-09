library(shiny)

shinyServer(function(input, output) {
  strLineTmp <- reactive({
    strLineTm <- fClearText(input$strInputText)
    if (str_count(strLineTm, "\\S+") > 5) {
      strLineTm <- word(strLineTm, start = -5, end = -1)
    }
    strLineTm
  })
  
  output$strLine <- renderText({strLineTmp()})
  
  output$strNextWords <- renderUI({
    HTML(paste(fNextWordPrediction(strLineTmp()), collapse = "<br/>"))
    })
})

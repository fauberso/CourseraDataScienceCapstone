#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    setTextSelection <- function(id, from, to){
        session$sendCustomMessage(type = 'setTextSelection', message = list('id'=id,'from'=from,'to'=to))
    }
    
    setTextPrediction <- function(id, pred){
        session$sendCustomMessage(type = 'setTextPrediction', message = list('id'=id,'pred'=pred))
    }
    
    observe({
        # We'll use the input$controller variable multiple times, so save it as x
        # for convenience.
        x <- input$controller
        key <- input$keypress
        
        # This will change the value of input$inText, based on x
        if (!is.null(key) && key==32) {
            #updateTextAreaInput(session, "textarea", value = paste(x, "schen"))
            setTextSelection("textarea", 8, 11)
        }
        setTextPrediction("textarea", "schen")
        output$debug <- renderTable(c(key))
    })
  
})

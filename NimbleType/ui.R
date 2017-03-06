#
# NimbleType is a proof-of-concept for a predictive text entry engine.
# It reacts to text entered by the user and attempts to predict good candidates for next works 
#

library(shiny)
library(shinyjs)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  
    # Application title
  titlePanel("NimbleType Proof-of-Concept"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(strong("Configuration parameers:"),
                 radioButtons("file", "Model Used:",
                              c("1% Sample of original corpus" = "model.1.rds",
                                "2% Sample of original corpus" = "model.2.rds",
                                "5% Sample of original corpus" = "model.5.rds")),
                 textOutput("filestatus"),
                 hr(),
                 sliderInput("conf", "Minimum confidence:", min = -3, max = 0, value = -2.475, step=0.025, pre="10<sup>", post="</sup>"),
                 div(textOutput("confCleartext"), style = "font-size:80%")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        textAreaInput("textarea", "Predictive Text Entry Field:", "", width = "600px", rows = 2, placeholder = "Start typing here...", resize = "vertical"),
        # Hack: Will fire an input change even on every keypress, and store the Unicode code of the key pressed in variable 'Keypress'
        # This way, we can continually update the display, and react to the spacebar (unicode 32) e.which)
        tags$script("
            Shiny.addCustomMessageHandler ('setTextSelection',function (m) {
                    var element = document.getElementById(m.id);
                    element.setSelectionRange(m.from, m.to); 
                });
            Shiny.addCustomMessageHandler ('setTextPrediction',function (m) {
                    var element = document.getElementById(m.id);
                    if ( element.value.length === element.selectionEnd ) {
                        if ( element.value.length !== element.selectionStart ) {
                            element.value = element.value.substring(0, element.selectionStart)
                        }
                        var predStart = element.value.length;
                        element.selectionStart = predStart;
                        element.value = element.value + m.pred;
                        element.selectionStart = predStart;
                        element.selectionEnd = element.value.length;
                    } 
                    });
        "),
        strong("Matches found in Model:"),
        div(dataTableOutput("details", width = "150px"), style = "font-size:65%")
        #textOutput("debug")
    )
  )
))

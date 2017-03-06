#
# NimbleType is a proof-of-concept for a predictive text entry engine.
# It reacts to text entered by the user and attempts to predict good candidates for next works 
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("NimbleType Proof-of-Concept"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel("Configuration",
        sliderInput("Configuration", "Minimum confidence:", min = 0, max = 100, value = 0.1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        textAreaInput("textarea", "Predictive Text Entry Field:", "", width = "100%", rows = 2, placeholder = "Start typing here...", resize = "vertical"),
        # Hack: Will fire an input change even on every keypress, and store the Unicode code of the key pressed in variable 'Keypress'
        # This way, we can continually update the display, and react to the spacebar (unicode 32) 
        tags$script("
            $(document).on('keypress', function (e) {
                    Shiny.onInputChange('keypress', e.which); 
                });
            Shiny.addCustomMessageHandler ('setTextSelection',function (m) {
                    var element = document.getElementById(m.id);
                    element.setSelectionRange(m.from, m.to); 
                });
            Shiny.addCustomMessageHandler ('setTextPrediction',function (m) {
                    var element = document.getElementById(m.id);
                    if ( element.value.length === element.selectionEnd ) {
                        var predStart = element.selectionEnd;
                        element.selectionStart = predStart;
                        element.value = element.value + m.pred;
                        element.selectionStart = predStart;
                        element.selectionEnd = element.value.length;
                    } 
                    });
        "),
        tableOutput("debug"),
        textAreaInput("details", NULL, "", width = "100%", rows = 1, placeholder = "Debugging Data")
    )
  )
))

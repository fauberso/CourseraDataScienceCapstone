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
  titlePanel(div("NimbleType Proof-of-Concept",h5(strong("Type some words followed by the space bar to predict the next word in a sentence."),br(),br()))),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        strong("Configuration parameters:"),
                 radioButtons("file", "Model Used:",
                              c("1% Sample of original corpus" = "model.1.rds",
                                "2% Sample of original corpus" = "model.2.rds",
                                "5% Sample of original corpus" = "model.5.rds")),
                 textOutput("filestatus"),
                 hr(),
                 sliderInput("conf", "Minimum confidence:", min = -3, max = 0, value = -2.475, step=0.025, pre="10<sup>", post="</sup>"),
                 div(textOutput("confCleartext"), style = "font-size:80%"),h5(strong("Type some words followed by the space bar to predict the next word in a sentence."),br(),br())
    
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
        div(dataTableOutput("details", width = "150px"), style = "font-size:65%; width:250px;")
        #textOutput("debug")
    )),
  div(br(),
      "This is a predictive text entry demonstrator: It shows how a textbox can be equipped with a server-side components that reacts to text entry and attempts to predict the next word the user is about to enter.",
      "Wait for the predictive model to load (the text entry field will be grayed out until the model is loaded) then enter some text in the text field: The predictive engine will attempt to guess which word is being typed and will complete it.",br(),
      "As a simple test, enter some words, and press the space bar: The prediction engine will attempt to guess which word is most likely to be next. ",
      "The predicted word or word ending is selected automatically. You can either continue typing to ignore the predicted text, or press the right arrow to accept the proposed word, and type the rest of your sentence.",
      "A table below the text entry field shows which potential matches were found in the model. The highlighted words also match the beginning of the word being typed.", br(),
      "The model file to use can be selected on the left sidebar: Larger models will boost prediction accuracy, but will take more time to load. The number of entries given is the number of group of (1 to 3) words for which predictions exist (and there can be many predictions per group).",br(),
      "A slider allows restricting the prediction engine to predictions for which a minimum probability has been calculated. This will cause the system to not predict a word in situations where a prediction would be to uncertain.",
      "A label below the slider gives the minimum probability of a prediction for it to be considered by the engine: This value (which you need to multiply by 100 to get a percentage) is also given for each prediction in the list of potential matches in the main part of the application."
      , style = "font-size:85%; width:950px;"
      )
))

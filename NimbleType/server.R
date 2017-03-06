#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    k <- 5 #"good enough" value above which we don't discount anymore
    discProp <- "$discounts" #Name of the Discount table in an environment

    setTextSelection <- function(id, from, to){
        session$sendCustomMessage(type = 'setTextSelection', message = list('id'=id,'from'=from,'to'=to))
    }
    
    setTextPrediction <- function(id, pred){
        session$sendCustomMessage(type = 'setTextPrediction', message = list('id'=id,'pred'=pred))
    }
    
    getSearchTerm <- function(words, count) {
        if (length(words)-(count-1)<1) return(NULL)
        paste(words[(length(words)-(count-1)):length(words)], collapse="_")
    }
    
    getPrediction <- function(searchTerm, pred) {
        if (is.null(searchTerm)) return (c())
        frequencies <- pred[[searchTerm]] 
    }
    
    splitString <- function(string) {
        hasIncompleteWord <- substr(string, nchar(string), nchar(string))!=" "
        parts <- strsplit(tolower(string), " ")[[1]]

        if (hasIncompleteWord) {
            return (list(
                words=parts[1:length(parts)-1],
                rest=parts[length(parts)]
                ))
        } else {
            return (list(
                words=parts[1:length(parts)],
                rest=""
            ))
        }
    }
    
    filename <- ""
    model <- list(c(),c(),c())
    
    loadFile <- function(name) {
        shinyjs::disable("textarea")
        shinyjs::disable("file")
        
        model <<- readRDS(name)
        
        shinyjs::enable("textarea")
        shinyjs::enable("file")
        name
    }
    
    predict <- function(words, conf) {
        resultsFromTrigrams  <- getPrediction(getSearchTerm(words,3), model[[3]])
        redidualFromTrigrams <- 1-sum(resultsFromTrigrams)
        
        resultsFromBigrams   <- getPrediction(getSearchTerm(words,2), model[[2]])
        redidualFromBigrams  <- 1-sum(resultsFromBigrams)
        resultsFromBigrams   <- resultsFromBigrams * redidualFromTrigrams
        
        resultsFromUnigrams  <- getPrediction(getSearchTerm(words,1), model[[1]])
        redidualFromUnigrams <- 1-sum(resultsFromUnigrams)
        resultsFromUnigrams  <- resultsFromUnigrams * redidualFromBigrams
        
        predictions <- sort(c(resultsFromTrigrams, resultsFromBigrams, resultsFromUnigrams), decreasing = TRUE)
        predictions<-predictions[!duplicated(names(predictions))]
        predictions<-predictions[predictions>=conf]
    }
    
    getBestPrediction <- function(predictions, rest) {
        result <- grep(paste("^", rest, sep=""), names(predictions), value=TRUE)[1]
        ifelse(is.na(result),"", substring(result, nchar(rest)+1,nchar(result)))
    }

    toTable <- function(predictions, rest, count) {
        values <- cbind(P=unname(predictions), Value=names(predictions))
        values[,1] <- round(as.numeric(values[,1]), 8)
        matchesRest <- grep(paste("^", rest, sep=""), names(predictions))
        
        
        tableOptions <- list(searching = FALSE, 
                             paging = FALSE, 
                             dom = "t",
                             pageLength = count, 
                             select.style="api",
                             autoWidth = FALSE,
                             columnDefs = list(list(width = '100px', targets = 0), list(width = '200px', targets = 1))
        )
        
        datatable(values, options = tableOptions, class = "compact", selection=list(mode = 'multiple', selected = matchesRest, target = 'row'))
    }
    
    observe({
        text <- splitString(input$textarea)
        key <- input$keypress
        conf <- 10^input$conf
        output$confCleartext <- renderText(paste("Suppressing entries with P <", format(round(conf, abs(input$conf)+1), scientific=F), "\n"))
        
        # To react to the spacebar, do something like:
        if (!is.null(key) && key==32) {
            #updateTextAreaInput(session, "textarea", value = paste(ctl, "schen"))
            #setTextSelection("textarea", 8, 11)
        }

        if (filename != input$file) {
            output$filestatus <- renderText(paste("Loading", input$file, "...\n"))
            filename <<- loadFile(input$file)
            output$filestatus <- renderText(paste("Model size:", sum(sapply(model, length)), "entries total\n"))
        }

        
        predictions <- predict(text$words, conf)
        if (!is.null(predictions)) {
            output$details <- renderDataTable(toTable(predictions, text$rest, 200))
            if (length(predictions)!=0) {
                setTextPrediction("textarea", getBestPrediction(predictions, text$rest))
            }
        }
        
    })
  
})

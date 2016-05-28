
library(shiny)
library(magrittr)
library(googleVis)
library(dplyr)
library(ggvis)
source("functions.R")

shinyServer(function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedAlgorithm <- reactive({
    input$algorithm
  })
  
  # Show the values using an HTML table
  output$values <- renderTable({
    ntext()$solution
  })
  
  output$nText <- renderText({
    tail(ntext()$value,1)
  })
  
  output$ui <- renderUI({
    algo <- input$algorithm
    
    dat_for_text <- AlgortihmsList[[algo]]
    dat_for_text <- dat_for_text[names(dat_for_text)!="dim_f"]
    textas <- paste("fluidRow(",paste0("numericInput('",names(dat_for_text),
                                       "','",names(dat_for_text),"',",dat_for_text,")",collapse = ", "),")")
    eval(parse(text=textas))
    
  })
  

  
  ntext <- eventReactive(input$goButton, {
    algo <-  AlgortihmsList[[input$algorithm]]
    to.evaluate <- input$parameters %>% strsplit(",") %>% unlist %>% gsub(" ", "", .) %>% 
      paste0("'", . , "'", collapse=",") %>% paste0("initial.x = c(", . , ")")
    eval(parse(text= to.evaluate ))
    
    if("dim_f"%in%names(algo)){
      dim_f <- length(initial.x)
      dim_f <- paste(",dim_f=", dim_f)
    }else{
      dim_f <- ""
    }

    x.for.function <- paste(initial.x,paste0("x[",1:length(initial.x),"]"),
                            sep='=',collapse = ", ")
    
    eval(
      parse(
        text=paste0("result =",input$algorithm,
                    "(function(x){FProblem=function(",paste(initial.x,collapse=","),"){",input$f,"};FProblem(",x.for.function,")},",
                    paste0("input$",names(algo)[names(algo)!="dim_f"],collapse = ","),dim_f,")")))
    
    solution <- result$solutions
    rownames(solution) <- input$initial.x
    solution <- data.frame(solution[,dim(solution)[2]])
    names(solution) <- "value"
    solution -> result$solutions
    result
  })
  
# tsp part
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })
 
  
  
})








library(shiny)
library(magrittr)
library(googleVis)
library(dplyr)
library(ggvis)
library(reshape2)
library(stargazer)
library(xtable)
source("functions.R")
source("GEfunction.R")
source("functionsGE.R")

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
  output$inputTSPtable <- renderUI({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header = input$header,
                     sep = input$sep)
    
    HTML(gsub('table style="text-align:center"',
              'table style="text-align:center;width:100%"',stargazer(data,type = "html",summary = F)))
    
  })
  
  
  
  
  TSPsolution <- eventReactive(input$goButtonTSP, {
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    data <- read.csv(inFile$datapath, header = input$header,
                     sep = input$sep)
    data.dist <- as.matrix(dist(data, method = "euclidean"))
    n <- input$TSP.n
    P <- input$TSP.P
    muta1 <- input$TSP.muta1
    muta2 <- input$TSP.muta2
    check.mutations <- input$TSP.checkmuta
    solution <- GEsemi(data.dist,P,n,muta1,muta2,check.mutations)
    result <- list()
    result$solution <- solution
    result$path.length <- apply(solution,2,function(x)TotalDistance2(x,data.dist))
    result$data <- data
    result
  })
  
  
  output$valuesTSP <- renderTable({
    data.frame(tail(t(TSPsolution()$solution),1))
  })
  output$nTextTSP <- renderText({
    tail(TSPsolution()$path.length,1)
  })
  
  output$BestPath <- renderUI({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data.frame(tail(t(TSPsolution()$solution),1)) %>% t %>% 
      stargazer(type = "html",summary = F,colnames = F,rownames=F) %>% 
      gsub('table style="text-align:center"',
           'table style="text-align:center;width:100%"', .) %>% 
      HTML
    
  })
  
  
  
  output$uiTSPplot <- renderUI({
    if (is.null(TSPsolution)){
      return(NULL)
    }else{
      
      
      textas <- paste0("sliderInput('TSP.plot', 'Best solutions',min=1, max=",
                       length(TSPsolution()$path.length),
                       ", value=",
                       length(TSPsolution()$path.length),",animate=TRUE)")
      
      eval(parse(text=textas))
    }
  })
  
  output$plotTSP <- renderPlot({
    if (is.null(TSPsolution())){
      return(NULL)
    }else{
      solutionPlot <- TSPsolution()$solution
      dataPlot <-  TSPsolution()$data
      dsitancePlot <- TSPsolution()$path.length
      PlotDistances(solutionPlot[,input$TSP.plot],dataPlot)
      title(paste0("Acquired in generation: ",
                   gsub("iter","",colnames(solutionPlot)[input$TSP.plot]),"\n",
                   "Distance: ",round(dsitancePlot[input$TSP.plot],4)))
    }
  })
  
  
  
  
})







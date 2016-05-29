

# Creates server where all calculation are done
shinyServer(function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedAlgorithm <- reactive({
    input$algorithm
  })
  
  # Show the values using an HTML table
  output$values <- renderTable({
    ntext()$solution
  })
  
  # smallest value of function
  output$nText <- renderText({
    tail(ntext()$value,1)
  })
  
  # create parameter for ui for optimization
  output$ui <- renderUI({
    algo <- input$algorithm
    
    dat_for_text <- AlgorithmsList[[algo]]
    dat_for_text <- dat_for_text[names(dat_for_text)!="dim_f"]
    textas <- paste("fluidRow(",paste0("numericInput('",names(dat_for_text),
                                       "','",names(dat_for_text),"',",dat_for_text,")",collapse = ", "),")")
    eval(parse(text=textas))
    
  })
  
  
  # function optimization part
  ntext <- eventReactive(input$goButton, {
    algo <-  AlgorithmsList[[input$algorithm]]
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
  
  # Data to optimize in TSP
  output$inputTSPtable <- renderUI({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    data <- read.csv(inFile$datapath, header = input$header,
                     sep = input$sep)
    
    HTML(gsub('table style="text-align:center"',
              'table style="text-align:center;width:100%"',stargazer(data,type = "html",summary = F)))
    
  })
  
  # TSP optimization part
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
  
  # Shortest path
  output$valuesTSP <- renderTable({
    data.frame(tail(t(TSPsolution()$solution),1))
  })
  
  # shortest path length text
  output$nTextTSP <- renderText({
    tail(TSPsolution()$path.length,1)
  })
  
  # Shortest path HTML table
  output$BestPath <- renderUI({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    tsp.path.best <- data.frame(tail(t(TSPsolution()$solution),1)) %>% t 
    colnames(tsp.path.best) <- "Path"
    tsp.path.best %>% 
      stargazer(type = "html",summary = F,rownames=F) %>% 
      gsub('table style="text-align:center"',
           'table style="text-align:center;width:100%"', .) %>% 
      HTML
    
  })
  
  # slider for plot
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
  
  # plot output
  output$plotTSP <- renderPlot({
    if (is.null(TSPsolution())){
      return(NULL)
    }else{
      solution.plot <- TSPsolution()$solution
      data.plot <-  TSPsolution()$data
      dsitance.plot <- TSPsolution()$path.length
      which.plot <- input$TSP.plot
      
      
      plot(rbind(data.plot[solution.plot[,which.plot], ],
                 data.plot[solution.plot[,which.plot][1],]), type="l", lwd=2)
      grid()
      lines(rbind(data.plot[solution.plot[,which.plot], ],
                 data.plot[solution.plot[,which.plot][1],]), type="l", lwd=2,col='#CA4242')
      points(data.plot,pch=16,col="#428BCA",cex = 3)
      text(data.plot,label = 1:nrow(data.plot),col='white',adj = 0.5)
      
      title(paste0("Acquired in generation: ",
                   gsub("iter","",colnames(solution.plot)[which.plot]),"\n",
                   "Distance: ",round(dsitance.plot[which.plot],4)))
    }
  })
  
  
  
  
})

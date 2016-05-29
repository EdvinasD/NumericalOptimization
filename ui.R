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

shinyUI(
  navbarPage(
    "NOS",
    tabPanel(
      'Function minimization',
      fluidPage(
        fluidRow(
          column(
            4,
            "Selection",
            hr(),
            selectInput('algorithm', 'Algorithm', names(AlgorithmsList)),
            textInput('f', 'Function', value = '', width = NULL, placeholder = NULL),
            helpText("Example: (1-x)^2+100*(y-x^2)^2"),
            textInput('parameters', 'Variables to optimize', value = '', width = NULL, placeholder = NULL),
            helpText("Example: x, y")
          ),
          column(
            4,
            "Parameters",
            hr(),
            uiOutput("ui"),
            actionButton("goButton", "Optimize!")
          ),
          column(
            4,
            "Output",
            hr(),
            tableOutput("values"),
            verbatimTextOutput("nText")
          )
          
        ))),
    tabPanel(
      'Traveling Salesmen Problem',
      # csv file reading
      fluidPage(
        fluidRow(
          column(
            4,
            fileInput('file1', 'Choose file to upload',
                      accept = c(
                        'text/csv',
                        'text/comma-separated-values',
                        'text/tab-separated-values',
                        'text/plain',
                        '.csv',
                        '.tsv'
                      )
            ),
            tags$hr(),
            checkboxInput('header', 'Header', TRUE),
            radioButtons('sep', 'Separator',
                         c(Comma=',',
                           Semicolon=';',
                           Tab='\t'),
                         ','),
            tags$hr(),
            p(
              'you can first download the sample',
              a(href = 'https://raw.githubusercontent.com/EdvinasD/NumericalOptimization/master/burma14.csv', 'burma14.csv')
            ),
            tags$hr(),
            fluidRow(
              column(
                9,
                uiOutput(outputId = "inputTSPtable")
              ),
              column(
                3,
                uiOutput(outputId = "BestPath")
              )
            )
          ),
          column(
            3,
            numericInput("TSP.P", "Population size",100),
            numericInput("TSP.n", "Number of generations",100),
            sliderInput("TSP.muta1", "Mutation type 1",min=0, max=1, value=0.05,step=0.01),
            sliderInput("TSP.muta2", "Mutation type 2",min=0, max=1, value=0.05,step=0.01),
            checkboxInput("TSP.checkmuta", "Check mutations", value = FALSE, width = NULL),
            actionButton("goButtonTSP", "Optimize!"),
            p("Shortest distance:"),
            verbatimTextOutput("nTextTSP"),
            uiOutput("uiTSPplot")
          ),
          column(
            5,
            plotOutput('plotTSP')
          )
        )
      )
    )
  )
)


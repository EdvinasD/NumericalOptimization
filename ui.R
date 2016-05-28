

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
            selectInput('algorithm', 'Algorithm', names(AlgortihmsList)),
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
        titlePanel("Uploading Files"),
        sidebarLayout(
          sidebarPanel(
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
            p('If you want a sample .csv or .tsv file to upload,',
              'you can first download the sample',
              a(href = 'mtcars.csv', 'mtcars.csv'), 'or',
              a(href = 'pressure.tsv', 'pressure.tsv'),
              'files, and then try uploading them.'
            )
          ),
          mainPanel(
            tableOutput('contents')
          )
        )
      )
      
      
      
      
      
      
      
      
    )
  )
)

ui <- function(request){
   fluidPage(
  theme = shinythemes::shinytheme(theme = ifelse("shinythemes" %in% installed.packages()[,"Package"], "yeti", NULL)),
  tags$head(
    # tags$link(rel="icon", href="favicon.ico", type = "image/x-icon"),
    tags$link(type = "text/css", rel = "stylesheet", href = "projstyle.css"),
    tags$link(rel="shortcut icon", href="favicon.ico", type = "image/x-icon"),
    tags$style(
      HTML(
        "
        .dataTable tbody tr:hover{
          background-color: #e6e6ff;
        }
        "
      )
    )
  ),
  titlePanel(h2(style = "font-weight: 900;", "ITS analyses of England's Teenage Pregnancy Strategy"), 
             windowTitle = "ITS analyses of England's Teenage Pregnancy Strategy"),
  withMathJax(),
  sidebarLayout(
    
    
    
    sidebarPanel(
      tags$head(
        tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; } 
                   #inline .form-group { display: table-row;}")
        ),
      fluidRow(style = "margin: 0px;",
      selectInput(inputId = "ages",
                  label = "Age group:",
                  choices = c("Under 16", "Under 18", "Under 20"),
                  selected = "Under 18"),
      selectInput(inputId = "main",
                  label = "Country to observe:",
                  choices = c("Scotland", "England", "Wales", "England and Wales"),
                  selected = "England"),
      selectInput(inputId = "control",
                  label = "Country to compare:",
                  choices = c("Scotland", "England", "Wales", "England and Wales", "none"),
                  selected = "none"),
      uiOutput("dateslider"),
      uiOutput("intyr1slider"),
      checkboxInput(inputId = "int2",
                    label = "Common shock",
                    value = FALSE),
      uiOutput("intyr2slider"),
      checkboxInput(inputId = "pi1",
                    label = "Phase-in",
                    value = FALSE),
      uiOutput("piyr1slider"),
      h4("Corrections"),
      checkboxInput(inputId = "parallel",
                    label = "Parallel pre-intervention trends",
                    value = FALSE),
      checkboxInput("pillscare",
                    "1996 'Pill scare' as outlier",
                    value = FALSE),
      h6("Autoregression correction"),
      column(6, numericInput("p", "AR: ", 0, min = 0)),
      column(6, numericInput("q", "MA: ", 0, min = 0)),
      ),
      fluidRow(style = "margin: 0px;",
      h4("Download Graph"),
      column(4, numericInput("width", "Width (mm)", 200)),
      column(4, numericInput("height", "Height (mm)", 150)),
      column(4, radioButtons("format", "Format", choices = c("png", "svg"), selected = "png")),
      ),
      fluidRow(style = "margin: 0px;",
      column(6, 
      # downloadButton("ggplot", label = "Download ggplot")
      downloadButton("dlppt", label = "Download .pptx",
                     style = "background-color: #c6c6e3; border-color: #9a9ab9;")
             ),
      column(6,
      downloadButton("dlgraph", label = "Download image",
                     style = "background-color: #c6c6e3; border-color: #9a9ab9;")
      )
      ),
      br(),
      fluidRow(style = "margin: 0px;",
        h4("Download analysis report (.docx)"),
        column(6, 
               downloadButton("downloadReport", label = "Download report", 
                              style = "background-color: #c6c6e3; border-color: #9a9ab9;")
        ),
        column(6,
               bookmarkButton("Save analysis", title = "Copy url with current values", 
                              style = "background-color: #c6c6e3; border-color: #9a9ab9;")
        ),
      )
    ),
    
    
    
    mainPanel(
      tabsetPanel(type = "tabs", id = "session",
                  
                  tabPanel("Introduction",
                           includeMarkdown("intro.md"),
                           actionButton("go2graph", "Go to graph and results")
                           ),
                  
                  tabPanel("Full Plot",
                           div(id = "plot",
                           h3(textOutput(outputId = "minmax")),
                           plotOutput(outputId = "modelplot")
                           ),
                           # textOutput("form1"),
                          div(id = "graph_info",
                           column(6, align = "left", p(htmlOutput(outputId = "rSquared"))),
                           column(6, align = "right", p(textOutput(outputId = "corr")))
                          ),
                           div(id = "graph_options",
                           column(4, align = "left",  checkboxInput("ribbons", "Show confidence intervals", value = FALSE)),
                          column(4, align = "center", checkboxInput("grey", "Grey-out controls", value = FALSE)),
                          column(4, align = "right", checkboxInput("lines", "Show trend lines", value = TRUE))
                           ),
                          div(id = "equation",
                           column(12, align = "center", uiOutput(outputId = "equation"))
                          ),
                           DT::dataTableOutput(outputId = "modelsummary")
                           ),
                  
                  tabPanel("Confidence Intervals",
                                      DT::dataTableOutput((outputId = "confint")),
                           # textOutput("form2"),
                           br(),
                           downloadButton("dlconfints")
                  ),
                  
                  tabPanel("Autocorrelation tests", 
                           h3("Autocorrelation plots"), p("Residuals plotted by time, and autocorrelation and partial-autocorrelation function plots"),
                           plotOutput("autocorr"),
                           h3("Durbin-Watson test"),
                           DT::dataTableOutput("dwt"),
                           br(),
                           # htmlOutput(outputId = "corrcompare")
                           h4(textOutput("pplus1_title")),
                           DT::dataTableOutput("pplus1"),
                           h4(textOutput("qplus1_title")),
                           DT::dataTableOutput("qplus1")
                           ),
                  
                  tabPanel("Dataframe for model", DT::dataTableOutput(outputId = "dataframesumm"))
                  # tabPanel("Dataframe for model", DT::dataTableOutput(outputId = "fulldata"))
      )
    )
  )
)
}
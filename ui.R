ui <- fluidPage(
  theme = shinythemes::shinytheme(theme = ifelse("shinythemes" %in% installed.packages()[,"Package"], "yeti", NULL)),
  titlePanel("ITS analyses of England's Teenage Pregnancy Strategy"),
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; } 
                   #inline .form-group { display: table-row;}")
        ),
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
                  choices = c("Scotland", "England", "Wales", "England and Wales"),
                  selected = "Scotland"),
      uiOutput("dateslider"),
      # checkboxInput(inputId = "int1",
      #               label = "Intervention 1",
      #               value = TRUE),
      uiOutput("intyr1slider"),
      checkboxInput(inputId = "int2",
                    label = "Intervention 2",
                    value = TRUE),
      uiOutput("intyr2slider"),
      checkboxInput(inputId = "pi1",
                    label = "Phase-in",
                    value = FALSE),
      uiOutput("piyr1slider"),
      p("Autoregression correction"),
      tags$div(id = "inline", column(6, numericInput("p", "AR: ", 0, min = 0)),
      column(6, numericInput("q", "MA: ", 0, min = 0))),
      h4("Download Graph"),
      column(4, numericInput("width", "Width (mm)", 200)),
      column(4, numericInput("height", "Height (mm)", 150)),
      column(4, radioButtons("format", "Format", choices = c("png", "svg"), selected = "png")),
      downloadButton("dlgraph")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Full Plot",
                           h3(textOutput(outputId = "minmax")),
                           plotOutput(outputId = "modelplot"),
                           column(6, checkboxInput("ribbons", "Show confidence intervals", value = FALSE)),
                           column(6, p(align = "right", textOutput(outputId = "corr"))),
                           dataTableOutput(outputId = "modelsummary")
                           ),
                  # tabPanel("testing objects",
                  #          h1(textOutput(outputId = "test")),
                  #          dataTableOutput(outputId = "fulldata"),
                  #                     plotOutput(outputId = "modelplotsimple"),
                  #                     dataTableOutput((outputId = "cfac"))
                  # ),
                  tabPanel("Autocorrelation tests", 
                           h3("Autocorrelation plots"), p("Residuals plotted by time, and autoregression and partial-autoregression function plots"),
                           plotOutput("autocorr"),
                           h3("Durbin-Watson test"),
                           dataTableOutput("dwt"),
                           br(),
                           htmlOutput(outputId = "corrcompare")
                           ),
                  tabPanel("Dataframe for model", dataTableOutput(outputId = "dataframesumm"))
      )
    )
  )
)

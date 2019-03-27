ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
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
      checkboxInput(inputId = "int1",
                    label = "Intervention 1",
                    value = TRUE),
      uiOutput("intyr1slider"),
      checkboxInput(inputId = "int2",
                    label = "Intervention 2",
                    value = TRUE),
      uiOutput("intyr2slider"),
      checkboxInput(inputId = "pi1",
                    label = "Phase-in",
                    value = FALSE),
      uiOutput("piyr1slider"),
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
                           dataTableOutput(outputId = "modelsummary")),
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
                           dataTableOutput("dwt")),
                  tabPanel("Dataframe for model", dataTableOutput(outputId = "dataframesumm"))
      )
    )
  )
)

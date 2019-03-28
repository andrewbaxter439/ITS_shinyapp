list.of.packages <- c("tidyverse", "readxl", "broom", "nlme", "car", "svglite")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

require(tidyverse)
require(readxl)
require(broom)
require(nlme)
require(car)
require(svglite)


# Setup data and functions -----------------------------------------------------------------------------------



testAutocorr <- function(model, data=NULL, max.lag = 10, time.points = 25) {
  data <- eval(model$call$data)  # Only works if 'lm' call has dataframe named in bracket
  # print(dwt(model, max.lag = max.lag, alternative = "two.sided"))
  par(cex = 0.7, mai = c(0.1, 0.1, 0.2, 0.1))
  par(fig = c(0.03, 1, 0.8, 1))
  plot(
    data$Year[1:time.points],
    residuals(model)[1:time.points],
    type = 'o',
    pch = 16,
    col = "red"
  )
  
  par(fig = c(0.03, 0.5, 0.05, 0.75), new = TRUE)
  acf(residuals(model))
  
  par(fig = c(0.55, 1, 0.05, 0.75), new = TRUE)
  acf(residuals(model), type = 'partial')
}

constructCIRibbon <- function(newdata, model) {
  newdata <- newdata %>%
    mutate(Predict = predict(model, newdata = newdata))
  mm <- model.matrix(as.formula(paste0("~ ", model$call$model[3])),
                     data = newdata)
  vars <- mm %*% vcov(model) %*% t(mm)
  sds <- sqrt(diag(vars))
  newdata <- newdata %>% mutate(lowCI = Predict - 1.96 * sds,
                                HiCI = Predict + 1.96 * sds)
}

printCoefficients <- function(model){
  as_tibble(trimws(format(round(summary(model)$tTable, 3), nsmall=3))) %>%
    mutate(Coefficient = rownames(summary(model)$tTable)) %>% 
    select(Coefficient, Value, Std.Error, 'p-value') %>% 
    print()
}

#** Server ----


server <- function(input, output) {
  

# Reactive inputs -------------------------------------------------------------------------------------------

  output$dlgraph <- downloadHandler(
    #   filename = function() {paste0("ITS controlled.", input$format)},
    filename = function() {paste0(input$main, " ", minYr(), "-", maxYr(), ".", input$format)},
    content = function(file) {ggsave(file, PlotInput(), dpi = 500, units = "mm", width = input$width, height = input$height)},
    contentType = paste0("image/", input$format)
  )
  
  output$dateslider <- renderUI({
    sliderInput(
      inputId = "obRange",
      label = "Select date range to observe",
      min = minYr(),
      max = maxYr(),
      value = c(minYr(), maxYr()),
      step = 1,
      sep=""
    )
  })
  
  output$intyr1slider <- renderUI({
    sliderInput(inputId = "int1yr",
                label = "Beginning of intervention 1:",
                min = minYr()+2,
                max = maxYr()-2,
                step = 1,
                sep="",
                value = 1999)
  })
  
  
  output$intyr2slider <- renderUI ({
    sliderInput(inputId = "int2yr",
                label = "Beginning of intervention 2:",
                min = startYr()+2,
                max = maxYr()-2,
                step = 1,
                sep="",
                value = 2008)
  })
  
  output$piyr1slider <- renderUI ({
    sliderInput(inputId = "pi1yr",
                label = "Intervention 1 phase-in period:",
                min = input$int1yr,
                max = maxYr()-1,
                step = 1,
                sep="",
                value = input$int1yr+1)
  })
  

# Dataframe setup --------------------------------------------------------------------------------------------

  
  
  all.UK.rates <- reactive({read_xlsx("Conception rates by age and country.xlsx", sheet = paste(input$ages))})
  output$fulldata <- renderDataTable(all.UK.rates())
  
  dfa <- reactive({
    all.UK.rates() %>% filter(Country == input$main |
                                Country == input$control) %>%
      gather("Year", "Value",-1) %>%
      filter(!is.na(Value)) %>%
      arrange(Country) %>%
      mutate(
        Year = as.numeric(Year),
        # Time = Year - min(Year) + 1,
        England = ifelse(Country == input$main, 1, 0)
        ) %>% 
      mutate(Year_Eng = Year*England)
  })
  
  minYr <- reactive({
    dfa() %>% filter(Country == input$main) %>% summarise(min(Year)) %>% pull()
  })
  
  maxYr <- reactive({
    dfa() %>% filter(Country == input$main) %>% summarise(max(Year)) %>% pull()
  })
  
  output$minmax <- renderText({
    paste(input$main, "compared with", input$control, input$obRange[1], "-", input$obRange[2], sep=" ")
  })
  
  startYr <- reactive({
    if (input$pi1) {
      input$pi1yr+1 
    } else {
      input$int1yr
    }
  })
  
  dfaPI <- reactive({
    if (input$pi1) {
      dfa() %>% 
        filter(Year < input$int1yr | Year > input$pi1yr)
    } else {
      dfa()
    }
  })
  
  
  dfb <- reactive({
      dfaPI() %>% 
        mutate(Cat1 = ifelse(Year < input$int1yr, 0, 1),
               Trend1 = ifelse(Cat1 == 0, 0, Year - startYr() + 1)
        ) %>%
        mutate_at(., colnames(.)[6:7], list(Eng = ~ .*England)) 
  })
  
  dfc <- reactive({
    if (input$int2) {
      dfb() %>% 
        mutate(Cat2 = ifelse(Year < input$int2yr, 0, 1),
               Trend2 = ifelse(Cat2 == 0, 0, Year - input$int2yr + 1)
        ) %>%
        mutate_at(., colnames(.)[10:11], list(Eng = ~ .*England)) %>% 
        filter(Year >= input$obRange[1],
               Year <= input$obRange[2])
    } else {
      dfb() %>% filter(Year >= input$obRange[1],
                       Year <= input$obRange[2]) %>% 
        mutate(Cat2 = 0)
    }
  })
  
  output$modelplotsimple <- renderPlot({
    ggplot(data=dfc(), aes(
      Year,
      Value,
      group = interaction(Country, Cat1, Cat2),
      col = Country
    )) +
      geom_point() + geom_smooth(method = "lm", se = FALSE)
  })
  
  output$dataframesumm <- renderDataTable(
    (arrange(dfc(), by=Year)),
    options = list(searching = FALSE)
  )
  
  modelGls_null <- reactive({  # add if statements for each model
    if (input$int2) {
      gls(
        Value ~ Year +
          England +
          Year_Eng +
          Cat1 +
          Trend1 +
          Cat1_Eng +
          Trend1_Eng +
          Cat2 +
          Trend2 +
          Cat2_Eng +
          Trend2_Eng,
        data = dfc(),
        correlation = NULL,
        method = "ML"
      )} else {
        gls(
          Value ~ Year +
            England +
            Year_Eng +
            Cat1 +
            Trend1 +
            Cat1_Eng +
            Trend1_Eng,
          data = dfc(),
          correlation = NULL,
          method = "ML"
        )}
  })
  
  modelGls <- reactive({  # add if statements for each model
    if (input$int2) {
      lm(
        Value ~ Year +
          England +
          Year_Eng +
          Cat1 +
          Trend1 +
          Cat1_Eng +
          Trend1_Eng +
          Cat2 +
          Trend2 +
          Cat2_Eng +
          Trend2_Eng,
        data = dfc()
      )} else {
        lm(
          Value ~ Year +
            England +
            Year_Eng +
            Cat1 +
            Trend1 +
            Cat1_Eng +
            Trend1_Eng,
          data = dfc()
        )}
  })
  
  
  
  output$modelsummary <- renderDataTable(
    printCoefficients(modelGls_null()), options = list(searching = FALSE, paging = FALSE)
  )
  
  # Create cfac -----
  
  modcfac_base <- reactive({
    if(input$int2) {
      tibble(
        Year       = c(startYr():maxYr()),
        England    = 1,
        Year_Eng   = c(startYr():maxYr()),
        Cat1       = 1,
        Trend1     = c(1:(maxYr()-startYr()+1)),
        Cat2       = c(rep(0,(input$int2yr-startYr())), rep(1,(maxYr()-input$int2yr+1))),
        Trend2     = c(rep(0,(input$int2yr-startYr())), 1:(maxYr()-input$int2yr+1)), 
        Cat1_Eng   = c(rep(0,(input$int2yr-startYr())), rep(1,(maxYr()-input$int2yr+1))), 
        Trend1_Eng = c(rep(0,(input$int2yr-startYr())), (input$int2yr-startYr()+1):(maxYr()-startYr()+1)),
        Cat2_Eng   = 0,
        Trend2_Eng = 0
        # Remove _Eng interactions (retaining 1st intervention interactions for 2nd int)
      ) 
    } else {
      tibble(
        Year       = c(startYr():maxYr()),
        England    = 1,
        Year_Eng   = c(startYr():maxYr()),
        Cat1       = 1,
        Trend1     = c(1:(maxYr()-startYr()+1)),
        Cat2       = 0,
        Trend2     = 0,
        Cat1_Eng   = 0,
        Trend1_Eng = 0,
        Cat2_Eng   = 0,
        Trend2_Eng = 0
        # Remove _Eng interactions (retaining 1st intervention interactions for 2nd int)
      ) 
      
    }
  })
  
  ###Tricky bits here
  modcfac <- reactive({
    left_join(modcfac_base(), constructCIRibbon(modcfac_base(), modelGls_null()))
  })
  ###
  
  output$cfac <- renderDataTable(modcfac())
  
  ylim <- reactive({
    c(0, 1.1*max(modcfac()$HiCI, dfd()$Value))
  })
  
  dfd <- reactive({
    left_join(dfc(), constructCIRibbon((dfc() %>% filter(England==1, Year >= startYr())), modelGls_null())) 
  })
  
  output$dfd <- renderDataTable(dfd())
  
  # Output plot ----
  
  output$modelplot <- renderPlot({
    print(PlotInput())
  })
  
  PlotInput <- reactive({
    dfd() %>% 
      mutate(Predict = predict(modelGls_null())) %>%  # Add Predicts for non-England
      ggplot(aes(
        Year,
        Value,
        col = Country,
        fill = Country,
        group = interaction(Country, Cat1, Cat2)
      )) +
      # Show all data points
      geom_point(data=dfa(), aes(Year, Value, col = Country), show.legend = FALSE, inherit.aes = FALSE) +
      # Counterfactual trend lines
      geom_line(
        data = modcfac(),
        aes(
          x = Year,
          y = Predict,
          group = Cat2,
          col = "Control",
          fill = NULL
        ),
        linetype = "longdash",
        size = 1,
        inherit.aes = FALSE
      ) +
      # Counterfactual confidence intervals (not shown in legend)
      geom_ribbon(
        data=modcfac(),
        aes(
          x=Year,
          ymin = lowCI,
          ymax=HiCI,
          group = Cat2,
          col=NULL,
          fill= ifelse(input$ribbons,"Control","#00000000")
        ),
        alpha=0.5,
        size = 1,
        show.legend = FALSE,
        inherit.aes = FALSE) +
      # Model trend lines
      geom_line(aes(y=Predict), size = 1) +
      # Confidence intervals (not shown in legend)
      geom_ribbon(
        aes(
          x=Year,
          ymin = lowCI,
          ymax=HiCI,
          col=NULL,
          fill= ifelse(input$ribbons,Country,"#00000000")
        ),
        alpha= 0.5,
        size = 1,
        show.legend = FALSE) +
      # Intervention time points
      geom_vline(xintercept = input$int1yr-0.5,
                 linetype = "dotted",
                 col = "#000000CC") +
      geom_vline(xintercept = input$int2yr-0.5,
                 linetype = "dotted",
                 col = ifelse(input$int2, "#000000CC", NA)) +
      geom_rect(
        xmin = input$int1yr-0.5,
        xmax = input$pi1yr+0.5,
        ymin = 0,
        ymax = ylim()[2],
        fill = ifelse(input$pi1, "grey", NA),
        alpha = 0.01,
        inherit.aes = FALSE
      ) +
      # Display parameters
      theme(panel.background = element_blank(),
            legend.key  = element_blank(),
            panel.grid = element_blank()) +
      ylab("Rate of pregnancies to under-18s, per 1,000") +
      xlab("Year") +
      coord_cartesian(ylim = ylim()) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(limits = c(minYr(), NA))+
      scale_colour_manual(
        breaks = c("England", "Wales", "Scotland", "England and Wales", "Control"),
        values = c("Wales" = "#00AB39",
                   "Scotland" = "#0072C6",
                   "England" = "#CF142B",
                   "England and Wales" = "#CF142B",
                   "Control" = "#F7D917"),
        aesthetics = c("colour", "fill"))
  })
  
  # autocorr tests ---------------------------------------------------------------------------------------------

  
  output$dwt <- renderDataTable(
    (
    data.frame(lag = 1:12, dwt(modelGls(), max.lag = 12, alternative = "two.sided")[1:3]) %>% rename(Autocorrelation =r, DW_Stat = dw, pvalue=p)
       ),
    options = list(searching = FALSE, paging = FALSE)
  )
  
  output$autocorr <- renderPlot({
    rows <- maxYr()-minYr()+1
    par(cex = 0.7, mai = c(0.1, 0.1, 0.2, 0.1))
    par(fig = c(0.03, 1, 0.8, 1))
    plot(
      dfc()$Year[1:rows],
      residuals(modelGls())[1:rows],
      type = 'o',
      pch = 16,
      col = "red"
    )
    
    par(fig = c(0.03, 0.5, 0.05, 0.75), new = TRUE)
    acf(residuals(modelGls()))
    
    par(fig = c(0.55, 1, 0.05, 0.75), new = TRUE)
    acf(residuals(modelGls()), type = 'partial')
  })


}

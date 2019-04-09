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
  data <- eval(model$call$data)
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

server <- function(input, output) {
  

# Reactive inputs -------------------------------------------------------------------------------------------

  output$dlgraph <- downloadHandler(
    #   filename = function() {paste0("ITS controlled.", input$format)},
    filename = function() {paste0(input$main, " ", minYr(), "-", maxYr(), ".", input$format)},
    content = function(file) {ggsave(file, PlotInput(), dpi = 400, units = "mm", width = input$width, height = input$height)},
    contentType = paste0("image/", input$format)
  )
  
  defaultMin <- reactive({
    max(
      dfa() %>% 
        filter(Country == input$main) %>% 
        summarise(min(Year)) %>% 
        pull(),
      dfa() %>% 
        filter(Country == input$control) %>% 
        summarise(min(Year)) %>% 
        pull()
    )
  })
  
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
  
# Start and end years ----------------------------------------------------------------------------------------

  
  minYr <- reactive({
    dfa() %>% 
      # filter(Country == input$main) %>% 
      summarise(min(Year)) %>% 
      pull()
  })
  
  maxYr <- reactive({
    dfa() %>% 
      # filter(Country == input$main) %>%
      summarise(max(Year)) %>% 
      pull()
  })
  
  output$minmax <- renderText({
    paste(input$main, ifelse(input$control=="none", "", paste("compared with", input$control, sep = " ")), input$obRange[1], "-", input$obRange[2], sep=" ")
  })

# Phase-in period --------------------------------------------------------------------------------------------

  startYr <- reactive({  # start of intervention offset by phase-in year
    if (input$pi1) {
      input$pi1yr+1 
    } else {
      input$int1yr
    }
  })
  
# Dataframe setup --------------------------------------------------------------------------------------------

  
  
  all.UK.rates <- reactive({read_xlsx("Conception rates by age and country.xlsx", sheet = paste(input$ages))})
  output$fulldata <- renderDataTable(all.UK.rates())
  
  dfa <- reactive({
    all.UK.rates() %>% filter(Country == input$main |
                                Country == input$control) %>%
      gather("Year", "Value",-1) %>%
      filter(!is.na(Value)) %>%
      mutate(
        Year = as.numeric(Year),
        Time = Year - min(Year) + 1,
        England = ifelse(Country == input$main, 1, 0)
        ) %>% 
      mutate(Time_Eng = Time*England)
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
        mutate_at(., colnames(.)[7:8], list(Eng = ~ .*England)) 
  })
  
  dfc <- reactive({
    if (input$int2) {
      dfb() %>% 
        mutate(Cat2 = ifelse(Year < input$int2yr, 0, 1),
               Trend2 = ifelse(Cat2 == 0, 0, Year - input$int2yr + 1)
        ) %>%
        mutate_at(., colnames(.)[11:12], list(Eng = ~ .*England)) %>% 
        filter(Year >= input$obRange[1],
               Year <= input$obRange[2])
    } else {
      dfb() %>% filter(Year >= input$obRange[1],
                       Year <= input$obRange[2]) %>% 
        mutate(Cat2 = 0)
    }
  })
  
  # output$modelplotsimple <- renderPlot({
  #   ggplot(data=dfc(), aes(
  #     Year,
  #     Value,
  #     group = interaction(Country, Cat1, Cat2),
  #     col = Country
  #   )) +
  #     geom_point() + geom_smooth(method = "lm", se = FALSE)
  # })  # Simple plot (not used)
  
  output$dataframesumm <- renderDataTable(
    (arrange(dfc(), by=Year)),
    options = list(searching = FALSE)
  )

# Construct model --------------------------------------------------------------------------------------------

  modelGls_null <- reactive({  # add if statements for each model
    if (input$control == "none") {       # outer if - with/without control (x1)
      if (input$p == 0 & input$q == 0){  # second if - with or without ARMA correction (x2)
        if (input$int2) {                # innermost if - with or without second intervention (x2x2)
          gls(
            Value ~ Time +
              Cat1 +
              Trend1 +
              Cat2 +
              Trend2,
            data = dfc(),
            correlation = NULL,
            method = "ML"
          )
        } else {
          gls(
            Value ~ Time +
              Cat1 +
              Trend1,
            data = dfc(),
            correlation = NULL,
            method = "ML"
          )
        }
      } else {
        if (input$int2) {
          gls(
            Value ~ Time +
              Cat1 +
              Trend1 +
              Cat2 +
              Trend2,
            data = dfc(),
            correlation = corARMA(p=input$p, q=input$q, form = ~ Time),
            method = "ML"
          )} else {
            gls(
              Value ~ Time +
                Cat1 +
                Trend1,
              data = dfc(),
              correlation =  corARMA(p=input$p, q=input$q, form = ~ Time),
              method = "ML"
            )}
        
      }
    } else {
      if (input$p == 0 & input$q == 0){
        if (input$int2) {
          gls(
            Value ~ Time +
              England +
              Time_Eng +
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
              Value ~ Time +
                England +
                Time_Eng +
                Cat1 +
                Trend1 +
                Cat1_Eng +
                Trend1_Eng,
              data = dfc(),
              correlation =  NULL,
              method = "ML"
            )}
      } else {
        if (input$int2) {
          gls(
            Value ~ Time +
              England +
              Time_Eng +
              Cat1 +
              Trend1 +
              Cat1_Eng +
              Trend1_Eng +
              Cat2 +
              Trend2 +
              Cat2_Eng +
              Trend2_Eng,
            data = dfc(),
            correlation = corARMA(p=input$p, q=input$q, form = ~ Time | England),
            method = "ML"
          )} else {
            gls(
              Value ~ Time +
                England +
                Time_Eng +
                Cat1 +
                Trend1 +
                Cat1_Eng +
                Trend1_Eng,
              data = dfc(),
              correlation =  corARMA(p=input$p, q=input$q, form = ~ Time | England),
              method = "ML"
            )}
        
      }
    }
  })
  

# Simple linear model ----------------------------------------------------------------------------------------

  modelGls <- reactive({  # model to check for autocorrelation
    if (input$control == "none") {
      if (input$int2) {
        lm(
          Value ~ Time +
            Cat1 +
            Trend1 +
            Cat2 +
            Trend2,
          data = dfc()
        )
      } else {
        lm(
          Value ~ Time +
            Cat1 +
            Trend1,
          data = dfc() 
        )
      }
    } else {
      if (input$int2) {
        lm(
          Value ~ Time +
            England +
            Time_Eng +
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
            Value ~ Time +
              England +
              Time_Eng +
              Cat1 +
              Trend1 +
              Cat1_Eng +
              Trend1_Eng,
            data = dfc()
          )}
    }
  })
  

# Outputs to display -----------------------------------------------------------------------------------------

  output$rSquared <- renderUI(HTML(paste0("R<sup>2</sup>: ", signif(summary(modelGls())$r.squared,digits = 4))))
  
  interceptName <- reactive({
    if (input$control == "none"){
      input$main
    } else {
      input$control
    }
  })
  
  
  modelTable <- reactive({  # labelling coefficients
    tb <- printCoefficients(modelGls_null())
    tb$Coefficient[1:2] <- c(paste0(interceptName(), " rate at ", minYr()-1),
                             paste0(interceptName(), " base trend"))
                             # paste0(input$main, " difference in rate at ", minYr()-1),
                             # paste0(input$main, " difference in base trend"))
    tb
  })
  
  
  output$modelsummary <- renderDataTable(
    modelTable(), options = list(searching = FALSE, paging = FALSE)
  )
  
  # Create cfac --------------------------------------------------------
  
  modcfac_base <- reactive({
    if(input$control == "none"){
    if(input$int2) {
      tibble(
        Time       = c((startYr()-minYr()+1):(maxYr() - minYr()+1)),
        Cat1       = c(rep(0,(input$int2yr-startYr())), rep(1,(maxYr()-input$int2yr+1))),
        Trend1     = c(rep(0,(input$int2yr-startYr())), ((maxYr()-input$int1yr+2)-(input$int2yr-startYr())):(maxYr()-input$int1yr+1)), 
        Cat2       = 0,
        Trend2     = 0
      ) 
    } else {
      tibble(
        Time       = c((startYr()-minYr()+1):(maxYr() - minYr()+1)),
        Cat1       = 0,
        Trend1     = 0,
        Cat2       = 0,
        Trend2     = 0
      )
    }
    } else {
    if(input$int2) {
      tibble(
        Time       = c((startYr()-minYr()+1):(maxYr() - minYr()+1)),
        England    = 1,
        Time_Eng   = c((startYr()-minYr()+1):(maxYr() - minYr()+1)),
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
        Time       = c((startYr()-minYr()+1):(maxYr() - minYr()+1)),
        England    = 1,
        Time_Eng   = c((startYr()-minYr()+1):(maxYr() - minYr()+1)),
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
  }
    })
  

  modcfac <- reactive({
    left_join(modcfac_base(), constructCIRibbon(modcfac_base(), modelGls_null()))
  })

  
  output$cfac <- renderDataTable(modcfac())
  
  ylim <- reactive({
    c(0, 1.1*max(modcfac()$HiCI, dfd()$Value))
  })
  
  dfd <- reactive({
    left_join(dfc(), constructCIRibbon((dfc() %>% filter(England==1, Year >= startYr())), modelGls_null())) 
  })
  
  output$dfd <- renderDataTable(dfd())
  
  # Output plot -----------------------------------------------------------------
  
  output$modelplot <- renderPlot({
    print(PlotInput())
  })
  
  PlotInput <- reactive({
    
     minlb <- ceiling(min(dfc()$Year, startYr())/5) * 5
     mxlb <- floor(max(dfc()$Year)/5) * 5
     
     mnlbTm <- unique(dfc()[which(dfc()$Year==minlb),]$Time)
     mxlbTm <- unique(dfc()[which(dfc()$Year==mxlb),]$Time)
    
    dfd() %>% 
      arrange(by = Country) %>%
      mutate(Predict = predict(modelGls_null(), .)) %>%  # Add Predicts for non-England
      ggplot(aes(
        Time,
        Value,
        col = Country,
        fill = Country,
        group = interaction(Country, Cat1, Cat2)
      )) +
      # Counterfactual confidence intervals (not shown in legend)
      geom_ribbon(
        data=modcfac(),
        aes(
          x=Time,
          ymin = lowCI,
          ymax=HiCI,
          group = interaction(Cat1, Cat2),
          col=NULL,
          fill= ifelse(input$ribbons,"Prediction","#00000000")
        ),
        alpha=0.5,
        size = 1,
        show.legend = FALSE,
        inherit.aes = FALSE) +
      # Model confidence intervals (not shown in legend)
      geom_ribbon(
        aes(
          x=Time,
          ymin = lowCI,
          ymax=HiCI,
          col=NULL,
          fill= ifelse(input$ribbons,Country,"#00000000")
        ),
        alpha= 0.5,
        size = 1,
        show.legend = FALSE) +
      # Show all data points
      geom_point(data=dfa(), aes(Time, Value, col = Country), show.legend = FALSE, inherit.aes = FALSE) +
      # Counterfactual trend lines
      geom_line(
        data = modcfac(),
        aes(
          x = Time,
          y = Predict,
          group = interaction(Cat1, Cat2),
          col = "Prediction",
          fill = NULL
        ),
        linetype = "longdash",
        size = 1,
        inherit.aes = FALSE
      ) +
      # Model trend lines
      geom_line(aes(y=Predict), size = 1) +
      # Intervention time points
      geom_vline(xintercept = input$int1yr-minYr()+0.5,
                 linetype = "dotted",
                 col = "#000000CC") +
      geom_vline(xintercept = input$int2yr-minYr()+0.5,
                 linetype = "dotted",
                 col = ifelse(input$int2, "#000000CC", NA)) +
      geom_rect(
        xmin = input$int1yr-minYr()+0.5,
        xmax = input$pi1yr-minYr()+1.5,
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
      scale_x_continuous(limits = c(0, NA), breaks = seq(mnlbTm, mxlbTm, by=5), labels = seq(minlb, mxlb, by=5)) +
      scale_colour_manual(
        breaks = c("England", "Wales", "Scotland", "England and Wales", "Prediction"),
        values = c("England" = "#CF142B",
                   "Wales" = "#00AB39",
                   "Scotland" = "#0072C6",
                   "England and Wales" = "#CF142B",
                   "Prediction" = "#F7D917"),
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
    rows <- input$obRange[2]-input$obRange[1]+1
    par(cex = 0.7, mai = c(0.1, 0.1, 0.2, 0.1))
    par(fig = c(0.03, 1, 0.8, 1))
    plot(
      unique(dfc()$Year)[1:rows],
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
  
corr <- reactive({
  curr <- paste0("AR", input$p, ", MA", input$q)
  paste0("Autocorrelation correction: ", ifelse(input$p == 0 & input$q == 0, "none", curr))
})  

output$corr <- renderText(corr())

output$corrcompare <- renderUI({
  pplus1 <- modelGls_null() %>% 
    update(correlation = corARMA(p = input$p + 1, q = input$q,
                                 form = ~ Time | England)) %>% 
    anova(modelGls_null()) %>% .[2, 'p-value']
  qplus1 <- modelGls_null() %>% 
    update(correlation = corARMA(p = input$p, q = input$q + 1,
                                 form = ~ Time | England)) %>% 
    anova(modelGls_null()) %>% .[2, 'p-value']
  HTML(paste(
    corr(),
    paste0("ANOVA comparison with model AR", input$p+1, ", MA", input$q, ": p = ", round(pplus1, 3)),
    paste0("ANOVA comparison with model AR", input$p, ", MA", input$q+1, ": p = ", round(qplus1, 3)),
    sep = "<br>"
  ))
  
})


}

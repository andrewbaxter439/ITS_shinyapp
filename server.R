require(tidyverse)
require(broom)
require(nlme)
require(car)
require(export)
require(svglite)
require(XLConnect)


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

constructCIRibbon <- function(newdata, model, formula) {
  
  model$call[[2]] <- formula
  
  df <- newdata %>%
    mutate(Predict = predict(model, newdata = .))
  
  mm <- model.matrix(formula,
                     data = df)
  
  vars <- mm %*% vcov(model) %*% t(mm)
  sds <- sqrt(diag(vars))
  
  df <- df %>% mutate(se = sds,
                                lowCI = Predict - 1.96 * sds,
                                HiCI = Predict + 1.96 * sds)
}
  

printCoefficients <- function(model){
  as_tibble(trimws(format(round(summary(model)$tTable, 3), nsmall=3))) %>%
    mutate(Coefficient = rownames(summary(model)$tTable)) %>% 
    select(Coefficient, Value, Std.Error, 'p-value') %>% 
    print()
}
# app server ---------------------------------------

server <- function(input, output, session) {
  
  # Reactive inputs -------------------------------------------------------------------------------------------
  
  observeEvent(input$go2graph, {
    updateTabsetPanel(session, "session", selected = "Full Plot")
  })
  
  output$dlgraph <- downloadHandler(
    filename = function() {paste0(input$main, " vs ", input$control, " ", input$obRange[1], "-", input$obRange[2], ".", input$format)},
    content = function(file) {
      ggsave(file, PlotInput(), dpi = 400, units = "mm", width = input$width, height = input$height)
    },
    contentType = paste0("image/", input$format)
  )
  
  output$dlppt <- downloadHandler(
    filename = function() {paste0(input$main, " vs ", input$control, " ", input$obRange[1], "-", input$obRange[2], ".pptx")},
    content = function(file){graph2ppt(PlotInput() + theme(text = element_text(size = 16), line = element_blank()), file = file, height = input$height/25.4, width = input$width/25.4)}
  )
  
  
  
  output$dlconfints <- downloadHandler(
    filename = function() {paste0(input$main, " vs ", input$control, " ", input$obRange[1], "-", input$obRange[2], ".xlsx")},
    content = function(file) {
      fname <- paste(file,"xlsx",sep=".")
      wb <- loadWorkbook(fname, create = TRUE)
      createSheet(wb, name = "model")
      createSheet(wb, name = "data")
      createSheet(wb, name = "counterfactual")
      writeWorksheet(wb, confintervals(), sheet = "model")
      writeWorksheet(wb, dfd(), sheet = "data")
      writeWorksheet(wb, modcfac(), sheet = "counterfactual")
      saveWorkbook(wb)
      file.rename(fname,file)
    },
    contentType = "file/xlsx"
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
    paste(input$main,
          ifelse(input$control=="none", "", paste("compared with", input$control, sep = " ")),
          input$obRange[1], "-",
          input$obRange[2],
          sep=" ")
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
  
  load("data/all_uk_rates.rdata")
  # output$fulldata <- renderDataTable(modcfac())
  output$fulldata <- renderDataTable(all_UK_rates[[input$ages]])
  
  dfa <- reactive({
    all_UK_rates[[input$ages]] %>% 
      filter(Country == input$main |
               Country == input$control) %>%
      filter(!is.na(Value)) %>%
      mutate(
        Year = as.numeric(Year),
        Time = Year - min(Year) + 1,
        England = ifelse(Country == input$main, 1, 0)
      )    %>%    
      mutate(Time_Eng = Time*England)
  })
  
  dfa2 <- reactive({
    dfa() %>% 
      mutate(Time = Year - input$obRange[1] + 1,
             Time_Eng = Time*England)
  })
  
  
  dfaPI <- reactive({
    if (input$pi1) {
      dfa2() %>% 
        filter(Year < input$int1yr | Year > input$pi1yr)
    } else {
      dfa2()
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
               Year <= input$obRange[2]) %>% 
        mutate(PillScare = ifelse(Year>1995, 1, 0))
    } else {
      dfb() %>% filter(Year >= input$obRange[1],
                       Year <= input$obRange[2]) %>% 
        mutate(Cat2 = 0)%>% 
        mutate(PillScare = ifelse(Year>1995, 1, 0))
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
  
  # formula  ----------------------------------------------------------------------------------------------
  # 
  mod_formula <- reactive({
    formula(
      paste0(
        "Value ~ Time",
        ifelse(input$control == "none", "", paste0(" + England", ifelse(input$parallel, "", " + Time_Eng"))),
        " + Cat1 + Trend1",
        ifelse(input$control == "none", "", " + Cat1_Eng + Trend1_Eng"),
        ifelse(input$int2,
               paste0(" + Cat2 + Trend2",
                      ifelse(input$control == "none", "", " + Cat2_Eng + Trend2_Eng")
               ),
               ""
        ),
        ifelse(input$pillscare, " + PillScare", "")
      )
    )
  })
  # 
  output$form1 <- renderText({as.character(predict(modelGls_null()))})
  # output$form2 <- renderText({as.character(modelGls_null()$call$model[3])})
  
  # Construct model --------------------------------------------------------------------------------------------
  
  modelGls_null <- reactive({  # add if statements for each model
    
    if (input$p == 0 & input$q == 0){
    model <- gls(
      model = mod_formula(),
      correlation = NULL,
      data = dfc(),
      method = "ML"
    )
    } else {
      if (input$control == "none") {
    model <- gls(
      model = mod_formula(),
      correlation = corARMA(p=input$p, q=input$q, form = ~ Time),
      data = dfc(),
      method = "ML"
    )
        
      } else {
    model <- gls(
      model = mod_formula(),
      correlation = corARMA(p=input$p, q=input$q, form = ~ Time | England),
      data = dfc(),
      method = "ML"
    )
      }
    }
    
    
    
  })
  modelGls_null_unused <- reactive({  # add if statements for each model
    
    
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
            # model = model1,
            Value ~ Time +
              Cat1 +
              Trend1 + 
              PillScare,
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
                Trend1 + 
                PillScare,
              data = dfc(),
              correlation =  corARMA(p=input$p, q=input$q, form = ~ Time),
              method = "ML"
            )}

      }
    } else if (input$parallel) {
      if (input$p == 0 & input$q == 0){
        if (input$int2) {
          gls(
            Value ~ Time +
              England +
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
                Cat1 +
                Trend1 +
                Cat1_Eng +
                Trend1_Eng + 
                PillScare,
              data = dfc(),
              correlation =  NULL,
              method = "ML"
            )}
      } else {
        if (input$int2) {
          gls(
            Value ~ Time +
              England +
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
                Cat1 +
                Trend1 +
                Cat1_Eng +
                Trend1_Eng,
              data = dfc(),
              correlation =  corARMA(p=input$p, q=input$q, form = ~ Time | England),
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

    lm(
      mod_formula(),
      data = dfc()
    )
  })
  
  
  # Outputs to display -----------------------------------------------------------------------------------------
  
  rSq <- reactive({signif(cor(dfd()$Predict, dfd()$Value),digits = 3)})
  mspe <- reactive({
    dfd() %>% 
      mutate(spe = (Predict - Value)**2) %>% 
      summarise(mspe = mean(spe)) %>% 
      pull() %>% 
      signif(3)
  })
  
  output$rSquared <- renderUI(HTML(paste0("R<sup>2</sup> = ", rSq(), "; MSPE = ", mspe())))
  # output$rSquared <- renderUI(HTML(paste0("R<sup>2</sup>: ", signif(summary(modelGls())$r.squared,digits = 4))))
  
  interceptName <- reactive({
    if (input$control == "none"){
      input$main
    } else {
      input$control
    }
  })
  
  
  # Naming model table -----------------------------------------------------------------------------------------
  
  
  
  modelTable <- reactive({  # labelling coefficients
    labs <-   tibble(lab1 = c("(Intercept)",
                              "Time",
                              "England",
                              "Time_Eng",
                              "Cat1",
                              "Trend1",
                              "Cat1_Eng",
                              "Trend1_Eng",
                              "Cat2",
                              "Trend2",
                              "Cat2_Eng",
                              "Trend2_Eng",
                              "PillScare"),
                     lab2 = c(paste0(interceptName(), " (est) rate at ", input$obRange[1]-1),
                              paste0(interceptName(), " base trend"),
                              paste0(input$main, " difference in rate at ", input$obRange[1]-1),
                              paste0(input$main, " difference in base trend"),
                              paste0(interceptName(), " change in level at intervention 1"),
                              paste0(interceptName(), " change in trend at intervention 1"),
                              paste0(input$main, " difference in level from control at intervention 1"),
                              paste0(input$main, " difference in trend from control at intervention 1"),
                              paste0(interceptName(), " change in level at intervention 2"),
                              paste0(interceptName(), " change in trend at intervention 2"),
                              paste0(input$main, " difference in level from control at intervention 2"),
                              paste0(input$main, " difference in trend from control at intervention 2"),
                              "'Pill Scare' corrector"
                     )
    )
    tb <- printCoefficients(modelGls_null())
    
    betas <- tibble(Beta =  c(
      paste0("\U03B2", "0"),
      paste0("\U03B2", "1"),
      paste0("\U03B2", "2"),
      paste0("\U03B2", "3"),
      paste0("\U03B2", "4"),
      paste0("\U03B2", "5"),
      paste0("\U03B2", "6"),
      paste0("\U03B2", "7"),
      paste0("\U03B2", "8"),
      paste0("\U03B2", "9"),
      paste0("\U03B2", "10"),
      paste0("\U03B2", "11"),
      paste0("\U03B2", "12")
    )
    )
    
    for (i in 1:length(tb$Coefficient)) {
      tb[i, 'Coefficient'] <- labs$lab2[which(labs$lab1==tb[[i, 'Coefficient']])] 
      tb[i, '\U03B2'] <- betas[i, 'Beta']
    }
    
    
    tb[, c(5, 1:4)]
    
  })
  
  # Outputting tables ------------------------------------------------------------------------------------------
  
  output$confint<- renderDataTable(confintervals() %>% select(-Std.Error), 
                                   options = list(searching = FALSE, paging = FALSE, info = FALSE))
  
  confintervals <- reactive({
    modelTable() %>%
      select(Coefficient, Value, Std.Error) %>%
      bind_cols(tibble("Lower CI" = round(confint(modelGls_null())[ ,1], 3),
                       "Upper CI" = round(confint(modelGls_null())[ ,2], 3)))
  })
  
  
  
  output$modelsummary <- renderDataTable(
    modelTable(), options = list(searching = FALSE, paging = FALSE, info = FALSE)
  )
  
  # Create cfac --------------------------------------------------------
  
  modcfac_base <- reactive({
    if(input$control == "none"){
      if(input$int2) {
        tibble(
          Time       = c((startYr()-input$obRange[1]+1):(maxYr() - input$obRange[1]+1)),
          Cat1       = c(rep(0,(input$int2yr-startYr())), rep(1,(maxYr()-input$int2yr+1))),
          Trend1     = c(rep(0,(input$int2yr-startYr())), (input$int2yr-startYr()+1):(maxYr()-startYr()+1)),
          Cat2       = 0,
          Trend2     = 0,
          PillScare  = 1,
          Value = 1
        ) 
      } else {
        tibble(
          Time       = c((startYr()-input$obRange[1]+1):(maxYr() - input$obRange[1]+1)),
          Cat1       = 0,
          Trend1     = 0,
          Cat2       = 0,
          Trend2     = 0,
          PillScare  = 1,
          Value = 1
        )
      }
    } else {
      if(input$int2) {
        tibble(
          Time       = c((startYr()-input$obRange[1]+1):(maxYr() - input$obRange[1]+1)),
          England    = 1,
          Time_Eng   = c((startYr()-input$obRange[1]+1):(maxYr() - input$obRange[1]+1)),
          Cat1       = 1,
          Trend1     = c(1:(maxYr()-startYr()+1)),
          Cat2       = c(rep(0,(input$int2yr-startYr())), rep(1,(maxYr()-input$int2yr+1))),
          Trend2     = c(rep(0,(input$int2yr-startYr())), 1:(maxYr()-input$int2yr+1)), 
          Cat1_Eng   = c(rep(0,(input$int2yr-startYr())), rep(1,(maxYr()-input$int2yr+1))), 
          Trend1_Eng = c(rep(0,(input$int2yr-startYr())), (input$int2yr-startYr()+1):(maxYr()-startYr()+1)),
          Cat2_Eng   = 0,
          Trend2_Eng = 0,
          PillScare  = 1,
          Value = 1
          # Remove _Eng interactions (retaining 1st intervention interactions for 2nd int)
        ) 
      } else {
        tibble(
          Time       = c((startYr()-input$obRange[1]+1):(maxYr() - input$obRange[1]+1)),
          England    = 1,
          Time_Eng   = c((startYr()-input$obRange[1]+1):(maxYr() - input$obRange[1]+1)),
          Cat1       = 1,
          Trend1     = c(1:(maxYr()-startYr()+1)),
          Cat2       = 0,
          Trend2     = 0,
          Cat1_Eng   = 0,
          Trend1_Eng = 0,
          Cat2_Eng   = 0,
          Trend2_Eng = 0,
          PillScare  = 1,
          Value = 1
          # Remove _Eng interactions (retaining 1st intervention interactions for 2nd int)
        )
      }
    }
  })
  
  
  modcfac <- reactive({
    left_join(modcfac_base(), constructCIRibbon(modcfac_base(), 
                                                modelGls_null(),
                                                mod_formula()))
  })
  
  
  output$cfac <- renderDataTable(modcfac())
  
  ylim <- reactive({
    c(0, 1.1*max(modcfac()$HiCI, dfd()$Value))
  })
  
  

# Final dataframe ---------------------------------------------------------


  dfd <- reactive({
    model <- modelGls_null()
    model$call[[2]] <- mod_formula()
    
    
    left_join(dfc(), constructCIRibbon((dfc() %>% filter(England==1, Year >= startYr())), modelGls_null(), mod_formula())) %>%
      arrange(by = Country) %>%
      mutate(Predict = predict(model, newdata = .))# Add Predicts for non-England
  })
  
  output$dfd <- renderDataTable(dfd())
  
  
  # Output equation --------------------------------------------------------------------------------------------
  output$equation <- renderUI({
    eqtext <-   if (!input$int2){
      if (input$control == "none") {
        "Equation: $$Rate = \\beta_0+\\beta_1*Time+\\beta_2*Intervention+\\beta_3*Trend+\\epsilon$$"
      } else if (!input$parallel) {
        "Equation: $$Rate = \\beta_0+\\beta_1*Time+\\beta_2*Group+\\beta_3*Group*Time+\\beta_4*Intervention+$$
        $$\\beta_5*Trend+\\beta_6*Intervention*Group+\\beta_7*Trend*Group+\\epsilon$$"
      } else {
        "Equation: $$Rate = \\beta_0+\\beta_1*Time+\\beta_2*Group+\\beta_3*Intervention+$$
        $$\\beta_4*Trend+\\beta_5*Intervention*Group+\\beta_6*Trend*Group+\\epsilon$$"
      }
    } else {
      if (input$control == "none") {
        "Equation: $$Rate = \\beta_0+\\beta_1*Time+\\beta_2*Intervention_1+\\beta_3*Trend_1+$$
        $$\\beta_4*Intervention_2+\\beta_5*Trend_2+\\epsilon$$"
      } else if (!input$parallel) {
        "Equation: $$Rate = \\beta_0+\\beta_1*Time+
        \\beta_2*Group+
        \\beta_3*Group*Time+
        \\beta_4*Intervention_1+$$
        $$\\beta_5*Trend_1+
        \\beta_6*Group*Intervention_1+
        \\beta_7*Group*Trend_1+
        \\beta_8*Intervention_2+$$
        $$\\beta_9*Trend_2+
        \\beta_{10}*Group*Intervention_2+
        \\beta_{11}*Group*Trend_2+
        \\epsilon$$" 
      } else {
        "Equation: $$Rate = \\beta_0+\\beta_1*Time+
        \\beta_2*Group+
        \\beta_3*Intervention_1+
        \\beta_4*Trend_1+$$
        $$\\beta_5*Group*Intervention_1+
        \\beta_6*Group*Trend_1+
        \\beta_7*Intervention_2+$$
        $$\\beta_8*Trend_2+
        \\beta_9*Group*Intervention_2+
        \\beta_{10}*Group*Trend_2+
        \\epsilon$$" 
      }
    }
    return(withMathJax(eqtext))

    
  })
  
  # Output plot -----------------------------------------------------------------
  
  output$modelplot <- renderPlot({
    req(input$obRange)
    print(PlotInput())
  })
  
  PlotInput <- reactive({
    
    alpha <- ifelse(input$lines, 1, 0)
    
    maxy <- dfa2() %>% 
      filter(Country != input$control) %>% 
      summarise(max = max(Value) + 10) %>% 
      pull()
    
    ScoCol <- "#0072C6"
    WalCol <- "#00AB39"
    
    minlb <- floor(input$obRange[1]/5) * 5
    mxlb <- floor(max(dfc()$Year)/5) * 5
    
    mnlbTm <- unique(dfc()[which(dfc()$Year==minlb+5),]$Time)-5
    mxlbTm <- unique(dfc()[which(dfc()$Year==mxlb),]$Time)
    
    if (input$grey){
      ScoCol <- "grey"
      WalCol <- "grey"
    }
    
    dfd()  %>%
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
      # control data points and trend line
        geom_point(data=dfa2()%>%
                     filter(Country == input$control,
                            Year >= ifelse(input$grey, input$obRange[1], min(dfa2()$Year))),
                   aes(Time, Value, col = Country),
                   show.legend = FALSE,
                   inherit.aes = FALSE) +
      geom_line(data = . %>% filter(Country == input$control), aes(y=Predict), size = 1.5, alpha = alpha) +
      # England data points and trend line
      geom_point(data=dfa2()%>%
                   filter(Country != input$control),
                 aes(Time, Value, col = Country),
                 show.legend = FALSE,
                 inherit.aes = FALSE) +
      geom_line(data = . %>% filter(Country != input$control), aes(y=Predict), size = 1.5, alpha = alpha) +
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
        size = 1.5,
        alpha = alpha,
        inherit.aes = FALSE
      ) +
      # Intervention time points
      geom_vline(xintercept = input$int1yr-input$obRange[1]+0.5,
                 linetype = "dotted",
                 col = "#000000CC") +
      geom_vline(xintercept = input$int2yr-input$obRange[1]+0.5,
                 linetype = "dotted",
                 col = ifelse(input$int2, "#000000CC", NA)) +
      geom_rect(
        xmin = input$int1yr-input$obRange[1]+0.5,
        xmax = input$pi1yr-input$obRange[1]+1.5,
        ymin = 0,
        ymax = ylim()[2] + 10,
        fill = ifelse(input$pi1, "grey", NA),
        alpha = 0.01,
        inherit.aes = FALSE
      ) +
      # Display parameters
      theme(panel.background = element_blank(),
            legend.key  = element_blank(),
            panel.grid = element_blank()) +
      ylab(paste0("Rate of pregnancies to ", input$ages, "s, per 1,000")) +
      xlab("Year") +
      # coord_cartesian(ylim = c(0, maxy)) +
      # coord_cartesian(ylim = c(0, 1.1*max(dfd()$Value, modcfac()$Predict))) +
      coord_cartesian(ylim = c(0, 56)) +
      # coord_cartesian(ylim = ylim()) +
      scale_y_continuous(expand = c(0, 0)) +
      # scale_x_continuous(limits = c(NA, NA), breaks = seq(mnlbTm, mxlbTm, by=5), labels = seq(minlb, mxlb, by=5)) +
      scale_x_continuous(limits = c(mnlbTm, NA), breaks = seq(mnlbTm, mxlbTm, by=5), labels = seq(minlb, mxlb, by=5)) +
      scale_colour_manual(
        breaks = c("England", "Wales", "Scotland", "England and Wales", "Prediction"),
        values = c("England" = "#CF142B",
                   "Wales" = WalCol,
                   "Scotland" = ScoCol,
                   "England and Wales" = "#A50115",
                   "Prediction" = "#FFC000"),
        aesthetics = c("colour", "fill"))
  })
  
  
  # autocorr tests ---------------------------------------------------------------------------------------------
  
  
  output$dwt <- renderDataTable(
    (
      data.frame(lag = 1:12, dwt(modelGls(), max.lag = 12, alternative = "two.sided")[1:3]) %>% rename(Autocorrelation =r, DW_Stat = dw, pvalue=p)
    ),
    options = list(searching = FALSE, paging = FALSE, info = FALSE)
  )
  
  output$autocorr <- renderPlot({
    
    model <- modelGls_null()
    model$call[[2]] <- mod_formula()
    
    rows <- input$obRange[2]-input$obRange[1]+1
    par(cex = 0.7, mai = c(0.1, 0.1, 0.2, 0.1))
    par(fig = c(0.03, 1, 0.8, 1))
    plot(
      unique(dfc()$Year)[1:rows],
      residuals(modelGls_null())[1:rows],
      type = 'o',
      pch = 16,
      col = "red"
    )
    
    par(fig = c(0.03, 0.5, 0.05, 0.75), new = TRUE)
    acf(residuals(model, type = "normalized"))
    
    par(fig = c(0.55, 1, 0.05, 0.75), new = TRUE)
    acf(residuals(model, type = "normalized"), type = 'partial')
  })
  
  modelText <- reactive({paste0("AR", input$p, ", MA", input$q)})
  
  corr <- reactive({
    paste0("Autocorrelation correction: ", ifelse(input$p == 0 & input$q == 0, "none", modelText()))
  })  
  
  output$corr <- renderText(corr())
  
  pplus1_text <- renderText({paste0("AR", input$p+1, ", MA", input$q)})
  
  output$pplus1_title <- renderText({paste0("ANOVA comparison with model ", pplus1_text(), ":")})
  
  output$pplus1 <- renderDataTable(options = list(searching = FALSE, paging = FALSE, info = FALSE), {
    
    model <- modelGls_null()
    model$call[[2]] <- mod_formula()
    
    model %>% 
      update(correlation = corARMA(p = input$p + 1, q = input$q,
                                   form = ~ Time | England)) %>% 
      anova(model, .)%>% 
      mutate(call = c(modelText(), pplus1_text())) })
  
  qplus1_text <- renderText({paste0("AR", input$p, ", MA", input$q+1)})
  
  output$qplus1_title <- renderText({paste0("ANOVA comparison with model ", qplus1_text(), ":")})
  
  output$qplus1 <- renderDataTable(options = list(searching = FALSE, paging = FALSE, info = FALSE), {
    model <- modelGls_null()
    model$call[[2]] <- mod_formula()
    
    model %>% 
      update(correlation = corARMA(p = input$p, q = input$q + 1,
                                   form = ~ Time | England)) %>% 
      anova(model,.)%>% 
      mutate(call = c(modelText(), qplus1_text()))
  })
  
  

  
  
  # knit report ------------------------------------------------------------------------------------------------
  output$downloadReport <- downloadHandler(
    filename = function() {paste0(input$main, " vs ", input$control, " ", input$obRange[1], "-", input$obRange[2], ".docx")},
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      tempTemplate <- file.path(tempdir(), "report_template.docx")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      file.copy("report_template.docx", tempTemplate, overwrite = TRUE)
      params <- list(
        set_title = paste(input$main,
                          ifelse(input$control=="none", "", paste("compared with", input$control, sep = " ")),
                          input$obRange[1], "-",
                          input$obRange[2],
                          sep=" ")
      )
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        output_format = "word_document",
                        output_options = list(reference_docx = tempTemplate)
                        # output_options = list(reference_docx = paste0(getwd(), "/report_template.docx"))
                        # output_format = rmarkdown::word_document(reference_docx = "report_template.docx")
                        # envir = new.env(parent = globalenv()))
      )
    }
  )
  
  
}

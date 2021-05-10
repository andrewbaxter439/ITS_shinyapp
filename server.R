require(tidyverse)
require(broom)
require(nlme)
require(car)
require(svglite)
require(officer)
require(rvg)
require(DT)
require(openxlsx)

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
  
  df <- df %>% mutate(
    #se = sds,
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
  
  setBookmarkExclude(c("modelsummary_columns_selected", 
                       "modelsummary_rows_current",
                       "modelsummary_cell_clicked",
                       "modelsummary_rows_all",
                       "modelsummary_rows_selected",
                       "modelsummary_state",
                       "modelsummary_cells_selected",
                       "modelsummary_search",
                       "height",
                       "width",
                       "format",
                       "go2graph"))
  
  # Reactive inputs -------------------------------------------------------------------------------------------
  
  observeEvent(input$go2graph, {
    updateTabsetPanel(session, "session", selected = "Full Plot")
  })
  
  output$dlgraph <- downloadHandler(
    filename = function() {paste0(input$main, " vs ", input$control, " ", input$obRange[1], "-", input$obRange[2], ".", input$format)},
    content = function(file) {
      graph <- PlotInput() +
        # ggtitle(paste(input$main,
        #               ifelse(input$control=="none", "", paste("compared with", input$control, sep = " ")),
        #               input$obRange[1], "-",
        #               input$obRange[2],
        #               sep=" ")) +
        theme(title = element_text(size = 12), 
              axis.title = element_text(size = 10),
              legend.text = element_text(size = 10))
      ggsave(file, graph,
             dpi = 400, units = "mm", width = input$width, height = input$height)
    },
    contentType = paste0("image/", input$format)
  )
  
  # ppt graph output --------------------------------------------------------
  
  
  output$dlppt <- downloadHandler(
    filename = function() {paste0(input$main, " vs ", input$control, " ", input$obRange[1], "-", input$obRange[2], ".pptx")},
    content = function(file){
      plot <- PlotInput() + theme(text = element_text(size = 16), line = element_blank())
      doc <- read_pptx()
      doc <- add_slide(doc)
      doc <- ph_with(doc, dml(ggobj = plot), location = ph_location(height = input$height/25.4,
                                                                    width = input$width/25.4,
                                                                    top = (190.5-input$height)/(2*25.4),
                                                                    left = (254-input$width)/(2*25.4)))
      print(doc, target = file)
    }
  )
  
  #   output$ggplot <- downloadHandler(
  #   filename = function() {paste0(input$main, " vs ", input$control, " ", input$obRange[1], "-", input$obRange[2], ".rdata")},
  #   content = function(file){
  #     graph <- renderPlot(PlotInput())
  #     save(graph, file = file)}
  # )
  
  
  
  output$dlconfints <- downloadHandler(
    filename = function() {paste0(input$main, " vs ", input$control, " ", input$obRange[1], "-", input$obRange[2], ".xlsx")},
    content = function(file) {

      fname <- paste(file,"xlsx",sep=".")
      wb <- createWorkbook("ITS_shinyapp", "ITS results")
      addWorksheet(wb, sheetName = "model")
      addWorksheet(wb, sheetName = "data")
      addWorksheet(wb, sheetName = "counterfactual")
      writeData(wb, confintervals(), sheet = "model")
      writeData(wb, dfd(), sheet = "data")
      writeData(wb, modcfac(), sheet = "counterfactual")
      saveWorkbook(wb, file = file)
      
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
  
  # output$dateslider <- renderUI({
  #   sliderInput(
  #     inputId = "obRange",
  #     label = "Select date range to observe",
  #     min = minYr(),
  #     max = maxYr(),
  #     value = c(max(minYr(), 1992), maxYr()),
  #     step = 1,
  #     sep=""
  #   )
  # })
  
  observeEvent(minYr(), {
    updateSliderInput(
      inputId = "obRange",
      min = minYr(),
      max = maxYr(),
    )
    
    updateSliderInput(
      inputId = "int2yr",
      min = startYr()+2,
      max = maxYr()-2
    )
    
    updateSliderInput(
      inputId = "int1yr",
                      min = minYr()+2,
                      max = maxYr()-2
      )
    
    updateSliderInput(
      inputId = "pi1yr",
      min = input$int1yr,
      max = maxYr()-1,
      value = input$int1yr+1
    )
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
  
  load("data/all_uk_rates_u.rdata")
  # output$fulldata <- DT::renderDataTable(modcfac())
  output$fulldata <- DT::renderDataTable(all_UK_rates[[input$ages]], options = list(searching = FALSE, paging = FALSE, info = FALSE, ordering = FALSE))
  
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
  
  output$dataframesumm <- DT::renderDataTable(
    (arrange(dfc(), by=Year)),
    options = list(searching = FALSE, paging = FALSE, info = FALSE)
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
        ifelse(input$int2, " + Trend2", ""),
        # ifelse(input$int2,
        #        paste0(" + Trend2",
        #               ifelse(input$control == "none", "", " + Trend2_Eng")
        #        ),
        #        ""
        # ),
        ifelse(input$pillscare, " + PillScare", "")
      )
    )
  })
  # 
  # output$form1 <- renderText({as.character(predict(modelGls_null()))})
  output$form2 <- renderText({deparse(mod_formula())})
  
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
    
    print(summary(model))
    model
    
  })
  modelGls_null_unused <- reactive({  # add if statements for each model
    
    
    if (input$control == "none") {       # outer if - with/without control (x1)
      if (input$p == 0 & input$q == 0){  # second if - with or without ARMA correction (x2)
        if (input$int2) {                # innermost if - with or without second intervention (x2x2)
          mod <- gls(
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
          mod <- gls(
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
          mod <- gls(
            Value ~ Time +
              Cat1 +
              Trend1 +
              Cat2 +
              Trend2,
            data = dfc(),
            correlation = corARMA(p=input$p, q=input$q, form = ~ Time),
            method = "ML"
          )} else {
            mod <- gls(
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
          mod <- gls(
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
            mod <- gls(
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
          mod <- gls(
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
            mod <- gls(
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
          mod <- gls(
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
            mod <- gls(
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
          mod <- gls(
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
            mod <- gls(
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
    
    print(summary(mod))
    mod
  })
  
  # Simple linear model ----------------------------------------------------------------------------------------
  
  modelGls <- reactive({  # model to check for autocorrelation
    
    lm(
      mod_formula(),
      data = dfc()
    )
  })
  
  
  # Outputs to display -----------------------------------------------------------------------------------------
  
  rSq <- reactive({
    df <- dfd() %>% 
      filter(Time %in% 1:100)
    
    signif(cor(df$Predict, df$Value),digits = 3)
  })
  
  logLik <- reactive({
    modelGls_null()$logLik
  })
  
  mspe <- reactive({
    dfd() %>% 
      filter(Time %in% 1:100) %>%
      mutate(spe = (Predict - Value)**2) %>% 
      summarise(mspe = mean(spe)) %>% 
      pull() %>% 
      signif(3)
  })
  
  output$rSquared <- renderUI({
    req(rSq())
    HTML(paste0("R<sup>2</sup> = ", rSq(), "; MSPE = ", mspe()))
  })
  # output$rSquared <- renderUI(HTML(paste0("R<sup>2</sup>: ", signif(summary(modelGls())$r.squared,digits = 4))))
  
  interceptName <- reactive({
    if (input$control == "none"){
      input$main
    } else {
      input$control
    }
  })
  
  
  # Naming model table -----------------------------------------------------------------------------------------
  
  
  
  modelTable <- reactive({# labelling coefficients
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
                              paste0(interceptName(), " pre-intervention trend"),
                              paste0(input$main, " difference in rate at ", input$obRange[1]-1),
                              paste0(input$main, " difference in base trend"),
                              paste0(interceptName(), " change in level at ", input$int1yr),
                              paste0(interceptName(), " change in trend at ", input$int1yr),
                              paste0(input$main, " difference in level from control at ", input$int1yr),
                              paste0(input$main, " difference in trend from control at ", input$int1yr),
                              paste0(interceptName(), " change in level at intervention 2"),
                              paste0("Common change in trend at ", input$int2yr, " shock"),
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
  
  output$confint<- DT::renderDataTable(confintervals() %>% select(-`Standard Error`), 
                                       options = list(searching = FALSE, paging = FALSE, info = FALSE, ordering = FALSE))
  
  confintervals <- reactive({
    modelTable() %>%
      select(Coefficient, Estimate = Value, `Standard Error` = Std.Error) %>%
      bind_cols(tibble("Lower 95% CI" = round(confint(modelGls_null())[ ,1], 3),
                       "Upper 95% CI" = round(confint(modelGls_null())[ ,2], 3)))
  })
  
  
  
  output$modelsummary <- DT::renderDataTable({
    req(input$int1yr)
    modelTable()}, options = list(searching = FALSE, paging = FALSE, info = FALSE, ordering = FALSE)
  )
  
  # Create cfac --------------------------------------------------------
  
  modcfac_base <- reactive({
    if(input$control == "none"){
      if(input$int2) {
        tibble(
          Time       = c((startYr()-input$obRange[1]+0.5), (startYr()-input$obRange[1]+1): (maxYr() - input$obRange[1]+1)),
          Cat1       = 0,
          Trend1     = 0,
          Cat2       = 0,
          Trend2     = c(rep(0,(input$int2yr-startYr() +1)), 1:(maxYr() - input$int2yr + 1)),
          PillScare  = 1,
          Value = 1
        ) 
      } else {
        tibble(
          Time       = seq((startYr()-input$obRange[1]+0.5), (maxYr() - input$obRange[1]+1), 0.5),
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
          Time       = c((startYr()-input$obRange[1]+0.5), (startYr()-input$obRange[1]+1): (maxYr() - input$obRange[1]+1)),
          England    = 1,
          Time_Eng   = c((startYr()-input$obRange[1]+0.5), (startYr()-input$obRange[1]+1): (maxYr() - input$obRange[1]+1)),
          Cat1       = 1,
          Trend1     = c(0.5, 1:(maxYr()-startYr()+1)),
          Cat2       = c(rep(0,(input$int2yr-startYr()+1)), rep(1,(maxYr()-input$int2yr+1))),
          Trend2     = c(rep(0,(input$int2yr-startYr()+1)), 1:(maxYr()-input$int2yr+1)), 
          Cat1_Eng   = 0,
          Trend1_Eng = 1,
          Cat2_Eng   = 0,
          Trend2_Eng = 0,
          PillScare  = 1,
          Value = 1
          # Remove _Eng interactions (retaining 1st intervention interactions for 2nd int)
        ) 
      } else {
        tibble(
          Time       = seq((startYr()-input$obRange[1]+0.5), (maxYr() - input$obRange[1]+1), 0.5),
          England    = 1,
          Time_Eng   = seq((startYr()-input$obRange[1]+0.5), (maxYr() - input$obRange[1]+1), 0.5),
          Cat1       = 1,
          Trend1     = seq(0.5, (maxYr()-startYr()+1), 0.5),
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
  
  
  output$cfac <- DT::renderDataTable(modcfac())
  
  ylim <- reactive({
    c(0, 1.1*max(modcfac()$HiCI, dfd()$Value))
  })
  
  
  
  # Final dataframe ---------------------------------------------------------
  
  
  dfd <- reactive({
    model <- modelGls_null()
    model$call[[2]] <- mod_formula()
    
    new_df <- dfc() %>% 
      bind_rows(
        tibble(
          Year = 1999,
          Value = 1,
          Time = input$int1yr-input$obRange[1]+0.5,
          Country = c(input$control, input$control, input$main, input$main),
          England = c(0,0,1,1),
          Time_Eng   = c(0, 0, input$int1yr-input$obRange[1]+0.5, input$int1yr-input$obRange[1]+0.5),
          Cat1       = c(1, 0, 1, 0),
          Trend1     = c(0.5, 0, 0.5, 0),
          Cat2       = 0,
          Trend2     = 0,
          Cat1_Eng   = c(0, 0, 1, 0),
          Trend1_Eng = c(0, 0, 0.5, 0),
          Cat2_Eng   = 0,
          Trend2_Eng = 0,
          PillScare  = 1)
      )
    
    new_df %>% 
      left_join(constructCIRibbon((new_df %>%
                                     filter(England==1,
                                            Time > input$int1yr-input$obRange[1]+0.5,
                                            # Year > (startYr()-1)
                                     )
      ), modelGls_null(), mod_formula())) %>%
      arrange(by = Country, Time, Cat1) %>%
      mutate(Predict = predict(model, newdata = .))# Add Predicts for non-England
  })
  
  output$dfd <- DT::renderDataTable(dfd(), options = list(ordering = FALSE))
  
  
  # Output equation --------------------------------------------------------------------------------------------
  eqtext <-   reactive({
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
        "Equation: $$Rate = \\beta_0+\\beta_1*Time+\\beta_2*Intervention+$$
        $$\\beta_3*Trend_1+\\beta_4*Trend_2+\\epsilon$$"
      } else if (!input$parallel) {
        "Equation: $$Rate = \\beta_0+\\beta_1*Time+
        \\beta_2*Group+
        \\beta_3*Group*Time+
        \\beta_4*Intervention+$$
        $$\\beta_5*Trend_1+
        \\beta_6*Group*Intervention+
        \\beta_7*Group*Trend_1+$$
        $$\\beta_8*Trend_2+
        \\epsilon$$" 
      } else {
        "Equation: $$Rate = \\beta_0+\\beta_1*Time+
        \\beta_2*Group+
        \\beta_3*Intervention+
        \\beta_4*Trend_1+$$
        $$\\beta_5*Group*Intervention+
        \\beta_6*Group*Trend_1+$$
        $$\\beta_7*Trend_2+
        \\epsilon$$" 
      }
    }
    return(withMathJax(eqtext))
  })
  
  output$equation <- renderUI({eqtext()})
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
      ScoCol <- "#666666"
      WalCol <- "#666666"
    }
    
    dfd()  %>%
      ggplot(aes(
        Time,
        Value,
        col = Country,
        fill = Country,
        group = Cat1,
        # group = interaction(Country, Cat1, Cat2)
      )) +
      # Counterfactual confidence intervals (not shown in legend)
      geom_ribbon(
        data=modcfac(),
        aes(
          x=Time,
          ymin = lowCI,
          ymax=HiCI,
          group = interaction(Cat1, Cat2),
          col=NULL
        ),
        fill= ifelse(input$ribbons,"#dddddd88","#ffffff00"),
        # alpha=0.5,
        size = 1,
        show.legend = FALSE,
        inherit.aes = FALSE) +
      # Model confidence intervals (not shown in legend)
      geom_ribbon(
        aes(
          x=Time,
          ymin = lowCI,
          y = Predict,
          ymax = HiCI,
          col=NULL
        ),
        fill= NA,  # no confints for England trend
        # fill= ifelse(input$ribbons,"#dddddd88","#ffffff00"),
        # alpha= 0.5,
        size = 1,
        show.legend = FALSE) +
      # control data points and trend line
      geom_point(data=dfa2()%>%
                   filter(Country == input$control,
                          # Year >= ifelse(input$grey, input$obRange[1], min(dfa2()$Year))
                   ),
                 aes(Time, Value, col = Country),
                 shape = 3,
                 show.legend = FALSE,
                 inherit.aes = FALSE) +
      geom_line(data = . %>% filter(Country == input$control), aes(y=Predict), size = 1.5, alpha = alpha) +
      # England data points
      geom_point(data=dfa2()%>%
                   filter(Country != input$control),
                 aes(Time, Value, col = Country),
                 shape = 3,
                 show.legend = FALSE,
                 inherit.aes = FALSE) +
      # Counterfactual trend lines
      geom_line(
        data = modcfac(),
        aes(
          x = Time,
          y = Predict,
          # group = interaction(Cat1, Cat2),
          # col = input$main,
          size = 1,
          # col = "No Strategy",
          linetype = "No Strategy",
          fill = NULL
        ),
        # col = "#CF142B",
        col = case_when(
          input$main == "England" ~ "#CF142B",
          input$main == "Wales" ~ WalCol,
          input$main == "Scotland" ~ ScoCol,
          input$main == "England and Wales" ~ "#A50115"
        ),
        # linetype = "longdash",
        size = 1,
        alpha = alpha,
        inherit.aes = FALSE
      ) +
      # England trend line
      geom_line(data = . %>% filter(Country != input$control), aes(y=Predict), size = 1.5, alpha = alpha) +
      # Intervention time points
      geom_vline(data = tibble(),
                 xintercept = input$int1yr-input$obRange[1]+0.5,
                 linetype = "dotted",
                 col = "#000000CC") +
      # geom_vline(xintercept = input$int2yr-input$obRange[1]+0.5,
      #            linetype = "dotted",
      #            col = ifelse(input$int2, "#000000CC", NA)) +
      geom_text(
        data = tibble(),
        aes(
          x =  input$int1yr - input$obRange[1] + 1,
          y = Inf,
          label = "1999 Strategy launch"
        ),
        col = "#666666",
        hjust = 0,
        vjust = 1.5,
        size = 5,
        inherit.aes = FALSE
      ) +
      geom_rect(
        data = tibble(),
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
            legend.position = "bottom",
            panel.grid.major = element_line(colour = "#e0e0e0"),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "#666666"),
            legend.margin = margin(unit = "cm"),
            legend.text = element_text(size = 12),
            legend.key.width = unit(1, "cm")) +
      ylab(paste0("Rate of pregnancies to ", input$ages, "s, per 1,000")) +
      xlab("Year") +
      # coord_cartesian(ylim = c(0, maxy)) +
      coord_cartesian(ylim = c(0, 1.1*max(dfd()$Value, modcfac()$Predict))) +
      # coord_cartesian(ylim = c(0, 56)) +
      # coord_cartesian(ylim = ylim()) +
      scale_y_continuous(expand = c(0, 0)) +
      # scale_x_continuous(limits = c(NA, NA), breaks = seq(mnlbTm, mxlbTm, by=5), labels = seq(minlb, mxlb, by=5)) +
      scale_x_continuous(limits = c(mnlbTm, NA), breaks = seq(mnlbTm, mxlbTm, by=5), labels = seq(minlb, mxlb, by=5)) +
      scale_colour_manual(
        name = "",
        breaks = c("England", "England and Wales", "Wales", "Scotland", "No Strategy"),
        values = c("England" = "#CF142B",
                   "England and Wales" = "#A50115",
                   "Wales" = WalCol,
                   "Scotland" = ScoCol,
                   "No Strategy" = "#FFC000"),
        labels = c("England", "England and Wales", "Wales", "Scotland", "No Strategy"),
        aesthetics = c("colour", "fill")) +
      scale_linetype_manual(name = "", 
                            values = c("No Strategy" = "dashed", "England" = "solid"), 
                            labels = c("No Strategy", input$main)
      ) +
      guides(colour = guide_legend(order = 1))
  })
  
  
  # autocorr tests ---------------------------------------------------------------------------------------------
  
  
  output$dwt <- DT::renderDataTable(
    (
      data.frame(lag = 1:12, dwt(modelGls(), max.lag = 12, alternative = "two.sided")[1:3]) %>% 
        rename(Autocorrelation =r, DW_Stat = dw, pvalue=p) %>% 
        mutate_if(is.double, round, 3)
      
    ),
    options = list(searching = FALSE, paging = FALSE, info = FALSE, ordering = FALSE)
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
  
  output$pplus1 <- DT::renderDataTable(options = list(searching = FALSE, paging = FALSE, info = FALSE), {
    
    model <- modelGls_null()
    model$call[[2]] <- mod_formula()
    
    model %>% 
      update(correlation = corARMA(p = input$p + 1, q = input$q,
                                   form = ~ Time | England)) %>% 
      anova(model, .) %>% 
      mutate_if(is.double, signif, digits = 4) %>% 
      mutate(call = c(modelText(), pplus1_text())) })
  
  qplus1_text <- renderText({paste0("AR", input$p, ", MA", input$q+1)})
  
  output$qplus1_title <- renderText({paste0("ANOVA comparison with model ", qplus1_text(), ":")})
  
  output$qplus1 <- DT::renderDataTable(options = list(searching = FALSE, paging = FALSE, info = FALSE), {
    model <- modelGls_null()
    model$call[[2]] <- mod_formula()
    
    model %>% 
      update(correlation = corARMA(p = input$p, q = input$q + 1,
                                   form = ~ Time | England)) %>% 
      anova(model,.)%>% 
      mutate_if(is.double, signif, digits = 4) %>% 
      mutate(call = c(modelText(), qplus1_text()))
  })
  
  
  
  
  
  # knit report ------------------------------------------------------------------------------------------------
  output$downloadReport <- downloadHandler(
    filename = function() {paste0(input$main, " vs ", input$control, " ", input$obRange[1], "-", input$obRange[2], ".docx")},
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.rmd")
      tempTemplate <- file.path(tempdir(), "report_template.docx")
      file.copy("report.rmd", tempReport, overwrite = TRUE)
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

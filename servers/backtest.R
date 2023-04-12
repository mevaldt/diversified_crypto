# ---- botoes de assets/weights ----

# ---- * first buttons ----
output$symbol_input_list <-
  renderUI({
    column(
      width = 6,
      div(
        id = 'inputs_list',
        generate_weight_input(unique(db_backtest$symbol), db_backtest),  
        generate_weight_input(unique(db_backtest$symbol), db_backtest),  
        generate_weight_input(unique(db_backtest$symbol), db_backtest),  
      ),
      actionBttn(inputId = "add_row",
                 label = "Add +",  
                 style = "minimal",
                 color = "primary")
    )
    
  })

# ---- * remove button ----
rvs = reactiveValues(observers = list()) 

observe(
  {
    input_names <-
      names(input)
    
    buttons_available <-
      input_names[stringr::str_detect(input_names, "remove_")]
    
    if(length(buttons_available) == 0) {
      
      return(NULL)
      
    } 
    
    rvs$observers = lapply(
      buttons_available, 
      function(i) {
        observeEvent(
          input[[i]],
          {
            unique_id <- stringr::str_remove(i, "remove_")
            removeUI(
              selector = paste0("div#", unique_id),
              immediate = TRUE
            )
            remove_shiny_inputs(paste0("ticker_", unique_id), input)
            remove_shiny_inputs(paste0("weight_", unique_id), input)
            remove_shiny_inputs(paste0("remove_", unique_id), input)
          }
        )
      }
    )
  })

# ---- * add button ----

observeEvent(
  input$add_row,
  {
    insertUI(
      selector = "div#inputs_list",
      where = "beforeEnd",
      ui = generate_weight_input(unique(db_backtest$symbol), db_backtest)
    )
  }
)


#---- benchmark button ----
output$benchmark_ui <- renderUI({
  pickerInput(
    inputId  = 'benchmark',
    label    = "Benchmark",
    choices  = c("Porfolio W/O Crypto", unique(db_backtest$symbol))
  )
})
#---- risk free button ----

output$risk_free_ui <- renderUI({
  pickerInput(
    inputId  = 'risk_free_choose',
    label    = "Risk Free",
    choices  = c("CDI", "Numeric")
  )
})

#---- Currency ----

# observeEvent(input$currency, {
#   
#   currency_choice = case_when(
#     input$currency == "Local" ~ 'local',
#     input$currency == "Local" ~ 'dolar'
#   )
#   
#   sub_db_symbol_country <- reactive({
#     db_symbol_country %>% 
#       filter(country == input$home_map_shape_click$id, moeda %in% c('curr', currency_choice))
#   })
#   
#   db_prices_updated <- reactive({
#     db_prices %>% 
#       filter(symbol %in% db_backtest$symbol)
#   })
#   
#   update_currency(input, output, session, symbols = unique(db_backtest$symbol), sub_db_symbol_country = db_backtest)
# })

#---- performance ----
observeEvent(input$gobacktest, {
  #---- * showing panels ----
  showTab(inputId = 'tab_backtest', target = 'Performance', session = session)
  showTab(inputId = 'tab_backtest', target = 'Risk', session = session)
  
  #---- * inputs ----
  input_names <- names(input)
  
  buttons_available <- input_names[stringr::str_detect(input_names, "remove_")]
  
  # check ativos zerado
  if(length(buttons_available) == 0) {
    
    sendSweetAlert(
      session = session,
      title = "oops...",
      text = 'No Assets Selected',
      type = "warning"
    )
    
    return()
    
  }
  
  # check ativos zerado
  if(any(map_chr(buttons_available, ~input[[.x]]) == '')) {
    
    sendSweetAlert(
      session = session,
      title = "oops...",
      text = 'Empty Assets',
      type = "warning"
    )
    
    return()
    
  }  
  
  # check soma pesos
  input_ids <- stringr::str_remove(buttons_available, "remove_")
  
  ativos <- map_chr(input_ids, ~input[[paste0("ticker_", .x)]])
  
  weights <- map_dbl(input_ids, ~input[[paste0("weight_", .x)]])
  
  
  if(sum(weights) != 1){
    
    sendSweetAlert(
      session = session,
      title = "oops...",
      text = 'Sum weights needs to add 1',
      type = "warning"
    )
    
    return()
  }
  
  # others 
  
  dt_ini <- input$daterange[1]
  dt_fim <- input$daterange[2]
  
  rebalance_choice <- ifelse(input$rebalance == 'none', NA_character_, input$rebalance)
  
  period_return_choice <- input$period_return
  
  assets_crypto <- db_backtest %>% filter(type %in% c('Crypto Currency', 'Crypto Index')) %>% pull(symbol) %>% unique()
  
  if(input$benchmark == 'Porfolio W/O Crypto'){
    benchmark_asset <- ativos[!(ativos %in% assets_crypto)]
    benchmark_old_weights <- weights[!(ativos %in% assets_crypto)]
    benchmark_weights <- benchmark_old_weights/sum(benchmark_old_weights)
  }else{
    benchmark_asset <- input$benchmark
    benchmark_weights <- 1
  } 
  
  #---- * returns ----
  db_backtest_filt <- db_backtest %>% filter(date >= dt_ini & date <= dt_fim)
  
  benchmark_returns <- db_backtest_filt %>% 
    get_returns(assets = benchmark_asset, period = period_return_choice)
  
  benchmark_performance <- get_performance(benchmark_returns, benchmark_asset, benchmark_weights, rebalance = rebalance_choice)
  
  
  portfolio_returns <- db_backtest_filt %>% 
    get_returns(assets = ativos, period = period_return_choice)
  
  portfolio_performance <- get_performance(portfolio_returns, ativos, weights = weights, rebalance = rebalance_choice)
  
  db_performance <- portfolio_performance %>% 
    left_join(benchmark_performance,
              by = 'date', suffix = c('_portfolio', '_benchmark')) 
  
  #---- * metrics ----
  portfolio_metrics <- get_individual_metrics(portfolio_performance) %>% 
    mutate(
      portfolio = 'Portfolio'
    )
  
  benchmark_metrics <- get_individual_metrics(benchmark_performance) %>% 
    mutate(
      portfolio = 'Benchmark'
    )
  
  db_metrics <- bind_rows(portfolio_metrics, benchmark_metrics)
  
  #---- * plot performance ----
  output$performance_plot <- renderPlotly({
    db_performance %>% 
      select(date, portfolio = daily_cum_portfolio, benchmark = daily_cum_benchmark) %>% #na.omit() %>%
      pivot_longer(cols = c('portfolio', 'benchmark'), names_to = 'return', values_to = 'value') %>% 
      plot_returns()
  })
  
  #---- * table ----
  output$individual_metrics_table <- renderDataTable({
    db_metrics %>% 
      filter(metric %in% c("Mean",
                           "SD",
                           "Median",
                           "Lower_tail",
                           "Upper_tail",
                           "AnnualizedReturn",
                           "AnnualizedSharpe(Rf=0%)",
                           "AnnualizedStdDev",
                           "MaximumDrawdown")) %>%
      pivot_wider(names_from = portfolio, values_from = value) %>% 
      mutate_if(is.numeric, ~round(., digits = 4)) %>% 
      datatable(
        rownames = F,
        colnames = c('Measure' = 1),
        options = list(dom = 'pr')
      )
  })
  
  #---- * plot drawdown ----
  
  output$drawdown_plot <- renderPlotly({
    db_performance %>% 
      select(date, portfolio = daily_cum_portfolio, benchmark = daily_cum_benchmark) %>% 
      pivot_longer(cols = c('portfolio', 'benchmark'), names_to = 'return', values_to = 'value') %>% 
      plot_drawdown()
  })
  
  #---- * plot rolling std ----
  
  output$roll_sd_plot <- renderPlotly({
    db_performance %>% 
      select(date, portfolio = daily_cum_portfolio, benchmark = daily_cum_benchmark) %>% 
      pivot_longer(cols = c('portfolio', 'benchmark'), names_to = 'return', values_to = 'value') %>% 
      plot_rolling_sd_returns(window = input$window_sd)
  })
  
  
})

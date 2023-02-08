
# ---- Observe Events to update portfolio tabs ----

is_portfolio <-
  reactive({
    
    if(input$intabset == "portfolio") {
      
      return(TRUE)
      
    } else {
      
      return(NULL)
    }
    
  })

observeEvent(is_portfolio(), {
  
  updatePickerInput(
    session,
    'asset_names_portfolio',
    selected = full_data %>% 
      pull(name) %>% 
      unique()
  )  
  
},
once = TRUE)

observeEvent(input$asset_names_portfolio, {
  
  if(input$portfolio_dolarized_returns) {
    
    var <- "ret.adjusted.prices.usd"
    
  } else {
    
    var <- "ret.adjusted.prices"
    
  }
  
  asset_names_portfolio <- input$asset_names_portfolio
  
  aux_pre <- 
    full_data %>%
    filter(
      name %in% asset_names_portfolio
    ) %>%
    select(name, date, {{var}}) %>%
    tidyr::pivot_wider(names_from = name,
                       values_from = {{var}}) %>% 
    na.omit() %>% 
    pull(date) %>% 
    as.character()
  
  # message(min(aux_pre, na.rm = TRUE))
  # message(max(aux_pre, na.rm = TRUE))
  
  updateDateRangeInput(
    session,
    "date_range_portfolio",
    start = min(aux_pre, na.rm = TRUE),
    end   = max(aux_pre, na.rm = TRUE)
  )
  
})

observeEvent(input$date_range_portfolio, {
  
  updateNumericInput(
    session,
    "RiskFreeRate",
    value = risk_free_rate(input$date_range_portfolio[2])
  )
  
})

rfr <-
  eventReactive(input$RiskFreeRate, {
    
    tnx_value <- input$RiskFreeRate/100
    
    rfr <-
      ((1 + tnx_value)^(1/252)) - 1
    
    return(rfr)
    
  })

#### Tailored Frontier Plot ----

portfolio_plot_reactive <- eventReactive(input$PortfolioActionButton, {
  
  asset_names_portfolio <- input$asset_names_portfolio
  date_range_portfolio <- input$date_range_portfolio
  RiskFreeRate <- rfr()
  
  if(input$portfolio_dolarized_returns) {
    
    var <- "ret.adjusted.prices.usd"
    
  } else {
    
    var <- "ret.adjusted.prices"
    
  }
  
  aux_pre <- full_data %>%
    filter(
      name %in% asset_names_portfolio,
      date >= date_range_portfolio[1],
      date <= date_range_portfolio[2]
    ) %>%
    select(date, name, ret.adjusted.prices = {{var}}) %>%
    spread(name, ret.adjusted.prices) %>%
    na.omit()
  
  min_pairwise_date <- min(aux_pre$date)
  max_pairwise_date <- max(aux_pre$date)
  
  aux_pre <- aux_pre - RiskFreeRate
  
  datas2 <- aux_pre %>% pull(date)
  
  portfolio_input_data <- aux_pre %>% 
    select(-date) %>% 
    xts(order.by = datas2)
  
  # Create a portfolio
  base_port_spec <- portfolio.spec(assets = colnames(portfolio_input_data))
  
  # Create two objective functions:
  base_port_spec <- add.objective(portfolio = base_port_spec,
                                  type = "return", name = "mean")
  base_port_spec <- add.objective(portfolio = base_port_spec,
                                  type = "risk", name = "StdDev")
  
  # Create two constraints:
  base_port_spec <- add.constraint(portfolio = base_port_spec,
                                   type = "full_investment")
  base_port_spec <- add.constraint(portfolio = base_port_spec,
                                   type = "long_only")
  
  if(input$checkbox_upper_constraint_crypto) {
    
    upper_constraint_crypto <- input$max_upper_constraint_crypto / 100
    
    pos_cryptos <- as_tibble(names(portfolio_input_data)) %>% # Crypto positions
      rename(name = value) %>% 
      left_join(lst_names) %>% 
      mutate(row_number = row_number()) %>% 
      filter(origin == 'Cryptocurrencies') %>% 
      pull(row_number)
    
    pos_non_cryptos <- as_tibble(names(portfolio_input_data)) %>% # Non-Crypto positions
      rename(name = value) %>% 
      left_join(lst_names) %>% 
      mutate(row_number = row_number()) %>% 
      filter(origin != 'Cryptocurrencies') %>% 
      pull(row_number)
    
    base_port_spec <- add.constraint(portfolio = base_port_spec, 
                                     type = "group", 
                                     groups = list(pos_cryptos, pos_non_cryptos), 
                                     group_min = c(0, 0), 
                                     group_max = c(upper_constraint_crypto, 1), 
                                     group_labels = c("Crypto", "NonCrypto"), 
                                     group_pos = c(2, 1))
    
  }

  
  # Maximize Sharpe Ratio
  # Optimization without rebalancing
  opt_without_rebalancing <- optimize.portfolio(R = portfolio_input_data,
                                                portfolio = base_port_spec,
                                                maxSR = TRUE, #<<<<<<<<<<<<<<<<<<<<Sharpe Ratio!
                                                optimize_method = "ROI",
                                                trace = TRUE)
  
  # plotly graph
  n.portfolios <- 100
  match.col <- "StdDev"
  
  portf <- opt_without_rebalancing$portfolio
  R <- opt_without_rebalancing$R
  frontier <- meanvar.efficient.frontier(portfolio=portf, R=R, n.portfolios=n.portfolios)
  
  xs <- scatterFUN(R=R, FUN=match.col)
  ys <- scatterFUN(R=R, FUN="mean")
  
  x.f <- frontier[, match.col]
  y.f <- frontier[, "mean"]
  
  df_sa <- tibble(name = colnames(portfolio_input_data), targetRisk = xs, targetReturn = ys)
  df_fp <- tibble(targetRisk = x.f, targetReturn = y.f)
  aux_tangency_point <- tibble(X = opt_without_rebalancing$opt_values[[1]], Y = opt_without_rebalancing$opt_values[[2]])
  aux_mv_point <- tibble(X = x.f[1], Y = y.f[1])
  
  t <- list(size = 14, color = "black")
  f <- list(size = 12, color = "black")
  
  df_sa %>%
    plot_ly(x = ~targetRisk,
            y = ~targetReturn,
            type = 'scatter', 
            mode = 'markers',
            hoverinfo = "text",
            text = paste0(df_sa$name, "<br>", 
                          "Risk: ", round(df_sa$targetRisk, 3), "<br>",
                          "Return: ", round(df_sa$targetReturn, 3)),
            showlegend = FALSE) %>%
    add_text(text = df_sa$name, textfont = t, fill=NULL, 
             name=paste("Hide/Show","<br>",
                        "names"), 
             showlegend = TRUE, 
             marker = NULL,
             textposition = "top"#,
             #visible = "legendonly" # The default is showing the names of the cryptos/markers
    ) %>%
    add_trace(x = ~targetRisk,
              y = ~targetReturn,
              type = 'scatter',
              mode = 'lines',
              data = df_fp,
              line = list(color = "#90353B"),
              inherit = FALSE, # Tip from https://community.plot.ly/t/error-when-add-trace-histogram-type/4053/3
              #showlegend = FALSE,
              name = 'Frontier') %>%
    add_trace(x = ~X,
              y = ~Y,
              type = 'scatter',
              #mode = 'lines',
              data = aux_tangency_point,
              inherit = FALSE, # Tip from https://community.plot.ly/t/error-when-add-trace-histogram-type/4053/3
              #showlegend = FALSE,
              name = 'Tangency Portfolio',
              hoverinfo = "text",
              text = paste0('Tangency Portfolio', "<br>", 
                            "Risk: ", round(aux_tangency_point$X, 3), "<br>",
                            "Return: ", round(aux_tangency_point$Y, 3)),
              marker = list(size = 15,
                            sizeref = .15, 
                            color = "#023A78", #"#936DB3",
                            symbol = 17)) %>%
    add_trace(x = ~X,
              y = ~Y,
              type = 'scatter',
              #mode = 'lines',
              data = aux_mv_point,
              inherit = FALSE, # Tip from https://community.plot.ly/t/error-when-add-trace-histogram-type/4053/3
              #showlegend = FALSE,
              name = 'Min. Var. Portfolio',
              hoverinfo = "text",
              text = paste0('Minimum Variance Portfolio', "<br>", 
                            "Risk: ", round(aux_mv_point$X, 3), "<br>",
                            "Return: ", round(aux_mv_point$Y, 3)),
              marker = list(size = 15,
                            sizeref = .15, 
                            color = "#90353B", # "#339635",
                            symbol = 17)) %>%
    layout(title = paste0('Efficient Frontier (fitted from ', min_pairwise_date, ' to ', max_pairwise_date, ')'),
           titlefont = t,
           xaxis = list(title = 'Risk',
                        showgrid = TRUE,
                        #range = x_range,
                        titlefont = f),
           yaxis = list(title = 'Expected Return',
                        showgrid = TRUE,
                        #range = y_range,
                        titlefont = f),
           showlegend = TRUE) %>%
    config(
      modeBarButtonsToRemove = list(
        'pan2d',
        'resetScale2d',
        'autoScale2d',
        'zoomIn2d',
        'zoomOut2d',
        'select2d',
        #'zoom2d',
        'hoverClosestCartesian',
        'lasso2d',
        'toggleSpikelines',
        'sendDataToCloud',
        'hoverCompareCartesian'
      )
    )
  
})

output$portfolio_plot <- renderPlotly({
  portfolio_plot_reactive()
})

#### Tangency Weights Pie ----

tangency_weights_pie_plot_reactive <- eventReactive(input$PortfolioActionButton, {
  
  asset_names_portfolio <- input$asset_names_portfolio
  date_range_portfolio <- input$date_range_portfolio
  RiskFreeRate <- rfr()
  
  if(input$portfolio_dolarized_returns) {
    
    var <- "ret.adjusted.prices.usd"
    
  } else {
    
    var <- "ret.adjusted.prices"
    
  }
  
  aux_pre <- full_data %>%
    filter(
      name %in% asset_names_portfolio,
      date >= date_range_portfolio[1],
      date <= date_range_portfolio[2]
    ) %>%
    select(date, name, ret.adjusted.prices = {{var}}) %>%
    spread(name, ret.adjusted.prices) %>%
    na.omit()
  
  min_pairwise_date <- min(aux_pre$date)
  max_pairwise_date <- max(aux_pre$date)
  
  aux_pre <- aux_pre - RiskFreeRate
  
  datas2 <- aux_pre %>% pull(date)
  
  portfolio_input_data <- aux_pre %>% 
    select(-date) %>% 
    xts(order.by = datas2)
  
  # Create a portfolio
  base_port_spec <- portfolio.spec(assets = colnames(portfolio_input_data))
  
  # Create two objective functions:
  base_port_spec <- add.objective(portfolio = base_port_spec,
                                  type = "return", name = "mean")
  base_port_spec <- add.objective(portfolio = base_port_spec,
                                  type = "risk", name = "StdDev")
  
  # Create two constraints:
  base_port_spec <- add.constraint(portfolio = base_port_spec,
                                   type = "full_investment")
  base_port_spec <- add.constraint(portfolio = base_port_spec,
                                   type = "long_only")
  
  if(input$checkbox_upper_constraint_crypto) {
    
    upper_constraint_crypto <- input$max_upper_constraint_crypto / 100
    
    pos_cryptos <- as_tibble(names(portfolio_input_data)) %>% # Crypto positions
      rename(name = value) %>% 
      left_join(lst_names) %>% 
      mutate(row_number = row_number()) %>% 
      filter(origin == 'Cryptocurrencies') %>% 
      pull(row_number)
    
    pos_non_cryptos <- as_tibble(names(portfolio_input_data)) %>% # Non-Crypto positions
      rename(name = value) %>% 
      left_join(lst_names) %>% 
      mutate(row_number = row_number()) %>% 
      filter(origin != 'Cryptocurrencies') %>% 
      pull(row_number)
    
    base_port_spec <- add.constraint(portfolio = base_port_spec, 
                                     type = "group", 
                                     groups = list(pos_cryptos, pos_non_cryptos), 
                                     group_min = c(0, 0), 
                                     group_max = c(upper_constraint_crypto, 1), 
                                     group_labels = c("Crypto", "NonCrypto"), 
                                     group_pos = c(2, 1))
    
  }
  
  # Maximize Sharpe Ratio
  # Optimization without rebalancing
  opt_without_rebalancing <- optimize.portfolio(R = portfolio_input_data,
                                                portfolio = base_port_spec,
                                                maxSR = TRUE, #<<<<<<<<<<<<<<<<<<<<Sharpe Ratio!
                                                optimize_method = "ROI",
                                                trace = TRUE)
  
  gw <- extractWeights(opt_without_rebalancing)
  
  aux_pie <- tibble(name = names(gw), weight = gw)
  
  aux_pie %>%
    filter(weight > 0) %>%
    plot_ly(labels = ~name, 
            values = ~weight, 
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            hoverinfo = 'text',
            text = ~paste0(name, ' (', round(weight, 5) * 100, '%)'),
            showlegend = FALSE) %>%
    layout(title = 'Tangency Weights') %>%
    config(
      modeBarButtonsToRemove = list(
        'toggleSpikelines',
        'pan2d',
        'resetScale2d',
        'autoScale2d',
        'zoomIn2d',
        'zoomOut2d',
        'select2d',
        'zoom2d',
        'hoverClosestCartesian',
        'lasso2d',
        'toggleSpikelines',
        'sendDataToCloud'
      )
    )
  
  
})

output$tangency_weights_pie_plot <- renderPlotly({
  tangency_weights_pie_plot_reactive()
})

#### Minimum Variance Weights Pie ----

minimum_variance_weights_pie_plot_reactive <- eventReactive(input$PortfolioActionButton, {
  
  asset_names_portfolio <- input$asset_names_portfolio
  date_range_portfolio <- input$date_range_portfolio
  RiskFreeRate <- rfr()
  
  if(input$portfolio_dolarized_returns) {
    
    var <- "ret.adjusted.prices.usd"
    
  } else {
    
    var <- "ret.adjusted.prices"
    
  }
  
  aux_pre <- full_data %>%
    filter(
      name %in% asset_names_portfolio,
      date >= date_range_portfolio[1],
      date <= date_range_portfolio[2]
    ) %>%
    select(date, name, ret.adjusted.prices = {{var}}) %>%
    spread(name, ret.adjusted.prices) %>%
    na.omit()
  
  min_pairwise_date <- min(aux_pre$date)
  max_pairwise_date <- max(aux_pre$date)
  
  aux_pre <- aux_pre - RiskFreeRate
  
  datas2 <- aux_pre %>% pull(date)
  
  portfolio_input_data <- aux_pre %>% 
    select(-date) %>% 
    xts(order.by = datas2)
  
  # Create a portfolio
  base_port_spec <- portfolio.spec(assets = colnames(portfolio_input_data))
  
  # Create objective functions:
  #base_port_spec <- add.objective(portfolio = base_port_spec,
  #                                type = "return", name = "mean")
  base_port_spec <- add.objective(portfolio = base_port_spec,
                                  type = "risk", name = "var") # pg. 10 of https://cran.r-project.org/web/packages/PortfolioAnalytics/vignettes/ROI_vignette.pdf
  
  # Create two constraints:
  base_port_spec <- add.constraint(portfolio = base_port_spec,
                                   type = "full_investment")
  base_port_spec <- add.constraint(portfolio = base_port_spec,
                                   type = "long_only")
  
  if(input$checkbox_upper_constraint_crypto) {
    
    upper_constraint_crypto <- input$max_upper_constraint_crypto / 100
    
    pos_cryptos <- as_tibble(names(portfolio_input_data)) %>% # Crypto positions
      rename(name = value) %>% 
      left_join(lst_names) %>% 
      mutate(row_number = row_number()) %>% 
      filter(origin == 'Cryptocurrencies') %>% 
      pull(row_number)
    
    pos_non_cryptos <- as_tibble(names(portfolio_input_data)) %>% # Non-Crypto positions
      rename(name = value) %>% 
      left_join(lst_names) %>% 
      mutate(row_number = row_number()) %>% 
      filter(origin != 'Cryptocurrencies') %>% 
      pull(row_number)
    
    base_port_spec <- add.constraint(portfolio = base_port_spec, 
                                     type = "group", 
                                     groups = list(pos_cryptos, pos_non_cryptos), 
                                     group_min = c(0, 0), 
                                     group_max = c(upper_constraint_crypto, 1), 
                                     group_labels = c("Crypto", "NonCrypto"), 
                                     group_pos = c(2, 1))
    
  }
  
  # Optimization without rebalancing
  opt_without_rebalancing <- optimize.portfolio(R = portfolio_input_data,
                                                portfolio = base_port_spec,
                                                #maxSR = TRUE, #<<<<<<<<<<<<<<<<<<<<Sharpe Ratio!
                                                optimize_method = "ROI",
                                                trace = TRUE)
  
  gw <- extractWeights(opt_without_rebalancing)
  
  aux_pie <- tibble(name = names(gw), weight = gw)
  
  aux_pie %>%
    filter(weight > 0) %>%
    plot_ly(labels = ~name, 
            values = ~weight, 
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            hoverinfo = 'text',
            text = ~paste0(name, ' (', round(weight, 5) * 100, '%)'),
            showlegend = FALSE) %>%
    layout(title = 'Minimum Variance Weights') %>%
    config(
      modeBarButtonsToRemove = list(
        'toggleSpikelines',
        'pan2d',
        'resetScale2d',
        'autoScale2d',
        'zoomIn2d',
        'zoomOut2d',
        'select2d',
        'zoom2d',
        'hoverClosestCartesian',
        'lasso2d',
        'toggleSpikelines',
        'sendDataToCloud'
      )
    )
  
  
})

output$minimum_variance_weights_pie_plot <- renderPlotly({
  minimum_variance_weights_pie_plot_reactive()
})



#### Backtest Plots ----

#performance_plot_reactive <- eventReactive(c(input$benchmark_performance, 
appended_benchmark_data <- eventReactive(
  c(input$benchmark_performance, 
    input$naive_portfolio,
    input$rebalance_window,
    input$rebalance_portfolio,
    input$PortfolioActionButton), 
  {
    
    rebalance_window <- input$rebalance_window
    asset_names_portfolio <- input$asset_names_portfolio
    date_range_portfolio <- input$date_range_portfolio
    RiskFreeRate <- rfr()
    benchmark_performance <- input$benchmark_performance
    
    if(input$portfolio_dolarized_returns) {
      
      var <- "ret.adjusted.prices.usd"
      
    } else {
      
      var <- "ret.adjusted.prices"
      
    }
    
    aux_pre <- full_data %>%
      filter(
        name %in% asset_names_portfolio,
        date >= date_range_portfolio[1],
        date <= date_range_portfolio[2]
      ) %>%
      select(date, name, ret.adjusted.prices = {{var}}) %>%
      spread(name, ret.adjusted.prices) %>%
      na.omit()
    
    min_pairwise_date <- min(aux_pre$date)
    max_pairwise_date <- max(aux_pre$date)
    
    aux_pre <- aux_pre - RiskFreeRate
    
    datas2 <- aux_pre %>% pull(date)
    
    portfolio_input_data <- aux_pre %>% 
      select(-date) %>% 
      xts(order.by = datas2)
    
    benchmark_input_data <- aux_pre %>% 
      select(benchmark_performance) %>% 
      xts(order.by = datas2)
    
    # Max Return portfolio
    max_ret_spec <- portfolio.spec(assets = colnames(portfolio_input_data))
    max_ret_spec <- add.objective(portfolio = max_ret_spec, type = "return", name = "mean")
    max_ret_spec <- add.objective(portfolio = max_ret_spec, type = "risk", name = "StdDev")
    max_ret_spec <- add.constraint(portfolio = max_ret_spec,type = "full_investment")
    max_ret_spec <- add.constraint(portfolio = max_ret_spec,type = "long_only")
    
    if(input$checkbox_upper_constraint_crypto) {
      
      upper_constraint_crypto <- input$max_upper_constraint_crypto / 100
      
      pos_cryptos <- as_tibble(names(portfolio_input_data)) %>% # Crypto positions
        rename(name = value) %>% 
        left_join(lst_names) %>% 
        mutate(row_number = row_number()) %>% 
        filter(origin == 'Cryptocurrencies') %>% 
        pull(row_number)
      
      pos_non_cryptos <- as_tibble(names(portfolio_input_data)) %>% # Non-Crypto positions
        rename(name = value) %>% 
        left_join(lst_names) %>% 
        mutate(row_number = row_number()) %>% 
        filter(origin != 'Cryptocurrencies') %>% 
        pull(row_number)
      
      max_ret_spec <- add.constraint(portfolio = max_ret_spec, 
                                       type = "group", 
                                       groups = list(pos_cryptos, pos_non_cryptos), 
                                       group_min = c(0, 0), 
                                       group_max = c(upper_constraint_crypto, 1), 
                                       group_labels = c("Crypto", "NonCrypto"), 
                                       group_pos = c(2, 1))
      
    }
    
    opt_max_ret <- optimize.portfolio(R = portfolio_input_data,
                                      portfolio = max_ret_spec,
                                      maxSR = TRUE, #<<<<<<<<<<<<<<<<<<<<Sharpe Ratio!
                                      optimize_method = "ROI",
                                      trace = TRUE)
    
    # Min Var portfolio
    min_var_spec <- portfolio.spec(assets = colnames(portfolio_input_data))
    min_var_spec <- add.objective(portfolio = min_var_spec,  type = "risk", name = "var")
    min_var_spec <- add.constraint(portfolio = min_var_spec, type = "full_investment")
    min_var_spec <- add.constraint(portfolio = min_var_spec, type = "long_only")
    
    if(input$checkbox_upper_constraint_crypto) {
      
      upper_constraint_crypto <- input$max_upper_constraint_crypto / 100
      
      pos_cryptos <- as_tibble(names(portfolio_input_data)) %>% # Crypto positions
        rename(name = value) %>% 
        left_join(lst_names) %>% 
        mutate(row_number = row_number()) %>% 
        filter(origin == 'Cryptocurrencies') %>% 
        pull(row_number)
      
      pos_non_cryptos <- as_tibble(names(portfolio_input_data)) %>% # Non-Crypto positions
        rename(name = value) %>% 
        left_join(lst_names) %>% 
        mutate(row_number = row_number()) %>% 
        filter(origin != 'Cryptocurrencies') %>% 
        pull(row_number)
      
      min_var_spec <- add.constraint(portfolio = min_var_spec, 
                                     type = "group", 
                                     groups = list(pos_cryptos, pos_non_cryptos), 
                                     group_min = c(0, 0), 
                                     group_max = c(upper_constraint_crypto, 1), 
                                     group_labels = c("Crypto", "NonCrypto"), 
                                     group_pos = c(2, 1))
      
    }
    
    # Optimization without rebalancing
    opt_min_var <- optimize.portfolio(R = portfolio_input_data,
                                      portfolio = min_var_spec,
                                      #maxSR = TRUE, #<<<<<<<<<<<<<<<<<<<<Sharpe Ratio!
                                      optimize_method = "ROI",
                                      trace = TRUE)
    
    gw_tp <- extractWeights(opt_max_ret)
    gw_mvp <- extractWeights(opt_min_var)
    
    weights_df <- data.frame(asset = names(gw_tp), Tangency_Weights = gw_tp, Min_Var_Weights = gw_mvp) %>% 
      mutate(asset = str_replace(asset, ',', ' -')) # Avoiding csv separator issue generated in the download data
    
    N <- length(asset_names_portfolio)
    weights_np <- rep(1/N, N)
    
    
    if ((rebalance_window != 'none') & (input$rebalance_portfolio)) {
      
      tp_wealth_with_rebalancing <- PerformanceAnalytics::Return.portfolio(portfolio_input_data,
                                                                           rebalance_on = rebalance_window,
                                                                           weights = gw_tp,
                                                                           wealth.index = TRUE) %>%
        tidy() %>%
        mutate(series = 'Tangency Portfolio with Rebalancing')
      
      mvp_wealth_with_rebalancing <- PerformanceAnalytics::Return.portfolio(portfolio_input_data,
                                                                            rebalance_on = rebalance_window,
                                                                            weights = gw_mvp,
                                                                            wealth.index = TRUE) %>%
        tidy() %>%
        mutate(series = 'Minimum Variance with Rebalancing')
      
      naive_wealth_with_rebalancing <- PerformanceAnalytics::Return.portfolio(portfolio_input_data,
                                                                              rebalance_on = rebalance_window,
                                                                              weights = weights_np,
                                                                              wealth.index = TRUE) %>%
        tidy() %>%
        mutate(series = 'Naive Portfolio with Rebalancing')
      
      list_rebalanced <- list(tp_wealth_with_rebalancing, 
                              mvp_wealth_with_rebalancing,
                              naive_wealth_with_rebalancing)
    }
    
    
    tp_wealth_without_rebalancing <- PerformanceAnalytics::Return.portfolio(portfolio_input_data,
                                                                            #rebalance_on = rebalance_window,
                                                                            weights = gw_tp,
                                                                            wealth.index = TRUE) %>%
      tidy() %>%
      mutate(series = 'Tangency Portfolio without Rebalancing')
    
    
    mvp_wealth_without_rebalancing <- PerformanceAnalytics::Return.portfolio(portfolio_input_data,
                                                                             #rebalance_on = rebalance_window,
                                                                             weights = gw_mvp,
                                                                             wealth.index = TRUE) %>%
      tidy() %>%
      mutate(series = 'Minimum Variance without Rebalancing')
    
    naive_wealth_without_rebalancing <- PerformanceAnalytics::Return.portfolio(portfolio_input_data,
                                                                               #rebalance_on = rebalance_window,
                                                                               weights = weights_np,
                                                                               wealth.index = TRUE) %>%
      tidy() %>%
      mutate(series = 'Naive Portfolio without Rebalancing')
    
    list_not_rebalanced <- list(tp_wealth_without_rebalancing,
                                mvp_wealth_without_rebalancing,
                                naive_wealth_without_rebalancing)
    
    if (!input$rebalance_portfolio) { list_rebalanced <- NULL }
    
    if (!input$naive_portfolio) { list_rebalanced[[3]] <- NULL; list_not_rebalanced[[3]] <- NULL }
    
    if (input$rebalance_portfolio) { list_plot <- c(list_rebalanced, list_not_rebalanced) }
    else { list_plot <- list_not_rebalanced }
    
    benchmark_wealth <- PerformanceAnalytics::Return.portfolio(benchmark_input_data,
                                                               weights = 1,
                                                               wealth.index = TRUE) %>%
      tidy() %>%
      mutate(series = paste0(benchmark_performance))
    
    list_plot <- c(list_plot, list(benchmark_wealth))
    
    appended_benchmark_data <- map_dfr(list_plot, bind_rows) %>%
      rename(date = index,
             Type = series,
             Exp_Return = value) %>%
      mutate(Exp_Return = Exp_Return - 1)
    
    return(list(min_pairwise_date, max_pairwise_date, appended_benchmark_data, weights_df))
    
  })

performance_plot_reactive <- eventReactive(c(input$benchmark_performance, 
                                             input$naive_portfolio,
                                             input$rebalance_window,
                                             input$rebalance_portfolio,
                                             input$PortfolioActionButton),{
  
   rebalance_window <- input$rebalance_window
   asset_names_portfolio <- input$asset_names_portfolio
   date_range_portfolio <- input$date_range_portfolio
   RiskFreeRate <- rfr()
   benchmark_performance <- input$benchmark_performance
  
  min_pairwise_date <- appended_benchmark_data()[1][[1]]
  max_pairwise_date <- appended_benchmark_data()[2][[1]]
  appended_benchmark_data <- appended_benchmark_data()[3][[1]]
  
  t <- list(size = 14, color = "black")
  
  appended_benchmark_data %>%
    plot_ly(
      x = ~date, 
      y = ~Exp_Return * 100,
      type = 'scatter', 
      mode = 'lines', 
      color = ~Type,
      #opacity = 0.65,
      hoverinfo = "text",
      text = ~paste0(Type, '<br>', 
                    'Cum. Ret. (%): ', round(Exp_Return, 4) * 100, '<br>',
                    'Date: ', date)
    ) %>%
    layout(
      hovermode = 'compare', # Compare data as default
      title = paste0("<br><br>Performance Comparison with ", benchmark_performance, ' (fitted from ', min_pairwise_date, ' to ', max_pairwise_date, ')'),
      titlefont = t,
      xaxis = list(title = '', showgrid = TRUE),
      yaxis = list(title = 'Cum. Return (%)', showgrid = TRUE)) %>%
    config(
      modeBarButtonsToRemove = list(
        'toggleSpikelines',
        'pan2d',
        'resetScale2d',
        'autoScale2d',
        'zoomIn2d',
        'zoomOut2d',
        'select2d',
        'zoom2d',
        'hoverClosestCartesian',
        'lasso2d',
        'toggleSpikelines',
        'sendDataToCloud'
      )
    )
  
})

output$performance_time_series <- renderPlotly({
  
  if(input$PortfolioActionButton) {
    performance_plot_reactive()  
  }
  
})



#### Backtest Statistics ----

benchmark_descriptive_reactive <- eventReactive(c(
  input$benchmark_performance, 
  input$naive_portfolio,
  input$rebalance_window,
  input$rebalance_portfolio,
  input$PortfolioActionButton),{
   
   rebalance_window <- input$rebalance_window
   asset_names_portfolio <- input$asset_names_portfolio
   date_range_portfolio <- input$date_range_portfolio
   RiskFreeRate <- rfr()
   benchmark_performance <- input$benchmark_performance
   
   tbl_subset <- appended_benchmark_data()[3][[1]]
   
   tbl_subset <- tbl_subset %>% 
     mutate(Exp_Return = Exp_Return - lag(Exp_Return)) # Converts the cumulative return to original returns
   
   tbl_subset <-
     tbl_subset %>% 
     group_by(Type) %>% 
     summarize(
       # Mean = mean(Exp_Return, na.rm = TRUE), # Not Annualized
       Mean = ((1 + mean(Exp_Return, na.rm = TRUE)) ^ 252 - 1),
       # SD = sd(Exp_Return, na.rm = TRUE), # Not Annualized
       SD = sd(Exp_Return, na.rm = T) * sqrt(252),
       # Sharpe_Ratio = mean(Exp_Return - 0.0000684171196396743, na.rm = TRUE)/sd(Exp_Return, na.rm = TRUE), # Not Annualized
       Sharpe_Ratio = (Mean - 0.0000684171196396743 ^ 252) / SD,
       Median = median(Exp_Return, na.rm = TRUE),
       Lower_tail = quantile(Exp_Return, 0.01, na.rm = TRUE),
       Upper_tail = quantile(Exp_Return, 0.99, na.rm = TRUE)#,
       #Number_of_Obs = n()
     ) %>% 
     mutate_if(is.numeric, list(~round(., digits = 4)))
   
   colnames(tbl_subset) <-
     c(
       'Name',
       'Avg. Ret.', 
       'SD', 
       'Sharpe Ratio', 
       'Median Ret.', 
       'Lower Tail (<1%)', 
       'Upper Tail (>99%)'
     )
   
   return(tbl_subset)
   
 })


output$benchmark_descriptive_table <- 
  DT::renderDT({
    benchmark_descriptive_reactive()
  },
  options = list(pageLength = 1000,
                 dom = 't'),
  rownames = FALSE
  )




#### Download Data ----


download_data_reactive <- eventReactive(c(input$PortfolioActionButton),{
    
    returns_generated <- appended_benchmark_data()[3][[1]] %>% spread(Type, Exp_Return)
    weights_generated <- appended_benchmark_data()[4][[1]]
    
    return(list(returns_generated, weights_generated))
    
  })

output$downloadData <- downloadHandler(
  filename = function() {
    paste("optimization_data", "zip", sep=".")
  },
  content = function(fname) {
    fs <- c()
    tmpdir <- tempdir()
    setwd(tempdir())
    
    paths <- c('returns_generated.csv', 'weights_generated.csv')

    write_csv(download_data_reactive()[1][[1]], paths[1])
    write_csv(download_data_reactive()[2][[1]], paths[2])
    
    zip(zipfile = fname, files = paths)
  },
  contentType = "application/zip"
)
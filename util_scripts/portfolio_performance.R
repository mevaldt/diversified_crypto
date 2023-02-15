library(dplyr)
library(tidyr)
library(tidyquant)

# 
# db_prices <- readRDS('data/db_prices.RDS')
# 
# assets <- unique(db_prices$symbol)
# 
# weights <- rep(1/length(assets), length(assets))
# 
# rebalance <- c(NA, 'years', 'months', 'weeks', 'days')
# 
# portfolio_returns <- get_returns(db_prices, assets, period = 'daily')
# 
# benchmark_returns <- get_returns(db_prices %>% filter(symbol == '^GSPC'), '^GSPC', period = 'daily')
# 
# portfolio_performance <- get_performance(portfolio_returns, assets, weights, rebalance = rebalance[2])
# 
# benchmark_performance <- get_performance(benchmark_returns, '^GSPC', 1, rebalance = rebalance[2])


#---- individual metrics ----

get_individual_metrics <- function(portfolio_returns){
  
  basic_metrics <- portfolio_returns %>% 
    summarise(
      Mean = mean(return, na.rm = TRUE),
      SD = sd(return, na.rm = TRUE),
      Median = median(return, na.rm = TRUE),
      Lower_tail = quantile(return, 0.01, na.rm = TRUE),
      Upper_tail = quantile(return, 0.99, na.rm = TRUE)
    ) %>% 
    mutate_if(is.numeric, list(~round(., digits = 4)))

  annualized_metrics <- portfolio_returns %>% 
    tq_performance(Ra = return,
                   performance_fun = table.AnnualizedReturns)
  
  
  downside_metrics <- portfolio_returns %>% 
    tq_performance(Ra = return,
                   performance_fun = table.DownsideRisk)
  
  var <- portfolio_returns %>% 
    tq_performance(Ra = return,
                   performance_fun = VaR)
  
  sharpe <- portfolio_returns %>% 
    tq_performance(Ra = return,
                   performance_fun = SharpeRatio)
  
  db_metrics <- bind_cols(basic_metrics, annualized_metrics, downside_metrics, var, sharpe) %>% 
    pivot_longer(cols = everything(), names_to = 'metric', values_to = 'value')
  
  return(db_metrics)
}



#---- plot returns ----

plot_returns <- function(db_performance){
  plot_ly(db_performance,
          x = ~date,
          y = ~value,
          color = ~return,
          type = 'scatter',
          mode = 'lines') %>% 
    rangeslider(borderwidth = 1) %>% 
    layout(xaxis = list(title = "",
                        rangeselector = list(
                          buttons = list(
                            list(
                              count    = 3,
                              label    = "3 mo",
                              step     = "month",
                              stepmode = "backward"),
                            list(
                              count    = 6,
                              label    = "6 mo",
                              step     = "month",
                              stepmode = "backward"),
                            list(
                              count    = 1,
                              label    = "1 yr",
                              step     = "year",
                              stepmode = "backward"),
                            list(
                              count    = 1,
                              label    = "YTD",
                              step     = "year",
                              stepmode = "todate"),
                            list(step = "all")
                          )
                        )), 
           yaxis = list(title      = "Return Performance",
                        tickformat = "%",
                        autorange  = T,
                        fixedrange = F),
           legend = list(itemclick       = "toggleothers",
                         itemdoubleclick = "toggle"),
           dragmode = 'zoom',
           datarevision = 0) %>% 
    config(displayModeBar = F)
  
}


#---- plot drawdown ----

plot_drawdown <- function(db_performance){
  
  
  db_drawdown <- db_performance %>% 
    group_by(return) %>% 
    mutate(
      id = row_number()
    ) %>% 
    na.omit() %>% 
    mutate(
      max_until = runMax(value, n = 1, cumulative = T),
      loss_relative = value/max_until - 1
    ) 
  
  db_drawdown %>% 
    plot_ly(
      x = ~date,
      y = ~loss_relative,
      color = ~return,
      type = 'scatter',
      mode = 'lines'
      #fill = 'tozeroy'
    ) %>% 
    layout(
      xaxis = list(type = 'date'),
      yaxis = list(title      = "Drawdown",
                   tickformat = "%",
                   autorange  = T,
                   fixedrange = F)
    ) %>% 
    config(displayModeBar = F)
  
}

#---- plot rolling sd ----

plot_rolling_sd_returns <- function(db_performance, window){
  
  rolling_sd_returns <- db_performance %>% 
    group_by(return) %>% 
    tq_mutate(
      select = value,
      mutate_fun = rollapply,
      width = window,
      align = 'right',
      FUN = sd,
      na.rm = T,
      col_rename = 'rolling_sd'
    )
  
  rolling_sd_returns %>% 
    plot_ly(
      x = ~date,
      y = ~rolling_sd,
      color = ~return,
      type = 'scatter',
      mode = 'lines'
      #fill = 'tozeroy'
    ) %>% 
    layout(
      xaxis = list(type = 'Date'),
      yaxis = list(title      = "Rolling StdDev",
                   autorange  = T,
                   fixedrange = F)
    ) %>% 
    config(displayModeBar = F)
  
}


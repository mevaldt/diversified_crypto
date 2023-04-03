library(dplyr)
library(tidyquant)
library(ggplot2)

get_returns <- function(db_prices, assets, period = c('daily', 'monthly', 'quarterly', 'yearly')){
  
  assets_returns_daily <- db_prices %>% 
    filter(symbol %in% assets) %>% 
    group_by(symbol) %>% 
    tq_transmute(
      select     = price.adjusted, 
      mutate_fun = periodReturn, 
      period     = period,
      col_rename = "return"
    ) 
  
  return(assets_returns_daily)
}


get_performance <- function(db_returns, assets, weights, rebalance){
  
  asset_weights <- tibble(assets = assets, weights = weights)
  
  portfolio_returns <- db_returns %>% 
    tq_portfolio(
      assets_col   = symbol,
      returns_col  = return,
      weights      = asset_weights,
      rebalance_on = rebalance,
      col_rename = "return"
    ) %>% 
    mutate(
      daily_cum = cumprod(1 + return)
    )
  
  return(portfolio_returns)
}
# 
# #---- test function ----
# 
# db_prices <- readRDS('data/db_prices.RDS')
# 
# assets <- unique(db_prices$symbol)
# 
# db_returns <- get_returns(db_prices, assets, period = 'daily')
# 
# weights <- rep(1/length(assets), length(assets))
# 
# rebalance <- c(NA, 'years', 'months', 'weeks', 'days')
# 
# returns <- get_performance(db_returns, assets, weights, rebalance[1])
# 
# returns %>%
#   ggplot(aes(x = date, y = daily_cum)) +
#   geom_line()

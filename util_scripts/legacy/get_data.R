library(dplyr)
library(tidyquant)
library(purrr)


#---- symbol by country ----

get_symbol_by_country <- function(){
  tibble(
    symbol = c(
      #brazil
      '^BVSP',
      'IVVB11.SA',
      'SMAL11.SA',
      'USDBRL=X',
      'GC=F',
      'BTC-USD',
      'XRP-USD',
      #usa
      '^TNX',
      '^TYX',
      '^GSPC',
      'OEX',
      'DJI',
      '^RMZ',
      'GC=F',
      'BTC-USD',
      'XRP-USD'),
    name = c(
      #brazil
      'Ibovespa',
      'iShares S&P 500 FIC',
      'iShares BM&FBovespa Small Cap',
      'USD-BRL',
      'Gold',
      'Bitcoin',
      'Ripple',
      #usa
      'T-bons 10y',
      'T-bons 30y',
      'S&P 500',
      'S&P 100',
      'Down Jones',
      'Msci US REIT',
      'Gold',
      'Bitcoin',
      'Ripple'),
    country = c('Brazil',
                'Brazil',
                'Brazil',
                'Brazil',
                'Brazil',
                'Brazil',
                'Brazil',
                'United States',
                'United States',
                'United States',
                'United States',
                'United States',
                'United States',
                'United States',
                'United States',
                'United States')
  ) %>% 
    mutate(
      rf = case_when(
        country == 'Brazil' ~ 'CDI',
        country == 'United States' ~ '^TNX'
      ),
      type = case_when(
        symbol %in% c("^BVSP", "IVVB11.SA", "SMAL11.SA", "^GSPC", "OEX", "^RMZ", "DJI") ~ 'Equity',
        symbol %in% c("USDBRL=X") ~ 'Currency',
        symbol %in% c("^TNX", "^TYX") ~ 'Fixed Income',
        symbol %in% c("BTC-USD", "XRP-USD") ~ 'Crypto Currency',
        symbol %in% c("USDBRL=X", "GC=F") ~ 'Others'
      )
    )
  
}

#---- get data  ----
# 
# tickers <- unique(get_symbol_by_country()$symbol)
# 
# dt_ini <- '2015-01-01'
# 
# dt_fim <- Sys.Date()
# 
# db_prices <- map_df(tickers, ~tq_get(.x, from = dt_ini, to = dt_fim))
#  
# # db_prices %>% 
# #   filter(is.na(adjusted)) %>% 
# #   count(symbol)
# 
# saveRDS(db_prices, 'data/db_prices.RDS')

# ---- Function to collect Ticker data from YF ----

# This function accepts a vector of Tickers to be searched
# using the Yahoo Finance API

collect_tickers_data <-
  function(tickers_db, start_date = '1990-01-01') {
    
    `%>%` <- magrittr::`%>%`
    
    # --- Gathering ticker data ----
    
    list_tickers <- 
      tickers_db %>% 
      dplyr::pull(symbol) %>% 
      paste0("^", .)
    
    list_tickers <- ifelse(list_tickers == '^IMOEX.ME', 'IMOEX.ME', list_tickers)
    
    ticker_df <-
      tidyquant::tq_get(
        list_tickers,
        from = start_date
      ) %>% 
      dplyr::select(
        -c(close)
      ) %>% 
      dplyr::rename(
        price.open = open,
        price.high = high,
        price.low = low,
        price.adjusted = adjusted
      ) %>% 
      dplyr::mutate(
        symbol = ifelse(symbol == 'IMOEX.ME', '^IMOEX.ME', symbol),
        symbol = stringr::str_remove_all(symbol, '\\^'),
        type = 'Ticker',
        price.open = ifelse(date == "2019-09-12" & symbol == "AXJO", 6638.00, price.open),
        price.high = ifelse(date == "2019-09-12" & symbol == "AXJO", 6687.40, price.high),
        price.low = ifelse(date == "2019-09-12" & symbol == "AXJO", 6638.00, price.low),
        price.adjusted = ifelse(date == "2019-09-12" & symbol == "AXJO", 6654.90, price.adjusted)
      ) 
    
    # ---- Gathering country fx data ----
    
    list_fx <-
      tickers_db %>% 
      dplyr::filter(
        !is.na(currency)
      ) %>%  
      dplyr::pull(currency) %>% 
      unique()
    
    fx_df <-
      tidyquant::tq_get(
        list_fx,
        from = start_date
      ) %>% 
      dplyr::select(
        currency = symbol,
        date,
        fx.adjusted = adjusted
      ) %>% 
      dplyr::mutate(
        ### Adjusting error in data for Indonesia
        fx.adjusted = ifelse(currency == "USDIDR=X" & date %in% c(as.Date('2010-11-01'), 
                                                                  as.Date('2012-02-07')), fx.adjusted *10, fx.adjusted),
        ### Adjusting error in data for Chile
        fx.adjusted = ifelse(currency == 'USDCLP=X' & date %in% c(as.Date('2014-04-10'),
                                                                  as.Date('2016-12-22')), fx.adjusted * 100, fx.adjusted),
        ### Another error in USDCLP=X series
        fx.adjusted = ifelse(currency == 'USDCLP=X' & date == as.Date('2016-12-22'), 663.76, fx.adjusted),
        
        ### Adjusting error in data for China
        fx.adjusted = ifelse(currency == 'USDCNY=X' & date == as.Date('2011-07-18'), 6.458, fx.adjusted),
        
        ### Adjusting error in data for Russia
        fx.adjusted = ifelse(currency == 'USDRUB=X' & date == as.Date('2016-01-06'), 71.62, fx.adjusted)
      )
    
    return_df <-
      ticker_df %>% 
      dplyr::left_join(tickers_db, by = 'symbol') %>% 
      dplyr::left_join(
        fx_df,
        by = c('date', 'currency')
      ) %>% 
      dplyr::mutate(
        price.adjusted.usd = price.adjusted/fx.adjusted
      ) %>% 
      dplyr::group_by(symbol) %>% 
      dplyr::arrange(symbol, date) %>% 
      dplyr::mutate(
        ret.adjusted.prices = (price.adjusted / dplyr::lag(price.adjusted)) - 1,
        ret.adjusted.prices.usd = (price.adjusted.usd / dplyr::lag(price.adjusted.usd)) - 1
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(sort(tidyselect::peek_vars())) %>% 
      dplyr::distinct()
    
    return(return_df)
    
  }

# ---- Function to collect Crypto data from YF ----

# This function accepts a vector of Cryptocurrencies to be searched
# using the Yahoo Finance API

collect_crypto_data <-
  function(crypto_db, start_date = '1990-01-01') {
    
    `%>%` <- magrittr::`%>%` 
    
    list_cryptos <-
      crypto_db %>% 
      dplyr::pull(symbol)
    
    list_cryptos <-
      ifelse(grepl('-USD', list_cryptos), list_cryptos, paste0(list_cryptos, '-USD'))
    
    return_df <-
      tidyquant::tq_get(
        list_cryptos,
        from = start_date
      ) %>% 
      dplyr::select(
        -c(close)
      ) %>% 
      dplyr::rename(
        price.open = open,
        price.high = high,
        price.low = low,
        price.adjusted = adjusted
      ) %>% 
      dplyr::mutate(
        symbol = stringr::str_remove_all(symbol, '-USD'),
        type = 'Crypto',
        country = 'Crypto'
      ) %>% 
      dplyr::group_by(symbol) %>% 
      dplyr::arrange(symbol, date) %>% 
      dplyr::mutate(
        ret.adjusted.prices = (price.adjusted / dplyr::lag(price.adjusted)) - 1
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(crypto_db, by = 'symbol') %>% 
      dplyr::select(sort(tidyselect::peek_vars()))
    
    return(return_df)
    
  }

# ---- Function to collect Commodities data from YF ----

collect_commodities_data <-
  function(commodities_db, start_date = '1990-01-01') {
    
    `%>%` <- magrittr::`%>%` 
    
    commodities_list <-
      commodities_db %>% 
      dplyr::pull(symbol)
    
    return_df <-
      tidyquant::tq_get(
        commodities_list,
        from = start_date,
        get = 'quandl'
      ) %>% 
      dplyr::select(
        -c('change', 'last', 'previous.day.open.interest',
           "block.volume", "efp.volume", "efs.volume", "prev.day.open.interest", "wave")
      ) %>% 
      dplyr::rename(
        price.open = open,
        price.high = high,
        price.low = low,
        price.adjusted = settle
      ) %>% 
      dplyr::mutate(
        type = 'Commodities'
      ) %>% 
      dplyr::group_by(symbol) %>% 
      dplyr::arrange(symbol, date) %>% 
      dplyr::mutate(
        ret.adjusted.prices = (price.adjusted / dplyr::lag(price.adjusted)) - 1
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(commodities_db, by = 'symbol') %>% 
      dplyr::select(sort(tidyselect::peek_vars()))
    
    return(return_df)
    
  }

# ---- Function to collect other assets data from YF ----

collect_other_assets_data <-
  function(other_assets_db, start_date = '1990-01-01') {
    
    `%>%` <- magrittr::`%>%` 
    
    list_other_assets <-
      other_assets_db %>% 
      dplyr::pull(symbol)
    
    return_df <-
      tidyquant::tq_get(
        list_other_assets,
        from = start_date
      ) %>% 
      dplyr::select(
        -c(close)
      ) %>% 
      dplyr::rename(
        price.open = open,
        price.high = high,
        price.low = low,
        price.adjusted = adjusted
      ) %>% 
      dplyr::mutate(
        type = ifelse(symbol %in% c("LQD", "HYG"), "Corporate Bonds", "Real Estate"),
        country = 'USA'
      ) %>% 
      dplyr::group_by(symbol) %>% 
      dplyr::arrange(symbol, date) %>% 
      dplyr::mutate(
        ret.adjusted.prices = (price.adjusted / dplyr::lag(price.adjusted)) - 1
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(other_assets_db, by = 'symbol') %>% 
      dplyr::select(sort(tidyselect::peek_vars()))
    
    return(return_df)
    
  }



get_latest_data <-
  function(full_collection = FALSE) {
    
    `%>%` <- magrittr::`%>%` 
    
    source('util_scripts/data_collection_functions.R')
    
    latest_file_update <-
      file.info('data/full_data.rds')
    
    crypto_source <- 
      readRDS('data/crypto_corresp.rds') %>% 
      dplyr::mutate_if(is.factor, as.character)
    
    tickers_source <- 
      readRDS('data/ticker_corresp.rds') %>% 
      dplyr::mutate_if(is.factor, as.character) %>% 
      dplyr::filter(
        symbol != "IPSA"
      )
    
    commodities_source <-
      readRDS('data/commodities_corresp.rds') %>% 
      dplyr::mutate_if(is.factor, as.character) %>% 
      dplyr::filter(
        symbol %in% c('CHRIS/CME_GC1',
                      'CHRIS/CME_CL1',
                      'CHRIS/ICE_B1')
      ) %>% 
      dplyr::rename(
        country = source
      )
    
    other_assets_source <- 
      readRDS('data/other_assests_corresp.rds') %>% 
      dplyr::mutate_if(is.factor, as.character)
    
    if(is.na(as.Date(latest_file_update$atime, tz = 'America/Sao_Paulo')) | full_collection) {
      
      crypto_db <- 
        collect_crypto_data(crypto_source)
      
      ticker_db <-
        collect_tickers_data(tickers_source)
      
      commodities_db <-
        collect_commodities_data(commodities_source)
      
      other_assets_db <-
        collect_other_assets_data(other_assets_source)
      
      full_data <-
        dplyr::bind_rows(
          ticker_db,
          crypto_db,
          commodities_db,
          other_assets_db
        ) %>% 
        dplyr::mutate(
          # Adjusting usd price series for assets already in USD
          price.adjusted.usd = ifelse(symbol == "GSPC", price.adjusted, price.adjusted.usd),
          price.adjusted.usd = ifelse(is.na(price.adjusted.usd) & type != "Ticker", price.adjusted, price.adjusted.usd),
          # Adjusting usd returns series for assets already in USD
          ret.adjusted.prices.usd = ifelse(symbol == "GSPC", ret.adjusted.prices, ret.adjusted.prices.usd),
          ret.adjusted.prices.usd = ifelse(is.na(ret.adjusted.prices.usd) & type != "Ticker", ret.adjusted.prices, ret.adjusted.prices.usd),
          # When currency is NA, it is USD
          currency    = ifelse(is.na(currency), 'USD', currency),
          fx.adjusted = ifelse(is.na(fx.adjusted), 1, fx.adjusted)
        )
      
      saveRDS(full_data, 'data/full_data.rds')
      
      return(full_data)
      
    } else if(as.Date(latest_file_update$atime, tz = 'America/Sao_Paulo') != Sys.Date()) {
      
      crypto_db <-
        collect_crypto_data(crypto_source, start_date = as.Date(latest_file_update$atime, tz = 'America/Sao_Paulo'))
      
      ticker_db <-
        collect_tickers_data(tickers_source, start_date = as.Date(latest_file_update$atime, tz = 'America/Sao_Paulo'))
      
      commodities_db <-
        collect_tickers_data(commodities_source, start_date = as.Date(latest_file_update$atime, tz = 'America/Sao_Paulo'))
      
      other_assets_db <-
        collect_other_assets_data(other_assets_source, start_date = as.Date(latest_file_update$atime, tz = 'America/Sao_Paulo'))
      
      new_data <-
        dplyr::bind_rows(
          crypto_db,
          ticker_db,
          commodities_db,
          other_assets_db
        )
      
      old_data <-
        readRDS('data/full_data.rds')
      
      full_data <-
        dplyr::bind_rows(
          old_data,
          new_data
        )  %>% 
        dplyr::mutate(
          price.adjusted.usd = ifelse(is.na(price.adjusted.usd), price.adjusted, price.adjusted.usd),
          ret.adjusted.prices.usd = ifelse(symbol == "GSPC", price.adjusted, ret.adjusted.prices.usd),
          ret.adjusted.prices.usd = ifelse(is.na(ret.adjusted.prices.usd) & type != "Ticker", ret.adjusted.prices, ret.adjusted.prices.usd)
        )
      
      saveRDS(full_data, 'data/full_data.rds')
      
      return(full_data)
      
      
    } else {
      
      full_data <-
        readRDS('data/full_data.rds')
      
      return(full_data)
      
    }
    
  }

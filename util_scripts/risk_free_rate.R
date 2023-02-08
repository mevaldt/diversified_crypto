risk_free_rate  <-
  function(filter_date) {
    
    `%>%` <- magrittr::`%>%`
    
    tnx_value <-
      tidyquant::tq_get('^TNX',
                        from = '1990-01-01') %>% 
      janitor::clean_names() %>% 
      tidyr::fill(adjusted) 
    
    if(as.character(filter_date) %in% tnx_value$date) {
      
      tnx_value <- tnx_value %>% 
        dplyr::filter(
          date == filter_date
        ) %>% 
        dplyr::pull(adjusted)
      
    }
    
    else {
      
      tnx_value <- tnx_value %>% 
        slice(n()) %>% 
        dplyr::pull(adjusted)
      
    }
    
    # tnx_value <- tnx_value/100
    # 
    # rfr <-
    #   ((1 + tnx_value)^(1/252)) - 1
    
    return(tnx_value)
    
  }

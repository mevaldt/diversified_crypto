remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}

generate_weight_input <-
  function(symbols, sub_db_symbol_country){
    
    symbols_by_group <- sub_db_symbol_country %>% 
      filter(symbols %in% symbols) %>% 
      select(type, symbol) %>% 
      distinct() %>% 
      ungroup() %>% 
      split(f = as.factor(.$type), drop = F) %>% 
      map("symbol") %>% 
      map(as.list)
    
    
    `%>%` <- magrittr::`%>%`
    
    unique_id <-
      stringi::stri_rand_strings(
        n = 1,
        length = 6
      ) %>% 
      tolower
    
    row_to_insert <-
      fluidRow(
        div(
          style="display:inline-block;vertical-align:top;",
          id = unique_id,
          column(
            width = 6,
            pickerInput(
              inputId  = paste0("ticker_", unique_id),
              label    = "Asset",
              choices  = symbols_by_group,
              options  = list(title = "Select One Asset")
            )
          ),
          column(
            width = 4,
            numericInput(
              inputId = paste0("weight_", unique_id),
              label   = "weight",
              min     = 0,
              max     = 1,
              value   = 0,
              step    = 0.1,
              width = '100%'
            ) 
          ),
          column(
            width = 2,
            style = 'margin-top:15px',
            actionBttn(
              inputId = paste0("remove_", unique_id),
              label = "",
              style = "material-circle", 
              color = "danger",
              icon = icon('times-circle')
            )
          )  
        )
      )
    
    return(row_to_insert)
    
  }


update_backtest_panel <- function(input, output, session, symbols, rf, sub_db_symbol_country){
  # ---- * inputs ----
  input_names <- names(input)

  buttons_available <- input_names[stringr::str_detect(input_names, "remove_")]

  input_ids <- stringr::str_remove(buttons_available, "remove_")

  #---- * update tickers ----
  
  symbols_by_group <- sub_db_symbol_country %>% 
    filter(symbols %in% symbols) %>% 
    select(type, symbol) %>% 
    distinct() %>% 
    ungroup() %>% 
    split(f = as.factor(.$type), drop = F) %>% 
    map("symbol") %>% 
    map(as.list)
  
  walk(paste0("ticker_", input_ids), ~updatePickerInput(session,
                                                        inputId = .x,
                                                        choices = symbols_by_group))

  walk(paste0("weight_", input_ids), ~updateNumericInput(session,
                                                         inputId = .x,
                                                         value = 0))
  #---- * update risk free  ----
  updatePickerInput(session,
                    inputId = 'risk_free_choose',
                    choices = c(unique(rf), 'Numeric'),
                    selected = unique(rf))
  
  #---- * update currency  ----
  # updatePickerInput(session,
  #                   inputId = 'currency',
  #                   choices  = c("Local", "USD"),
  #                   selected = 'Local')
  # 
  #---- * benchmark ----
  updatePickerInput(session,
    inputId  = 'benchmark',
    label    = "Benchmark",
    choices  = unique(symbols)
  )
  
  #---- plot + dt----
  
  output[["performance_plot"]] <- NULL
  
  output[["individual_metrics_table"]] <- NULL
  

}

update_currency <- function(input, output, session, symbols, sub_db_symbol_country){
  # ---- * inputs ----
  input_names <- names(input)
  
  buttons_available <- input_names[stringr::str_detect(input_names, "remove_")]
  
  input_ids <- stringr::str_remove(buttons_available, "remove_")
  
  #---- * update tickers ----
  
  symbols_by_group <- sub_db_symbol_country %>% 
    filter(symbols %in% symbols) %>% 
    select(type, symbol) %>% 
    distinct() %>% 
    ungroup() %>% 
    split(f = as.factor(.$type), drop = F) %>% 
    map("symbol") %>% 
    map(as.list)
  
  walk(paste0("ticker_", input_ids), ~updatePickerInput(session,
                                                        inputId = .x,
                                                        choices = symbols_by_group))
  
  walk(paste0("weight_", input_ids), ~updateNumericInput(session,
                                                         inputId = .x,
                                                         value = 0))
}


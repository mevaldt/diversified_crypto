output$check <- reactive({
  length(input$asset_names)
})

outputOptions(output, 'check', suspendWhenHidden=FALSE)

### Plotly Time Options ----

ts_options <-
  # reactive({
  function(asset_names) {
    
    d <- 
      event_data(
        "plotly_relayout", 
        source = 'ts_subset'
      )
    
    # asset_names <- input$asset_names
    
    data_sub <- 
      full_data %>%
      filter(
        name %in% asset_names
      ) %>% 
      select(name, date)
    
    if(input$asset_names_pairwise) {
      
      date_filter <-
        data_sub %>% 
        tidyr::pivot_wider(names_from = name,
                           values_from = 1) %>% 
        na.omit() %>% 
        dplyr::pull(date)
      
      data_sub <-
        data_sub %>% 
        filter(date %in% date_filter)
      
    }
    
    if (!is.null(d)) {
      
      if(!is.na(unlist(d)[1]) & !is.na(unlist(d)[2]) &
         !is.null(unlist(d)[1]) & !is.null(unlist(d)[2])){
        
        start <- as.Date(sub(" .*", "", as.character(unlist(d)[1]))) # Sometimes the Date might come "2019-01-01 23:43:19.2278" (we need the elements before the first space)
        end <- as.Date(sub(" .*", "", as.character(unlist(d)[2])))
        
      } else {
        
        start <- min(data_sub$date)
        end <- max(data_sub$date)
        
      }
      
    }
    else {
      
      start <- min(data_sub$date)
      end <- max(data_sub$date)
      
    }
    
    return(list(start = start, end = end))
    
    # })
  }

### Time Series -----

ts_reactive <-
  reactive({
    asset_names <- input$asset_names
    variable <- input$variable
    
    aux <- 
      full_data %>%
      filter(
        name %in% asset_names
      ) %>%
      rename(variable = variable)
    
    if(input$asset_names_pairwise) {
      
      date_filter <-
        aux %>% 
        select(name, date) %>% 
        tidyr::pivot_wider(names_from = name,
                           values_from = 1) %>% 
        na.omit() %>% 
        dplyr::pull(date)
      
      aux <-
        aux %>% 
        filter(date %in% date_filter)
      
    }
    
    aux %>%
      rename(Date = date) %>% 
      plot_ly(
        x = ~Date, 
        y = ~variable,
        type = 'scatter', 
        mode = 'lines', 
        color = ~name,
        #opacity = 0.65,
        source = 'ts_subset'
      ) %>%
      layout(
        #xaxis = list(title = "Date"), # This crashes the plotly connection and bottom slider
        yaxis = list(title = names(numeric_variables_names)[which(numeric_variables_names == variable)]),
        hovermode = 'compare', # Compare data as default
        title = paste0("<br><br>Time Series of ", names(numeric_variables_names)[which(numeric_variables_names == variable)]),
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(
                count = 3,
                label = "3 mo",
                step = "month",
                stepmode = "backward"
              ),
              list(
                count = 6,
                label = "6 mo",
                step = "month",
                stepmode = "backward"
              ),
              list(
                count = 1,
                label = "1 yr",
                step = "year",
                stepmode = "backward"
              ),
              list(
                count = 1,
                label = "Current Year",
                step = "year",
                stepmode = "todate"),
              list(step = "all")
            )
          ),
          rangeslider = list(
            type = "date"
          )
        )
      ) %>%
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

output$assets_time_series <- renderPlotly({
  
  ts_reactive()
  
})

### Heatmap ----

hmap_reactive <-
  # reactive({
  function(asset_names){
    
    # asset_names <- input$asset_names
    
    if(length(asset_names) == 1)  {
      return(NULL)
    }
    
    variable <- input$variable
    cor_type <- input$cor_type
    
    dates_ <- ts_options(asset_names)
    
    aux <- 
      full_data %>% 
      filter(
        name %in% asset_names,
        date >= dates_[['start']],
        date <= dates_[['end']]
      ) %>%
      rename(variable = variable) %>%
      select(
        date, 
        name, 
        variable
      ) %>%
      tidyr::pivot_wider(
        names_from  = 'name',
        values_from = 'variable' 
      ) %>% 
      # spread(name, variable) %>% # Consider `pivot_` like in the future (https://twitter.com/sharon000/status/1107771331012108288)
      select(-date) %>%
      cor(use = "pairwise.complete.obs", method = cor_type)
    
    title_string <- paste0('Cor. from ', format(dates_[['start']], "%b, %d of %Y"), ' to ', format(dates_[['end']], "%b, %d of %Y"))
    
    if (!input$checkbox_dendrogram) { dedrogram_status = 'none' }
    if (input$checkbox_dendrogram)  { dedrogram_status = 'both' }
    
    # It was necessary to declare two times the font size due to the masking issues with the plotly wrapper.
    f <- 
      list(size = 5, color = "black")
    
    hmap <-
      heatmaply(
        aux, 
        main = title_string, 
        dendrogram = dedrogram_status,
        hclust_method = "complete",
        titlefont = list(size = 5, color = "black"),
        label_names = c("Row", "Column", "Value"),
        margins = c(100, 40),
        scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "red", high = "#023A78", midpoint = 0)
      ) %>%
      layout(
        title = title_string,
        font=t
      ) %>%
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
    
    return(hmap)
    
    # })
  }

output$ts_assets_heatmap <- renderPlotly({
  
  an <- input$asset_names
  
  hmap_reactive(an)
  
})

output$assets_heatmap2 <- renderPlotly({

  an <- input$asset_names

  hmap_reactive(an)

})

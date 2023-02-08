# ---- plot to build density and histograms ----

plot1 <- reactive({
  
  inputs <- input$asset_names_ds
  
  serie <-
    full_data %>% 
    dplyr::filter(
      name %in% inputs &
        date >= input$stats_plots_dates[1] &
        date <= input$stats_plots_dates[2]
    )
  
  if(input$stats_plots_pairwise) {
    
    date_filter <-
      serie %>% 
      select(name, date) %>% 
      tidyr::pivot_wider(names_from = name,
                         values_from = 1) %>% 
      na.omit() %>% 
      dplyr::pull(date)
    
    serie <-
      serie %>% 
      filter(date %in% date_filter)
    
  }
  
  serie <-
    serie %>% 
    select(name, ret.adjusted.prices) %>% 
    na.omit() %>% 
    split(.$name) %>% 
    purrr::map_df(
      .f = function(df) {
        
        df %>% 
          summarise(
            serie = list(.$ret.adjusted.prices),
            density = list(density(.$ret.adjusted.prices))
          )
        
      },
      .id = 'name'
    )
  
  if(input$graph_type == "Density") {
    
    p <- plot_ly(alpha = 0.6)
    
    purrr::walk(
      serie %>% 
        split(.$name),
      function(df_series) {
        
        p <<- 
          add_trace(
            p,
            x = df_series$density[[1]]$x,
            y = df_series$density[[1]]$y,
            type = 'scatter',
            mode = 'lines', 
            fill = 'tozeroy',
            name =  df_series$name[[1]], 
            alpha = 0.6
          )
        
      }
    )
    
    p %>% 
      layout(
        yaxis2 = list(overlaying = 'y', side = 'right'),
        barmode = 'overlay'
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
    
  } else {
    
    p <- plot_ly(alpha = 0.6)
    
    purrr::walk(
      serie %>% 
        split(.$name),
      function(df_series) {
        
        p <<- 
          add_histogram(
            p,
            x = df_series$serie[[1]],
            name =  df_series$name[[1]], 
            histnorm = 'probability',
            alpha = 0.6
          )
        
      }
    )
    
    p %>% 
      layout(
        yaxis2 = list(overlaying = 'y', side = 'right'),
        barmode = 'overlay'
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
    
  }
  
}) 


output$hist_assets <- renderPlotly({
  plot1()
})

# ---- building stats table ----

reac_table_stats <- reactive({
  
  tbl_subset <-
    full_data %>% 
    dplyr::filter(
      date >= input$stats_table_dates[1] &
        date <= input$stats_table_dates[2] &
        name %in% input$asset_names_tb
    ) %>% 
    na.omit()
  
  if(input$stats_table_pairwise) {
    
    date_filter <-
      tbl_subset %>% 
      select(name, date) %>% 
      tidyr::pivot_wider(names_from = name,
                         values_from = 1) %>% 
      na.omit() %>% 
      dplyr::pull(date)
    
    tbl_subset <-
      tbl_subset %>% 
      filter(date %in% date_filter)
    
  }
  
  tbl_subset <-
    tbl_subset %>% 
    group_by(name) %>% 
    summarize(
      Mean = mean(ret.adjusted.prices, na.rm = TRUE),
      SD = sd(ret.adjusted.prices, na.rm = TRUE),
      Sharpe_Ratio = mean(ret.adjusted.prices - 0.0000684171196396743, na.rm = TRUE)/sd(ret.adjusted.prices, na.rm = TRUE),
      Median = median(ret.adjusted.prices, na.rm = TRUE),
      Lower_tail = quantile(ret.adjusted.prices, 0.01, na.rm = TRUE),
      Upper_tail = quantile(ret.adjusted.prices, 0.99, na.rm = TRUE),
      Number_of_Obs = n()
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
      'Upper Tail (>99%)',
      'Number of Observations'
    )
  
  return(tbl_subset)
  
})


output$tab_assets <- 
  DT::renderDT({
    reac_table_stats()
  },
  options = list(pageLength = 1000,
                 dom = 't',
                 scrollX = TRUE),
  rownames = FALSE
  )

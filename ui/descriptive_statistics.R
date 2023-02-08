descriptive_statistics <-
  tabPanel(
    "Descriptive Statistics", 
    icon = icon("chart-bar"),  
    value = "descriptive",
    tabsetPanel(
      tabPanel(
        id = "hists",
        title = "Histogram",
        column(
          width = 3,
          h3("Returns"),
          p("In the histogram to the side you can check the distribution of the asset's returns"),
          pickerInput(
            "asset_names_ds",
            "Choose the assets to be used on the plot:",
            choices = choices_hierarchical_list,
            choicesOpt = list(
              subtext = lst_names %>% 
                pull(symbol)
            ),
            selected = c('Bitcoin', 'XRP'),
            options = list(`actions-box` = TRUE),
            multiple = T
          ),
          pickerInput(
            inputId = 'graph_type', 
            label = 'Type of the Plot:', 
            choices = c('Density', 'Histogram'),
            multiple = F,
            selected = 'Density'
          ),
          dateRangeInput(
            'stats_plots_dates', 
            'Choose the time range: ', 
            start = min(full_data$date), 
            end = (max(full_data$date) - 1), 
            min = min(full_data$date),
            max = (max(full_data$date) - 1), 
            format = "yyyy-mm-dd", 
            startview = "day"
          ),
          awesomeCheckbox(
            inputId = 'stats_plots_pairwise',
            label   = 'Force Listwise deletion'%>%label.help("lbl_force_listwise_descriptives_plot"),
            value   = FALSE,
            status  = 'primary'
          ),
          bsTooltip(
            id = "lbl_force_listwise_descriptives_plot",
            title = "Listwise deletion (also known as complete-case analysis) discards the data for any year-day that has one or more missing values for the selected set of assets. The default is pairwise deletion: it uses all available data for each time series.",
            placement = "right",
            trigger = "hover",
            options = list(container = "body")
          )
        ),
        column(
          width = 9,
          br(),
          plotlyOutput(
            "hist_assets", 
            height = "600px", 
            width = "100%"
          ) %>% 
            withSpinner(type = 6, color = '#023A78')
        )
      ),
      tabPanel(
        id = 'stats_table_tab',
        title = "Stats",
        column(
          width = 3,
          h3("Statistical Info"),
          p("In this table you can find the assets's mean, standard deviation, etc"),
          pickerInput(
            "asset_names_tb",
            "Choose the assets to be used:",
            choices = choices_hierarchical_list,
            choicesOpt = list(
              subtext = lst_names %>% 
                pull(symbol)
            ),
            selected = c('Bitcoin', 'XRP', 'S&P 500', 'Ibovespa'),
            options = list(`actions-box` = TRUE),
            multiple = T
          ),
          dateRangeInput(
            'stats_table_dates', 
            'Choose the time range: ', 
            start = min(full_data$date), 
            end = (max(full_data$date) - 1), 
            min = min(full_data$date),
            max = (max(full_data$date) - 1), 
            format = "yyyy-mm-dd", 
            startview = "day"
          ),
          awesomeCheckbox(
            inputId = 'stats_table_pairwise',
            label   = 'Force Listwise deletion'%>%label.help("lbl_force_listwise_descriptives_table"),
            value   = FALSE,
            status  = 'primary'
          ),
          bsTooltip(
            id = "lbl_force_listwise_descriptives_table",
            title = "Listwise deletion (also known as complete-case analysis) discards the data for any year-day that has one or more missing values for the selected set of assets. The default is pairwise deletion: it uses all available data for each time series.",
            placement = "right",
            trigger = "hover",
            options = list(container = "body")
          )
        ),
        column(
          width = 9,
          br(),
          DT::DTOutput("tab_assets") %>% 
            withSpinner(type = 6, color = '#023A78')
        )
      )          
    )
  )

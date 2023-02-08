time_series <- 
  tabPanel(
    title = " Time Series and Correlations", 
    icon = icon("line-chart"), 
    value = "time_series",
    fluidRow(
      column(
        width = 4,
        pickerInput(
          inputId = "asset_names",
          "Choose the assets to be plotted:",
          choices = choices_hierarchical_list,
          choicesOpt = list(
            subtext = lst_names %>% 
              pull(symbol)
          ),
          selected = c('Bitcoin', 'XRP', 'Ibovespa','S&P 500'),
          # selected = full_data %>% pull(name) %>% unique(),
          options = list(`actions-box` = TRUE),
          multiple = T
        ),
        awesomeCheckbox(
          inputId = 'asset_names_pairwise',
          label   = 'Force Listwise deletion'%>%label.help("lbl_force_listwise_ts"),
          value   = FALSE,
          status  = 'primary'
        ),
        bsTooltip(
          id = "lbl_force_listwise_ts",
          title = "Listwise deletion (also known as complete-case analysis) discards the data for any year-day that has one or more missing values for the selected set of assets. The default is pairwise deletion: it uses all available data for each time series.",
          placement = "right",
          trigger = "hover",
          options = list(container = "body")
        )
      ),
      column(
        width = 4,
        pickerInput(
          'variable', 
          'Choose the variable of the analysis:', 
          choices = numeric_variables_names,
          #selected = "ret.adjusted.prices_currency",
          selected = "ret.adjusted.prices",
          multiple = F
        )
      ),
      column(
        width = 4,
        conditionalPanel(
          'output.check > 1',
          pickerInput(
            inputId = 'cor_type', 
            label = 'Correlation Method:',
            choices = c("Pearson" = "pearson", 
                        "Spearman" = "spearman", 
                        "Kendall" = "kendall"),
            selected = "pearson",
            multiple = F
          ),
          awesomeCheckbox(
            "checkbox_dendrogram", 
            label = "Include dendrogram", 
            value = FALSE,
            status = "primary"
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 8,
        plotlyOutput(
          "assets_time_series", 
          height = "700px"
        ) %>% 
          withSpinner(type = 6, color = '#023A78'), 
        align = "center"
      ), 
      column(
        width = 4,
        plotlyOutput(
          "ts_assets_heatmap", 
          height = "500px"
        ) %>% 
          withSpinner(type = 6, color = '#023A78'),
        actionButton(
          'modal_heatmap',
          label = 'View Fullscreen',
          icon = icon('arrows-alt')
        ),
        bsModal(
          "bs_modal_heatmap",
          "",
          "modal_heatmap",
          size = "large",
          plotlyOutput("assets_heatmap2",
                       height = "700px") %>%
            withSpinner(type = 6, color = '#023A78')
        ),
        align = "center"
        # , height = "500px", width = "85%"
      ) 
    )
  )
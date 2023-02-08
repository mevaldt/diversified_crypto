### Function to add tooltip to awesomecheckbox

label.help <- function(label,id){
  shiny::HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}

portfolio_analysis <- 
  tabPanel(
    title = "Portfolio Analysis", 
    icon = icon("briefcase"), 
    value = "portfolio",
    sidebarLayout(
      sidebarPanel(
        width = 3, # Number of columns
        h3("Portfolio Analysis"),
        p(HTML("After selecting a set of candidate assets, a time range, and a risk-free rate, click on <strong> Run Optimization </strong> to draw the efficient frontier and the tangent and minimum variance portfolios.")),
        pickerInput(
          "asset_names_portfolio",
          "Choose the assets to be used:",
          choices = choices_hierarchical_list,
          choicesOpt = list(
            subtext = lst_names %>% 
              pull(symbol)
          ),
          # selected = all_crypto_markers_name,
          options = list(`actions-box` = TRUE),
          multiple = T
        ),
        dateRangeInput(
          'date_range_portfolio', 
          'Choose the time range: ', 
          start = min(full_data$date), 
          end = (max(full_data$date) - 1), 
          min = min(full_data$date),
          max = (max(full_data$date) - 1), 
          format = "yyyy-mm-dd", 
          startview = "day"
        ),
        bsTooltip("date_range_portfolio", 
                  title = "The date range used for the optimization will only span where all data are available for all assets chosen.", 
                  placement = "right", 
                  trigger = "hover",
                  options = list(container = "body")),
        numericInput(
          inputId = 'RiskFreeRate', 
          label =  'Yearly Risk Free Rate (%):',
          value = risk_free_rate(max(full_data$date) - 1)
        ),
        bsTooltip(
          "RiskFreeRate",
          "The default value of the Risk-Free Rate is the yield of the 10 Year U.S. Treasury Bond (^TNX).",
          "right",
          options = list(container = "body")
        ),
        awesomeCheckbox(
          'portfolio_dolarized_returns',
          label = 'Use returns in US$',
          value = TRUE,
          status = 'primary'
        ),
        awesomeCheckbox(
          'checkbox_upper_constraint_crypto',
          label = 'Specify maximum value for Crypto weights sum',
          value = FALSE,
          status = 'primary'
        ),
        conditionalPanel(
          condition = 'input.checkbox_upper_constraint_crypto',
          numericInput('max_upper_constraint_crypto', 
                       'Threshold (%):', 
                       value = 5, 
                       min = 0, 
                       max = 100)
        ),
        actionButton(
          'PortfolioActionButton', 
          'Run Optimization!', 
          icon = icon("hand-holding-usd"),
          width = '100%'
        ),
        hr(),
        downloadButton("downloadData", label = "Download Optimization Data", style = "width:100%;"),
      ),
      # Show the plots
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel(
            id = "tab_portfolio_weights",
            title = "Portfolio Weights",
            column(
              width = 8,
              br(),
              plotlyOutput(
                "portfolio_plot", 
                height = "700px", 
                width = "100%"
              ) %>% 
                withSpinner(type = 6, color = '#023A78'), 
              align = "center"
            ),
            column(
              width = 4,
              br(),
              plotlyOutput(
                "tangency_weights_pie_plot", 
                height = "350px", 
                width = "100%"
              ) %>% 
                withSpinner(type = 6, color = '#023A78'), 
              plotlyOutput(
                "minimum_variance_weights_pie_plot", 
                height = "350px", 
                width = "100%"
              ) %>% 
                withSpinner(type = 6, color = '#023A78'), 
              align = "center"
            )
          ),
          tabPanel(
            id = 'portfolio_performance',
            title = 'Backtest',
            fluidRow(
              column(
                width = 4,
                pickerInput(
                  "benchmark_performance",
                  "Choose the performance benchmark:", 
                  choices = choices_hierarchical_list,
                  choicesOpt = list(
                    subtext = lst_names %>% 
                      pull(symbol)
                  ),
                  selected = c('Ibovespa'),
                  options = list(`actions-box` = TRUE),
                  multiple = F
                )
              ),
              column(
                width = 4,
                br(),
                awesomeCheckbox(
                  inputId = 'naive_portfolio',
                  label   = 'Include Naive Portfolio'%>%label.help("lbl_naive"),
                  value   = FALSE,
                  status  = 'primary'
                )
              ),
              bsTooltip(
                id = "lbl_naive",
                title = "The Naive Portfolio represents homogeneous shares of investment (i.e. all shares equal to 1/N, where N is the number of assets chosen).",
                placement = "right",
                trigger = "hover",
                options = list(container = "body")
              ),
              column(
                width = 4,
                br(),
                awesomeCheckbox(
                  inputId = 'rebalance_portfolio',
                  label   = 'Rebalance Portfolio'%>%label.help("lbl_rebal"),
                  value   = FALSE,
                  status  = 'primary'
                ),
				bsTooltip(
                  id = "lbl_rebal",
                  title = "Rebalancing your portfolio allows you to maintain a desired asset allocation over time. Otherwise, the composition of the portfolio changes because of differences in cumulative returns.",
                  placement = "right",
                  trigger = "hover",
                  options = list(container = "body")
                ),
                conditionalPanel(
                  condition = 'input.rebalance_portfolio',
                  pickerInput(
                    "rebalance_window",
                    "Choose the rebalance window:", 
                    choices = c("Annualy" = "years", 
                                "Quarterly" = "quarters", 
                                "Monthly" = "months", 
                                "Weekly" = "weeks", 
                                "Daily" = "days")
                  )
                )
              )
            ),
            plotlyOutput(
              "performance_time_series", 
              height = "550px"
            ) %>% 
              withSpinner(type = 6, color = '#023A78'),
            DT::DTOutput("benchmark_descriptive_table") %>% 
              withSpinner(type = 6, color = '#023A78')
          )
          
        )
      )
    )
  )
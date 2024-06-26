backtest <- 
  tabPanel(
    title = "Backtest", 
    icon = icon("coins"), 
    value = "backtest",
    fluidRow(
      column(
        width = 5,
        uiOutput("symbol_input_list")
      ),
      column(
        width = 3,
        dateRangeInput(inputId   = "daterange",
                       label     = "Time Series Interval:",
                       start     = min(db_backtest$date),
                       end       = max(db_backtest$date),
                       min       = min(db_backtest$date),
                       max       = max(db_backtest$date),
                       separator = " - "),
        tags$br(),
        div(
          prettyRadioButtons(inputId      = "rebalance",
                             label        = "Rebalance Schedule:", 
                             choiceValues = c('none', 'years', 'months', 'weeks', 'days'),
                             choiceNames  = c('None', 'Yearly', 'Monthly', 'Weekly', 'Daily'),
                             icon         = icon("check"), 
                             bigger       = TRUE,
                             status       = "info",
                             animation    = "jelly"
        ),
        class = "custom-radio-buttons")
      ),
      column(
        width = 3,
        uiOutput("benchmark_ui"),
        tags$br(),
        div(
          prettyRadioButtons(inputId      = "period_return",
                             label        = "Returns:", 
                             choiceValues = c('daily', 'monthly', 'quarterly', 'yearly'),
                             choiceNames  = c('Daily', 'Monthly', 'Quarterly', 'Yearly'),
                             icon         = icon("check"), 
                             bigger       = TRUE,
                             status       = "info",
                             animation    = "jelly"
          ),
          class = "custom-radio-buttons"),
      ),
      column(width = 1,
             uiOutput('risk_free_ui'),
             conditionalPanel(
               condition = "(input.risk_free_choose == 'Numeric')",
               numericInput(inputId = "risk_free_numeric",
                            label   = "Rf %",
                            value   = 0,
                            min     = 0,
                            max     = 100)
             )#,
             # pickerInput(
             #   inputId  = 'currency',
             #   label    = "Currency",
             #   choices  = c("Local", "USD"),
             #   selected = 'Local'
             # )
      ),
      fluidRow(
        column(12,
               div(actionBttn(inputId = "gobacktest",
                              label   = "Backtest",
                              style   = "jelly", 
                              color   = "default"
               ),
               br(),
               br(),
               align = 'center'
               )
        )
      ),
      fluidRow(
        tabsetPanel(id = 'tab_backtest',
                    tabPanel("Performance",
                             column(6,
                                    plotlyOutput("performance_plot")
                             ),
                             column(6,
                                    dataTableOutput("individual_metrics_table")
                             )
                    ),
                    tabPanel("Risk",
                             column(6,
                                    plotlyOutput("drawdown_plot")
                             ),
                             column(6,
                                    tags$br(),
                                    align = "center",
                                    dropdownButton(
                                      tags$h3("Pick"),
                                      numericInput(inputId = "window_sd",
                                                   label   = "Window Rolling Sd",
                                                   value   = 24,
                                                   min     = 1),
                                      circle = TRUE,
                                      status = "danger",
                                      icon = icon("align-justify"), 
                                      tooltip = tooltipOptions(title = "Click to change rolling-window")
                                    ),
                                    plotlyOutput("roll_sd_plot")
                             )
                    )
        )
      )
    )
  )
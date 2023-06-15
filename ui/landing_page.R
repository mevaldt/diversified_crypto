landing_page <-
  tabPanel(
    title = " Home", 
    icon = icon("home"),
    value = "home",
    mainPanel(
      width = 11, 
      style = "margin-left:4%; margin-right:4%",
      fluidRow(
        h3(
          "Welcome to the Crypto Diversifier app!",
          style = "margin-top:0px;"
        )
      ),
      fluidRow(
        h4(
          "Explore and analyze data on cryptocurrencies and other asset classes. For example, you may visualize the co-movements, draw the efficient frontier, and estimate optimal portfolios considering both cryptocurrencies and traditional assets.", 
          style = "margin-top:0px;"
        )
      ),
      fluidRow(
        #Time Series and Correlations box
        column(
          width = 4, 
          class = "landing-page-column",
          div(
            class = "landing-page-box", 
            div(
              "Time Series and Correlations", 
              class = "landing-page-box-title"
            ),
            div(
              class = "landing-page-icon", 
              style = "background-image: url('imgs/multiple_ts_and_heatmap.png');
                       background-size: auto 80%; 
                       background-position: center; 
                       background-repeat: no-repeat;"
            ),
            actionButton(
              'jump_to_time_series', 
              'Explore time series and correlations among cryptocurrencies and other financial assets', 
              class = "landing-page-button", 
              icon = icon("arrow-circle-right", "icon-lp")
            )
          )
        ),
        #Descriptive box
        column(
          width = 4, 
          class = "landing-page-column",
          div(
            class = "landing-page-box", 
            div(
              "Descriptive Statistics", 
              class = "landing-page-box-title"
            ),
            div(class = "landing-page-icon", 
                style = "background-image: url('imgs/descriptive_new.png');
                       background-size: auto 80%; 
                       background-position: center; 
                       background-repeat: no-repeat;"
            ),
            actionButton(
              'jump_to_descriptive', 
              'Check descriptive statistics of cryptocurrencies and other financial assets',
              class = "landing-page-button", 
              icon = icon("arrow-circle-right", "icon-lp")
            )
          )
        ),
        #Portfolio Analysis box
        column(
          width = 4, 
          class = "landing-page-column",
          div(
            class = "landing-page-box",
            div(
              "Portfolio Analysis", 
              class = "landing-page-box-title"
            ),
            div(
              class = "landing-page-icon", 
              style = "background-image: url('imgs/portfolio_new.png');
                     background-size: auto 80%; 
                     background-position: center; 
                     background-repeat: no-repeat;"
            ),
            actionButton(
              'jump_to_portfolio', 
              'Explore how you can estimate your portfolio with optimizations', 
              class = "landing-page-button", 
              icon = icon("arrow-circle-right", "icon-lp")
            )
          )
        )
      ),
      #fluidRow(h4("Explore more options below...")),
      fluidRow(
        # #Co-movements tracer box
        # column(
        #   width = 4, 
        #   class = "landing-page-column",
        #   div(
        #     class = "landing-page-box", 
        #     div(
        #       "Co-movements tracer", 
        #       class = "landing-page-box-title"
        #     ),
        #     div(class = "landing-page-icon", 
        #         style = "background-image: url('imgs/co_mov_tracer.png');
        #         background-size: auto 80%; 
        #         background-position: center; 
        #         background-repeat: no-repeat;"
        #     ),
        #     actionButton(
        #       'jump_to_co_mov_tracer', 
        #       'Explore the co-movements of cryptos and tickers',
        #       class = "landing-page-button", 
        #       icon = icon("arrow-circle-right", "icon-lp")
        #     )
        #     )
        # ),
        #Backtest box
        column(
          width = 4, 
          class = "landing-page-column",
          div(
            class = "landing-page-box", 
            div(
              "Backtest", 
              class = "landing-page-box-title"
            ),
            div(class = "landing-page-icon", 
                style = "background-image: url('imgs/backtest.png');
                  background-size: auto 80%; 
                  background-position: center; 
                  background-repeat: no-repeat;"
            ),
            actionButton(
              'jump_to_backtest', 
              'Explore how a custom portifolio with cryptocurrencies would\'ve performed in the past',
              class = "landing-page-button", 
              icon = icon("arrow-circle-right", "icon-lp")
            )
          )
        ),
        # Download user Guide
        column(
          width = 4, 
          class = "landing-page-column",
          div(
            class = "landing-page-box", 
            div(
              "User Guide", 
              class = "landing-page-box-title"
            ),
            div(class = "landing-page-icon", 
                style = "background-image: url('imgs/user_guide.png');
                  background-size: auto 80%; 
                  background-position: center; 
                  background-repeat: no-repeat;"
            ),
            actionButton(
              'download_activator', 
              'Download the app user guide',
              class = "landing-page-button",
              icon = icon("arrow-circle-right", "icon-lp")
            ),
            downloadButton(
              'download_user_guide',
              'Download',
              style = 'visibility: hidden;'
            )
          )
        ),
        #About box
        column(
          width = 4, 
          class = "landing-page-column",
          div(
            class = "landing-page-box", 
            div(
              "About Us", 
              class = "landing-page-box-title"
            ),
            div(class = "landing-page-icon", 
                style = "background-image: url('imgs/about.png');
                  background-size: auto 80%; 
                  background-position: center; 
                  background-repeat: no-repeat;"
            ),
            actionButton(
              'jump_to_about', 
              'About the project and its developers',
              class = "landing-page-button", 
              icon = icon("arrow-circle-right", "icon-lp")
            )
          )
        )
      )
    )
  )

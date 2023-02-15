# Define UI 
shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(includeHTML('www/google_analytics.html')),
    navbarPage(
      id = "intabset", #needed for landing page
      title = 
        div(
          tags$a(
            img(
              src = "imgs/fgv_eesp_logo_eng.png", 
              height = 25
            ), 
            href= "empty_url"
          ),
          style = "position: relative; top: -5px;"
        ), # Navigation bar
      windowTitle = "Crypto Diversifier", # Browser Tab Title
      theme = shinytheme("readable"), # Theme of the app
      collapsible = TRUE, #tab panels collapse into menu in small screens
      header =         
        tags$head( #CSS styles
          #cookie_box, ##Cookie box
          
          tags$link(rel = 'stylesheet', href = "https://fonts.googleapis.com/css?family=Montserrat:400,700&display=swap"),
          
          tags$link(rel="shortcut icon", href="imgs/xrp_badge.ico"), #Icon for browser tab
          
          #Including Google analytics and Cookie control
          #includeScript("google-analytics.js"),
          # HTML('<script src="https://cc.cdn.civiccomputing.com/8/cookieControl-8.x.min.js"></script>'),
          # includeScript("cookie-control.js"),
          
          #  Styling panels properly
          
          tags$link(rel = "stylesheet", type = "text/css", href = "scripts/styles.css"),
          tags$head(tags$style(".modal-dialog{ width:80%}")),
          tags$head(tags$style(".modal-body{ min-height:700px}")),
          HTML("<base target='_blank'>")
        ),
      
      ###############################################.
      ## Landing page ----
      ###############################################.
      landing_page,
      
      ###############################################.
      ## Time Series ----
      ###############################################.
      
      time_series,
      
      ###############################################.
      ## Descriptive Statistics ----
      ###############################################.
      
      descriptive_statistics,
      
      ###############################################.
      ## Portfolio Analysis ----
      ###############################################.
      
      portfolio_analysis,
      
      # ###############################################.
      # ## Co-movements tracer ----
      # ###############################################.
      # 
      # co_movements,
      
      # ###############################################.
      # ## Backtest ----
      # ###############################################.
      
      backtest,
      
      ###############################################.
      ## About ----
      ###############################################.
      
      about
      
    ),
    ###############################################.
    ## Footer ----
    ###############################################.
    div(class = "footer",
        includeHTML("www/footer.html")#,
        #div(includeHTML("html/google_analytics.html"))
    )
  ))

# Define server logic 

# Plotly event link 1: https://plot.ly/r/shinyapp-plotly-events/
# Plotly event link 2: https://plot.ly/r/shiny-coupled-events/

shinyServer(function(input, output, session) {
  
  ###############################################.
  ## Landing page ----
  ###############################################.
  # Creating events that take you to different tabs
  # activated when pressing buttons from the landing page
  observeEvent(input$jump_to_time_series, {
    updateTabsetPanel(session, "intabset", selected = "time_series")
  })
  
  observeEvent(input$jump_to_portfolio, {
    updateTabsetPanel(session, "intabset", selected = "portfolio")
  })
  
  observeEvent(input$jump_to_about, {
    updateTabsetPanel(session, "intabset", selected = "about")
  })
  
  observeEvent(input$jump_to_descriptive, {
    updateTabsetPanel(session, "intabset", selected = "descriptive")
  })
  
  observeEvent(input$jump_to_co_mov_tracer, {
    updateTabsetPanel(session, "intabset", selected = "co_mov_tracer")
  })
  
  observeEvent(input$download_activator, {
    shinyjs::runjs("document.getElementById('download_user_guide').click();")
  })
  
  output$download_user_guide <-
    downloadHandler(
      filename = 'crypto_diversifier_user_guide.pdf',
      content = function(file) {
        
        file.copy('www/crypto_diversifier_user_guide.pdf', file)
        
      }
    )
  
  source('servers/landing_page.R', local = TRUE)
  source('servers/time_series.R', local = TRUE)
  source('servers/portfolios.R', local = TRUE)
  source('servers/descriptives.R', local = TRUE)
  
  
})

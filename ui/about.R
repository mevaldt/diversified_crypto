about <-
  tabPanel(
    "About us", 
    icon = icon("info-circle"),  
    value = "about",
    column(
      width = 1
    ),
    column(
      width = 10,
      # h4(
      #   "About", 
      #   style = "color:black;"
      # ),
      br(),
      hr(),
      includeHTML(rmarkdown::render('descriptions/jefferson.Rmd')), br(),
      
      includeHTML(rmarkdown::render('descriptions/fernando.Rmd')), br(),
      
      includeHTML(rmarkdown::render('descriptions/renan.Rmd')), br(),
      
      includeHTML(rmarkdown::render('descriptions/paese.Rmd')), br(),
      
      includeHTML(rmarkdown::render('descriptions/matheus.Rmd')), br()
      
    )
  )
# ---- Send sweet alert to landing page ----

observeEvent(input$intabset,{
  
  iframe_txt <-
    HTML('<p>The objective of the app is to assist the user in the decision-making process regarding asset allocation. 
          However, it does not constitute, under whichever circumstances, any investment recommendation or advice on our part.</p>
          <br>
          <p>Any information or analysis provided in this app comes from the authors’ alone and do not represent the view or 
          has the consent of Ripple or FGV EESP. The accuracy, completeness, and validity of any data or analysis within 
          this app are not guaranteed. We accept no liability for any errors, omissions, or representations.</p>
          <br>
          <iframe height="200px" src="https://www.youtube.com/embed/ldmQoOMC3pc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  
  
  if(input$intabset == "home"){
    sendSweetAlert(
      session = session,
      title = "Disclaimer and tutorial",
      text = iframe_txt,
      type = "info", 
      html = TRUE,
      closeOnClickOutside = FALSE,
      width = "60%"
    )
  }
},
once = TRUE)

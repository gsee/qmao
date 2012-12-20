shinyServer(function(input, output) {
  output$plot_symbol <- reactivePlot(function() { 
    plot(getQuote(toupper(input$Symbol), src='BATS', what=input$what, 
         exch=input$exch))
    invalidateLater(1000)
  })
})
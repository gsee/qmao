shinyServer(function(input, output) {
  output$plot_symbol <- reactivePlot(function() { 
    plot(qmao::getQuote.BATS(toupper(input$Symbol), what=input$what, 
         exch=input$exch))
    invalidateLater(1000)
  })
})

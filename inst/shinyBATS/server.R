shinyServer(function(input, output, session) {
  output$plot_symbol <- reanderPlot({ 
    plot(qmao::getQuote.BATS(toupper(input$Symbol), what=input$what, 
         exch=input$exch))
    invalidateLater(1000, session)
  })
})

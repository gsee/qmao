shinyUI(pageWithSidebar(
  headerPanel("BATS"),
  sidebarPanel( 
    wellPanel(
      textInput(inputId="Symbol", "Ticker:", "SPY"),
      helpText("Do NOT press enter when you change the Symbol.",
             "the plot will update automatically."),
      selectInput(inputId="what",
                  label="What:",
                  choices=c("bats"="bats", 
                            "ladder"="ladder", 
                            "depth"="depth")
      ),
      selectInput(inputId="exch",
                  label="Exchange:",
                  choices=c("bzx"="bzx", "byx"="byx"))
 
    ) #,
      #submitButton("Update")
  ),
  
  mainPanel(
    plotOutput(outputId = "plot_symbol")
  )
))

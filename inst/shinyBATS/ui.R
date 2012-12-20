shinyUI(pageWithSidebar(
  headerPanel("BATS"),
  sidebarPanel( 
    wellPanel(
      textInput(inputId="Symbol", "Ticker:", "SPY"),
 
      selectInput(inputId="what",
                  label="what",
                  choices=c("bats"="bats", 
                            "ladder"="ladder", 
                            "depth"="depth")
      ),
      selectInput(inputId="exch",
                  label="exchange",
                  choices=c("bzx"="bzx", "byx"="byx"))
 
    ) #,
      #submitButton("Update")
  ),
  
  mainPanel(
    plotOutput(outputId = "plot_symbol")
  )
))
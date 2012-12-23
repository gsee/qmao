shinyUI(pageWithSidebar(
  headerPanel("BATS"),
  sidebarPanel( 
    wellPanel(
      # work around a shiny bug: http://groups.google.com/d/msg/shiny-discuss/77EU0HfYIZw/_yOnUTDzby0J
      tags$script("$('form').on('submit', function(e){e.preventDefault();});"),
      textInput(inputId="Symbol", "Ticker:", "SPY"),
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

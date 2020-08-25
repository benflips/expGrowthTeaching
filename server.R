library(shiny)
library(plotly)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # identifies model type
  tType <- reactive({
    input$timeType = "Discrete"
  })
  
  # generates appropriate time sequence given time type
  tSeq <- reactive({
    if (tType()) {
      0:maxTime
    } else {
      seq(0, maxTime, 0.1)
    }
  })
  
  # Calculates N(t) vector, given time type
  Nt <- reactive({
    if (tType()){
      geomGrowth(t = tSeq(), b = input$births, d = input$deaths)
    } else {
      expGrowth(t = tSeq(), b = input$births, d = input$deaths)
    }
    
  })
  
  
  
  output$popPlot <- renderPlot({
    
  })
}



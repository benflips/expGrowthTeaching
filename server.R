library(shiny)
library(plotly)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # identifies model type
  tType <- reactive({
    input$timeType == "Discrete"
  })
  
  observe({
    if (!tType()){
      updateSelectizeInput(session, inputId = "births", label = "Birth rate (per capita)")
      updateSelectizeInput(session, inputId = "deaths", label = "Death rate (per capita)")
    } else {
      updateSelectizeInput(session, inputId = "births", label = "Births (per capita)")
      updateSelectizeInput(session, inputId = "deaths", label = "Deaths (per capita)")
    }
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
  
  
  
  output$popPlot <- renderPlotly({
    
  })
}



library(shiny)
library(plotly)
source("functions.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # maximum time horizon
  maxTime <- 20
  
  # identifies model type
  tType <- reactive({
    input$timeType == "Discrete"
  })
  
  # update menu labels, given time type
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
  
  # main plot
  output$popPlot <- renderPlotly({
    NFrame <- data.frame(t = tSeq(), Nt = Nt())
    yMax <- 0.5*5^maxTime
    yRange <- list(0, yMax)
    fig <- plot_ly(data = NFrame, type = "scatter", mode = "none") %>%
      add_trace(y = ~ Nt,
                x = ~ t) %>%
      layout(showlegend = FALSE,
             yaxis = list(range = yRange,
                          title = list(text = "N(t)")),
             xaxis = list(title = list(text = "Time"))) %>%
      config(displayModeBar = FALSE)
  })
}



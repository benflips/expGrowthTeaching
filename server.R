library(shiny)
library(plotly)
source("functions.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # maximum time horizon
  maxTime <- 15
  
  # identifies model type
  tType <- reactive({
    input$timeType == "Discrete"
  })
  
  # update menu labels, given time type
  observe({
    if (!tType()){
      updateSliderInput(session, inputId = "births", 
                           label = "Birth rate (per capita)",
                           min = round(log(1),2),
                           max = round(log(5),2),
                           value = round(log(2),2))
      updateSliderInput(session, inputId = "deaths", label = "Death rate (per capita)")
    } else {
      updateSliderInput(session, inputId = "births", 
                           label = "Births (per capita)",
                           min = 1,
                           max = 5,
                           value = 2)
      updateSliderInput(session, inputId = "deaths", label = "Deaths (per capita)")
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
  
  # identify if we are plotting on log scale or not
  logScale <- reactive({
    input$logScale == "Yes"
  })
  
  # Calculates N(t) vector, given time type
  Nt <- reactive({
    if (tType()){
      geomGrowth(t = tSeq(), b = input$births, d = input$deaths)
    } else {
      expGrowth(t = tSeq(), b = input$births, d = input$deaths)
    }
    
  })
  
  # reports growth rate
  output$growthParameter <- renderTable({
    if (tType()){
      vName <- "R"
      gVal <- RCalc(b = input$births, d = input$deaths)
    } else {
      vName <- "r"
      gVal <- rCalc(b = input$births, d = input$deaths)
    }
    matrix(gVal, dimnames = list(NULL, vName))
  })
  
  # main plot
  output$popPlot <- renderPlotly({
    NFrame <- data.frame(t = tSeq(), Nt = Nt())
    plotType <- "linear"
    if (logScale()){
      yRange <- list(log(0.01), log(yMax))
      plotType <- "log"
    }
    traceType <- if (tType()) "markers" else "lines"
    fig <- plot_ly(data = NFrame, type = "scatter", mode = "none") %>%
      add_trace(y = ~ Nt,
                x = ~ t,
                mode = traceType,
                hoverinfo = "text",
                text = paste("t = ", NFrame$t, ": ", format(round(NFrame$Nt, 0), big.mark = ","), sep = "")) %>%
      layout(showlegend = FALSE,
             yaxis = list(title = list(text = "N(t)"),
                          type = plotType),
             xaxis = list(title = list(text = "Time"))) %>%
      config(displayModeBar = FALSE)
  })
}



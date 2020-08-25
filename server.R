library(shiny)
library(plotly)
source("functions.R")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # maximum time horizon
  maxTime <- 10
  
  # identifies model type
  tType <- reactive({
    input$timeType == "Discrete"
  })
  
  # update menu labels, given time type
  observe({
    minMaxSel <- c(0, 5, 2)
    lnMinMaxSel <- round(log(minMaxSel+0.01), 2)
    if (!tType()){
      updateSliderInput(session, inputId = "births", 
                           label = "Birth rate (per capita)",
                           min = lnMinMaxSel[1],
                           max = lnMinMaxSel[2],
                           value = lnMinMaxSel[3])
      updateSliderInput(session, inputId = "deaths", label = "Death rate (per capita)")
    } else {
      updateSliderInput(session, inputId = "births", 
                           label = "Births (per capita)",
                           min = minMaxSel[1],
                           max = minMaxSel[2],
                           value = minMaxSel[3])
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
      vName <- "Lambda"
      gVal <- lambdaCalc(b = input$births, d = input$deaths)
    } else {
      vName <- "r"
      gVal <- rCalc(b = input$births, d = input$deaths)
    }
    matrix(gVal, dimnames = list(NULL, vName))
  })
  
  # main plot
  output$popPlot <- renderPlotly({
    NFrame <- data.frame(t = tSeq(), Nt = Nt())
    yMax <- 5^(maxTime-2)
    yRange <- list(0, yMax)
    plotType <- "linear"
    if (logScale()){
      yRange <- list(log(0.01), log(yMax))
      plotType <- "log"
    }
    traceType <- if (tType()) "markers" else "lines"
    fig <- plot_ly(data = NFrame, type = "scatter", mode = "none") %>%
      add_trace(y = ~ Nt,
                x = ~ t,
                mode = traceType) %>%
      layout(showlegend = FALSE,
             yaxis = list(range = yRange,
                          title = list(text = "N(t)"),
                          type = plotType),
             xaxis = list(title = list(text = "Time"))) %>%
      config(displayModeBar = FALSE)
  })
}



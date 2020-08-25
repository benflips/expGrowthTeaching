#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Geometric and exponential growth"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("timeType", 
                         label = "Time", 
                         choices = c("Discrete", "Continuous"), 
                         inline = TRUE),
            radioButtons("logScale", 
                         label = "Log scale?", 
                         choices = c("No", "Yes"), 
                         inline = TRUE),
            sliderInput("births",
                        label = "Births (per capita)",
                        min = 0,
                        max = 5,
                        value = 2,
                        step = 0.1),
            sliderInput("deaths",
                        label ="Deaths (per capita)",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.1),
            tableOutput("growthParameter")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("popPlot")
        )
    )
)


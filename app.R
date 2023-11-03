#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(heatmaply)
library(rsconnect)
library(DT)


accident <- read.csv("accident.csv", fileEncoding="UTF-8-BOM")

ui <- fluidPage(
  titlePanel("Accident Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stateInput", "Select a State:", c("All States", unique(accident$STATENAME))),
      selectInput("monthInput", "Select a Month:", unique(accident$MONTHNAME))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Light and Weather", plotlyOutput("barPlot")),
        tabPanel("Gender", plotlyOutput("pieChart")),
        tabPanel("Day Data Table", DTOutput("DayTable"))
      )
    )
  )
)

server <- function(input, output) {
  # Filter data based on user inputs
  filteredData <- reactive({
    req(input$stateInput, input$monthInput)
    data <- accident
    
    if (input$stateInput != "All States") {
      data <- data %>% filter(STATENAME == input$stateInput)
    }
    
    if (!is.null(input$monthInput)) {
      data <- data %>% filter(MONTHNAME == input$monthInput)
    }
    
    return(data)
  })
  
  # Render the total number of accidents in the selected state
  output$summary <- renderPrint({
    req(filteredData())
    if (input$stateInput == "All States") {
      total_accidents <- nrow(filteredData())
      paste("Total number of accidents in all states:", total_accidents)
    } else {
      paste("Total number of accidents in", input$stateInput, ":", nrow(filteredData()))
    }
  })
  
  # Render the bar plot of LGT_CONDNAME and WEATHERNAME
  output$barPlot <- renderPlotly({
    req(filteredData())
    
    plot_data <- filteredData() %>%
      group_by(LGT_CONDNAME, WEATHERNAME) %>%
      summarise(count = n()) %>%
      ungroup()
    
    fig <- plot_ly(plot_data, x = ~LGT_CONDNAME, y = ~count, color = ~WEATHERNAME, type = 'bar') %>%
      layout(title = "Light Condition and Weather")
    
    fig
  })
  
  # Render the pie chart of SEXNAME
  output$pieChart <- renderPlotly({
    req(filteredData())
    
    plot_data <- filteredData() %>%
      count(SEXNAME)
    
    fig <- plot_ly(labels = ~plot_data$SEXNAME, values = ~plot_data$n, type = 'pie') %>%
      layout(title = "Gender")
    
    fig
  })
  
  # Render the data table of WEEK
  output$DayTable <- renderDT({
    req(filteredData())
    
    filteredData() %>%
      group_by(DAY_WEEKNAME) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      DT::datatable(options = list(pageLength = 10))
  })
}

shinyApp(ui, server)

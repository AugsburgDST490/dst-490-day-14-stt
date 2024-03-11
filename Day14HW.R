library(shiny)
library(tidyverse)
library(plotly)

# Load the 2019 dataset
churn_data_2019 <- read.csv("churn_augsburg_2019_final.csv")

# UI definition
ui <- fluidPage(
  titlePanel("Churn Rates in Hennepin County (2019)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sex",
                  "Select Sex:",
                  choices = c("All", unique(churn_data_2019$Sex))),
      selectInput("raceEthnicity",
                  "Select Race/Ethnicity:",
                  choices = c("All", unique(churn_data_2019$RaceEthnicity))),
      actionButton("update", "Update")
    ),
    mainPanel(
      plotlyOutput("churnPlot")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  filtered_data <- eventReactive(input$update, {
    data <- churn_data_2019
    if (input$sex != "All") {
      data <- data[data$Sex == input$sex, ]
    }
    if (input$raceEthnicity != "All") {
      data <- data[data$RaceEthnicity == input$raceEthnicity, ]
    }
    # Group by RaceEthnicity and summarize to get the total counts
    summary_data <- data |>
      group_by(RaceEthnicity, Sex) |>
      summarise(TotalCount = n(), 
                ChurnCount = sum(churn4, na.rm = TRUE)) |>
      mutate(ChurnRate = (ChurnCount / TotalCount) * 100) # Churn rate as a percentage
    
    return(summary_data)
  })
  
  output$churnPlot <- renderPlotly({
    data <- filtered_data()
    
    # Plot churn rates
    plot_ly(data, x = ~RaceEthnicity, y = ~ChurnRate, type = 'bar', color = ~Sex) %>%
      layout(title = "Churn Rates by Race/Ethnicity (2019)",
             xaxis = list(title = "Race/Ethnicity"),
             yaxis = list(title = "Churn Rate (%)"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)


library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)

# Load the dataset
data <- read.csv("C:/Users/Joren Hanz/Documents/cleaned_DA_FinalProj.csv")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Cybersecurity Threat Analyzer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filters", tabName = "filters", icon = icon("filter")),
      menuItem("Visualizations", tabName = "viz", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "filters",
              box(width = 12,
                  selectInput("country", "Select Country:",
                              choices = c("All", unique(data$Country))),
                  selectInput("attack_type", "Select Attack Type:",
                              choices = c("All", unique(data$Attack.Type))),
                  sliderInput("years", "Select Year Range:",
                              min = min(data$Year), max = max(data$Year),
                              value = c(min(data$Year), max(data$Year)),
                              step = 1, sep = "")
              )
      ),
      tabItem(tabName = "viz",
              fluidRow(
                box(width = 6, plotlyOutput("loss_by_attack")),
                box(width = 6, plotlyOutput("trend_over_time"))
              ),
              fluidRow(
                box(width = 12, plotlyOutput("impact_analysis"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    df <- data
    if(input$country != "All") {
      df <- df %>% filter(Country == input$country)
    }
    if(input$attack_type != "All") {
      df <- df %>% filter(Attack.Type == input$attack_type)
    }
    df %>% filter(Year >= input$years[1], Year <= input$years[2])
  })
  
  output$loss_by_attack <- renderPlotly({
    df <- filtered_data()
    plot_ly(df, x = ~Attack.Type, y = ~Financial.Loss..in.Million..., 
            type = "bar", color = ~Attack.Type,
            hoverinfo = 'y',
            hovertemplate = '$%{y:.1f}M<extra></extra>') %>%
      layout(title = "Financial Loss by Attack Type",
             xaxis = list(title = ""),
             yaxis = list(title = "Loss (Millions USD)"))
  })
  
  output$trend_over_time <- renderPlotly({
    df <- filtered_data()
    plot_ly(df, x = ~Year, y = ~Financial.Loss..in.Million..., 
            color = ~Attack.Type, type = 'scatter', mode = 'lines+markers',
            hoverinfo = 'text',
            text = ~paste("Year:", Year, "<br>Loss: $", 
                          Financial.Loss..in.Million..., "M")) %>%
      layout(title = "Financial Loss Over Time",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Loss (Millions USD)"))
  })
  
  output$impact_analysis <- renderPlotly({
    df <- filtered_data()
    plot_ly(df, x = ~Number.of.Affected.Users, 
            y = ~Financial.Loss..in.Million...,
            color = ~Attack.Type,
            size = ~Financial.Loss..in.Million...,
            type = 'scatter', mode = 'markers',
            hoverinfo = 'text',
            text = ~paste("Attack:", Attack.Type, "<br>Users:", 
                          Number.of.Affected.Users, "<br>Loss: $", 
                          Financial.Loss..in.Million..., "M")) %>%
      layout(title = "Impact Analysis: Users Affected vs Financial Loss",
             xaxis = list(title = "Number of Affected Users"),
             yaxis = list(title = "Financial Loss (Millions USD)"))
  })
}

shinyApp(ui, server)
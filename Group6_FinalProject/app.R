library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)

# Load the dataset
data <- read.csv("C:/Users/Joren Hanz/Documents/cleaned_DA_FinalProj.csv")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Interactive Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Controls", tabName = "controls", icon = icon("sliders")),
      menuItem("Plots", tabName = "plots", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "controls",
              fluidRow(
                box(
                  title = "Inputs", width = 4,
                  sliderInput("slider", "Number of Points:", min = 10, max = 100, 
                              value = 50),
                  checkboxInput("show_plotly", "Show Plotly Plot", value = TRUE),
                  selectInput("country", "Select Country:",
                              choices = unique(data$Country)),
                  selectInput("attack_type", "Select Attack Type:",
                              choices = unique(data$Attack.Type)),
                  actionButton("update", "Update")
                )
              )),
      tabItem(tabName = "plots",
              fluidRow(
                box(title = "Total Financial Loss by Attack Type", width = 6, plotOutput("bar_plot")),
                box(title = "Financial Loss Over the Years", width = 6, plotOutput("line_plot")),
                box(title = "ggplot2 Scatter Plot", width = 6, plotOutput("ggplot")),
                box(title = "Plotly Interactive Plot", width = 6, plotlyOutput("plotly"))
              ))
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data[data$Country == input$country & data$Attack.Type == input$attack_type, ]
  })
  
  output$bar_plot <- renderPlot({
    ggplot(data, aes(x = Attack.Type, y = Financial.Loss..in.Million..., fill = Attack.Type)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Total Financial Loss by Attack Type", 
           x = "Attack Type", 
           y = "Total Financial Loss (in Millions)")
  })
  
  output$line_plot <- renderPlot({
    ggplot(data, aes(x = Year, y = Financial.Loss..in.Million..., group = Attack.Type, color = Attack.Type)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Financial Loss Over the Years", 
           x = "Year", 
           y = "Financial Loss (in Millions)")
  })
  
  output$ggplot <- renderPlot({
    ggplot(filtered_data(), aes(x = Financial.Loss..in.Million..., y = Number.of.Affected.Users)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Scatter Plot (ggplot2)", 
           x = "Financial Loss (in Millions)", 
           y = "Number of Affected Users")
  })
  
  output$plotly <- renderPlotly({
    if (input$show_plotly) {
      plot_ly(filtered_data(), x = ~Financial.Loss..in.Million..., 
              y = ~Number.of.Affected.Users, type = 'scatter', mode = 'markers') %>%
        layout(title = "Interactive Scatter Plot (Plotly)", 
               xaxis = list(title = "Financial Loss (in Millions)"), 
               yaxis = list(title = "Number of Affected Users"))
    }
  })
  
}

# Run the app
shinyApp(ui, server)
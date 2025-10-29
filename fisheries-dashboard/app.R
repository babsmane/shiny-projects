library(shiny)
library(leaflet)
library(plotly)
library(ggplot2)
library(dplyr)

# Sample fisheries data
fisheries_data <- data.frame(
  month = rep(month.name, 2),
  catch_kg = c(rnorm(12, 500, 100), rnorm(12, 600, 120)),
  community = rep(c("Community A", "Community B"), each = 12),
  latitude = c(14.7167, 14.7167),
  longitude = c(-17.4677, -17.4677)
)

ui <- fluidPage(
  titlePanel("Fisheries Monitoring Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("community", "Select Community:", 
                  choices = unique(fisheries_data$community)),
      dateRangeInput("dates", "Date Range:",
                     start = "2024-01-01",
                     end = "2024-12-31")
    ),
    mainPanel(
      leafletOutput("map"),
      plotlyOutput("catch_trend"),
      plotOutput("community_metrics")
    )
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = -17.4677, lat = 14.7167,
                 popup = "Fishing Community Zone")
  })
  
  output$catch_trend <- renderPlotly({
    filtered_data <- fisheries_data %>%
      filter(community == input$community)
    
    p <- ggplot(filtered_data, aes(x = month, y = catch_kg, group = 1)) +
      geom_line(color = "#2E86AB") +
      geom_point(color = "#A23B72") +
      labs(title = "Monthly Catch Trends", 
           x = "Month", y = "Catch (kg)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$community_metrics <- renderPlot({
    ggplot(fisheries_data, aes(x = community, y = catch_kg, fill = community)) +
      geom_boxplot() +
      labs(title = "Catch Distribution by Community",
           x = "Community", y = "Catch (kg)") +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal()
  })
}

shinyApp(ui, server)
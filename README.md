Project Overview


This repository contains scripts for analyzing and visualizing Uber trip data from April 2014 to September 2014. The project includes data preprocessing, exploratory data analysis, and visualization components, along with a Shiny app for interactive analysis.

Environment Setup


To run the scripts, you need R installed on your computer along with several R packages. You can install the required packages using the following R command:

install.packages(c("tidyverse", "readxl", "lubridate", "ggplot2", "hrbrthemes", "viridis", "shiny", "leaflet", "data.table", "DT", "rsconnect", "scales", "ggthemes", "shinythemes"))

Data
The dataset includes Uber trip records in New York City. It contains details like date and time of trips, latitude and longitude coordinates, and base company codes. Data files are named according to the month of trips (e.g., uber-raw-data-apr14.csv).

Scripts


Data Loading and Cleaning


The following script loads the data from multiple CSV files, combines them, and cleans up the date/time information:

library(readr)
library(dplyr)
library(lubridate)

# Load and combine data
files <- list.files(path = "/path/to/data", pattern = "*.csv", full.names = TRUE)
combined <- bind_rows(lapply(files, read_csv))

# Clean and preprocess data
combined <- combined %>%
  mutate(Date.Time = mdy_hms(Date.Time),
         Hour = hour(Date.Time),
         Month = month(Date.Time),
         Day = day(Date.Time),
         DayOfWeek = wday(Date.Time, label = TRUE))

Visualization
Example of creating a heatmap of trips by hour and day:

library(ggplot2)

ggplot(combined, aes(x = Hour, y = DayOfWeek, fill = Total)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Heatmap by Hour and Day",
       x = "Hour",
       y = "Day of Week") +
  theme_minimal()

Shiny App
Code snippet to set up a basic Shiny user interface:

R
library(shiny)

ui <- fluidPage(
  titlePanel("Uber Trips Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Choose Month", choices = unique(combined$Month))
    ),
    mainPanel(
      plotOutput("plot1")
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    filtered_data <- combined %>% filter(Month == input$month)
    ggplot(filtered_data, aes(x = Hour, fill = Base)) +
      geom_histogram(stat = "count") +
      theme_minimal() +
      ggtitle(paste("Hourly Trips for", input$month))
  })
}

shinyApp(ui, server)

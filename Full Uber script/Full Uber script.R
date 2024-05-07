library(tidyr)
library(plyr)

library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(tidyverse)
library(dplyr)

library(lubridate)
library(readr)


library(tidyr)

library(glmnet)
library(pROC)
library(corrplot)

library(gridExtra)
library(ggcorrplot)
library(shiny)
library(leaflet)
library(data.table)
library(DT)
library(rsconnect)
library(scales)
library(ggthemes)
library(shinythemes)

#We'll primarily need the libraries below for this project:
#library(ggplot2)
#library(ggthemes)
#library(lubridate)
#library(dplyr)
#library(tidyr)
#library(DT)
#library(scales)

rm(list =ls())

setwd("/Users/samfraolmulugeta/Documents/Data_332/Uber")


df <-read.csv("~/Documents/Data_332/Uber/uber-raw-data-apr14.csv")
df_1 <-read.csv("~/Documents/Data_332/Uber/uber-raw-data-aug14.csv")
df_2 <-read.csv("~/Documents/Data_332/Uber/uber-raw-data-jul14.csv")
df_3 <-read.csv("~/Documents/Data_332/Uber/uber-raw-data-sep14.csv")
df_4 <-read.csv("~/Documents/Data_332/Uber/uber-raw-data-jun14.csv")
df_5 <-read.csv("~/Documents/Data_332/Uber/uber-raw-data-may14.csv")

combined <- rbind(df, df_1, df_2, df_3, df_4, df_5)
write.csv(combined, "combined_data.csv", row.names = FALSE) 

# Convert the "date_column" to date format with the desired format
#  'combined' is the name of your data frame
cleaned <- combined %>%
  mutate(Date.Time = strptime(Date.Time, format = "%m/%d/%Y %H:%M:%S"))

# Convert Date/Time column to POSIXct format
combined$Date.Time <- as.POSIXct(combined$Date.Time, format="%m/%d/%Y %H:%M:%S")
head(combined)

# Extract hour and month from Date/Time column
combined$Hour <- hour(combined$Date.Time)
combined$Month <- month(combined$Date.Time)

combined$Date <- as.Date(combined$Date)

combined$dayofweek <- factor(wday(combined$Date.Time))

# Group by Hour and Month and count trips
hour_month <- combined %>%
  group_by(Hour,Month) %>%
  dplyr::summarize(Total = n()) 
write.csv(hour_month, "result_hour_month.csv")
datatable(hour_month)

combined$Date <- as.Date(combined$Date)
ggplot(hour_month, aes(Hour, Total, fill =factor(Month))) +
  geom_bar(stat = "identity", color = "red") +
  scale_fill_manual(values = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")) + # Set custom colors for each month
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

ggplot(hour_month, aes(Hour, Total, color = factor(Month))) +
  geom_line(size = 1) +
  labs(x = "Hour", y = "Trips", color = "Month") +
  ggtitle("Trips by Hour and Month") +
  theme_minimal() +
  scale_color_manual(values = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")) + # Set custom colors for each month
  scale_y_continuous(labels = comma)


combined$Date <- as.Date(combined$Date)

# Aggregate data by day of the month
day_group <- combined %>%
  mutate(Day = day(Date)) %>%
  group_by(Hour,Month,Base,Day) %>%
  dplyr::summarize(Total = n()) 
write.csv(day_group, "result_hour_month.csv")

# Create a line plot
ggplot(day_group, aes(Day, Total)) +
  geom_line(size = 1) +
  labs(x = "Day of Month", y = "Trips") +
  ggtitle("Trips by Day of Month") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:31) # Set x-axis breaks to show all days of the month

ggplot(day_group, aes(Day, Total)) +
  geom_bar(stat = "identity", width = 0.7, fill = "steelblue") +
  labs(x = "Day of Month", y = "Trips") +
  ggtitle("Trips by Day of month") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:31)


base_month_group <- combined %>%
  mutate(Day = lubridate::day(Date)) %>%
  group_by(Day, Month, Base, Hour) %>%
  dplyr::summarize(Total = dplyr::n())

write.csv(base_month_group, "base_month_group.csv")

# Convert Month to factor data type
base_month_group $Month <- as.factor(base_month_group $Month)

# Create the chart using ggplot
ggplot(base_month_group , aes(Base, Total, fill = Month)) +
  geom_col(position = "dodge", width = 0.8) +
  ggtitle("Trips by Bases and Month") +
  xlab("Bases") +
  ylab("Trips") +
  scale_fill_discrete(name = "Month") +
  theme_minimal()

Heatmap_by_Hour_and_Day<- combined %>%
  mutate(Day = day(Date)) %>%
  group_by(Day,Hour) %>%
  dplyr::summarize(Total = dplyr::n())
write.csv(Heatmap_by_Hour_and_Day, "Heatmap_by_Hour_and_Day.csv")

hours <- 0:23
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
data <- expand.grid(Hour = hours, Day = days)
data$Value <- runif(nrow(data), 1, 100)


ggplot(Heatmap_by_Hour_and_Day, aes(x = Hour, y = Day, fill = Total)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Heatmap by Hour and Day",
       x = "Hour",
       y = "Day") +
  theme_minimal()

Heatmap_by_Month_and_Day<- combined %>%
  mutate(Day = day(Date)) %>%
  group_by(Day,Hour,Month) %>%
  dplyr::summarize(Total = dplyr::n())
write.csv(Heatmap_by_Month_and_Day, "Heatmap_by_Month_and_Day.csv")

ggplot(Heatmap_by_Month_and_Day, aes(Day, Month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")

dayofweek = rep(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), times = 12)

Heatmap_by_Month_and_Week<- combined %>%
  mutate(Day = day(Date)) %>%
  group_by(dayofweek,Hour,Month) %>%
  dplyr::summarize(Total = dplyr::n())
write.csv(Heatmap_by_Month_and_Week, "Heatmap_by_Month_and_Week.csv")


ggplot(Heatmap_by_Month_and_Week, aes(dayofweek, Month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")

Heat_Map_by_Bases_and_Day_of_Week<- combined %>%
  mutate(Day = day(Date)) %>%
  group_by(Base,dayofweek) %>%
  dplyr::summarize(Total = dplyr::n())
write.csv(Heat_Map_by_Bases_and_Day_of_Week, "Heat_Map_by_Bases_and_Day_of_Week.csv")

ggplot(Heat_Map_by_Bases_and_Day_of_Week, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")

min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004
ggplot(combined, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")
ggplot(combined, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by Day of Week")



# Load data
#hour_month <- read.csv("result_hour_month.csv")
#day_group <- read.csv("result_day_group.csv")
#base_month_group<- read.csv("result_day_group.csv")
# Define UI

#Shiny app for uber assignment

getwd()
ui <- fluidPage(
  titlePanel("Trips Analysis"),
  tabsetPanel(
    tabPanel("Trips by Hour and Month",
             fluidRow(
               column(6,
                      selectInput("month", "Select Month", choices = unique(hour_month$Month), multiple = TRUE),
                      plotOutput("plot1")),
               column(6, 
                      h4("Trips by Hour and Month"),
                      selectInput('hour', 'Choose Hour', names(hour_month), 'Hour'),
                      selectInput('total', 'Choose Total', names(hour_month), 'Total'),
                      selectInput('month2', 'Choose Month', names(hour_month), 'Month'),
                      plotOutput('plot2'))
             )),
    tabPanel("Trips by Day of Month",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("start_day", "Select start day:", min = 1, max = 31, value = 1),
                 sliderInput("end_day", "Select end day:", min = 1, max = 31, value = 31)
               ),
               mainPanel(
                 plotOutput("plot3")
               )
             ))
  )
)



server <- function(input, output) {
  
  # Filter data based on selected month
  filtered_hour_month <- reactive({
    if (is.null(input$month)) {
      return(NULL)
    } else {
      hour_month[hour_month$Month %in% input$month, ]
    }
  })
  
  # Render Plot 1
  output$plot1 <- renderPlot({
    if (is.null(filtered_hour_month())) {
      return()
    }
    p <- ggplot(filtered_hour_month(), aes(Hour, Total, fill = factor(Month))) +
      geom_bar(stat = "identity", color = "red") +
      scale_fill_manual(values = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")) +
      ggtitle("Trips Every Hour") +
      theme(legend.position = "none") +
      scale_y_continuous(labels = comma)
    print(p)
  })
  
  # Render the second plot
  output$plot2 <- renderPlot({
    ggplot(data = hour_month, aes_string(x = input$hour, y = input$total, color = factor(input$month2))) +
      geom_line(size = 1) +
      labs(x = input$hour, y = input$total, color = factor(input$month2)) +
      ggtitle("Trips by Hour and Month") +
      scale_color_manual(values = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")) + # Set custom colors for each month
      theme_minimal() +
      scale_y_continuous(labels = comma)
  })
  
  # Filter data based on selected days
  filtered_day_group <- reactive({
    day_group[day_group$Day >= input$start_day & day_group$Day <= input$end_day, ]
  })
  
  # Render plot
  output$plot3 <- renderPlot({
    ggplot(filtered_day_group(), aes(Day, Total)) +
      geom_bar(stat = "identity", width = 0.7, fill = "steelblue") +
      labs(x = "Day of Month", y = "Trips") +
      ggtitle("Trips by Day of Month") +
      theme_minimal() +
      scale_x_continuous(breaks = 1:31) +
      scale_y_continuous(labels = comma)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)







































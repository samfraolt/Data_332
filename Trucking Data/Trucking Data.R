library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

rm(list = ls())

# setting up working directory

setwd("C:/Users/samfr/OneDrive/Documents/Data_332/Trucking Data")


df_truck <- read_excel('C:/Users/samfr/OneDrive/Documents/Data_332/Trucking Data/NP_EX_1-2.xlsx',
                       sheet = 2, skip = 3, .name_repair = 'universal')

# selecting columns
df  <- df_truck[, c(4:15)]
# deselect columns
df <- subset(df, select = -c(...10))

#getting difference in days within a column
#Option 1
date_min <- min(df$Date)
date_max <- max(df$Date)

number_of_days_on_the_road <- date_max - date_min
print(number_of_days_on_the_road)

#Option 2
days <- difftime(max(df$Date), min(df$Date))
print(days)



#Calculating the number of hours driving
total_hours <- sum(df$Hours)
print(paste("Total number of hours driving is", total_hours))

#Calculating average hours per day
number_of_days_recorded <- nrow(df)
#Instances for the days of driving
avg_hrs_per_day_rec <- round(total_hours / number_of_days_recorded, digits = 3)
print(paste("Average hours driven per day is", avg_hours_per_day))

#Adding a column to a new table to show fuel cost
df$fuel_cost <- df$Gallons * df$Price.per.Gallon

#calculating OTHER COSTS and total other costs  
df$other_costs <- df$Misc + df$Tolls
total_other_costs <- round(sum(df$other_costs), digits = 3)

print(paste("The sum of other total costs is", total_other_costs))


#TOTAL GALLONS CONSUMED 
total_gallons <- sum(df$Gallons)

print(paste("Total gallons consumes is", total_gallons))

#TOTAL MILES DRIVEN with a new column
df$total_miles <- df$Odometer.Ending - df$Odometer.Beginning
total_miles_driven <- sum(df$total_miles)

print(paste("Total number of miles driven is", total_miles_driven))

#MILES PER GALLON 
avg_miles_per_gallon <- round(total_miles_driven / total_gallons, digits = 3)

print(paste("Average miles driven per gallon is", avg_miles_per_gallon))

#TOTAL COST PER MILE 
sum_of_total_cost <- sum(df$total_cost)
sum_of_total_miles <- sum(df$total_miles)
total_cost_per_mile <- round(sum_of_total_cost / sum_of_total_miles, digits = 3)

print(paste("The total cost per mile was", total_cost_per_mile))


#string extract the commas 
df$city_state <- gsub(',', "", df$city_state)

#Separating columns (city and state)
# df[c('col1', 'col2')] <- str_split_fixed(df$city_state, '', 2)  ##this doesn't do the job because there needs to be more data cleaning 

#remove the two omahas ie white spaces 
df$ending_city_state <- str_squish(df$ending_city_state)

#renaming city_state, 
df[(c('warehouse', 'starting_city_state'))] <- 
  str_split_fixed(df$Starting.Location, ',', 2)

#making pivot tables(starting city_state and count) 
df_starting_pivot <- df%>%
  group_by(starting_city_state) %>% 
  summarize(count = n(), 
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm = TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )

ggplot(df_starting_pivot, aes(x= starting_city_state, y = count)) + 
  geom_col() + 
  theme(axis.text = element_text(angle =  45, vjust = 5, hjust = 1))



#charting the ending location with pivot tables 

df[c('ending_location', 'ending_city_state')] <- 
  str_split_fixed(df$Delivery.Location, ',', 2)

df_ending_pivot <- df%>%
  group_by(ending_city_state) %>% 
  summarize(count = n(), 
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm = TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )

ggplot(df_ending_pivot, aes(x= ending_city_state, y = count)) + 
  geom_col() + 
  theme(axis.text = element_text(angle =  45, vjust = 5, hjust = 1))


df[c('warehouse', 'starting_city_state')] <-
  str_split_fixed(df$Starting.Location, ',', 2)

df$starting_city_state <- gsub(',', "", df$starting_city_state)

# df[c('col1', 'col2')] <-
#   str_split_fixed(df$city_state, ' ', 2)

df_starting_pivot <- df %>%
  group_by(starting_city_state) %>%
  summarize(count = n(),
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm = TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )

ggplot(df_starting_pivot, aes(x = starting_city_state, y = count)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))





df_ending_pivot <- df %>%
  group_by(ending_city_state) %>%
  summarize(count = n(),
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm = TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )

ggplot(df_ending_pivot, aes(x = ending_city_state, y = count)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))


df$other_Expenses<- df$Tolls + df$Misc + df$fuel_cost

df$Miles_driven <- df$Odometer.Ending - df$Odometer.Begining

df$Miles_per_gallon <- df$Miles_driven/df$Gallons
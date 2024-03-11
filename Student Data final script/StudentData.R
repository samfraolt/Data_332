library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(stringr)


setwd("C:/Users/samfr/OneDrive/Documents/Data_332/Student Data")

#reading files 
df_truck01 <- read_excel('Student.xlsx', .name_repair = 'universal')
df_truck02 <- read_excel('Registration.xlsx', .name_repair = 'universal')
df_truck03 <- read_excel('Course.xlsx', .name_repair = 'universal')


#left join to the student data 
df <- df_truck01 %>%
  left_join(df_truck02, by = "Student.ID") %>%
  left_join(df_truck03, by = "Instance.ID")


#charting major counts 
major_counts <- df %>%
  group_by(Title) %>%
  summarise(count = n())

# Create bar plot for major count 
ggplot(major_counts, aes(x = Title, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Students per Major", x = "Major", y = "Number of Students") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#chart on birth year of student 
df$year <- str_sub(df$Birth.Date, 1, 4)

df_birth_years <- df %>%
  group_by(year) %>%
  summarize(
    studentPerYear = n()
  )
ggplot(df_birth_years, aes(x = year, y = studentPerYear)) + 
  geom_col() +
  geom_bar(stat="identity", fill="skyblue") + 
  theme_light() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))


#Total cost per major, segment by payment plan
cost_per_major <- df %>%
  group_by(Title, Payment.Plan) %>%
  summarize(Total_Cost = sum(Total.Cost))

ggplot(df, aes(x = Title, y = Total.Cost, fill = Payment.Plan)) +
  geom_bar(stat = "identity") +  # Remove position = "dodge"
  labs(title = "Total Cost per Major Segmented by Payment Plan",
       x = "Major",
       y = "Total Cost",
       fill = "Payment Plan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Total balance due by major, segment by payment plan 
balancedue_by_major <- df %>%
  group_by(Title, Payment.Plan) %>%
  summarize(Total_Balance_Due = sum(Balance.Due))


ggplot(df, aes(x = Title, y = Balance.Due, fill = Payment.Plan)) +
  geom_bar(stat = "identity") +  # Remove position = "dodge"
  labs(title = "Total Balance Due by Major Segmented by Payment Plan",
       x = "Major",
       y = "Total Balance Due",
       fill = "Payment Plan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))










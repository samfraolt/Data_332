library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)

rm(list = ls())
setwd("C:/Users/samfr/OneDrive/Documents/Data_332/Paitent Data")

df_patient = read_excel("C:/Users/samfr/OneDrive/Documents/Data_332/Paitent Data/Patient.xlsx", sheet = 1)
df_visit = read_excel("C:/Users/samfr/OneDrive/Documents/Data_332/Paitent Data/Visit.xlsx", sheet = 1)
df_billing = read_excel("C:/Users/samfr/OneDrive/Documents/Data_332/Paitent Data/Billing.xlsx", sheet = 1)

df_visit$VisitDate = ymd(df_visit$VisitDate)


df_patient1 = df_patient %>% 
  dplyr::select("LastName", "FirstName", "Phone", "Address", "City", "Email",
                "PatientID") %>% distinct()

##Earlier Visits Query
earlierVisits = df_visit %>% 
  dplyr::select("VisitID", "PatientID", "Reason", "VisitDate") %>%
  left_join(df_patient1, by = c("PatientID")) %>%
  dplyr::filter(VisitDate < as.Date("2021-01-01"))

allVisits <- df_visit %>%
  select(VisitID, PatientID, Reason, VisitDate) %>%
  left_join(df_patient1, by = "PatientID")

patients_all_city <- allVisits %>%
  group_by(City) %>%
  summarise(number = n())

# Joining billing data with visits
df_final <- df_billing %>%
  left_join(df_visit, by = "VisitID")


### Data Visualizations

patients_city = earlierVisits %>%
  dplyr::group_by(City) %>%
  dplyr::summarise(number = n())
hist(patients_city$number)

#Patients per city prior to 2021-01-01
early_patient_chart = ggplot(patients_city, aes(y = number, x = City)) +
  geom_bar(stat = "identity") +
  xlab("City") +
  ylab("Number of Patients per City") +
  ggtitle("Number of Patients per City Prior to 2021-01-01") +
  theme(axis.text = element_text(angle = 45, vjust = 1, hjust = 1))
plot(early_patient_chart)

# Reason for visit segmented (stacked bar chart) by month of the year

N <- 5 # Change N to display more or fewer reasons
top_reasons <- df_final %>%
  count(Reason) %>%
  top_n(N, wt = n) %>%
  pull(Reason)

#filter the dataset to include only visits for these top reasons
filtered_visits <- df_final %>%
  mutate(Month = month(VisitDate, label = TRUE, abbr = TRUE)) %>%
  filter(Reason %in% top_reasons)

# Create the stacked bar chart for the filtered dataset
filtered_visits %>%
  count(Month, Reason) %>%
  ggplot(aes(x = Month, y = n, fill = Reason)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(y = "Number of Visits", title = "Top Reasons for Visit by Month", x = "Month") +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Reason for visit based on walk-in or not
filtered_visits %>%
  mutate(WalkIn = as.factor(WalkIn)) %>%
  ggplot(aes(x = Reason, fill = WalkIn)) +
  geom_bar(position = position_dodge(), aes(y = ..count..)) +
  labs(y = "Number of Visits", title = "Reason for Visit by Walk-in Status", x = "Reason for Visit") +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Reason for visit based on City
filtered_visits %>%
  left_join(df_patient1, by = "PatientID") %>%
  ggplot(aes(x = City, fill = Reason)) +
  geom_bar(position = "stack") +
  labs(y = "Count", title = "Reason for Visit by City", x = "City") +
  scale_fill_brewer(palette = "Paired") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Total invoice amount based on reason for visit, segmented by payment status
filtered_visits %>%
  group_by(Reason, InvoicePaid) %>%
  summarise(TotalInvoiceAmount = sum(InvoiceAmt), .groups = 'drop') %>%
  ggplot(aes(x = Reason, y = TotalInvoiceAmount, fill = as.factor(InvoicePaid))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(y = "Total Invoice Amount", title = "Total Invoice Amount by Reason for Visit and Payment Status", x = "Reason for Visit") +
  scale_fill_brewer(palette = "Paired", name = "Invoice Paid") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculate the average invoice amount for each invoice item
avg_invoice_amount_by_item <- df_final %>%
  group_by(InvoiceItem) %>%
  summarise(AvgInvoiceAmt = mean(InvoiceAmt, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(AvgInvoiceAmt)) # Optional: arrange by descending average amount for better visualization

# Plot the average invoice amount for each invoice item
ggplot(avg_invoice_amount_by_item, aes(x = reorder(InvoiceItem, -AvgInvoiceAmt), y = AvgInvoiceAmt)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Invoice Amount by Invoice Item",
       x = "Invoice Item",
       y = "Average Invoice Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


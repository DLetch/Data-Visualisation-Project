library(readxl) # To read the spreadsheet with synthetic data
library(lubridate)
library(dplyr)
library(ggplot2)

# Read the data -----------------------------------------------------------


# Read the synthetic data into a tibble
data <- read_xlsx("~/Synthetic_data.xlsx")

# Create multiple new columns for months
data[c("Jan","Feb","Mar","Apr","Jun","Jul",
       "Aug","Sep","Oct","Nov","Dec")] <- NA

# Partner effort ----------------------------------------------------------

# Convert data into dates
data$Start <- ymd(data$`Start date`)
data$End <- ymd(data$`End date`)

# Effort per month
data$EffortPerDay <- data$Effort/as.numeric(data$End-data$Start)

data %>% select(Partner="Partner/contributor",Effort,Start,End) %>% 
   ggplot(aes(x=floor_date(Start,unit="month"),y=Effort,fill=Partner)) + 
   geom_col(position="stack", colour="black") + xlab("2020") +
   ylab("Effort (FTE)")

data %>% select(Partner="Partner/contributor",Effort,Start,End,Project) %>% 
  ggplot(aes(x=floor_date(Start,unit="month"),y=Effort,fill=Partner)) + 
  geom_area(position="stack", colour="black") + facet_wrap(~vars(Project)) +xlab("2020") +
  ylab("Effort (FTE)")

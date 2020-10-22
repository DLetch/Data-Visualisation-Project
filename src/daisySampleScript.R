# These packages need to be loaded.

library(readxl) # To read the spreadsheet with synthetic data
library(lubridate)
library(dplyr)
library(ggplot2)

# Read the data -----------------------------------------------------------

# Make sure that the working directory is where the script is located as the
# path to the data are all taken to be relative to this path. In the files
# panel in RStudio navigate to where the R code file is located and then
# do More -> Set as working directory.

# Read the synthetic data into a tibble
data <- read_xlsx("../data/Synthetic_data.xlsx")


# Data transformation -----------------------------------------------------

# Create multiple new columns for months
data[c("Jan","Feb","Mar","Apr","Jun","Jul",
       "Aug","Sep","Oct","Nov","Dec")] <- NA

# Convert data into dates
data$Start <- ymd(data$`Start date`)
data$End <- ymd(data$`End date`)

# Effort per month
data$EffortPerDay <- data$Effort/as.numeric(data$End-data$Start)

# Partner effort ----------------------------------------------------------

data %>% select(Partner="Partner/contributor",Effort,Start,End) %>% 
   ggplot(aes(x=floor_date(Start,unit="month"),y=Effort,fill=Partner)) + 
   geom_col(position="stack", colour="black") + xlab("2020") +
   ylab("Effort (FTE)")


# Project Area ------------------------------------------------------------

# This is not working
data %>% select(Partner="Partner/contributor",Effort,Start,End,Project) %>% 
  ggplot(aes(x=floor_date(Start,unit="month"),y=Effort,fill=Partner)) + 
  geom_area(position="stack", colour="black") + facet_wrap(~vars(Project)) +xlab("2020") +
  ylab("Effort (FTE)")

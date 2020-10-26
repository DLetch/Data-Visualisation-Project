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


# Data transformations -----------------------------------------------------

# Convert data into dates but put in new columns
data$Start <- ymd(data$`Start date`)
data$End <- ymd(data$`End date`)

# Create multiple new columns for months - stackoverflow 25357194. Initialise to zero
Months <- c("Jan","Feb","Mar","Apr","Jun","Jul", "Aug","Sep","Oct","Nov","Dec")
data <-cbind(data,setNames(lapply(Months, function(x) x=0),Months))

data[1,Months[1]]  <- 5

# days_in_month()

# Get today's date
Today = today()

# Loop over the rows to get the days used - it does not take into account years.
# For years another inner loop would have to be added.
for(i in 1:nrow(data)){
   
   # Dates of start and end of project
   Start <- data[i,"Start"]
   End <- data[i,"End"]
   
   # Check if the project has started
   if(today < Start){
      # Project has not started, move to the next row
      next
   }

   # If years were involved would have to start looping over years here.
   
   # Month of Start and End, Today
   mStart <- month(Start)
   mEnd <- month(End)
   mToday <- month(Today)
   
   # Loop over months in a year
   for(m in 1:12){
      
      # if outside the start or end move to the next month
      if(mStart < m|mEnd < m) next
      
   } # End loops over months
   
}# End loop over years



# https://ro-che.info/articles/2017-02-22-group_by_month_r

# Effort per month - more complicated than this - need to take today into account.
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

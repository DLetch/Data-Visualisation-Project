# These packages need to be loaded.

library(readxl)     # To read the spreadsheet with synthetic data
library(lubridate)  # To deal with date data
library(tidyr)      # To reformat the data
library(dplyr)      # To manipulate the data
library(ggplot2)    # To plot the data


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

Months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul", "Aug","Sep","Oct","Nov","Dec")

# Create multiple new columns for months - stackoverflow 25357194 and initialise
# to zero if the columns have not already been created. 
if(! "Jan" %in% colnames(data)){ 
   data <-cbind(data,setNames(lapply(Months, function(x) x=0),Months))
}

# Get today's date
Today = today()

# Effort per day.
data$EffortPerDay <- ifelse(data$End <= rep(Today,nrow(data)), 
                            round(data$Effort/as.numeric(data$End-data$Start),3),
                            round(data$Effort/as.numeric(Today-data$Start),3))

# Loop over the rows to get the days used to calculate the effort - it does not 
# take into account years. For years another inner loop would have to be added.
for(i in 1:nrow(data)){
   
   # Dates of start and end of project
   Start <- data[i,"Start"]
   End <- data[i,"End"]
   
   # Get the effort per day
   EffortPerDay <- data[i,"EffortPerDay"]
   
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
      if(m < mStart | mEnd < m) next
      
      # We are at the start of the project
      if(mStart == m & mEnd == m){
         
         if(Today < End){
            data[i,Months[m]] <- (day(Today) - day(Start))*EffortPerDay
         }else{
            data[i,Months[m]] <- (day(End) - day(Start))*EffortPerDay
         }
         
         # Do the next month
         next
         
      }else if(mStart == m & m < mToday) {
         
         data[i,Months[m]] <- (days_in_month(m) - day(Start))*EffortPerDay
         
         # move to the the next month
         next
         
      }
      
      # Today is past the current month but less than the end date. 
      # Give it all the days in the month
      if(m < mToday & m < mEnd){
         data[i,Months[m]] <- days_in_month(m)*EffortPerDay
      }
      
      if(mEnd == m & Today < End){
         data[i,Months[m]] <- day(Today)*EffortPerDay
      }else if(mEnd == m){
         data[i,Months[m]] <- day(End)*EffortPerDay
      }
      
   } # End loops over months
   
}# End loop over rows

# Convert the month columns to become row values in a Month column and the 
# corresponding effort values in a MonthEffort column.
d <- pivot_longer(data,cols=Months,names_to="Month",values_to="MonthEffort")

# Partner effort ----------------------------------------------------------

# Without black lines
d %>% select(Partner="Partner/contributor",Month,MonthEffort) %>% 
      ggplot(aes(x=factor(Month,levels=Months),y=MonthEffort,fill=Partner)) + 
      geom_col(aes(group=Partner),position="stack") + xlab("2020") +
      ylab("Effort (FTE)")

# With black lines
d %>% select(Partner="Partner/contributor",Month,MonthEffort,Project) %>% 
   group_by(Month,Partner)                                            %>% 
   summarise(Effort=sum(MonthEffort))                                 %>% 
   ggplot(aes(x=factor(Month,levels=Months),y=Effort,fill=Partner)) + 
   geom_col(aes(group=Partner),position="stack",colour="black") + xlab("2020") +
   ylab("Effort (FTE)")

# Project Area ------------------------------------------------------------

# This is not working
d %>% select(Partner="Partner/contributor",Month,MonthEffort,Project) %>%
      ggplot(aes(x=factor(Month,levels=Months),y=MonthEffort,fill=Partner),colour="black") + 
      geom_area(stat="identity",position="stack") + xlab("2020") +
      ylab("Effort (FTE)")

d %>% select(Partner="Partner/contributor",Month,MonthEffort,Project) %>%
      group_by(Month, Project, Partner) %>% 
      summarise(Effort=sum(MonthEffort),.groups="keep") %>% 
      ggplot(aes(x=factor(Month,levels=Months),y=Effort,group=Project,fill=Project)) +
      geom_line(aes(colour=Project)) 

d %>% select(Partner="Partner/contributor",Month,MonthEffort,Project) %>%
      group_by(Month, Project, Partner)                               %>% 
      summarise(Effort=sum(MonthEffort),.groups="keep")               %>% 
      ggplot(aes(x=factor(Month,levels=Months),y=Effort,group=Project,fill=Project)) +
      geom_line(position="stack",aes(colour=Project)) + geom_area()

d %>% select(Partner="Partner/contributor",Month,MonthEffort,Project) %>% 
      group_by(Month, Project)                                        %>% 
      summarise(Effort=sum(MonthEffort),.groups="keep")               %>% 
      ggplot(aes(x=factor(Month,levels=Months),y=Effort,group=Project,fill=Project)) +
      geom_area(aes(colour=Project)) + geom_line(position="stack",colour="black") +
      xlab("2020")

ggplot(d,aes(x=factor(Month,levels=Months),y=MonthEffort,group=Project,fill=Project)) +
   geom_line(position="stack",aes(colour=Project)) + geom_area() + xlab("2020") +
   ylab("Effort (FTE)")

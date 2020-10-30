# These packages need to be loaded (or installed if not present).

library(readxl)      # To read the spreadsheet with synthetic data
library(lubridate)   # To deal with date data
library(tidyr)       # To reformat the data
library(dplyr)       # To manipulate the data
library(ggplot2)     # To plot the data
library(ggalluvial)  # For alluvial plots
library(ggmap)       # For maps
library(stringr)     # To split lat/long

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
# corresponding effort values in a MonthEffort column. This facilitates
# the plotting when using ggplot.
d <- pivot_longer(data,cols=Months,names_to="Month",values_to="MonthEffort")

# Partner effort ----------------------------------------------------------

# Without black lines delineating the project partners.
d %>% select(Partner="Partner/contributor",Month,MonthEffort) %>% 
      ggplot(aes(x=factor(Month,levels=Months),y=MonthEffort,fill=Partner)) + 
      geom_col(aes(group=Partner),position="stack") + xlab("2020") +
      ylab("Effort (FTE)")

# With black lines delineating the project partner segments.
d %>% select(Partner="Partner/contributor",Month,MonthEffort,Project) %>% 
   group_by(Month,Partner)                                            %>% 
   summarise(Effort=sum(MonthEffort))                                 %>% 
   ggplot(aes(x=factor(Month,levels=Months),y=Effort,fill=Partner)) + 
   geom_col(aes(group=Partner),position="stack",colour="black") + xlab("2020") +
   ylab("Effort (FTE)")

# Effort Area Charts ------------------------------------------------------------

# Monthly effort by Project
d %>% select(Partner="Partner/contributor",Month,MonthEffort,Project) %>% 
      group_by(Month, Project)                                        %>% 
      summarise(Effort=sum(MonthEffort),.groups="keep")               %>% 
      ggplot(aes(x=factor(Month,levels=Months),y=Effort,group=Project,fill=Project)) +
      geom_area(aes(colour=Project)) + geom_line(position="stack",colour="black") +
      xlab("2020") + ggtitle("Effort by Project")

# Normalised monthly effort by Project - warning from zero values in plots I think
d %>% select(Partner="Partner/contributor",Month,MonthEffort,Project) %>% 
   group_by(Month, Project)                                        %>% 
   summarise(Effort=sum(MonthEffort),.groups="keep")               %>% 
   ggplot(aes(x=factor(Month,levels=Months),y=Effort,group=Project,fill=Project)) +
   geom_area(aes(colour=Project),position="fill") + geom_line(position="fill",colour="black") +
   xlab("2020") + ggtitle("Normlised effort by Project") + 
   ylab("% Effort for project") + scale_y_continuous(labels = scales::percent) 

# Monthly effort by Partner
d %>% select(Partner="Partner/contributor",Month,MonthEffort,Project) %>% 
   group_by(Month, Partner,.drop=FALSE)                               %>% 
   summarise(Effort=sum(MonthEffort),.groups="keep")                  %>% 
   ggplot(aes(x=factor(Month,levels=Months),y=Effort,group=Partner,fill=Partner)) +
   geom_area(aes(colour=Partner)) + geom_line(position="stack",colour="black") +
   xlab("2020") + ggtitle("Effort by Partner/Contributor")

# Normalised monthly effort by Partner (warnings for the same reason)
d %>% select(Partner="Partner/contributor",Month,MonthEffort,Project) %>% 
   group_by(Month, Partner,.drop=FALSE)                               %>% 
   summarise(Effort=sum(MonthEffort),.groups="keep")                  %>% 
   ggplot(aes(x=factor(Month,levels=Months),y=Effort,group=Partner,fill=Partner)) +
   geom_area(aes(colour=Partner),position="fill") + geom_line(position="fill",colour="black") +
   xlab("2020") + ggtitle("Normalised effort by Partner/collaborator") + 
   ylab("% Effort for Partner/Contributor") + scale_y_continuous(labels = scales::percent) 


# Alluvial charts ---------------------------------------------------------

# Documentation:
# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html

# Shorten names of Partner/Contributors
d %>% select(Partner="Partner/contributor",Task, Project,MonthEffort)                  %>%
      mutate(Partner=replace(Partner,Partner=="British Heart Foundation","BHF"))       %>% 
      mutate(Partner=replace(Partner,Partner=="Intel Corporation","Intel"))            %>% 
      mutate(Partner=replace(Partner,Partner=="Office for National Statistics","ONS")) %>% 
      mutate(Task=gsub("/","/\n",Task))                                                %>% 
      group_by(Partner, Task, Project, MonthEffort)                                    %>% 
      summarise(TotEffort=sum(MonthEffort), .groups="keep")                            %>% 
      ggplot(aes(axis1=Partner, axis2=Task, axis3=Project,y=TotEffort)) +
      geom_alluvium(aes(fill=Partner), width = 0, reverse = FALSE) +
      guides(fill = FALSE) +
      geom_stratum(width = 1/5, reverse = FALSE) +
      geom_text(stat = "stratum", aes(label = after_stat(stratum)),
                reverse = FALSE, size=2) +  
      scale_x_continuous(breaks = 1:3, labels = c("Partner", "Task", "Project")) +
      coord_flip() + ylab("Total Effort")


# Geographic plot ---------------------------------------------------------

# Use of Google Maps requires you to register to use their maps API, see:
# https://cran.r-project.org/web/packages/ggmap/readme/README.html
#
# OSM not supported, see https://github.com/dkahle/ggmap/issues/117

# Get a GB bounding box, from:
# https://gist.github.com/graydon/11198540
gb <- c(-7.57216793459, 49.959999905, 1.68153079591, 58.6350001085)
gb <- c(-7.58, 49.97, 1.68, 58.64)

# See help for get_stamenmap to see the different type of maptype supported.
# watercolor has no labels but looks a bit amateurish.
# toner-background has no labels but is a bit heavy.
ukmap <- get_stamenmap(gb, zoom = 5, maptype = "watercolor") 
ukmap2 <- get_map()

# Take a peek at the map.
ukmap %>% ggmap() 

# Split lat/long into distinct new columns
d$lat <- as.numeric(str_split_fixed(d$`Partner latitude/longitude`,",",2)[,1])
d$lon <- as.numeric(str_split_fixed(d$`Partner latitude/longitude`,",",2)[,2])

# Stack overflow -10368180 for overlaying pie charts on maps
ggmap(ukmap) + geom_point(data=d,aes(x=lon,y=lat))

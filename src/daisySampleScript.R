# These packages need to be loaded (or installed if not present).

library(readxl)     
# To read the spreadsheet with synthetic data
library(lubridate)   
# To deal with date data
library(tidyr)      
# To reformat the data
library(dplyr)       
# To manipulate the data
library(ggplot2)    
# To plot the data
library(ggalluvial)  
# For alluvial plots

library(tibble)      # To add columns
library(ggmap)       # For maps
library(stringr)     # To split lat/long
library(scatterpie)  # For piecharts
library(ggrepel)     # For non-overlapping labels
library(ggfittext)   # Resize labels for alluvial charts

# Read the data -----------------------------------------------------------

# Make sure that the working directory is where the script is located as the
# path to the data are all taken to be relative to this path. In the files
# panel in RStudio navigate to where the R code file is located and then
# do More -> Set as working directory.

# Read the synthetic data into a tibble
data <- read_xlsx("../data/September_data.xlsx")

# Data transformations -----------------------------------------------------

# Convert data into dates but put in new columns
data$Start <- ymd(data$`Start date`)
data$End <- ymd(data$`End date`)

# Months in a year
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
   
   if(Today < Start){
      
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
d <- pivot_longer(data,cols=all_of(Months),names_to="ProjectMonth",values_to="MonthEffort",
                  names_repair = "unique")

# Partner effort ----------------------------------------------------------

# Without black lines delineating the project partners.
d %>% dplyr::select(Contributor,ProjectMonth,MonthEffort) %>% 
      ggplot(aes(x=factor(ProjectMonth,levels=Months),y=MonthEffort,fill=Contributor)) + 
      geom_col(aes(group=Contributor),position="stack") + xlab("2020") +
      ylab("Effort (FTE)")

# With black lines delineating the project partner segments.
d %>% dplyr::select(Contributor,ProjectMonth,MonthEffort,Project)     %>% 
   group_by(ProjectMonth,Contributor)                                 %>% 
   summarise(Effort=sum(MonthEffort),.groups="keep")                  %>% 
   ggplot(aes(x=factor(ProjectMonth,levels=Months),y=Effort,fill=Contributor)) + 
   geom_col(aes(group=Contributor),position="stack",colour="black") + xlab("2020") +
   ylab("Effort (FTE)")

# Effort Area Charts ------------------------------------------------------------

# Monthly effort by Project
d %>% dplyr::select(Contributor,ProjectMonth,MonthEffort,Project)     %>% 
      group_by(ProjectMonth, Project)                                 %>% 
      summarise(Effort=sum(MonthEffort),.groups="keep")               %>% 
      ggplot(aes(x=factor(ProjectMonth,levels=Months),y=Effort,group=Project,fill=Project)) +
      geom_area(aes(colour=Project)) + geom_line(position="stack",colour="black") +
      xlab("2020") + ggtitle("Effort by Project")

# Normalised monthly effort by Project - warning from zero values in plots I think
d %>% dplyr::select(Contributor,ProjectMonth,MonthEffort,Project)  %>% 
   group_by(ProjectMonth, Project)                                 %>% 
   summarise(Effort=sum(MonthEffort),.groups="keep")               %>% 
   ggplot(aes(x=factor(ProjectMonth,levels=Months),y=Effort,group=Project,fill=Project)) +
   geom_area(aes(colour=Project),position="fill") + geom_line(position="fill",colour="black") +
   xlab("2020") + ggtitle("Normlised effort by Project") + 
   ylab("% Effort for project") + scale_y_continuous(labels = scales::percent) 

# Monthly effort by Partner
d %>% dplyr::select(Contributor,ProjectMonth,MonthEffort,Project)     %>% 
   group_by(ProjectMonth, Contributor,.drop=FALSE)                    %>% 
   summarise(Effort=sum(MonthEffort),.groups="keep")                  %>% 
   ggplot(aes(x=factor(ProjectMonth,levels=Months),y=Effort,group=Contributor,fill=Contributor)) +
   geom_area(aes(colour=Contributor)) + geom_line(position="stack",colour="black") +
   xlab("2020") + ggtitle("Effort by Contributor")

# Normalised monthly effort by Partner (warnings for the same reason)
d %>% dplyr::select(Contributor,ProjectMonth,MonthEffort,Project)         %>% 
   group_by(ProjectMonth, Contributor,.drop=FALSE)                        %>% 
   summarise(Effort=sum(MonthEffort),.groups="keep")                      %>% 
   ggplot(aes(x=factor(ProjectMonth,levels=Months),y=Effort,group=Contributor,fill=Contributor)) +
   geom_area(aes(colour=Contributor),position="fill") + geom_line(position="fill",colour="black") +
   xlab("2020") + ggtitle("Normalised effort by Contributor") + 
   ylab("% Effort for Contributor") + scale_y_continuous(labels = scales::percent) 


# Alluvial charts ---------------------------------------------------------

# Documentation:
# https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html

# Shorten names of Partner/Contributors -using ggfixtext, cf
# https://cran.r-project.org/web/packages/ggalluvial/vignettes/labels.html
# https://cran.r-project.org/web/packages/ggfittext/vignettes/introduction-to-ggfittext.html
d %>% dplyr::select(Partner=Contributor,Task, Project,MonthEffort)                  %>%
   group_by(Partner, Task, Project, MonthEffort)                                    %>% 
   summarise(TotEffort=sum(MonthEffort), .groups="keep")                            %>% 
   ggplot(aes(axis1=Partner, axis2=Task, axis3=Project,y=TotEffort)) +
   geom_alluvium(aes(fill=Partner), width = 0, reverse = FALSE) +
   guides(fill = FALSE) +
   geom_stratum(width = 1/4, reverse = FALSE) +
   ggfittext::geom_fit_text(aes(label = after_stat(stratum)), 
                 stat = "stratum", outside = TRUE,reflow=TRUE, grow=TRUE, reverse = FALSE, 
                 min.size=2, width = 1/4) + 
   scale_x_continuous(breaks = 1:3, labels = c("Partner", "Task", "Project")) +
   ylab("Total Effort")


# Geographic plot ---------------------------------------------------------

# OSM not supported, see https://github.com/dkahle/ggmap/issues/117

# Split lat/long into distinct new columns
d$lat <- as.numeric(str_split_fixed(d$Contributor_Lat_Long ,",",2)[,1])
d$lon <- as.numeric(str_split_fixed(d$Contributor_Lat_Long,",",2)[,2])

# Stack overflow -10368180 for overlaying pie charts on maps
ggmap(ukmap) + geom_point(data=d,aes(x=lon,y=lat))

# Use scatterpie - based on stackoverflow - 51398344

# Load the map
world = map_data("world", resolution=0)

# Remap the data to use with scatterpie
d %>% dplyr::select(Contributor, Task, MonthEffort, lon, lat)  %>% 
      group_by(Contributor, lon, lat, Task,.drop = FALSE)      %>% 
      summarise(TotEffort=sum(MonthEffort),.groups="drop")     %>% 
      pivot_wider(names_from = Task, values_from=TotEffort, values_fill=0) -> e

# Rename some of the labels to shorten them
e$Contributor <- gsub("Alan Turing Institute","ATI",e$Contributor)
e$Contributor <- gsub("University of Exeter","UoE",e$Contributor)
e$Contributor <- gsub("University of Leeds","UoL",e$Contributor)
e$Contributor <- gsub("University of Cambridge","UoC",e$Contributor)

# Convert the Contributors into a factor as it expects this
e$Contributor <- factor(e$Contributor)

# Columns we want
tasks <- names(e)[4:ncol(e)]

# Duplicate lon and lat to introduce some jitter to only one layer
e <- add_column(e,lon2=e$lon,.after=3)
e <- add_column(e,lat2=e$lat,.after=4)

# Manually giving the coords an offset - have not found a clever way of doing
# this for geom_scatterpie.
offset <- 0.25
# r <- sample.int(nrow(e))
# e$lon2 <- e$lon2 + (-1)**r * offset
# e$lat2 <- e$lat2 + (-1)**r * offset

# e[e$Contributor == "Alan Turing Institute","lon2"] <- e[e$Contributor == "Alan Turing Institute","lon2"] + offset
# e[e$Contributor == "Alan Turing Institute","lat2"] <- e[e$Contributor == "Alan Turing Institute","lat2"] + offset

e[e$Contributor == "ATI","lon2"] <- e[e$Contributor == "ATI","lon2"] + offset
e[e$Contributor == "ATI","lat2"] <- e[e$Contributor == "ATI","lat2"] + offset

e[e$Contributor == "UCL","lon2"] <- e[e$Contributor == "UCL","lon2"] - offset
e[e$Contributor == "UCL","lat2"] <- e[e$Contributor == "UCL","lat2"] - offset

e[e$Contributor == "UCLH","lon2"] <- e[e$Contributor == "UCLH","lon2"] - offset
e[e$Contributor == "UCLH","lat2"] <- e[e$Contributor == "UCLH","lat2"] + offset

e[e$Contributor == "REG","lon2"] <- e[e$Contributor == "REG","lon2"] + offset
e[e$Contributor == "REG","lat2"] <- e[e$Contributor == "REG","lat2"] - offset

# e[e$Contributor == "University of Cambridge","lon2"] <- e[e$Contributor == "University of Cambridge","lon2"] + offset
# e[e$Contributor == "University of Cambridge","lat2"] <- e[e$Contributor == "University of Cambridge","lat2"] + offset
e[e$Contributor == "UoC","lon2"] <- e[e$Contributor == "UoC","lon2"] + offset
e[e$Contributor == "UoC","lat2"] <- e[e$Contributor == "UoC","lat2"] + offset


# e[e$Contributor == "University of Exeter","lon2"] <- e[e$Contributor == "University of Exeter","lon2"] - offset
# e[e$Contributor == "University of Exeter","lat2"] <- e[e$Contributor == "University of Exeter","lat2"] + offset
e[e$Contributor == "UoE","lon2"] <- e[e$Contributor == "UoE","lon2"] - offset
e[e$Contributor == "UoE","lat2"] <- e[e$Contributor == "UoE","lat2"] + offset

e[e$Contributor == "UHB","lon2"] <- e[e$Contributor == "UHB","lon2"] - offset
e[e$Contributor == "UHB","lat2"] <- e[e$Contributor == "UHB","lat2"] - offset

e[e$Contributor == "MRC Harwell","lon2"] <- e[e$Contributor == "MRC Harwell","lon2"] - offset
e[e$Contributor == "MRC Harwell","lat2"] <- e[e$Contributor == "MRC Harwell","lat2"] - offset

# e[e$Contributor == "University of Leeds","lon2"] <- e[e$Contributor == "University of Leeds","lon2"] - offset
# e[e$Contributor == "University of Leeds","lat2"] <- e[e$Contributor == "University of Leeds","lat2"] - offset
e[e$Contributor == "UoL","lon2"] <- e[e$Contributor == "UoL","lon2"] - offset
e[e$Contributor == "UoL","lat2"] <- e[e$Contributor == "UoL","lat2"] - offset


# scatterpie vignette 
# https://cran.r-project.org/web/packages/scatterpie/vignettes/scatterpie.html
ggplot(data=world, aes(x=long, y=lat, group=group)) + 
   geom_polygon(data=world, aes(x=long, y=lat), fill="darkseagreen", color="black") + 
   coord_quickmap(xlim=c(-5.0, 1.68), ylim=c(50,54)) +
   ylab("Latitude") + xlab("Longitude") + 
   geom_point(aes(x=lon,y=lat),data=e, inherit.aes = FALSE) +
   geom_scatterpie(data = e,aes(x=lon2, y=lat2, group=Contributor), cols=tasks ,pie_scale = 3,
                   legend_name ="Tasks",sorted_by_radius = TRUE) +
   geom_label_repel(aes(x=lon2,y=lat2,label= Contributor), data=e,
                    size=2, inherit.aes = FALSE, force=5, alpha=0.5,segment.size = 0) +
   theme(
      panel.background = element_rect(fill="lightsteelblue2"),
      panel.grid.minor = element_line(colour="grey90", size=0.5), 
      panel.grid.major = element_line(colour="grey90", size=0.5), 
      legend.position = "top") 



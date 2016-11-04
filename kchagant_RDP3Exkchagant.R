
#=======================================================#
#======= Author:        Karthik Chaganti         =======#
#======= Username :     kchagant                 =======#
#======= UB ID:         50169441                 =======#             
#=======================================================#

#======== Problem 3b RealDirect: EDA on Rolling Sales data across all 5 boroughs! =========#

# Clear existing
rm(list = ls())
graphics.off()

# Set up working directory
(WD <- getwd())
if (!is.null(WD)) setwd(WD)

# Function to check if package exists already
checkForPackage <- function(pack){
  is.element(pack, installed.packages()[,1])
}

# Install Packages required
# doBy package
if(!checkForPackage("doBy")){
  install.packages("doBy")
}

# ggplot2 package
if(!checkForPackage("ggplot2")){
  install.packages("ggplot2")
}

if(!checkForPackage("plyr")){
  install.packages("plyr")
}

if(!checkForPackage("dplyr")){
  install.packages("dplyr")
}
if(!checkForPackage("gridExtra")){
  install.packages("gridExtra")
}

if(!checkForPackage("gdata")){
  install.packages("gdata")
}

if(!checkForPackage("RgoogleMaps")){
  install.packages("RgoogleMaps")
}
if(!checkForPackage("ggmap")){
  install.packages("ggmap")
}
if(!checkForPackage("lubridate")){
  install.packages("lubridate")
}
#load libraries
library("doBy")
library("ggplot2")
library("plyr")
library("dplyr")
library("gridExtra")
library("gdata")
library("RgoogleMaps")
library("ggmap")
library("lubridate")
#=======================================================#
# BOROUGH CENTRIC DATA ANALYSIS
#=======================================================#
# 1. Clean data and remove outliers etc...
#=======================================================#
files <- c("bronx","brooklyn","manhattan","statenisland","queens")
borough_data <- data.frame()

# Run loop and collect data from all 5 borough files and append data to one dataframe
for(j in files)
{
  file_name <- paste("rollingsales_",j,".xls",sep="")

  borough_data_temp <- read.xls(xls=file_name,pattern="BOROUGH",perl="C:/Perl64/bin/perl.exe")
  names(borough_data_temp) <- tolower(names(borough_data_temp))
  borough_data <- rbind(borough_data,borough_data_temp)
  remove(borough_data_temp)

}


# check to see that all formattings are proper, correct if not!
borough_data$sale.price.n <- as.numeric(gsub("[^[:digit:]]", "", borough_data$sale.price))
borough_data$gross.sqft <- as.numeric(gsub("[^[:digit:]]", "", borough_data$gross.square.feet))
borough_data$land.sqft <- as.numeric(gsub("[^[:digit:]]", "", borough_data$land.square.feet))
borough_data$sale.date <- as.Date(borough_data$sale.date)
borough_data$year.built <- as.numeric(as.character(borough_data$year.built))

# Add a months column to the data frame!
borough_data$month <- months(borough_data$sale.date)

#keep only the actual sales
borough_data.sale <- borough_data[borough_data$sale.price.n!=0,]

# Search for outliers
ggplot(borough_data.sale, aes(gross.sqft, sale.price.n)) + geom_point()
ggplot(borough_data.sale, aes(log(gross.sqft), log(sale.price.n))) + geom_point()

# Analysis : There seems to be too much of a crowd. So lets categorize by building class and then plot accordingly. 
# But there do exists quite a few outliers. Lets see if we can eliminate them

# Categorize the building class category
borough_data.sale$building.class.category <- factor(borough_data.sale$building.class.category)

# Summarize as shown:
summaryBy(gross.sqft+land.sqft~building.class.category, 
          data=borough_data.sale, FUN=c(min, max))

# Remove outliers as shown!
borough_data$outliers <- (log(borough_data$sale.price.n) <=5) + 0
borough_data <- borough_data[which(borough_data$outliers==0),]

# lets plot again and check
ggplot(borough_data, aes(log(gross.sqft), log(sale.price.n))) + geom_point()

#=======================================================#
# 2. EDA Across BOROUGHS
#=======================================================#
# Note: It is observed by intuition that for sales data of real estate,
# "cost/sqft" is the most sensible metric. Hence all the 
# analysis and inference is centered around it!

# Categorize the borough numbers as names of the NYC boroughs    
borough_data$borough <- cut(borough_data$borough, c(0,1,2,3,4,5),
                                          labels = c("Manhattan","Bronx","Brooklyn","Queens","StatenIsland"))

# Calculate the cost per sqft and append the column to the data table
    borough_data$costpersqft <- borough_data$sale.price.n/borough_data$land.sqft
# Replace all the infinity values by NA
    borough_data$costpersqft[mapply(is.infinite,borough_data$costpersqft)] <- NA

# Create a new table with cost per sqft categorized on boroughs 
    borough_mean_costp_summary <- as.data.frame(summaryBy(costpersqft ~ borough,
                                                          data = borough_data,FUN=mean,na.rm=TRUE))
  
# Plot a Line plot of MEAN COST PER SQFT AREA OF ALL BUILDINGS ACROSS BOROUGHS
# This will give a broad picture on what are the most and least expensive areas!
    ggplot(borough_mean_costp_summary,aes(y=costpersqft.mean,x=borough,group=1)) + geom_line() +geom_point()+
    ggtitle("Mean Cost per sq.ft of *all* buildings across boroughs : Manhattan Rocks!")
  
# Create a new table with cost per sqft of OFFICES categorized on boroughs    
    borough_office_summary <- as.data.frame(summaryBy(costpersqft ~ borough,
                                          data = subset(borough_data,grepl("OFFICE",borough_data$building.class.category)
                                                                      ,FUN=mean,na.rm=TRUE)))
    
# Plot a bar plot of MEAN COST PER SQFT AREA OF ALL *OFFICE* BUILDINGS ACROSS BOROUGHS  
    ggplot(borough_office_summary,aes(y=costpersqft.mean,x=borough,fill=borough,group=1)) + 
      geom_bar(stat="identity")+geom_point()+geom_line()+ 
      ggtitle("Mean Cost per sq.ft of all *OFFICE* buildings across boroughs : Manhattan Rocks!")
  
# Create a new table with cost per sqft of HOTELS categorized on boroughs  
    borough_hotel_summary <- as.data.frame(summaryBy(costpersqft ~ borough,
                                                    data = subset(borough_data,grepl("HOTEL",borough_data$building.class.category)
                                                                  ,FUN=mean,na.rm=TRUE)))

# Plot: Mean Cost per sq.ft of all *HOTEL* buildings across boroughs      
    ggplot(borough_hotel_summary,aes(y=costpersqft.mean,x=borough,fill=borough,group=1)) + 
      geom_bar(stat="identity")+geom_point()+geom_line()+ 
      ggtitle("Mean Cost per sq.ft of all *HOTEL* buildings across boroughs : Manhattan Rocks!")

# Create a new table with TOTAL MONTHLY SALES ACROSS BOROUGHS  
    borough_totsales_monthly <-as.data.frame(summaryBy(sale.price.n ~ borough+month,
                                                       data =borough_data,FUN=length))
    
    ggplot(borough_totsales_monthly,aes(y=sale.price.n.length,x=month,group=1)) + 
      geom_point()+geom_line()+ facet_grid(. ~ borough)+
      ggtitle("TOTAL MONTHLY SALES ACROSS BOROUGHS")
    
    
#=================== end of file ==========================#

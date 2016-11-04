
#=======================================================#
#======= Author:        Karthik Chaganti         =======#
#======= Username :     kchagant                 =======#
#======= UB ID:         50169441                 =======#             
#=======================================================#

#======== Problem 3 RealDirect: EDA on Rolling Sales data for Brooklyn =========#

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
# BROOKLYN DATA SET
#=======================================================#
# 1. Clean data and remove outliers etc...
#=======================================================#


 brook <- read.xls(xls="rollingsales_brooklyn.xls",pattern="BOROUGH",perl="C:/Perl64/bin/perl.exe")
 names(brook) <- tolower(names(brook))
 load("locations.Rdata")
 brook <- merge.data.frame(brook,table,by = "neighborhood")

brook$sale.price.n <- as.numeric(gsub("[^[:digit:]]", "", brook$sale.price))
brook$gross.sqft <- as.numeric(gsub("[^[:digit:]]", "", brook$gross.square.feet))
brook$land.sqft <- as.numeric(gsub("[^[:digit:]]", "", brook$land.square.feet))

brook$sale.date <- as.Date(brook$sale.date)
brook$year.built <- as.numeric(as.character(brook$year.built))

# EDA
# Total Sales

ggplot(brook, aes(x=sale.price.n)) + 
  geom_histogram(binwidth = diff(range(brook$sale.price.n)))

#keep only the actual sales
brook.sale <- brook[brook$sale.price.n!=0,]

# Search for outliers
ggplot(brook.sale, aes(gross.sqft, sale.price.n)) + geom_point()
ggplot(brook.sale, aes(log(gross.sqft), log(sale.price.n))) + geom_point()

#Analysis : There seems to be too much of a crowd. So lets categorize by building class and then plot accordingly. 
# But there do exists quite a few outliers. Lets see if we can eliminate them

#Categorize the building class category
brook.sale$building.class.category <- factor(brook.sale$building.class.category)

# Summarize as shown:
summaryBy(gross.sqft+land.sqft~building.class.category, 
          data=brook.sale, FUN=c(min, max))

brook$outliers <- (log(brook$sale.price.n) <=5) + 0
brook <- brook[which(brook$outliers==0),]

# lets plot again and check
ggplot(brook, aes(log(gross.sqft), log(sale.price.n))) + geom_point()

#=======================================================#
# 2. EDA Across Neighborhoods
#=======================================================#
# Note: It is observed by intuition that for sales data of real estate,
# "cost/sqft" is the most sensible metric. Hence all the 
# analysis and inference is centered around it!

# Calculate the cost per sqft and append the column to the data table
  brook$costpersqft <- brook$sale.price.n/brook$land.sqft
# Replace all the infinity values by NA
  brook$costpersqft[mapply(is.infinite,brook$costpersqft)] <- NA

# Create a new table with cost per sqft and neigborhood details distribution of offices 
  brook_neigh_costp_office <- as.data.frame(summaryBy(costpersqft ~ neighborhood, 
                                                   data = subset(brook,grepl("OFFICE",brook$building.class.category)),
                                                   FUN=mean))
  
# Cost per Sq.Ft. of luxury hotels distributed across neighbourhoods in Brooklyn
  ggplot(brook_neigh_costp_office, aes(x=neighborhood,y=costpersqft.mean, fill=neighborhood)) +
    geom_bar(stat="identity")
  # Analysis: Gravesend leads followed by Brooklyn Heights!
  
# The relations across neighborhoods can be better plotted using maps
  mapper <- get_map(location =c(lon=-73.90606,lat=40.64023),zoom="auto")
  
# Summarize the data table by Latitude and longitude of Office Buildings across neighborhoods
  
  brook_neigh_costp_off_map <- as.data.frame(summaryBy(costpersqft ~ neighborhood+lat+lon, 
                                                       data = subset(brook,grepl("OFFICE",brook$building.class.category)),FUN = mean))
  brook_neigh_costp_off_map$costpersqft_cat <- cut(brook_neigh_costp_off_map$costpersqft,
                                                   c(-Inf,250,700,2010), labels = c("Cheap","Moderate","Expensive"))
  
  # Plot a map of distribution of Office spaces across neighborhoods by their cost per sqft
  ggmap(mapper) + geom_point(data=brook_neigh_costp_off_map,aes(x=brook_neigh_costp_off_map$lon,y=brook_neigh_costp_off_map$lat,colour=costpersqft_cat))
  
# Summarize the data table by Latitude and longitude of hotels across neighborhoods
  
  brook_neigh_costp_hotel_map <- as.data.frame(summaryBy(costpersqft ~ neighborhood+lat+lon, 
                                                       data = subset(brook,grepl("HOTELS",brook$building.class.category)),FUN = mean))
  brook_neigh_costp_hotel_map$costpersqft_cat <- cut(brook_neigh_costp_hotel_map$costpersqft,
                                                   c(-Inf,250,1300,Inf), labels = c("Cheap","Moderate","Expensive"))
  
  # Plot a map of distribution of hotel spaces across neighborhoods by their cost per sqft
  ggmap(mapper) + geom_point(data=brook_neigh_costp_hotel_map,aes(x=brook_neigh_costp_hotel_map$lon,
                                                                  y=brook_neigh_costp_hotel_map$lat,colour=costpersqft_cat))

  

  #=======================================================#
  # 3. EDA Across Neighborhoods ~ MONTHLY!!!
  #=======================================================#

  # Add Months column to the data set!
    brook$month <- months(brook$sale.date)

    brook_times_offices <- as.data.frame(summaryBy(sale.price.n ~ neighborhood+month+lat+lon, 
                                                        data = subset(brook,grepl("OFFICE",brook$building.class.category)),
                                                        FUN=length))
    
  # Distribution of total number of sales of offices across neighborhoods across months 
    ggplot(brook_times_offices, aes(x=month,y=sale.price.n.length, fill=neighborhood)) +
      geom_bar(stat="identity") 
    
 
    brook_times_hotels <- as.data.frame(summaryBy(sale.price.n ~ neighborhood+month+lat+lon, 
                                                  data = subset(brook,grepl("HOTELS",brook$building.class.category)),
                                                  FUN=length))
    
  # Distribution of total number of sales of hotels across neighborhoods across months 
    ggplot(brook_times_hotels, aes(x=month,y=sale.price.n.length, fill=neighborhood)) +
      geom_bar(stat="identity")    
    
    brook_times_summary <- as.data.frame(summaryBy(sale.price.n ~ neighborhood+month,data = brook,FUN=length))
  # Distribution of total number of ALL sales across neighborhoods across months 
    ggplot(brook_times_summary, aes(x=month,y=sale.price.n.length, fill=neighborhood)) +
      geom_bar(stat="identity")
  

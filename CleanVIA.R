library(dplyr)
library(plyr)
library(zoo)
library(reshape2)
#############################################################################################
#' Title: "Clean VIA Excel Datasets"
#' Mausam Duggal, Systems Analysis Group, WSP|PB", April 19, 2016"
#############################################################################################
# set working directory

wd <- setwd("c:/personal/R")

##############################################################################################
#' set the years the data is available

year.lst <- c(2015, 2014, 2013, 2012, 2011, 2010, 2009)

for (i in 1: length(year.lst)) {
  
  # create filename handle
  filename <- paste0(year.lst[i], sep="")
  
#' read in csv file of VIA (2015 per Weekday Origin Tab in this case). We can loop
#' this to read all the tabs. 

via <- read.csv(file = paste0(filename[i], "VIA.csv"), stringsAsFactors = FALSE)

#' rename columns to make them more readable
  via <- rename(via, c("X"="Month", "Origin"="Day", "X.1"="Stn", "X.2"="Name"))
    via$Month[via$Month == ""] <- NA
    via$Day[via$Day == ""] <- NA

  # make copy to do transformation on
  via1 <- via

#' copy in the first month because the excel file when exported to CSV does not populate the
#' first row due to formatting issues. I am trying to do the least amount of formatting in Excel
#' except deleting the first 17 rows

via1$Month[1] <- "Jan"


#' #' Now use the ZOO package to populate with the earlier non-NA value
#' via1$Origin <- na.locf(via1$Origin)   # first do it for the weekdays
#' via1$X <- na.locf(via1$X)    # next do it for the month

################################################################################################
#' using a function instead of Zoo to populate WEEKDAY columns
################################################################################################

fill.vals = function(x) {
  ind = which(!is.na(x$Day))      # get positions of nonmissing values
    #ind1 <- as.data.frame(ind)
  if(is.na(x$Day[1]))             # if it begins with a missing, add the
    ind = c(1,ind)           # first position to the indices
      #ind2 <- as.data.frame(ind)
  x$Day <- rep(x$Day[ind], times = diff(   # repeat the values at these indices
    c(ind, nrow(x) + 1)))
}

#' call for via1 
  via1$Day <- fill.vals(via1)


#' using a function instead of Zoo to populate MONTH columns

fill.vals1 = function(x) {
  ind = which(!is.na(x$Month))      # get positions of nonmissing values
  #ind1 <- as.data.frame(ind)
  if(is.na(x$Month[1]))             # if it begins with a missing, add the
    ind = c(1,ind)           # first position to the indices
  #ind2 <- as.data.frame(ind)
  x$Month <- rep(x$Month[ind], times = diff(   # repeat the values at these indices
    c(ind, nrow(x) + 1)))
}

#' call for via1 to fill in the Month 
  via1$Month <- fill.vals1(via1)

#' Now use Grep function to clean the dataset by 
#' getting rid of the total rows

via1.clean <- via1[-grep("Total|Origin", via1$Day),]

#' Now let's melth this data and add year to the cleaned dataset
via1.clean.m <- melt(via1.clean, id.vars = c("Month", "Day", "Stn", "Name"))
  via1.clean.m$year <- year.lst[i]

#' write csv file
write.csv(via1.clean.m, file = paste0(year.lst[i],"cleanedVIA.csv"))

}
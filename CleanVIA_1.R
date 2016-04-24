library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
#############################################################################################
#' Title: "Clean VIA Excel Datasets"
#' Mausam Duggal, Systems Analysis Group, WSP|PB", April 19, 2016"
#############################################################################################
# set working directory

wd <- setwd("c:/personal/R")

##############################################################################################
#' set the years the data is available

year.lst <- c(2015, 2014, 2013, 2012, 2011, 2010, 2009)

#' Start for loop to go through each of the years in the year.lst user
#' defined input

for (i in 1:length(year.lst)) {
  
  # create filename handle
  filename <- paste0(year.lst[i], sep="")
 
#' read in csv file of VIA (2015 per Weekday Origin Tab in this case). We can loop
#' this to read all the tabs. 

via <- read.csv(file = paste0(filename, "VIA.csv"), stringsAsFactors = FALSE)

#' rename columns to make them more readable. Only 2015 has weekdays hence 
#' the If statement

if (filename == "2015"){
  via <- rename(via, c("X"="Month", "Origin"="Day", "X.1"="Stn", "X.2"="Name"))
    via$Month[via$Month == ""] <- NA
    via$Day[via$Day == ""] <- NA
}else{
  via <- rename(via, c("X"="Month", "Origin"="Stn", "X.1"="Name"))
  via$Month[via$Month == ""] <- NA
}
  # make copy to do transformation on
  via1 <- via

#' copy in the first month because the excel file when exported to CSV does not populate the
#' first row due to formatting issues. I am trying to do the least amount of formatting in Excel
#' except deleting the first 17 rows

via1$Month[1] <- "Jan"

################################################################################################
#' using a function instead of Zoo to populate WEEKDAY columns
################################################################################################

if (filename == "2015") {
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
  
} else {
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
}

#' Now use Grep function to clean the dataset by 
#' getting rid of the total rows. Once again test 
#' for filename condition

if (filename == "2015"){
  via1.clean <- via1[-grep("Total|Origin", via1$Day),]
} else {
  via1.clean <- via1[-grep("Origin", via1$Stn),]
  via1.clean <- via1.clean[!(via1.clean$Stn == ""), ]
}


#' Now let's melth this data and add year to the cleaned dataset
#' once again first test for year.

if (filename == "2015"){
  via1.clean.m <- melt(via1.clean, id.vars = c("Month", "Day", "Stn", "Name"))
  via1.clean.m$year <- year.lst[i]
} else {
  via1.clean.m <- melt(via1.clean, id.vars = c("Month", "Stn", "Name"))
  via1.clean.m$year <- year.lst[i]
}


#' write csv file, but before doing that add in Day column
if (filename=="2015"){
  write.csv(via1.clean.m, file = paste0(year.lst[i],"cleanedVIA.csv"))
} else {
  via1.clean.m$Day <- NA
  write.csv(via1.clean.m, file = paste0(year.lst[i],"cleanedVIA.csv"))
}

}    # close for loop

##############################################################################
#' batchin the cleaned up files and format the dataset in the way Rick wants it

file_names <- c("2015cleanedVIA.csv","2014cleanedVIA.csv", "2013cleanedVIA.csv", "2012cleanedVIA.csv",
                "2011cleanedVIA.csv", "2010cleanedVIA.csv", "2009cleanedVIA.csv") #file names

all_via <- do.call(rbind,lapply(file_names,read.csv))

#' reorder the columns as per Rick's requirement and also rename them

all_via <- rename(all_via, c("Stn"="Origin", "variable"="Destination", "value"="Flow"))
  all_via <- all_via[c("Origin", "Destination", "year", "Month", "Day", "Flow")]
  #' also set the NA values in Flow to zero
  all_via$Flow[is.na(all_via$Flow)] <- 0

#' write out final csv file
  write.csv(all_via, file = "AllcleanedVIA.csv")
  

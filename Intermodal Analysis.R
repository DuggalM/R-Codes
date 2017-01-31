library(dplyr)
library(reshape2)
library(tidyr)
library(foreign)
library(ggplot2)
##########################################################################################

#' Title: "Explore Intermodal and Rail commodity percentages by distance"
#' Author: "Mausam Duggal"
#' Date: "Nov 7th, 2016"

#########################################################################################

#' set working directory
wd <- setwd("c:/personal/R")

#+ reduce margins for plotting
par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.7)

#' read rail summaries and only keep intermodal records and those for 2011
rail <- read.csv("C:/Personal/R/Rail Traffic Summary.csv")  
rail_im <- subset(rail, Intermodal_Flag == "Intermodal")
rail_im11 <- subset(rail_im, year == 2011)
#' read equivalency file that will conver all the Origin and Destination codes in the rail data
#' to a numeric zone id, for easy manipulation. These zone ID's also corresspond to the distance
#' skims that were created in ArcMap. The ArcMap skims are a placeholder as the Emme skims were
#' unavailable. These will be replaced later on, if needed.
im_equiv <- read.csv("intermodal_equiv.csv", stringsAsFactors = FALSE)
skim <- read.dbf("Dist_Intermodal.dbf") %>% 
  subset(., Total_Leng != 0) %>% 
  subset(., select = c(Name, Total_Leng))
#' set factor columns to character
i <- sapply(skim, is.factor)
skim[i] <- lapply(skim[i], as.character) 
skim <- separate(skim, Name, into = c("Orig", "Dest"), sep = " - ") %>% 
  transform(., Total_Leng = Total_Leng/1000)


#' create a pivot table of flows
m <- rail_im11 %>% group_by(Origin, Destination, sctg2) %>% summarise(ton = sum(tonnes))

#' Merge the equiv file to create appropriate Orig IDs
m1 <- merge(m, im_equiv, by.x = "Origin", by.y = "STATE_NAME", all.x = TRUE) %>% 
  subset(., select = - RAILZONE_3) %>% 
  rename(., OrigID = ID_1)

#' Now merge the Dest IDs
m1 <- merge(m1, im_equiv, by.x = "Destination", by.y = "STATE_NAME", all.x = TRUE) %>% 
  subset(., select = - RAILZONE_3) %>% 
  rename(., DestID = ID_1)
#' set all NA records to 999 as they are in Mexico
m1[is.na(m1)] <- 999
str(m1)

#' Now merge the skim distance to this table
m2 <- merge(m1, skim, by.x = c("OrigID", "DestID"), by.y = c("Orig","Dest"), all.x = TRUE)
# set Mexico to a default distance of 5000 km.
m2[is.na(m2)] <- 5000
# get skim codes
m2 <- transform(m2, DCode = ifelse(Total_Leng <= 500, 1,
                                   ifelse(Total_Leng > 500 & Total_Leng <= 1000, 2,
                                          ifelse(Total_Leng > 1000 & Total_Leng <= 1500, 3,
                                                 ifelse(Total_Leng > 1500 & Total_Leng < 2000, 4, 5)))))

# Add trade type using the Railzone definitions. If less than 50 then 
# both trip ends were within Canada; international otherwise
m2 <- transform(m2, Type = ifelse(OrigID < 50 & DestID < 50, "Domestic", "International"))



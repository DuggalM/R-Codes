library(dplyr)
library(reshape2) 

###############################################################################
#' Title: "Creation of IE/EI PAs for Waterloo model"
#' Creator: "Mausam Duggal, Systems Analysis Group, WSP|PB"
#' Date: "Nov 27, 2016"

#########################################################################################
#' This code was developed to generate a PA table of IE and EI trips to be used in the
#' GO mode choice model for the Waterloo model. It works off the V4 model's PA tables
#' and merges in the necessary Waterloo model equivalencies. While in the Waterloo model
#' it is difficult to create a one-one equivalency between the RMOW zones and those in the 
#' V4, it was neccessary to do so here. I have used the min function to avoid significant
#' overlap between the RMOW zone numbers and those in the V4. The impact is expected to be 
#' minimal given the scale of the aggregation.
##########################################################################################

#' Set working directory
wd <- setwd("c:/projects/waterloo/PA")

# function for setting NAs in dataframe to 0
f_rep <- function(df) {
  # this function is used to set all NA values to zero in a dataframe
  df[is.na(df)] <- 0
  return(df)
}
################################################################################

#' read in the equivalency file than Nem created. We only need the TTS_Zone
#' field, which will be used to subset the PA tables
eq <- read.csv("c:/projects/waterloo/equiv.csv", stringsAsFactors = F)
# get a one to one equivalency using min, as this will ensure a low overlap of zone
# numbers between the V4 and the RMOW model
one <- eq %>% group_by(TTS_Zone) %>% summarise(WM_Zone = min(WaterlooZone))

###############################################################################
#' Do this for the first file and save it as this will be used to bind all
#' the rest of the PA

#' get a list of csv files in the folder to process
temp = list.files(pattern=".csv$")

first <- read.csv(temp[1], stringsAsFactors = F) %>% 
  subset(., .$Origin %in% eq$TTS_Zone | 
           .$Destination %in% eq$TTS_Zone)
  # reset column names 
  colnames(first) <- c("Origin", "Dest", "PAvals")

#' Now batch in each of the remaining files and bind them to the first.
for (i in 2:length(temp)){
  
  # read in each CSV file and subset it to keep records that have at least one trip
  # end within Waterloo
  s <- read.csv(temp[i], stringsAsFactors = F) %>% subset(., .$Origin %in% eq$TTS_Zone | 
                     .$Destination %in% eq$TTS_Zone)
      # reset column names
      colnames(s) <- c("Origin", "Dest", "PAvals")
  
  # Bind the files together
  wat <- rbind(first, s)
}

#' Now group the above PA master file to create a PA table for Waterloo
pa_master <- wat %>% group_by(Origin, Dest) %>% 
  summarise(PA = sum(PAvals)) %>% 
  subset(., PA > 0)

#' write out the finalPA
write.csv(pa_master, "WaterlooIE_PAs_V4.csv")

###############################################################################
#' Now get skims for zone-zone pairs that have a PA between the GGH and RMOW
#' get a list of csv files in the folder to process
wd <- setwd("c:/projects/waterloo/Skims")
temp = list.files(pattern=".csv$")

for (i in 1:length(temp)) {
  
  # read in each CSV file and subset it to keep records that have at least one trip
  # end within Waterloo
  f <- read.csv(temp[i], stringsAsFactors = F) 
    colnames(f) <- c("Origin", "Dest", "time")
  
  f1 <- merge(f, pa_master, by.x = c("Origin", "Dest"), 
              by.y = c("Origin", "Dest")) %>% 
    subset(., select = - c(PA))
  
  write.csv(f1, paste0("c:/projects/waterloo/skim processed/", temp[i],".csv"))
  
}




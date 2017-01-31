library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)

#' set working directory
wd <- setwd("c:/personal/r")

#' batch in the AM Trucks from 2011. Nem output these on Dec 21st, 2016.
#' These do not represent a GC assignment on Hwy 407.
lt <- read.csv("am_light.csv")
md <- read.csv("am_medium.csv")
hy <- read.csv("am_heavy.csv")

#' now read in the 2011 employment data by TAZs
att <- read.csv("Zone Attributes.csv")

###############################################################################
#' Summarize the matrices to trip end vectors
###############################################################################
lt_o <- lt %>% group_by(Origin) %>% summarise(LT_Orig = sum(X0))
lt_d <- lt %>% group_by(Destination) %>% summarise(LT_Dest = sum(X0))

md_o <- md %>% group_by(Origin) %>% summarise(MD_Orig = sum(X0)/1.75)
md_d <- md %>% group_by(Destination) %>% summarise(MD_Dest = sum(X0)/1.75)

hy_o <- hy %>% group_by(Origin) %>% summarise(HY_Orig = sum(X0)/2.5)
hy_d <- hy %>% group_by(Destination) %>% summarise(HY_Dest = sum(X0)/2.5)

#' all origins
################################################################################
all_o <- merge(lt_o, md_o, by = "Origin", all = TRUE) %>% 
  merge(., hy_o, by = "Origin", all = TRUE) %>% 
  transform(., TotalOrigin = rowSums(.[, 2:4]))

#' now merge the attribute information to origin truck trip ends and get total NOCS 
#' employment and population
att_o <- merge(all_o, att, by.x = "Origin", by.y = "taz", all.x = TRUE) %>% drop_na()
att_o$TEmp <- rowSums(att_o[,c(8:17)])

#' calculate average ratios
avg_LT_Rat <- sum(att_o$LT_Orig)/sum(att_o$TEmp)
avg_MD_Rat <- sum(att_o$MD_Orig)/sum(att_o$TEmp)
avg_HY_Rat <- sum(att_o$HY_Orig)/sum(att_o$TEmp)

#' calculate origin ratios
att_o <- transform(att_o, LT_Rat = ifelse(is.infinite(att_o$LT_Orig/(att_o$TEmp )) | is.na(att_o$LT_Orig/(att_o$TEmp)),
                                          avg_LT_Rat, att_o$LT_Orig/(att_o$TEmp )))
att_o <- transform(att_o, MD_Rat = ifelse(is.infinite(att_o$MD_Orig/att_o$TEmp) | is.na(att_o$MD_Orig/att_o$TEmp),
                                          avg_MD_Rat, att_o$MD_Orig/att_o$TEmp))
att_o <- transform(att_o, HY_Rat = ifelse(is.infinite(att_o$HY_Orig/att_o$TEmp) | is.na(att_o$HY_Orig/att_o$TEmp),
                                          avg_HY_Rat, att_o$HY_Orig/att_o$TEmp))

# plot the ratios
ggplot(att_o, aes(x=Origin, y=LT_Rat))+geom_line(color="grey")+geom_point(color="red")+
  ggtitle("Light Truck Ratios - Origin Trip Ends")

#' all destinations
###############################################################################
all_d <- merge(lt_d, md_d, by = "Destination", all = TRUE) %>% 
  merge(., hy_d, by = "Destination", all = TRUE) %>% 
  transform(., TotalDest = rowSums(.[, 2:4]))

#' now merge the attribute information to destination truck trip ends and get total NOCS employment
att_d <- merge(all_d, att, by.x = "Destination", by.y = "taz", all.x = TRUE) %>% drop_na()
att_d$TEmp <- rowSums(att_d[,c(8:17)])

#' calculate average ratios
avg_LT_Rat_d <- sum(att_d$LT_Dest)/sum(att_d$TEmp)
avg_MD_Rat_d <- sum(att_d$MD_Dest)/sum(att_d$TEmp)
avg_HY_Rat_d <- sum(att_d$HY_Dest)/sum(att_d$TEmp)

#' calculate origin ratios
att_d <- transform(att_d, LT_Rat = ifelse(is.infinite(att_d$LT_Dest/(att_d$TEmp)) | is.na(att_d$LT_Dest/(att_d$TEmp)),
                                          avg_LT_Rat_d, att_d$LT_Dest/(att_d$TEmp)))
att_d <- transform(att_d, MD_Rat = ifelse(is.infinite(att_d$MD_Dest/att_d$TEmp) | is.na(att_d$MD_Dest/att_d$TEmp),
                                          avg_MD_Rat_d, att_d$MD_Dest/att_d$TEmp))
att_d <- transform(att_d, HY_Rat = ifelse(is.infinite(att_d$HY_Dest/att_d$TEmp) | is.na(att_d$HY_Dest/att_d$TEmp),
                                          avg_HY_Rat_d, att_d$HY_Dest/att_d$TEmp))

# plot the ratios
ggplot(att_d, aes(x=Destination, y=LT_Rat))+geom_line(color="grey")+geom_point(color="red")+
  ggtitle("Light Truck Ratios - Destination Trip Ends")

###############################################################################
#' Now generate the future trip ends by multiplying the ratios for each truck 
#' type by the corressponding 2031 total employment
att31 <- read.csv("Zone Attributes_2031_MD.csv") %>% transform(., TEmp = rowSums(.[, c(4:13)]))

# Generate 2031 Origin trip ends by multiplying the ratios calculated in 2011 to 2031 emp numbers
# for each zone. First, limit the upper bound of the ratios to the median as not
# doing so results in significant growth in the matrices.
orig_rat <- att_o[, c(1, 48:50)]

# calculate the upper bounds for trip origins
cap_LT <- mean(orig_rat$LT_Rat) + sd(orig_rat$LT_Rat)*1.0
cap_MD <- mean(orig_rat$MD_Rat) + sd(orig_rat$MD_Rat)*1.0
cap_HY <- mean(orig_rat$HY_Rat) + sd(orig_rat$HY_Rat)*1.0

#' cap the origin ratios
orig_rat <- transform(orig_rat, LT_Rat = ifelse(LT_Rat > cap_LT, avg_LT_Rat, LT_Rat))
orig_rat <- transform(orig_rat, MD_Rat = ifelse(MD_Rat > cap_MD, avg_MD_Rat, MD_Rat))
orig_rat <- transform(orig_rat, HY_Rat = ifelse(HY_Rat > cap_HY, avg_HY_Rat, HY_Rat))

#' Now generate future trip ends
orig_31trips <- merge(orig_rat, att31, by.x = "Origin", by.y = "taz", all.x = TRUE) %>% 
  .[, c(1:4, 22, 46)] %>% 
  transform(., LT_Orig31 = LT_Rat*(TEmp)) %>% 
  transform(., MD_Orig31 = MD_Rat*TEmp) %>%
  transform(., HY_Orig31 = HY_Rat*TEmp) %>% 
  transform(., TotalOrigin31 = rowSums(.[, 7:9]))

# Generate 2031 Destination trip ends by multiplying the ratios calculated in 2011 to 2031 emp numbers
# for each zone
dest_rat <- att_d[, c(1, 48:50)]

# calculate the upper bounds for trip dests
cap_LT <- mean(dest_rat$LT_Rat) + sd(dest_rat$LT_Rat)*1.0
cap_MD <- mean(dest_rat$MD_Rat) + sd(dest_rat$MD_Rat)*1.0
cap_HY <- mean(dest_rat$HY_Rat) + sd(dest_rat$HY_Rat)*1.0

#' cap the origin ratios
dest_rat <- transform(dest_rat, LT_Rat = ifelse(LT_Rat > cap_LT, avg_LT_Rat_d, LT_Rat))
dest_rat <- transform(dest_rat, MD_Rat = ifelse(MD_Rat > cap_MD, avg_MD_Rat_d, MD_Rat))
dest_rat <- transform(dest_rat, HY_Rat = ifelse(HY_Rat > cap_HY, avg_HY_Rat_d, HY_Rat))

# Now generate future trip ends
dest_31trips <- merge(dest_rat, att31, by.x = "Destination", by.y = "taz", all.x = TRUE) %>% 
  .[, c(1:4, 22, 46)] %>% 
  transform(., LT_Dest31 = LT_Rat*(TEmp)) %>% 
  transform(., MD_Dest31 = MD_Rat*TEmp) %>%
  transform(., HY_Dest31 = HY_Rat*TEmp) %>%
  transform(., TotalDest31 = rowSums(.[, 7:9]))

# sum(orig_31trips$LT_Orig31)
# sum(orig_31trips$MD_Orig31)
# sum(orig_31trips$HY_Orig31)

#' bring the 2031 and 2011 origins and destinations in single files for comparison 
#' purpose and also populate those rows where in 2011 there was a trip end even though
#' the forecast shows zero. This is necessary to ensure that the furnessing goes thru
orig_31trips <- merge(orig_31trips, all_o, by = "Origin", all.x = TRUE) %>% 
  transform(., LT_Orig31 = ifelse(LT_Orig31 == 0, LT_Orig, LT_Orig31)) %>%
  transform(., MD_Orig31 = ifelse(MD_Orig31 == 0, MD_Orig, MD_Orig31)) %>%
  transform(., HY_Orig31 = ifelse(HY_Orig31 == 0, HY_Orig, HY_Orig31))

#' now add in the external trips. These will be just factored by 50% to represent the growth in 
#' employment
fin_31orig <- orig_31trips[,c(1,7:9)]
ext_o <- subset(all_o, Origin > 9400 & Origin < 9600) %>% .[, -5]  
  ext_o[2:4] <- 1.5*ext_o[, (2:4)] #' increase by 50% to account for employment growth
  colnames(ext_o) <- c("Origin", "LT_Orig31", "MD_Orig31", "HY_Orig31")
#bind it all together
fin_31orig <- rbind(fin_31orig, ext_o)

dest_31trips <- merge(dest_31trips, all_d, by = "Destination", all.x = TRUE) %>% 
  transform(., LT_Dest31 = ifelse(LT_Dest31 == 0, LT_Dest, LT_Dest31)) %>%
  transform(., MD_Dest31 = ifelse(MD_Dest31 == 0, MD_Dest, MD_Dest31)) %>%
  transform(., HY_Dest31 = ifelse(HY_Dest31 == 0, HY_Dest, HY_Dest31)) 

#' now add in the external trips. These will be just factored by 50% to represent the growth in 
#' employment
fin_31dest <- dest_31trips[,c(1,7:9)]
ext_d <- subset(all_d, Destination > 9400 & Destination < 9600) %>% .[, -5]  
ext_d[2:4] <- 1.5*ext_d[, (2:4)] #' increase by 50% to account for employment growth
colnames(ext_d) <- c("Destination", "LT_Dest31", "MD_Dest31", "HY_Dest31")
#bind it all together
fin_31dest <- rbind(fin_31dest, ext_d)

###################################################################################
#' write out the files
write.csv(fin_31orig, "2031CVOrigins.csv", row.names = FALSE)
write.csv(fin_31dest, "2031CVDests.csv", row.names = FALSE)

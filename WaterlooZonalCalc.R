library(reshape2)
library(dplyr)
require(PBSmapping)
require(maptools)
library(rgdal) 
library(sp)

#' set working director
wd <- setwd("c:/personal/r")

#' read the GGHM output of households that have auto sufficiency and income
#'  imputation values
hh <- read.csv("c:/personal/r/households_out.csv", stringsAsFactors = FALSE)

#' create IncCat field
hh <- transform(hh, IncCat = ifelse(hh$hhinc <= 60000, 1, 2)) 
  hh <- transform(hh, IncAuto = paste0(hh$IncCat, hh$auto_suff))

#' get number of instances as a function of TAZ and IncAuto categories
hh_group <- hh %>% group_by(taz, IncAuto) %>% summarise(count = n())

#' cast the above dataframe into a format that Bill can work with and we can 
#' compute shares of income and auto sufficiency
hh_incauto <- dcast(hh_group, taz ~ IncAuto, value.var = "count")
  # set NA to zero
  hh_incauto[is.na(hh_incauto)] <- 0

#' get list of column names for enumeration and get rid of the first column
ls <- c(colnames(hh_incauto)[2:7])

#' create list of column names
c_names <- c("Prop10", "Prop11", "Prop12", "Prop20", "Prop21", "Prop22")

#' make copy
hh_incauto2 <- hh_incauto

#' creat loop to generate proportion columns

for (i in 1: length(ls)) {
hh_incauto2[[c_names[i]]] <- hh_incauto[[ls[i]]]/(hh_incauto$`10`+ 
                                                hh_incauto$`11` + 
                                                hh_incauto$`12` + 
                                                hh_incauto$`20` + 
                                                hh_incauto$`21` + 
                                                hh_incauto$`22`)
}


#' read in the TAZ shapefile of the Region of Waterloo that has the corressponding
#' GGHM zone number in it. Then transfer the income and auto sufficiency calculations 
#' to the Waterloo zones.

#' read in the file
wat_ggh <- readOGR(wd, "TAZ1385_GGHMJoin")

# grab the data slot
wat_df <- wat_ggh@data

# Now join the IncAuto proportions from the GGHM to Waterloo
wat_df1 <- merge(wat_df, hh_incauto2, by.x = "TAZ_NO", by.y = "taz", all.x = TRUE)
  wat_df1[is.na(wat_df1)] <- 0
  
  wat_df2 <- transform(wat_df1, flag = ifelse(wat_df1$Prop10 == 0 & wat_df1$Prop11 == 0 & 
                                                wat_df1$Prop12 == 0 & wat_df1$Prop20 == 0 & 
                                                wat_df1$Prop21 == 0 | wat_df1$Prop22 == 0, 1,0 ))
  wat_df3 <- subset(wat_df2, flag == 0)


#' there are a bunch of NAs, which represent zones that had no values from the GGHM. Some
#' of these legitimately have no households, while the others are probably due to aggregation.
#' Further, we also need to account for the fact that in the future some of the zones could
#' undergo a transformation from non-hholds to household zones. So, we will develop
#' default proportions from the surrounding zones to add in values.

# get rows that have no NAs to compute the means of the proportions

means <- as.data.frame(t(colMeans(wat_df1[, 18:23], na.rm = TRUE)))

#' get row sums
msum <- rowSums(means)
#' the row sums do not add  up to 1. So, we need to scale them back again
means$Prop10 <- means$Prop10*(1/msum)
means$Prop11 <- means$Prop11*(1/msum)
means$Prop12 <- means$Prop12*(1/msum)
means$Prop20 <- means$Prop20*(1/msum)
means$Prop21 <- means$Prop21*(1/msum)
means$Prop22 <- means$Prop22*(1/msum)

#Now populate the values for all the zones that did not get a corressponding GGH zone value
wat_1 <- subset(wat_df2, flag ==1)

wat_1$Prop10 <- means$Prop10
wat_1$Prop11 <- means$Prop11
wat_1$Prop12 <- means$Prop12
wat_1$Prop20 <- means$Prop20
wat_1$Prop21 <- means$Prop21
wat_1$Prop22 <- means$Prop22

#' final proportions file for Waterloo

wat_df_final <- rbind(wat_1, wat_df3)
  wat_df_final$tot <- rowSums(wat_df_final[18:23])
  
  write.csv(wat_df_final, "c:/personal/r/proportionsWaterloo.csv")
  
#' create ggplot showing frequency distrbution within the Region, but first batch in the 2011 Pop
pop_11 <- read.csv("c:/projects/Waterloo/Pop_2011.csv", stringsAsFactors = FALSE)
ggplot

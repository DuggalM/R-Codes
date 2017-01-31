#install.packages("dplyr")

library(dplyr)


#### Metadata

#' Title: "Pivot Table Comparisons for Validating GGHM V4 Assignment"
#' Author: "Mausam Duggal"
#' Date: "May 12th, 2015"

# set working directory to personal directory, for convenience

setwd("c:/personal/R")

#### read text file. Each EMME scenario will have its own .txt file
#### the text file will be outputted frmom EMME modeler after running the assingment

PTsc1 <- read.table("C:/Personal/R/Sc5.txt", header=TRUE)


# add count column. Not necessary,
# but just to understand the frequency of each category, like VDF and Volgrp

PTsc1 <- PTsc1 %>% mutate(occ=1)

# populate Volgrp with discrete values. 
# These Values can be changed or expanded based on the thresholds
# for evaluation

PTsc1$volgrp[PTsc1$X.amphr<1000]<-1
PTsc1$volgrp[PTsc1$X.amphr>=1000 & PTsc1$X.amphr<2500]<-2
PTsc1$volgrp[PTsc1$X.amphr>=2500 & PTsc1$X.amphr<5000]<-3
PTsc1$volgrp[PTsc1$X.amphr>=5000 & PTsc1$X.amphr<10000]<-4
PTsc1$volgrp[PTsc1$X.amphr>=10000]<-5

## Provide a summary of statistics for inspection

summary(PTsc1)

###### prepare pivot table, by summarizing across VDF's
###### t1...t3 represents temporary variables

t1 <- PTsc1 %>% group_by(vdf)%>% summarise(volau=sum(volau), amphr=sum(X.amphr), count1=sum(occ))

# add comparison columns
# ModeObs is modeled vs observed
# GEH is the statistic, with
# GEH<5 is good; GEH>5 and GEH<10 is ok, GEH >10 is poor
t2 <- t1 %>% mutate(ModObs = volau/amphr)
t3 <- t2 %>% mutate(GEH = sqrt(2*((volau-amphr)^2/(volau+amphr))))

# display results with table frame wrapper for better visualization
t3 <- tbl_df(t3)
t3

## Provide a summary of statistics for inspection
#summary(t3)


###### prepare pivot table, by summarizing across Volume Groups
###### t5...t9 represents temporary variables

t5 <- PTsc1 %>% group_by(volgrp)%>% summarise(volau=sum(volau), amphr=sum(X.amphr), count1=sum(occ))

# add comparison columns
# ModeObs is modeled vs observed
# GEH is the statistic, with
# GEH<5 is good; GEH>5 and GEH<10 is ok, GEH >10 is poor
t6 <- t5 %>% mutate(ModObs = volau/amphr)
t7 <- t6 %>% mutate(GEH = sqrt(2*((volau-amphr)^2/(volau+amphr))))

# display results with table frame wrapper for better visualization
t7 <- tbl_df(t7)
t7

# Provide a summary of statistics for inspection
summary(t7)


#### fit least-squares line across the data
#### and plot regression line 

# create least squares
fit = lm(volau~X.amphr, data=PTsc1)
coef(fit)
fit

# create plots with regression line using geom_smooth
library(ggplot2)
p2 <- ggplot(PTsc1, aes(x=volau, y=X.amphr))
p2 + geom_point(aes(color= "red")) +
    geom_smooth(method="lm", se=FALSE)


library(dplyr)
library(ggplot2)
library(reshape2)
library(manipulate)
library(gridExtra)
#library(fBasics)

#stitch("c:/personal/r/marine_chord.R", system.file("misc", "knitr-template.Rhtml", package = "knitr"))

############################## Metadata ########################################

#' Title: "Count Frequency comparison by CD"
#' Author: "Mausam Duggal"
#' Date: "Sept 16th, 2015"

# set working directory to personal directory, for convenience
setwd("c:/personal/R")

# call functionlist.R file for accessing the functions. This keeps
# the data wrangling portion of the script easy to read
# Function files used are listed below:
#
#   readfile - reads csv files
#   add.field - adds ID field 
#   summary - performs the main data summary tasks
#   balloonplot - ggplot2 code for making balloon charts
source("functionlist.R")

############################## Read Data ########################################

# read MasterJoin1.csv file. This file contains CDUID, CDNAME, CLASS, 
# Join_count variables a field value of 1 in Join_Count is used to summarize frequency
join <- readfile("C:/Personal/R/MasterJoin1.csv")

# Read CountPost file created by running a Unity (one's) matrix
UM <- readfile("C:/Personal/R/AllCountPosts_UnityMatrix.csv")

# Read CountPost file created by running GGHV3.0 Drive matrix
AD <- readfile ("C:/Personal/R/AllCountPosts_GGHV3.csv")

############################## Run the Analysis ################################

# run summary function for Unity matrix
unity <- summary(UM, join)
balloondata <- summary(UM, join)[[5]]  # get data for balloon plot
unity.plot <- balloonplot(balloondata)

# run summary function for GGHV 3.0 Auto Drive matrix
GGHV3 <- summary(AD, join)
balloondata1 <- summary(AD, join)[[5]]  # get data for balloon plot
GGHV3.plot <- balloonplot(balloondata1)

# generate scatter plot of volumes using the Unity and GGHV3 matrices
# batchin data for all links 
UM1 <- readfile("C:/Personal/R/AllLinks_unitymatrix.csv")

AD1 <- readfile ("C:/Personal/R/AllLinks_GGHV3.csv")

# use add field function to populate unique ID field
UM2 <- add.field (UM1)
AD2 <- add.field (AD1)

# merge UM2 and AD2 using ID to generate one dataset for quadrant calculation
mer1 <- merge(UM2, AD2, by="ID")

# set user defined intercept values
xint = median(mer1[["volau.x"]])
yint = median(mer1[["volau.y"]])

# only use volau values of links for scatter plot
p1 <- ggplot(mer1, aes(x = volau.x, y = volau.y), colour=vdf)
p1 + geom_point(aes(color = factor(vdf.x))) + geom_vline(xintercept = xint, color="red") + 
  geom_hline(yintercept = yint, color="red") + labs(x="Unity Matrix", 
                                                    y = "GGHV3 Auto Drive Matrix")

# make scatter plot using counts and lane kms, grouped by county and functional class

pivot1 <- summary(AD,join)[[6]]    # get pivot table for counts and lanekms
write.csv(pivot1, file="Pivot1.csv")

# pivot table
p2 <- ggplot(pivot1, aes(x=lane.kms, y = cnts))
p2 + geom_point(aes(color = factor(vdf)))

p3 <- ggplot(pivot1, aes(x=lane.kms, y = cnts))
p3 + geom_point(aes(color = factor(CDNAME)))  


############################## Transfer quadrant information to count locations #

# transfer quadrant information to joined dataset
mer1 <- mer1 %>% mutate(Quad=0)

# populate quadrant information
mer1$Quad[mer1$volau.x<xint & mer1$volau.y<yint]<-1
mer1$Quad[mer1$volau.x<xint & mer1$volau.y>yint]<-2
mer1$Quad[mer1$volau.x>xint & mer1$volau.y>yint]<-3
mer1$Quad[mer1$volau.x>xint & mer1$volau.y<yint]<-4

#basicStats(mer1$Quad)
# call merged files from above summary function to attach quadrant information
mer2 <- summary(UM,join)[[2]]
mer3 <- summary(AD, join)[[2]]

mer4 <- merge(mer2, mer1, by ="ID")  # join unity matrix file to master
mer5 <- merge(mer3, mer1, by ="ID")  # join GGHV3 matrix file to master 

############################## Write dataset ###################################

write.csv(mer4[,c("i", "j", "ID", "Quad")], file="finalunity.csv")



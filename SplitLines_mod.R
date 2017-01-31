library(spatialEco)
library(rgeos)
library(rgdal)
library(maptools)
library(spatstat)
library(dplyr)
library(sp)
library(reshape)

##########################################################################################

#' Title: "Split line segments in to links less than 2 km or any other user value"
#' Programmer: "Mausam Duggal, Systems Analysis Group, WSP|PB"
#' Algorithm Credits: "Mausam Duggal, Systems Analysis Group, WSP|PB" 
#' Date: "Feb 26th, 2016"

#########################################################################################

wd <- setwd("C:/Projects/Province-wide/Sundar EMME")

#' user defined threshold for generating point spacings
thresh <- 2000

#########################################################################################

#' read polyline shapefile to be split

line <- readOGR(wd, "UpdatedFeb25_v2_only_Ontario_2kmplus")
line.shp <- proj4string(line)

# dont select links in GGHM and TRANS
line <- line[line@data$Geog != "GGHM" & line@data$Geog != "TRANS", ]

# get number of records in shapefile
line.lst <- as.list(line@lines)
enum <- length(line.lst)

# populate unique column with a sequence for indexing
line@data$Unique <- seq(1, enum)
d <- line@data

#' create empty dataframe to receive wide format
c3<- data.frame(matrix(nrow = enum, ncol=0))
c3 <- line@data

#' now calculate the euclidean distance between the end and start points
#' if it exceed the user defined threshold then add a mid-point

c3.dist <- c3    #### make copy

#' calculate euclidean distance between end and start points of a line
c3.dist$euc <- sqrt((c3.dist$xstart-c3.dist$xend)^2+
                      (c3.dist$ystart-c3.dist$yend)^2)

#' generate midpoint for links

# select all links less than 4000 m and divide into half
c3.dist.rnd1 <- subset(c3.dist, c3.dist$euc <= 4000)
c3.dist.rnd1$xmid <- (c3.dist.rnd1$xstart + c3.dist.rnd1$xend)/2
c3.dist.rnd1$ymid <- (c3.dist.rnd1$ystart + c3.dist.rnd1$yend)/2

# only keep the new mid point ids
# this list will be merged with the rest of 
# of the points to create a spatialPoints dataframe
c3.dist.rnd1 <- subset(c3.dist.rnd1, select = c(xmid, ymid))    #### set for merging
write.csv(c3.dist.rnd1, file="splitpointsrnd1.csv")


# select all links above 6000 m and take mid points
c3.dist.rnd2 <- subset(c3.dist, c3.dist$euc > 4000)
c3.dist.rnd2$xmid <- (c3.dist.rnd2$xstart + c3.dist.rnd2$xend)/2
c3.dist.rnd2$ymid <- (c3.dist.rnd2$ystart + c3.dist.rnd2$yend)/2 

# save this set of points for the next round of analysis
c3.dist.rnd3 <- c3.dist.rnd2

# save only the mid-points
c3.dist.rnd2 <- subset(c3.dist.rnd2, select = c(xmid, ymid))    #### set for merging
write.csv(c3.dist.rnd2, file="splitpointsrnd2.csv")

#' estimate euclidean distance between mid and start points from round 2
#' estimate euclidean distance between mid and end points from round 2

# first estimate euclidean distances between start, mid, and end points
# when euclidean distances do not exceed 4000 m then finish the code with
# round 4 and round 5 points, otherwise continue on.
c3.dist.rnd3$eucstart <- sqrt((c3.dist.rnd3$xstart-c3.dist.rnd3$xmid)^2+
                                (c3.dist.rnd3$ystart-c3.dist.rnd3$ymid)^2) 
c3.dist.rnd3$eucend <- sqrt((c3.dist.rnd3$xend-c3.dist.rnd3$xmid)^2+
                              (c3.dist.rnd3$yend-c3.dist.rnd3$ymid)^2)

#' create X and Y coordinates for mid points

# Midpoints between mid and end points
c3.dist.rnd3$xmid <- (c3.dist.rnd3$xstart + c3.dist.rnd3$xmid)/2
c3.dist.rnd3$ymid <- (c3.dist.rnd3$ystart + c3.dist.rnd3$ymid)/2 
c3.dist.rnd4 <- subset(c3.dist.rnd3, select = c(xmid, ymid))    #### set for merging
write.csv(c3.dist.rnd4, file="splitpointsrnd4.csv")

# Midpoints between mid and start points
c3.dist.rnd3$xmid <- (c3.dist.rnd3$xstart + c3.dist.rnd3$xmid)/2
c3.dist.rnd3$ymid <- (c3.dist.rnd3$ystart + c3.dist.rnd3$ymid)/2 
c3.dist.rnd5 <- subset(c3.dist.rnd3, select = c(xmid, ymid))    #### set for merging
write.csv(c3.dist.rnd5, file="splitpointsrnd5.csv")


#' finally merge points
all <- rbind(c3.dist.rnd1, c3.dist.rnd2, c3.dist.rnd4, c3.dist.rnd5)

write.csv(all, file="splitpoints.csv")


test <- group_by(c3, xstart)

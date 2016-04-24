library(sp)
library(spatialEco)
library(rgeos)
library(rgdal)
library(dplyr)
library(data.table)

##########################################################################################

#' Title: "Create Activity-based Centroids for QT Grid Cells"
#' Programmer: "Mausam Duggal, Systems Analysis Group, WSP|PB"
#' Date: "Dec 12th, 2015"

#########################################################################################
#' set working directory
wd <- setwd("c:/personal/R")

#' input files for Quad tree and Truck Points 
geog.name <- "QuadTree"
#ps.name <- "TruckGPS_UTM"
ps.name <- "Revised_DAs_centroids_3"

########################################################################################
#' read QT and points shapefiles
#' need to ensure that the shapefiles are in the same projection
#' system, otherwise the analysis will be incomplete
qt <- readOGR(wd, geog.name)
  qt.points <- qt    # make copies
gps <- readOGR(wd, ps.name)
  gps.points <- gps    # make copies

########################################################################################
#' Add weighted values by multiplying X and Y coordinates with value field
#' and then carry out a point in polygon analysis to transfer GRID_ID to points data

#' MAKE SURE TO HAVE A "VAL" FIELD IN THE POINTS SHAPEFILE INSTEAD OF GPS OR DU

#' add weights
gps@data$wtx <- ifelse(gps@data$val*gps@data$Xcoord==0, gps@data$Xcoord*1, 
                       gps@data$val*gps@data$Xcoord)
gps@data$wty <- ifelse(gps@data$val*gps@data$Ycoord==0, gps@data$Ycoord*1, 
                         gps@data$val*gps@data$Ycoord)

gps.df <- gps@data

#' point in poly using Spatial Eco
pts <- point.in.poly(gps, qt)
  # save pts data slot
   cc <- pts@data

#' create new value field to store 1's for all records that have 0 in them
#' if this is not done than the revised centroid coordinates will be incorrect
   activity.cc <- cc
   
   activity.cc = within(activity.cc, {
     valrev = ifelse(val == 0, 1, val)
   })
 
#' group cc data by ID and summarize across valrev, wtx, and wty
#' make sure that the ID field is not named as ID_1
activity.cc <- activity.cc %>% group_by(ID) %>% 
     summarize(wtx = sum(wtx), wty = sum(wty), val = sum(valrev))

#' add activity centroid X and Y coords

activity.cc = within(activity.cc, {
  X = ifelse(val == 0, activity.cc$wtx, activity.cc$wtx/activity.cc$val)
  Y = ifelse(val == 0, activity.cc$wty, activity.cc$wty/activity.cc$val)
})
  
  
#' make activity centroids spatial points
coords <- cbind(activity.cc$X, activity.cc$Y)
data <- as.data.frame(activity.cc) 
  sp = SpatialPoints(coords, proj4string = qt@proj4string)
  spdf = SpatialPointsDataFrame(sp, data)
  ss <- spdf@data
#' write shapefile
  writeOGR(spdf, layer = paste0(ps.name,"activity_centroids1"), wd, driver="ESRI Shapefile", 
           overwrite_layer=T )
  



library(sp)
library(spatialEco)
library(rgeos)
library(rgdal)
library(dplyr)
require(PBSmapping)
require(maptools)

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
ps.name <- "ActivityCentroids"

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
  writeOGR(spdf, layer = paste0(ps.name,"activity_centroids_Processed"), wd, 
           driver="ESRI Shapefile", overwrite_layer=T )

###############################################################################
#' Now select all those QTs that did not have an activity centroid. Estimate their
#' geometric centroid and merge with activity centroids processed file
###############################################################################

ss$processed <- 1

# merge the points from SS to QT to identify the Quads that do need a 
# geometric centroid
qt.points@data = data.frame(qt.points@data, 
                            ss[match(qt.points@data$ID, ss$ID),]) 

#' set NA values to zero
qt.points@data[is.na(qt.points@data)] <- 0

  #' Now subset for points that need a centroid
  qt.points.gc <- qt.points[qt.points@data$processed == 0, ]

  #' write out shapefile for input into the Centroid Script
  writeOGR(qt.points.gc, layer = paste0("GC_QT"), wd, 
           drive = "ESRI Shapefile", overwrite_layer=T)
  
  #' create centroids
  centers <- SpatialPointsDataFrame(gCentroid(qt.points.gc, byid=TRUE), 
                                        qt.points.gc@data, match.ID=FALSE)
 
###############################################################################
#' Create final centroid file for all the Quads
############################################################################### 

  #' Now combine the geometric centroids with the acitivy centroids

  #'   strip unnecessary fields in CSD and DA level shapefiles
  centers <- centers[, c("ID", "wtx", "wty", "val", "Y", "X")]

  # make final QT activity centroid file
  final.centroids <- rbind(spdf, centers)
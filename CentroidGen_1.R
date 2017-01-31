library(sp)
library(spatialEco)
library(rgeos)
library(rgdal)
library(maptools)
library(ggmap)
library(dplyr)
library(data.table)

##########################################################################################

#' Title: "Create Activity-based Centroids for QT Grid Cells"
#' Programmer: "Mausam Duggal, Systems Analysis Group, WSP|PB"
#' Algorithm Credits: "Mausam Duggal, Systems Analysis Group, WSP|PB" 
#' Date: "Dec 12th, 2015"

#########################################################################################
#' set working directory
wd <- setwd("c:/personal/R")

#' input files for Quad tree and Truck Points 
geog.name <- "QuadTree"
ps.name <- "TruckGPS_UTM"

########################################################################################
#' read QT and points shapefiles
#' need to ensure that the shapefiles are in the same projection
#' system, otherwise the analysis will be incomplete
qt <- readOGR(wd, geog.name)
  qt.points <- qt    # make copies
gps <- readOGR(wd, ps.name)
  gps.points <- gps    # make copies

########################################################################################
#' select only grids that have a GPS Truck point in them
#' These are the only grids that qualify for a freight activity centroid

qt.points$GPS <- over(qt.points, gps["val"], fun = sum)    # sum up the value field
  qt.points <- sp.na.omit(qt.points)    # take out NAs
  proj4string(qt.points) <- CRS(proj4string(qt))    # ensure that the projection is correct

#' create list of QT grids for enumeration
qt.master.lst <- as.list(qt.points$ID)

#' select first QT and create a dataframe to receive the activity centroids as 
#' they are created. The first activity centroid is created outside a loop
 
qt.first.grid <- qt.master.lst[1]    # inovoke first instance of the QT grid

qt.first.shp <- qt.points[qt.points@data$ID == qt.first.grid,]    # select first QT
  proj4string(qt.first.shp) <- CRS(proj4string(qt))
  plot(qt.first.shp)
  s <- qt.first.shp@data

#' select points that fall inside first grid

gps.first <- gps.points # make copy
gps.first$Gid <- over(gps.first, qt.first.shp["ID"], fun = mean)
  proj4string(gps.first) <- CRS(proj4string(qt))
    gps.first <- sp.na.omit(gps.first)    # take out NAs
    points(gps.first)

########################################################################################
#' With the first QT and the GPS points in it identified, undertake the generation of the 
#' first activity centroid. 

  #+ make copy of gps.first data slot
     activity.cen <- gps.first@data

  #+ create new variables for assigning scaled up X and Y coordinates
  #+ if value field is zero, default to an unweighted mean calculation
  #+ multiplying with 1 instead of value
  activity.cen$wtx <- ifelse(activity.cen$Xcoord*activity.cen$val==0, activity.cen$Xcoord*1, 
                             activity.cen$Xcoord*activity.cen$val)
  activity.cen$wty <- ifelse(activity.cen$Ycoord*activity.cen$val==0, activity.cen$Ycoord*1, 
                             activity.cen$Ycoord*activity.cen$val)
  
  #+ create variable to carry total GPS points in grid in question 
  #+ set num to 1 if no value is present in the GPS points. This
  #+ will result in the activity centroid defaulting to an average
  #+ of X and Y coordinates of all points in the grid
  num <- ifelse(sum(activity.cen$val)>0, sum(activity.cen$val), 1)
  #+ create new variables for assigning weighted up X and Y coordinates
  activity.cen$cen.x <- sum(activity.cen$wtx)/num
  activity.cen$cen.y <- sum(activity.cen$wty)/num

#' save new activity-based centroid. First create empty list of centroids 
#' and summarize to make unique centroid points, especially in the case of
#' multiple points in a grid
cen <- as.data.frame(cbind(activity.cen$cen.x, activity.cen$cen.y, activity.cen$Gid)) 
cen1 <- cen %>% 
  group_by(activity.cen$cen.x, activity.cen$cen.y) %>% 
  summarise(ID=mean(ID))

points(cen1, pch=16, col = "blue")

########################################################################################
#' Now enumerate over the remaining grids within the QT that have a GPS point
#' and estimate the activity centroid while row binding it to the Cen dataframe

lst <- qt.master.lst

for (i in 2:length(lst)) {
  
  qt.next.shp <- qt.points[qt.points@data$ID == lst[i],]    # select first QT
  proj4string(qt.next.shp) <- CRS(proj4string(qt))

  #' select points that fall inside first grid
  
  gps.next <- gps.points # make copy
  gps.next$Gid <- over(gps.next, qt.next.shp["ID"], fun = mean)
  proj4string(gps.next) <- CRS(proj4string(qt))
  gps.next <- sp.na.omit(gps.next)    # take out NAs
  

  #+ make copy of gps.next data slot
  activity.cen.n <- gps.next@data
 
  #+ create new variables for assigning scaled up X and Y coordinates
  #+ if value field is zero, default to an unweighted mean calculation
  #+ multiplying with 1 instead of value
  activity.cen.n$wtx <- ifelse(activity.cen.n$Xcoord*activity.cen.n$val==0, activity.cen.n$Xcoord*1, 
                             activity.cen.n$Xcoord*activity.cen.n$val)
  activity.cen.n$wty <- ifelse(activity.cen.n$Ycoord*activity.cen.n$val==0, activity.cen.n$Ycoord*1, 
                             activity.cen.n$Ycoord*activity.cen.n$val)
  
  #+ create variable to carry total GPS points in grid in question 
  #+ set num to 1 if no value is present in the GPS points. This
  #+ will result in the activity centroid defaulting to an average
  #+ of X and Y coordinates of all points in the grid
  num <- ifelse(sum(activity.cen.n$val)>0, sum(activity.cen.n$val), 1)
  #+ create new variables for assigning weighted up X and Y coordinates
  activity.cen.n$cen.x <- sum(activity.cen.n$wtx)/num
  activity.cen.n$cen.y <- sum(activity.cen.n$wty)/num

  #' save new activity-based centroid
  
  cen2 <- as.data.frame(cbind(activity.cen.n$cen.x, activity.cen$cen.y, activity.cen$Gid)) 
  cen3 <- cen2 %>% 
    group_by(activity.cen.n$cen.x, activity.cen.n$cen.y) %>% 
    summarise(ID=mean(ID))

  cen1[i,] <- cen3
  
}

#' rename columns of final list of acitivity centroid
setnames(cen1, old = c('activity.cen$cen.x','activity.cen$cen.y'), new = c('X','Y'))

#' make activity centroids spatial points
coords <- cbind(cen1$X, cen1$Y)
data <- as.data.frame(coords) %>% setnames(., old = c('V1', 'V2'), new = c('X', 'Y'))
  sp = SpatialPoints(coords)
  spdf = SpatialPointsDataFrame(sp, data)
#' write shapefile
  writeOGR(spdf, layer = "activity_centroids", wd, driver="ESRI Shapefile", 
           overwrite_layer=T )
  



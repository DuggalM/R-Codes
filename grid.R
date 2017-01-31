library(spatialEco)
library(PBSmapping)
library(rgeos)
library(rgdal)
library(maptools)
library(ggmap)
library(sp)
##########################################################################################

#' Title: "Provincial Raster Grid Creation and Spatial Analysis"
#' Author: "Mausam Duggal"
#' Date: "Oct 17th, 2015"

#########################################################################################

# set working directory
wd <- setwd("c:/personal/R")

# call functionlist.R file for accessing the functions. This keeps
# the data wrangling portion of the script easy to read
# Function files used are listed below:
#
#   readfile - reads csv files
#   add.field - adds ID field 
#   summary - performs the main data summary tasks
#   balloonplot - ggplot2 code for making balloon charts
source("functionlist.R")

# reduce margins for plotting
par(mar=c(0,0,0,0))

################################################################################################
################################################################################################
# create list or named vectors for user to change if required

cl <- c(50000, 50000, 100000, 200000, 200000, 50000, 50000, 50000)    # grid extent for each clip, can be modified by user
names(cl) <- c("clip1", "clip2", "clip3", "clip4", "clip5", "clip6", "clip7", "clip8")    # can be modified by user

pre <- as.name("GGHTAZ_CSD_combined_") # prefix of each file before clip name

################################################################################################
############################## Read File and Define Grid ########################################
# read province wide polygon and create grid using the bbox slot of the shapefile

for (i in 1: length(cl)) {
  # start loop using clip list or named vectors
  # read Provincial shapefile
  prov <- readshp(wd, paste0("GGHTAZ_CSD_combined_", names(cl)[i]))
  # declare projection
  crs.shp <- proj4string(prov)
  crs.shp
  U <- prov
  #slotNames(U)
  #prov1 <- prov@data
  
  # get bounding box coordinates of the province (U)
  minx=U@bbox[1,1]
  maxx=U@bbox[1,2]
  miny=U@bbox[2,1]
  maxy=U@bbox[2,2]
  
  # define custom square grid
  
  l=cl[i] # size in meters
  
  Lx=maxx-minx
  Ly=maxy-miny
  
  Nbx=round((Lx+l)/l)  # number of rows
  Nby=round((Ly+l)/l)  # number of cols
  
  # create grid
  G=GridTopology(c(minx, miny), c(l,l), c(Nbx, Nby))
  G=SpatialGrid(G)
  G=as(G, 'SpatialPolygons')
  
  # extract polygon Id's to make dataframe
  Gid <- sapply(slot(G, "polygons"), function(x) slot(x, "ID")) 
  
  # Create dataframe with correct rownames
  G.df <- data.frame( ID=1:length(G), row.names = Gid)  
  
  G <- SpatialPolygonsDataFrame(G, data=G.df)    # make Polygondataframe to export as shapefile
  proj4string(G) <- CRS(proj4string(prov))

  # plot grid
  #plot(G, axes=FALSE,col=rgb(0,0,0,0), density=0)
  
  # Export grid
  writeOGR(G, layer = paste0('grids_', names(cl)[i]), wd, driver="ESRI Shapefile", overwrite_layer=T)
  
  ############################## Batch in Grid and create Centroids ########################################
  # create a centroid for each grid cell, spatially join it to each grid and transfer its ID
  
  # batch in grid.shp
  grid <- readshp(wd, paste0("grids_", names(cl)[i]))
  #plot(grid, col ="red")
  grid.ps <- SpatialPolygons2PolySet(grid)    # convert to PBSmapping polyset obj
  
  # calculate centroids using PBSmapping package
  grid.centroids <- calcCentroid(grid.ps, rollup=1)
  
  # combine centroids to grid and transfer grid ID
  grid.with.cent <- cbind(grid.centroids, grid)
  # project data in UTM
  coordinates(grid.with.cent) <- c("X", "Y")
  
  # plot(grid.with.cent, pch = 19)
  gc <- as.data.frame(grid.with.cent) # make into regular data frame
  proj4string(grid.with.cent) <- CRS(proj4string(prov))
  proj4string(grid.with.cent)
  
  # write out shapefile
  writeOGR(grid.with.cent, layer = paste0('GridCentroids_', names(cl)[i]), wd , driver="ESRI Shapefile", overwrite_layer=T)
  
  ############################## Batch in GridCentroids and spatially join ########################################
  # spatially join the above GridCentroids to TAZs in the GGH and CSDs for the rest of Ontario
  
  # read GGHTAZs and CSD combined shapefile
  comb <- readshp(wd, paste0("GGHTAZ_CSD_combined_", names(cl)[i]))
  plot(comb, col ="black")
  
  # clip grid centroids to combined boundary
  grid.with.cent.sub <- grid.with.cent[comb, ]   # subset centroids to province
  points(grid.with.cent.sub, col ="red")
  
  # point-in-polygon using OVER
  h <- grid.with.cent.sub <- over(grid.with.cent.sub, comb)   # report datafile showing spatial join info

    # write CSV file
  write.csv(h, file = paste0("pointinpoly", names(cl)[i], ".csv"))
  }
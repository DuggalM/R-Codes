library(raster)
library(sp)
library(rgdal)
library(spatialEco)
library(dplyr)

#######################################################################

#' This script starts by using the NorthernLights outputs (shapefiles) and 
#' builds the rest of the GIS procedures that are necessary to feed in to the
#' Centroid Generation script.
#' Title: "Fills the gap between the NorthernLights and Centroid Gen"
#' Programmer: "Mausam Duggal, Systems Analysis Group, WSP|PB"
#' Date: "May 27th, 2016"

#' set working directory and save all the files required for this procedure
#' in the working folder.
    
wd <- setwd("c:/personal/r")

#' declare all the file names that will be needed during this process, other than 
#' the NorthernLight Outputs, which will be batched in directly. These are the following:

nl <- "OntarioAll_20_Merge2"    # NorthernLights Shapefile of pixel points
qt <- "QuadTree"                # Quad Tree
csd.pol <- "CSD"                # Census sub-division polygons
da.pol <- "DA"                  # Census DA polygons
da.pt <- "DA_Centroids4"        # tCensus DA Centroids
pop <- "persons"                # PopSyn3
hh <- "households"              # PopSyn3
gghm.pt <- "GGHM_Cen"           # GGHM zone centroid
gghm.pol <- "GGH_TAZ"           # GGHM TAZ polygon

#' Also declare any user defined variables that are needed for the process
 

area.thresh <- 14.0625    # this represents the area of a 3.75 Km Quad. 

###############################################################################
#' Step 0: Batchini all the files and test for projection systems consistency
###############################################################################

#' Start with batching in the shapefiles and test if the projection systems
#' are the same. If not, stop script from proceeding.

nl.20 <- readOGR(wd, nl)
qt.final <- readOGR(wd, qt)
csd.poly <- readOGR(wd, csd.pol)
da.poly <- readOGR(wd, da.pol)
da.cen <- readOGR(wd, da.pt)
gghm.cen <- readOGR(wd, gghm.pt)
gghm.poly <- readOGR(wd, gghm.pol)


  # Check if the projection systems for the two Quad Tree and Nightlights are the same.
  # If not the same, then return error and stop()

  crs.nl.20 <- proj4string(nl.20)  
  crs.qt.final <- proj4string(qt.final)
  crs.csd.poly <- proj4string(csd.poly)  
  crs.da.poly <- proj4string(da.poly)  
  crs.da.cen <- proj4string(da.cen)  
  crs.gghm.cen <- proj4string(gghm.cen) 
  crs.gghm.poly <- proj4string(gghm.poly)

  # check if the projections are the same
  {
    if (crs.nl.20 != crs.qt.final || crs.nl.20 != crs.csd.poly || 
        crs.nl.20 != crs.da.poly  || crs.nl.20 != crs.da.cen || 
        crs.nl.20 != crs.gghm.cen || crs.nl.20 != crs.gghm.poly) 
  stop("Projection systems are not the same. 
       Please make them same before proceeding")
  }

  #' Start cross-checking for missing attribute information in files and raise
  #' errors for user to fix it.
  
    # QT shapefile check
  {
    col.check <- "Area" 
    col <- names(qt.final@data) # get names from the data slot of the Quad Tree shapefile
    
    if(intersect(col.check, col) != "Area") 
      stop("Area field for Quad Cells is missing")
  }
  
  # CSD Shapfile check
  {
    col.check <- "CSDUID" 
    col <- names(csd.poly@data) # get names from the data slot of the CSD shapefile
    
    if(intersect(col.check, col) != "CSDUID") 
      stop("CSDID field in CSD shapefile is missing")
  }
  
  # DA Shapfile check
  {
    col.check <- c("CSDUID") 
    col <- names(da.poly@data) # get names from the data slot of the CSD shapefile
    
    if(intersect(col.check, col) != "CSDUID") 
      stop("CSDUID field in DA shapefile is missing")
    
    col.check <- c("DAUID")
    
    if(intersect(col.check, col) != "DAUID") 
      stop("DAUID field in DA shapefile is missing")
    
    col.check <- c("GGH")
    col <- names(da.cen@data)
    
    if(intersect(col.check, col) != "GGH") 
      stop("GGH field in DA Centroids shapefile is missing, which identifies 
           if the DA belongs to the GGH Area")
    
    gg.check <- 12611
    
    if(sum(da.poly@data$GGH != gg.check))
       stop("The number of DA polygons within the GGH area is incorrect. 
            It should be exactly 12611")
    
    if(sum(da.cen@data$GGH != gg.check))
      stop("The number of DA centroids within the GGH area is incorrect. 
           It should be exactly 12611")
    
    } 
  
  # strip unnecessary fields in CSD and DA level shapefiles
  csd.poly <- csd.poly[, c("CSDUID", "CSDNAME", "GGH")]
  da.poly <- da.poly[, c("CSDUID", "DAUID", "GGH", "dwell")]
  da.cen <- da.cen[, c("CSDUID", "DAUID", "GGH", "dwell", "X", "Y")]
  
  
###############################################################################
#' Step 1. Select NorthernLight pixel points that fall in Quad Tree cells that 
#' are 7.5Km and above. 
#' Also discard NorthernLight pixel points that are within the GGH TAZs
###############################################################################

# create a subset of Quad Trees that are 7.5km and above using the 
# area field in the data slot
  
qt.final.sub <- qt.final[qt.final@data$Area >= area.thresh, ]    # QuadTree subset

  #' Do Point in Polygon analysis and only select those NorthernLight pixel 
  #' points that lie within the subset QUadTree
  nl.20.sub <- nl.20[qt.final.sub, ]
  print(nrow(nl.20.sub@data))
  
  #' Now also remove those pixel points that lie within the GGHM. 
  #' The points left after this spatial subsetting are those that will be carried 
  #' forward. Use the DAs that lie within the GGH to do this analysis for sake of 
  #' simplicity.
  
  #' make DA subset that belongs to GGH to remove pixel points that fall inside
  #' GGH area
  
  da.poly.ggh <- da.poly[da.poly@data$GGH == 1, ]
  
  nl.20.sub = nl.20.sub[is.na(over(nl.20.sub,as(da.poly.ggh,"SpatialPolygons"))),]
    print(nrow(nl.20.sub@data))
    
    #' Write the NOrthernLights pixel points that are outside the GGH area and fall
    #' within a QUad cell of 7.5km or greater
    writeOGR(nl.20.sub, layer = paste0("NorthernLights_LeftOver"), wd, 
             drive = "ESRI Shapefile", overwrite_layer=T)
    
 ###############################################################################
#' Step 3: Assign a CSDUID to each of the pixel points that were 
#' subsetted to the QTs that were 7.5 km and beyond. Then distribute the pop
#' and households to each pixel point based on its RGB value
###############################################################################  

#' Transfer DA information to pixel points based on point in polygon
pix.csd.da <- point.in.poly(nl.20.sub, da.poly)


  #' Now summarize on DAUID to estimate the number of pixel points within 
  #' each DA and their total pixel value, and join it back    
  pix.sum.da <- pix.csd.da@data %>% group_by(DAUID) %>%
    summarise(PixCnt = n(), TotPixval = sum(PixVal))
  
  pix.csd.da@data = data.frame(pix.csd.da@data, 
                             pix.sum.da[match(pix.csd.da@data$DAUID, pix.sum.da$DAUID),]) 


  #' calcualte fractions for each of the pixel and distribute the households 
  #' and population from the CSD to those.
  
  pix.csd.da@data <- transform(pix.csd.da@data, Frac = PixVal / TotPixval) %>%
    transform(., PixHhold = Frac * dwell) 
  
  print(paste0("The total households assigned to the pixel points are ", 
               sum(pix.csd.da@data$PixHhold)))
  
  p <- pix.csd.da@data
  
###############################################################################
#' Step 4: Assign a DAUID to each of the pixel points from Step 3. 
#' the master DA list and replaced with the Pixel points
############################################################################### 

#' Now summarize on DAUID to estimate the unique DAs   
pix.sum.da <- pix.csd.da@data %>% group_by(DAUID) %>%
  summarise(DAPixHhold = sum(PixHhold))

#' Add column to tag the DAs that should be removed from the master DA list
#' and also remove the hhold column
pix.sum.da <- transform(pix.sum.da, remove = 1) %>% 
  subset(., select = -c(DAPixHhold))

#' merge datasets to identify DAs to be removed. The DAs are identified as
#' those that have a remove = 1 boolean.

# Make copy
da.cen.rm <- da.cen
  #da.cen.rm <- da.cen.rm[da.cen.rm@data$GGH != 1, ]    # remove the GGH DAs
  
  d1 <- da.cen.rm@data

  # merge the DA identifier to shapefile
  da.cen.rm@data = data.frame(da.cen.rm@data, 
                          pix.sum.da[match(da.cen.rm@data$DAUID, pix.sum.da$DAUID),]) 

  #' Only keep necessary columns in shapefile data slot
  da.cen.rm <- da.cen.rm[, c("CSDUID", "DAUID", "GGH", "X", "Y", "dwell", "remove")]

    #' set NA values to zero
    da.cen.rm@data[is.na(da.cen.rm@data)] <- 0 

  #' Now remove the DAs that have a remove value of 0
  da.cen.rm <- da.cen.rm[da.cen.rm@data$remove !=1, ]
  
  sum(da.cen.rm@data$dwell)


###############################################################################
#' Step 5: Merge the Pixel Points with the remaining DAs
############################################################################### 

#' Only keep the columns needed in Pixel shapefile
pix.csd.da <- pix.csd.da[, c("CSDUID", "x", "y", "GGH", "PixHhold")]


  #' rename columns and add in DAUID
  names(pix.csd.da@data) <- c("CSDUID", "X", "Y", "GGH", "dwell")
  pix.csd.da@data <- transform(pix.csd.da@data, DAUID = 0)

#' Only keep the columns needed for the DA centroids
da.cen.rm <- da.cen.rm[, c("CSDUID", "X", "Y", "GGH", "dwell", "DAUID")]
  
#' Merge the shapefiles to make activity centroid for input in the Centorid 
#' script.

activity.centroids <- rbind(pix.csd.da, da.cen.rm)
  sum(activity.centroids@data$dwell)

  print(paste0("The total households in the activity centroids is 5,308,785 and 21,708 points instead of 19,964 DA centroids"))
  

###############################################################################
#' Step 6: Clean up Activity Centroids file for input in to the Centroid Script
###############################################################################
  
#' rename columns and add in DAUID
names(activity.centroids@data) <- c("CSDUID", "Xcoord", "Ycoord", "GGH", "val", "DAUID")

#' write out shapefile for input into the Centroid Script
writeOGR(activity.centroids, layer = paste0("ActivityCentroids"), wd, 
         drive = "ESRI Shapefile", overwrite_layer=T)

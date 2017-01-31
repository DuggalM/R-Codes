library(spatialEco)
library(PBSmapping)
library(rgeos)
library(rgdal)
library(maptools)
library(ggmap)
library(sp)
##########################################################################################

#' Title: "Dissolving DAs for Provincial Zones"
#' Author: "Mausam Duggal"
#' Date: "Oct 30th, 2015"

#########################################################################################

# set working directory
wd <- setwd("c:/Personal/R")

#########################################################################################
# read DA and CT centroid files
# read CSD and CT polygon files
da.cen <- readOGR(wd, "DA_Centroids_1")
ct.cen <- readOGR(wd, "CT_Centroids")
  ct.cen@data$Pop[is.na(ct.cen@data$Pop)] <- 0    # set NA values to zero
ct.poly <- readOGR(wd, "CT")
  ct.poly@data$Pop[is.na(ct.poly@data$Pop)] <- 0    # set NA values to zero
csd.poly <- readOGR(wd, "CSD")
  csd.poly@data$Pop[is.na(csd.poly@data$Pop)] <- 0    # set NA values to zero

########################################################################################
# Plot above shapefiles
  
plot(csd.poly, col=rgb(230,230,230,maxColorValue=255))  
points(da.cen, col="steelblue3", pch=".", cex=1.5 )
points(ct.cen, col="red", pch=".", cex=5)

########################################################################################
# Start rasterization, but first select threhsold identified in pop.emp filter
  # select CSD's with pop plus emp less than 2000
popemp.filter <- 2000

# First round selection of CSD's that are below the popemp threhold
# this file is called csd_first.shp
csd.poly.first <- csd.poly[csd.poly$Pop < popemp.filter,]
  plot(csd.poly.first, col="blue")

# do Point in Poly of all DA that fall in the csd.poly.first
da.cen.first <- da.cen[csd.poly.first,]    # subset DA centroids in CSD
  # populate merge field with CSD IDs
  # populate merge criteria
  da.cen.first@data$mer <- da.cen.first@data$CSDUID
  da.cen.first@data$crit <- "CSD Below"

  plot(csd.poly.first, col="grey")
  points(da.cen.first, pch=".", col="red", cex=1.5)
  
########################################################################################
# Second round selection of CSD's that are greater than popemp threshold
  
# test which Census Tracts lie in those and highlight 
csd.poly.second <- csd.poly[csd.poly$Pop >= popemp.filter,]    # CSD's greater than popemp filter
  # select DAs that lie in CSD's above threshold
  da.cen.second <- da.cen[csd.poly.second,]
  da.cen.second@data[is.na(da.cen.second@data$CTUID)>0
  da.cen.second <- subset()
    # set merge field and criterial with default values of
    # DAUID and DA, respectively

      s <- da.cen.second@data[is.na(da.cen.second$CTUID),]
      s$mer <- s$DAUID
      s$crit <- "DA"

      da.cen.second <- SpatialPointsDataFrame(G, data=G.df)    # make Polygondataframe to export as shapefile
      proj4string(G) <- CRS(proj4string(prov))

    # select CT centroids that lie in DAs making up CSDs greater than popemp threhold
    ct.cen.second <- ct.cen[csd.poly.second,] 
    ct.poly.second <- ct.poly[ct.cen.second,] # select CT polys from above

      # test if any selected CTs are lesser than threshold
      # if they are create DA file for those CTs
      ct.poly.second <- ct.poly.second[ct.poly.second$Pop < popemp.filter,]
        # for those that are lower, transfer info
        # about CT to DA for dissolving
        if (length(ct.poly.second)>0){
          #da.cen.second@data$mer <- (da.cen.second@data$CTUID != NA)[da.cen.second@data$CTUID]
           da.cen.second@data$mer <- (da.cen.second@data$CTUID)
           da.cen.second@data$crit <- "CT Below"
        }
      p <- data.frame(da.cen.second@data)

###############################################################################
# write Shapefiles
  
writeOGR(da.cen.first, layer = "DA_FirstRound", wd, driver="ESRI Shapefile", overwrite_layer=T)
writeOGR(da.cen.second, layer = "DA_SecondRound", wd, driver="ESRI Shapefile", overwrite_layer=T)      
    
#########################################################################################
# Disslove two point shapefiles and create one provincial zone file

# read DA shapefile
da <- readOGR(wd, "DA")
  plot(da)
    #da@data[1:5,]    # check first five records


#########################################################################################
# batchin Rick's text file with provincial zone numbers

zones.csv <- read.csv("zones",stringsAsFactors=F, encoding="utf8", sep=";", header=T)
  zones.csv$DAUID <- as.character(zones.csv$DAUID)    # make DAUID as character for join
    zones[1:5,]

da1 <- da    # save a copy
  da1@data$DAUID <- as.character(da1@data$DAUID)    # make character field for join

  da1@data <- full_join(da1@data, zones.csv, by = "DAUID")

  # Now dissolve
  zone.shp <- gUnaryUnion(da1, id = da1@data$provzone)
    plot(zone.shp)

#######################################################################################
# write shapefile

writeOGR(zone.shp, layer = "ProvZones", wd, driver="ESRI Shapefile", overwrite_layer=T)




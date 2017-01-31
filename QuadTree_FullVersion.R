library(spatialEco)
library(rgeos)
library(rgdal)
library(maptools)
library(ggmap)
library(dplyr)
library(sp)
##########################################################################################

#' Title: "Provincial Raster Grid Creation and Spatial Analysis"
#' Author: "Mausam Duggal"
#' Date: "Nov 13th, 2015"

#########################################################################################
#' set user inputs needed to run the Quad Tree Algorithm, including directory
#' Quad Tree depth, starting grid size, dwelling unit and GPS thresholds
#' and name of the input geography

wd <- setwd("c:/personal/R")

#+ set the tree depth for the Quad Tree, including the starting grid size
n = 6

#+ create directories to call later in the code
for (i in 1:n) {
  dir.create(paste0("split",i), showWarnings = TRUE)
}

#+ starting grid size
cl <- c(60000)  

#+ set dwelling unit and GPS thresholds
thold <- 2000
gps <- 250

#+ reduce margins for plotting
par(mar=c(0,0,0,0))

#+ set file name for geography and DA's and GPS
geog.name <- "All_SO"
da.name <- "DA_Centroids4"
gps.name <- "GPS_Points"


# Now remove them (no need for messing about with do.call)
# for (i in (1:n)) {
#   mydir <- paste0(wd,"/split", i)
#   # What phrase do you want contained in
#   # the files to be deleted?
#   deletephrase <- paste0("R",i,"*.*.*")
#   
#   # Look at directory
#   dir(mydir)
#   # Figure out which files should be deleted
#   id <- grep(deletephrase, dir(mydir))
#   # Get the full path of the files to be deleted
#   todelete <- dir(mydir, full.names = TRUE)[id]
#   # BALEETED
#   unlink(todelete)  
# }

############################ STEP1 : Read File and Define Grid #############################
#' Read province wide polygon and create starting grid based on user input (cl)
#' also save projection system of file to apply to all the later spatial outputs

prov <- readOGR(wd, geog.name)
  U <- prov    # set prov to U
  #+ declare projection and save its handle
  crs.shp <- proj4string(prov) 

  #' get bounding box coordinates of the province (U)
  #' to create initial grid, set by user on line 37
  minx=U@bbox[1,1]
  maxx=U@bbox[1,2]
  miny=U@bbox[2,1]
  maxy=U@bbox[2,2]

  # grab custom square grid from user input
  # and set variables required to create grid

  l=cl # size in meters
  Lx=maxx-minx
  Ly=maxy-miny
  Nbx=round((Lx+l)/l)  # number of rows
  Nby=round((Ly+l)/l)  # number of cols

#+ create grid
G1 = GridTopology(c(minx, miny), c(l,l), c(Nbx, Nby))
  G1 = SpatialGrid(G1)
  G1 = as(G1, 'SpatialPolygons')

  # extract polygon Id's to make dataframe
  Gid <- sapply(slot(G1, "polygons"), function(x) slot(x, "ID")) 
  # Create dataframe with correct rownames
  G1.df <- data.frame( ID=1:length(G1), row.names = Gid)  
  G1 <- SpatialPolygonsDataFrame(G1, data=G1.df)   # make Polygondataframe to export as shapefile
    proj4string(G1) <- CRS(proj4string(prov))    # set projection system to be same as input geography
  # Export grid
  writeOGR(G1, layer = "G1", wd, driver="ESRI Shapefile", overwrite_layer=T)
  writeOGR(G1, layer = "G1_original", wd, driver="ESRI Shapefile", overwrite_layer = T)
  
  
######################## Step 2: Start Grid Splitting Using DU's Only #########################
#' batch in da file to undertake a point in polygon analysis
#' where the da's under a user defined threshold (THOLD) are exported out
#' This FIRST ROUND OF GRID SPLITTING is not in a loop because it requires some
#' atypical outputs

#+ read in DA and GPS threshold files 
da <- readOGR(wd, da.name)
  
#+ use OVER method to find DA's (da) that lie in Grid (G)
#+ and sum the dwelling and GGH tags that lie within each polygon
#+ and subset only those records that lie below the user defined
#+ threshold (THOLD).

for (b in 1:n) {
# Start outer loop

G <- readOGR(wd, layer = paste0("G", b))

#' this is the master dataframe of GRIDS (G) that have points inside them
#' as well as DU's are greater than zero

sp.join <- over(G, da[20], fn=sum) %>% subset(., dwell>0)
  sp.join$gridID <- rownames(sp.join) 
    # conver to integer and add 1 because the FID is always 1 less than ID
    sp.join$gridID <- as.integer(sp.join$gridID) %>% +1 

#' make copy of grid G and merge sp.join dataframe, and remove all those grids into a shapefile
#' that have no DA centroid or have zero DUs, or are below the thold in them 
#' because these will not be split
M.nopnts <- G
  M.nopnts@data <- merge(M.nopnts@data,sp.join,by.x="ID", by.y="gridID", all.x=TRUE)
  #+ remove NA values because they represent empty grids and export
  M.nopnts1 <- M.nopnts[is.na(M.nopnts@data$dwell),]
  #M.nopnts1 <- M.nopnts %>% sp.na.omit(.)
  writeOGR(M.nopnts1, layer = paste0("Grids_noPnts", b), wd, drive = "ESRI Shapefile", 
           overwrite_layer=T)
  
#' Now select only those grids which are equal to or below 
#' user defined Dwelling Units threshold
sp.join.below <- subset(sp.join, dwell <= thold) 

#+ Join to Grid "G" to exporting as shapefile. This shapefile
#+ represents grids that are under the threshold and will not be 
#+ divided further to make copy of grid G and merge sp.join.sub
M2 <- G
  M2@data <- merge(M2@data,sp.join.below,by.x="ID", by.y="gridID", all.x=TRUE) 
  # remove NA values and export shapefile
  M2 <- sp.na.omit(M2)
  writeOGR(M2, layer = paste0("Grids_BelowThold",b), wd, drive = "ESRI Shapefile", overwrite_layer=T)
  
#' get those grids and DA's that were above the user 
#' defined threshold (THOLD)
sp.join.above1 <- subset(sp.join, dwell >thold) 
  M3 <- G 
  M3@data <- merge(M3@data,sp.join.above1,by.x="ID", by.y="gridID", all.x=TRUE) 
  # remove NA values and export shapefile
  M3 <- sp.na.omit(M3)
  writeOGR(M3, layer = paste0("Grids_ToSplit",b), wd, drive = "ESRI Shapefile", overwrite_layer=T)

#' split G2 polygons in half. In this splitting there is no need to test for 
#' dwellings in grid. 
M3.lst <- as.list(M3@data$ID)    # use list to enumerate grid cells

#' select each individual polygon in G2 and split into four
  
for (i in 1:length(M3.lst)) {
  
  R1 <- M3[M3@data$ID==M3.lst[i],]
  
  #' get bounding box coordinates of the province (U)
  minx=R1@bbox[1,1]
  maxx=R1@bbox[1,2]
  miny=R1@bbox[2,1]
  maxy=R1@bbox[2,2]
  
  #+ define custom square grid
  #+ and set variables required to create grid
  l = cl/(2**b) # size in meters, b is the number of outer loops

  Lx=maxx-minx
  Ly=maxy-miny
  Nbx=round((Lx)/l)  # number of rows
  Nby=round((Ly)/l)  # number of cols
  
  minx = minx + l/2    # to dampen any offsetting in the new grid
  miny = miny + l/2    # to dampen any offsetting in the new grid
  
  #+ create grid
  M4.split=GridTopology(c(minx, miny), c(l,l), c(Nbx, Nby))
    M4.split=SpatialGrid(M4.split)
    M4.split=as(M4.split, 'SpatialPolygons')

  # extract polygon Id's to make dataframe
  Gid.split <- sapply(slot(M4.split, "polygons"), function(x) slot(x, "ID")) 
  # Create dataframe with correct rownames
    M4.split.df <- data.frame( ID=1:length(M4.split), row.names = Gid.split)  
    M4.split <- SpatialPolygonsDataFrame(M4.split, data=M4.split.df)   # make Polygondataframe to export as shapefile
    proj4string(M4.split) <- CRS(proj4string(prov))
  
    writeOGR(M4.split, layer = paste0('R',b,"_", M3.lst[i]), paste0(wd,"/split", b) , driver="ESRI Shapefile", 
             overwrite_layer=T)
}  

#' merge shapefiles from Split folder for evaluating threshold

wd1 <- setwd(paste0("c:/personal/r", "/Split", b))

files <- list.files(pattern = paste0("R",b,".*.shp"))

# get polygons from first file
data.first <- readOGR(files[1], gsub(".shp","",files[1]))
  polygons <- slot(data.first, "polygons")

# add polygons from remaining files
for (i in 2:length(files)) {
  data.temp <- readOGR(files[i], gsub(".shp","",files[i]))
  polygons <- c(slot(data.temp, "polygons"),polygons)
}

# rename IDs of Polygons
for (i in 1:length(polygons)) {
  slot(polygons[[i]], "ID") <- paste(i)
}

# ain't that spatial
spatialPolygons <- SpatialPolygons(polygons)
  G <- SpatialPolygonsDataFrame(spatialPolygons, 
                                    data.frame(ID=1:length(polygons)))
  proj4string(G) <- CRS(proj4string(G1))

  writeOGR(G, layer = paste0(("G"),b+1), wd, driver="ESRI Shapefile", 
           overwrite_layer=T )
}
  
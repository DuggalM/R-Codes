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
#' Date: "Nov 13th, 2015"

#########################################################################################

#' set working directory
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
#' create list or named vectors for user to change if required
#' also create user inputs for evaluating threhsolds

cl <- c(60000)    # grid extent for each clip, can be modified by user
names(cl) <- c("clip1")    # can be modified by user

pre <- as.name("GGHTAZ_CSD_combined_") # prefix of each file before clip name

thold <- 2000

################################################################################################
############################## Read File and Define Grid ########################################
# read province wide polygon and create grid using the bbox slot of the shapefile


  #' start loop using clip list or named vectors
  #' read Provincial shapefile and its projection
  prov <- readOGR(wd, "SO_Clipped") 
    U <- prov    # set prov to U
  #+ declare projection and save its handle
    crs.shp <- proj4string(prov) 

  #' get bounding box coordinates of the province (U)
  #' to create initial grid, set by user on line 37
  minx=U@bbox[1,1]
  maxx=U@bbox[1,2]
  miny=U@bbox[2,1]
  maxy=U@bbox[2,2]
  
  #+ define custom square grid
  #+ and set variables required to create grid
  
  l=cl[1] # size in meters
    Lx=maxx-minx
    Ly=maxy-miny
    Nbx=round((Lx+l)/l)  # number of rows
    Nby=round((Ly+l)/l)  # number of cols
  
    #+ create grid
    G=GridTopology(c(minx, miny), c(l,l), c(Nbx, Nby))
    G=SpatialGrid(G)
    G=as(G, 'SpatialPolygons')
  
      # extract polygon Id's to make dataframe
      Gid <- sapply(slot(G, "polygons"), function(x) slot(x, "ID")) 
        # Create dataframe with correct rownames
        G.df <- data.frame( ID=1:length(G), row.names = Gid)  
          G <- SpatialPolygonsDataFrame(G, data=G.df)   # make Polygondataframe to export as shapefile
          proj4string(G) <- CRS(proj4string(prov))
         
      # Export grid
      writeOGR(G, layer = paste0('grids_', names(cl)[1]), wd, driver="ESRI Shapefile", overwrite_layer=T)
  
  #' batch in da file and undertake a point in polygon analysis
  #' where the da's under a user defined threshold (THOLD) are
  #' exported out
  #+ read in DA file with Dwelling Unit information
  da <- readOGR(wd, "DA_Centroids4")

  # sp.join1 <- point.in.poly(da, G)
  #+ use OVER method to find DA's (da) that lie in Grid (G)
  #+ and sum the dwelling and GGH tags that lie within each polygon
  #+ and subset only those records that lie below the user defined
  #+ threshold (THOLD). 
  
  #' this is the master dataset of points inside grids
  sp.join <- over(G, da[19:20], fn=sum) %>% subset(., dwell>0)
    sp.join$gridID <- rownames(sp.join) 
    sp.join$gridID <- substring(sp.join$gridID, 2, nchar(sp.join$gridID))   # get rid of character "g"
  
  #' make copy of grid G and merge sp.join
  #' and remove all those grids into a shapefile
  #' that have no DA centroid in them
  G.nopnts <- G
    G.nopnts@data <- merge(G.nopnts@data,sp.join,by.x="ID", by.y="gridID", all.x=TRUE)
      # remove NA values because they represent empty grids and export
    G.nopnts1 <- G.nopnts  
      G.nopnts1 <- G.nopnts[is.na(G.nopnts@data$dwell),]
      writeOGR(G.nopnts1, layer = "G_noPoints", wd, drive = "ESRI Shapefile", overwrite_layer=T)
  
  #' select only those grids which are below threshold
  sp.join.below <- subset(sp.join, dwell <thold) 
  # make column with gridID to join back to grid shapefile to remove grids
  # below this threshold
#   sp.join.sub$gridID <- rownames(sp.join.sub) 
#   sp.join.sub$gridID <- substring(sp.join.sub$gridID, 2, nchar(sp.join.sub$gridID))   # get rid of character "g"
  
  #+ Step 1: join to Grid "G" to exporting as shapefile. This shapefile
  #+ represents grids that are under the threshold and 
  #+ will not be divided further
  #+ make copy of grid G and merge sp.join.sub
  G1 <- G
    G1@data <- merge(G1@data,sp.join.below,by.x="ID", by.y="gridID", all.x=TRUE) 
    p <- G1@data
    # remove NA values and export shapefile
    G1 <- sp.na.omit(G1)
      writeOGR(G1, layer = "G1", wd, drive = "ESRI Shapefile", overwrite_layer=T)

  #' get those grids and DA's that were above the user 
  #' defined threshold (THOLD)
  sp.join.above1 <- subset(sp.join, dwell >thold) 
  G2 <- G
    G2@data <- merge(G2@data,sp.join.above1,by.x="ID", by.y="gridID", all.x=TRUE) 
  # remove NA values and export shapefile
    G2 <- sp.na.omit(G2)
      writeOGR(G2, layer = "G2", wd, drive = "ESRI Shapefile", overwrite_layer=T)
      
  #' split G2 polygons in half. In this round of splitting there is
  #' no need to test for dwellings in grid. The code begins by selecting 
  #' each grid cell and first splitting it up, aggregating the dwellings 
  #' within each of the "child" split cells, and deciding if it needs to 
  #' split further
  G2.lst <- as.list(G2@data$ID)    # use list to enumerate grid cells

    # select each individual polygon in G2 and 
    # split into four
  
    for (i in 1:length(G2.lst)) {
      R1 <- G2[G2@data$ID==G2.lst[i],]
      
      #' get bounding box coordinates of the province (U)
      minx=R1@bbox[1,1]
      maxx=R1@bbox[1,2]
      miny=R1@bbox[2,1]
      maxy=R1@bbox[2,2]
      
      #+ define custom square grid
      #+ and set variables required to create grid
      l=(cl[1])/2 # size in meters
      
      Lx=maxx-minx
      Ly=maxy-miny
      Nbx=round((Lx)/l)  # number of rows
      Nby=round((Ly)/l)  # number of cols
      
      minx = minx +5000    # to dampen any offsetting in the new grid
      miny = miny +5000    # to dampen any offsetting in the new grid
      
      #+ create grid
      G2.split=GridTopology(c(minx, miny), c(l,l), c(Nbx, Nby))
      G2.split=SpatialGrid(G2.split)
      G2.split=as(G2.split, 'SpatialPolygons')
      
      # extract polygon Id's to make dataframe
      Gid.split <- sapply(slot(G2.split, "polygons"), function(x) slot(x, "ID")) 
      # Create dataframe with correct rownames
      G2.split.df <- data.frame( ID=1:length(G2.split), row.names = Gid.split)  
      G2.split <- SpatialPolygonsDataFrame(G2.split, data=G2.split.df)   # make Polygondataframe to export as shapefile
      proj4string(G2.split) <- CRS(proj4string(prov))
 
      writeOGR(G2.split, layer = paste0('R1_', G2.lst[i]), "c:/personal/r/G2Split", driver="ESRI Shapefile", overwrite_layer=T)
        
    }
  
  #' merge shapefiles from G2split folder for evaluating threshold
  #' This step requires a revised WD to point the code to the 
  #' correct folder that is storing the individual grids from the 
  #' step above
  
  wd1 <- setwd("c:/personal/r/G2split")
  
  files <- list.files(pattern = "R1.*.shp")
  
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
  G2.comb <- SpatialPolygonsDataFrame(spatialPolygons, 
                                   data.frame(ID=1:length(polygons)))
  proj4string(G2.comb) <- CRS(proj4string(G2))
  
  writeOGR(G2.comb, layer = "G2_split_combined", wd1,driver="ESRI Shapefile", overwrite_layer=T )

#################################################################################################  

  #' select those grids from G2.comb that are yet above the threshold
  #' even after splitting. These will be further cut into half

    #+ this is the master dataset of points inside the split grids from G2.comb
  sp.join.G2.comb <- over(G2.comb, da[19:20], fn=sum) 
  sp.join.G2.comb$gridID <- rownames(sp.join.G2.comb) 

  G3 <- G2.comb
    G3@data <- merge(G3@data,sp.join.G2.comb, by.x="ID", by.y="gridID", all.x=TRUE)
  # remove NA values because they represent grids that must not be split and export
  G3.nopnts1 <- G3[is.na(G3@data$dwell),]
  writeOGR(G3.nopnts1, layer = "G3_noPoints", wd, drive = "ESRI Shapefile", overwrite_layer=T)
  # remove grids that are below the user defined threshold (THOLD)
  # remove NA values and export shapefile
  G3.noNA <- sp.na.omit(G3)
    G3.below.thold <- G3.noNA[G3.noNA@data$dwell < thold,]
    writeOGR(G3.below.thold, layer = "G3_below", wd, drive = "ESRI Shapefile", overwrite_layer=T)
  
  #' select those grids that were above the threhold (THOLD) 
  #' and split these further
  G3.above.thold <- G3.noNA[G3.noNA@data$dwell > thold,]
  writeOGR(G3.above.thold, layer = "G3_above", wd, drive = "ESRI Shapefile", overwrite_layer=T)
  
  
  # select each individual polygon in G3_above and 
  # split into four parts
  
  G3.above.thold.lst <- as.list(G3.above.thold@data$ID)    # use list to enumerate grid cells

  for (i in 1:length(G3.above.thold.lst)) {
    R2 <- G3.above.thold[G3.above.thold$ID==G3.above.thold.lst[i],]
    
    #' get bounding box coordinates of the province (U)
    minx=R2@bbox[1,1]
    maxx=R2@bbox[1,2]
    miny=R2@bbox[2,1]
    maxy=R2@bbox[2,2]
    
    #+ define custom square grid
    #+ and set variables required to create grid
    l=(cl[1])/4 # size in meters
    
    Lx=maxx-minx
    Ly=maxy-miny
    Nbx=round((Lx)/l)  # number of rows
    Nby=round((Ly)/l)  # number of cols
    
    minx = minx +2500    # to dampen any offsetting in the new grid
    miny = miny +2500    # to dampen any offsetting in the new grid
    
    #+ create grid
    G3.above.split=GridTopology(c(minx, miny), c(l,l), c(Nbx, Nby))
    G3.above.split=SpatialGrid(G3.above.split)
    G3.above.split=as(G3.above.split, 'SpatialPolygons')
    
    # extract polygon Id's to make dataframe
    Gid.split <- sapply(slot(G3.above.split, "polygons"), function(x) slot(x, "ID")) 
    # Create dataframe with correct rownames
    G3.above.split.df <- data.frame( ID=1:length(G3.above.split), row.names = Gid.split)  
    G3.above.split <- SpatialPolygonsDataFrame(G3.above.split, data=G3.above.split.df)   # make Polygondataframe to export as shapefile
    proj4string(G3.above.split) <- CRS(proj4string(prov))
    
    writeOGR(G3.above.split, layer = paste0('R2_', G3.above.thold.lst[i]), "c:/personal/r/G3Split", driver="ESRI Shapefile", overwrite_layer=T)
    
  }
  
  #' merge shapefiles from G3split folder for evaluating threshold
  #' This step requires a revised WD to point the code to the 
  #' correct folder that is storing the individual grids from the 
  #' step above
  
  wd2 <- setwd("c:/personal/r/G3split")
  
  files <- list.files(pattern = "R2.*.shp")
  
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
  G3.comb <- SpatialPolygonsDataFrame(spatialPolygons, 
                                      data.frame(ID=1:length(polygons)))
  proj4string(G3.comb) <- CRS(proj4string(G2))
  
  writeOGR(G3.comb, layer = "G3_split_combined", wd2,driver="ESRI Shapefile", overwrite_layer=T )
  
#################################################################################################  
  
  #' select those grids from G3.comb that are yet above the threshold
  #' even after splitting. These will be further cut into half
  
  #+ this is the master dataset of points inside the split grids from G2.comb
  sp.join.G3.comb <- over(G3.comb, da[19:20], fn=sum) 
  sp.join.G3.comb$gridID <- rownames(sp.join.G3.comb) 
  
  G4 <- G3.comb
  G4@data <- merge(G4@data,sp.join.G3.comb, by.x="ID", by.y="gridID", all.x=TRUE)
  # remove NA values because they represent grids that must not be split and export
  G4.nopnts1 <- G4[is.na(G4@data$dwell),]
  writeOGR(G4.nopnts1, layer = "G4_noPoints", wd, drive = "ESRI Shapefile", overwrite_layer=T)
  # remove grids that are below the user defined threshold (THOLD)
  # remove NA values and export shapefile
  G4.noNA <- sp.na.omit(G4)
  G4.below.thold <- G4.noNA[G4.noNA@data$dwell < thold,]
  writeOGR(G4.below.thold, layer = "G4_below", wd, drive = "ESRI Shapefile", overwrite_layer=T)
  
  #' select those grids that were above the threhold (THOLD) 
  #' and split these further
  G4.above.thold <- G4.noNA[G4.noNA@data$dwell > thold,]
  writeOGR(G4.above.thold, layer = "G4_above", wd, drive = "ESRI Shapefile", overwrite_layer=T)
  
  
  # select each individual polygon in G3_above and 
  # split into four parts
  
  G4.above.thold.lst <- as.list(G4.above.thold@data$ID)    # use list to enumerate grid cells
  
  for (i in 1:length(G4.above.thold.lst)) {
    R3 <- G4.above.thold[G4.above.thold$ID==G4.above.thold.lst[i],]
    
    #' get bounding box coordinates of the province (U)
    minx=R3@bbox[1,1]
    maxx=R3@bbox[1,2]
    miny=R3@bbox[2,1]
    maxy=R3@bbox[2,2]
    
    #+ define custom square grid
    #+ and set variables required to create grid
    l=(cl[1])/8 # size in meters
    
    Lx=maxx-minx
    Ly=maxy-miny
    Nbx=round((Lx)/l)  # number of rows
    Nby=round((Ly)/l)  # number of cols
    
    minx = minx +1250    # to dampen any offsetting in the new grid
    miny = miny +1250    # to dampen any offsetting in the new grid
    
    #+ create grid
    G4.above.split=GridTopology(c(minx, miny), c(l,l), c(Nbx, Nby))
    G4.above.split=SpatialGrid(G4.above.split)
    G4.above.split=as(G4.above.split, 'SpatialPolygons')
    
    # extract polygon Id's to make dataframe
    Gid.split <- sapply(slot(G4.above.split, "polygons"), function(x) slot(x, "ID")) 
    # Create dataframe with correct rownames
    G4.above.split.df <- data.frame( ID=1:length(G4.above.split), row.names = Gid.split)  
    G4.above.split <- SpatialPolygonsDataFrame(G4.above.split, data=G4.above.split.df)   # make Polygondataframe to export as shapefile
    proj4string(G4.above.split) <- CRS(proj4string(prov))
    
    writeOGR(G4.above.split, layer = paste0('R3_', G4.above.thold.lst[i]), "c:/personal/r/G4Split", driver="ESRI Shapefile", overwrite_layer=T)
    
  }
  
  #' merge shapefiles from G3split folder for evaluating threshold
  #' This step requires a revised WD to point the code to the 
  #' correct folder that is storing the individual grids from the 
  #' step above
  
  wd3 <- setwd("c:/personal/r/G4split")
  
  files <- list.files(pattern = "R3.*.shp")
  
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
  G4.comb <- SpatialPolygonsDataFrame(spatialPolygons, 
                                      data.frame(ID=1:length(polygons)))
  proj4string(G4.comb) <- CRS(proj4string(G2))
  
  writeOGR(G4.comb, layer = "G4_split_combined", wd3, driver="ESRI Shapefile", overwrite_layer=T )
  
  #################################################################################################  
  
  #' select those grids from G4.comb that are yet above the threshold
  #' even after splitting. These will be further cut into half
  
  #+ this is the master dataset of points inside the split grids from G2.comb
  sp.join.G4.comb <- over(G4.comb, da[19:20], fn=sum) 
  sp.join.G4.comb$gridID <- rownames(sp.join.G4.comb) 
  
  G5 <- G4.comb
  G5@data <- merge(G5@data,sp.join.G4.comb, by.x="ID", by.y="gridID", all.x=TRUE)
  # remove NA values because they represent grids that must not be split and export
  G5.nopnts1 <- G5[is.na(G5@data$dwell),]
  writeOGR(G5.nopnts1, layer = "G5_noPoints", wd, drive = "ESRI Shapefile", overwrite_layer=T)
  # remove grids that are below the user defined threshold (THOLD)
  # remove NA values and export shapefile
  G5.noNA <- sp.na.omit(G5)
  G5.below.thold <- G5.noNA[G5.noNA@data$dwell < thold,]
  writeOGR(G5.below.thold, layer = "G5_below", wd, drive = "ESRI Shapefile", overwrite_layer=T)
  
  #' select those grids that were above the threhold (THOLD) 
  #' and split these further
  G5.above.thold <- G5.noNA[G5.noNA@data$dwell > thold,]
  writeOGR(G5.above.thold, layer = "G5_above", wd, drive = "ESRI Shapefile", overwrite_layer=T)
  
  
  # select each individual polygon in G3_above and 
  # split into four parts
  
  G5.above.thold.lst <- as.list(G5.above.thold@data$ID)    # use list to enumerate grid cells
  
  for (i in 1:length(G5.above.thold.lst)) {
    R4 <- G5.above.thold[G5.above.thold$ID==G5.above.thold.lst[i],]
    
    #' get bounding box coordinates of the province (U)
    minx=R4@bbox[1,1]
    maxx=R4@bbox[1,2]
    miny=R4@bbox[2,1]
    maxy=R4@bbox[2,2]
    
    #+ define custom square grid
    #+ and set variables required to create grid
    l=(cl[1])/16 # size in meters
    
    Lx=maxx-minx
    Ly=maxy-miny
    Nbx=round((Lx)/l)  # number of rows
    Nby=round((Ly)/l)  # number of cols
    
    minx = minx +625    # to dampen any offsetting in the new grid
    miny = miny +625    # to dampen any offsetting in the new grid
    
    #+ create grid
    G5.above.split=GridTopology(c(minx, miny), c(l,l), c(Nbx, Nby))
    G5.above.split=SpatialGrid(G5.above.split)
    G5.above.split=as(G5.above.split, 'SpatialPolygons')
    
    # extract polygon Id's to make dataframe
    Gid.split <- sapply(slot(G5.above.split, "polygons"), function(x) slot(x, "ID")) 
    # Create dataframe with correct rownames
    G5.above.split.df <- data.frame( ID=1:length(G5.above.split), row.names = Gid.split)  
    G5.above.split <- SpatialPolygonsDataFrame(G5.above.split, data=G5.above.split.df)   # make Polygondataframe to export as shapefile
    proj4string(G5.above.split) <- CRS(proj4string(prov))
    
    writeOGR(G5.above.split, layer = paste0('R4_', G5.above.thold.lst[i]), "c:/personal/r/G5Split", driver="ESRI Shapefile", overwrite_layer=T)
    
  }
  
  #' merge shapefiles from G3split folder for evaluating threshold
  #' This step requires a revised WD to point the code to the 
  #' correct folder that is storing the individual grids from the 
  #' step above
  
  wd4 <- setwd("c:/personal/r/G5split")
  
  files <- list.files(pattern = "R4.*.shp")
  
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
  G5.comb <- SpatialPolygonsDataFrame(spatialPolygons, 
                                      data.frame(ID=1:length(polygons)))
  proj4string(G5.comb) <- CRS(proj4string(G2))
  
  writeOGR(G5.comb, layer = "G5_split_combined", wd4, driver="ESRI Shapefile", overwrite_layer=T )
  

  #################################################################################################  
  
  #' select those grids from G5.comb that are yet above the threshold
  #' even after splitting. These will be further cut into half
  
  #+ this is the master dataset of points inside the split grids from G2.comb
  sp.join.G5.comb <- over(G5.comb, da[19:20], fn=sum) 
  sp.join.G5.comb$gridID <- rownames(sp.join.G5.comb) 
  
  G6 <- G5.comb
  G6@data <- merge(G6@data,sp.join.G5.comb, by.x="ID", by.y="gridID", all.x=TRUE)
  # remove NA values because they represent grids that must not be split and export
  G6.nopnts1 <- G6[is.na(G6@data$dwell),]
  writeOGR(G6.nopnts1, layer = "G6_noPoints", wd, drive = "ESRI Shapefile", overwrite_layer=T)
  # remove grids that are below the user defined threshold (THOLD)
  # remove NA values and export shapefile
  G6.noNA <- sp.na.omit(G6)
  G6.below.thold <- G6.noNA[G6.noNA@data$dwell < thold,]
  writeOGR(G6.below.thold, layer = "G6_below", wd, drive = "ESRI Shapefile", overwrite_layer=T)
  
  #' select those grids that were above the threhold (THOLD) 
  #' and split these further
  G6.above.thold <- G6.noNA[G6.noNA@data$dwell > thold,]
  writeOGR(G6.above.thold, layer = "G6_above", wd, drive = "ESRI Shapefile", overwrite_layer=T)
  
  
  # select each individual polygon in G3_above and 
  # split into four parts
  
  G6.above.thold.lst <- as.list(G6.above.thold@data$ID)    # use list to enumerate grid cells
  
  for (i in 1:length(G6.above.thold.lst)) {
    R5 <- G6.above.thold[G6.above.thold$ID==G6.above.thold.lst[i],]
    
    #' get bounding box coordinates of the province (U)
    minx=R5@bbox[1,1]
    maxx=R5@bbox[1,2]
    miny=R5@bbox[2,1]
    maxy=R5@bbox[2,2]
    
    #+ define custom square grid
    #+ and set variables required to create grid
    l=(cl[1])/32 # size in meters
    
    Lx=maxx-minx
    Ly=maxy-miny
    Nbx=round((Lx)/l)  # number of rows
    Nby=round((Ly)/l)  # number of cols
    
    minx = minx +(625/2)    # to dampen any offsetting in the new grid
    miny = miny +(625/2)    # to dampen any offsetting in the new grid
    
    #+ create grid
    G6.above.split=GridTopology(c(minx, miny), c(l,l), c(Nbx, Nby))
    G6.above.split=SpatialGrid(G6.above.split)
    G6.above.split=as(G6.above.split, 'SpatialPolygons')
    
    # extract polygon Id's to make dataframe
    Gid.split <- sapply(slot(G6.above.split, "polygons"), function(x) slot(x, "ID")) 
    # Create dataframe with correct rownames
    G6.above.split.df <- data.frame( ID=1:length(G6.above.split), row.names = Gid.split)  
    G6.above.split <- SpatialPolygonsDataFrame(G6.above.split, data=G6.above.split.df)   # make Polygondataframe to export as shapefile
    proj4string(G6.above.split) <- CRS(proj4string(prov))
    
    writeOGR(G6.above.split, layer = paste0('R5_', G6.above.thold.lst[i]), "c:/personal/r/G6Split", driver="ESRI Shapefile", overwrite_layer=T)
    
  }
  
  #' merge shapefiles from G3split folder for evaluating threshold
  #' This step requires a revised WD to point the code to the 
  #' correct folder that is storing the individual grids from the 
  #' step above
  
  wd5 <- setwd("c:/personal/r/G6split")
  
  files <- list.files(pattern = "R5.*.shp")
  
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
  G6.comb <- SpatialPolygonsDataFrame(spatialPolygons, 
                                      data.frame(ID=1:length(polygons)))
  proj4string(G6.comb) <- CRS(proj4string(G2))
  
  writeOGR(G6.comb, layer = "G6_split_combined", wd5, driver="ESRI Shapefile", overwrite_layer=T )
 
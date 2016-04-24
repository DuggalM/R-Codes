library(maptools)
library(sp)
library(rgdal)
library(dplyr)

#########################################################################################

#' Title: "Build path/line for a given set of uniqe route points"
#' Mausam Duggal, Systems Analysis Group, WSP|PB", March 24, 2016"

#########################################################################################

#' set your working directory

wd <- setwd("c:/personal/r")
###################################################################################
#' read Steve's file

way <- read.csv("Waypoints.csv")
###################################################################################

  #' first get unique id's in your Waypoints table to enumerate with the 
  #' dreaded FOR loop in R. One could also write an Apply function, if you are brave
  way.unique <- (way %>% group_by(TRIPID) %>% summarize(SEQ = n_distinct(SEQ)) %>% 
    subset(., select = -c(SEQ)))
    # get number of rows
    lst <- nrow(way.unique)
    way.unique <- as.vector(way.unique$TRIPID)   #### use this to populate the spatiallines dataframe
    

#' set coordinates to whatever the equivalent is in your data.frame in future.
#' One could specify an actual proj system by declaring it as a variable too 
coordinates(way)<-~LONGITUDE+LATITUDE 
                                
  #' Way object has now changed to a spatial points
  #' data frame. Check to see the magic actually happened
  class(way)
  #' plot it to make sure of the magic
    plot(way)
  
    #' write it out to display it in GIS if you would like
    writeOGR(way, layer = "waypoints", wd, driver="ESRI Shapefile", overwrite_layer = T)

###########################################################################################
    
#' now this piece of code makes the lines based on 
#' sequential numbers for each trip ID. Create empty lists
#' for receving results

    # start by getting the first line and creating a spatial lines object
    way1 <- way[way@data$TRIPID == way.unique[1],]
      lin.id <- as.character(way.unique[1])
      sp = SpatialLines( list(Lines(list(Line(way1)), lin.id)))
    
#' start loop from second line now
      
for (i in 2:lst) {
  #+ subset points by unique ID
  way2 <- way[way@data$TRIPID == way.unique[i],]
  #+ save line ids
    lin.id <- as.character(way.unique[i])

  #+ make spatial lines from points
  spl = SpatialLines( list(Lines(list(Line(way2)), lin.id)))
  
  #+ save results 
  sp <- spRbind(sp, spl)

}

#' Extract line ids
Lid <- sapply(slot(sp, "lines"), function(x) slot(x, "ID")) 
  Lid.df <- data.frame( ID=1:length(sp), row.names = Lid) 
    Lid.df$Linid <- row.names(Lid.df)


#' Finally ready to write your lines as a shapefile, but
#' first convert to spatial dataframe

sldf <- SpatialLinesDataFrame(sp, data = Lid.df)
  dd <- sldf@data
  # write points out to CSV
  write.csv(dd, file = "SpatialLinesIDs.csv")
  
    plot(sldf)

###########################################################################################
#' write shapefile and rejoice

    writeOGR(sldf, layer = "spatiallines", wd, driver="ESRI Shapefile", overwrite_layer = T)
    
    

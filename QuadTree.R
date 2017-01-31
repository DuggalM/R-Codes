library(spatialEco)
library(PBSmapping)
library(rgeos)
library(rgdal)
library(maptools)
library(ggmap)
library(sp)

wd <- setwd("c:/Personal/R")


# reduce margins for plotting
par(mar=c(0,0,0,0))


prov <- readOGR(wd, "Ontario_Boundary_projected_UTM")

# declare projection
  crs.shp <- proj4string(prov)
  crs.shp
  U <- prov

  # get bounding box coordinates of the province (U)
  minx=U@bbox[1,1]
  maxx=U@bbox[1,2]
  miny=U@bbox[2,1]
  maxy=U@bbox[2,2]

  Lx=maxx-minx    # generate extents of box
  Ly=maxy-miny    # generate extents of box

  Nbx=1  # number of rows, set at 1 for master square
  Nby=1  # number of cols, set at 1 for master square

    # create grid
    G=GridTopology(c(minx, miny), c(Lx,Ly), c(Nbx, Nby))
    G=SpatialGrid(G)
    G=as(G, 'SpatialPolygons')
    p <- proj4string(G) <- CRS(proj4string(prov))

    # extract polygon Id's to make dataframe
    Gid <- sapply(slot(G, "polygons"), function(x) slot(x, "ID")) 

    # Create dataframe with correct rownames
    G.df <- data.frame( ID=1:length(G), row.names = Gid)

    G <- SpatialPolygonsDataFrame(G, data=G.df)    # make Polygondataframe to export as shapefile

    writeOGR(G, layer = "Ontario_bounds", wd, driver="ESRI Shapefile", overwrite_layer=T)

##########################
    # potential for adding this to loop/function
# read square back to split
sq.main <- readOGR(wd, "Ontario_bounds")
proj4string(sq.main)

  # generate midpoint by using bbox slotNames
  minx1=sq.main@bbox[1,1]
  maxx1=sq.main@bbox[1,2]
  miny1=sq.main@bbox[2,1]
  maxy1=sq.main@bbox[2,2]

  # generate mid-points of square for generating crossing lines
  A1.x <- (maxx1+minx1)/2
  A1.y <- miny1

  A2.x <- A1.x
  A2.y <- maxy1

  A3.x <- minx1
  A3.y <- (maxy1+miny1)/2

  A4.x <- maxx1
  A4.y <- A3.y

    # bind it all together to make dataframe
    l1 <- rbind(c(A1.x, A1.y))
    l2 <- rbind(c(A2.x, A2.y))
    l3 <- rbind(c(A3.x, A3.y))
    l4 <- rbind(c(A4.x, A4.y))
    l5 <- cbind(rbind(l1,l2,l3,l4))
    #l6 <- data.frame(l5)

    # make spatial lines
    l.v <- SpatialLines(list(Lines(Line(cbind(rbind(l1,l2))), ID="a")))
    l.h <- SpatialLines(list(Lines(Line(cbind(rbind(l3,l4))), ID="b")))
    
    # extract Line Id's to make dataframe
    Lid.h <- sapply(slot(l.h, "lines"), function(x) slot(x, "ID")) 
    Lid.v <- sapply(slot(l.v, "lines"), function(x) slot(x, "ID"))
    
    # Create dataframe 
    Lid.h.df <- data.frame( ID=1:length(Lid.h), row.names = Lid.h)
    Lid.v.df <- data.frame( ID=1:length(Lid.v), row.names = Lid.v)
    
    #make spatial lines dataframe and export
    l.v.df <- SpatialLinesDataFrame(l.v, data=Lid.v.df)
    l.h.df <- SpatialLinesDataFrame(l.h, data=Lid.h.df)
    writeOGR(l.v.df, layer="vertical", wd, driver="ESRI Shapefile", overwrite_layer=T)
    writeOGR(l.h.df, layer="horizontal",wd, driver="ESRI Shapefile", overwrite_layer=T)
    
    #plot(prov)
    plot(G, col="red")
    lines(l.v.df, col="blue")
    lines(l.h.df, col="black")




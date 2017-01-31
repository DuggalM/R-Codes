colquad <- function(plot, crossAt = 0, xlims = c(-10,10), ylims = c(-10,10), colours = c("blue", "red","yellow", "green")) {
  #colours of rects are from left to right starting at the top
  library(ggplot2)
  plot <- plot + coord_cartesian(xlim = c(xlims[1],xlims[2]), ylim = c(ylims[1], ylims[2])) 
  plot + 
    annotate("rect", xmin = xlims[1], xmax = crossAt, ymin = ylims[2], ymax = crossAt, fill = colours[1]) + 
    annotate("rect", xmin = crossAt, xmax = xlims[2], ymin = crossAt, ymax = ylims[2], fill = colours[2])  +
    annotate("rect", xmin = xlims[1], xmax = crossAt, ymin = ylims[1], ymax = crossAt , fill= colours[3]) + 
    annotate("rect", xmin = crossAt, xmax = xlims[2], ymin = crossAt, ymax = ylims[1], fill = colours[4]) + 
    geom_point()
}


Piv2 <- read.csv("C:/Personal/R/Pivot1.csv",header=TRUE,
                 quote="\"",
                 stringsAsFactors= TRUE,
                 strip.white = TRUE)
str(Piv2)


###################################################################################
# plot(1,xlab=' ',ylab=' ',xlim=c(minx,maxx),ylim=c(miny,maxy),axes=FALSE)   # empty plot
# sequential plotting of polygons
# for(i in 1:length(U)){
#for(po in 1:length(U@polygons[[i]]@Polygons)){
#pol=U@polygons[[i]]@Polygons[[po]]
#xp=fortify(pol)[,1]
#yp=fortify(pol)[,2]
#polygon(xp,yp,col=rgb(230,230,230,maxColorValue=255),border=NA)
#}
#}

#for(i in 1:length(G)){
#for(po in 1:length(G@polygons[[i]]@Polygons)){
#pol=G@polygons[[i]]@Polygons[[po]]
#xp=fortify(pol)[,1]
#yp=fortify(pol)[,2]
#polygon(xp,yp,col=rgb(0, 0, 0, 0),border='black')
#}
#}
####################################################################################



***********read shapefiles from geodatabase

library(rgdal)

fgdb = "C:/Projects/Province-wide/ProvinceGrid.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list = ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc = readOGR(dsn=fgdb,layer="ontario_projected_grid_150m")

# Determine the FC extent, projection, and attribute information
summary(fc)

# View the feature class
plot(fc)

############################create square 

library(spatstat)
w <- owin()
w <- owin(c(0,1), c(0,1))
# the unit square

w <- owin(c(10,20), c(10,30), unitname=c("foot","feet"))
# a rectangle of dimensions 10 x 20 feet
# with lower left corner at (10,10)

slotNames(w)

plot(w)


library(formatR)
tidy_eval("C:/Personal/R/QuadTree.R")
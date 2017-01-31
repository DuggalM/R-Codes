library(sp)
library(rgeos)
library(rgdal)
library(dplyr)
library(maptools)
library(ggplot2)
library(PBSmapping)


wd <- setwd("c:/personal/r/")

zones <- "railzones"


rail <- read.csv("Rail Traffic Summary.csv", stringsAsFactors = FALSE) %>% subset(., year == 2011)

rail15 <- subset(rail, sctg2 == 19)
#' given that the Origin and destination files have both numbers and text, 
#' make this into a numeric column, thereby automatically introducing NA.
#' Also subset to only keep 2011
rail1 <- transform(rail, Origin = as.numeric(Origin)) %>% 
  transform(., Destination = as.numeric(Destination)) %>% 
  subset(., year == 2011) 

rail1 <- subset(rail, year == 2011) 

rail1[is.na(rail1)] <- 0

rm(rail1)
#' Now remove records where the Origin and Destinations are zero because these
#' records represents O-D interchanges that do not include any trip end within
#' Ontario

rail1 <- subset(rail1, Origin != 0 & Destination != 0)


u <- unique(rail1 ["Origin"])
  colnames(u) <- "Unique"
u1 <- unique(rail1 ["Destination"])
  colnames(u1) <- "Unique"
  
  un <- rbind(u, u1) 
    un <- unique(un ["Unique"])
    
    write.csv(un, "railunique.csv")


   let <- LETTERS 
   
   data <- data.frame(animal = sample(c("cat","dog","bird", 'doggy','kittycat'), 50, replace = T))
   matches <- c("cat","dog")
   
  co1 <- grepl(paste(let, collapse = "|"), rail1$Origin)
  co2 <- grepl(paste(let, collapse = "|"), rail1$Destination)
   
   t1 <- transform(rail1, type = ifelse(co1|co2, "EI","II"))
   
   t1 <- as.data.frame(rail1[grepl(paste(let, collapse = "|"), rail1$Origin),])
   
   
   
   
   test <- subset(rail1, Origin == let)
    
   grep("[a-z]", letters)
   txt <- c("arm","foot","lefroo", "bafoobar")
   if(length(i <- grep("foo", txt)))
     cat("'foo' appears at least once in\n\t", txt, "\n")
   i # 2 and 4
   txt[i]
    
    rail2 <- subset(rail1, select = c("year", "sctg2", "tonnes", "Commodity_Description", "Trade_Type_Flag", 
                                      "Origin", "Destination", "Intermodal_Flag"))
    
    
    
    
    rail2 <- na.omit(rail2)

rail2_ont<- subset(rail2, Origin < 29 & Destination <29)


railz <- readOGR(wd, zones)
railz <- railz[railz@data$RAILZONE_3 <29, ]

railz@data$ID <- railz@data$RAILZONE_3

railz.df <- railz@data

railz.cen <- gCentroid(railz,byid=TRUE)


railz.cen.spdf <- SpatialPointsDataFrame(railz.cen, railz.df, match.ID = TRUE)
railz.cen.spdf_df <- railz.cen.spdf@data

writeOGR(railz.cen.spdf, layer = paste0("railzonecen"), wd, 
         drive = "ESRI Shapefile", overwrite_layer=T)

cen_coords <- as.data.frame(railz.cen.spdf@coords)

id <- as.data.frame(railz.cen.spdf_df$RAILZONE_3)
names(id)[1] <- "RailZone_3"

cen_coords <- cbind(cen_coords, id) 


rail2_coords <- merge(rail2_ont, cen_coords, by.x = "Origin", by.y = "RailZone_3", all.x = TRUE )

names(rail2_coords)[8] <- "OrX"
names(rail2_coords)[9] <- "OrY"


rail2_coords <- merge(rail2_coords, cen_coords, by.x = "Destination", by.y = "RailZone_3", all.x = TRUE )

names(rail2_coords)[10] <- "DtX"
names(rail2_coords)[11] <- "DtY"


xquiet <- scale_x_continuous("", breaks = NULL)
yquiet <- scale_y_continuous("", breaks = NULL)
quiet<-list(xquiet, yquiet)



ggplot(rail2_coords_sub, aes(OrX, OrY))+
  geom_segment(aes(x=OrX, y=OrY,xend=DtX, yend=DtY, alpha=tonnes), col="black")+
  scale_alpha_continuous(range = c(0.03, 0.8))+
  theme(panel.background = element_rect(fill='transparent',colour= NA))+
  quiet+coord_equal() + geom_polygon(data=railz.df, aes(), fill = NA)


geom_polygon(data=countries_robin_df, aes(long,lat, group=group, fill=hole))


plot(g)
plot(railz, add = TRUE, usePolyPath = FALSE)

# generate map using PBSmapping plotting functions
plotPolys(g, border="gray",
          xlab="Longitude", ylab="Latitude")
addPolys(railz, pch=20)



library(threejs) # devtools::install_github("bwlewis/rthreejs")
library(RColorBrewer)

rail2_coords_sub <- subset(rail2_coords, year == 2011)

write.csv(rail2_coords, "railodclean.csv")

df4 <- arrange(rail2_coords_sub, Origin)

df4$colors <- rep(brewer.pal(7, 'Set2'), each = 146)

weights <- 0.005 * df4$tonnes

arcs <- data.frame(lat1 = df4$OrY, lon1 = df4$OrX, 
                   lat2 = df4$DtX, lon2 = df4$DtY)

globejs(arcsLwd = weights, arcs = arcs, arcsColor = df4$colors)
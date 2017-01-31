library(spatialEco)
library(rgeos)
library(rgdal)
library(sp)


wd <- setwd("c:/personal/R")

#' using a subset of idaho lines because I cannot open all
#' of them in R because of laptop limitation
#' 
idaho <- readOGR(".", "idaholines")
  plot(idaho)
  slotNames(idaho)    # just to check the slots available

  lins <- idaho@lines

# if you want to report with line names

for (i in 1:length(lins)) {
  print(paste0("Line #", i))
  print(slot(slot(lins[[i]], "Lines")[[1]], "coords"))
}


# just node coordinates

allcoordinates = function(x) {
      ret = NULL
       #lins = x@idaho@lines
       for(i in 1:length(lins)) {
             ll = lins[[i]]@Lines
             for (j in 1:length(ll))
                   ret = rbind(ret, coordinates(ll[[j]]))
           }
       ret
}

q <- allcoordinates(idaho)

write.csv(q, file="idahopnts.csv")







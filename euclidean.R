library(dplyr)
library(reshape2)
library(SpatialTools)
#' set working directory
wd <- setwd("c:/personal/R")

#' batch in activity centroid text file
#' make sure it already has the x (Long) and Y (Lat) 
#' in it as column names. It must also have the grid id as ID

activity <- read.csv("Revised_DA_ActivityCen.txt", header = TRUE) %>% 
  subset(.,select = c(ID, Lat, Long))
  rownames(activity) <- activity$ID 
  
  # save row and column names
  rows.n <- as.list(activity$ID)
  cols.n <- as.list(activity$ID)
  
  #' create copy for estimating euclidean distance
  activity1 <- subset(activity, select= c(Lat, Long)) %>% data.matrix(., rownames.force = TRUE) 
    c <- as.data.frame(dist1(activity1)) 
      row.names(c) <- c(rows.n) #### rename rows
      colnames(c) = c(cols.n)   #### rename cols
      
#       cc <-c[1:100, 1:100]
#       
#       cc <- add_rownames(cc, "EUC") 
#   
#   #' melt to long form for exporting. It takes forever, so watch out
#   ccm<- melt(cc, id.var = "EUC")
      

      # add column with rowname to melt on
      c <- add_rownames(c, "EUC")
      #' now melt
      cc <- melt(c, id.var = "EUC")
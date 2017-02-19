library(sp)    # this is the workhorse of the spatial world in R
library(rgdal) # for reading and writing shapefiles effortlessly
library(rgeos) # gRelate and gUnary functions for DE-9IM
library(dplyr)
library(maptools) # needed for SpRbind
library(reshape2)  

###############################################################################
#' Set Functions for generating the first and second set of TAZs using GA
###############################################################################

fun_un1 <- function(field, val, sseq){
  " 
  This function generates a list of dissolved TAZs based on the first case noted
  in script.
  
  Inputs: Dissemination Area shapefile; attribute column; value in 
  column that helps select the DA.
  
  Arguments:
  First, activity points column in the shapefile table that will be used to subset 
  dataframe.
  Second, dwelling unit or DA value by which to subset dataframe i.e. 0 for first case
  and 1 for second case.
  Third, starting sequence value.
  
  Returns: 
  First, shapefile subsetted to represent either the first case.
  Second, shapefile table.
  Third, table with unique CSDs and a sequence of numbers to dissolve on. 
  
  "
  
  taz <- da1[da1@data[[field]] == val & da1@data[["GGH"]] == 0 & 
               da1@data[["CntDA"]] > 1,]
  
  # get table from shapefile and start process
  tt <- taz@data
  un <- as.data.frame(unique(tt$CSDUID)) %>% 
    transform(., 
              UnNum = seq(sseq,length.out = nrow(as.data.frame(unique(tt$CSDUID)))))
  # reset column names
  colnames(un) <- c("CSDUID", "DisNum") 
  
  # return 
  list(taz, tt, un)
} 


#' this function generates a unique table of CSDs that is then fed to the other
#' functions. This is used for the SECOND set of TAZs only

fun_un2 <- function(field, val, sseq){
  " 
  This function generates a list of dissolved TAZs based on the first and
  second cases noted before in script.
  
  Inputs: Dissemination Area shapefile; attribute column; value in 
  column that helps select the DA.
  
  Arguments:
  First, Count of DA in the shapefile table that will be used to subset 
  dataframe.
  Second, dwelling unit or DA value by which to subset dataframe; 1 for second case.
  Third, starting sequence value.
  
  Returns: 
  First, shapefile subsetted to represent either the first or second case.
  Second, shapefile table.
  Third, table with unique CSDs and a sequence of numbers to dissolve on. 
  
  "
  
  taz <- da1[da1@data[[field]] == val & da1@data[["GGH"]] == 0,]
  
  # get table from shapefile and start process
  tt <- taz@data
  un <- as.data.frame(unique(tt$CSDUID)) %>% 
    transform(., 
              UnNum = seq(sseq,length.out = nrow(as.data.frame(unique(tt$CSDUID)))))
  # reset column names
  colnames(un) <- c("CSDUID", "DisNum") 
  
  # return 
  list(taz, tt, un)
} 


#' this function takes the unique CSD dataframe from the above two functions and 
#' generates a shapefile. 

fun_shp <- function(tt, un, taz){
  "
  This function takes the unique dataframe created in the previous function and
  generates a dissolved shapefile
  
  Arguments:
  First, shapefile table. 
  Second, table with unique CSDs and a sequence of numbers to dissolve on.
  Third, shapefile subsetted to represent either the first or second case.
  
  Returns: Dissolved shapefile that contains Provincial TAZs
  
  "
  # Now merge the information together and join it back to shapefile datatable
  tt3 <- merge(tt, un, by.x = "CSDUID", by.y = "CSDUID", all.x = TRUE) %>% 
    subset(., select = c("CSDUID", "DisNum", "units", "TruckStops", "dwell", "TotAct"))
  
  # get rid of factor columns
  indx <- sapply(tt3, is.factor)
  tt3[indx] <- lapply(tt3[indx], function(x) as.numeric(as.character(x)))
  
  # merge the unique values for dissolving back to the shapefile table
  taz@data = data.frame(taz@data,tt3[match(taz@data$CSDUID, 
                                           tt3$CSDUID),])
  
  # now dissolve the DAs on the field "DisNum"
  taz2 <- gUnaryUnion(taz, id = taz@data$DisNum)
  
  # prepare table to attach to dissolved polygons to make into spatial dataframe
  spdf <- tt3 %>% group_by(DisNum) %>% summarise(DU = sum(dwell), Stops = sum(TruckStops), 
                                                 CSD = min(CSDUID), Act = sum(TotAct))
  spdf <- as.data.frame(spdf)
  
  rownames(spdf) <- spdf$DisNum
  
  # make shapefile 
  taz2_spdf <- SpatialPolygonsDataFrame(taz2, spdf)
  
  return(taz2_spdf)
}

###############################################################################
######### Functions for fixing the multipart polygons #########################
###############################################################################

multipart <- function(k){
  
  "
  This function gets the list of DAs that belong to the CSDs that are
  multipart and cannot be fixed by the procedure in functions Step 1 to
  Step 3.
  
  Inputs: dataframe describing the CSDs that need to be hacked; census 
  subdivision polygons and the DA polygons.
  
  Arguments: the number representing the row in the multi dataframe
  
  Returns: dissolved spatial polygon data frame
  
  "
  
  taz_fourth_multi <- csd_poly[csd_poly@data$CSDUID == multi$TAZ_NO[k],]
  #' dissolve the poly using the maptools function as that does not care if the 
  #' polygon is single or multi-part
  taz_fourth_multi1 <- unionSpatialPolygons(taz_fourth_multi, ID = multi$TAZ_NO[k])
  #' create dataframe for dissolved poly
  temp_spdf <- as.data.frame(subset(du_avg, CSDUID %in% multi$TAZ_NO[k]) %>% group_by(.) %>% 
                               summarise(TAZ_NO = min(.$CSDUID), TotAct = sum(dwell*4 + TruckStops/365)))
  #'set rownames  
  rownames(temp_spdf) <- temp_spdf[, 1]
  #' create spatial polygons dataframe
  multi1 <- SpatialPolygonsDataFrame(taz_fourth_multi1, temp_spdf)
  
  return(multi1)
}

###############################################################################
################# Functions for the Generalized Case ##########################
###############################################################################

step1 <- function(origin_DA, all_DA) {
  
  "
  This function gets the starting TAZ in the CSD after sorting the activity in 
  the descending order, as already done in the TDF dataframe.
  In the first iteration the starting TAZ is the very first one,
  but in subsequent TAZs the list of TAZs (TDF) needs to be reduced.
  
  Inputs: TAZ shapefile belonging to a CSD(t); Dataframe of TAZs within a CSD(tdf);
  Starting TAZ (start)
  
  Arguments: Same as inputs 
  
  Returns: Melted form of a relate table that shows the DE-9IM topology for the
  starting TAZ against all the other TAZs in the CSD; and the list of TAZs
  that were used for calculating the gRelate function.
  "
  rel <- as.data.frame(gRelate(t1, t, byid = T)) 
  #' drop the first row as it represents the zone itself and then reset columnames
  #' Also add in the rownames (DAs) as a column for merging in the activity values
  #' and then sorting them in descending order.
  rel$Start <- colnames(rel[1])
  rel$DAUID <- rownames(rel)
  colnames(rel) <- c("Spatial", "Start", "DAUID")
  rel$Touch <- substr(rel$Spatial, 5, 5)
  #' create dataframe of the starting DA. The related table will be appended to this.
  first <- rel %>% .[1, ]
  first$DAUID <- first$Start
  first$Spatial <- "FF2F11212"
  first$Touch <- "1"
  rownames(first) <- first$Start
  
  #' bind dataframe and merge the TotAct information. Two merges have to be done here
  #' The first merge is with R1 (a dataframe of the round 1 dissolving) and then with
  #' taz_fourth
  rel1 <- rbind(first, rel)
  rel1 <- merge(rel1, r1, by = "DAUID", all.x = TRUE ) %>% 
    merge(., taz_fourth@data, by = "DAUID", all.x = TRUE) %>% 
    subset(., select = c("DAUID", "Start", "Spatial", "TotAct", "CumAct")) %>%
    arrange(., CumAct, -TotAct)
  # set NAs to zero and trnasfer the CumActivity value to the TotalActivity for the
  # first row
  rel1[is.na(rel1)] <- 0
  rel1$TotAct[1] <- rel1$CumAct[1]
  
  rel1 <- transform(rel1, Touch = substr(rel1$Spatial, 5, 5)) %>% 
    subset(., Touch == "1") %>% 
    arrange(., -CumAct, -TotAct) %>%
    transform(., CumAct = cumsum(TotAct))
  
  return(rel1)
}
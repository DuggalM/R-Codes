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
  # get rid of factor columns

  tt3 <- merge(tt, un, by.x = "CSDUID", by.y = "CSDUID", all.x = TRUE) %>% 
    subset(., select = c("CSDUID", "DisNum", "units", "TruckStops", "dwell", "TotAct"))
  
  # get rid of factor columns
  indx <- sapply(tt3, is.factor)
  tt3[indx] <- lapply(tt3[indx], function(x) as.numeric(as.character(x)))
  
  # merge the unique values for dissolving back to the shapefile table
  taz@data = data.frame(taz@data,tt3[match(taz@data$CSDUID, 
                                           tt3$CSDUID),])
  t <- taz@data
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
  rel <- as.data.frame(gRelate(round1_spdf, t, byid = T)) 
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
  # only set TotAct column to cumulative if cumulative has a non-zero value.
  # This will indicate that it is the next round of analysis that is using 
  # an already dissolved polyugon
  if(rel1$CumAct[1] != 0){
    rel1$TotAct[1] <- rel1$CumAct[1]
  }
  
  rel1 <- transform(rel1, Touch = substr(rel1$Spatial, 5, 5)) %>% 
    subset(., Touch == "1") %>% 
    arrange(., -CumAct, -TotAct) %>%
    transform(., CumAct = cumsum(TotAct))
  
  return(rel1)
}


step2 <- function(){
  
  "
  This function takes the relative dataframe obtained from Step 1 and dissolving it
  while depreciating the shapefile and returning the first dissolved shapefile, 
  remaining shapefiles within the CSD, and a dataframe of the dissolved shapefile

  
  "
  
  
  rel_diss <- rel1[1:2, ]
  # get the TAZs that need to be dissolved
  diss1 <- t[t@data$DAUID %in% rel_diss$DAUID, ]

  # check for similarity. In the first round, the columns must be identical,
  # but in the second round i.e. when a dissovled polygon from step 1 is being
  # tested, the column names will have to be set equal to be able to bind them
  # together for dissolving
  if(identical(round1_spdf@data, diss1@data)){
    round1 <- spRbind(round1_spdf, diss1)
  } else {
    round1_spdf@data$TAZ_NO <- round1_spdf@data$DAUID
    round1_spdf@data$CSDUID <- 0
    round1_spdf@data$dwell <- 0
    round1_spdf@data$GGH <- 0 
    round1_spdf@data$TruckStops <- 0
    
    round1_spdf <- round1_spdf[, c("TAZ_NO", "CSDUID", "dwell", "GGH", 
                                   "DAUID", "TruckStops", "TotAct", "CumAct")]
    # Now bind
    round1 <- spRbind(round1_spdf, diss1)
  }
  
  round1@data$Dis <- "D" # create column to dissolve
  # dissolve the polygons
  round1 <- gUnaryUnion(round1, id = round1@data$Dis)
  
  # get the second row of the datafame as this has the cumulative totals
  spdf <- rel_diss[2,] %>% transform(., TotAct = CumAct)
  # set rownames of the dataframe and the polygon ID for binding them together
  rownames(spdf) <- spdf$DAUID 
  round1@polygons[[1]]@ID <- as.character(spdf$DAUID)
  #' get the merged polygon shapefile
  round1_spdf <- SpatialPolygonsDataFrame(round1, spdf)

  # Depreciate the list of DAs in the CSD by removing the ones that were merged.
  t <- t[!(t@data$DAUID %in% diss1@data$DAUID), ]
  tdf <- subset(tdf, !(DAUID %in% diss1@data$DAUID))
  
  taz_fourth <- taz_fourth[!(taz_fourth@data$DAUID %in% rel_diss$DAUID), ]
  r1 <- round1_spdf@data %>% subset(., select = c("DAUID", "CumAct"))
  
  list(round1_spdf, t, r1, tdf, taz_fourth)
  
}

###############################################################################
################# Function  for creating Unique Polys #########################
###############################################################################
# Function to set unique names and polygon IDs
un_names <- function(spdf_temp, val){
  " 
  This function generates an unique ID for each TRESO polygon. When used in the proper
  sequence i.e. called for each of TRESO's zone categories and corresponding starting
  values for the IDs, this function allows one to spRbind all the TRESO zones into one
  master list.
  
  Arguments: A spatial polygon dataframe, starting value of Polygon ID sequence
  
  Returns: A spatial polygon dataframe where IDs match to those of the data 
  
  
  "
  # reset rownames of the spatial data frame
  df <- spdf_temp@data
  rownames(df)[1:nrow(df)] <- paste0(seq(from = val, by = 1, 
                                         length.out = nrow(df)))
  
  #' set polygon IDs
  for (i in 1:nrow(spdf_temp@data)){
    #' set polygon IDs to match TAZ numbers
    spdf_temp@polygons[[i]]@ID <- as.character(rownames(df)[i])
    
  }
  
  # create new spatial polygons dataframe
  new_spdf <- SpatialPolygonsDataFrame(spdf_temp, df)
  
  return(new_spdf)
}

###############################################################################
################# Function for splitting TRESO zones ##########################
###############################################################################
#' Zone splitting function
zone_splits <- function(spdf, sval){
  "
  This function splits a zone by the single line highway network. Also, if the area of 
  the split zone is below a certain percentage (user defined) of the parent zone
  then it is discarded.
  
  Inputs: Single line road network (rd_ln)

  Arguments: TRESO zones to be split, starting value for the Polygon IDs


  Returns:

  
  "
  # get zone
  get_zone <- spdf[i, ]
  print(spdf[i, ]@polygons[[1]]@ID)
  #plot(get_zone, col = "green")
  # create multiple pieces of the single line road network after intersection
  int_1 <- gIntersection(get_zone, rd_ln)
  # get the area of the polygons and populate the necessary fields to assist 
  # in binding later on
  gzone_area <- get_zone@polygons[[1]]@area
  get_zone@data$area <- get_zone@polygons[[1]]@area
  get_zone@data$PArea <- get_zone@polygons[[1]]@area
  
  # dummy buffer value that
  w1 <- 0.000001
  
  if(length(int_1) >0){
    
    w1 <- 0.000001
    blpi <- gBuffer(int_1, width = 0.000001)  # create a very thin polygon 
    # There are instances when the buffer width is too small and the 
    # program crashes because blpi becomes null. Check for it and reset the
    # width.
    if (is.null(blpi)){
      w1 <- 0.000001*1000
      blpi <- gBuffer(int_1, width = w1)
      # drop area threshold
      #athold <- athold/2
    } else if (is.null(blpi)) {
      w1 <- 0.000001*10000
      blpi <- gBuffer(int_1, width = w1)
      # drop area threshold
      #athold <- athold/4
    }
    #buffer of the intersected line
    dpi <- gDifference(get_zone, blpi)
    # diaggregate the multipart polygons created by the above step
    d <- disaggregate(dpi)
    #' create dataframe and set columns
    df1 <- as.data.frame(matrix(nrow = length(d), ncol = 8))
    df1$V1 <- seq(sval, length.out = length(d))
    df1$V2 <- 0
    df1$V3 <- 0
    df1$V4 <- 0
    df1$V5 <- 0
    df1$V6 < "temp"
    df1$V7 <- get_zone@data$Tag
    df1$V8 <- get_zone@data$area
    colnames(df1) <- c("DisNum", "DU", "Stops", "CSD", "Act", "Type", "Tag", "PArea")
    rownames(df1) <- df1$DisNum
    #' create spatialdataframe, but first set ids
    d <- spChFIDs(d,rownames(df1)) 
    dpi_spdf <- SpatialPolygonsDataFrame(d, df1)
    #' now add in area and get rid of polygons 
    # Extract areas from polygon objects then attach as attribute
    areas <- data.frame(area=sapply(dpi_spdf@polygons, 
                                    FUN=function(x) {slot(x, 'area')}))
    row.names(areas) <- sapply(dpi_spdf@polygons, FUN=function(x) {slot(x, 'ID')})
    # Combine attributes info and areas 
    dpi_spdf1 <- spCbind(dpi_spdf, areas)
    
    ###########################################################################
    # This piece of code has been discontinued for the time being. It should only
    # be uncommented and run if the user wishes to run the algorithm below.
    ###########################################################################
    # If the newly minted polygons are below the user defined thresholds then don't
    # cut the TRESO zones using the highways as this creates very small polygons. This
    # is done once for the TRESO zones within the CSDs and once in the CTs.
    
    # if(dpi_spdf1@data$Tag[1] == "CSD"){
    #   # select the new zones that are below the threshold
    #   dpi_spdf2 <- dpi_spdf1[dpi_spdf1@data$area < athold_csd*gzone_area, ]
    #   # if there are zones with small areas then reset to the orignal zone
    #   if(nrow(dpi_spdf2@data) > 0){
    #     
    #     dpi_spdf1 <- get_zone
    # 
    #   }
    #   # if no small zones than carry on with the split zones
    #   dpi_spdf1 <- dpi_spdf1
    # 
    #   } else if (dpi_spdf1@data$Tag[1] == "CT") {
    #     # select the new zones that are below the threshold
    #     dpi_spdf2 <- dpi_spdf1[dpi_spdf1@data$area < athold_ct*gzone_area, ]
    #     # if there are zones with small areas then reset to the orignal zone
    #     if (nrow(dpi_spdf2@data) > 0){
    #       
    #       dpi_spdf1 <- get_zone
    # 
    #     }
    #     # if no small zones than carry on with the split zones
    #     dpi_spdf1 <- dpi_spdf1
    # 
    #   } else {
    #     
    #     dpi_spdf1 <- dpi_spdf1
    #   } 
    ###########################################################################     
    
    return(dpi_spdf1)
 
  }
    
    
  
  return(get_zone)
  
}

###############################################################################
################# Function for remergin TRESO zones ##########################
###############################################################################



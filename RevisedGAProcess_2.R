library(sp)    # this is the workhorse of the spatial world in R
library(rgdal) # for reading and writing shapefiles effortlessly
library(rgeos) # gRelate and gUnary functions for DE-9IM
library(dplyr)
library(maptools) # needed for SpRbind
library(reshape2)  


#' Set working directory
wd <- setwd("c:/personal/r")

###############################################################################
#' Title: "Provincial Gradual Aggregation"
#' Programmer: "Mausam Duggal, Systems Analysis Group, WSP|PB"
#' Algorithm Credits: "Rick Donnelly, Systems Analysis Group, WSP|PB", 
#'                    "Mausam Duggal, Systems Analysis Group, WSP|PB"
#' Date: "October 21, 2016"

#########################################################################################
# This code was developed to generate a traffic analysis zone system using three
# primary inputs. First, dwelling units at the DA level, truck points also 
# allocated to the DAs, and finall accessibility values of each DA.
# Similar to the Gradual Rasterization (Quad Tree) procedure, the intention is
# to be able to develop a zone system on the fly. Another consideration in this GA
# process is that DAs can be dissolved only within a CSD. Further, DAs within the GGH
# cannot be merged with thosed outside.

##########################################################################################

#' Add in two functions that I like to use. First is treating NAs and second is the
#' opposite of the %in% function in R

# function for %notin%
`%notin%` <- function(x,y) !(x %in% y) 

# function for setting NAs in dataframe to 0
f_rep <- function(df) {
  # this function is used to set all NA values to zero in a dataframe
  df[is.na(df)] <- 0
  return(df)
}

###############################################################################
#' Step 0: Batch in all the files and test for projection systems consistency
#' as well as the availability of fields. Also add in the thresholds
###############################################################################

#' User thresholds
ap_cnt <- 15000 # Activity threshold outside the GGH
ap_trk <- 20 # Truck Activity threshold outside the GGH

#' Start with batching in the shapefiles.
da_pol <- "DA"                  # Census DA Polygons
da_pt <- "DA_Centroids4"        # Census DA Centroids
csd_pol <- "CSD"                # Census CSD geography
ggh_taz <- "GGH_TAZ_1"          # GGH TAZ system
ggh_pt <- "ggh_taz_cen"         # GGH TAZ Centroidss

# batch in the shapefiles
da_poly <- readOGR(wd, da_pol)
da_cen <- readOGR(wd, da_pt)
csd_poly <- readOGR(wd, csd_pol)
ggh <- readOGR(wd, ggh_taz)
ggh_cen <- readOGR(wd, ggh_pt)


# Only keep relevant fields
da_poly <- da_poly[, c("CSDUID", "DAUID", "GGH", "dwell", "TruckStops")]
da_cen <- da_cen[, c("CSDUID", "DAUID", "GGH", "dwell", "TruckStops")]

# Get the projection systems 
crs_da_poly <- proj4string(da_poly) 
crs_da_cen <- proj4string(da_cen) 
crs_csd_poly <- proj4string(csd_poly) 
crs_ggh <- proj4string(ggh) 
crs_ggh_cen <- proj4string(ggh_cen)

#' Start cross-checking for missing attribute information in files and raise
#' errors for user to fix it.
{
  col_check <- c("CSDUID") # get column to check
  col <- names(da_poly@data) # get names from the data slot of the DA shapefile
  
  if(intersect(col_check, col) != "CSDUID")
    stop("Shapefile is missing CSDUID field.")
  
  
  col_check <- c("DAUID") # get column to check
  
  if(intersect(col_check, col) != "DAUID")
    stop("Shapefile is missing DAUID field.")
  
  col_check <- c("GGH") # get column to check
  
  if(intersect(col_check, col) != "GGH")
    stop("Shapefile is missing GGH field.")
  
  col_check <- c("dwell") # get column to check
  
  if(intersect(col_check, col) != "dwell")
    stop("Shapefile is missing dwell field.")
  
  col_check <- c("TruckStops") # get column to check
  
  if(intersect(col_check, col) != "TruckStops")
    stop("Shapefile is missing dwell field.")
  
}

# Check the GGH zone file
{
  col_check <- c("CSDUID") # get column to check
  col <- names(ggh@data) # get names from the data slot of the DA shapefile
  
  if(intersect(col_check, col) != "CSDUID")
    stop("Shapefile is missing CSDUID field.")
  
  
  col_check <- c("dwell") # get column to check
  
  if(intersect(col_check, col) != "dwell")
    stop("Shapefile is missing DAUID field.")
  
  col_check <- c("DAUID") # get column to check
  
  if(intersect(col_check, col) != "DAUID")
    stop("Shapefile is missing GGH field.")
  
}

# check if the projections are the same
{
  if (crs_da_poly != crs_da_cen || crs_da_poly != crs_ggh) 
    stop("Projection systems are not the same. 
         Please make them same before proceeding")
}

###############################################################################
#' There are four types of cases that are accounted for explicitly in GA. 
#' The FIRST: where there are 0 DUs in a CSD. This is the simplest case 
#' and the CSD is a TAZ. This can be further broken down based on external info
#' but has not been as yet implemented as the extra info desired in unavailable.
#' The SECOND: Where there is only one DA inside a CSD. The DA is set to be a TAZ.
#' Once again, upon the availability of extra information the single DA will be
#' further subdivided.
#' The THIRD: Where the number of DAs within a CSD are >1 and <= user defined
#' value (n_rows <- default of 4) and the total dwelling units are below the value 
#' (du_cnt <- default of 1500) defined by the user. 
#' In this case, all the DAs within the CSD are dissolved to form a TAZ.
#' The FOURTH: This is the generalized case where the algorithm works by selecting
#' a DA then successively selecting an adjacent DA to add together and dissolve.

###############################################################################

#' The Census CSD polygons come with multiparts, so it is needed to first disaggregate
#' those to individual polygons.

#' Disaggregate the CSD polygons
# dagg <- disaggregate(csd_poly)
# dagg_df <- dagg@data

# get the total DUs in a CSD as well as the average number in each DA. 
du_avg <- da_poly@data 
du_avg1 <- du_avg %>% group_by(CSDUID) %>% 
  summarise(units = sum(dwell), CntDA = n(), GGH = min(GGH), Stops = sum(TruckStops)) %>% 
  transform(., TotAct = as.integer(units*4 + Stops/365))
#' convert factors to numbers
indx <- sapply(du_avg, is.factor)
du_avg[indx] <- lapply(du_avg[indx], function(x) as.numeric(as.character(x)))

# create copy of the DA Polygons to start process and also populate activity points
da1 <- da_poly
da1@data <- transform(da1@data, TotAct = dwell*4 + TruckStops/365)
# transfer the data from the calculations of average values to shapefile table  
da1@data = data.frame(da1@data,du_avg1[match(da1@data$CSDUID, 
                                             du_avg1$CSDUID),])
# strip unnecessary fields in CSD and DA level shapefiles
da1 <- da1[, c("CSDUID", "DAUID", "GGH", "dwell", "TruckStops", "TotAct", "units", 
               "CntDA", "TotAct.1")]
#' rename column to represent CSD_Activities
da1@data <- rename(da1@data, CSD_TotAct = TotAct.1)

#' also set the activity points in the da_centroids file
da_cen@data <- transform(da_cen@data, TotAct = dwell*4 + TruckStops/365)
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
#' FIRST CASE OF PROVINCIAL ZONES
###############################################################################

# field in the shapefile to be used for getting CSDs with no activity. 
# select all those DA's that belong to a CSD that has 0 Dwelling Units in it
# select all those DA's that belong to a CSD that has 0 TruckStops in it
# starting sequence for numbering the DAs to dissolve on is set to 1

a1 <- "CSD_TotAct"
a2 <- 0
a3 <- 1

# this will get you a list of outputs
taz3 <- fun_un1(a1, a2, a3)

# unpack the list in to variables that will be used in the next function
taz <- taz3[[1]]
tt <- taz3[[2]]
un <- taz3[[3]]

# call second function that dissolves the DA shapefile and creates the shapefile
# for the FIRST CASE of the GRADUAL AGGREGATION process.

first_taz <- fun_shp(tt, un, taz)
#' save the file
writeOGR(first_taz, layer = paste0("TRESO1_", ap_cnt), wd, 
         driver="ESRI Shapefile", overwrite_layer = T)
###############################################################################
#' SECOND CASE OF PROVINCIAL ZONES
###############################################################################

# field in the shapefile to be used. In this case "CntDA".
# select all those DA's that belong to a CSD that has only 1 DA in it.
# starting sequence set to one number above the final value of the FIRST CASE.

a1 <- "CntDA"
a2 <- 1
a3 <- nrow(first_taz@data) + 1

# this will get you a list of outputs
taz3 <- fun_un2(a1, a2, a3)

# unpack the list in to variables that will be used in the next function
taz <- taz3[[1]]
tt <- taz3[[2]]
un <- taz3[[3]]

# call second function that dissolves the DA shapefile and creates the shapefile
# for the FIRST CASE of the GRADUAL AGGREGATION process.
sec_taz <- fun_shp(tt, un, taz)
#' save the file
writeOGR(sec_taz, layer = paste0("TRESO2_", ap_cnt), wd, 
         driver="ESRI Shapefile", overwrite_layer = T)

###############################################################################
#' THIRD CASE OF PROVINCIAL ZONES
###############################################################################
#' select those CSDs where there are more than 1 DA within a CSD and activity
#' is lower than the threshold.

# starting sequence set to one number above the final value of the FIRST and 
# SECOND CASES COMBINED.
a10 <- nrow(sec_taz@data) + nrow(first_taz@data) + 1
#' Prepare the inputs for the fun_shp function that will dissolve the DAs to 
#' make the THIRD CASE of provincial zones.

# first reduce the list of CSDs by removing those that have already been used
used <- rbind(first_taz@data, sec_taz@data) 
du_avg_left <- du_avg1[! du_avg1$CSDUID %in% used$CSD, ]
# get the CSDs that belong to the THIRD CASE of provincial zones
csd_below <- subset(du_avg_left, (CntDA > 1 & GGH == 0) & 
                      (TotAct > 0 & TotAct <= ap_cnt)) 

# Create the third set of TAZs
taz_third <- da1[da1@data$CSDUID %in% csd_below$CSDUID,]
# get dataframe of the shapefile
tt_df <- taz_third@data
# generate unique values to dissolve
un_df <- as.data.frame(unique(tt_df$CSDUID)) %>% 
  transform(., 
            UnNum = seq(a10,length.out = 
                          nrow(as.data.frame(unique(tt_df$CSDUID)))))
# reset column names
colnames(un_df) <- c("CSDUID", "DisNum")
# Generate the THIRD CASE of Provincial zones using the fun-shp function
third_taz <- fun_shp(tt_df, un_df, taz_third)
#' write out the files
writeOGR(third_taz, layer = paste0("TRESO3_", ap_cnt), wd, 
         driver="ESRI Shapefile", overwrite_layer = T)


###############################################################################
#' Generalized Case
###############################################################################
###########
#' The logic being tested is the following:
#' Step 0: Get the starting TAZ within the candidate CSD being studied
#' Step 1: Select the starting TAZ that touches it and check if the activity threshold is met
#' Step 2: If not met, then repeat Step 1
#' Step 3: When all the zones that touch the starting TAZ are exhausted and the threshold is yet not met:
  #' Then get the zones that touched the second TAZ that was merged and exhaust that list
  #' If threhold is yet not met, then go to the third TAZ that was merged and exhaust the list
  #' Continue till threshold is met
#' Deprecate the list of TAZs to avoid overlap
#' 
######### Functions for creating Gradual Aggregation within the GGH
step1 <- function(t, tdf, start) {
  
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
  #' Now reduce the Master TAZ list for the CSD in question by removing
  #' the Starting TAZ. This is required so that the relate dataframe is 
  #' easier to work with.
  t1 <- t[!(t@data$TAZ_NO %in% start@data$TAZ_NO[1]), ]
  if(nrow(t1) > 0){
    #' Create a gRelate dataframe using the DE-9IM topology model
    rel <- as.data.frame(gRelate(start, t1, byid = T)) 
    rel$ID <- rownames(rel)
    rel$Start <- colnames(rel[1])
    rel$Touch <- substr(rel[[1]], 5,5) 
  } else {
    #' set t1 equal to t as this is the last TAZ in the CSD and also reset the
    #' DE-9IM code that indicates that the TAZ touches another as the code is
    #' reporting a TAZ touching itself.
    t1 <- t
    rel <- as.data.frame(gRelate(t1, byid = T)) 
    rel$ID <- rownames(rel)
    rel$Start <- colnames(rel[1])
    rel$Touch <- substr(rel[[1]], 5,5)
    rel$Touch <- "0"
  }

#' GRelate Output
  list(rel, t1)
}


step2 <- function(t, rel) {
  
  "
  This function gets rid of all those records that do not satisfy the touch 
  criteria and further only keep the first TAZ that touches the starting TAZ

  Inputs: TAZ shapefile belonging to a CSD(t); GRelate table for the starting zone
          to all other zones(rel)

  Arguments: Same as inputs 

  Returns: First, a GRelate dataframe that only has records where the starting
           TAZ touches any other TAZ. Second, a shapefile of TAZs that coresspond
           to the zones in GRelate dataframe         
  "
  rel_df <- subset(rel, Touch == "1") 

  if(nrow(rel_df)>=1){
    #' set condition such that if there is no TAZ touching the starting TAZ then
    #' it needs to be output as an individual zone.  
    rel_df <- subset(rel_df, select = c("ID", "Start", "Touch"))
    #' add in a row for the Starting TAZ and calculate cumulative value of activity
    newrow <- c(rel_df$Start[1], rel_df$Start[1], "1" )
    rel_df = rbind(newrow,rel_df) 
    #' get rid of character columns
    indx1 <- sapply(rel_df, is.character)
    rel_df[indx1] <- lapply(rel_df[indx1], function(x) as.numeric(x))
    #' join the data to get cumulative activity, but sort the rows, except for the first
    #' by asending order of activity. This will avoid merging zones with significant activity.
    #' Once this is done, then select the FIRST TAZ that touches the STARTING TAZ. This will 
    #' always be row number two (2) 
    rel_df <- inner_join(rel_df, tdf, by = c("ID" = "TAZ_NO")) 
    rel_sub <- rel_df[2:nrow(rel_df),] %>% arrange(., TotAct)
    rel_df <- rbind(rel_df[1,], rel_sub) %>% subset(., select = c("ID", "Start", "Touch", "TotAct")) %>% 
      transform(., CumAct = cumsum(TotAct)) %>% .[2, ]
    
    #' get shapefile of TAZ the FIRST that touches the starting TAZ.  
    ftaz <- t[t@data$TAZ_NO == tail(rel_df$ID, 1),]
    ftaz@data$CumAct <- 0
    ftaz@data$Start <- 0
    ftaz@data$Touch <- "1"
    ftaz <- ftaz[, c("TAZ_NO", "Start", "Touch", "TotAct", "CumAct")]
    
  } else {
    #' this ftaz does not matter as it is never used once this If condition is satisfied, which
    #' is that the number of rows in the rel_df dataframe are 0.
    ftaz <- t[1,]
    ftaz@data$CumAct <- ftaz@data$TotAct
    ftaz@data$Start <- 0
    ftaz@data$Touch <- "F"
    ftaz <- ftaz[, c("TAZ_NO", "Start", "Touch", "TotAct", "CumAct")]
    
    #' return empty dataframe 
    rel_df <- data.frame(TAZ_NO = double(),
                            Start = double(),
                            Touch = double(),
                            TotAct = double(),
                            CumAct = double()) 

  }
  #' returns dataframe and subsetted shapefiles
  list(rel_df, ftaz)
}

step3 <- function(start, rel_df, ftaz, number) {
  
  "
  This function takes the outputs from Step 1 and Step 2 and creates a dissolved
  polygon while adding in a column for cumulative values.
  
  Inputs: Dataframe of TAZs within a CSD (tdf); GRelate dataframe that only has 
          records where the starting TAZ touches any other TAZ (rel_df); a shapefile 
          of TAZs that coresspond to the zones in GRelate dataframe (ftaz); 
          TAZ shapefile of the starting TAZ within a CSD (start) 
  
  Arguments: TAZ shapefile of the starting TAZ within a CSD; GRelate dataframe that 
             only has records where the starting TAZ touches any other TAZ; a shapefile 
             of TAZs that coresspond to the zones in GRelate dataframe; number to add 
             to the TAZ ID for establishing whether it lies in the GGH or Case 4 of the
             DAs that are outside the GGH
  
  Returns: TAZ shapes to be dissolved; Dissolve Polygon Shapefile(round1_spdf)

  "
  diss1 <- spRbind(start, ftaz)
  #' create field to dissolove on and dissolve the polygons. Add an extra 10000 to the TAZ id
  #' to use it later for tracking deleting the summarised TRESO zones from the master TAZ list
  if (ftaz@data$Touch != "F"){
    diss1@data$Dis <- tail(rel_df$ID, 1) + number
  } else {
    diss1@data$Dis <- tail(start@data$TAZ_NO, 1) + number
  }
    round1 <- gUnaryUnion(diss1, id = diss1@data$Dis)
  #' prepare table to attach to dissolved polygons to make into spatial 
  #' dataframe. 
  spdf <- tail(rel_df, 1)
  spdf$ID <- spdf$ID + number
  spdf <- rename(spdf, TAZ_NO = ID)
  #' set rownames of the spdf to the TAZ_NO as this is needed for the 
  #' successful creation of a spatial polygons dataframe
  rownames(spdf) <- spdf$TAZ_NO
  #' make shapefile 
  round1_spdf <- SpatialPolygonsDataFrame(round1, spdf)
  
  list(round1_spdf, diss1, spdf)
}



###############################################################################
###############################################################################
#' FOURTH CASE OF PROVINCIAL ZONES
###############################################################################
# select those records where there are more than 1 DA within a CSD and dwelling
# units are greater than DU threshold and trucks stops are greater than threshold 
# and it does not belong in the GGH area


# starting sequence set to one number above the final value of the FIRST,  
# SECOND, and THIRD cases combined.
a20 <- nrow(third_taz@data) + nrow(sec_taz@data) + nrow(first_taz@data) + 1

#' first reduce the list of CSDs by removing those that have already been used
used1 <- rbind(used, third_taz@data) 
du_avg_left1 <- du_avg1[! du_avg1$CSDUID %in% used1$CSD, ] %>% subset(., GGH == 0)

#' remove multipart CSDs as these are a cause for significant
#' headache. This CSD will be automatically set to a TRESO ZONE after dissolving
multi <- data.frame(TAZ_NO = double())
multi[1,] <- 3549005
multi[2, ] <- 3558075
multi[3, ] <- 3552093

#' Multipart Polygon TRESO ZONE. Create a function first and the run it.
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

#' get dissolved polygos
t_multi1 <- multipart(1)
t_multi2 <- multipart(2)
t_multi3 <- multipart(3)

#' now bind them
taz_fourth_multi <- spRbind(t_multi1, t_multi2)
taz_fourth_multi <- spRbind(taz_fourth_multi, t_multi3)
#' write out the multi-part CSD as a TRESO Zone
writeOGR(taz_fourth_multi, layer = paste0("TRESO4_Multi", ap_cnt), wd, 
         driver="ESRI Shapefile", overwrite_layer = T)

# get the CSDs that belong to the FOURTH CASE of provincial zones. The first
# element in the list has only 2 DAs within the CSD 
csd_ab <- subset(du_avg_left1, !(du_avg_left1$CSDUID %in% multi$TAZ_NO))

# Create the DAs that lie within the CSDs that are being evaluated. But first
# take out those DAs that are above the truck point threshold identified 
# early on in the process by the user. The user defines daily truck activity
# thresholds that are converted to annual estimates by multiplying by 365
taz_da_activity <- da_poly[(da_poly@data$TruckStops > ap_trk*365) & 
                             (da_poly@data$GGH == 0), ]

#' write out the DAs that exceed the truck activity threshold
writeOGR(taz_da_activity, layer = paste0("TRESO4_Tr_activity", ap_trk), wd, 
         driver="ESRI Shapefile", overwrite_layer = T)

#' get remaining DAs for generalized processing
taz_fourth <- da1[(da1@data$CSDUID %in% csd_ab$CSDUID) & 
                    !(da1@data$DAUID %in% taz_da_activity@data$DAUID),]


# get table from subsetted shapefile and get unique CSD's while sequentially
# numbering them for dissolving later on.
un20 <- as.data.frame(taz_fourth@data) %>% group_by(CSDUID) %>% 
  summarise(Activity = sum(TotAct)) %>% 
  transform(., DisNum = seq(a20, length.out = nrow(.))) %>%
  subset(., select = c(CSDUID, DisNum, Activity))

# get the dataframe of the taz_fourth shapefile
tf <- taz_fourth@data %>% subset(., select = c(DAUID, CSDUID, dwell, TruckStops, 
                                               TotAct, CSD_TotAct)) 
# get unique CSDs for the DAs that are above the thresholds and convert from 
# factors to numerics
lst <- as.data.frame(as.numeric(as.character(unique(un20$CSDUID))))
colnames(lst) <- "CSD"
lst <- arrange(lst, CSD) 


###################################################################
############## Start with first CSD that belongs to the ###########
############## FOURTH CASE ########################################

#' start loop over FIRST CSDs
for (j in 1:1){
  print(lst$CSD[j])
  #' get copy of the TAZs to be processed
  #' attempt this for one CSD for the time being. It will get repeated later on 
  #' for each CSD
  t <- taz_fourth[taz_fourth@data$CSDUID %in% lst$CSD[j], ]
  t@data$TAZ_NO <- t@data$DAUID
  t <- t[, c("TAZ_NO", "CSDUID", "dwell", "GGH", "DAUID", "TruckStops", "TotAct")]
  indx <- sapply(t@data, is.factor)
  t@data[indx] <- lapply(t@data[indx], function(x) as.numeric(as.character(x)))
  rownames(t@data) <- t@data$TAZ_NO
  

  # set polygon ids to match TAZ Nos to avoid errors later in the process
  for (i in 1:nrow(t@data)){
    #' set polygon IDs to match TAZ numbers
    t@polygons[[i]]@ID <- as.character(t@data$TAZ_NO[i])
    
  }
  # get dataframe and sort the TAZs in ascending order of Activity
  tdf <- t@data %>% arrange(., -c(TotAct))
  
  ##########################################################################
  ########### Outer Step 0: START DA in FIRST CSD #########################

  #' to ensure that the TAZs only belong to the Case 4
  tdf <- subset(tdf, TAZ_NO < 36000000)
  #' get first TAZ in CSD master list or reduced list
  sub_lst <- as.data.frame(tdf$TAZ_NO[1])
  colnames(sub_lst) <- "TAZ_NO"
  #' get starting TAZ and add in column for Cumulative Acitvity
  start <- t[t@data$TAZ_NO %in% sub_lst$TAZ_NO[1], ] 
  start@data$CumAct <- 0
  start@data$Start <- 0
  start@data$Touch <- "1"
  start <- start[, c("TAZ_NO", "Start", "Touch", "TotAct", "CumAct")]

  ######## **Step 1** #########
  s1 <- step1(t, tdf, start)
  rel <- s1[[1]]
  t1 <- s1[[2]]
  
  ######## **Step 2** #########
  s2 <- step2(t, rel)
  #' get outputs
  rel_df <- s2[[1]]
  ftaz <- s2[[2]]
  
  ######## **Step 3** #########
  s3 <- step3(start, rel_df, ftaz, 1000000)
  diss1 <- s3[[2]]
  round1_spdf <- s3[[1]]
  spdf1 <- s3[[3]]
  
  s <- round1_spdf@data
  
  ######## **Step 4** #########
  #' Reduce the TAZ dataframe for the CSD in question, but add in the data for
  #' the dissolved polygon that will allow the Step 2 function to perform an 
  #' inner join between the tdf and rel_df Dataframes. 
  tdf <- subset(tdf, !(TAZ_NO %in% rel_df$ID) & !(TAZ_NO %in% rel_df$Start))
  if(nrow(tdf) > 0){
    newrow <- c(spdf1$TAZ_NO[1], 0, 0, 1, spdf1$TAZ_NO[1], 0, spdf1$CumAct[1])
    tdf <- rbind(tdf, newrow)
  }
  #' select zones that are now only in the revised tdf Dataframe
  t <- t[t@data$TAZ_NO %in% tdf$TAZ_NO,]

  ######## **Step 5**
  #' evaluate if the polygon created in Step 3 has passed the threshold.
  #' if it has not, then loop back to Step 1. Otherwise save it as a zone
  #' and move to the next TAZ within the CSD.
  
  while (round1_spdf@data$CumAct < ap_cnt){
    
    #' get starting TAZ
    start <- round1_spdf
    
    ######## **Step 1** #########
    s1 <- step1(t, tdf, start)
    rel <- s1[[1]]
    t1 <- s1[[2]]
    
    ######## **Step 2** #########
    s2 <- step2(t, rel)
    #' get outputs
    rel_df <- s2[[1]]
    ftaz <- s2[[2]]
    f <- ftaz@data
    
    ######## **Step 3** #########
    s3 <- step3(start, rel_df, ftaz, 1000000)
    diss1 <- s3[[2]]
    round1_spdf <- s3[[1]]
    spdf2 <- s3[[3]]
    
    plot(round1_spdf, col = "red")
    d <- diss1@data
    s <- round1_spdf@data
    ######## **Step 4** #########
    #' Reduce the TAZ dataframe for the CSD in question
    tdf <- subset(tdf, !(TAZ_NO %in% rel_df$ID) & !(TAZ_NO %in% rel_df$Start))
    newrow <- c(spdf2$TAZ_NO[1], 0, 0, 1, spdf2$TAZ_NO[1], 0, spdf2$CumAct[1])
    tdf <- rbind(tdf, newrow)
    #' select zones that are now only in the revised tdf Dataframe
    t <- t[t@data$TAZ_NO %in% tdf$TAZ_NO,]
  }
  
  
  ###############################################################################
  #' Remaining zones now in the First CSD
  
  #' test to see if any valid TAZs are left in the tdf dataframe
  tdf <- subset(tdf, TAZ_NO < 36000000)
  #' if only one record is left after Round 1 then that becomes the a TRESO zone
  #' and just append it to the Round1 TRESO Zones
  if(nrow(tdf) == 1) {
    lastTAZ <- t[t@data$TAZ_NO %in% tdf$TAZ_NO[1], ]
    lastTAZ@data$CumAct <- 0
    lastTAZ@data$Start <- 0
    lastTAZ@data$Touch <- "1"
    lastTAZ <- lastTAZ[, c("TAZ_NO", "Start", "Touch", "TotAct", "CumAct")]
    
    # now bind to the round1 Polygon
    round1_spdf <- spRbind(round1_spdf, lastTAZ)
  }
  
  #' If the above test is unsuccessful for the last TAZ then continue below.    
  while (nrow(tdf) > 1){
    
    tdf <- subset(tdf, TAZ_NO < 36000000)
    
    #' get first TAZ in CSD master list or reduced list
    sub_lst <- as.data.frame(tdf$TAZ_NO[1])
    colnames(sub_lst) <- "TAZ_NO"
    #' get starting TAZ and add in column for Cumulative Acitvity
    start <- t[t@data$TAZ_NO %in% sub_lst$TAZ_NO[1], ] 
    start@data$CumAct <- 0
    start@data$Start <- 0
    start@data$Touch <- "1"
    start <- start[, c("TAZ_NO", "Start", "Touch", "TotAct", "CumAct")]
    
    ######## **Step 1** #########
    s1 <- step1(t, tdf, start)
    rel <- s1[[1]]
    t1 <- s1[[2]]
    
    ######## **Step 2** #########
    s2 <- step2(t, rel)
    #' get outputs
    rel_df <- s2[[1]]
    ftaz <- s2[[2]]
    
    ######## **Step 3** #########
    if(nrow(rel_df) > 0){
      s3 <- step3(start, rel_df, ftaz, 1000000)
      diss1 <- s3[[2]]
      round2_spdf <- s3[[1]]
      spdf3 <- s3[[3]]
    } else {
      round2_spdf <- start
      round2_spdf@data$CumAct <- round2_spdf@data$TotAct 
      spdf3 <- round2_spdf@data[1,] %>% transform(., CumAct = TotAct) %>% 
        transform(., TAZ_NO = TAZ_NO + 1000000) %>% 
        transform(., Start = TAZ_NO)
    }
    
    s <- round2_spdf@data
    
    ######## **Step 4** #########
    #' Reduce the TAZ dataframe for the CSD in question, but add in the data for
    #' the dissolved polygon that will allow the Step 2 function to perform an 
    #' inner join between the tdf and rel_df Dataframes.
    if (nrow(rel_df) > 0){
      tdf <- subset(tdf, !(TAZ_NO %in% rel_df$ID) & !(TAZ_NO %in% rel_df$Start))
    } else {
      tdf <- subset(tdf, !(TAZ_NO %in% start@data$TAZ_NO))
    }
    #' create and add in the newrow
    newrow <- c(spdf3$TAZ_NO[1], 0, 0, 1, spdf3$TAZ_NO[1], 0, spdf3$CumAct[1])
    tdf <- rbind(tdf, newrow)
    #' select zones that are now only in the revised tdf Dataframe
    t <- t[t@data$TAZ_NO %in% tdf$TAZ_NO,]
    
    ######## **Step 5**
    #' evaluate if the polygon created in Step 3 has passed the threshold.
    #' if it has not, then loop back to Step 1. Otherwise save it as a zone
    #' and move to the next TAZ within the CSD. Also one needs to check if there
    #' are enough records or zones left for merging, as well as if the rel_df dataframe
    #' was null then it means that there are no touching TAZs and no more zones can
    #' be merged. 
    
    while (round2_spdf@data$CumAct < ap_cnt && nrow(tdf) > 0 && nrow(rel_df) > 0){
      
      #' get starting TAZ
      start <- round2_spdf
      
      #' get the areas of the two polygons. If they are the same, then it means that
      #' the cumulative activity after mergign TAZs has not met the threhold, and there
      #' is a need to exit the loop otherwise it will end up being an infinite loop. The
      #' way to do that is to check if the start and roun2_spdf have the same area, and if
      #' so then break the while loop. Also, if only one row is left in the tdf dataframe
      #' then the loop needs to break.
      s_area <- sapply(slot(start, "polygons"), slot, "area")
      round2_area <- sapply(slot(start, "polygons"), slot, "area")
      
      if(s_area != round2_area){
        ######## **Step 1** #########
        s1 <- step1(t, tdf, start)
        rel <- s1[[1]]
        t1 <- s1[[2]]
        
        ######## **Step 2** #########
        s2 <- step2(t, rel)
        #' get outputs
        rel_df <- s2[[1]]
        ftaz <- s2[[2]]
        
        ######## **Step 3** #########
        if(nrow(rel_df) > 0){
          s3 <- step3(start, rel_df, ftaz, 1000000)
          diss1 <- s3[[2]]
          round2_spdf <- s3[[1]]
          spdf4 <- s3[[3]]
        } else {
          round2_spdf <- start
          round2_spdf@data$CumAct <- round2_spdf@data$TotAct 
          spdf4 <- round2_spdf@data[1,] %>% transform(., CumAct = TotAct) %>% 
            transform(., TAZ_NO = TAZ_NO + 1000000) %>% 
            transform(., Start = TAZ_NO)
        }
        
        d <- diss1@data
        s <- round2_spdf@data
        ######## **Step 4** #########
        #' Reduce the TAZ dataframe for the CSD in question
        if (nrow(rel_df) > 0){
          tdf <- subset(tdf, !(TAZ_NO %in% rel_df$ID) & !(TAZ_NO %in% rel_df$Start))
        } else {
          tdf <- subset(tdf, !(TAZ_NO %in% start@data$TAZ_NO))
        }
        #' create and add in the newrow
        newrow <- c(spdf4$TAZ_NO[1], 0, 0, 1, spdf4$TAZ_NO[1], 0, spdf4$CumAct[1])
        tdf <- rbind(tdf, newrow)
        #' select zones that are now only in the revised tdf Dataframe
        t <- t[t@data$TAZ_NO %in% tdf$TAZ_NO,]
        
      } #' break the loop
      if (s_area == round2_area | nrow(tdf) == 1) {
        
        break
      }
      
    }
    
    #' bind the provincial zones
    round1_spdf <- spRbind(round1_spdf, round2_spdf)
    plot(round1_spdf, col = "blue")
    
    #' close the outer while loop  
  }
  #' close the for loop
}

#################### NOW REMAINING CSDs #######################################
###############################################################################
#' start loop over all the CSDs

for (j in 2:nrow(lst)){
  
  print(lst$CSD[j])
  #' get copy of the TAZs to be processed
  #' attempt this for one CSD for the time being. It will get repeated later on 
  #' for each CSD
  t <- taz_fourth[taz_fourth@data$CSDUID %in% lst$CSD[j], ]
  t@data$TAZ_NO <- t@data$DAUID
  t <- t[, c("TAZ_NO", "CSDUID", "dwell", "GGH", "DAUID", "TruckStops", "TotAct")]
  indx <- sapply(t@data, is.factor)
  t@data[indx] <- lapply(t@data[indx], function(x) as.numeric(as.character(x)))
  rownames(t@data) <- t@data$TAZ_NO

  # set polygon ids to match TAZ Nos to avoid errors later in the process
  for (i in 1:nrow(t@data)){
    #' set polygon IDs to match TAZ numbers
    t@polygons[[i]]@ID <- as.character(t@data$TAZ_NO[i])
    
  }
  # get dataframe and sort the TAZs in ascending order of Activity
  tdf <- t@data %>% arrange(., -c(TotAct))
  
  ##########################################################################
  ########### Outer Step 0: START TAZ in FIRST CSD #########################
  
  #' to ensure that the TAZs only belong to the GGH
  tdf <- subset(tdf, TAZ_NO < 36000000)
  #' get first TAZ in CSD master list or reduced list
  sub_lst <- as.data.frame(tdf$TAZ_NO[1])
  colnames(sub_lst) <- "TAZ_NO"
  #' get starting TAZ and add in column for Cumulative Acitvity
  start <- t[t@data$TAZ_NO %in% sub_lst$TAZ_NO[1], ] 
  start@data$CumAct <- 0
  start@data$Start <- 0
  start@data$Touch <- "1"
  start <- start[, c("TAZ_NO", "Start", "Touch", "TotAct", "CumAct")]
  
  ######## **Step 1** #########
  s1 <- step1(t, tdf, start)
  rel <- s1[[1]]
  t1 <- s1[[2]]
  
  ######## **Step 2** #########
  s2 <- step2(t, rel)
  #' get outputs
  rel_df <- s2[[1]]
  ftaz <- s2[[2]]
  
  ######## **Step 3** #########
  if(nrow(rel_df) != 0){
    s3 <- step3(start, rel_df, ftaz, 1000000)
    diss1 <- s3[[2]]
    round3_spdf <- s3[[1]]
    spdf1 <- s3[[3]]
  } else {
    round3_spdf <- start
    round3_spdf@data$CumAct <- round3_spdf@data$CumAct 
    spdf1 <- round3_spdf@data[1,] %>% 
      transform(., TAZ_NO = TAZ_NO + 1000000) %>% 
      transform(., Start = TAZ_NO)
    # merge the DA with the provincial zone system as this
    # DA cannot find an adjacent DA
    round1_spdf <- spRbind(round1_spdf, round3_spdf)

  }

  s <- round3_spdf@data
   ######## **Step 4** #########
  #' Reduce the TAZ dataframe for the CSD in question, but add in the data for
  #' the dissolved polygon that will allow the Step 2 function to perform an 
  #' inner join between the tdf and rel_df Dataframes. 
  if(nrow(rel_df) != 0){
    tdf <- subset(tdf, !(TAZ_NO %in% rel_df$ID) & !(TAZ_NO %in% rel_df$Start))
  } else {
    tdf <- subset(tdf, !(DAUID %in% s$TAZ_NO))
  }

  if(nrow(tdf) > 0){
    newrow <- c(spdf1$TAZ_NO[1], 0, 0, 1, spdf1$TAZ_NO[1], 0, spdf1$CumAct[1])
    tdf <- rbind(tdf, newrow)
  } 
  
  #' select zones that are now only in the revised tdf Dataframe
  t <- t[t@data$TAZ_NO %in% tdf$TAZ_NO,]
  #plot(round3_spdf)
  
  
  ######## **Step 5**
  #' evaluate if the polygon created in Step 3 has passed the threshold and if 
  #' the master TAZ list of the CSD has any shapes left.
  #' if it has not, then loop back to Step 1. Otherwise save it as a zone
  #' and move to the next TAZ within the CSD. 

  while (round3_spdf@data$CumAct < ap_cnt && nrow(tdf) > 0 | nrow(t@data) != 0){
    
    if(nrow(rel_df) == 0){
      # the round3 zone is the same as the starting zone
      # which will happen if the starting taz does not touch another DA in the CSD
      # and the rel_df will be null as the activity DAs create holes in the system 
      # then set the starting TAZ to the next available DA in the CSD. 
      new_t <- t[t@data$TAZ_NO != start@data$TAZ_NO, ]
      round3_spdf <- new_t[1,]
      round3_spdf@data$Start <- 0
      round3_spdf@data$Touch <- "F"
      round3_spdf@data$CumAct <- 0
      
      round3_spdf <- round3_spdf[, c("TAZ_NO", "Start", "Touch", "TotAct", "CumAct")]
    } else {
      round3_spdf <- round3_spdf
    }

    # if the round3 DA and the final remaining DA in the master are the same then 
    # just rbind it to the provincial zone system
    if(round3_spdf@data$TAZ_NO[1] != t@data$TAZ_NO[1]){
      
      start <- round3_spdf
      #plot(start, col = "blue")
      ######## **Step 1** #########
      s1 <- step1(t, tdf, start)
      rel <- s1[[1]]
      t1 <- s1[[2]]

      ######## **Step 2** #########
      s2 <- step2(t, rel)
      #' get outputs
      rel_df <- s2[[1]]
      ftaz <- s2[[2]]
      f <- ftaz@data

      ######## **Step 3** #########
      
      if(nrow(rel_df) > 0){
        s3 <- step3(start, rel_df, ftaz, 1000000)
        diss1 <- s3[[2]]
        round3_spdf <- s3[[1]]
        spdf2 <- s3[[3]]
      } else {
        round3_spdf <- start
        round3_spdf@data$CumAct <- round3_spdf@data$CumAct 
        spdf2 <- round3_spdf@data[1,] %>% 
          transform(., TAZ_NO = TAZ_NO + 1000000) %>% 
          transform(., Start = TAZ_NO)
        # merge the DA with the provincial zone system as this
        # DA cannot find an adjacent DA
        round1_spdf <- spRbind(round1_spdf, round3_spdf)
      }

      d <- diss1@data
      s <- round3_spdf@data
      #plot(round3_spdf)
      ######## **Step 4** #########
      #' Reduce the TAZ dataframe for the CSD in question
       if(nrow(rel_df) != 0){
        tdf <- subset(tdf, !(TAZ_NO %in% rel_df$ID) & !(TAZ_NO %in% rel_df$Start))
      } else {
        tdf <- subset(tdf, !(DAUID %in% s$TAZ_NO))
      }
      
      #plot(round3_spdf, col = "green")
      if(nrow(tdf) > 0){
        
        newrow <- c(spdf2$TAZ_NO[1], 0, 0, 1, spdf2$TAZ_NO[1], 0, spdf2$CumAct[1])
        tdf <- rbind(tdf, newrow)
      }
      #' select zones that are now only in the revised tdf Dataframe
      t <- t[t@data$TAZ_NO %in% tdf$TAZ_NO,]
      
    } else {
      # remove the final DA from the taz file
      t <- t[!(t@data$TAZ_NO %in% round3_spdf@data$TAZ_NO), ]
    } 
  }  
  #' bind the provincial zones
  round1_spdf <- spRbind(round1_spdf, round3_spdf)

  ###############################################################################
  #' Remaining zones now in the CSD
  
  #' test to see if any valid TAZs are left in the tdf dataframe
  tdf <- subset(tdf, TAZ_NO < 36000000)
  #' if only one record is left after Round 1 then that becomes the a TRESO zone
  #' and just append it to the Round1 TRESO Zones
  if(nrow(tdf) == 1) {
    lastTAZ <- t[t@data$TAZ_NO %in% tdf$TAZ_NO[1], ]
    lastTAZ@data$CumAct <- 0
    lastTAZ@data$Start <- 0
    lastTAZ@data$Touch <- "1"
    lastTAZ <- lastTAZ[, c("TAZ_NO", "Start", "Touch", "TotAct", "CumAct")]
    
    # now bind to the round1 Polygon
    round1_spdf <- spRbind(round1_spdf, lastTAZ)
  }
  
  #' If the above test is unsuccessful for the last TAZ the continue below.    
  while (nrow(tdf) > 1){
    
    tdf <- subset(tdf, TAZ_NO < 36000000)
    
    #' get first TAZ in CSD master list or reduced list
    sub_lst <- as.data.frame(tdf$TAZ_NO[1])
    colnames(sub_lst) <- "TAZ_NO"
    #' get starting TAZ and add in column for Cumulative Acitvity
    start <- t[t@data$TAZ_NO %in% sub_lst$TAZ_NO[1], ] 
    start@data$CumAct <- 0
    start@data$Start <- 0
    start@data$Touch <- "1"
    start <- start[, c("TAZ_NO", "Start", "Touch", "TotAct", "CumAct")]
    
    ######## **Step 1** #########
    s1 <- step1(t, tdf, start)
    rel <- s1[[1]]
    t1 <- s1[[2]]
    
    ######## **Step 2** #########
    s2 <- step2(t, rel)
    #' get outputs
    rel_df <- s2[[1]]
    ftaz <- s2[[2]]
    
    ######## **Step 3** #########
    if(nrow(rel_df) > 0){
      s3 <- step3(start, rel_df, ftaz, 1000000)
      diss1 <- s3[[2]]
      round4_spdf <- s3[[1]]
      spdf3 <- s3[[3]]
    } else {
      round4_spdf <- start
      round4_spdf@data$CumAct <- round4_spdf@data$TotAct 
      spdf3 <- round4_spdf@data[1,] %>% transform(., CumAct = TotAct) %>% 
        transform(., TAZ_NO = TAZ_NO + 1000000) %>% 
        transform(., Start = TAZ_NO)
    }
    
    s <- round4_spdf@data

    ######## **Step 4** #########
    #' Reduce the TAZ dataframe for the CSD in question, but add in the data for
    #' the dissolved polygon that will allow the Step 2 function to perform an 
    #' inner join between the tdf and rel_df Dataframes.
    if (nrow(rel_df) > 0){
      tdf <- subset(tdf, !(TAZ_NO %in% rel_df$ID) & !(TAZ_NO %in% rel_df$Start))
    } else {
      tdf <- subset(tdf, !(TAZ_NO %in% start@data$TAZ_NO))
    }
    #' create and add in the newrow
    if(nrow(tdf) > 0){
      newrow <- c(spdf3$TAZ_NO[1], 0, 0, 1, spdf3$TAZ_NO[1], 0, spdf3$CumAct[1])
      tdf <- rbind(tdf, newrow)
      #' select zones that are now only in the revised tdf Dataframe
      t <- t[t@data$TAZ_NO %in% tdf$TAZ_NO,]
    }

    
    ######## **Step 5**
    #' evaluate if the polygon created in Step 3 has passed the threshold.
    #' if it has not, then loop back to Step 1. Otherwise save it as a zone
    #' and move to the next TAZ within the CSD. Also one needs to check if there
    #' are enough records or zones left for merging, as well as if the rel_df dataframe
    #' was null then it means that there are no touching TAZs and no more zones can
    #' be merged. 
    
    while (round4_spdf@data$CumAct < ap_cnt && nrow(tdf) > 0 && nrow(rel_df) > 0){
      
      #' get starting TAZ
      start <- round4_spdf
      
      #' get the areas of the two polygons. If they are the same, then it means that
      #' the cumulative activity after mergign TAZs has not met the threhold, and there
      #' is a need to exit the loop otherwise it will end up being an infinite loop. The
      #' way to do that is to check if the start and roun2_spdf have the same area, and if
      #' so then break the while loop. Also, if only one row is left in the tdf dataframe
      #' then the loop needs to break.
      s_area <- sapply(slot(start, "polygons"), slot, "area")
      round4_area <- sapply(slot(start, "polygons"), slot, "area")
      
      if(s_area != round4_area){
        ######## **Step 1** #########
        s1 <- step1(t, tdf, start)
        rel <- s1[[1]]
        t1 <- s1[[2]]
        
        ######## **Step 2** #########
        s2 <- step2(t, rel)
        #' get outputs
        rel_df <- s2[[1]]
        ftaz <- s2[[2]]
        
        ######## **Step 3** #########
        if(nrow(rel_df) > 0){
          s3 <- step3(start, rel_df, ftaz, 1000000)
          diss1 <- s3[[2]]
          round4_spdf <- s3[[1]]
          spdf4 <- s3[[3]]
        } else {
          round4_spdf <- start
          round4_spdf@data$CumAct <- round4_spdf@data$TotAct 
          spdf4 <- round4_spdf@data[1,] %>% transform(., CumAct = TotAct) %>% 
            transform(., TAZ_NO = TAZ_NO + 1000000) %>% 
            transform(., Start = TAZ_NO)
        }
        
        d <- diss1@data
        s <- round4_spdf@data
        ######## **Step 4** #########
        #' Reduce the TAZ dataframe for the CSD in question
        if (nrow(rel_df) > 0){
          tdf <- subset(tdf, !(TAZ_NO %in% rel_df$ID) & !(TAZ_NO %in% rel_df$Start))
        } else {
          tdf <- subset(tdf, !(TAZ_NO %in% start@data$TAZ_NO))
        }
        #' create and add in the newrow
        newrow <- c(spdf4$TAZ_NO[1], 0, 0, 1, spdf4$TAZ_NO[1], 0, spdf4$CumAct[1])
        tdf <- rbind(tdf, newrow)
        #' select zones that are now only in the revised tdf Dataframe
        t <- t[t@data$TAZ_NO %in% tdf$TAZ_NO,]
        
      } #' break the loop
      if (s_area == round4_area | nrow(tdf) == 1) {
        
        break
      }
      
    }
    
    #' bind the provincial zones
    round1_spdf <- spRbind(round1_spdf, round4_spdf)
    
    #' close the outer while loop  
  }
  #' close the for loop
}
plot(round1_spdf)
#' Save the zones to those that represent the GGH Zones
Case4Zones <- round1_spdf
writeOGR(Case4Zones, layer = paste0("TRESO4_", ap_cnt), wd, 
         driver="ESRI Shapefile", overwrite_layer = T)


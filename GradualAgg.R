library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(maptools)
library(raster)
library(reshape2)  
library(SpatialTools)

library(gaussfacts)
library(rmsfact)

gaussfact()
rmsfact()

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
du_cnt <- 500 # Dwelling unit threshold outside the GGH
du_ggh <- 3500 # Dwelling unit threshold within the GGH
truck <- 5000 # Truck stop threshold across the province

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
da_cen <- da_cen[, c("CSDUID", "DAUID", "GGH", "dwell")]


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

# get the total DUs in a CSD as well as the average number in each DA. 
du_avg <- da_poly@data 
du_avg1 <- du_avg %>% group_by(CSDUID) %>% 
  summarise(units = sum(dwell), CntDA = n(), GGH = min(GGH)) %>% 
  transform(., AvgDU = as.integer(units/CntDA )) %>% 
  transform(., AvgDA = as.integer(units/du_cnt))

  # set NAs to zero with user defined function (f_rep)
  du_avg1 <- f_rep(du_avg1)

# create copy of the DA Polygons to start process
da1 <- da_poly
# transfer the data from the calculations of average values to shapefile table  
da1@data = data.frame(da1@data,du_avg1[match(da1@data$CSDUID, 
                                                     du_avg1$CSDUID),])

###############################################################################
#' Set Functions for generating the first and second set of TAZs using GA
###############################################################################

#' this function generates a unique table of CSDs that is then fed to the other
#' functions.

fun_un <- function(field, val, sseq){
  " 
  This function generates a list of dissolved TAZs based on the first and
  second cases noted before in script.
  
  Inputs: Dissemination Area shapefile; attribute column; value in 
  column that helps select the DA.

  Arguments:
  First, column in the shapefile table that will be used to subset 
  dataframe.
  Second, value by which to subset dataframe i.e. 0 for first case
  and 1 for second case.
  Third, starting sequence value.

  Returns: 
  First, shapefile subsetted to represent either the first or second case.
  Second, shapefile table.
  Third, table with unique CSDs and a sequence of numbers to dissolve on. 

  "
  
  taz <- da1[da1@data[[field]] == val & da1@data[["GGH"]] == 0,]
  
  # get table from shapefile and start process
  tt <- taz@data
  un <- as.data.frame(unique(tt$CSDUID.1)) %>% 
    transform(., 
              UnNum = seq(sseq,length.out = nrow(as.data.frame(unique(tt$CSDUID)))))
  # reset column names
  colnames(un) <- c("CSDUID", "DisNum") 
  
  # return 
  list(taz, tt, un)
} 


#' this function takes the unique CSD dataframe from the above function and 
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
  tt3 <- merge(tt, un, by.x = "CSDUID.1", by.y = "CSDUID", all.x = TRUE) %>% 
    subset(., select = c("CSDUID", "DisNum", "units"))
  
  # merge the unique values for dissolving back to the shapefile table
  taz@data = data.frame(taz@data,tt3[match(taz@data$CSDUID.1, 
                                                   tt3$CSDUID),])
  
  # now dissolve the DAs on the field "DisNum"
  taz2 <- gUnaryUnion(taz, id = taz@data$DisNum)
  
  # prepare table to attach to dissolved polygons to make into spatial dataframe
  spdf <- tt3 %>% group_by(DisNum) %>% summarise(DU = sum(units))
    spdf <- as.data.frame(spdf)
  
  rownames(spdf) <- spdf$DisNum
  
  # make shapefile of the FIRST CASE
  taz2_spdf <- SpatialPolygonsDataFrame(taz2, spdf)
  
  return(taz2_spdf)
}

###############################################################################
#' FIRST CASE OF PROVINCIAL ZONES
###############################################################################

# field in the shapefile to be used. In this case "units".
# select all those DA's that belong to a CSD that has 0 Dwelling Units in it
# starting sequence for numbering the DAs to dissolve on is set to 1

a1 <- "units"
a2 <- 0
a3 <- 1

# this will get you a list of outputs
taz3 <- fun_un(a1, a2, a3)

# unpack the list in to variables that will be used in the next function
taz <- taz3[[1]]
tt <- taz3[[2]]
un <- taz3[[3]]

# call second function that dissolves the DA shapefile and creates the shapefile
# for the FIRST CASE of the GRADUAL AGGREGATION process.

first_taz <- fun_shp(tt, un, taz)

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
taz3 <- fun_un(a1, a2, a3)

# unpack the list in to variables that will be used in the next function
taz <- taz3[[1]]
tt <- taz3[[2]]
un <- taz3[[3]]

# call second function that dissolves the DA shapefile and creates the shapefile
# for the FIRST CASE of the GRADUAL AGGREGATION process.
sec_taz <- fun_shp(tt, un, taz)

###############################################################################
#' THIRD CASE OF PROVINCIAL ZONES
###############################################################################
#' select those records where there are more than 1 DA within a CSD and dwelling
#' units are lesser than DU threshold and it does not belong in the GGH area

# starting sequence set to one number above the final value of the FIRST and 
# SECOND CASES COMBINED.
a10 <- nrow(sec_taz@data) + nrow(first_taz@data) + 1

#' Prepare the inputs for the fun_shp function that will dissolve the DAs to 
#' make the THIRD CASE of provincial zones.

# get the CSDs that belong to the THIRD CASE of provincial zones
csd_below <- subset(du_avg1, units > 0 & units < du_cnt & CntDA != 1 & GGH == 0)
# Create the third set of TAZs
taz_third <- da1[da1@data$CSDUID %in% csd_below$CSDUID,]

# get dataframe of the shapefile
tt_df <- taz_third@data
# generate unique values to dissolve
un_df <- as.data.frame(unique(tt_df$CSDUID.1)) %>% 
  transform(., 
            UnNum = seq(a10,length.out = 
                          nrow(as.data.frame(unique(tt_df$CSDUID)))))
  # reset column names
  colnames(un_df) <- c("CSDUID", "DisNum")

# Generate the THIRD CASE of Provincial zones using the fun-shp function
third_taz <- fun_shp(tt_df, un_df, taz_third)

###############################################################################
#' FOURTH CASE OF PROVINCIAL ZONES
###############################################################################
# select those records where there are more than 1 DA within a CSD and dwelling
# units are greater than DU threshold and it does not belong in the GGH area

# starting sequence set to one number above the final value of the FIRST,  
# SECOND, and THIRD cases combined.
a20 <- nrow(third_taz@data) + nrow(sec_taz@data) + nrow(first_taz@data) + 1

# get the CSDs that belong to the FOURTH CASE of provincial zones
csd_ab <- subset(du_avg1, units > 0 & units >= du_cnt & CntDA != 1 & GGH == 0)

# Create the DAs that lie within the CSDs that are being evaluated
taz_fourth <- da1[da1@data$CSDUID %in% csd_ab$CSDUID,]

# get table from subsetted shapefile and get unique CSD's while sequentially
# numbering them for dissolving later on.
un20 <- as.data.frame(taz_fourth@data) %>% group_by(CSDUID) %>% 
  summarise(DU = sum(dwell)) %>% 
  transform(., DisNum = seq(a20, length.out = nrow(.))) %>%
  subset(., select = c(CSDUID, DisNum, DU))

# get the dataframe of the taz_fourth shapefile
tf <- taz_fourth@data %>% subset(., select = c(DAUID, CSDUID, dwell)) 

#' Given that multiple CSDs and their DAs fall in the Fourth Case, we are using
#' a function 

# get unique CSDs for the DAs that are above the thresholds and convert from 
# factors to numerics
lst <- as.data.frame(as.numeric(as.character(unique(un20$CSDUID))))
colnames(lst) <- "CSD"
lst <- arrange(lst, CSD)


#####################
# This section of the code will be put in a for loop
####

# Function to get all the DAs in a CSD and calculate their Euclidean distance
euc <- function(i, shp){
 
  " 
  This function gets the list of DAs within a CSD and calculates the euclidean
  distance between the DA centroids
  
  Inputs: Dataframe of Unique CSDs; DA centroids file

  Arguments: i, which is the iterator in the FOR loop; shapefile; 

  Returns: 
  First, dataframe of DA Centroids.
  Second, euclidean distance dataframe of DAs. 

  "
  
  # get All DAs for first CSD
  da_cen_1 <- shp[shp@data$CSDUID == lst$CSD[i], ]
  df <- da_cen_1@data %>% subset(., select = c("DAUID", "CSDUID", "GGH", "dwell"))
  
  # convert factors to numeric
  indx <- sapply(df, is.factor)
    df[indx] <- lapply(df[indx], function(x) as.numeric(as.character(x)))
    # sort by DAUID
    df1 <- arrange(df, dwell)
  
  #' get euclidean distance of all DAs selected in a CSD
  # get matrix of coordinates and convert to dataframe
  cds <- data.matrix(da_cen_1@coords)
  euc <- as.data.frame(dist1(cds))
    rownames(euc) <- df$DAUID
    colnames(euc) <- df$DAUID 
  
  #' create column for melting and rename variables
  euc$ID <- rownames(euc) # create column for melting
  cc <- melt(euc, id.vars = "ID") %>% subset(., value > 0)  %>% arrange(., ID, value)
    colnames(cc) <- c("Orig", "Dest", "Dist") 
  
  # get rid of character and factor columns
  indx <- sapply(cc, is.factor)
  indx1 <- sapply(cc, is.character)
    cc[indx] <- lapply(cc[indx], function(x) as.numeric(as.character(x)))
    cc[indx1] <- lapply(cc[indx1], function(x) as.numeric(x)) 
  
  list(df1, cc)
  
}


# Function to get a single DA and then process it till it meets the threshold identified for
# Dwelling Units
zone <- function(df1, cc, tf, du_threshold, taz_fourth){
  
  " 
  This function gets a single DA from the list of DAs within a single CSD and starts the process
  of aggregating DAs surrounding it till one reaches the dwelling unit threshold
  
  Inputs: CSD Dataframe from the Euc function; melted euclidean matrix of DAs within the CSD;
          dataframe (tf) of the shapefile of the case of the provincial zones being used; dwelling
          unit threshold to be used.
  
  Arguments: CSD Dataframe; melted euclidean matrix; dataframe of shapefile of provincial zones that
             belong the case of Provincial Model zones; dwelling unit threshold; shapefile that contains
             the TAZs being evaluated; 
  
  Returns: 
  First, dataframe of DAs that should be dissolved.
  Dataframe of DAs that are dropped from the master DA list for each CSD
  Trimmed down euclidean matrix (cc) that does not contain the DAs that have been dissolved. 
  Shapefile of dissolved DAs that represents a Provincial TAZ
  
  "
 
  # Now get the first DA within the CSD 
  start <- df1[1, ]
  
  # subset the distance matrix for the first DA obtained from start and 
  # add a row at the end of the DA being analysed
  cc_f <- subset(cc, Orig == start$DAUID[1]) %>% 
    rbind(., c(start$DAUID, start$DAUID, 0)) %>% 
    arrange(., Dist)
  
  # merge the subseted distance matrix to the DA dataframe (tf) and calculate
  # the cumulative dwellings for the DAs in ascending distance.
  tf_1 <- merge(cc_f, tf, by.x = "Dest", by.y = "DAUID", all.x = TRUE) %>% 
    arrange(., Dist) %>% 
    transform(., CumDU = cumsum(dwell))
  
  # get the dataframe that corressponds to the user defined threshold and
  # populate it with the DAUID number that will be used for dissolving
  tf_1_dis <- subset(tf_1, CumDU <= du_threshold) 
  
  if (nrow(tf_1_dis) > 0){
    tf_1_dis <- transform(tf_1_dis, DisNum = start$DAUID) %>% 
      rename(., DAUID = Dest)
  } else {
    tf_1_dis <- tf_1[1,] %>% transform(., DisNum = start$DAUID) %>% 
      rename(., DAUID = Dest)
  }
  
    #' get DAs that corresspond to the dissolving dataframe and add in dissolve field
    da_sh <- taz_fourth[taz_fourth@data$DAUID %in% tf_1_dis$DAUID, ]
    da_sh@data$DisNum <- start$DAUID
    # now dissolve the DAs on the field "DisNum"
    taz4 <- gUnaryUnion(da_sh, id = da_sh@data$DisNum)
    
  # prepare table to attach to dissolved polygons to make into spatial dataframe
  spdf <- tf_1_dis %>% group_by(DisNum) %>% summarise(DU = sum(dwell))
    spdf <- as.data.frame(spdf)
    rownames(spdf) <- spdf$DisNum
  
  # make shapefile of the the DAs 
  taz4_spdf <- SpatialPolygonsDataFrame(taz4, spdf)
  
  # Now get the list of total DAs that are left over after the above dissolving
  # This is a recursive list, as DAs that get dissolved in the step above will
  # need to dissapear from the master list of DAs to avoid overlap
  df_leftover <- anti_join(df1, tf_1_dis, by = "DAUID") %>% 
    arrange(., dwell)
  
  # make a temp copy of the DAs that need to be dropped and get rid of them from
  # the euclidean dataframe
  df_drop <- subset(tf_1_dis, select = c(DAUID, Orig)) %>% 
    rename(., Dest = DAUID)
    df_drop_c <- df_drop %>% rename(., Orig = Dest, Dest = Orig)
    df_drop <- rbind(df_drop, df_drop_c)
  cc_left <- anti_join(cc, df_drop, by = "Dest") %>% 
    anti_join(., df_drop, by = "Orig") 
  
  # Return
  list(tf_1_dis, df_leftover, cc_left, taz4_spdf, da_sh)
  
}

########
# Start the FOR loop with the first CSD. This is required for binding the shapefiles to
# each other for one consolidated set of zones under the FOURTH CASE.
for (i in 1:1){
  
  # get first Dissovled zone for first CSD
  input_lst <- euc(1, da_cen)
  # Unlist the list of returns from the EUC function to get ready to feed into the next function
  df1 <- input_lst[[1]]
  cc <- input_lst[[2]]
 
  # Start with the first DA
  f_da <- zone(df1, cc, tf, du_cnt, taz_fourth)
  # get each element of the output from the zone list
  taz4_prov <- f_da[[4]]
  df_leftover <- f_da[[2]]
  cc_f <- f_da[[3]]

  while (nrow(df_leftover)> 1){
    
      # Start with the second DA that is leftover
      s_da <- zone(df_leftover, cc_f, tf, du_cnt, taz_fourth)
    
      taz4_prov1 <- s_da[[4]]
      df_leftover <- s_da[[2]]
      cc_f <- s_da[[3]]
      
      # assimilate the zones
      taz4_prov <- spRbind(taz4_prov, taz4_prov1)
    
      }
# if only one DA is left then just make it into a zone
if (nrow(df_leftover) == 1) {
  
  taz4_single <- da_poly[da_poly@data$DAUID == df_leftover$DAUID[1], ]
    taz4_single <- taz4_single[, c("DAUID", "dwell")]
    taz4_single@data <- rename(taz4_single@data, DisNum = DAUID, DU = dwell)
      rownames(taz4_single@data) <- taz4_single@data$DisNum
  
      # reset polygon ID of DA
      taz4_single@polygons[[1]]@ID <- as.character(df_leftover$DAUID[1])
  # tag the single DA to the TAZ layer
  taz4_prov <- spRbind(taz4_prov, taz4_single)
  }
}


#######
# Now run the dissolving procedures for the leftover DAs
for (i in 2:nrow(lst)){
  
  # get first Dissovled zone for first CSD
  input_lst <- euc(i, da_cen)
  # Unlist the list of returns from the EUC function to get ready to feed into the next function
  df1 <- input_lst[[1]]
  cc <- input_lst[[2]]
  
  # Start with the first DA
  f_da <- zone(df1, cc, tf, du_cnt, taz_fourth)
  # get each element of the output from the zone list
  taz4_prov1 <- f_da[[4]]
  df_leftover <- f_da[[2]]
  cc_f <- f_da[[3]]
  
  while (nrow(df_leftover)> 1){
    
    # Start with the second DA that is leftover
    s_da <- zone(df_leftover, cc_f, tf, du_cnt, taz_fourth)
    
    taz4_prov2 <- s_da[[4]]
    df_leftover <- s_da[[2]]
    cc_f <- s_da[[3]]
    
    # assimilate the zones
    taz4_prov1 <- spRbind(taz4_prov1, taz4_prov2)
    
  }
  
  taz4_prov <- spRbind(taz4_prov, taz4_prov1)

# if only one DA is left then just make it into a zone
if (nrow(df_leftover) == 1) {
  
  taz4_single1 <- da_poly[da_poly@data$DAUID == df_leftover$DAUID[1], ]
    taz4_single1 <- taz4_single1[, c("DAUID", "dwell")]
      taz4_single1@data <- rename(taz4_single1@data, DisNum = DAUID, DU = dwell)
        rownames(taz4_single1@data) <- taz4_single1@data$DisNum
        
    # reset polygon ID of DA
    taz4_single1@polygons[[1]]@ID <- as.character(df_leftover$DAUID[1])
  
  # tag the single DA to the TAZ layer
  taz4_prov <- spRbind(taz4_prov, taz4_single1)
  
  }
}

#' With the processing completed, do a check if the procedure above has created 
#' multipart polygons. If it has, then it signifies that the euclidean calculation
#' is creating the DAs to select non-adjacent DAs and it needs to be looked into.
pols <- lapply(taz4_prov@polygons, slot, "Polygons")

# Stop process if the number of polys in the TAZ4_poly shapefile is more than the
# number of rows in the TAZ4_poly dataframe
{
  if (length(pols) != nrow(taz4_prov@data)) 
    stop("The procedure has resulted in multipart polygons. 
           Please check the algorithm before proceeding.")
}

###############################################################################
# FIFTH Case of the Provincial Zones
###############################################################################
#' Now handle the TAZs inside the GGH area.
a30 <- a20 + 1
# get the CSDs that belong to the FIFTH CASE of provincial zones
csd_ggh <- subset(du_avg1, GGH == 1)

# get the dataframe of the GGH TAZs shapefile
tf <- ggh@data  

# get all the TAZs that belong to a CSD that in aggregate has fewer hholds than the
# dwelling unit threshold defined for the GGH area. These TAZs will be fused together
# to make on provincial zone.
csd_ggh_b <- subset(csd_ggh, units <= du_ggh)

# get list of TAZs that belong to CSDs that are below the dwelling unit threshold
fifth_taz <- ggh[ggh@data$CSDUID %in% csd_ggh_b$CSDUID,]
  ft <- fifth_taz@data

# Dissolve the TAZs
taz5 <- gUnaryUnion(fifth_taz, id = fifth_taz@data$CSDUID)
  
# prepare table to attach to dissolved polygons to make into spatial dataframe
spdf <- ft %>% group_by(CSDUID) %>% summarise(DU = sum(dwell))
  spdf <- as.data.frame(spdf)
  rownames(spdf) <- spdf$CSDUID

########
# make shapefile of the the TAZs 
taz5_spdf <- SpatialPolygonsDataFrame(taz5, spdf) 
  colnames(taz5_spdf@data) <- c("DisNum", "DU")
#######
  
# get the remaining CSDs and GGH TAZs
csd_ggh_a <- subset(csd_ggh, units > du_ggh)
taz_fifth <- ggh[ggh@data$CSDUID %in% csd_ggh_a$CSDUID, ]

# get unique CSDs for the DAs that are above the thresholds and convert from 
# factors to numerics
lst <- as.data.frame(as.numeric(as.character(csd_ggh_a$CSDUID)))
  colnames(lst) <- "CSD"
  lst <- arrange(lst, CSD)

########
# Start the FOR loop with the first CSD. This is required for binding the shapefiles to
# each other for one consolidated set of zones under the FIFTH CASE.

  for (i in 1:1){
    
    # get first Dissovled zone for first CSD
    input_lst <- euc(i, ggh_cen)
    # Unlist the list of returns from the EUC function to get ready to feed into the next function
    df1 <- input_lst[[1]]
    cc <- input_lst[[2]]
    
    # Start with the first DA
    f_da <- zone(df1, cc, tf, du_ggh, taz_fifth)
    # get each element of the output from the zone list
    taz5_prov <- f_da[[4]]
    df_leftover <- f_da[[2]]
    cc_f <- f_da[[3]]
    
    while (nrow(df_leftover)> 1){
      
      # Start with the second DA that is leftover
      s_da <- zone(df_leftover, cc_f, tf, du_ggh, taz_fifth)
      
      taz5_prov1 <- s_da[[4]]
      df_leftover <- s_da[[2]]
      cc_f <- s_da[[3]]
      
      # assimilate the zones
      taz5_prov <- spRbind(taz5_prov, taz5_prov1)
      
    }
    
    # if only one TAZ is left then just make it into a zone
    if (nrow(df_leftover) == 1) {
      
      taz5_single <- ggh[ggh@data$DAUID == df_leftover$DAUID[1], ]
      taz5_single <- taz5_single[, c("DAUID", "dwell")]
      taz5_single@data <- rename(taz5_single@data, DisNum = DAUID, DU = dwell)
        rownames(taz5_single@data) <- taz5_single@data$DisNum
      
      # reset polygon ID of DA
      taz5_single@polygons[[1]]@ID <- as.character(df_leftover$DAUID[1])
      # tag the single DA to the TAZ layer
      taz5_prov <- spRbind(taz5_prov, taz5_single)
    }
  }
  
#######
# Now run the dissolving procedures for the leftover TAZs 

  for (i in 2:nrow(lst)){
    
    # get first Dissovled zone for first CSD
    input_lst <- euc(i, ggh_cen)
    # Unlist the list of returns from the EUC function to get ready to feed into the next function
    df1 <- input_lst[[1]]
    cc <- input_lst[[2]]
    
    # Start with the first DA
    f_da <- zone(df1, cc, tf, du_ggh, taz_fifth)
    # get each element of the output from the zone list
    taz5_prov1 <- f_da[[4]]
    df_leftover <- f_da[[2]]
    cc_f <- f_da[[3]]
    
    while (nrow(df_leftover)> 1){
      
      # Start with the second DA that is leftover
      s_da <- zone(df_leftover, cc_f, tf, du_ggh, taz_fifth)
      
      taz5_prov2 <- s_da[[4]]
      df_leftover <- s_da[[2]]
      cc_f <- s_da[[3]]
      
      # assimilate the zones
      taz5_prov1 <- spRbind(taz5_prov1, taz5_prov2)
      
    }
    
    taz5_prov <- spRbind(taz5_prov, taz5_prov1)
    
    # if only one DA is left then just make it into a zone
    if (nrow(df_leftover) == 1) {
      
      taz5_single1 <- ggh[ggh@data$DAUID == df_leftover$DAUID[1], ]
      taz5_single1 <- taz5_single1[, c("DAUID", "dwell")]
      taz5_single1@data <- rename(taz5_single1@data, DisNum = DAUID, DU = dwell)
      rownames(taz5_single1@data) <- taz5_single1@data$DisNum
      
      # reset polygon ID of DA
      taz5_single1@polygons[[1]]@ID <- as.character(df_leftover$DAUID[1])
      # tag the single DA to the TAZ layer
      taz5_prov <- spRbind(taz5_prov, taz5_single1)
    }
  } 

#####
#' Merge the FIFTH CASE of TAZs together

taz5_prov <- spRbind(taz5_prov, taz5_spdf)
    
#' With the processing completed, do a check if the procedure above has created 
#' multipart polygons. If it has, then it signifies that the euclidean calculation
#' is creating the DAs to select non-adjacent DAs and it needs to be looked into.
pols <- lapply(taz5_prov@polygons, slot, "Polygons")
  
# Stop process if the number of polys in the TAZ4_poly shapefile is more than the
# number of rows in the TAZ4_poly dataframe
{
  if (length(pols) != nrow(taz5_prov@data)) 
    stop("The procedure has resulted in multipart polygons. 
          Please check the algorithm before proceeding.")
}  

###############################################################################
#' Combine all the different cases of TAZS
###############################################################################
all_taz <- spRbind(first_taz, sec_taz)
all_taz <- spRbind(all_taz, third_taz)
all_taz <- spRbind(all_taz, taz4_prov)
all_taz <- spRbind(all_taz, taz5_prov)

###############################################################################
#' Conduct an area test on the zone system
###############################################################################

# create function for estimating the propensity of each TAZ to be nicely square
square <- function(i){
  
  " 
  This function gets a single zone from the master list of TAZs and computes
  the proportion of its area to its bounding box. The TAZs are all rotated by
  -30 degrees for accounting for the fact that the Province is tilted
  
  Inputs: The merged TAZs for the provincial model
  
  Arguments: iterator; 
  
  Returns: 
  First, dataframe of TAZs with its proportions.

  "
  
  # get the first TAZ and calculate area
  b <- all_taz[all_taz@data$DisNum[i], ]
  b@data$Area <- sapply(slot(b, "polygons"), slot, "area")
  b_df <- b@data
  # Rotate it by a fixed value. This is currently estimated at -30. Some DAs will
  # surely do better under different angle assumptions, but this angle was calculated
  # on the global scale for Ontario
  b_r <- elide(b, rotate = -30)
  # get bounding coordinates of rotate TAZ. This will be used to generate the 
  # bounding Polygon
  b_coords <- matrix(bbox(b_r))
  
  # Now create a polygon using the bounding box coordinates.
  e <- as(raster::extent(b_coords[1], b_coords[3], b_coords[2], b_coords[4]), "SpatialPolygons")
  proj4string(e) <- crs_da_poly
  # set polygon id
  e@polygons[[1]]@ID <- as.character(b_df$DisNum[1])
  
  # create a spatialpolygon dataframe and add in the area
  e_spdf <- SpatialPolygonsDataFrame(e, b_df) 
  e_spdf@data$Area <- sapply(slot(e_spdf, "polygons"), slot, "area")
  
  # get proportion of ZONE area to Bounding Box area. As this proportion approaches 1
  # we are assured of nicer geometrical shapes, for the most part. This is a good check
  # as many thresholds of Dwelling Units and different approaches to starting the DA dissolve
  # process are tested.
  b@data$Prop <- b@data$Area/e_spdf@data$Area
    prop_df <- b@data
  
  return(prop_df)
}

######
# Start the FOR loop for calculating the square proportions
######

# Get the first DA in the All_Tazs shapefile
shapeprop <- square(1)

# Now run for the remaining TAZs and bind the information together
for (i in 2:nrow(all_taz@data)){
  # run the function
  shapeprop1 <- square(i)
  
  # bind it all together
  shapeprop <- rbind(shapeprop, shapeprop1)
  
}

#####
# merge the shapeproportions to the All_TAZ shapefile
all_taz@data = data.frame(all_taz@data, shapeprop[match(all_taz@data$DisNum, 
                                             shapeprop$DisNum),])

#####
# This reports the performance of the above threshold and algorithm in terms of
# getting nice geometrical shapes.
#####

# Report the number of TAZs within a repeating interval of 0.2 E.g. TAZs
# that have a proportion of only 0.2 are the furthest from a square and 
# are oddly shaped. 
c1 <- sum(all_taz@data$Prop <= 0.2)
c2 <- sum(all_taz@data$Prop > 0.2 & all_taz@data$Prop <= 0.4)
c3 <- sum(all_taz@data$Prop > 0.4 & all_taz@data$Prop <= 0.6)
c4 <- sum(all_taz@data$Prop > 0.6 & all_taz@data$Prop <= 0.8)
c5 <- sum(all_taz@data$Prop > 0.8 & all_taz@data$Prop <= 1.1)

print(paste0("The number of TAZs that have a proportion of less than or equal to 0.2 are ", 
             c1))
print(paste0("The number of TAZs that have a proportion of greater than 0.2 and less than 0.4 are ", 
             c2))
print(paste0("The number of TAZs that have a proportion of greater than 0.4 and less than 0.6 are ", 
             c3))
print(paste0("The number of TAZs that have a proportion of greater than 0.6 and less than 0.8 are ", 
             c4))
print(paste0("The number of TAZs that have a proportion of greater than 0.8 and less than 1.0 are ", 
             c5))

###############################################################################
#' EXPORT THE FILES
###############################################################################
# individual cases of the zone system
writeOGR(first_taz, layer = "FirstTAZ_GA", wd, driver="ESRI Shapefile", overwrite_layer = T)
writeOGR(sec_taz, layer = "SecondTAZ_GA", wd, driver="ESRI Shapefile", overwrite_layer = T)
writeOGR(third_taz, layer = "ThirdTAZ_GA", wd, driver="ESRI Shapefile", overwrite_layer = T)
writeOGR(taz4_prov, layer = "Fourth_GA", wd, driver="ESRI Shapefile", overwrite_layer = T)
writeOGR(taz5_prov, layer = "Fourth_GA", wd, driver="ESRI Shapefile", overwrite_layer = T)

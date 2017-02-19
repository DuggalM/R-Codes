library(sp)    # this is the workhorse of the spatial world in R
library(rgdal) # for reading and writing shapefiles effortlessly
library(rgeos) # gRelate and gUnary functions for DE-9IM
library(dplyr)
library(maptools) # needed for SpRbind
library(reshape2)  

#' Set working directory
wd <- setwd("c:/personal/r")

#' get functions
source("GA_Functions.R")

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
ap_cnt <- 5000 # Activity threshold outside the GGH
ap_trk <- 10 # Truck Activity threshold outside the GGH. This threshold is multiplied
# to the scale (540) as the data in the input DA files is for 18 months.
scale_trk <- 540 # Bryce's truck points represent an 18 month period


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
  transform(., TotAct = as.integer(units*4 + Stops/scale_trk))
#' convert factors to numbers
indx <- sapply(du_avg, is.factor)
du_avg[indx] <- lapply(du_avg[indx], function(x) as.numeric(as.character(x)))

# create copy of the DA Polygons to start process and also populate activity points
da1 <- da_poly
da1@data <- transform(da1@data, TotAct = dwell*4 + TruckStops/scale_trk)
# transfer the data from the calculations of average values to shapefile table  
da1@data = data.frame(da1@data,du_avg1[match(da1@data$CSDUID, 
                                             du_avg1$CSDUID),])
# strip unnecessary fields in CSD and DA level shapefiles
da1 <- da1[, c("CSDUID", "DAUID", "GGH", "dwell", "TruckStops", "TotAct", "units", 
               "CntDA", "TotAct.1")]
#' rename column to represent CSD_Activities
da1@data <- rename(da1@data, CSD_TotAct = TotAct.1)

#' also set the activity points in the da_centroids file
da_cen@data <- transform(da_cen@data, TotAct = dwell*4 + TruckStops/scale_trk)


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
multi[1, ] <- 3549005
multi[2, ] <- 3558075
multi[3, ] <- 3552093

#' Run multipart function and save them into a TRESO zones
t_multi1 <- multipart(1)
t_multi2 <- multipart(2)
t_multi3 <- multipart(3)

#' now bind them
taz_fourth_multi <- spRbind(t_multi1, t_multi2)
taz_fourth_multi <- spRbind(taz_fourth_multi, t_multi3)
#' write out the multi-part CSD as a TRESO Zone
writeOGR(taz_fourth_multi, layer = paste0("TRESO4_Multi", ap_cnt), wd, 
         driver="ESRI Shapefile", overwrite_layer = T)

# get the CSDs that belong to the FOURTH CASE of provincial zones. 
csd_ab <- subset(du_avg_left1, !(du_avg_left1$CSDUID %in% multi$TAZ_NO))

# Create the DAs that lie within the CSDs that are being evaluated. But first
# take out those DAs that are above the truck point threshold identified 
# early on in the process by the user. The user defines daily truck activity
# thresholds that are converted to annual estimates by multiplying by 365
taz_da_activity <- da_poly[(da_poly@data$TruckStops > ap_trk*scale_trk) & 
                             (da_poly@data$GGH == 0), ]

#' write out the DAs that exceed the truck activity threshold
writeOGR(taz_da_activity, layer = paste0("TRESO4_Tr_activity", paste0(ap_trk,"_",scale_trk)), wd, 
         driver="ESRI Shapefile", overwrite_layer = T)

#' reduce remaining DAs for generalized processing
taz_fourth <- da1[(da1@data$CSDUID %in% csd_ab$CSDUID) & 
                    !(da1@data$DAUID %in% taz_da_activity@data$DAUID),]

#' Get DAs that are within a CSD with only one DA and save that as a TRESO zone
#' And then also reduce the CSD list as the removed CSDs represent those CSDs that
#' have only one DA left after removing the DA with significant commercial activity
# get a count of DAs within a CSD and only keep the records that have a 
# frequency of 1.

tf_df_sum <- taz_fourth@data %>% 
  group_by(CSDUID) %>% 
  summarise(Freq = n()) %>% 
  subset(., Freq == 1)

if(nrow(tf_df_sum) > 0){
  left_zone <- da_poly[da_poly@data$CSDUID %in% tf_df_sum$CSDUID, ]
  csd_ab <- subset(csd_ab, !(csd_ab$CSDUID %in% tf_df_sum$CSDUID))
  taz_fourth <- taz_fourth[!(taz_fourth@data$CSDUID %in% left_zone@data$CSDUID), ]
  #' write out the DAs given that after removing the truck activity DAs there is
  #' only one DA left in the CSD
  writeOGR(left_zone, layer = paste0("TRESO4_LeftZone"), wd, 
           driver="ESRI Shapefile", overwrite_layer = T)
  
} 
  
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

#############################################################################
###############################################################################
#' create dummy shapefiles for receiving the Case 4 TRESO zones
gen_spdf <- taz_fourth[taz_fourth@data$DAUID == taz_fourth@data$DAUID[1], ]
  gen_spdf@data$Start <- 0
  gen_spdf@data$Spatial <- "Temp"
  gen_spdf@data$Touch <- "1"
  gen_spdf@data$CumAct <- 0
    gen_spdf <- gen_spdf[, c("DAUID", "Start", "Spatial", "TotAct", 
                             "CumAct", "Touch")]

alone_spdf <- taz_fourth[taz_fourth@data$DAUID == taz_fourth@data$DAUID[2], ]
  alone_spdf@data$TAZ_NO <- alone_spdf@data$DAUID
  alone_spdf@data$CumAct <- 0
  alone_spdf@data$GGH <- 100 # set this temp flag so that we can remove this temp zone finally
    alone_spdf <- alone_spdf[, c("TAZ_NO", "CSDUID", "dwell", "GGH", "DAUID", 
                             "TruckStops", "TotAct", "CumAct")]

# For loop across CSDs  

for (i in 1:nrow(lst)){
  print(lst$CSD[i])
  #' create dummy dataframe everytime a new CSD is being run or there is a break 
  #' in the DAs i.e. DAs within a CSD have reached their limit and a new set 
  #' needs to be started, but within a CSD.
  r1 <- data.frame(DAUID = integer(),
                   CumAct = double())
  #' get ALL DAs in first CSD
  t <- taz_fourth[taz_fourth@data$CSDUID %in% lst$CSD[i], ] %>% 
    .[order(-.$TotAct), ]
  # set TAZ_NO
  t@data$TAZ_NO <- t@data$DAUID
  t@data$CumAct <- 0
  # save dataframe
  tdf <- t@data
  
  t <- t[, c("TAZ_NO", "CSDUID", "dwell", "GGH", "DAUID", "TruckStops", "TotAct", "CumAct")]
  indx <- sapply(t@data, is.factor)
  t@data[indx] <- lapply(t@data[indx], function(x) as.numeric(as.character(x)))
  rownames(t@data) <- t@data$TAZ_NO
  
  # set polygon ids to match TAZ Nos to avoid errors later in the process
  for (i in 1:nrow(t@data)){
    #' set polygon IDs to match TAZ numbers
    t@polygons[[i]]@ID <- as.character(t@data$TAZ_NO[i])
    
  }
plot(t, col = "red")
  
  ###############################################################################
  #' Get STARTING DA and run this till there are no DAs left in the CSD
  while(nrow(tdf) > 0) {
    
    #' create dummy dataframe everytime a new CSD is being run or there is a break 
    #' in the DAs i.e. DAs within a CSD have reached their limit and a new set 
    #' needs to be started, but within a CSD.
    r1 <- data.frame(DAUID = integer(),
                     CumAct = double())
    
    round1_spdf <- t[1, ]
    print(round1_spdf@data$DAUID)
    t <- t[!(t@data$DAUID %in% round1_spdf@data$DAUID), ]
    tdf <- subset(tdf, !(DAUID %in% round1_spdf@data$DAUID))
    plot(t,  col = "blue")
    plot(round1_spdf, col = "green", add = T)
    
    #' if the shapefile becomes null then it means that this was the last zone available
    #' and it will need to be saved into a TRESO zone by itself
    if(nrow(t) == 0){
      alone_spdf <- spRbind(alone_spdf, round1_spdf)
    } 

    # this loop runs till either the threshold is met or if we run of out DAs to merge
    while (round1_spdf@data$CumAct < ap_cnt && nrow(tdf) > 0){
      
      #' run the Step 1 function to get the dataframe that shows all the DAs that touch each
      #' other. 
      rel1 <- step1(round1_spdf, t)
      # if there is only one row in the relative dataframe then there is no
      # touching DA. So save the DA as a TRESO zone.
      if(nrow(rel1) == 1){
        
        # When the DAs don't reach the threshold but run out of adjacent DAs then this if statement will
        # help bind the correct DAs
        if(length(setdiff(colnames(round1_spdf@data), colnames(alone_spdf@data))) == 0){
          # save to TRESO zone and depreciate the list of TAZs
          alone_spdf <- spRbind(alone_spdf, round1_spdf)
          t <- t[!(t@data$DAUID %in% round1_spdf@data$DAUID), ]
          tdf <- subset(tdf, !(DAUID == round1_spdf@data$DAUID))
        } 
        
      } else {
        # run Step2 function subsequent to the above if statement, because this means that there are
        # DAs that are touching each other.
        n_round <- step2()
        round1_spdf <- n_round[[1]]
        t <- n_round[[2]]
        r1 <- n_round[[3]]
        tdf <- n_round[[4]]
        taz_fourth <- n_round[[5]]
        
      }
      #' if the number of rows of the relative dataframe are equal to 1 then it means
      #' that there are no more adjoining DAs even though the threshold has not been met
      #' and the while loop needs to be broken.
      if (nrow(rel1) == 1){
        break
      }
      # inner WHILE loop
    }
    
    # bind the TRESO zones only if the columns match
    if(length(setdiff(colnames(round1_spdf@data), colnames(gen_spdf@data))) == 0){
      # bind TRESO zones
      gen_spdf <- spRbind(gen_spdf, round1_spdf)
      tdf <- subset(tdf, !(DAUID %in% round1_spdf@data$DAUID))
    }

    
    # Outer WHILE loop
  }
}

#' now remove the temp shapes that were created for receiving the shapes
gen_spdf <- gen_spdf[!(gen_spdf@data$Spatial == "Temp"), ]
alone_spdf <- alone_spdf[!(alone_spdf@data$GGH == 100), ]

# write out the files
writeOGR(gen_spdf, layer = paste0("TRESO4_Gen", ap_cnt), wd, 
         driver="ESRI Shapefile", overwrite_layer = T)

writeOGR(alone_spdf, layer = paste0("TRESO4_Alone", ap_cnt), wd, 
         driver="ESRI Shapefile", overwrite_layer = T)

###############################################################################
# Report the zones
print(nrow((first_taz@data)))
print(nrow((sec_taz@data)))
print(nrow((third_taz@data)))
print(nrow((taz_da_activity@data)))
print(nrow((taz_fourth_multi@data)))
print(nrow((gen_spdf@data)))
print(nrow((alone_spdf@data)))

# Merge the shapefiles to produce a single file for output in addition to the 
# individual files

# merge first three sets of zones
first_three <- spRbind(first_taz, sec_taz)
first_three <- spRbind(first_three, third_taz)

# now merge the Fourth generation TRESO zones
taz_da_activity@data$DisNum <- 0
taz_da_activity@data$DU <- taz_da_activity@data$dwell
taz_da_activity@data$Stops <- taz_da_activity@data$TruckStops
taz_da_activity@data$CSD <- taz_da_activity@data$CSDUID
taz_da_activity@data$Act <- taz_da_activity@data$dwell*4 + 
  taz_da_activity@data$TruckStops/scale_trk
# only keep relevant columns
taz_da_activity <- taz_da_activity[, c("DisNum", "DU", "Stops", "CSD", "Act")]

# Now the Multiple CSDs
taz_fourth_multi@data$DisNum <- 0
taz_fourth_multi@data$DU <- 0
taz_fourth_multi@data$Stops <- 0
taz_fourth_multi@data$CSD <- taz_fourth_multi@data$TAZ_NO
taz_fourth_multi@data$Act <- taz_fourth_multi@data$TotAct
# only keep relevant columns
taz_fourth_multi <- taz_fourth_multi[, c("DisNum", "DU", "Stops", "CSD", "Act")]

# Now the generalized case
gen_spdf@data$DisNum <- 0
gen_spdf@data$DU <- 0
gen_spdf@data$Stops <- 0
gen_spdf@data$CSD <- 0
gen_spdf@data$Act <- gen_spdf@data$CumAct
# only keep the relevant columns
gen_spdf <- gen_spdf[, c("DisNum", "DU", "Stops", "CSD", "Act")]

# Now the fourth case of TRESO for the Alone Case of TRESO zones.
alone_spdf@data$DisNum <- 0
alone_spdf@data$DU <- alone_spdf@data$dwell
alone_spdf@data$Stops <- alone_spdf@data$TruckStops
alone_spdf@data$CSD <- alone_spdf@data$CSDUID
alone_spdf@data$Act <- gen_spdf@data$TotAct



###############################################################################

f <- first_taz@data
s <- sec_taz@data
t <- third_taz@data
tda <- taz_da_activity@data
tmulti <- taz_fourth_multi@data
gs <- gen_spdf@data
as <- alone_spdf@data


###############################################################################
# Plot the TRESO zones

plot(gen_spdf, col = "red")
plot(alone_spdf, add = T, col = "red")
plot(first_taz, add = T, col = "red")
plot(sec_taz, add = T, col = "red")
plot(third_taz, add = T, col = "red")
plot(taz_fourth_multi, add = T, col = "red")
plot(taz_da_activity, add = T, col = "red")

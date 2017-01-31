library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(reshape2)
library(stringi)
library(maptools)
library(spatialEco)
library(progress)

#' Set working directory
wd <- setwd("c:/personal/r")

###############################################################################
# This algorithm takes the ELF by Place of Residence that has no Fixed Place of
# work and assigns it to the Quads using the following logic:
# + Assign Tom's estimates of workers with NFPoW to the respective Quads within a CSD
# + For each NAICS, use the skim matrix to tag all Qauds within a user speficied travel time
# + One travel time threshold is used for Construction and another for non-Construction
# as construction workers can travel longer distances
# + Once the candidate Quads are tagged using distance, only those Quads are kept that have
# a corresspoding NAICS employment. This is done to focus the temp POW. 
# + With the final Quads selected, they are dissolved to form a continous or discontinous polygon
# + Then tht total number employment by NAICS is randomly smeared across the above polygon
# + A point in polygon is then peformed to tabulate the temp POW by QUad and NAICS
# + Given that such temp employment could visit two/three job sites in a day, this process is
# repeated three times to establish the three different daily locations these workers 
# can be found in.

##############################################################################

#' Function for setting NAs
f_na <- function(df) {
  # This function is used to set all NA values to zero in a dataframe.
  
  df[is.na(df)] <- 0
  return(df)
}

#' function for expanding dataframes
exp_f <- function(df, field){
  # This function cleans and expandsd the dataframe while sequentially 
  # adding in a unique ID
  temp <- subset(df, df[[field]] != 0, select = c("CSDUID", "ID", field)) 
  temp <- temp[rep(seq(nrow(temp)), temp[[field]]), 1:2] 
  
  return(temp)
}


###############################################################################
#' User defined thresholds
const_val <- 120
other_val <- 60

# fraction that defines the proportion of construction vs other employment.
# user can change this to accommodate different sensitivities, if desired
c_frac <- 0.3
o_frac <- 0.7
###############################################################################

#' Bring in the population file from PopSYn3_Allocation.R. The number of persons
#' per QuadZone will be used as a surrogate for employed labor force. This is 
#' preferred as opposed to allocating the 2011 NHS's POR by NAICS2 data to the 
#' Quad zones prior to undertaking the analysis.
#' 
#' Bring in a clipped QuadTree file. This is important if better accuracy is 
#' desired. Because without clipping the QUadTree to the Ontario boundary, a 
#' number of Quad Centroids fall outside Ontario thus cannot be assigned to a
#' CSD, which results in missing values for NFPW employment.
#' 
#' Also batch in the new files that Tom produced that show the list of employment
#' by NFPoW by CSD and another one by NAICS. Note, Tom did not have a file that 
#' combined the two so some code is needed to create a file that has 
#' CSD by NAICS for NFPoW.
#' 
#' Finally, we need the skim matrix to tag zones within a certain travel time

#' Personss by QT file
person_qt <- read.csv("person_qt.csv", stringsAsFactors = FALSE) 
#' Skim matrix
skim <- read.csv("mf100.csv", stringsAsFactors = FALSE, nrows = 6495, 
                 header = TRUE) %>% .[1:6496]    # limit the matrix to Quad
#' Tom's files
csd_nfp <- read.csv("Ontario EPOR by POW.csv", stringsAsFactors = FALSE) 
naics_nfp <- read.csv("Ontario EPOR and EPOW by Industry.csv", stringsAsFactors = FALSE)
  naics_nfp <- transform(naics_nfp, frac = NFPW/sum(naics_nfp$NFPW))

#' batch in the QuadTree and CSD shapefiles and strip out any 
#' unnecessary fields
qt <- "QuadTree_clipped"
csd <- "CSD"

cs <- csd_poly@data

write.csv(cs, "csd_ggh.csv")

qt_poly <- readOGR(wd, qt) 
  q_df <- qt_poly@data
csd_poly <- readOGR(wd, csd) %>% .[, c("CSDUID", "CSDNAME", "GGH")]

  # Check if the projection systems for the two Quad Tree and CSD are the same.
  # If not the same, then return error and stop()
  crs_qt_poly <- proj4string(qt_poly)
  crs_csd_poly <- proj4string(csd_poly)  

  # check if the projections are the same
  {
    if (crs_qt_poly != crs_csd_poly) 
      stop("Projection systems are not the same. 
           Please make them same before proceeding")
  }
 
###############################################################################
# transfer CSD ID to Quad Tree. First get the IDs of the QuadTree for these
# will be used to attach to the centroids of the Quads
qt_id <- qt_poly@data %>% .[-2]
qt_cen = gCentroid(qt_poly, byid = TRUE) %>% SpatialPointsDataFrame(., qt_id)

writeOGR(qt_cen, layer = paste0("QTclipped_cen"), wd, 
         driver="ESRI Shapefile", overwrite_layer=T )

#' transfer CSD info to each QT centroid
qt_poly1 <- point.in.poly(qt_cen, csd_poly)

qt_poly1_df <- qt_poly1@data %>% subset(., select = c("ID", "CSDUID"))

###############################################################################
#' group persons by Quad. This will act as the surrogate for existing ELF at POR
per_qt <- person_qt %>% group_by(ID) %>% summarise(Per = n()) %>% 
  merge(., qt_poly1_df, by = "ID", all.x = TRUE) %>% # transfer CSDID
  merge(., csd_nfp, by.x = "CSDUID", by.y = "CSD", all.x = TRUE) %>%    # transfer 'Total', 'NFPW'
  subset(., select = c("CSDUID", "ID", "Per", "NFPW")) %>% na.omit(.)

#' get total jobs in CSDUID and then split the NFPW based on the proportion of 
#' each NAICS2
sum <- per_qt %>% group_by(CSDUID) %>% summarise(TotPersons = sum(Per), 
                                                 TotNFPW = min(NFPW))

per_qt_sum <- merge(per_qt, sum, by = "CSDUID", all.x = TRUE) %>% 
  transform(., NFPW_Jobs = round((Per/TotPersons)*NFPW),0) %>% na.omit(.) %>% 
  subset(., NFPW_Jobs != 0)

#' build queries for populating construction and other NFPW 
per_qt_sum1 <- transform(per_qt_sum, ConstNFPW = 
                           ifelse(round((NFPW_Jobs*c_frac),0) < 1, NFPW_Jobs, 
                                  round((NFPW_Jobs*c_frac),0))) %>% 
  transform(., OthNFPW = NFPW_Jobs - ConstNFPW) %>% .[order(.$ID), ]

###############################################################################
#' Get the skims ready for batching in. There are rownames but no column names
#' in the current matrix. This will change as we batch in the skims.311 file.
#' For the time being this code block is written to work with the skims that I
#' had currently available with me.

q_names <- skim[1]   # get copy of first column as this is the rownames

# fix column names
skim1 <- skim
colnames(skim1)[2:6496] <- q_names[,1]

#' subset dataframe to all those Quad zones that have NFPW and then convert to 
#' long format. Also get rid of trip interchanges with a travel time of greater
#' than 120 mins. These are not considered candidates
selectedRows <- skim1$p.q..val. %in% per_qt_sum$ID
skim2 <- skim1[selectedRows,]

#' melt and subset
skim2_long <- melt (skim2, id.vars = "p.q..val.") %>% 
  setNames(., c("Origin", "Dest", "Ttime")) %>% 
  subset(., Ttime < const_val) %>% .[order(.$Origin), ]

write.csv(skim2_long, "skims_long.csv")

###############################################################################
#' Expand the data that is to be assigned. This is needed as each record will get
#' appended to a randomly generated point

#' Create two **unexpanded** dataframes; one for Other and one for Const NFPW
other <- subset(per_qt_sum1, OthNFPW != 0, select = c("ID", "OthNFPW")) %>% 
  .[order(.$ID),]
const <- subset(per_qt_sum1, ConstNFPW != 0, select = c("ID", "ConstNFPW")) %>%
  .[order(.$ID),]

#' create two **expanded** dataframes; one for Other and one for Const NFPW
other_df <- exp_f(per_qt_sum1, "OthNFPW") %>% 
  transform(., UID = seq(1, length.out = nrow(.))) %>% .[order(.$ID), ]
#' the UID for the construction dataframe needs to be renumbered based on the numbers
const_df <- exp_f(per_qt_sum1, "ConstNFPW") %>% 
  transform(., UID = seq(nrow(other_df)+1, length.out = sum(per_qt_sum1$ConstNFPW))) %>%
  .[order(.$ID), ]

###############################################################################
###############################################################################
############################ BEGIN ALLOCATION #################################
#' Get set of Other and Construction NFPW jobs for ***FIRST Quad Zone***
#' We are using a function for doing the needful - first_f

first_f <- function(num, data, val, data_df){
  #' This function creates the first of the random points.
  #' Num - the QuadZone id; should start with 1 as that is the first row
  #' data - unexpanded dataframe of the CONST or OTHER NFPW
  #' val - travltime value defined by the user at the beginning of script
  #' data_df - exanded dataframe of the CONST or OTHER NFPW
  
  # get the first quad
  first_quad <- data$ID[num]
    # select skims that belong to First Quad Zone 
  orig_first <- subset(skim2_long, Origin == first_quad & Ttime <= val)
  
  #' query for selecting destination Quad zones
  dest_query <- qt_poly@data$ID %in% orig_first$Dest %>% cbind(., q_df) %>% 
    setNames(., c("cond", "ID")) %>% 
    subset(., cond == TRUE) %>%
    subset(., select = "ID")  
  
  # merge the query to the database of the shapefile
  qt_poly@data = data.frame(qt_poly@data, 
                            dest_query[match(qt_poly@data$ID, dest_query$ID),]) 
  
  #' Now subset the Quad Tree to only select those records that do not contain a NA.
  #' This essentially will give only those Quads that were successfully joined.
  qt_poly_i <- qt_poly[!is.na(qt_poly@data[[3]]),]
  # to contain the dissolving ID
  qt_poly_i@data$Diss <- first_quad
  
  # dissolve QUads
  region <- gUnaryUnion(qt_poly_i, id = qt_poly_i@data$Diss)
  
  # get spatial dataframe of points
  nfpw_i <- subset(data_df, ID == first_quad)
  
  # randomly assign the points
  sp <- spsample(region, nrow(nfpw_i), type = "random", iter = 6)
  
  #' convert to spatial points data frame
  SPDF = SpatialPointsDataFrame(sp, nfpw_i)
  
  #' reset the third column of the Quad Zones to 0. 
  qt_poly@data[3] <- 0
  
  return(SPDF)
}

#' use the function to generate the first set Quad zone points for Other 
#' and Const NFPW 
other_first  <- first_f(1, other, other_val, other_df)
const_first  <- first_f(1, const, const_val, const_df)

###############################################################################
#' Now enumerate over the remaining Quad zones. First Other and then Const NFPW

#' generate progress bar
pb <- winProgressBar(title="Example progress bar", label="0% done", 
                     min=0, max=100, initial=0)

# get list of QuadZones for both Const and Other NFPW
enum_other <- as.list(other$ID)
enum_const <- as.list(const$ID) 

#' This code block runs the random sample point generation procedure for the
#' rest of the Quad zones for Other NFPW 
for (i in 2:length(enum_other)){
  #' use function to populate each Quadzone catchment area
  other_rem <- first_f(i, other, other_val, other_df)
  #' cumulatively merge the points to the first quad zone
  other_first <- spRbind(other_first, other_rem)
  #' update progress bar
  Sys.sleep(0.1) # slow down the code for illustration purposes
  info <- sprintf("%d%% done", round((i/length(enum_other))*100))
  setWinProgressBar(pb, i/length(enum_other)*100, label=info)
  
}

#' This code block runs the random sample point generation procedure for the
#' rest of the Quad zones for Construction NFPW 
for (i in 2:length(enum_const)){
  #' use function to populate each Quadzone catchment area
  const_rem <- first_f(i, const, const_val, const_df)
  #' cumulatively merge the points to the first quad zone
  const_first <- spRbind(const_first, const_rem)
  #' update progress bar
  Sys.sleep(0.1) # slow down the code for illustration purposes
  info <- sprintf("%d%% done", round((i/length(enum_const))*100))
  setWinProgressBar(pb, i/length(enum_const)*100, label=info)
  
}


#' write shapefile
writeOGR(other_first, layer = paste0("NFPW_Other"), wd, 
         driver="ESRI Shapefile", overwrite_layer=T )

writeOGR(const_first, layer = paste0("Const_Other"), wd, 
         driver="ESRI Shapefile", overwrite_layer=T )


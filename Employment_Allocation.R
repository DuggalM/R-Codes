library(sp)
library(rgdal)
library(rgeos)
library(dplyr)
library(maptools)
library(spatialEco)
library(progress)

#' Set working directory
wd <- setwd("c:/personal/r")

###############################################################################
gghm.pol <- "GGH_TAZ"           # GGHM TAZ polygon
emp <- "EmpFile1"
qt <- "QuadTree"

#' read shapefiles
emp_shp <- readOGR(wd, emp) 
  emp_shp <- emp_shp[, "ID"]
#equiv <- read.csv("equiv.csv")
gghm_poly <- readOGR(wd, gghm.pol)
qt_shp <- readOGR(wd, qt)

#' Get employment values by SyntheticEmp corressponding to either a GGH TAZ
#' or DA by 2-digit NAICS and sort by DAUID
#' Also batch in the total employment at the geography

on_emp_naics <- read.csv("SyntheticEmp_DAGGH.csv", stringsAsFactors = FALSE, 
                         col.names = c("DAUID", "naics2", "seqID")) %>% .[order(.$DAUID),]

# total employment
on_emp_tot <- read.csv("SyntheticTotEmp_DAGGH.csv", stringsAsFactors = FALSE) %>% 
  subset(., emp != 0)

###############################################################################
#' Join the total employment to the employment shapefile
###############################################################################

emp_shp@data = data.frame(emp_shp@data, 
                             on_emp_tot[match(emp_shp@data$ID, on_emp_tot$taz),]) 

# get rid of NA's and zeros in the TAZ. These are basically geographies that do not have
# any employment in them
emp_shp <- emp_shp[!is.na(emp_shp@data$taz),] 


###############################################################################
#' Select the first polygon and add randomly create the number of points that
#' correspond to the employment
###############################################################################
    
#' Order the shapefile and subset the first polygon
emp_shp1 <- emp_shp[order(emp_shp@data$ID),]

# select the first polygon

emp_poly_1 <- emp_shp1[emp_shp1@data$ID == emp_shp1@data$ID[1], ]
  first <- emp_poly_1@data
  
#' create random points based on the number of jobs
#' in the first DA

  # get the number of jobs in first DA
    jobs_n <- emp_shp1@data$emp[1]
  
sp <- spsample(emp_poly_1, jobs_n, type = "random")

#' subset the records from the ON_Emp data frame 
#' that corressponds to the first DA
emp_1 <- subset(on_emp_naics, DAUID == first$ID)

#' convert to spatial points data frame
SPDF = SpatialPointsDataFrame(sp, emp_1)


###############################################################################
#' Now do this for the reamining polygons
###############################################################################
  
  # get unique DA ids
  id <- emp_shp1@data %>% .[1]
  
  M3.lst <- as.list(id$ID)    # use list to enumerate over TAZs
  #' create progress bar
  pb <- winProgressBar(title="Example progress bar", label="0% done", 
                       min=0, max=100, initial=0)

for (i in 2:length(M3.lst)){
  # for loop that gets every polygon and creates random points in it based on 
  # the number of jobs
  
  #' select a DA
  emp_poly_i <- emp_shp1[emp_shp1@data$ID == M3.lst[i], ]
  
  # get the number of jobs in each polygon
  n_jobs_i <- emp_shp1@data$emp[i]
 
  #' create random points
  sp_i <- spsample(emp_poly_i, n_jobs_i, type = "random", iter = 6)
  
  #' subset the records to dataframe
    jobs_sub <- subset(on_emp_naics, DAUID == M3.lst[i])

  #' convert to spatial points data frame
  SPDF.i = SpatialPointsDataFrame(sp_i, jobs_sub)

  #+ save results
  SPDF <- spRbind(SPDF, SPDF.i)
  
  #' update progress bar
  Sys.sleep(0.1) # slow down the code for illustration purposes
  info <- sprintf("%d%% done", round((i/length(M3.lst))*100))
  setWinProgressBar(pb, i/length(M3.lst)*100, label=info)

}
  
  #' make copy
  SPDF_on <- SPDF

  #' write shapefile
  writeOGR(SPDF, layer = paste0("EmploymentOntario_Firm"), wd, 
           driver="ESRI Shapefile", overwrite_layer=T )

###############################################################################
#' Do a point in polygon and transfer the QuadID to the points
###############################################################################

SPDF_qt <- point.in.poly(SPDF, qt_shp)

ss <- SPDF_qt@data %>% .[-c(1,5)] 
colnames(ss) <- c("Naics2", "EmpID", "ProvID")

###############################################################################
#' Join the Firm ID to the table above
###############################################################################
#' Firms are joined to the employee table 

#' batch in Rick's Firm Synthesis file
firm <- read.csv("c:/personal/r/firms.csv", stringsAsFactors = FALSE) %>% 
  subset(., taz > 0 & n_employees > 0, select = c("firm_id", "n_employees", "taz")) %>% 
  .[order(.$taz),] 


# create an empty dataframe
firm_expanded <- as.data.frame(matrix(NA_integer_, 
                                      nrow = sum(firm$n_employees), ncol = 3))
# expand the firms to number of employees
firm_expanded <- firm[rep(seq(nrow(firm)), firm$n_employees), 1:3] 

# Now cbind the expanded firms to the employees table assigned to the QT
ss <- cbind(ss, firm_expanded) %>% .[-c(6,7)]

###############################################################################
#' write out the final file
write.csv(ss, "Jobs_QT.csv", row.names = FALSE)

  
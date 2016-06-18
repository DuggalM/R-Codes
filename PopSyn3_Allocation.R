#library(raster)
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
hh <- "households" 
pp <- "persons"
ac <- "ActivityCentroids"
da <- "DA"
qt <- "QuadTree"

#' read activity file which is output from Allocation_StatsCAN_DA.R

ac.shp <- readOGR(wd, ac)
da.shp <- readOGR(wd, da)
  da.shp <- da.shp[, c("CSDUID", "DAUID", "GGH")]
equiv <- read.csv("equiv.csv")
gghm.poly <- readOGR(wd, gghm.pol)
qt.shp <- readOGR(wd, qt)

  g <- gghm.poly@data 
#' bring in PopSyn3 hholds
hhold <- read.csv(paste0(hh, ".csv"))
  hhold.sum <- hhold %>% group_by(taz) %>% summarise(hh.taz = n())    # summarize household file
    print(paste0("The total households from POpSyn3 are ", sum(hhold.sum$hh.taz)))
    
    #' only keep GGHM households
    hhold.sum.g <- subset(hhold.sum, taz < 9332)
      print(paste0("The total households in the GGH are ", sum(hhold.sum.g$hh.taz)))


###############################################################################
#' Select the first TAZ and add randomly create the number of points that
#' correspond to the households
###############################################################################
    
#' subset the TAz that corressponds to the first TAZ
gghm.poly.1 <- gghm.poly[gghm.poly@data$TAZ_NO == gghm.poly@data$TAZ_NO[1], ]
  plot(gghm.poly.1)
  
#' create random points based on the number of households
#' in the first TAZ

  # get the number of households, but first only keep the GGHM TAZs
    hh_n <- hhold.sum.g$hh.taz[1]
  
sp <- spsample(gghm.poly.1, hh_n, type = "random")

#' subset the TAz from the households data frame 
#' that corressponds to the first TAZ
hhold.1001 <- subset(hhold, taz == 1001)

#' convert to spatial points data frame
SPDF = SpatialPointsDataFrame(sp, hhold.1001)
  plot(SPDF, add = T)

  
###############################################################################
#' Now do this for the reamining GGHM TAZs
###############################################################################
  
  M3.lst <- as.list(hhold.sum.g$taz)    # use list to enumerate over TAZs
  #' create progress bar
  pb <- winProgressBar(title="Example progress bar", label="0% done", 
                       min=0, max=100, initial=0)

for (i in 2:length(M3.lst)){
  # for loop that gets every TAZ and creates random points in it based on 
  # the number of households
  
  #' select a TAZ
  gghm.poly.i <- gghm.poly[gghm.poly@data$TAZ_NO == M3.lst[i], ]
  
  # get the number of households in each TAZ
  hh_i <- hhold.sum$hh.taz[i]
 
  #' create random points
  sp.i <- spsample(gghm.poly.i, hh_i, type = "random", iter = 6)

  #' subset the ith TAZ and save as dataframe
    hhold.i <- subset(hhold, taz == M3.lst[i])

  #' convert to spatial points data frame
  SPDF.i = SpatialPointsDataFrame(sp.i, hhold.i)
  
  #+ save results
  SPDF <- spRbind(SPDF, SPDF.i)
  
  #' update progress bar
  Sys.sleep(0.1) # slow down the code for illustration purposes
  info <- sprintf("%d%% done", round((i/length(M3.lst))*100))
  setWinProgressBar(pb, i/length(M3.lst)*100, label=info)

}
  
  #' make copy
  SPDF.ggh <- SPDF
  
  #' write shapefile
  writeOGR(SPDF, layer = paste0("hholds_centroids_Processed_GGH"), wd, 
           driver="ESRI Shapefile", overwrite_layer=T )

  
###############################################################################
#' Now take the remaining CSDs, including northernlight points. This is done by
#' using the Activity Centroids shapefile
###############################################################################

#' The challenge here is that the CSD and DA level household information used in the 
#' Quad Tree is different from that in the PopSYn3. On speaking to Mauricio he 
#' noted that the difference could be due to post-census adjustments. Given 
#' that the Quad Tree process cannot be rerun because MTO has already created
#' the Centorid Connectors which is a semi-automatic process, we will stick to
#' using PopSYn3 outputs at the CSD level as a starting point here.
#' But this means that the household information for each Activity Centroid 
#' that was based on "post census, as per Mauricio" DA level info will be scaled.
  
  rm(a.cen)
  a.cen <- ac.shp@data
  #' subset for outside GGH and summarize at CSD level from Quad Tree 
  a.cen.sum <- a.cen %>% subset(., GGH == 0) %>% group_by(CSDUID) %>% 
    summarise(hh.qt = sum(val))
  
  #' subset PopSyn3 to CSD level information only
  hhold.sum.pop3 <- hhold.sum %>% subset(., taz > 9332) 
    names(hhold.sum.pop3)[names(hhold.sum.pop3) == "hh.taz"] <- "hh.pop3"
    
  #' Now merge the above two summaries to understand what and where are the 
  #' differences by CSDs
  a.cen.sum <- merge(a.cen.sum, hhold.sum.pop3, by.x = "CSDUID", by.y = "taz", 
                     all.x = TRUE)
    a.cen.sum[is.na(a.cen.sum)] <- 0
    
    #' Now join the tables together
    a.cen <- merge(a.cen, a.cen.sum, by.x = "CSDUID", by.y = "CSDUID", all.x = TRUE)
      a.cen <- transform(a.cen, Frac = val / hh.qt) %>%  
        transform(., val1.qt = Frac * hh.pop3) 
      
      #' set NaN to zero
      a.cen[is.na(a.cen)] <- 0
          
    
    #' First assign each Northern Light to a DA and then subset to gather DAs
    #' that are outside the GGH and include NorthernLights. These DAs will then 
    #' be used to generate random points based on the households within them
    
      #' first get the DA households that are outside the GGH and not with
      #' any Northern Lights
      da.newhh <- subset(a.cen, GGH == 0 & DAUID != 0) %>% group_by(DAUID) %>% 
        summarise(No.hhold = sum(val1.qt))
      
        print(paste0("The total households that are outside the GGH and do not have the Northern Lights are ", sum(da.newhh$No.hhold)))
      
      #' Now do a point in poly with the NorthernLights and DA polygon
      #' but first merge back val1.qt. The intention is to summarise 
      #' the hholds by DAs
      
      #' transfer the revised household values (val1.qt) back to the Activity
      #' Centroids shapefile
      ac.shp@data = data.frame(ac.shp@data, 
                                   a.cen[match(ac.shp@data$Xcoord, a.cen$Xcoord),]) 
      
        #' Now subset only the NorthernLights
        da.nl <- ac.shp[ac.shp@data$DAUID == 0, ] 
          # only keep selected columns
          da.nl <- da.nl [, c("CSDUID", "Xcoord", "Ycoord", "val1.qt")]  
      
           #' Do Point in Polygon to transfer DAUID to Northern Lights
           da.nl <- point.in.poly(da.nl, da.shp)
             # summarize the data
             da.nl.df <- da.nl@data  %>% group_by(DAUID) %>% 
               summarise(val1.qt = sum(val1.qt))
               
             print(paste0("The total Northern Lights households are ", sum(da.nl.df$val1.qt)))
      
      #' Now join the household estimates for DAs that are outside
      #' the GGH and those that contain the Northern Lights. This final DA
      #' file shoudl include the remaining households that we need to create
      #' random points for to carry out the transfer of Quad Tree IDs
      
      hholds.out <- merge(da.newhh, da.nl.df, by.x = "DAUID", by.y = "DAUID", all = T)
      
        #' set NaN to zero
        hholds.out[is.na(hholds.out)] <- 0
        #' add field for tot households
        hholds.out <- transform(hholds.out, tothh = round(val1.qt + No.hhold)) %>% 
          transform(., DAUID1 = as.numeric(as.character(DAUID))) %>% 
          subset(., select = -c(DAUID)) %>% subset(., tothh != 0) 
        
        #' transfer back CSDUID and also calculate the difference in households between 
        #' the CSD ouputs from PopSyn3 and those got after processing the data to the
        #' DA level. Specifically, the Fraction calculation causes decimals and the
        #' rounding results in 4 more households, but at the individual CSD there could        #' be higher variance.

        
        # create temporary CSD dataframe
        hhold.sum.out <- subset(hhold.sum, taz > 10000)    
        
        #' Join to transfer CSDUID
        m <- merge(hholds.out, equiv, by.x = "DAUID1", by.y = "DAUID1", all.x = T) %>% 
          group_by(CSDUID1) %>% summarise(hh.process = sum(tothh))
        
          #' join results from above to temp CSD dataframe and estimate the differences
          m1 <- merge(hhold.sum.out, m, by.x = "taz", by.y = "CSDUID1", all.x = T)
            m1$diff <- m1$hh.taz-m1$hh.process
            
            #' merge the CSDUID1 and the difference field to hholds.out
          hholds.out <- merge(hholds.out, equiv, by.x = "DAUID1", by.y = "DAUID1")
            hholds.out <- merge(hholds.out, m1, by.x = "CSDUID1", by.y = "taz")
            #' Now tag the first DAs within each CSD. This households within this
            #' DA will be adjusted to exactly match PopSyn3's CSD level inputs
            tag <- subset(hholds.out, diff != 0)
            tag <- tag[!duplicated(tag$CSDUID1),] %>% 
              transform(., adj = 1) %>% subset(., select = c("CSDUID1", "DAUID1", "adj"))
            
            #' Join the tag field back to hholds.out
            hholds.out <- merge(hholds.out, tag, by.x = "DAUID1", by.y = "DAUID1", all.x = T) 
              hholds.out[is.na(hholds.out)] <- 0
              
              #' Now adjust the household estimates
              
              hholds.out$tothh1 <- ifelse(hholds.out$adj == 1, hholds.out$tothh + 
                                            hholds.out$diff, hholds.out$tothh)
              
                hholds.out <- hholds.out[order(hholds.out$DAUID1),]    # sort on DAUID1
          
          print(paste0("The total households outside the GGH that are to be converted to points are ", sum(hholds.out$tothh1)))
          
###############################################################################
#' Now create random spatial points for the DAs (hholds.out)
###############################################################################
        
          
#' create ID
hholds.out$idvalueentfier <- 1

#' join the hholds.out data to the DA shapefile to subset DAs that 
#' need to be populated by spatial points
da.shp@data = data.frame(da.shp@data, 
                         hholds.out[match(da.shp@data$DAUID, hholds.out$DAUID),]) 

  da.shp@data[is.na(da.shp@data)] <- 0     # set NAs to zero
  
  #' subset DAs that have an identifier boolean of 1 or a tothh value of 0
  da.shp.sub <- da.shp[da.shp@data$identfier == 1, ] 
    da.shp.sub <- da.shp[da.shp@data$tothh != 0, ]
  
#' Now enumerate over the left over DAs to create spatial points

  #' first create equivalency tables and sort them
  dd1 <- da.shp.sub@data
  equiv <- dd1 %>% subset(., select = c("CSDUID", "DAUID1")) %>% 
    transform(., CSDUID1 = as.numeric(as.character(CSDUID)))
    equiv <- equiv[order(equiv$DAUID1),]
    
  #' create DAUID list for enumeration
    M3.lst <- as.list(equiv$DAUID1)   # use list to enumerate over DAs

    #' create progress bar
    pb <- winProgressBar(title="Example progress bar", label="0% done", 
                         min=0, max=100, initial=0)
    
  #' start for loop  
  for (i in 1: length(M3.lst)){
    # for loop that gets every DA and creates random points in it based on 
    # the number of households
          
    #' select a DA
    da.poly.i <- da.shp.sub[da.shp.sub@data$DAUID1 == M3.lst[i], ]
    temp <- da.poly.i@data
       
    # get the number of households in each DA
    hh_i <- hholds.out$tothh1[i]
         
    #' create random points
    sp.i <- spsample(da.poly.i, hh_i, type = "random", iter = 7)
   
    #' subset the ith DA and save as dataframe
    
      #' first get the corressponding CSD from the equivalency file
      csd.i <- equiv$CSDUID1[i]
  
      #' now subset records from household file based on CSD and then randomly sample
      #' records based on tothh field
      hhold.i <- subset(hhold, taz == csd.i) 
        hhold.i <- hhold.i[sample(1:nrow(hhold.i), hh_i, replace = FALSE), ]
    
      #' convert to spatial points data frame
      SPDF.i = SpatialPointsDataFrame(sp.i, hhold.i)
          
       #+ save results
        SPDF <- spRbind(SPDF, SPDF.i)
        
        #' update progress bar
        Sys.sleep(0.1) # slow down the code for illustration purposes
        info <- sprintf("%d%% done", round((i/length(M3.lst))*100))
        setWinProgressBar(pb, i/length(M3.lst)*100, label=info)
        

  }
        
    #' write shapefile
    writeOGR(SPDF, layer = paste0("hholds_centroids_Processed_All1"), wd, 
             driver="ESRI Shapefile", overwrite_layer=T )
    
    #' make copy of the SPDF at this stage
    SPDF.All1 <- SPDF
###############################################################################
#' get a list of CSDs and DAs that did not produce enough points
#' Write a error statement here to check if this step is really needed

    # get the SPDF data frame
    #SPDF <- SPDF.All1
    ss <- SPDF@data
    
    #' summarise to find out where is the underreporting
    ss.sum <- ss %>% group_by(taz) %>% summarise(hh_spatial = n())
    
    #' join the summaries to the original inputs and calculate the differences
    temp10 <- merge(hhold.sum, ss.sum, by.x = "taz", by.y = "taz", all.x = T)
      temp10[is.na(temp10)] <- 0 
    
      temp10 <- transform(temp10, diff = hh.taz - hh_spatial)
        temp10.sub <- subset(temp10, diff > 0) %>% transform(., leftover = 1)
        
        #' create a new hholds.out file to sample from and get rid of household 
        #' values of 0
        
        hholds.out.sub <- merge(temp10.sub, hholds.out, by.x = "taz", 
                                by.y = "CSDUID", all.x = T)
        out <- subset(hholds.out.sub, tothh1 == 0) %>% subset(., select = c("DAUID1"))
          hholds.out.sub <- hholds.out.sub[order(hholds.out.sub$DAUID1), ] %>% subset(., tothh1 > 0)
          
         
        #' Now join back the leftover CSD tags to SPDF. Because some CSDs even though they 
        #' did not meet their housing spatial point targets are left inside the SPDF because 
        #' of partial points
        
        SPDF@data = data.frame(SPDF@data, 
                                     temp10[match(SPDF@data$taz, temp10$taz),]) 
          #' create a revised SPDF that only includes those DAUID or CSDs with a 
          #' difference of zero 
          SPDF <- SPDF[SPDF@data$diff == 0, ]
            # strip unnecessary fields in CSD and DA level shapefiles
            SPDF <- SPDF[, c("hhid", "taz", "hhinc", "dtype")]

            #' get new equivalency file that corressponds to the leftover CSDs only
          
            dd1.sub <- da.shp.sub@data
          equiv.sub <- dd1.sub %>% subset(., select = c("CSDUID", "DAUID1")) %>% 
            transform(., CSDUID1 = as.numeric(as.character(CSDUID))) %>% 
            merge(., temp10.sub, by.x = "CSDUID1", by.y = "taz", all.y = T)
          equiv.sub <- equiv.sub[order(equiv.sub$DAUID1),]
            equiv.sub <- subset(equiv.sub, DAUID1 != out$DAUID1)   # get rid of the zero household DA
          

          #' make the above list of DA's that did not get the requisite spatial points
          #' into a list for enumeration
          
          M4.lst <- as.list(equiv.sub$DAUID1)
          
          #' create progress bar
          pb <- winProgressBar(title="Example progress bar", label="0% done", 
                               min=0, max=100, initial=0)
          
          #' start for loop  
          for (i in 1:105){
            # for loop that gets leftover DA and creates random points in it based on 
            # the number of households
            
            #' select a DA
            da.poly.i <- da.shp.sub[da.shp.sub@data$DAUID1 == M4.lst[i], ]
            temp1 <- da.poly.i@data
            
            #plot(da.poly.i)

            # get the number of households in each DA
            hh_i <- hholds.out.sub$tothh1[i]
                  
            #' create random points
            sp.i <- spsample(da.poly.i, hh_i, type = "random", iter = 6)
                    
            #' subset the ith DA and save as dataframe
            
            #' first get the corressponding CSD from the equivalency file
            csd.i <- equiv.sub$CSDUID1[i]
          
            #' now subset records from household file based on CSD and then randomly sample
            #' records based on tothh field
            hhold.i <- subset(hhold, taz == csd.i) 
              hhold.i <- hhold.i[sample(1:nrow(hhold.i), hh_i, replace = FALSE), ]
               
            #' convert to spatial points data frame
            SPDF.i = SpatialPointsDataFrame(sp.i, hhold.i)

            #plot(SPDF.i, add = T)
            
                    #+ save results
            SPDF <- spRbind(SPDF, SPDF.i)
                 
            #' update progress bar
            Sys.sleep(0.1) # slow down the code for illustration purposes
            info <- sprintf("%d%% done", round((i/length(M4.lst))*100))
            setWinProgressBar(pb, i/length(M4.lst)*100, label=info)
            
          }
  
          #' write shapefile
          writeOGR(SPDF, layer = paste0("hholds_centroids_Processed_All2"), wd, 
                   driver="ESRI Shapefile", overwrite_layer=T )
###############################################################################
#' Do a Point in Polygon for the     

          SPDF.QT <- point.in.poly(SPDF,qt.shp )
            spdf.qt.df <- SPDF.QT@data
            write.csv(spdf.qt.df, "households_qt.csv")
          
            #' bring in PopSyn3 Population
            pop3 <- read.csv(paste0(pp, ".csv"))
              pop3 <- merge(pop3, spdf.qt.df, by.x = "hhid", by.y = "hhid") %>% 
                subset(., select = -Area)
              write.csv(pop3, "population_qt.csv")

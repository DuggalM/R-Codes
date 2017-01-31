library(plyr)
library(dplyr)
library(reshape2)
library(DT)
library(ggplot2)

#' Set working directory
wd <- setwd("c:/personal/r")

#' batch in the hhold weights for the TTS trips
#' and the raw TTS PA table, and the lookup table that
#' Nem prepared
pa <- read.csv("c:/personal/r/Updated PA Table.csv")
lk <- read.csv("c:/personal/r/lookup.csv")

#' merge the expansion factors to copy over expansion factors
pp1 <- merge(pa, lk, by.x="o_pd", by.y="PD", all.x = T) 
  pp1$lnden <- pp1$LaneKm/pp1$Area
  pp1$socioden <- pp1$Pop_Emp/pp1$Area

# generate trip purpose list
  purpose.lst <- pp1 %>% group_by(purpose) %>% 
    summarise(trips = sum(gghm_weight)) %>% select(., purpose)
    purpose.lst <- as.vector(purpose.lst$purpose) #### convert to list
    names(purpose.lst) <- c("H2H", "HBE", "HBM", "HBO", "HBS", "HBU", "HBW", "NHB", "WBX")
    

#' Create LaneDensity Analysis using the named list of trip purposes to 
#' query for multiple trip purposes

  #' Now create a pivot table using the information wrangled from the above steps
  #' and also remove missing rows and subset data to AM peak period (600-900)

    # run the code below for all the trip purposes
    for (i in 1: length(purpose.lst)){
      
      # create daily dataframe
      pp.sum.daily <- subset(pp1, purpose == purpose.lst[i]) %>% group_by(o_pd, lnden, socioden) %>% 
        summarise(Dailytrips = sum(gghm_weight)) %>%   na.omit(.)
      
      # create peak period dataframe
      pp.sum.am <- subset(pp1,start_time>=600 & start_time<900 & purpose == purpose.lst[i]) %>% group_by(o_pd, lnden, socioden) %>% 
        summarise(AMPKtrips = sum(gghm_weight)) %>%   na.omit(.)
      
      # merge the pp.sum files and transfer over the data
      # and also drop repeated columns. This step also calculates
      # the am peak period ratio for HBW
      
      pp.sum <- merge(pp.sum.am, pp.sum.daily, by.x = "o_pd", by.y = "o_pd", all.x = T) %>% 
        subset(., select = -c(lnden.y, socioden.y))
      pp.sum$amratio <- pp.sum$AMPKtrips/pp.sum$Dailytrips 
      pp.sum$TPurp <- names(purpose.lst)[i]    #### add column with trip purpose
      # order by lane density
      pp.sum <- pp.sum[order(pp.sum$lnden.x), ]
      
      assign(paste0("pp.sum.lnden.", names(purpose.lst)[i]), pp.sum)
    }

#' rbind the dataframes to allow for facet type scatter plots
  # create a list of dataframe
  l <- list(pp.sum.lnden.H2H, pp.sum.lnden.HBE, pp.sum.lnden.HBM, pp.sum.lnden.HBO, pp.sum.lnden.HBS, 
            pp.sum.lnden.HBU, pp.sum.lnden.HBW, pp.sum.lnden.NHB, pp.sum.lnden.WBX)
  # rbind the lists using plyr
  pp.sum.all.lnden <- as.data.frame(do.call(rbind.fill, l))

#' Scatter Plot using purpose for faceting
  
    ggplot(pp.sum.all.lnden, aes(x=lnden.x, y=amratio)) + geom_point(shape=1) +
    geom_smooth(method = lm) + facet_wrap( ~ TPurp, ncol=3)

#' Scatter Plot using purpose for faceting
    
    ggplot(pp.sum.all.lnden, aes(x=socioden.x, y=amratio)) + geom_point(shape=1) +
      geom_smooth(method = lm) + facet_wrap( ~ TPurp, ncol=3)

#' Create scatter plot by PDs for amratio vs lane and socio density
  # create interaction variable
    pp.sum.lnden.HBW$int <- pp.sum.lnden.HBW$socioden.x/pp.sum.lnden.HBW$lnden.x
    
    ggplot(pp.sum.lnden.HBW, aes(x=socioden.x, y = amratio)) + geom_point(shape=1) +
      geom_smooth(method = lm)
    
    ggplot(pp.sum.lnden.HBW, aes(x=lnden.x, y = amratio)) + geom_point(shape=1) +
      geom_smooth(method = lm)
    
    ggplot(pp.sum.lnden.HBW, aes(x=int, y = amratio)) + geom_point(shape=1) +
      geom_smooth(method = lm)
    
#' K-means clustering

    ss <- subset(pp.sum.lnden.HBW, select = c(int, amratio))
    k <- kmeans(ss, 8)
    k

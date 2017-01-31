
library(plyr)
library(dplyr)
library(reshape2)
library(DT)
library(ggplot2)


#' Set working directory
wd <- setwd("c:/personal/r")

#' This attempt focuses on using a ratio of lanekms to popemp density
#' in an attempt to develop a relationship between am peak period ratio
#' of trips and the lanekms(popemp) ratio itself.

#' batch in the hhold weights for the TTS trips
#' and the raw TTS PA table, and the lookup table that
#' Nem prepared
pa <- read.csv("c:/personal/r/Updated PA Table.csv")
lk <- read.csv("c:/personal/r/lookup.csv")

#' merge the expansion factors to copy over expansion factors
pp1 <- merge(pa, lk, by.x="o_pd", by.y="PD", all.x = T) 
pp1$lnden <- pp1$LaneKm/pp1$Area
pp1$socioden <- pp1$Pop_Emp/pp1$Area
pp1$ratio <- pp1$lnden/pp1$socioden    #### use this ratio for the calculation in this version of the script

# generate trip purpose list
purpose.lst <- pp1 %>% group_by(purpose) %>% 
  summarise(trips = sum(gghm_weight)) %>% select(., purpose)
  purpose.lst <- as.vector(purpose.lst$purpose) #### convert to list
    names(purpose.lst) <- c("H2H", "HBE", "HBM", "HBO", "HBS", "HBU", 
                            "HBW", "NHB", "WBX")    #### now it is a named list


#' Analysis using the named list of trip purposes to. 
#' Now create a pivot table using the 
#' information wrangled from the above steps and also remove missing rows 
#' and subset data to AM peak period (600-900). Filter for Auto Drive and Passenger only

# run the code below for all the trip purposes
for (i in 1: length(purpose.lst)){
  
  # create daily dataframe
  pp.sum.daily <- subset(pp1, purpose == purpose.lst[i] & primary_mode == "Auto Drive") %>% 
    group_by(o_pd, ratio) %>% summarise(Dailytrips = sum(gghm_weight)) %>%   na.omit(.)
  
  # create peak period dataframe
  pp.sum.am <- subset(pp1,start_time>=600 & start_time<900 & purpose == purpose.lst[i] & 
                        primary_mode == "Auto Drive") %>% 
    group_by(o_pd, ratio) %>% summarise(AMPKtrips = sum(gghm_weight)) %>%   na.omit(.)
  
  # merge the pp.sum files and transfer over the data
  # and also drop repeated columns. This step also calculates
  # the am peak period ratio for HBW
  
  pp.sum <- merge(pp.sum.am, pp.sum.daily, by.x = "o_pd", by.y = "o_pd", all.x = T)
  pp.sum$amratio <- pp.sum$AMPKtrips/pp.sum$Dailytrips 
  pp.sum$TPurp <- names(purpose.lst)[i]    #### add column with trip purpose
  pp.sum <- merge(pp.sum, lk[, c("PD", "RegionNum", "RegionName")], by.x = "o_pd", 
                  by.y = "PD", all.x = T)     #### transfer region name and number for further segmenting

  # write file names
  assign(paste0("pp.sum.lnden.", names(purpose.lst)[i]), pp.sum)
}

#' rbind the dataframes to allow for facet type scatter plots
# create a list of dataframe
l <- list(pp.sum.lnden.H2H, pp.sum.lnden.HBE, pp.sum.lnden.HBM, pp.sum.lnden.HBO, pp.sum.lnden.HBS, 
          pp.sum.lnden.HBU, pp.sum.lnden.HBW, pp.sum.lnden.NHB, pp.sum.lnden.WBX)
  # rbind the lists using plyr
  pp.sum.all.lnden <- as.data.frame(do.call(rbind.fill, l))

#' scatter plot of amration against ratio.x
  ggplot(pp1, aes(x=lnden, y=socioden)) + geom_point(shape=1) +
    geom_smooth()
  
  
  #' Scatter Plot using purpose for faceting

ggplot(pp.sum.all.lnden, aes(x=ratio.x, y=amratio)) + geom_point(shape=1) +
  geom_smooth(method = lm) + facet_wrap( ~ TPurp, ncol=3)

#' scatter plot and regression for HBW trips only

ggplot(pp.sum.lnden.HBW, aes(x=ratio.x, y = amratio)) + geom_point(shape=1) +
  geom_smooth(method = lm) + facet_wrap(~ RegionName, ncol = 2)
  
  #' run regression
  regHBW <- lm(amratio~ratio.x, pp.sum.lnden.HBW)
    summary(regHBW)

#' Aggregate HBW and HBS mandatory tours together to mine for patterns
#' this is because these two mandatory tours are linked together by
#' start times and the fact that dropping to school is very much
#' part of the HBW tour

pp.sum.all.lnden.man <- subset(pp.sum.all.lnden, TPurp == "HBW" | TPurp == "HBS")

#' Create scatter plot by PDs for amratio vs lane/socio density ratio for mandatory trips

ggplot(pp.sum.all.lnden.man, aes(x=ratio.x, y = amratio)) + geom_point(shape=1) +
  geom_smooth(method = lm)

  #' run regression
  regMan <- lm(amratio~ratio.x, pp.sum.all.lnden.man)
    summary(regMan)


  #' attempt a K-means cluster

  # ss <- subset(pp.sum.lnden.all.man, select = c(ratio.x, amratio))
  #   k <- kmeans(ss, 7)
  #     k
  #     plot(pp.sum.lnden.HBW[c("ratio.x", "amratio")], col= k$cluster)

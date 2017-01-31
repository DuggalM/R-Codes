library(circlize)
library(dplyr)
library(reshape2)
library(DT)

#' Set working directory
wd <- setwd("c:/personal/r")

#' batch in the expansion factors for the TTS trips
#' which will then be used to develop into a pivot table
hh <- read.csv("c:/personal/r/final_household_weights.csv")

#' batch in Sonya's defined PD definitions based on Harold's sketch
pd <- read.table("EglintonPD2.txt", header = T, sep = ",", stringsAsFactors = FALSE) %>% 
  select(., c(TAZ_NO,NewName, NewNumber, NewName2, NewNumber2))
  pd$NewName[pd$NewName2=="Eglinton Crosstown LRT"] <- "Eglinton Corridor"
  pd$NewName[pd$NewName=="Eglinton Crosstown LRT"] <- "Eglinton Corridor"
  
  # reset zone 4144 to MCC
  #pd$NewName[pd$TAZ_NO==4144] <- "Mississauga Airport Corporate Centre"
  #pd$NewName2[pd$TAZ_NO==4144] <- "Mississauga Airport Corporate Centre"

#' batchin the trip tables and get rid of unwanted columns
#' basically keep the hhold numb, and orig and dest fields
#' merge the expansion factors to copy over expansion factors
pp1 <- read.table("trip11.txt", header = T, sep = ",") %>% select(., c(hhld_num, ggh_orig, ggh_dest, start_time)) %>% 
  merge(.,hh, by.x="hhld_num", by.y="HHID", all.x = T)

#' undertake a vlookup and transfer Sonya's PD definitions to make 
#' the chord diagram informative
pp[2:3] <- pd[match(as.character(unlist(pp[2:3])),     
                               as.character(pd[[1]])), 4]


#' Now create a pivot table using the information wrangled from the above steps
#' and also remove missing rows and subset data to AM peak period (600-900)
pp.sum <- subset(pp,start_time>=600 & start_time<900) %>% group_by(ggh_orig, ggh_dest) %>% summarise(trips = sum(Weight)) %>% 
  na.omit(.)

#' recast above into a matrix for chord diagram
pp.sum1 <-
  acast(pp.sum, ggh_orig ~ ggh_dest, value.trips = "z")    # reshape matrix
pp.sum1[is.na(pp.sum1)] <- 0    # set NA to zero
  #+ display data table as an interactive table using DT package
  datatable(pp.sum1)
  write.csv(pp.sum1, file="pp.sum1.csv")

#' Create Chord Diagram
c <- chordDiagram(pp.sum1,annotationTrack="grid",preAllocateTracks=list(track.height = 0.3))
##change axis
c <- c + circos.trackPlotRegion(track.index=1, panel.fun=function(x,y) {
  xlim = get.cell.meta.data("xlim") 
  ylim = get.cell.meta.data("ylim")
  sector.name=get.cell.meta.data("sector.index")
  #circos.axis(labels.cex=0.8, direction = "outside",
  #labels.away.percentage = 0.15)
  #circos.axis(h = "top", labels.cex = 0.25, major.tick.percentage = 0.25, sector.index = sector.name, track.index = 2)
  circos.text(mean(xlim), ylim[1], sector.name,facing="clockwise",
              niceFacing=TRUE,adj=c(0,0.4), cex = 0.75)},bg.border=NA)
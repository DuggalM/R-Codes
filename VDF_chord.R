library(dplyr)
library(ggplot2)
library(reshape2)
library(manipulate)
library(gridExtra)
library(circlize)
##########################################################################################

#' Title: "Marine Data Chord Diagram"
#' Author: "Mausam Duggal"
#' Date: "Oct 23rd, 2015"

#########################################################################################

#stitch("c:/personal/r/marine_chord.R", system.file("misc", "knitr-template.Rhtml", package = "knitr"))

# set working directory
wd <- setwd("c:/personal/R")

# call functionlist.R file for accessing the functions. This keeps
# the data wrangling portion of the script easy to read
# Function files used are listed below:
#
#   readfile - reads csv files
#   add.field - adds ID field 
#   summary - performs the main data summary tasks
#   balloonplot - ggplot2 code for making balloon charts
source("functionlist.R")

# reduce margins for plotting
par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.7)

# read marine summaries
vdf <- read.csv("C:/Personal/R/Links_scenario12016_nov19.csv")
#+ take out centroid connectors and non-road links
  vdf.sub <- subset(vdf, VDF!=90 & VDF!=0)
  vdf.sub$freq <- 1

# group and summarize by O-D
vdf.sub.sum <- vdf.sub %>%group_by(VDF, X.olvdf) %>%
  summarise(agg1 = sum(freq))    # prepare pivot table
vdf.sub.sum1 <- acast(vdf.sub.sum, VDF~X.olvdf, value.agg1="z")    # reshape matrix
vdf.sub.sum1[is.na(vdf.sub.sum1)] <- 0    # set NA to zero


# creat Rail Chord
c <- chordDiagram(vdf.sub.sum1,annotationTrack="grid",preAllocateTracks=list(track.height = 0.3))
##change axis
c <- c + circos.trackPlotRegion(track.index=1, panel.fun=function(x,y) {
  xlim = get.cell.meta.data("xlim") 
  ylim = get.cell.meta.data("ylim")
  sector.name=get.cell.meta.data("sector.index")
  #circos.axis(labels.cex=0.8, direction = "outside",
  #major.at=seq(0,floor(rail.sum$tons)), minor.ticks=1,
  #labels.away.percentage = 0.15)
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.25, sector.index = sector.name, track.index = 2)
  circos.text(mean(xlim), ylim[1], sector.name,facing="clockwise",
              niceFacing=TRUE,adj=c(0,0.4), cex = 0.5)},bg.border=NA)


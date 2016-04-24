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
marine <- read.csv("C:/Personal/R/MarineDataSummary.csv")

  # group and summarize by O-D
  marine.sum <- marine %>%group_by(Handling_Port, OD_Port_Country) %>%
    summarise(tons <-sum(tonnes))    # prepare pivot table
  marine.sum1 <- acast(marine.sum, Handling_Port~OD_Port_Country, value.tons="z")    # reshape matrix
  marine.sum1[is.na(marine.sum1)] <- 0    # set NA to zero

 # read rail summaries
rail <- read.csv("C:/Personal/R/Rail Traffic Summary.csv")  
  # replace empty cells with Unknown
  rail$Border_Enter<- sub("^$", "Unknown", rail$Border_Enter)
  rail$Border_Leave<- sub("^$", "Unknown", rail$Border_Leave)
  # group and summarize by O-D
    rail.sum <- rail %>% group_by(Border_Enter, Border_Leave) %>%
      summarise(tons <- sum(tonnes))    # prepare pivot table

    rail.sum1 <-
      acast(rail.sum, Border_Enter ~ Border_Leave, value.tons = "z")    # reshape matrix
    rail.sum1[is.na(rail.sum1)] <- 0    # set NA to zero

# creat Rail Chord
  c <- chordDiagram(rail.sum1,annotationTrack="grid",preAllocateTracks=list(track.height = 0.3))
  ##change axis
  c <- c + circos.trackPlotRegion(track.index=1, panel.fun=function(x,y) {
    xlim = get.cell.meta.data("xlim") 
    ylim = get.cell.meta.data("ylim")
    sector.name=get.cell.meta.data("sector.index")
    circos.text(mean(xlim), ylim[1], sector.name,facing="clockwise",
                niceFacing=TRUE,adj=c(0,0.4), cex = 0.4)},bg.border=NA)
  
  # creat Marine Chord
    c <- chordDiagram(marine.sum1,annotationTrack="grid",preAllocateTracks=list(track.height = 0.3))
    ##change axis
    c <- c + circos.trackPlotRegion(track.index=1, panel.fun=function(x,y) {
      xlim = get.cell.meta.data("xlim") 
      ylim = get.cell.meta.data("ylim")
      sector.name=get.cell.meta.data("sector.index")
      circos.text(mean(xlim), ylim[1], sector.name,facing="clockwise",
                  niceFacing=TRUE,adj=c(0,0.4), cex = 0.4)},bg.border=NA)
 

  
  
  

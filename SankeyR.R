library(dplyr)
library(ggplot2)
library(reshape2)
library(manipulate)
library(gridExtra)
library(circlize)
##########################################################################################

#' Title: "Sankey Diagram Rail Commodities"
#' Author: "Mausam Duggal"
#' Date: "Oct 23rd, 2015"

#########################################################################################

# set working directory
wd <- setwd("c:/personal/R")

# reduce margins for plotting
par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.7)

  # read rail summaries
  rail <- read.csv("C:/Personal/R/Rail Traffic Summary.csv")  
  # replace empty cells with Unknown
    rail$Border_Enter<- sub("^$", "Unknown", rail$Border_Enter)
    rail$Border_Leave<- sub("^$", "Unknown", rail$Border_Leave)
   # group and summarize by O-D
    rail.sum <- rail %>% group_by(Border_Enter, Border_Leave) %>%
      summarise(tons <- sum(tonnes))    # prepare pivot table

      # rename columns to make them compatible with Sankey plot
      rail.sum <- rename(rail.sum, target = Border_Leave)
      rail.sum <- rename(rail.sum, Source = Border_Enter)
      rail.sum <- rename(rail.sum, value = "tons <- sum(tonnes)")

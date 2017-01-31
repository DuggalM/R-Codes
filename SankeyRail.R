library(dplyr)
library(networkD3)
library(rjson)
library(data.table)
library(jsonlite)
library(knitr)
knitr::opts_chunk$set(fig.width = 15, fig.height = 15, fig.align = "left")
##########################################################################################

#' Title: "Sankey Diagram Rail Commodities"
#' Author: "Mausam Duggal"
#' Date: "Nov 7th, 2015"

#########################################################################################

#' set working directory
wd <- setwd("c:/personal/R")

#+ reduce margins for plotting
par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.7)

#' user can change this to make diagram vary by tonnage.
#' set to zero if all points to be included
user <- 0

#' read rail summaries
#' replace empty values with Unknown
#' then take the Unknown values out to make the diagram better
#' then create pivot table with tonnes summed up
#' finally rename columns to make compatible with Sankey Diagram
  rail <- read.csv("C:/Personal/R/Rail Traffic Summary.csv")  
  
  #+ Unknown
    rail$Border_Enter<- sub("^$", "Unknown", rail$Border_Enter)
    rail$Border_Leave<- sub("^$", "Unknown", rail$Border_Leave)
      # Subset
      rail <- subset(rail, Border_Enter!="Unknown")
      rail <- subset(rail, Border_Leave!="Unknown")
  #+ Pivot table
    rail.sum <- rail %>% group_by(Border_Enter, Border_Leave) %>%
      summarise(tons = sum(tonnes)) 

    #+ user input that varies visualization results
        rail.sum <- subset(rail.sum, tons>user)
  #+ rename columns 
    rail.sum <- setnames(rail.sum, old = c("Border_Leave", "Border_Enter", "tons"), new = c("target", "source", "value"))

  #' get unique names from source and target columns to make vlookup
  #' this vlookup will be used to replace the text values in rail.sum with 
  #' integers created from the Vlookup
    
    #+ unique names from source and target column 
      u.source <- data.frame(unique(rail.sum$source)) 
        names(u.source)[names(u.source)== "unique.rail.sum.source."] <- "unique"
      u.target <- data.frame(unique(rail.sum$target))
        names(u.target)[names(u.target)== "unique.rail.sum.target."] <- "unique"
        
        #' append unique values together
        #' and then get final set of unique values
        #' then populate a corressponding ID field starting from 1
        #' this is required by Sankey and also serves as a lookup
      u.all <- rbind(u.source, u.target)
        u.all <- data.frame(unique(u.all$unique)) 
          names(u.all)[names(u.all)== "unique.u.all.unique."] <- "Areas"
            u.all$id <- seq(1:nrow(u.all))    
            
          
      #' vlookup and transfer unique Ids to rail.sum
      #' transfer ID values from u.all to rail.sum using
      #' match function
      #' then deduct one from each value in the source and
      #' target column because indexing in Java starts from 0
      #' finally need to make a dataframe and create a list
      #' of two dataframes. First, rail.sum that shows flows
      #' and second of the Area names
            
        rail.sum[1:2] <- u.all[match(as.character(unlist(rail.sum[1:2])),     # vlookup
                               as.character(u.all[[1]])), 2]
          rail.sum$source = rail.sum$source-1    # subtract 1
          rail.sum$target = rail.sum$target-1    # subtract 1
      
            rail.sum1 <- data.frame(rail.sum)    # make dataframe
 
            rail.list <- list(rail.sum1, u.all)    # create list
            names(rail.list) <- c("links", "nodes")    # name list
            

#' Load package

write(toJSON(rail.list), 'jsrail')
  res <- jsonlite::fromJSON( "jsrail")

      #+ Plot
      sankeyNetwork(Links = res$links, Nodes = res$nodes, Source = "source",
                    Target = "target", Value = "value", NodeID = "Areas",
                    units = "Tonnes", fontSize = 18, nodeWidth = 50, nodePadding = 5)




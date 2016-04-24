library(dplyr)
library(networkD3)
library(rjson)
library(data.table)
library(jsonlite)
library(knitr)
knitr::opts_chunk$set(fig.width = 15, fig.height = 15, fig.show = "asis")
##########################################################################################

#' Title: "Sankey Diagram Marine Commodities"
#' Author: "Mausam Duggal"
#' Date: "Nov 7th, 2015"

#########################################################################################

#' set working directory
wd <- setwd("c:/personal/R")

#+ reduce margins for plotting
par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.2)

#' user can change this to make diagram vary by tonnage.
#' set to zero if all points to be included
user <- 100000

#' read rail summaries
#' replace empty values with Unknown
#' then take the Unknown values out to make the diagram better
#' then create pivot table with tonnes summed up
#' finally rename columns to make compatible with Sankey Diagram
marine <- read.csv("C:/Personal/R/MarineDataSummary.csv")  

  #+ Pivot table
  marine.sum <- marine %>% group_by(Handling_Port, OD_Port_Country) %>%
    summarise(tons = sum(tonnes)) 
    
      #+ user input that varies visualization results
      marine.sum <- subset(marine.sum, tons>user)

    #+ rename columns 
      marine.sum <- setnames(marine.sum, old = c("Handling_Port", "OD_Port_Country", "tons"), new = c("target", "source", "value"))

#' get unique names from source and target columns to make vlookup
#' this vlookup will be used to replace the text values in rail.sum with 
#' integers created from the Vlookup

#+ unique names from source and target column 
u.source <- data.frame(unique(marine.sum$source)) 
  names(u.source)[names(u.source)== "unique.marine.sum.source."] <- "unique"
u.target <- data.frame(unique(marine.sum$target))
  names(u.target)[names(u.target)== "unique.marine.sum.target."] <- "unique"

#+ append unique values together
#+ and then get final set of unique values
#+ then populate a corressponding ID field starting from 1
#+ this is required by Sankey and also serves as a lookup
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

marine.sum[1:2] <- u.all[match(as.character(unlist(marine.sum[1:2])),     # vlookup
                             as.character(u.all[[1]])), 2]
  marine.sum$source = marine.sum$source-1    # subtract 1
  marine.sum$target = marine.sum$target-1    # subtract 1

  marine.sum1 <- data.frame(marine.sum)    # make dataframe

marine.list <- list(marine.sum1, u.all)    # create list
  names(marine.list) <- c("links", "nodes")    # name list
  
#' Write and Read JSON file for plotting

write(toJSON( marine.list),'jsmarine')
  res <- jsonlite::fromJSON("jsmarine")

    #+ Plot
    sankeyNetwork(Links = res$links, Nodes = res$nodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "Areas",
                  units = "Tonnes", fontSize = 12, nodeWidth = 50, nodePadding = 5)




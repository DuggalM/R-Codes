
library(dplyr)
library(ggplot2)
library(reshape2)
library(manipulate)
library(gridExtra)
#library(fBasics)

setwd("c:/personal/R")

####
#### create functions to be used in the code
####

readfile <- function(directory){
  # create readile function
  #
  # Args:
  #   directory : csv file with path to be batched in
  #
  # Returns: 
  #   file
  data <- read.csv(directory,header=TRUE,
                   quote="\"",
                   stringsAsFactors= TRUE,
                   strip.white = TRUE)
  return(data)
}

add.field <- function(file) {
  # create function for adding ID field for merging
  #
  # Args:
  #  file: the counts file from EMME. Either the GGHV3 or Unity matrix
  #
  # Returns:
  #   new field in file  
  m1 <- mutate(file, ID = paste(i, j, sep = '-'))
  return(m1)
}

summary <- function(file, master){
  # create function for summarizing and plotting data
  #
  # Args:
  #  file: the counts file from EMME. Either the GGHV3 or Unity matrix
  #  master: the file that contains all the CDUID, CDNAME, CLASS info for
  #          all links in the EMME network, excluding centroid connectors
  #
  # Returns: a list of all the handles within the function
  
  f <- add.field(file)  # create unique ID column using add.field function
  
  mer <- merge(f, master, by ="ID")  # join file to master 
  t1 <- mer%>% group_by(CDNAME, CLASS) %>% 
    summarise(Join_Count=sum(Join_Count))   # prepare pivot table
  t2 <- acast(t1, CDNAME~CLASS, value.Join_Count="z")  # reshape to matrix
  t2[is.na(t2)] <-0  # set NA to zero
  #t2
  t3 <- melt(t2, value.name="count")  # rehape to long format for bubble diagram
  colnames(t3)[colnames(t3)=="Var1"] <- "Region"  # rename columns
  colnames(t3)[colnames(t3)=="Var2"] <- "Class"   # rename columns
  
  t4 <- mer %>% group_by(CDNAME, vdf) %>%
    summarise(cnts=sum(Join_Count),lane.kms=sum(length))   # prepare pivot table for lanekms and counts
  #t4 <- acast(t4, CDNAME~vdf, value.Join_Count='z')
  
  list(f, mer, t1, t2, t3, t4)
}

balloonplot <- function(data) {
  # create ballon plot function
  #
  # Args:
  #  data: balloon data that is represented by t3 from the summary function
  #
  # Returns: balloon plot
  x<- ggplot (data, aes(x=Class, y= Region))+
    geom_point(aes(size=count), shape=21, color="grey30", fill="cornsilk")+
    scale_size_area(max_size=15, guide=FALSE)+
    geom_text(aes(y=as.numeric(Region)-sqrt(count*70)/220, label=count), vjust=1,
              color="grey60", size=2.5)
  print(x)
}

readshp <- function(dir, lay) {
  shp <- readOGR(dsn = dir, layer = lay)
  return(shp)}



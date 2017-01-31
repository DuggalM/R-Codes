#install.packages("dplyr")

library(dplyr)
library(ggplot2)
library(reshape2)
library(manipulate)


#### Metadata

#' Title: "Count Frequency comparison by CD"
#' Author: "Mausam Duggal"
#' Date: "Sept 16th, 2015"

# set working directory to personal directory, for convenience

setwd("c:/personal/R")

#### read text file. This file contains a join_count fiel
#### a field value of 1 indicates that a count has been geocoded to this field
#### in Waterloo, Toronto, Hamilton, and Brantford more than one link gets tagged when
#### a point to line spatial join is undertaken because points rarely align and some fall on an intersection

PTsc1 <- read.csv("C:/Personal/R/EMME_links_countlocations_1.csv", header=TRUE,
                  quote="\"",
                  stringsAsFactors= TRUE,
                  strip.white = TRUE)


###### prepare pivot table, by summarizing across CD's and VDF
###### t1...t3 represents temporary variables

t1 <- PTsc1 %>% group_by(CDNAME, CLASS)%>% summarise(Join_Count=sum(Join_Count))

#### reshape to matrix

t2 <- acast(t1, CDNAME~CLASS, value.Join_Count="z")

#### set NA to zero

t2[is.na(t2)] <-0
t2

write.table(t2, "c:/personal/t.txt", sep="\t")

library(reshape2)
t3 <- melt(t2, value.name="count")

#### Balloon plot

ggplot (t3, aes(x=Var2, y= Var1))+
  geom_point(aes(size=count), shape=21, color="grey30", fill="cornsilk")+
  scale_size_area(max_size=18, guide=FALSE)+
  geom_text(aes(y=as.numeric(Var1)-sqrt(count*70)/250, label=count), vjust=1,
            color="grey60", size=3)

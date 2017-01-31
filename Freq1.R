#install.packages("dplyr")

library(dplyr)
library(ggplot2)
library(reshape2)
library(manipulate)
library(gridExtra)


#### Metadata

#' Title: "Count Frequency comparison by CD"
#' Author: "Mausam Duggal"
#' Date: "Sept 16th, 2015"

# set working directory to personal directory, for convenience

setwd("c:/personal/R")

##############################################################
#### read csv files function
##############################################################

readfile <- function(directory){
  data <- read.csv(directory,header=TRUE,
                   quote="\"",
                   stringsAsFactors= TRUE,
                   strip.white = TRUE)
  return(data)
}

####################################################################
#### read MasterJoin1.csv file. This file contains CDUID, CDNAME, CLASS, 
#### Join_count variables 
#### a field value of 1 in Join_Count is used to summarize frequency
##################################################################


PTsc1 <- readfile("C:/Personal/R/MasterJoin1.csv")

##################################################################
#### Read CountPost file created by running a Unity (one's) matrix
##################################################################

PTsc2 <- readfile("C:/Personal/R/AllCountPosts_UnityMatrix.csv")

#### create unique ID column in PTsc2

PTsc2 = mutate(PTsc2, ID = paste(i, j, sep = '-'))

#### join PTsc1 to PTsc2 to transfer CDUID, CDNAME, CLASS variables

PTsc3 <- merge(PTsc2, PTsc1, by ="ID")


#### prepare pivot table, by summarizing across CD's and VDF
#### t1...t3 represents temporary variables

t1 <- PTsc3 %>% group_by(CDNAME, CLASS)%>% summarise(Join_Count=sum(Join_Count))

#### reshape to matrix

t2 <- acast(t1, CDNAME~CLASS, value.Join_Count="z")

#### set NA to zero

t2[is.na(t2)] <-0
t2

write.table(t2, "c:/personal/countposts_Unity_matrixt.txt", sep="\t")

#### reshape to long format

t3 <- melt(t2, value.name="count")

#### rename columns
colnames(t3)[colnames(t3)=="Var1"] <- "Region"
colnames(t3)[colnames(t3)=="Var2"] <- "Class"

#### Balloon plot

g1<-ggplot (t3, aes(x=Class, y= Region))+
  geom_point(aes(size=count), shape=21, color="grey30", fill="cornsilk")+
  scale_size_area(max_size=18, guide=FALSE)+
  geom_text(aes(y=as.numeric(Region)-sqrt(count*70)/250, label=count), vjust=1,
            color="grey60", size=3)


##################################################################
#### Read CountPost file created by running GGHV3 AD matrix
##################################################################

PTsc4 <- readfile ("C:/Personal/R/AllCountPosts_GGHV3.csv")

#### create unique ID column in PTsc4

PTsc4 = mutate(PTsc4, ID = paste(i, j, sep = '-'))

#### join PTsc4 to PTsc1 to transfer CDUID, CDNAME, CLASS variables

PTsc5 <- merge(PTsc4, PTsc1, by ="ID")


#### prepare pivot table, by summarizing across CD's and VDF
#### t4...t6 represents temporary variables

t4 <- PTsc5 %>% group_by(CDNAME, CLASS)%>% summarise(Join_Count=sum(Join_Count))

#### reshape to matrix

t5 <- acast(t4, CDNAME~CLASS, value.Join_Count="z")

#### set NA to zero

t5[is.na(t5)] <-0
t5

write.table(t5, "c:/personal/countposts_GGHV3_matrixt.txt", sep="\t")

#### reshape to long format

t6 <- melt(t5, value.name="count")

#### rename columns
colnames(t6)[colnames(t6)=="Var1"] <- "Region"
colnames(t6)[colnames(t6)=="Var2"] <- "Class"

#### Balloon plot

g2<-ggplot (t6, aes(x=Class, y= Region))+
  geom_point(aes(size=count), shape=21, color="grey30", fill="cornsilk")+
  scale_size_area(max_size=18, guide=FALSE)+
  geom_text(aes(y=as.numeric(Region)-sqrt(count*70)/250, label=count), vjust=1,
            color="grey60", size=3)


###############################################################################
#### Plot multiple bubble diagrams for visualization using gridExtra library
###############################################################################

grid.arrange(g1, g2,ncol=2)

###############################################################################
#### generate scatter plot of volumes using the Unity and GGHV3 matrices
###############################################################################

library(dplyr)
library(ggplot2)

#' set working directory
wd <- setwd("c:/personal/R")

data <- read.csv("c:/personal/R/MTO Data.csv", header=TRUE, sep=",")

data1 <- arrange(data, Avg_Am_Au)
data1$ID1 <- seq(1,223)

ggplot(data1,aes(x=as.factor(ID1))) +
  geom_boxplot(aes(lower = Min_Am_Au, upper = Max_Am_Au, middle = Avg_Am_Au, 
                   ymin = Min_Am_Au, ymax = Max_Am_Au), stat="identity")




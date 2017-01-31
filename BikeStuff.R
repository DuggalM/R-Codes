library(dplyr) 
library(ggplot2)
library(googleVis)

#' Title: "Some interesting trends and models of bike sharing from the 
#' UCI Dataset"
#' Author: "Mausam Duggal"
#' Date: "May 11th, 2016"

# set working directory to personal directory, for convenience

wd <- setwd("c:/personal/r")

#' load the day and hour rental files

day <- read.csv(file = paste0(wd, "/day.csv"), stringsAsFactors = FALSE)
hour <- read.csv(file = paste0(wd, "/hour.csv"), stringsAsFactors = FALSE)


day1 <- subset(day1, select = c(instant, season, weekday, mnth, temp, atemp, hum,
                                windspeed, casual, registered, cnt))

r1 <- gvisMotionChart(day1, "season", "instant")
plot(r1)

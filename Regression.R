library(dplyr)
library(ggplot2)
library(reshape2)
library(manipulate)
library(gridExtra)
library("GGally")
#library(fBasics)

############################## Metadata ########################################

#' Title: "Multiple Regression Sketch Model"
#' Author: "Mausam Duggal"
#' Date: "Oct 8th, 2015"

# set working directory to personal directory, for convenience
setwd("c:/personal/R")

# call functionlist.R file for accessing the functions. This keeps
# the data wrangling portion of the script easy to read
# Function files used are listed below:
#
#   readfile - reads csv files
#   add.field - adds ID field 
#   summary - performs the main data summary tasks
#   balloonplot - ggplot2 code for making balloon charts
source("functionlist.R")

############################## Read Data ########################################

input <- readfile("C:/Personal/R/Edmonton.csv")
  # drop dummy variables to make a meaningful correlation
  input1 <- subset(input, select = -c(Terminal, Park_Ride, Feeder_Bus, Year))

# create pairwise correlation
  #ggpairs(input1, lower=list(continuous="smooth", params=c(colour= "blue")),
  #        diag=list(continuous="bar", params=c(colour="blue")), 
  #       upper=list(params=list(corSize=18)), axisLabels='show')

ggscatmat(input1)
############################## Run Regression####################################

sketch = lm(BoardingTot ~ Park_Spaces, data = input)
plot(sketch)
  coeffs = coefficients(sketch) 


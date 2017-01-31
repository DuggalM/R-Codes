library(fitdistrplus)
library(gumbel)

wd <- setwd("C:/PERSONAL/R")

DAT <- read.csv("timefreq.csv")

par(mar = rep(2, 4))


serving <- DAT$Freq

dgumbel <- function(x,a,b) 1/b*exp((a-x)/b)*exp(-exp((a-x)/b))
pgumbel <- function(q,a,b) exp(-exp((a-q)/b))
qgumbel <- function(p,a,b) a-b*log(-log(p))

fitgumbel <- fitdist(serving, "gumbel", start=list(a=5, b=10))
summary(fitgumbel)
plot(fitgumbel)


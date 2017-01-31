library(dplyr)
library(reshape2)
library(forecast)
library(ggplot2)

skim1_long <- melt (skim, id.vars = "p.q..val.") %>%
  setNames(., c("Origin", "Dest", "Ttime")) %>% .[order(.$Origin), ]

nozero <- subset(skim1_long, Ttime != 0)

sum <- function(df, field, bins){
  ### This function estimates the mean, median, sd, and 
  ### plots the histogram.

  m <- mean(df[[field]])
  med <- median(df[[field]])
  s <- sd(df[[field]])
  
  # save plot
    #plots <- hist(df[[field]], bins, col = 'black')
  plots <- ggplot(df, aes(x = df[[field]])) +
    geom_histogram(binwidth = 100)
  
    return(list(m, med, s, plots))
}


l180 <- subset(skim1_long, Ttime <= 180)

n_zero_sum <- sum(nozero, "Ttime", 100)

n_zero_sum

mean(l180$Ttime)
median(l180$Ttime)

nozero$Ttime_Sq <- sqrt(nozero$Ttime)
hist(nozero$Ttime, 100, col = 'black')


nozero1 <- nozero[1:10000000,]

lambda <- BoxCox.lambda(nozero1$Ttime)
l <- -0.2
trans_vec <- as.data.frame(BoxCox(nozero1$Ttime, l)) 
  colnames(trans_vec)[1] <- "Ttime"

hist(trans_vec$Ttime, 100, col = 'black')

mean(trans_vec$`BoxCox(nozero1$Ttime, lambda)`)
median(trans_vec$`BoxCox(nozero1$Ttime, lambda)`)
sd(trans_vec$`BoxCox(nozero1$Ttime, lambda)`)

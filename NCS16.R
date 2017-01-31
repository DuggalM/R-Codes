library(plotly)
library(plyr)
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
knitr::opts_chunk$set(fig.width = 30, fig.height = 30, fig.show = "asis")

#' Title: "NCS16"
#' Author: "Mausam Duggal"
#' Date: "Nov 11th, 2015"

#' set working directory and batch in data
wd <- setwd("c:/personal/r")

#' This reflects the 2006 VDF definitions
#' and encompasses the entire GGHM network

data <- read.csv("Links_scenario12014_nov11.csv")
  #+ take out centroid connectors and non-road links
  data1.sub <- subset(data, VDF!=90 & VDF!=0)


#' Plot interactive 3-D scatter plot
  plot_ly(data1.sub, x = VDF, y = ul3, z=ul2, text = paste("Density: ", Density_km2),
          type="scatter3d", mode="markers", filename="c:/personal/r")

  #' plot interactive facet scatter plots  
  p <- ggplot(data = data1.sub, aes(x = ul3, y = ul2, color = factor(Orig))) +
    geom_point(aes(text = paste("Density:", Density_km2)), size = 3) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black', size=10)) +
    theme(axis.title = element_text(size = 25)) +
     facet_wrap(~ Orig, nrow=8) 
  
  (gg <- ggplotly(p))
  
#   ggplot(data = data.sub, aes(x = ul3, y = ul2)) +
#     geom_point(aes(text = paste("Density:", Density_km2)), size = 4) +
#     facet_wrap(~ VDF, nrow=8)

  #volcano plot
  ggplot(data1.sub, aes(x = ul3)) +
    stat_density(aes(ymax = ..count..,  ymin = -..count..,
                     fill = VDF, color = VDF),
                 geom = "ribbon", position = "identity") +
    facet_grid(. ~ VDF) +
    coord_flip() +
    theme(legend.position = "none")
  
  # #' Plot interactive 3-D scatter plot
  #   plot_ly(data.sub, x = VDF, y = ul3, z=ul2, text = paste("Density: ", Density_km2),
  #           type="scatter3d", mode="markers", filename="c:/personal/r")
  
  
  #' This reflects the TMG VDF definitions
  #' and encompasses the only the GTHA network
  
  data2.sub <- subset(data1.sub, From<=70000)

    #' plot interactive facet scatter plots  
  p <- ggplot(data = data2.sub, aes(x = ul3, y = ul2, color = factor(TMG))) +
    geom_point(aes(text = paste("Density:", Density_km2)), size = 3) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=.5,colour='black', size=10)) +
    theme(axis.text.y=element_text(size=15))+
    theme(axis.title = element_text(size = 25)) +
    facet_wrap(~ TMG, nrow=8) 
  
  (gg <- ggplotly(p))

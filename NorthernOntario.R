library(raster)
library(sp)
library(rgdal)
library(dplyr)

#######################################################################

#' set working directory and identify name of image file

wd <- setwd("c:/personal/r")

par(mai=c(1.02,0.82,0.82,0.42))

image.lst <- c(1000, 1001, 1002, 1003, 1004, 1005)

#' Specify out raster with "filename" argument also test the pixel value (pix).
#' Pixel values can be adjusted for each TIF tile specifically. 
#' identify variable (val) to encode the pixels greater than a certain value
val <- 1200
pix <- c(20, 20, 20, 20, 20, 20)

no.temp <- raster("Ontario1003.tif")
plot(no.temp)


pxy <- locator(1) # click on plot where you want to query pixel value
bb <- as.data.frame(extract(no.temp, cbind(pxy$x, pxy$y)))  # returns value associated with click query

# start loop

for (i in 1: length(image.lst)){
  
  # call each image
  image.name <- paste0("Ontario", image.lst[i],".tif")

#######################################################################

#' load Tif as raster image
no <- raster(image.name)
  plot(no)    # plot image

#' use the locator function to identify the pixel value to use in the 
#' calculator function below
# 
     #pxy <- locator(1) # click on plot where you want to query pixel value
    #extract(no, cbind(pxy$x, pxy$y))) # returns value associated with click query

r05 <- calc(no, fun=function(x) { x[x >pix[i]] <- val; return(x) })
  plot(r05)

#' write the raster to points while subsetting for 

r2.points <- rasterToPoints(r05) %>% as.data.frame(.) %>% 
  subset(., .$PixVal == val)

  # write points out to CSV
  write.csv(r2.points, file = paste0("Ontario", image.lst[i], "_", pix[i], ".csv"))


}


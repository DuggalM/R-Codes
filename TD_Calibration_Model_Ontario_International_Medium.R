library(dplyr)
library(reshape2)
library(tidyr)
library(foreign)

###############################################################################
#' Title: "Calibrating a Doubly Constrained Gravity Model - International"
#' Programmer: "Mausam Duggal, Systems Analysis Group, WSP|PB"
#' Date: "January 11, 2017"

###############################################################################
#' This code was written to calibrate the beta value in a doubly constrained 
#' for the empty truck model. The data used was the 2012 CVS data. Bryce 
#' prepared the empty truck matrices for both heavies and medium trucks. 
#' The trucks have been currently combined because the commodity flow model is
#' not currently distinguishing between heavy and medium truck tonnage. Further,
#' while Rolf's Empty Truck model can be developed for different truck types it
#' was not deemed necessary in this version of the model.
###############################################################################

########################### INPUTS ############################################
#' read observed truck trip matrix and combine
obs_med <- read.csv("C:/Projects/Province-wide/Empties/empty_straight_truck_matrix_int.csv", 
                stringsAsFactors = FALSE)

#' Combine all empties and also cast it in a format desired and getting rid of 
#' intraCSD trips.
all_obs <- transform(obs_med, Flag = paste0(Orig,Dest)) %>% 
  subset(., Orig != Dest)
#' wide format and set NAs to zero
all_obs_cast <- dcast(all_obs, Orig ~ Dest, value.var = "Trips")
rownames(all_obs_cast) <- all_obs_cast[, 1]
all_obs_cast <- all_obs_cast[, 2:ncol(all_obs_cast)]
all_obs_cast[is.na(all_obs_cast)] <- 0
#' get total trips
totals <- sum(apply(all_obs_cast, 1, function(x) sum(x)))

#' batch in distance skims and cast it in a format desired. There is also a need
#' to only keep distance skims for those O-D pairs that are in the combined trips.
#' The diagonal value of the distance matrix is set to 0 to avoid making that 
#' cell receive any values during balancing 
dist <- read.csv("C:/Projects/Province-wide/Empties/od_road_distances_all.csv", 
                 stringsAsFactors = FALSE) %>% 
  transform(., Flag = paste0(Orig, Dest))
#' only keep those o-D pairs that match the all_obs truck trip table
dist1 <- subset(dist, dist$Flag %in% all_obs$Flag)
#' wide format and set NAs to zero
dist1_cast <- dcast(dist1, Orig ~ Dest, value.var = "Dist")
rownames(dist1_cast) <- dist1_cast[, 1]
dist1_cast <- dist1_cast[, 2:ncol(dist1_cast)]
dist1_cast[is.na(dist1_cast)] <- 0
#' set diagonal to global value of 10
#diag(dist1_cast) <- 0

####################### COST MATRIX ###########################################
#' generate a cost matrix using distance between CSDs and set the beta to -0.01
#' Rolf uses a very small beta of -0.001. This will have a tendency to require 
#' longer to calibrate and also it does not create enough variation in the 
#' value of the various interchanges, especially those that are far apart. 
cost <- dist1_cast
initbeta <- -0.01
################ COST FUNCTION MATRIX #########################################
#' Cost Function Matrix. We are using an exponential function here. Other formulations
#' can be tested as well, if desired. All those cells that had a value of 0
#' would automatically get set to 1 after taking the exponent. This would 
#' incorrectly make these interchanges a candidate to receive trips thereby
#' making it difficult to calibrate and converge the betas. These cells are
#' forced to zero. 
cfunc <- exp(initbeta*cost)
cfunc[cfunc == 1] <- 0
#' Estimate observed average trip lengths
tlfd_obs <- sum(dist1_cast * all_obs_cast)/totals
print(tlfd_obs)
############################ FURNESSING ##############################
#' This is the furnessing function, which is the backbone of the calibration 
#' procedure. 
furness <- function(cmat){
  
  "
    This function conducts furnessing or two dimensional balancing

    Inputs: observed truck matrix

    Arguments: cost function

    Results: balanced matrix
    "
  
  #' row sum of cost function matrix
  skim_trow <- as.data.frame(t(colSums(cmat)))
  #' row sum of observed matrix
  obs_tatt <- as.data.frame(t(colSums(all_obs_cast)))
  
  #' row balancing factors
  rowbal <- obs_tatt/skim_trow
  rowbal1 <- do.call("rbind", replicate(nrow(all_obs_cast), rowbal, 
                                        simplify = FALSE))
  
  #first iteration
  cfunc1 <- cmat*rowbal1
  
  #' col sum of cost function matrix
  skim_tcol <- as.data.frame(rowSums(cfunc1))
  #' col sum of observed matrix
  obs_tprod <- as.data.frame(rowSums(all_obs_cast))
  
  #' column balancing factors
  colbal <- obs_tprod/skim_tcol
  colbal1 <- do.call("cbind", replicate(ncol(all_obs_cast), colbal, 
                                        simplify = FALSE))
  
  # next iteration
  cfunc1 <- cfunc1*colbal1
  
  return(cfunc1)
  
}

############################### CALIBRATION PROCESS ###########################
#' the outer loop is to calculate revised beta values. The user can set the 
#' number of outer loops one wants to evaluate.
outer_iter <- 30

#' Start outer loop
for (j in 1:outer_iter) {
  ################## BETA COEFFICIENTS ########################################
  #' set initial beta coefficient and cost matrix
  if(j == 1){
    beta <- initbeta
    print(beta)
    #' compute cfunc again
    cfunc <- exp(initbeta*cost)
    cfunc[cfunc == 1] <- 0
  } else {
    beta <- beta1
    print(beta1)
    #' compute cfunc again
    cfunc <- exp(beta*cost)
    cfunc[cfunc == 1] <- 0
  }
  
  
  #' run the loop with user defined iterations
  maxiter <- 15
  i <- 1
  
  #' run the first iteration of furnessing
  for (i in 1:1){
    
    t <- furness(cfunc)
  }
  #' now loop furnessing through till the maxiterations
  for (i in 2:maxiter){
    t1 <- furness(t)
    t <- t1
    #print(i)
  }
  
  ###############################################################################
  #' Estimate simulated average trip lengths
  tlfd_sim <- sum(dist1_cast * t1)/totals
  #' calculate revised beta value to feed back to outer loop
  if (j == 1){
    beta1 <- initbeta * (tlfd_sim/tlfd_obs)
  } else {
    beta1 <- beta1 * (tlfd_sim/tlfd_obs)
  }
  
}


write.csv(t, "/Projects/Province-wide/Empties/t.csv") 

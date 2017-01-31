library(dplyr)

library(gaussfacts)
library(rmsfact)

gaussfact()
rmsfact()

#' Set working directory
wd <- setwd("c:/personal/r")

###############################################################################

#' read in the nhs file
nhs <- read.csv("2011NHS.csv", stringsAsFactors = FALSE)
pp <- read.csv("persons.csv", stringsAsFactors = FALSE)
hhold <- read.csv("households.csv", stringsAsFactors = FALSE)

#' filter out records that correspond to those belonging to Ontario
on <- nhs[c(19844:40115), ] 

#' only keep the income columns
on1 <- on[c(1, 2231:2621)]

#' only keep records with DA numbers. This will get rid of sub-totals by various
#' sub-level geographies
on2 <- filter(on1, grepl("^[35]",Geography))


#' All the columns come in as characters. So, we need to convert them back to 
#' numeric for undertaking analysis

# create vector of column names to apply the conversion to
cols <- colnames(on2)
cols1 <- cols[-1]    # get rid of the first element in the column vector

#' conver to numeric
on2[,cols1]<-lapply(cols1, function(x) as.numeric(as.character(on2[,x])))
on2[is.na(on2)] <- 0

cs <- c('Less50k', '50-80k', '80-100k', '>100k')

# get columns for estimating annual income
on2_t <- on2[c(5:17)]

# get column sums
on2_sum <- as.data.frame(t(colSums(on2_t, na.rm=TRUE)))

# create data frame to receive results
on3_sum <- data.frame(matrix(NA_real_, ncol=4, nrow=3))
# set column names
colnames(on3_sum) <- cs

# populate the cells
on3_sum[1,1] <- on2_sum %>% .[, c(1:7)] %>% rowSums(na.rm = TRUE)

on3_sum[1,2] <- on2_sum %>% .[, c(8:9)] %>% rowSums(na.rm = TRUE)

on3_sum[1,3] <- on2_sum[,10]

on3_sum[1,4] <- on2_sum %>% .[, c(11:13)] %>% rowSums(na.rm = TRUE)

# Now populate the values from the pop file

pp1 <- left_join(pp, hhold, by.x = "hhid", by.y = "hhid") %>% 
  subset(., age>14) %>% subset(., select = c(hhid, hhinc))

pp1 <- transform(pp1, incat = ifelse(hhinc < 50001, 1,
                                     ifelse(hhinc > 50000 & hhinc < 80001, 2,
                                            ifelse(hhinc > 80000 & hhinc < 100001, 3, 4))))

pp1.sum <- pp1 %>% group_by(incat) %>% summarise(cnt = n())

for (i in 1:4){
  on3_sum[2,i] <- pp1.sum[i,2]
}




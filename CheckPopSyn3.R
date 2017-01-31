library(dplyr)
library(ggplot2)
library(reshape2)

#############################################################################################
#' Title: "Check PopSyn3 performance against CSD totals"
#############################################################################################
# set working directory

wd <- setwd("c:/personal/r")

##############################################################################################
#' batch in PopSyn3 Pop and HHold Outputs and CSD level Inputs
#' for comparing popsyn3 accuracy

pop3 <- read.csv(file = "synpop_person_with_required_fields.csv", stringsAsFactors = FALSE)
hh3 <- read.csv(file = "synpop_hh.csv", stringsAsFactors = FALSE)

# read control total file
csdin <- read.csv(file = "tazData.csv", stringsAsFactors = FALSE)

##############################################################################################
#' Summarize control data files
csdin.sum <- csdin %>% group_by(csduid)%>% 
  summarise(hhin=sum(tothh), popin=sum(totpop))

#' summarize Popsyn3 Population data
pop3.sum <- pop3 %>% group_by(taz) %>%
  summarise(popl3 = sum(finalweight))

#' summarize and count Popsyn3 Household data
hh3.sum <- hh3 %>% group_by(taz) %>%
  summarise(hh3 = sum(finalweight))


#' Now join the data to gauge differences in Popsyn3 outputs and inputs
#' The GGH area does not have any corressponding CSD IDs so it is removed
joined <- merge(csdin.sum,pop3.sum, by.x="csduid", by.y="taz", all.x = T) %>% 
  subset(., csduid!=0)

#now join the housing data
joined <- merge(joined, hh3.sum, by.x="csduid", by.y="taz", all.x = T)

##############################################################################################
#' estimate differences between input and output fields
joined$popdiff <- joined$popin-joined$popl3
joined$hhdiff <- joined$hhin-joined$hh3

  joined <- subset(joined, joined$popin !=0)

  sum(joined$popdiff)
  sum(joined$hhdiff)

# plot the differences of households
ggplot(joined, aes(x=csduid, y=hhdiff))+geom_line(color="grey")+geom_point(color="red")+
  ggtitle("Household Differences (Input Households - PopSyn3 Households)")

# plot the differences of population
ggplot(joined, aes(x=csduid, y=popdiff))+geom_line(color="grey")+geom_point(color="red")+
  ggtitle("Population Differences (Input Population - PopSyn3 Population)")


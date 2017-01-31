library(rgeos)

#' batch in the 2011NHS semi-custom tabulation
nhs <- read.csv("2011NHS.csv")

nhs3 <- nhs[, c(1, 2555:2621)]
cc <- as.data.frame(colnames(nhs3))

#' Only Ontario DAs
nhs4 <- subset(nhs3,grepl("^35", Geography)) %>% subset(.,Household.total.income.in.2010.of.private.households != "x")
nhs4$Geography <- lapply(nhs4$Geography, as.character)
nhs4$Geography <- substr(nhs4$Geography,1,nchar(nhs4$Geography)-8)

#' set factors to numeric
indx <- sapply(nhs4, is.factor)
nhs4[indx] <- lapply(nhs4[indx], function(x) as.numeric(as.character(x)))

indx1 <- sapply(nhs4, is.character)
nhs4[indx1] <- lapply(nhs4[indx1], function(x) as.numeric(x)) 

#' batch in the DA shapefile to get the DAs within the GGH
da_pol <- "DA" 
da_poly <- readOGR("c:/personal/r", da_pol)

da_df <- da_poly@data %>% subset(., select = c(DAUID, GGH)) %>% subset(., GGH == 1)

#'merge the GGH DAs to the NHS
nhs5 <- merge(nhs4, da_df, by.x = "Geography", by.y = "DAUID", all.y = T)
nhs5[is.na(nhs5)] <- 0

nhs5$Tothholds <- rowSums(nhs5[3:15])
sum(nhs5$Tothholds)

# hholds below 60k
fi <- sum(nhs5$X..Under..5.000)
se <- sum(nhs5$X...5.000.to..9.999)
th <- sum(nhs5$X...10.000.to..14.999)
fo <- sum(nhs5$X...15.000.to..19.999)
fif <- sum(nhs5$X...20.000.to..29.999)
si <- sum(nhs5$X...30.000.to..39.999)
sev <- sum(nhs5$X...40.000.to..49.999)
ei <- sum(nhs5$X...50.000.to..59.999)

# below 60k average income
l_inc_avg <- (fi*2500 + se*7500 + th*12500 + fo*17500 + fif*25000 + si*35000 + sev*45000 + ei*55000)/(fi + se + th + fo + fif + si + sev + ei)

# hholds above 60k
ni <- sum(nhs5$X...60.000.to..79.999)
ten <- sum(nhs5$X...80.000.to..99.999)
ele <- sum(nhs5$X...100.000.to..124.999)
twe <- sum(nhs5$X...125.000.to..149.999)
thi <- sum(nhs5$X...150.000.and.over)

# above 60k
# A statsCAN link showed that there were nearly 600k individuals that made between 200-250k and over 250k; and around 700k individuals that made between 150-200k.
# So, I took the midpoint of 150-250, at 200k for the weighted average
h_inc_avg <- (ni*70000 + ten*90000 + ele*112500 + twe*137500 + thi*200000)/(ni + ten + ele + twe + thi)

#' total average across hholds
all_inc_avg <- (fi*2500 + se*7500 + th*12500 + fo*17500 + fif*25000 + si*35000 + sev*45000 + ei*55000 +
                  ni*70000 + ten*90000 + ele*112500 + twe*137500 + thi*200000)/ 3172470 #(fi + se + th + fo + fif + si + sev + ei + ni + ten + ele + twe + thi)

inc <- mean(nhs5$Average.household.total.income..)

low_hholds <- fi + se + th + fo + fif + si + sev + ei
high_hholds <- ni + ten + ele + twe + thi

View(nhs5)

sum(nhs5$Household.total.income.in.2010.of.private.households)

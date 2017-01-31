# This is a first shot at generating a provincial model zone system based upon
# Census geography. It uses a threshold to flag Census areas that fall below it,
# which are made unique zones. The logic is twisted because the hierarchy
# differs depending upon whether you're within a CMA or not. For areas outside
# of CMA we're forced to use dissemination areas except in cases where the 
# threshold isn't met at the subdivision level. I was bummed about this at
# first, but then realized that the land area in each of these DAs outside of
# the CMAs are probably still pretty large. 
library(foreign); library(data.table); library(dplyr)
msg <- function(s) print(s, quote=FALSE)

# Set the threshold a Census area needs to exceed in order to be further divided
THRESHOLD <- 3500  # dwellings per zone

# Start by reading the database from the dissemination areas (DA) shapefile that
# Gurbani found on the Statistics Canada website. The tract, subdivision,
# division and other Census areas are coded for each DA. 
geography <- read.dbf("DA shapefile/DA.dbf", as.is=TRUE) %>%
    mutate(DAUID = as.character(DAUID), CDUID = as.character(CDUID),
        CSDUID = as.character(CSDUID), CTUID = as.character(CTUID),
        CMAUID = as.character(CMAUID)) %>%
    select(DAUID, CTUID, CSDUID, CMAUID, CDUID) %>%
    arrange(DAUID)
msg(paste(nrow(geography), "DA records found"))

# Unfortunately there are no demographic data attached to the shapefile, so we
# will read a different database to glean that info by DA, and merge it with
# the shapefile database.
FN <- "Dissemination areas/98-316-XWE2011001-1501-ONT.csv"
demographics <- fread(FN) %>%
    filter(Prov_name=="Ontario", Topic=="Population and dwelling counts",
        Characteristic=="Total private dwellings") %>%
    mutate(Dwellings = Total, DAUID = as.character(Geo_Code)) %>%
    select(DAUID, Dwellings) %>%
    arrange(DAUID)
msg(paste(nrow(demographics), "total private dwelling records read"))
combined <- merge(geography, demographics, by="DAUID", all=TRUE)

# There are some cases where the number of dwellings (and presumably other 
# demographic characteristics) are masked. These are likely DAs with small 
# enough level of activity that the data are masked for privacy purposes. So 
# replace missing value with one, which will make it insignificant.
combined$Dwellings[is.na(combined$Dwellings)] <- 1


# EVALUATE CENSUS SUBDIVISIONS
# We'll start by looking for subdivisions that fall below our threshold. To do
# this sum the dwellings for each subdivision.
csd <- combined %>%
    group_by(CSDUID) %>%
    summarise(Dwellings = sum(Dwellings, na.rm=TRUE)) %>%
    mutate(Criteria = ifelse(Dwellings<THRESHOLD, "CSD below", "Retain"))
n_below <- nrow(filter(csd, Criteria=="CSD below"))
msg(paste(n_below, "of", nrow(csd), "subdivisions below threshold"))

# Now we've identified which CSDs are below the threshold, so tag all DAs within
# those CSDs with a temporary zone number.
mole <- sort(unique(csd$CSDUID[csd$Criteria=="CSD below"]))
tagged <- data.table(CSDUID = mole, zed = 1:length(mole), Criteria = "CSD below")
zones_so_far <- max(tagged$zed)

# If we merge this with the original data table (of all dissemination areas)
# those that don't match (i.e., didn't fall below threshold) will have missing
# values for the temporary zone number (zed). So we'll be able to pull those
# records that don't have missing zed into the results, and continue processing
# those left over.
remainder <- merge(combined, tagged, by="CSDUID", all.x=TRUE)
keep <- filter(remainder, !is.na(zed)) 
remainder <- filter(remainder, is.na(zed)) %>% select(-zed, -Criteria)

# PULL OUT DISSEMINATION AREAS OUTSIDE OF CMA
# At this point we're left with tracts and dissemination areas. But the former
# only exist within metropolitan areas. Thus, the dissemination areas outside
# of tracts by definition will be zones, because we have no way to split them
# down further, or aggregate them up to something analagous to tracts.
remainder$Criteria <- ifelse(is.na(remainder$CTUID), "DA outside CMA", "Retain")
mole <- sort(unique(remainder$DAUID[remainder$Criteria=="DA outside CMA"]))
tagged <- data.table(DAUID = mole, zed = (1:length(mole))+zones_so_far)
zones_so_far <- max(tagged$zed)

# Merge these tagged DA with the remainder as we did before, such that those
# with missing values for zed will be retained for further processing, but 
# otherwise added to the list of zones.
reduced <- merge(remainder, tagged, by="DAUID", all.x=TRUE)
keep <- rbind(keep, filter(reduced, !is.na(zed)))
reduced <- filter(reduced, is.na(zed)) %>% select(-zed, -Criteria)

# ONLY DISSEMINATION AREAS WITHIN TRACTS REMAIN
# So what's left is to figure out which tracts are below the threshold, and 
# pull them out. The processing for this will be similiar that used to split
# the subdivisions.
tracts <- reduced %>%
    group_by(CTUID) %>%
    summarise(Dwellings = sum(Dwellings, na.rm=TRUE)) %>%
    mutate(Criteria = ifelse(Dwellings<THRESHOLD, "CT below", "Retain"))
n_below <- nrow(filter(tracts, Criteria=="CT below"))
msg(paste(n_below, "of", length(unique(tracts$CTUID)), "tracts below threshold"))

# So pull the tracts below our criteria out
mole <- sort(unique(tracts$CTUID[tracts$Criteria=="CT below"]))
tagged <- data.table(CTUID = mole, zed = (1:length(mole))+zones_so_far,
    Criteria = "CT below")
zones_so_far <- max(tagged$zed)
tracked <- merge(reduced, tagged, by="CTUID", all.x=TRUE)
keep <- rbind(keep, filter(tracked, !is.na(zed)))
tracked <- filter(tracked, is.na(zed)) %>% select(-zed, -Criteria)

# Everything left over is a dissemination area within a tract, where the sum
# of the dwellings within the tract exceeded our threshold. So we will keep 
# each of these dissemination areas as separate zones. By definition each 
# dissemination area is unique, so adding the zone numbers ought to be simple.
tracked$Criteria <- "CT above"
tracked$zed <- (1:nrow(tracked))+zones_so_far
zones_so_far <- max(tracked$zed)
keep <- rbind(keep, tracked)
msg(paste(zones_so_far, "zones created with threshold=", THRESHOLD))

# Save the results, which should have a zone number that we associate with
# each of the original dissemination areas
final <- select(keep, DAUID, zed, Criteria)
write.table(final, file="Zone numbers by DA.csv", sep=',', row.names=FALSE,
    quote=FALSE)

# Show us how many zones by criteria?
percent <- function(x, places=1) round((x/sum(x))*100.0, places)
eh <- keep %>%
    group_by(zed, Criteria) %>%
    summarize(Dwellings = sum(Dwellings, na.rm=TRUE)) %>%
    group_by(Criteria) %>%
    summarize(Dwellings = sum(Dwellings), n=n()) %>%
    mutate(Percent = percent(n))
print(eh)


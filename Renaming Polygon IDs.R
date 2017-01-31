# make shapefile of the THIRD CASE
# rename IDs of Polygons to coincide with the rownumbers in dataframe
for (i in 1:length(taz_fourth)) {
  taz_fourth@polygons[[i]]@ID <- as.character(spdf$DisNum[i])
}
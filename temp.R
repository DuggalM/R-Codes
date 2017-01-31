c <- as.data.frame(colnames(on))

on.ps <- on[c(1, 1775)]


#' only keep records with DA numbers. This will get rid of sub-totals by various
#' sub-level geographies.

on1.ps <- filter(on.ps, grepl("^[35]",Geography))

sum(on1.ps$X..Postsecondary.certificate..diploma.or.degree)

installr::check.for.updates.R()
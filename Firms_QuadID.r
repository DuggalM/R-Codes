emp <- read.csv("c:/personal/r/firms.csv", stringsAsFactors = FALSE) %>% subset(., taz > 0)
j <- read.csv("c:/personal/r/Jobs_QT.csv", stringsAsFactors = FALSE)

emp1 <- subset(emp, n_employees > 0)

emp1 <- merge(emp1, j_sum, by.x = "firm_id", by.y = "firm_id", all.x = TRUE)

write.csv(emp1, "Firms_QuadID.csv", row.names = FALSE)

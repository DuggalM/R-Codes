firms <- read.csv("c:/personal/r/firms.csv", stringsAsFactors = FALSE) %>% subset(., taz != 0)

mf <- read.csv("c:/personal/r/mf85.csv", stringsAsFactors = FALSE)


mf1 <- mf[, 1:4]




to.write <- file("c:/personal/r/binfile1.dat", "wb")

writeBin(colnames(mf1)[1:4], to.write)

writeBin(mf1$p.q..val., to.write)
writeBin(mf1$X1001, to.write)
writeBin(mf1$X1002, to.write)
writeBin(mf1$X1003, to.write)


n <- dim(mf1)[1]

for (i in 1:n){
  temp <- as.vector(mf1[i,1:4], mode = "numeric")
  writeBin(temp, to.write)
}

close(to.write)



newdata <- file("c:/personal/r/binfile1.dat", "rb")

datavals <- readBin(newdata, integer(), size = 4, endian = "little")


variables <- readBin(newdata, character(), n=5)
variables

datavals = file("c:/personal/r/readbindata.dat", "rb")

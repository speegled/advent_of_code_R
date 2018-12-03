dd <- read.csv("data/data1.csv", sep = " ", header = FALSE, stringsAsFactors = FALSE)$V1
sum(dd)

ddlong <- rep(dd, 300)
dd2 <- cumsum(ddlong)
which.max(duplicated(dd2))

dd2[145558]

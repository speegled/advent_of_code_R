dd <- read.csv("data/data3.csv", sep = ",", header = FALSE, stringsAsFactors = FALSE)
library(tidyverse)
head(dd)
mat <- matrix(rep(0, 1000 * 1000), nrow = 1000)
for(i in 1:1341) {
  c <- dd$V1[i] + 1
  r <- dd$V2[i] + 1
  for(c2 in c:(dd$V3[i] + c - 1))
    mat[seq(r, r + dd$V4[i] - 1), c2] <- mat[seq(r, r + dd$V4[i] - 1), c2] + 1
}
sum(mat > 1)


for(i in 1:1341) {
  OK <- TRUE
  c <- dd$V1[i] + 1
  r <- dd$V2[i] + 1
  for(c2 in c:(dd$V3[i] + c - 1)) {
    if(any( mat[seq(r, r + dd$V4[i] - 1), c2] > 1))
      OK <- FALSE
  }
  if(OK) print(i)
  OK <- TRUE
}

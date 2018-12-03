dd <- read.csv("data/data2.csv", sep = " ", header = FALSE, stringsAsFactors = FALSE)$V1
library(stringr)
x <- dd[1]
two <- function(x) {
  letters <- str_split(x, pattern = "")[[1]]
  any(table(letters) == 2)
}
three <- function(x) {
  letters <- str_split(x, pattern = "")[[1]]
  any(table(letters) == 3)
}
sum(sapply(1:250, function(x) {
  two(dd[x])
})) * sum(sapply(1:250, function(x) {
  three(dd[x])
}))

for(i in 1:250) {
  for(j in 1:250) {
    if(sum(str_split(dd[i], pattern = "")[[1]] != str_split(dd[j], pattern = "")[[1]]) ==1) {
      f <- i
      g <- j
    }
  }
}
str_flatten(str_split(dd[f], pattern = "")[[1]][str_split(dd[f], pattern = "")[[1]] == str_split(dd[g], pattern = "")[[1]]])

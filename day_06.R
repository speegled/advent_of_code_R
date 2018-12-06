

dat <- read.csv("data/data6.csv", header = FALSE)
names(dat) <- c("x", "y")
dat$index <- 1:50
library(tidyverse)
ggplot(dat, aes(x, y, label = index))+ geom_text()

#'
#' The plot was important to me on this one. I used it to guess which points
#' would be associated with infinite area. I busied myself with that while the
#' loop to compute all the nearest points was going on.
#'

infinite <- c(0, 13, 10, 6, 7, 50, 36, 9, 46, 23, 2, 31, 5, 48, 12, 3, 16, 42, 29, 41)
map <- matrix(rep(".", 500^2), nrow = 500)

#'
#' Using dplyr was almost certainly a mistake here. It is just so slow. But,
#' development time is faster for me, so I did it.
#'

for(i in 1:500) {
  for(j in 1:500) {
    mins <- mutate(dat, distance = abs(i - x) + abs(j - y)) %>% 
      arrange(distance) %>%
      head(n = 2) 
    if(mins$distance[1] != mins$distance[2]) map[i,j] <- mins$index[1]
  }
}

#'
#' Goodness, I really need to get away from using table. Everything is a special case.
#' 

which.max(as.vector(table(map))[-(infinite + 1)])
as.vector(table(map))[-(infinite + 1)][19] #star 1

#'
#' Would this be easier???
#'

mytable <- function(x) {
  dd <- table(x)
  data.frame(labels = attr(dd, "dimnames")[[1]], values = as.vector(dd))
}

mytable(map) %>% 
  filter(!(labels %in% (infinite))) %>% 
  arrange(desc(values)) %>% 
  head(n = 1)

#'
#' This next part flew by. First idea I had worked and worked fast.
#'

tot <- 0
i <- 1
j <- 1
for(i in 1:500) {
  for(j in 1:500) {
    if(sum(abs(i - dat$x) + abs(j - dat$y)) < 10000)
      tot <- tot + 1
  }
  print(tot)
}
# star 2

#'
#' This first star seemed interesting. I wanted to try something clever with igraph, but I didn't know 
#' exactly how to do it. So, I just coded it up.
#'

library(tidyverse)
dd <- read.csv("data/data7.csv", sep = ",", header = FALSE, stringsAsFactors = FALSE)$V1
library(stringr)
dd <- str_split_fixed(dd, "", 2)
dd <- data.frame(dd, stringsAsFactors = FALSE)

library(igraph)
names(dd) <- c("first", "second")

path <- character(0)
dd$second
#'
#' Did I really do this?
#'
#' alphabet <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")
alphabet <- LETTERS
tt <- dd
available <- alphabet
while(nrow(tt) > 0) {
  i <- 1
  let <- available[i]
  use <- FALSE
  while(!use) {
    if(!(let %in% tt$second)) {
      path <- c(path, let)
      use <- TRUE
      tt <- tt[-which(tt$first == let),]
      i <- 1
      available <- available[-which(available == let)]
      let <- available[1]
    } else {
      i <- i + 1
      let <- available[i]
    }
  }  
}

str_flatten(path) #Star 1 is this plus the letter L. Hack.

#'
#' I really didn't want to do this one. It seems so lame. I knew how to do it in a boring way (see below),
#' but I couldn't think of anything interesting about it. Maybe I should've just let it go and thought some
#' more.
#'

work_plan <- data.frame(Second = 0:10000, W1 = "O", W2 = "P", W3 = ".", W4 =  ".", W5 = ".", Done = "", stringsAsFactors = FALSE)
work_plan
work_plan$W1[2:10000] <- "."
work_plan$W2[2:10000] <- "."
work_plan$W1[1:(60 + which(alphabet == "O") - 1)] <- "O"
work_plan$W2[1:(60 + which(alphabet == "P") - 1)] <- "P"
available <- alphabet
available <- setdiff(alphabet, c("O", "P"))
second <- 1

task_available <- function(dd, available) {
  rr <- FALSE
  rrlet <- ""
  for(let in available) {
    if(!(let %in% dd$second)) {
      rr <- TRUE
      rrlet <- let
      break
    }
  }
  list(x = rr, y = rrlet)
}


second <- 1
tt <- dd
#tt <- tt[-which(tt$first == "O"),]
#tt <- tt[-which(tt$first == "P"),]
while(str_length(work_plan$Done[second]) < 26) {
  for(i in 2:6) {
    if(work_plan[second + 1, i] == ".") {
      if(work_plan[second, i] != ".") {
        work_plan$Done <- str_c(work_plan$Done, work_plan[second, i])
        tt <- tt[-which(tt$first == work_plan[second, i]),]
      }
      ta <- task_available(tt, available)
      if(ta$x) {
        let <- ta$y
        work_plan[(second + 1):(second + 60 + which(alphabet == let)), i] <- let
        available <- setdiff(available, let)
      }
    }
  }
  second <- second + 1
}

which.max(apply(work_plan[,2:6], 1, function(x) all(x == "."))) - 1 #Star 2



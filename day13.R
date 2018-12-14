#'
#' OK, there is nothing to see here. Don't read this code. Just don't.
#' 
#' I wish I could've thought of a cool way to do this problem, but I couldn't.
#'

dd <- readLines("data/data13")
library(stringr)
str_length(dd[1])
str_split(dd[1], "")[[1]]
nn <- length(dd)
ma <- matrix(".", nrow = nn, ncol = nn)
for(i in 1:nn) {
  ma[i,] <- str_split(dd[i], "")[[1]]
}
num_cars <- sum(ma %in% c("<",">","^", "v"))
k <- 1
direction <- character(0)
for(i in 1:nn) {
  for(j in 1:nn) {
    if(ma[i,j] %in% c("<",">","^", "v")) {
      direction <- c(direction, ma[i,j])
      ma[i,j] <- k
      k <- k + 1
    }
  }
}

library(dplyr)
to_dir <- function(x) {
  if(x == "<")
    return(c(0, -1))
  if(x == ">") 
    return(c(0, 1))
  if(x == "^")
    return(c(-1,0))
  if(x == "v")
    return(c(1,0))
}

to_dir_plus <- function(x) {
  if(x$direction == "<") {
    if(x$parity == 0)
      return("v")
    if(x$parity == 2)
      return("^")
  }
  if(x$direction == "^") {
    if(x$parity == 0)
      return("<")
    if(x$parity == 2)
      return(">")
  }
  if(x$direction == ">") {
    if(x$parity == 0)
      return("^")
    if(x$parity == 2)
      return("v")
  }
  if(x$direction == "v") {
    if(x$parity == 0)
      return(">")
    if(x$parity == 2)
      return("<")
  }
  if(x$parity == 1)
    return(x$direction)
}

locations <- data.frame(row = integer(0), col = integer(0))
for(i in 1:num_cars)
  locations <- rbind(locations, which(ma == i, arr.ind = TRUE))
locations$index <- 1:num_cars
locations$direction <- direction
locations$parity <- 0
continue <- TRUE
j <- 1
while(continue) {
  for(i in 1:num_cars) {

    locations[i, 1:2] <- to_dir(locations$direction[i]) + locations[i, 1:2]
    cur_ind <- unlist(locations[i, 1:2])
      
    if(ma[cur_ind[1], cur_ind[2]] == "/") {
      if(locations$direction[i] == "<") {
        locations$direction[i] <- "v"
      } else if(locations$direction[i] == "^") {
        locations$direction[i] <- ">"
      } else if(locations$direction[i] == "v") {
        locations$direction[i] <- "<"
      } else if(locations$direction[i] == ">") {
        locations$direction[i] <- "^"
      } 
    }
    if(ma[cur_ind[1], cur_ind[2]] == "\\") {
      if(locations$direction[i] == "<") {
        locations$direction[i] <- "^"
      } else if(locations$direction[i] == "^") {
        locations$direction[i] <- "<"
      } else if(locations$direction[i] == "v") {
        locations$direction[i] <- ">"
      } else if(locations$direction[i] == ">") {
        locations$direction[i] <- "v"
      } 
    }
    if(ma[cur_ind[1], cur_ind[2]] == "+") {
      locations$direction[i] <- to_dir_plus(locations[i,])
      locations$parity[i] <- (locations$parity[i] + 1) %%3
    }
    if(anyDuplicated(locations[,1:2])) {
      continue <- FALSE
      break
    }
  }
  locations <- arrange(locations, row, col)
  print(j)
  j <- j + 1
}
locations #Star 1. Sigh. This one wasn't much fun.



#num_cars <- 17
locations <- data.frame(row = integer(0), col = integer(0))
for(i in 1:num_cars)
  locations <- rbind(locations, which(ma == i, arr.ind = TRUE))
locations$index <- 1:num_cars
locations$direction <- direction
locations$parity <- 0
continue <- TRUE
j <- 1
while(continue) {
  rmv <- NULL
  for(i in 1:num_cars) {
    
    locations[i, 1:2] <- to_dir(locations$direction[i]) + locations[i, 1:2]
    cur_ind <- unlist(locations[i, 1:2])
    
    if(ma[cur_ind[1], cur_ind[2]] == "/") {
      if(locations$direction[i] == "<") {
        locations$direction[i] <- "v"
      } else if(locations$direction[i] == "^") {
        locations$direction[i] <- ">"
      } else if(locations$direction[i] == "v") {
        locations$direction[i] <- "<"
      } else if(locations$direction[i] == ">") {
        locations$direction[i] <- "^"
      } 
    }
    if(ma[cur_ind[1], cur_ind[2]] == "\\") {
      if(locations$direction[i] == "<") {
        locations$direction[i] <- "^"
      } else if(locations$direction[i] == "^") {
        locations$direction[i] <- "<"
      } else if(locations$direction[i] == "v") {
        locations$direction[i] <- ">"
      } else if(locations$direction[i] == ">") {
        locations$direction[i] <- "v"
      } 
    }
    if(ma[cur_ind[1], cur_ind[2]] == "+") {
      locations$direction[i] <- to_dir_plus(locations[i,])
      locations$parity[i] <- (locations$parity[i] + 1) %%3
    }
    if(anyDuplicated(locations[,1:2])) {
      ll <- nrow(locations)
      rmv <- integer(0)
      for(p in 1:ll) {
        for(q in 1:ll) {
          if(locations[p,1] == locations[q,1] && locations[p,2] == locations[q,2] && p!=q)
            rmv <- c(rmv, c(p, q))
        }
      }
    }
  }
  if(!is.null(rmv)) {
    rmv <- unique(rmv)
    print(locations[rmv,])
    locations <- locations[-rmv,]
    num_cars <- num_cars - length(rmv)
    rmv <- NULL
  }
  if(nrow(locations) ==1) 
    continue <- FALSE
  locations <- arrange(locations, row, col)
  if(j %% 100 == 0) {
    print(j)
    print(num_cars)
  }
  j <- j + 1
}
locations #Star 2. I suck.

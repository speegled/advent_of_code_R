library(tidyverse)
dd <- read.csv("data/data5.csv", 
               header = FALSE, 
               stringsAsFactors = FALSE, 
               sep = ",")$V1
library(stringr)

#'
#' OK, I made a couple of bad decisions at the start of this. First, I decided not to use regex and 
#' str_remove_all, because I didn't know a regex that matched the pattern we are looking for. Then, I
#' made some horrible decisions because I was trying to code fast. So, it took forever to run. 
#' 
#' This one is really embarrasing.

continue <- TRUE
ds <- str_split_fixed(dd, "", 50000)
ds <- as.vector(ds)
while (continue) {
  continue <- FALSE
  i <- 1 #This was a big mistake in runtime, when combined with line 26 that was uncommented. Bad, Darrin!
  while(i < length(ds)) {
    if((toupper(ds[i]) == toupper(ds[i + 1])) && (ds[i] != ds[i + 1])) {
      ds <- ds[-c(i, i + 1)]
      continue <- TRUE
      #i <- length(ds) OMG, what was I thinking?? In my original code, this was uncommented. I need to drink less wine before the next challenge.
      print(length(ds))
    }
    i <- i + 1
  }  
}
length(ds) #Star 1

#'
#' I knew I couldn't just throw that in a loop; it took like five minutes. I also didn't know whether I
#' could just remove letters from the reduced string, or whether I needed to start completely over. Turns
#' out, I could just remove from the reduced string.
#'
#' 

dd <- str_flatten(ds) #This wasn't part of my original solution; I just used the original dd.

min <- 108780
alp <- as.vector(str_split_fixed("qwertyuiopasdfghjklzxcvbnm", "", 26))
let <- alp[1]
for(let in alp) {
  temp <- str_remove_all(dd, let)
  temp <- str_remove_all(temp, toupper(let)) #removing all instances of upper or lower case let
  ds <- str_split_fixed(temp, "", str_length(temp))
  ds <- as.vector(ds)
  continue <- TRUE
  jj <- 1
  while (continue) {
    continue <- FALSE
    i <- max(jj - 1, 1) #THIS is how to get it to run much, much faster. Sigh.
    while(i < length(ds)) {
      if((toupper(ds[i]) == toupper(ds[i + 1])) && (ds[i] != ds[i + 1])) {
        ds <- ds[-c(i, i + 1)]
        continue <- TRUE
        jj <- i
        i <- length(ds) #OR comment out this.
        #print(length(ds))
      }
      i <- i + 1
    }  
  }
  if(length(ds) < min) {
    min <- length(ds)
    print(length(ds))
  }
  print(length(ds))
}
min #This is the second star.



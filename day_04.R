library(tidyverse)
dd <- read.csv("data/data4.csv", header = FALSE, stringsAsFactors = FALSE, sep = ",")

#' 
#' A time/date puzzle! Ooh, lucky me, I just learned the basics of lubridate, so I can 
#' practice. I'll start by converting the times into POSIXct via the intuitively named
#' ymd_hm
#' 


dd$V1 <- lubridate::ymd_hm(dd$V1)
dd <- dd %>% arrange(V1)

#'
#' Next, I wanted a variable that gave the guard number for each observation. The setup of 
#' this data set reminded me of some spreadsheet documents I have used, which have data 
#' as headers interspersed throughout the spreadsheet.
#'

dd$V3 <- str_extract(dd$V2, "[0-9]+")
dd$V3 <- as.integer(dd$V3)
c <- 1973

for(i in 1:957) {
  if(is.na(dd$V3[i])) {
    dd$V3[i] <- c
  }  
  else {
    c <- dd$V3[i]
  }
}

#' Now, I don't need the observations that give the guard number and start time (I hope)!
#' I stored this in dd2, just in case I didn't want to throw those observations away.

dd2 <- filter(dd, !str_detect(dd$V2, "[0-9]"))

#'
#' The magic dplyr chain! This computes the length of time that each guard was asleep. 
#' In hours, which I wasn't necessarily expecting.
#'

dd2 %>% 
  group_by(V3, V2) %>% 
  summarize(tot = sum(V1)) %>% 
  ungroup %>% group_by(V3) %>% 
  summarize(tt = last(tot) - first(tot)) %>% 
  arrange(-tt)

#guard 641 slept the most

#'
#' For the next part, I decided to create a vector that contains all of the individual
#' minutes that guard 641 was asleep, and use table. I regretted using table later when
#' I needed to automate this and I had to remember how to pull out the "names" attribute.
#'

tt <- filter(dd2, V3 == 641) %>% mutate(mm = lubridate::minute(V1)) %>% pull(mm)
times <- numeric(0)
for(i in 1:27) {
  times <- c(times, seq(tt[2*i - 1],(tt[2*i] - 1) ))
}
max(table(times))
attr(table(times)[which.max(table(times))], "names")
41 * 641 #This was the answer to Question 1. Finished 132, best rank yet!

#'
#' For Question 2, I wanted to throw the previous code in a loop. But, it turns out
#' that one of the guards never slept, which my code didn't take into account. So, I had
#' to add a test on tt so that it isn't empty. I wasn't sure how to do that, so I used
#' the crazy condition 
#' 
#' length(tt) > 0 && !is.na(tt) && !is.null(tt)
#'

max <- 0
guard <- 0
minute <- 0

for(i in unique(dd2$V3)) {
  tt <- filter(dd2, V3 == i) %>% mutate(mm = lubridate::minute(V1)) %>% pull(mm)
  times <- numeric(0)
  if(length(tt) > 0 && !is.na(tt) && !is.null(tt)) {
    for(j in 1:(length(tt)/2)) {
      times <- c(times, seq(tt[2*j - 1],(tt[2*j] - 1) ))
    }
    mmm <- max(table(times))
    table(times)
    if(mmm > max) {
      minute <- attr(table(times)[which.max(table(times))], "names")
      max <- mmm
      guard <- i
    }
  }
}

#' Answer to Question 2: guard * minute I had this after 30 minutes; not too bad. But, I somehow got
#' confused where the answer was in my loop above, and got the wrong answer when I submitted.
#' I was pretty confident in the code, so I thought that there must have been guards 
#' sleeping before midnight (I didn't read the whole story, was that possible?). I spent
#' a long time implementing that and got the same "answer." Finally, I realized the answer
#' was stored in guard and got it. (In the original version, I had a bunch of extraneous
#' variables floating around that I had tried out and kept.)

guard * as.integer(minute) 


  
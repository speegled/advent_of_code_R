make_recipe <- function(sc, e1, e2) {
  s <- sc[e1] + sc[e2]
  if(floor(s/10) == 0)
    return(s)
  return(c(floor(s/10), s%%10))
}

my_mod <- function(x, m) {
  ifelse(x%%m == 0, m, x%%m)
}

#cur_pos <- 1
sc <- c(3, 7)
e1 <- 1
e2 <- 2
#cur_pos <- cur_pos + 2

sc <- c(sc, make_recipe(sc, e1, e2))
a <- length(sc)
e1 <- my_mod(e1 + sc[e1] + 1, a)
e2 <- my_mod(e2 + sc[e2] + 1, a)

builder <- integer(0)
while(a < 400000) {
  if(e1 < length(sc) - 20 && e2 < length(sc) - 20) {
    builder <- c(builder, make_recipe(sc, e1, e2))
  } else {
    if(length(builder) > 0) {
      sc <- c(sc, builder)
      builder <- integer(0)
    }
    sc <- c(sc, make_recipe(sc, e1, e2))
  }
  a <- length(sc)
  e1 <- my_mod(e1 + sc[e1] + 1, a)
  e2 <- my_mod(e2 + sc[e2] + 1, a)
  if(length(builder) == 0)
    print(a)
}
sc[2019:2028]
sc[380622:380631] #Star 1

#'
#' Note: this was my original code below. It was super slow, so I modified it as above when I got to the 
#' next problem. Unfortunately, it still was too slow, so I get to use data.table!
#' 
#' 
#' while(a < 400000) {
#'  sc <- c(sc, make_recipe(sc, e1, e2))
#'  a <- length(sc)
#'  e1 <- my_mod(e1 + sc[e1] + 1, a)
#'  e2 <- my_mod(e2 + sc[e2] + 1, a)
#'  if(a %% 1000 == 0)
#'    print(a)
#' }






library(stringr)
ss <- str_flatten(as.character(sc))
str_detect(ss, "380621") #Darn it

#'
#' OK, this was too slow when I tried to ploink it in a loop. So I hacked something. Still took forever.
#'

#sc <- rep(0, 50000000)
cur_pos <- 1
sc <- c(3, 7)
e1 <- 1
e2 <- 2
#cur_pos <- cur_pos + 2
add_vec <- make_recipe(sc, e1, e2)
#sc[(cur_pos: (cur_pos + length(add_vec)))] <- add_vec
#cur_pos <- cur_pos + length(add_vec)
a <- length(sc)
sc <- c(sc, make_recipe(sc, e1, e2))
e1 <- my_mod(e1 + sc[e1] + 1, a)
e2 <- my_mod(e2 + sc[e2] + 1, a)





sc
old_sc <- sc
builder <- integer(0)
continue <- TRUE
i <- 1
build_max <- 1000
while(continue) {
  i <- i + 1
  if(e1 < a - 11 && e2 < a - 11 && length(builder) < build_max) {
    builder <- c(builder, make_recipe(sc, e1, e2))
  } else {
    if(length(builder) > 0) {
      sc <- c(sc, builder)
      builder <- integer(0)
    }
    sc <- c(sc, make_recipe(sc, e1, e2))
  }
  a <- length(sc)
  e1 <- my_mod(e1 + sc[e1] + 1, a)
  e2 <- my_mod(e2 + sc[e2] + 1, a)
  if(i%%20000 == 0) {
    print(a)
    sc_cont <- sc
    e1_cont <- e1
    e2_cont <- e2
    a_cont <- a
    builder_cont <- builder
    l1 <- max(a - 40000, 1)
    #ss <- str_flatten(as.character(sc[l1:a]))
    #continue <- !str_detect(ss, "380621")
    if(a > 500000)
      build_max <- 1500
    if(a > 1000000)
      build_max <- 2000
    if(a > 2000000)
      build_max <- 2500
    if(a > 4000000)
      build_max <- 3000
    if(a > 8000000)
      build_max <- 3500
    if(a > 16000000)
      build_max <- 4000
  }
}

ss <- str_flatten(as.character(sc[l1:a]))
str_locate(ss, "380621") #Star 2


#'
#' The above works better, but I would like to figure out how to do this faster, so that I don't have
#' to wait around like a dufus. So, now the promised data.table solution! It is a lot faster than the hack above,
#' but I would still like an actually fast solution...
#'

sc <- rep(0L, 50000000) #assume max size is 50,000,000
sc <- data.table(sc)
sc$sc[1]
make_recipe_data_table <- function(sce1, sce2) {
  s <- sce1 + sce2  #sc$sc[e1] + sc$sc[e2]
  if(floor(s/10) == 0)
    return(s)
  return(c(floor(s/10), s%%10))
}

my_mod_data_table <- function(x, m) {
  ifelse(x%%m == 0, m, x%%m)
}


cur_pos <- 1L
set(sc, i = c(1L, 2L), j = 1L, value = c(3L, 7L))
e1 <- 1L
e2 <- 2L
cur_pos <- cur_pos + 2L

sce1 <- sc$sc[e1]
sce2 <- sc$sc[e2]

add_vec <- as.integer(make_recipe_data_table(sce1, sce2))
if(length(add_vec) == 1L) {
  rows <- cur_pos
} else{
  rows <- c(cur_pos, cur_pos + 1L)
}

set(sc, i = rows, j = 1L, value = add_vec)
cur_pos <- cur_pos + length(add_vec)
a <- cur_pos - 1L
e1 <- as.integer(my_mod_data_table(e1 + sc$sc[e1] + 1, a))
e2 <- as.integer(my_mod_data_table(e2 + sc$sc[e2] + 1, a))


continue <- TRUE
i <- 1
while(continue) {
  i <-  i + 1
  sces <- sc$sc[c(e1, e2)]
  add_vec <- as.integer(make_recipe_data_table(sces[1], sces[2]))
  if(length(add_vec) == 1L) {
    rows <- cur_pos
  } else{
    rows <- c(cur_pos, cur_pos + 1L)
  }
  
  set(sc, i = rows, j = 1L, value = add_vec)
  cur_pos <- cur_pos + length(add_vec)
  a <- cur_pos - 1L
  e1 <- as.integer(my_mod_data_table(e1 + sces[1] + 1, a))
  e2 <- as.integer(my_mod_data_table(e2 + sces[2] + 1, a))
  if(i%%1000000 == 0) {
    print(a)
    ss <- str_flatten(as.character(sc$sc))
    continue <- !str_detect(ss, "380621")
    print(continue)
  }
}
microbenchmark::microbenchmark(sc$sc[c(e1, e2)],
                               90943%%1000000,
                               as.integer(my_mod_data_table(e1 + sces[1] + 1, a)),
                               set(sc, i = rows, j = 1L, value = add_vec),
                               sc[c(e1, e2),]
                               )
                               
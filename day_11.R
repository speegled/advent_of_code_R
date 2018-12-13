m1 <- matrix(1:300 + 10, byrow = FALSE, nrow = 300, ncol = 300) *
  matrix(1:300, byrow = TRUE, nrow = 300, ncol = 300)
m1 <- m1 + 5535  #5535 was my data
m1 <- m1 * matrix(1:300 + 10, byrow = FALSE, nrow = 300, ncol = 300)
m1 <- m1 %% 1000
m1 <- floor(m1/100) - 5


my_con <- function(x) {
  convolve(x, c(1,1,1), type = "filter")
}
t1 <- t(apply(m1, 1, my_con))  #This threw me. I forgot that apply would fill out matrix by column.
t2 <- apply(t1, 2, my_con)
which(t2 == max(t2), arr.ind = T)  #Star 1

#'
#' For once, I did the first star in a way that made the second star super easy.
#'


current_max <- data.frame(val = 0, row = 0, col = 0, i = 0)
for(i in 1:300) {
  my_con <- function(x) {
    convolve(x, rep(1, i), type = "filter")
  }
  t1 <- t(apply(m1, 1, my_con))  
  t2 <- apply(t1, 2, my_con)
  m <- max(t2)
  if(m > current_max$val) {
    current_max[,2:3] <- which(t2 == m, arr.ind = T) 
    current_max$val = m
    current_max$i = i
  }
  print(i)
}
current_max #Star 2


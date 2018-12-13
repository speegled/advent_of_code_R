#'
#' For this problem, I was totally not motivated to do it for a long time. However, I decided to learn the data.tree package well enough
#' to do the problem with that package. I hope this is useful!
#'

#install.packages("data.tree") See, I had to install it! I've never used it before.
library(data.tree)

#' Read in data and put it into vector. I need to get better at this.

dd <- readLines("data/data8.csv")
dd <- str_split(dd, " ")
dd <- as.integer(dd[[1]])
head(dd)

i <- 1
pl <- 1
advent <- Node$new(1, num_children = dd[pl], cur_children = 0, num_meta = dd[pl + 1], metadata = list(0))
pl <- pl + 2
i <- i + 1
cur_node <- advent
continue <- TRUE
while(continue && !(is.na(dd[pl]))) {
  if(cur_node$num_children > cur_node$cur_children) {
    cur_node$AddChild(name = i, num_children = dd[pl], cur_children = 0, num_meta = dd[pl + 1], metadata = list(0))
    cur_node$cur_children <- cur_node$cur_children + 1
    i <- i + 1
    pl <- pl + 2
    cur_node <- cur_node$children[[cur_node$cur_children]]
  }
  if(cur_node$num_children == cur_node$cur_children) {
    cur_node$metadata <- list(dd[pl:(pl + cur_node$num_meta - 1)])
    pl <- pl + cur_node$num_meta
    cur_node <- cur_node$parent
  }
  print(pl)
  if(cur_node$isRoot) {
    if(sum(advent$Get("num_children") > advent$Get("cur_children")) == 0 ) {
      continue <- FALSE
    }
  }
}
cur_node$metadata <- list(dd[pl:length(dd)])
print(advent, "num_children", "cur_children", "num_meta", "metadata")

sum(sapply(advent$Get("metadata"), function(x) sum(x))) #Star 1

#' 
#' OK, I think it is a bit of a hack to keep track of cur_children; there is almost surely a better
#' way to do that.
#' 
#' Next up: star 2!
#' 

#'
#' Hmmm, I tried this exact function with print(advent, cost = Cost) and it doesn't work.
#' Not sure what is going on here.
#'

Cost <- function(node) {
  if(node$isLeaf) {
    result <- sum(node$metadata[[1]])
  } else{
    follow <- node$metadata[[1]]
    follow <- follow[follow <= node$num_children]
    if(length(follow) == 0) {
      result <- 0
    } else {
      result <- sum(sapply(node$children[follow], Cost))
    }
  }
  return(result)
}

# print(advent, cost = Cost) Not sure why this doesn't work.
advent$Do(function(node) node$cost <- Cost(node))
print(advent, "cost") #Star 2, read off the top line


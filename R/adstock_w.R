#'Apply Adstock Weights
#'
#' @param x vector
#' @param w weights
#' @param l maximum adstock length
#'
#'@export

adstock_w <- function(x, w, l){

  v <- c(rep(0, l-1), x, rep(0, l-1))

  y <- vector(length = length(x) + (l-1))

  for(i in 1:(length(x) + (l-1))){
    y[i] <- sum(w*(v[i:(i+(l-1))]))
  }
  y <- y[l:length(y)]
  return(y)
}


## https://en.wikipedia.org/wiki/Generalised_logistic_function
## https://en.wikipedia.org/wiki/Logistic_function
## https://en.wikipedia.org/wiki/Hill_equation_(biochemistry)


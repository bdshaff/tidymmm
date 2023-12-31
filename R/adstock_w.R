#' Apply Adstock Weights
#'
#' @description
#' A function that will convolve over the one dimensional vector of raw impressions over time
#' and apply the adstock weights.
#'
#' @returns a numeric vector of length(x) with adstock transformed media impressions.
#'
#' @param x vector
#' @param w weights
#' @param l maximum adstock length
#'
#' @examples
#' data(mmm_imps)
#' x <- mmm_imps$mi_banners
#' w <- geometric_adstock_weights()
#' l <- length(w)
#' a <- adstock_w(x, w, l)
#'
#' x[1:12]
#' a[1:12]
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


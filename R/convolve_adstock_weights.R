#' Convolve Adstock Weights
#'
#' @description
#' A function that will convolve over the one dimensional vector of raw impressions over time
#' and apply the adstock weights.
#'
#' @returns a numeric vector of length(x) with adstock transformed media impressions.
#'
#' @param x vector
#' @param w weights
#' @param g vector of group names. Must be length(x)
#'
#' @examples
#' data(mmm_imps)
#' x <- mmm_imps$mi_banners
#' w <- geometric_adstock_weights()
#' a <- convolve_adstock_weights(x, w)
#'
#' x[1:12]
#' a[1:12]
#'
#'@export

# convolve_adstock_weights <- function(x, w){
#
#   l = length(w) - 1
#   w = rev(w)
#
#   #padding x
#   paddep_x <- c(rep(0, l), x, rep(0, l))
#
#   y <- vector(length = length(x) + l)
#
#   for(i in 1:(length(x) + l)){
#     sub_vector <- paddep_x[i:(i+l)]
#     y[i] <- sum(w * sub_vector)
#   }
#   y <- y[1:length(x)]
#
#   return(y)
# }

convolve_adstock_weights <- function(x, w, g = NULL){

  l = length(w) - 1
  w = rev(w)

  if(!is.null(g)){

    stopifnot(length(g) == length(x))

    xg = split(x, g)
    y = map(xg, ~convolve_x(.x, w, l)) %>% unlist() %>% as.numeric()

  }else{
    y = convolve_x(x, w, l)
  }

  return(y)
}

## https://en.wikipedia.org/wiki/Generalised_logistic_function
## https://en.wikipedia.org/wiki/Logistic_function
## https://en.wikipedia.org/wiki/Hill_equation_(biochemistry)


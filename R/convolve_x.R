#' Convolve Adstock x
#'
#' @description
#' A function that will convolve over the one dimensional vector of raw impressions over time
#' and apply the adstock weights.
#'
#' @returns a numeric vector of length(x) with adstock transformed media impressions.
#'
#' @param x vector
#' @param w weights
#' @param l len
#'
#'
#'@export

convolve_x = function(x, w, l){
  #padding x
  paddep_x <- c(rep(0, l), x, rep(0, l))

  y <- vector(length = length(x) + l)

  for(i in 1:(length(x) + l)){
    sub_vector <- paddep_x[i:(i+l)]
    y[i] <- sum(w * sub_vector)
  }
  y <- y[1:length(x)]
  return(y)
}

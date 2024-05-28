#' Weibull Adstock Transformations
#'
#' @param shape decay rate
#' @param scale delay shift parameter
#' @param max_carryover Max Carryover
#' @param normalize should weights be normalized
#'
#'@export

weibull_adstock <- function(x, shape = 1, scale = 1, max_carryover = 12, normalize = TRUE){

  w <- weibull_adstock_weights(shape, scale, max_carryover, normalize)
  y <- convolve_adstock_weights(x, w)

  return(y)
}

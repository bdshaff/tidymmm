#' Delayed Adstock Transformations
#'
#' @param decay decay rate
#' @param delay delay shift parameter
#' @param max_carryover Max Carryover
#' @param normalize should weights be normalized
#'
#'@export
delayed_adstock <- function(x, decay = 0.5, delay = 2, max_carryover = 12, normalize = TRUE){

  w <- delayed_adstock_weights(decay, delay, max_carryover, normalize)
  y <- convolve_adstock_weights(x, w)

  return(y)
}

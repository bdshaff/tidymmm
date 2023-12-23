#'Geometric Adstock
#'
#' @param x input vector
#' @param decay decay rate
#' @param max_carryover maximum carryover
#' @param normalize  should the result be scaled?
#'
#'@export
geometric_adstock <- function(x, decay = 0.5, max_carryover = 12, normalize = TRUE){

  w <- geometric_adstock_weights(decay, max_carryover, normalize)
  y <- adstock_w(x, w, max_carryover)

  return(y)
}

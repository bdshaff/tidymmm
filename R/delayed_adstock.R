#'Delayed Adstock Transformations
#'
#' @param decay decay rate
#' @param delay delay shift parameter
#' @param max_carryover Max Carryover
#' @param normalize should weights be normalized
#'
#'@export


delayed_adstock_weights = function(decay = 0.5, delay = 2, max_carryover = 12, normalize = TRUE){

  stopifnot(delay < max_carryover)

  s = (seq(1, max_carryover) - delay)^2
  w = decay^s

  if(normalize){
    w = w/sum(w)
  }

  return(w)
}

delayed_adstock <- function(x, decay = 0.5, delay = 2, max_carryover = 12, normalize = TRUE){

  w <- delayed_adstock_weights(decay, delay, max_carryover, normalize)
  y <- adstock_w(x, w, max_carryover)

  return(y)
}

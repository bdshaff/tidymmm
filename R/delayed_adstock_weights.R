#' Delayed Adstock Weights
#'
#' @param decay decay rate
#' @param delay delay in the peak of the adstock
#' @param max_carryover maximum carryover
#' @param normalize  should the result be scaled?
#' @returns a numeric vector of weights of length (max_carryover)
#'
#' @description
#' delayed_adstock_weights is a function that is called to generate a vector of weights
#' that define how media impressions will be decayed in present and carried over into the future.
#'
#' @examples
#' delayed_adstock_weights()
#' delayed_adstock_weights(decay = 0.1, delay = 2)
#' delayed_adstock_weights(decay = 0.1, delay = 5)
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

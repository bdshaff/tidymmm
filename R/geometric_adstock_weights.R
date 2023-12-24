#' Geometric Adstock Weights
#'
#' @param decay decay rate
#' @param max_carryover maximum carryover
#' @param normalize  should the result be scaled?
#' @returns a numeric vector of weights of length (max_carryover)
#'
#' @description
#' geometric_adstock_weights is a function that is called to generate a vector of weights
#' that define how media impressions will be decayed in present and carried over into the future.
#'
#' @examples
#' geometric_adstock_weights()
#' geometric_adstock_weights(decay = 0.1)
#' geometric_adstock_weights(max_carryover = 3)
#' geometric_adstock_weights(decay = 0.4, max_carryover = 4, normalize = FALSE)
#'
#'@export

geometric_adstock_weights = function(decay = 0.5, max_carryover = 12, normalize = TRUE){
  s = seq(1, max_carryover)
  w = decay^s

  if(normalize){
    w = w/sum(w)
  }

  return(w)
}

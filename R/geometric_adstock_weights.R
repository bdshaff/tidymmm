#'Geometric Adstock Weights
#'
#' @param decay decay rate
#' @param max_carryover maximum carryover
#' @param normalize  should the result be scaled?
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

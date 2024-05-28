#' Weibull Adstock Weights
#'
#' @param shape weibull shape parameter
#' @param scale weibull scale parameter
#' @param max_carryover max_carryover parameter
#' @param normalize  should the result be scaled?
#' @returns a numeric vector of weights of length (max_carryover)
#'
#' @description
#' weibull_adstock_weights is a function that is called to generate a vector of weights
#' that define how media impressions will be decayed in present and carried over into the future.
#'
#' @examples
#' weibull_adstock_weights()
#' weibull_adstock_weights(scale = 0.5, scale = 10)
#' weibull_adstock_weights(scale = 1.5, scale = 20)
#'
#'@export
weibull_adstock_weights = function(shape = 1, scale = 1, max_carryover = 12, normalize = TRUE){

  stopifnot(max_carryover > 0)
  stopifnot(scale > 0)
  stopifnot(shape > 0)

  ww = stats::dweibull(1:max_carryover, shape = shape, scale = scale)
  ww = (ww - min(ww))/(max(ww) - min(ww))

  if(normalize){
    ww = ww/sum(ww)
  }

  return(ww)

}

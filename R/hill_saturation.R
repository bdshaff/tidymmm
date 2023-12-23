#'Hill Saturation
#'
#' @param x input vector
#' @param half_saturation half_saturation
#' @param shape shape
#' @param nu nu
#' @param max_ref should result be scale back to scale of x?
#'
#'@export
hill_saturation <- function(x, half_saturation = NULL, shape = NULL, nu = 1, max_ref = FALSE){

  if(is.null(half_saturation)){
    half_saturation <- mean(x, na.rm = TRUE)
  }

  if(is.null(shape)){
    shape = 5
  }

  y <- (1 / (1 + (x/half_saturation)^(-shape) )^(1/nu))

  if(max_ref){
    y <- max(x)*y
  }

  return(y)

}


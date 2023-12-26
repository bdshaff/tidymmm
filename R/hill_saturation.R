#'Hill Saturation
#'
#'@description
#'A function to apply the Hill Saturation to a vector of media impressions
#'
#'@returns a numeric vector of length(x) with saturated media impressions.
#'
#'@param x input vector
#'@param half_saturation half_saturation
#'@param shape shape
#'@param nu nu
#'@param max_ref should result be scale back to scale of x?
#'
#'#'@examples
#'data(mmm_imps)
#'x <- mmm_imps$mi_banners
#'y <- hill_saturation(x, shape = 1)
#'y_scaled <- hill_saturation(x, shape = 1, max_ref = TRUE)
#'
#'x[1:12]
#'y[1:12]
#'y_scaled[1:12]
#'
#'plot(sort(x), sort(y), type = "p", main = 'Hill Saturated x (scaled to [0-1])')
#'plot(sort(x), sort(y_scaled), type="p", main = "Scaled to Back to scale of x")
#'points(sort(x), sort(x), col = 2)
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


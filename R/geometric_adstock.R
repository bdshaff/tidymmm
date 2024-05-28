#'Geometric Adstock
#'
#'@description
#'A function to apply the geometric adstock transfromation to a vector of media
#'impressions
#'
#'@return a numeric vector of length(x) with adstock transformed media impressions.
#'
#'@param x input vector
#'@param decay decay rate
#'@param max_carryover maximum carryover
#'@param normalize  should the result be scaled?
#'
#'@examples
#'data(mmm_imps)
#'x <- mmm_imps$mi_banners
#'a <- geometric_adstock(x)
#'b <- geometric_adstock(x, decay = 0.7, max_carryover = 2)
#'
#'x[1:12]
#'a[1:12]
#'b[1:12]
#'
#'plot(x[1:20], type = "l", main = "Geometric Adstock Transformation of x")
#'lines(a[1:20], col = 2)
#'lines(b[1:20], col = 3)
#'
#'
#'@export
geometric_adstock <- function(x, decay = 0.5, max_carryover = 12, normalize = TRUE){

  w <- geometric_adstock_weights(decay, max_carryover, normalize)
  y <- convolve_adstock_weights(x, w)

  return(y)
}

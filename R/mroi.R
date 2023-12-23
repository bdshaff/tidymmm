#' mROI Estimate
#'
#'@export
mroi <- function(response_curves){

  lout <- nrow(response_curves) / (length(unique(response_curves$var)) * length(unique(response_curves$model)))

  response_curves |>
    group_by(var, model) |>
    summarise(
      ROI = (y[lout/2]/x[lout/2]),
      mROI = (y[lout/2 + 1] - y[lout/2 - 1])/(x[lout/2 + 1] - x[lout/2 - 1])
    )
}

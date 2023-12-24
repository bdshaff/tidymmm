#'Channel Metrics
#'
#'@param model_decomp model decomposition table
#'
#'@export

channel_metrics <-function(model_decomp){
  model_decomp |>
    tidyr::unnest(roi) |>
    dplyr::select(model, term, sales, ROI) |>
    tidyr::pivot_wider(names_from = model, values_from = c(sales, ROI))
}

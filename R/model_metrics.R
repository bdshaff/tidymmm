#' Model Metrics
#'
#' @param model_decomp model decomposition table
#' @param new_data model impressions data
#'
#'@export

model_metrics <- function(model_decomp, new_data){
  model_metrics <-
    model_decomp$workflow |>
    tibble::enframe() |>
    dplyr::mutate(name = model_decomps$model) |>
    dplyr::mutate(
      metrics = purrr::map(
        value, ~generics::augment(.x, new_data = new_data) |>
          dplyr::summarise(
            rsq = yardstick::rsq_vec(kpi_sales, .pred),
            mape = yardstick::mape_vec(kpi_sales, .pred)
          )
      )
    ) |>
    tidyr::unnest(metrics) |>
    dplyr::rename(model = name, workflow = value) |>
    dplyr::arrange(mape)

  return(model_metrics)
}

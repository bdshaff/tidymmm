#' Plot Model Fit
#'
#' @param model_decomp model decomposition table
#'
#' @export

plot_model_fit <- function(model_decomp){

  x <- model_decomp$workflow[[1]]
  x_mold <- workflows::extract_mold(x)
  kpi_sym <- rlang::sym(names(x_mold$outcomes))
  temp_sym <- rlang::sym(names(x_mold$extras$roles$temp))

  model_decomp |>
    tidyr::unnest(decomp) |>
    dplyr::select(model, !!temp_sym, !!kpi_sym) |>
    dplyr::distinct() |>
    dplyr::bind_cols(
      model_decomps |>
        tidyr::unnest(cols = c(model, pred)) |>
        dplyr::select(.pred)
    ) |>
    ggplot2::ggplot(ggplot2::aes(Date, .pred, color = model)) +
    ggplot2::geom_line() +
    ggplot2::geom_line(ggplot2::aes(Date, kpi_sales), color = "black", linetype = 2) +
    ggplot2::facet_grid(model~.) +
    ggplot2::theme_minimal() +
    ggsci::scale_color_npg()
}

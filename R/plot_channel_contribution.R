#'Plot Channel Contribution
#'
#' @param model_decomp model decomposition table
#'
#'@export

plot_channel_contribution <- function(model_decomp){
  model_decomp |>
    tidyr::unnest(decomp) |>
    dplyr::group_by(model, term) |>
    dplyr::summarise(
      sales_contribution = sum(contrib_adj),
      total_sales = sum(kpi_sales)
    ) |>
    dplyr::mutate(perent_contribution = sales_contribution/total_sales) |>
    ggplot2::ggplot(ggplot2::aes(model, sales_contribution, fill = term)) +
    ggplot2::geom_col(position = "stack", alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(perent_contribution, accuracy = .1)),
              position = ggplot2::position_stack(vjust = .5)) +
    ggsci::scale_fill_npg() +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(labels = scales::number) +
    ggplot2::labs(y = "", x = "", title = "Percent Contribution by Channel")
}

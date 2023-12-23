#'Plot Base Contribution
#'
#' @param model_decomp model decomposition table
#'
#'@export

plot_base_contribution <- function(model_decomp){
  model_decomp |>
    tidyr::unnest(decomp) |>
    dplyr::group_by(model, source = dplyr::if_else(term == "base", "base", "mi")) |>
    dplyr::summarise(
      sales_contribution = sum(contrib_adj),
      total_sales = sum(kpi_sales)
    ) |>
    dplyr::mutate(perent_contribution = sales_contribution/total_sales) |>
    ggplot2::ggplot(ggplot2::aes(model, sales_contribution, fill = source)) +
    ggplot2::geom_col(position = "stack", alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(perent_contribution, accuracy = .1)),
              position = ggplot2::position_stack(vjust = .5)) +
    ggsci::scale_fill_aaas()+
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(labels = scales::number) +
    ggplot2::labs(y = "", x = "", title = "Percent Contribution: Base vs Media Investment")
}

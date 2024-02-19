#'Plot Channel Contribution Mopnth
#'
#' @param model_decomp model decomposition table
#' @param begin_date minimum date
#' @param end_date maximum date
#'
#'@export

plot_channel_contribution_month <- function(model_decomp, begin_date, end_date){
  model_decomps |>
    tidyr::unnest(decomp) |>
    dplyr::mutate(Month = lubridate::floor_date(Date, unit = "months")) |>
    dplyr::group_by(Month, model, term) |>
    dplyr::summarise(
      sales_contribution = sum(contrib_adj),
      total_sales = sum(kpi_sales)
    ) |>
    dplyr::mutate(perent_contribution = sales_contribution/total_sales) |>
    dplyr::filter(Month >= begin_date, Month <= end_date) |>
    ggplot2::ggplot(ggplot2::aes(Month, sales_contribution, fill = term)) +
    ggplot2::geom_col(position = "stack", alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(perent_contribution, accuracy = .1)),
                       position = ggplot2::position_stack(vjust = .5), check_overlap = TRUE) +
    ggplot2::facet_grid(model~.) +
    ggsci::scale_fill_npg() +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(labels = scales::number) +
    ggplot2::labs(y = "", x = "", title = "Percent Contribution by Channel")
}

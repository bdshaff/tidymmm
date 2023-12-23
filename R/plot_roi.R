#'Plot ROI
#'
#' @param model_decomp model decomposition table
#'
#'@export

plot_roi <- function(model_decomp){
  model_decomp |>
    tidyr::unnest(roi) |>
    dplyr::filter(term != "base") |>
    ggplot2::ggplot(ggplot2::aes(term, ROI, fill = model)) +
    ggplot2::geom_col(position = "dodge", alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = scales::dollar(ROI, accuracy = .01)),
              position = ggplot2::position_dodge(.9)) +
    ggsci::scale_fill_flatui() +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(labels = scales::dollar)
}

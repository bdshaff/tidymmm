#' user step function that tackes the recipe and adds to it
#'
#'  @param response_curves response curves data table
#'
#' @export
plot_response_curves <- function(response_curves){
  plotly::ggplotly(
    response_curves |>
      ggplot2::ggplot(ggplot2::aes(x,y, color = var)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(mid_point_x, mid_point_y)) +
      ggsci::scale_color_aaas() +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(model~.) +
      ggplot2::scale_x_continuous(labels = scales::dollar) +
      ggplot2::scale_y_continuous(labels = scales::number) +
      ggplot2::labs(x = "Spend", y = "KPI", title = "Media Response Curves")
  )
}

#' ROI Calculation
#'
#'@param x trained mmm model workflow
#'@param new_data impressions data
#'@param spend_data spend data
#'
#'@export
roi <- function(x, new_data, spend_data){

  workflows::is_trained_workflow(x)

  x_mold <- workflows::extract_mold(x)
  x_df <- dplyr::bind_cols(x_mold$extras$roles$temp, x_mold$outcomes, x_mold$predictors)
  kpi_sym <- rlang::sym(names(x_mold$outcomes))
  temp_sym <- rlang::sym(names(x_mold$extras$roles$temp))

  cd <- contrib_decomp(x, new_data = new_data)

  res <-
    cd |>
    dplyr::select(-estimate, -volume, -contrib, -.pred) |>
    tidyr::pivot_wider(names_from = term, values_from = contrib_adj, values_fill = 0) |>
    dplyr::select(tidyselect::contains("base"), tidyselect::contains("mi"))  |>
    dplyr::summarise_all(sum) |>
    t() |>
    data.frame() |>
    tibble::rownames_to_column() |>
    tibble::as_tibble() |>
    stats::setNames(c("term","sales")) |>
    dplyr::left_join(
      spend_data |>
        dplyr::select(-!!temp_sym)  |>
        dplyr::summarise_all(sum) |>
        t() |>
        data.frame() |>
        tibble::rownames_to_column() |>
        tibble::as_tibble() |>
        stats::setNames(c("term","spend"))
    ) |>
    dplyr::mutate(ROI = sales/spend)

  return(res)
}

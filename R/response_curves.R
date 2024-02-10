#'Generate Response Curves
#'
#'@param model_decomp model decomposition table/s
#'
#'@export
response_curves <- function(model_decomp){
  saturation_parameters <- extract_params(model_decomp, type = "step_hill_saturation")

  mid_point_x <-
    mmm_spend |>
    dplyr::summarise_at(saturation_parameters$var, sum) |> t() |>
    data.frame() |>
    tibble::rownames_to_column() |>
    stats::setNames(c("var","mid_point_x"))

  mid_point_y <-
    model_decomp |>
    tidyr::unnest(decomp) |>
    dplyr::group_by(model, var = term) |>
    dplyr::summarise(mid_point_y = sum(contrib_adj))

  sp <-
    saturation_parameters |>
    dplyr::left_join(mid_point_x) |>
    dplyr::left_join(mid_point_y)

  lout <- 2000

  response_curves <-
    sp |>
    dplyr::rowwise() |>
    dplyr::mutate(
      x = list(seq(0, 2*mid_point_x, length.out = lout)),
      y = list(2*mid_point_y*hill_saturation(x, shape = var_shape, nu = var_nu))
    ) |>
    tidyr::unnest(cols = c(x,y))

  return(response_curves)
}

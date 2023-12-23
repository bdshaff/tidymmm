#'Contribution Decomposition
#'
#' @param new_data data
#'
#'@export

## all functions are reliant on naming conventions for data columns
contrib_decomp = function(x, new_data){

  # check x is workflow and is trained/fit
  workflows::is_trained_workflow(x)

  # extract the coefs
  coefs <- generics::tidy(x) |> dplyr::select(term, estimate)
  coefs[coefs$term == "(Intercept)",]["term"] = "base"

  # extract the mold
  x_mold <- workflows::extract_mold(x)
  x_df <- dplyr::bind_cols(x_mold$extras$roles$temp, x_mold$outcomes, x_mold$predictors)

  # set kpi and temp names
  kpi_sym <- rlang::sym(names(x_mold$outcomes))
  temp_sym <- rlang::sym(names(x_mold$extras$roles$temp))

  contrib <-
    x_df |>
    dplyr::bind_cols(predict(x, new_data = new_data)) |>
    dplyr::mutate(cf = !!kpi_sym/.pred, base = 1) |>
    tidyr::pivot_longer(-c(!!temp_sym, !!kpi_sym, .pred, cf), names_to = "term", values_to = "volume") |>
    dplyr::left_join(coefs) |>
    dplyr::mutate(
      contrib = volume * estimate,
      contrib_adj = volume * estimate * cf
    ) |>
    dplyr::mutate(term = forcats::fct_reorder(term, contrib, sum))

  return(contrib)
}

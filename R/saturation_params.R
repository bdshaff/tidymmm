#' user step function that tackes the recipe and adds to it
#'
#' @param model_decomp model decomposition table
#'
#' @export
saturation_params <- function(model_decomp){

  ef <- function(m){
    wflow <- model_decomp |>
      dplyr::filter(model == m) |>
      dplyr::pull(workflow)

    steps <- workflows::extract_recipe(wflow[[1]])$steps
    ix <- which(purrr::map_chr(steps, ~class(.x)[[1]]) == "step_hill_saturation")
    s_params <- purrr::map(steps[ix], ~extract_hill_saturation_params(.x)) |> dplyr::bind_rows()
    s_params$model <- m

    return(s_params)
  }

  models <- model_decomp$model

  purrr::map(models, ~ef(.x)) |> dplyr::bind_rows()

}

extract_hill_saturation_params <- function(step){

  stopifnot("step_hill_saturation" %in% class(step))

  s <- unlist(step)
  var <- rlang::quo_name(s$terms)
  var_half_saturation <- s$half_saturation
  var_shape <- s$shape
  var_nu <- s$nu
  tibble::tibble(var, var_half_saturation, var_shape, var_nu)
}

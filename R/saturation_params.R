#' Extract step transformation parameters
#'
#' @param model_decomp model decomposition table
#'
#' @export
extract_params <- function(model_decomp, type = "step_hill_saturation"){

  ef <- function(m){
    wflow <- model_decomp |>
      dplyr::filter(model == m) |>
      dplyr::pull(workflow)

    steps <- workflows::extract_recipe(wflow[[1]])$steps
    ix <- which(purrr::map_chr(steps, ~class(.x)[[1]]) == type)

    if(type == "step_hill_saturation"){
      s_params <- purrr::map(steps[ix], ~extract_hill_saturation_params(.x)) |> dplyr::bind_rows()
    }else if(type == "step_geometric_adstock"){
      s_params <- purrr::map(steps[ix], ~extract_geometric_adstock_params(.x)) |> dplyr::bind_rows()
    }

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
extract_geometric_adstock_params <- function(step){

  stopifnot("step_geometric_adstock" %in% class(step))

  s <- unlist(step)
  var <- rlang::quo_name(s$terms)
  var_decay <- s$decay
  var_max_carryover <- s$max_carryover
  tibble::tibble(var, var_decay, var_max_carryover)
}


# saturation_params <- function(model_decomp){
#
#   ef <- function(m){
#     wflow <- model_decomp |>
#       dplyr::filter(model == m) |>
#       dplyr::pull(workflow)
#
#     steps <- workflows::extract_recipe(wflow[[1]])$steps
#     ix <- which(purrr::map_chr(steps, ~class(.x)[[1]]) == "step_hill_saturation")
#     s_params <- purrr::map(steps[ix], ~extract_hill_saturation_params(.x)) |> dplyr::bind_rows()
#     s_params$model <- m
#
#     return(s_params)
#   }
#
#   models <- model_decomp$model
#
#   purrr::map(models, ~ef(.x)) |> dplyr::bind_rows()
#
# }

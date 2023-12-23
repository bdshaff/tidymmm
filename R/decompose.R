#'Decompose Model Contributions
#'
#' @param x trained mmm model workflow
#' @param new_data model impression data
#' @param spend_data spend data
#'
#'@export

decompose <- function(x, new_data, spend_data){

  if(class(x) == "workflow"){
    workflows::is_trained_workflow(x)
    frm <- tibble::enframe(list(x))
  }else if(is.list(x)){
    stopifnot(sum(purrr::map_lgl(x, workflows::is_trained_workflow)) == length(x))
    frm <- tibble::enframe(x)
  }

  model_decomps <-
    frm |>
    dplyr::mutate(
      decomp = purrr::map(value, ~contrib_decomp(.x, new_data = new_data)),
      roi = purrr::map(value, ~roi(.x, new_data = new_data, spend_data = spend_data)),
      pred = purrr::map(value, ~stats::predict(.x, new_data = new_data))
    ) |>
    dplyr::rename(model = name, workflow = value)

  return(model_decomps)
}

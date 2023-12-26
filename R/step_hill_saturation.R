#' Hill Saturation Step
#'
#' step_hill_saturation() creates a specification of a recipe step that will apply the hill saturation transformation to data.
#'
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables for this step. See selections() for more details.
#' @param role For model terms created by this step, what analysis role should they be assigned? By default, the new columns created by this step from the original variables will be used as predictors in a model.
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param half_saturation Half Saturation of the hill saturation transformation
#' @param shape shape parameter of the hill saturation transformation
#' @param nu nu parameter of the hill saturation transformation
#' @param max_ref A logical. Should the data be scaled back to the original scale.
#' @param columns A character string of the selected variable names. This field is a placeholder and will be populated once prep() is used.
#' @param skip A logical. Should the step be skipped when the recipe is baked by bake()? While all operations are baked when prep() is run, some operations may not be able to be conducted on new data (e.g. processing the outcome variable(s)). Care should be taken when using skip = TRUE as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it
#'
#'@return An updated version of recipe with the new step added to the sequence of any existing operations.
#'
#'@examples
#' library(tidymodels)
#' library(tidymmm)
#' data(mmm_imps)
#' mmm_recipe <-
#'   recipe(kpi_sales ~ mi_tv, data = mmm_imps) |>
#'   step_hill_saturation(mi_tv)
#'
#' @export
step_hill_saturation <- function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    half_saturation = NULL,
    shape = 1,
    nu = 1,
    max_ref = FALSE,
    columns = NULL,
    skip = FALSE,
    id = recipes::rand_id("hill_saturation")
) {
  recipes::add_step(
    recipe,
    step_hill_saturation_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      half_saturation = half_saturation,
      shape = shape,
      nu = nu,
      max_ref = max_ref,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}


#' @exportS3Method recipes::prep
prep.step_hill_saturation<- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, col_names], types = c("double", "integer"))

  step_hill_saturation_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    half_saturation = x$half_saturation,
    shape = x$shape,
    nu = x$nu,
    max_ref = x$max_ref,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

step_hill_saturation_new <- function(terms, role, trained, half_saturation, shape, nu, max_ref, columns, skip, id) {
  recipes::step(
    subclass = "hill_saturation",
    terms = terms,
    role = role,
    trained = trained,
    half_saturation = half_saturation,
    shape = shape,
    nu = nu,
    max_ref = max_ref,
    columns = columns,
    skip = skip,
    id = id
  )
}

#' @exportS3Method recipes::bake
bake.step_hill_saturation <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  recipes::check_new_data(col_names, object, new_data)


  for (col_name in col_names) {
    tmp <- new_data[[col_name]]

    tmp <- get_hill_saturation(tmp, list(half_saturation = object$half_saturation, shape = object$shape, nu = object$nu, max_ref = object$max_ref))

    new_data[[col_name]] <- tmp
  }

  new_data
}

print.step_hill_saturation <- function(x, width = max(20, options()$width - 31), ...) {
  half_saturation <- x$half_saturation
  shape <- x$shape
  nu <- x$nu
  msg <- glue::glue("Hill Saturation")
  title <- glue::glue("{msg} transformation on ")
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

get_hill_saturation <- function(x, args = NULL) {
  res <- rlang::exec("hill_saturation", x = x, !!!args)
  res
}

#' @exportS3Method generics::tunable
tunable.step_hill_saturation<- function (x, ...) {
  tibble::tibble(
    name = c("shape", "nu"),
    call_info = list(
      list(pkg = NULL, fun = "shape"),
      list(pkg = NULL, fun = "nu")
    ),
    source = "recipe",
    component = "step_hill_saturation",
    component_id = x$id
  )
}

#' @export
shape <- function(range = c(0.01, 10.0), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(shape = "# Hill Saturation Shape"),
    finalize = NULL
  )
}

#' @export
nu <- function(range = c(0.01, 10.0), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(nu = "# Hill Saturation Nu"),
    finalize = NULL
  )
}

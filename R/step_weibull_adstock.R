#'Weibull Adstock Transformation
#'
#'step_weibull_adstock() creates a specification of a recipe step that will adstock transform data.
#'
#'@param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#'@param ... One or more selector functions to choose variables for this step. See selections() for more details.
#'@param role For model terms created by this step, what analysis role should they be assigned? By default, the new columns created by this step from the original variables will be used as predictors in a model.
#'@param trained A logical to indicate if the quantities for preprocessing have been estimated.
#'@param shape Shape parameter
#'@param scale Scale parameter
#'@param max_carryover Maximum Carryover parameter for the weibull adstock transformation
#'@param normalize Should adstock weights be normalized to sum up to 1
#'@param columns A character string of the selected variable names. This field is a placeholder and will be populated once prep() is used.
#'@param skip A logical. Should the step be skipped when the recipe is baked by bake()? While all operations are baked when prep() is run, some operations may not be able to be conducted on new data (e.g. processing the outcome variable(s)). Care should be taken when using skip = TRUE as it may affect the computations for subsequent operations.
#'@param id A character string that is unique to this step to identify it
#'
#'@return An updated version of recipe with the new step added to the sequence of any existing operations.
#'
#' @examples
#' library(tidymodels)
#' library(tidymmm)
#' data(mmm_imps)
#' mmm_recipe <-
#'   recipe(kpi_sales ~ mi_tv, data = mmm_imps) |>
#'   step_weibull_adstock(mi_tv)
#'
#'@export
step_weibull_adstock <- function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    shape = 1,
    scale = 1,
    max_carryover = 12,
    normalize = TRUE,
    columns = NULL,
    skip = FALSE,
    id = recipes::rand_id("weibull_adstock")
) {
  recipes::add_step(
    recipe,
    step_weibull_adstock_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      shape = shape,
      scale = scale,
      max_carryover = max_carryover,
      normalize = normalize,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}


#'@export
prep.step_weibull_adstock <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, col_names], types = c("double", "integer"))

  step_weibull_adstock_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    shape = x$shape,
    scale = x$scale,
    max_carryover = x$max_carryover,
    normalize = x$normalize,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

step_weibull_adstock_new <- function(terms, role, trained, shape, scale, max_carryover, normalize, columns, skip, id) {
  recipes::step(
    subclass = "weibull_adstock",
    terms = terms,
    role = role,
    trained = trained,
    shape = shape,
    scale = scale,
    max_carryover = max_carryover,
    normalize = normalize,
    columns = columns,
    skip = skip,
    id = id
  )
}

#'@export
bake.step_weibull_adstock <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  recipes::check_new_data(col_names, object, new_data)


  for (col_name in col_names) {
    tmp <- new_data[[col_name]]

    tmp <- get_weibull_adstock(tmp, list(
      shape = object$shape,
      scale = object$scale,
      max_carryover = object$max_carryover,
      normalize = object$normalize
      ))

    new_data[[col_name]] <- tmp
  }

  new_data
}

print.step_weibull_adstock <- function(x, width = max(20, options()$width - 31), ...) {
  shape <- x$shape
  scale <- x$scale
  max_carryover <- x$max_carryover
  normalize <- x$normalize
  msg <- glue::glue("Weibull Adstock (shape {shape}, scale {scale}, maximum carryover {max_carryover} | normalize={normalize})")
  title <- glue::glue("{msg} transformation on ")
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

get_weibull_adstock <- function(x, args = NULL) {
  res <- rlang::exec("weibull_adstock", x = x, !!!args)
  res
}

#'@export
tunable.step_weibull_adstock <- function (x, ...) {
  tibble::tibble(
    name = c("shape", "scale", "max_carryover"),
    call_info = list(
      list(pkg = NULL, fun = "decay"),
      list(pkg = NULL, fun = "scale"),
      list(pkg = NULL, fun = "max_carryover")
    ),
    source = "recipe",
    component = "step_weibull_adstock",
    component_id = x$id
  )
}

#' @export
weibull_shape <- function(range = c(0, Inf), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(decay = "# Weibull Shape"),
    finalize = NULL
  )
}

#' @export
weibull_scale <- function(range = c(0, Inf), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(scale = "# Weibull Scale"),
    finalize = NULL
  )
}

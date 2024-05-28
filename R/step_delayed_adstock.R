#'Delayed Adstock Transformation
#'
#'step_delayed_adstock() creates a specification of a recipe step that will adstock transform data.
#'
#'@param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#'@param ... One or more selector functions to choose variables for this step. See selections() for more details.
#'@param role For model terms created by this step, what analysis role should they be assigned? By default, the new columns created by this step from the original variables will be used as predictors in a model.
#'@param trained A logical to indicate if the quantities for preprocessing have been estimated.
#'@param decay Decay Rate parameter for the delayed adstock transformation
#'@param delay Delay parameter
#'@param max_carryover Maximum Carryover parameter for the delayed adstock transformation
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
#'   step_delayed_adstock(mi_tv)
#'
#'@export
step_delayed_adstock <- function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    decay = 0.5,
    delay = 2,
    max_carryover = 12,
    columns = NULL,
    skip = FALSE,
    id = recipes::rand_id("delayed_adstock")
) {
  recipes::add_step(
    recipe,
    step_delayed_adstock_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      decay = decay,
      delay = delay,
      max_carryover = max_carryover,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}


#'@export
prep.step_delayed_adstock <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, col_names], types = c("double", "integer"))

  step_delayed_adstock_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    decay = x$decay,
    delay = x$delay,
    max_carryover = x$max_carryover,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

step_delayed_adstock_new <- function(terms, role, trained, decay, delay, max_carryover, columns, skip, id) {
  recipes::step(
    subclass = "delayed_adstock",
    terms = terms,
    role = role,
    trained = trained,
    decay = decay,
    delay = delay,
    max_carryover = max_carryover,
    columns = columns,
    skip = skip,
    id = id
  )
}

#'@export
bake.step_delayed_adstock <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  recipes::check_new_data(col_names, object, new_data)


  for (col_name in col_names) {
    tmp <- new_data[[col_name]]

    tmp <- get_delayed_adstock(tmp, list(decay = object$decay, delay = object$delay, max_carryover = object$max_carryover))

    new_data[[col_name]] <- tmp
  }

  new_data
}

print.step_delayed_adstock <- function(x, width = max(20, options()$width - 31), ...) {
  decay <- x$decay
  delay <- x$delay
  max_carryover <- x$max_carryover
  msg <- glue::glue("delayed Adstock (decay {decay},delay {delay} maximum carryover {max_carryover})")
  title <- glue::glue("{msg} transformation on ")
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

get_delayed_adstock <- function(x, args = NULL) {
  res <- rlang::exec("delayed_adstock", x = x, !!!args)
  res
}

#'@export
tunable.step_delayed_adstock <- function (x, ...) {
  tibble::tibble(
    name = c("decay","delay","max_carryover"),
    call_info = list(
      list(pkg = NULL, fun = "decay"),
      list(pkg = NULL, fun = "delay"),
      list(pkg = NULL, fun = "max_carryover")
    ),
    source = "recipe",
    component = "step_delayed_adstock",
    component_id = x$id
  )
}

#' @export
delay <- function(range = c(0, 100), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(decay = "# Delay"),
    finalize = NULL
  )
}

#'Step Geometric Adstock
#'
#'user step function that takes the recipe and adds to it
#'
#' @param recipe recipe
#' @param role role
#' @param trained trained or not
#' @param decay decay rate
#' @param max_carryover max_carryover
#' @param columns columns
#' @param skip FALSE
#' @param id id
#'
#'
#' @export
#'
step_geometric_adstock <- function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    decay = 0.5,
    max_carryover = 12,
    columns = NULL,
    skip = FALSE,
    id = recipes::rand_id("geometric_adstock")
) {
  recipes::add_step(
    recipe,
    step_geometric_adstock_new(
      terms = rlang::enquos(...),
      trained = trained,
      role = role,
      decay = decay,
      max_carryover = max_carryover,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}


#' @exportS3Method recipes::prep
prep.step_geometric_adstock <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, col_names], types = c("double", "integer"))

  step_geometric_adstock_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    decay = x$decay,
    max_carryover = x$max_carryover,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

step_geometric_adstock_new <- function(terms, role, trained, decay, max_carryover, columns, skip, id) {
  recipes::step(
    subclass = "geometric_adstock",
    terms = terms,
    role = role,
    trained = trained,
    decay = decay,
    max_carryover = max_carryover,
    columns = columns,
    skip = skip,
    id = id
  )
}

#' @exportS3Method recipes::bake
bake.step_geometric_adstock <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  recipes::check_new_data(col_names, object, new_data)


  for (col_name in col_names) {
    tmp <- new_data[[col_name]]

    tmp <- get_geometric_adstock(tmp, list(decay = object$decay, max_carryover = object$max_carryover))

    new_data[[col_name]] <- tmp
  }

  new_data
}

print.step_geometric_adstock <- function(x, width = max(20, options()$width - 31), ...) {
  decay <- x$decay
  max_carryover <- x$max_carryover
  msg <- glue::glue("Geometric Adstock (decay {decay}, maximum carryover {max_carryover})")
  title <- glue::glue("{msg} transformation on ")
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

get_geometric_adstock <- function(x, args = NULL) {
  res <- rlang::exec("geometric_adstock", x = x, !!!args)
  res
}

#' @exportS3Method generics::tunable
tunable.step_geometric_adstock <- function (x, ...) {
  tibble::tibble(
    name = c("decay","max_carryover"),
    call_info = list(
      list(pkg = NULL, fun = "decay"),
      list(pkg = NULL, fun = "max_carryover")
    ),
    source = "recipe",
    component = "step_geometric_adstock",
    component_id = x$id
  )
}

#' @export
decay <- function(range = c(0.01, 0.99), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(decay = "# Decay Rate"),
    finalize = NULL
  )
}

#' @export
max_carryover <- function(range = c(1, 12), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(max_carryover = "# Maximum Carryover"),
    finalize = NULL
  )
}

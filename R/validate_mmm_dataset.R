#' Validate media investment and spend datasets
#'
#' @param imps_df media investment data
#' @param spend_df media spend data
#'
#'@export

validate_mmm_dataset <- function(imps_df, spend_df){

  stopifnot(is.data.frame(imps_df))
  stopifnot(is.data.frame(spend_df))

  # check same dims
  t <- sum(dim(imps_df) == dim(spend_df)) == 2
  stopifnot(t)

  #check same colnames
  t <- sum(names(imps_df) == names(spend_df)) == ncol(imps_df)
  stopifnot(t)

  #check there is a kpi_ columns, and only one
  t <- sum(stringr::str_detect(colnames(imps_df), "kpi_")) == 1
  stopifnot(t)
  t <- sum(stringr::str_detect(colnames(spend_df), "kpi_")) == 1
  stopifnot(t)

  #check there is a Date columns, and only one
  t <- sum(colnames(imps_df) == "Date") == 1
  stopifnot(t)
  t <- sum(colnames(spend_df) == "Date") == 1
  stopifnot(t)

  #check all other columns are media investment columns
  t <- sum(stringr::str_detect(colnames(imps_df), "mi_")) == ncol(imps_df) - 2
  stopifnot(t)
  t <- sum(stringr::str_detect(colnames(spend_df), "mi_")) == ncol(spend_df) - 2
  stopifnot(t)

  cat("data validated")

}

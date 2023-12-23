#' @keywords internal
"_PACKAGE"


## usethis namespace: start
#' @importFrom dials new_quant_param
#' @importFrom dplyr bind_cols
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr summarise_all
#' @importFrom forcats fct_relevel
#' @importFrom generics tunable
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 position_stack
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggsci scale_color_aaas
#' @importFrom ggsci scale_fill_npg
#' @importFrom glue glue
#' @importFrom plotly ggplotly
#' @importFrom purrr map
#' @importFrom purrr map_chr
#' @importFrom recipes check_new_data
#' @importFrom recipes check_type
#' @importFrom recipes recipes_eval_select
#' @importFrom recipes step
#' @importFrom rlang sym
#' @importFrom scales number
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unnest
#' @importFrom tidyselect contains
#' @importFrom workflows extract_mold
#' @importFrom yardstick mape
## usethis namespace: end
NULL

#'@description
#'Tidymmm is a package with two main objectives:
#'1: extend the tydymodels package with transformation steps uniquely relevant to MMM
#'2: provide pre and post modeling tools for MMM projects that are closely integreated
#'with the tidyverse ecosystem
#'

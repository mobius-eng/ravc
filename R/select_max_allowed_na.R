#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#' @importFrom rlang `:=`
#' @importFrom broom tidy
#' @importFrom recipes prep
#' @importFrom recipes bake
#' @importFrom tidyselect all_of
NULL

#' Select columns with not-too-many missing values
#'
#' @param recipe Recipe to which to add the step
#' @param ... Columns which need to be checked
#' @param role For internal use
#' @param trained For internal use
#' @param na_max Max portion of missing values in a column
#' @param selected_cols After training will contain names of selected columns
#' @param dropped_cols After training will contain names of dropped columns
#' @param skip Whether to skip the step during [recipes::bake()]-ing
#' @param id Step id
#'
#' @returns Updated recipe with the step added
#'
#' @export
step_select_max_allowed_na <- function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    na_max = .05,
    selected_cols = NULL,
    dropped_cols = NULL,
    skip = FALSE,
    id = recipes::rand_id("select_max_allowed_na")
) {
  # collect terms
  terms <- recipes::ellipse_check(...)
  # call actual constructor
  recipes::add_step(
    recipe,
    step_select_max_allowed_na_new(
      terms = terms,
      role = role,
      trained = trained,
      na_max = na_max,
      selected_cols = selected_cols,
      dropped_cols = dropped_cols,
      skip = skip,
      id = id
    )
  )
}

step_select_max_allowed_na_new <- function(terms, role, trained, na_max, selected_cols, dropped_cols, skip, id) {
  recipes::step(
    subclass = "select_max_allowed_na",
    terms = terms,
    role = role,
    trained = trained,
    na_max = na_max,
    selected_cols = selected_cols,
    dropped_cols = dropped_cols,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_select_max_allowed_na <- function(x, ...) {
  args <- c(as.list(environment()), list(...))
  training <- args[["training"]]
  info <- args[["info"]]
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  # TODO: Check column types
  # Compute location and scale of the argument
  na_max <- x$na_max
  n <- nrow(training)
  result <- purrr::reduce(
    col_names,
    .init = list(keep = c(), drop = c()),
    .f = \(acc, col) {
      if (sum(is.na(training[[col]])) / n > na_max) {
        list(keep = acc$keep, drop = c(acc$drop, col))
      } else {
        list(keep = c(acc$keep, col), drop = acc$drop)
      }
    }
  )
  step_select_max_allowed_na_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    na_max = na_max,
    selected_cols = result$keep,
    dropped_cols = result$drop,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_select_max_allowed_na <- function(object, ...) {
  args <- c(as.list(environment()), list(...))
  new_data <- args[["new_data"]]
  vars <- object$dropped_cols
  purrr::reduce(
    vars,
    .init = new_data,
    .f = \(acc, col) {
      acc %>% dplyr::mutate("{col}" := NULL)
    }
  )
}

#' @export
tidy.step_select_max_allowed_na <- function(x, ...) {
  args <- c(as.list(environment()), list(...))
  show_dropped <- ifelse(is.null(args[["shw_dropped"]]), TRUE, args[["show_dropped"]])
  if (show_dropped) {
    tibble::tibble(Dropped = x$dropped_cols)
  } else {
    tibble::tibble(Selected = x$selected_cols)
  }
}

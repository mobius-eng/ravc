#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#' @importFrom rlang `:=`
#' @importFrom broom tidy
#' @importFrom recipes prep
#' @importFrom recipes bake
#' @importFrom tidyselect all_of
NULL


#' Robust centering and scaling using median and MAD
#'
#' @param recipe A recipe to which to add the step
#' @param ... List of columns (tidyverse's select) to which apply the step
#' @param role For internal use
#' @param trained For internal use
#' @param loc_scale Once trained (`prep`-ed), this will contain location and scale information.
#' Use `broom::tidy()` to access the information
#' @param skip Whether to skip this step in `bake`-ing
#' @param id ID of the step for the recipe
#'
#' @return An updated recipe with the step added
#'
#' @export
step_robust_scale <- function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    loc_scale = NULL,
    skip = FALSE,
    id = recipes::rand_id("robust_scale")
) {
  # collect terms
  terms <- recipes::ellipse_check(...)
  # call actual constructor
  recipes::add_step(
    recipe,
    step_robust_scale_new(
      terms = terms,
      role = role,
      trained = trained,
      loc_scale = loc_scale,
      skip = skip,
      id = id
    )
  )
}

step_robust_scale_new <- function(terms, role, trained, loc_scale, skip, id) {
  recipes::step(
    subclass = "robust_scale",
    terms = terms,
    role = role,
    trained = trained,
    loc_scale = loc_scale,
    skip = skip,
    id = id
  )
}


#' @export
prep.step_robust_scale <- function(x, ...) {
  args <- c(as.list(environment()), list(...))
  training <- args[["training"]]
  info <- args[["info"]]
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  # TODO: Check column types
  # Compute location and scale of the argument
  location <- "Location"
  scale <- "Scale"
  loc_scale <- training %>%
    tidyr::pivot_longer(cols = all_of(col_names), names_to = "Variable") %>%
    dplyr::group_by(all_of("Variable")) %>%
    dplyr::summarise(
      "{location}" := stats::median(all_of("value"), na.rm = TRUE),
      "{scale}" := stats::mad(all_of("value"), na.rm = TRUE))
  step_robust_scale_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    loc_scale = loc_scale,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_robust_scale <- function(object, ...) {
  args <- c(as.list(environment()), list(...))
  new_data <- args[["new_data"]]
  vars <- object$loc_scale %>% dplyr::pull(all_of("Variable"))
  loc_scale <- object$loc_scale
  purrr::reduce(
    vars,
    .init = new_data,
    .f = function(acc, col) {
      x <- loc_scale %>% dplyr::filter(.data[["Variable"]] == col)
      centre <- x %>% dplyr::pull(all_of("Location"))
      scale <- x %>% dplyr::pull(all_of("Scale"))
      if (scale != 0) {
        dplyr::mutate(acc, "{col}_scaled" := (.data[[col]] - centre) / scale)
      } else {
        acc
      }
    }
  )
}

#' @export
tidy.step_robust_scale <- function(x,...) {
  x$loc_scale
}

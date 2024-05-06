#' @importFrom dplyr n across
#' @importFrom magrittr `%>%`
#' @importFrom magrittr `%<>%`
#' @importFrom rlang .data
#' @importFrom rlang `:=`
#' @importFrom rlang `!!!`
#' @importFrom tidyselect where all_of
NULL

#' Computes summary statistics for numeric variables
#'
#' @param df a data frame for which to provide the summary
#' @param ... "Name = Function" pairs for summary statistics
#' @param add_std_fns an indicator whether to add standard descriptive
#' @param is_tidy Indicator if data is in tidy format. If so, describe
#' variables in `tidy_names` column using values in `tidy_vals`
#' @param tidy_names If in tidy format, columns with names and values
#' statistics (count, number of missing values, mean, standard deviation,
#' min, 25%, 50%, 75% quantiles, and max)
#' @param tidy_vals See `tidy_names`
#'
#' @returns A tibble with statistics in rows and variables in columns
#'
#' @export
describe <- function(df, ..., add_std_fns = TRUE, is_tidy = FALSE, tidy_names = ".names", tidy_vals = ".x") {
  args <- rlang::enexprs(...)
  std_fns = rlang::exprs(
    count = n(),
    missing = sum(is.na(.data[[".x"]])),
    mean = mean(.data[[".x"]], na.rm = TRUE),
    sd = stats::sd(.data[[".x"]], na.rm = TRUE),
    min = min(.data[[".x"]], na.rm = TRUE),
    `25%` = stats::quantile(.data[[".x"]], probs = 0.25, na.rm = TRUE),
    `50%` = stats::quantile(.data[[".x"]], probs = 0.5, na.rm = TRUE),
    `75%` = stats::quantile(.data[[".x"]], probs = 0.75, na.rm = TRUE),
    max = max(.data[[".x"]], na.rm = TRUE)
  )
  if (!is_tidy) {
    df_prep <- df %>%
      tidyr::pivot_longer(where(is.numeric), names_to = ".names", values_to = ".x") %>%
      dplyr::group_by(across(all_of(".names")))
  } else {
    df_prep <- df %>%
      dplyr::mutate(".names" := .data[[tidy_names]]) %>%
      dplyr::mutate(".x" := .data[[tidy_vals]]) %>%
      dplyr::select(all_of(c(".names", ".x"))) %>%
      dplyr::group_by(across(all_of(".names")))
  }
  if (add_std_fns) {
    df1 <- df_prep %>%
      dplyr::summarize(!!!std_fns, !!!args)
  } else {
    df1 <- df_prep %>%
      dplyr::summarize(!!!args)
  }
  df1 %>% dplyr::select(-all_of(".names")) %>%
    t() %>%
    tibble::as_tibble(rownames = "Statistic", .name_repair = \(x) df1[[".names"]])
}

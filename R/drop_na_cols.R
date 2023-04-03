#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#' @importFrom rlang `:=`
NULL


#' Drop columns with too many missing values
#'
#' @param .data A data frame
#' @param ... tidy-select columns
#' @param .na_max Maximum proportion of missing values allowed
#'
#' @returns A `drop_na_cols` object
#'
#' @export
drop_na_cols <- function(.data, ..., .na_max = 0.25) {
  df <- dplyr::select(.data, ...)
  cols <- colnames(df)
  nobs <- nrow(df)
  nas <- sapply(cols, \(col) sum(is.na(df[[col]])) / nobs)
  keep_cols <- cols[nas < .na_max]
  drop_cols <- cols[nas > .na_max]
  structure(
    list(
      cols = cols,
      na_max = .na_max,
      keep_cols = keep_cols,
      drop_cols = drop_cols
    ),
    class = "drop_na_cols"
  )
}

#' @export
predict.drop_na_cols <- function(object, newdata, ...) {
  dplyr::select(newdata, -tidyselect::all_of(object$drop_cols))
}

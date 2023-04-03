#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
#' @importFrom rlang `:=`
NULL


#' Standardize data in the data frame
#'
#' @param .data A data frame
#' @param ... tidy-select columns
#' @param .loc_fn Location function (e.g., `mean` or `median`)
#' @param .scale_fn Scale function (e.g., `sd` or `mad`)
#'
#' @returns A standardize object
#'
#' @export
standardize <- function(.data, ..., .loc_fn = mean, .scale_fn = stats::sd) {
  df <- dplyr::select(.data, ...)
  cols <- colnames(df)
  params <- sapply(cols, \(col) c(.loc_fn(df[[col]]), .scale_fn(df[[col]])))
  colnames(params) <- cols
  params_df <- params %>%
    tibble::as_tibble() %>%
    tibble::add_column(.params = c("location", "scale"), .before = 1)
  structure(
    list(
      params = params_df,
      cols = cols,
      loc_fn = .loc_fn,
      scale_fn = .scale_fn
    ),
    class = "standardize"
  )
}

#' @export
predict.standardize <- function(object, newdata, .to_latent = TRUE, ...) {
  df <- newdata
  if (.to_latent) {
    for (col in object$cols) {
      df <- dplyr::mutate(df, "{col}" := (.data[[col]] - object$params[[1, col]]) / object$params[[2, col]])
    }
  } else {
    for (col in object$cols) {
      df <- dplyr::mutate(df, "{col}" := .data[[col]] * object$params[[2, col]] + object$params[[1, col]])
    }
  }
  return(df)
}

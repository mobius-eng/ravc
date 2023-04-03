#' @importFrom magrittr `%>%`
#' @importFrom rlang .data
NULL


#' PCA transfomration on a data frame
#'
#' Apply PCA to a data frame. Convenience wrapper over `prcomp`
#'
#' @param .data A data frame to apply PCA to
#' @param ... tidy-select columns to use
#' @param .center An indicator whether to center the data prior to PCA
#' @param .scale An indicator whether to scale the data prior to PCA
#' @param .max_var A parameter contolling the number of PCs: how much of total
#'   variance should the PCs explain.
#'
#' @returns A `tidypca` (S3) object with fields `prcomp`, `cols` and `ncomp`.
#'
#' @export
tidypca <- function(.data, ..., .center = TRUE, .scale = FALSE, .max_var = 1) {
  df <- .data %>% dplyr::select(...) %>% tidyr::drop_na(tidyselect::everything())
  cols <- colnames(df)
  mat <- as.matrix(df)
  pca <- stats::prcomp(mat, center = .center, scale. = .scale)
  vars_full <- cumsum(pca$sdev ^ 2)
  vars_rel <- vars_full / utils::tail(vars_full, 1)
  ncomp <- min(which(vars_rel >= .max_var))
  structure(
    list(
      prcomp = pca,
      cols = cols,
      ncomp = ncomp,
      full_cumvars = vars_full,
      rel_cumvars = vars_rel
    ),
    class = "tidypca"
  )
}


#' Transform values to or from PCA latent space
#'
#' @param object A `tidypca` object
#' @param newdata A data frame-ilke data
#' @param to_latent if `TRUE`, performs usual transformation to PCs (latent
#'   space). Otherwise, transforms from latent to original variables.
#' @param ... Unused
#'
#' @export
predict.tidypca <- function(object, newdata, to_latent = TRUE, ...) {
  if (to_latent) {
    df <- newdata %>%
      tibble::as_tibble() %>%
      dplyr::select(dplyr::all_of(object$cols))
    mat <- as.matrix(df)
    raw_pca <- stats::predict(object$prcomp, mat)
    raw_pca[,seq(object$ncomp)] %>% tibble::as_tibble()
  } else {
    rot_mat <- object$prcomp$rotation[,seq(object$ncomp), drop = FALSE]
    cols <- dimnames(rot_mat)[[2]]
    df <- newdata %>% tibble::as_tibble() %>% dplyr::select(dplyr::all_of(cols))
    mat <- as.matrix(df)
    raw_x <- mat %*% t(rot_mat)
    raw_x %>% tibble::as_tibble()
  }
}

#' Compute chi squared score for PCs
#'
#' @param tidypca A `tidypca` object
#' @param newdata Either original data or PCA-transformed
#' @returns A data frame with PCs and overall chi squared score
#'
#' @export
chisq_score <- function(tidypca, newdata) {
  cols <- colnames(newdata)
  var_cols <- tidypca$cols
  .predict <- all(tidypca$cols %in% cols)
  if (.predict) {
    df <- stats::predict(tidypca, newdata)
  } else {
    df <- newdata
  }
  n <- tidypca$ncomp
  sdev <- tidypca$prcomp$sdev[1:n]
  mat <- df %>% as.matrix()
  chi2score <- apply(mat, 1, \(row) sum((row / sdev) ^2))
  df %>% tibble::add_column(ChiSqScore = chi2score)
}


# tidy.tidypca <- function(object, type = c("rotation", "scale", "center"), ...) {
#
# }

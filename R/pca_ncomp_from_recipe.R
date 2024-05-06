#' Returns number of PCA components from a PCA recipe step
#'
#' @param recipe A trained ([recipes::prep()]-ed) recipe
#' @param id ID of the PCA step
#'
#' @returns Number of PCA components or `NULL` if a PCA step is not found
#'
#' @export
pca_ncomp_from_recipe <- function(recipe, id = "pca") {
  pca_step <- NULL
  for(i in seq(recipe$steps)) {
    if (recipe$steps[[i]]$id == id) {
      pca_step <- recipe$steps[[i]]
      break()
    }
  }
  if (!is.null(pca_step)) {
    pca_step$num_comp
  } else {
    NULL
  }
}

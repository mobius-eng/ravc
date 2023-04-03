
#' Load either original or cached data
#'
#' @param fname File name
#' @param load_fun Function of no arguments to load the data (thunk)
#' @param write_fun Function accepting a data frame and file name to write
#'   cached version of the data
#' @param read_fun Function accepting one argument, a file name, to read cached
#'   data
#' @param reload Whether load fresh or cached data
#'
#' @returns Loaded or read data
#'
#' @export
load_cached <- function(fname, load_fun, write_fun, read_fun, reload = FALSE) {
  if (!reload && file.exists(fname)) {
    read_fun(fname)
  } else {
    data <- load_fun()
    write_fun(data, fname)
    data
  }
}

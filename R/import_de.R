#' Import DE results from files
#'
#' Read differential expression result files into a SimpleList.
#'
#' @param paths Character vector of file paths.
#'
#' @return A SimpleList of data frames.
#' @export
#' @importFrom utils read.csv
#' @importFrom S4Vectors SimpleList
importDE <- function(paths) {
  files <- normalizePath(paths, mustWork = TRUE)
  out <- lapply(files, utils::read.csv, stringsAsFactors = FALSE)
  if (is.null(names(paths))) {
    names(out) <- sub("\\.[^.]*$", "", basename(files))
  } else {
    names(out) <- names(paths)
  }
  S4Vectors::SimpleList(out)
}

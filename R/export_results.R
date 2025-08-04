#' Export DE and pathway results
#'
#' Write differential expression and pathway results to CSV files.
#'
#' @param x A DESummarizedExperiment.
#' @param dir Directory to write results.
#'
#' @return Invisibly returns a character vector of file paths written.
#' @export
#' @importFrom utils write.csv
exportResults <- function(x, dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  paths <- character()
  for (nm in names(x@deResults)) {
    file <- file.path(dir, paste0(nm, ".csv"))
    utils::write.csv(x@deResults[[nm]], file, row.names = TRUE)
    paths <- c(paths, file)
  }
  for (nm in names(x@pathwayResults)) {
    file <- file.path(dir, paste0(nm, ".csv"))
    utils::write.csv(x@pathwayResults[[nm]], file, row.names = TRUE)
    paths <- c(paths, file)
  }
  invisible(paths)
}

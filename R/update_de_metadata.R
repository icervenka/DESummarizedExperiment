#' Update metadata with DE result names
#'
#' @param x A DESummarizedExperiment.
#'
#' @return The updated object.
#' @keywords internal
#' @importFrom S4Vectors metadata metadata<-
updateDEmetadata <- function(x) {
  m <- S4Vectors::metadata(x)
  m$de_results <- names(x@deResults)
  S4Vectors::metadata(x) <- m
  x
}

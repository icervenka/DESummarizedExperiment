#' Merge DESummarizedExperiment objects
#'
#' Combine multiple DESummarizedExperiment objects column-wise.
#'
#' @param ... DESummarizedExperiment objects.
#'
#' @return A merged DESummarizedExperiment.
#' @export
#' @importFrom S4Vectors SimpleList metadata metadata<-
#' @importFrom methods is validObject
mergeDESummarizedExperiment <- function(...) {
  objects <- list(...)
  if (!length(objects)) {
    stop("no objects supplied")
  }
  if (!all(vapply(objects, methods::is, logical(1), "DESummarizedExperiment"))) {
    stop("all inputs must be DESummarizedExperiment objects")
  }
  row_ids <- rownames(objects[[1]])
  same_rows <- vapply(objects[-1], function(x) identical(rownames(x), row_ids), logical(1))
  if (!all(same_rows)) {
    stop("all objects must have identical rownames")
  }
  merged <- do.call(cbind, objects)
  merged@design <- NULL
  merged@contrastList <- list()
  merged@deResults <- S4Vectors::SimpleList()
  merged@pathwayResults <- S4Vectors::SimpleList()
  m <- S4Vectors::metadata(merged)
  m$design <- NULL
  m$de_results <- NULL
  S4Vectors::metadata(merged) <- m
  methods::validObject(merged)
  merged
}

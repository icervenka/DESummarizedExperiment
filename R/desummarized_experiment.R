#' DESummarizedExperiment class
#'
#' Extends `SingleCellExperiment` to store differential expression and pathway results.
#'
#' @slot design Design matrix or formula describing the experiment.
#' @slot contrastList Named list of contrasts.
#' @slot deResults SimpleList of differential expression results.
#' @slot pathwayResults SimpleList of pathway enrichment results.
#'
#' @export
#' @importFrom S4Vectors SimpleList
setClass(
  "DESummarizedExperiment",
  contains = "SingleCellExperiment",
  slots = c(
    design = "ANY",
    contrastList = "list",
    deResults = "SimpleList",
    pathwayResults = "SimpleList"
  ),
  prototype = list(
    design = NULL,
    contrastList = list(),
    deResults = SimpleList(),
    pathwayResults = SimpleList()
  ),
  validity = .valid_desummarized_experiment
)

#' Validity check for DESummarizedExperiment objects
#'
#' @param object A DESummarizedExperiment.
#' @return TRUE if valid, otherwise a character vector of error messages.
#' @keywords internal
.valid_desummarized_experiment <- function(object) {
  errors <- character()
  if (!is.null(object@design)) {
    valid_rows <- nrow(object@design) == ncol(object) &&
      identical(rownames(object@design), colnames(object))
    if (!valid_rows) {
      errors <- c(errors, "design rows must match colnames(object)")
    }
  }
  if (!is.list(object@contrastList)) {
    errors <- c(errors, "contrastList must be a list")
  }
  if (!is(object@deResults, "SimpleList")) {
    errors <- c(errors, "deResults must be a SimpleList")
  } else if (length(object@deResults)) {
    if (!all(lengths(object@deResults) == nrow(object))) {
      errors <- c(errors, "deResults elements must have same number of rows as object")
    }
    same_rows <- vapply(
      object@deResults,
      function(x) identical(rownames(x), rownames(object)),
      logical(1)
    )
    if (!all(same_rows)) {
      errors <- c(errors, "deResults elements must have rownames matching rownames(object)")
    }
  }
  if (!is(object@pathwayResults, "SimpleList")) {
    errors <- c(errors, "pathwayResults must be a SimpleList")
  } else if (length(object@pathwayResults)) {
    if (!all(lengths(object@pathwayResults) == nrow(object))) {
      errors <- c(errors, "pathwayResults elements must have same number of rows as object")
    }
    same_rows <- vapply(
      object@pathwayResults,
      function(x) identical(rownames(x), rownames(object)),
      logical(1)
    )
    if (!all(same_rows)) {
      errors <- c(errors, "pathwayResults elements must have rownames matching rownames(object)")
    }
  }
  if (length(errors)) errors else TRUE
}

#' DESummarizedExperiment
#'
#' Create a DESummarizedExperiment object.
#'
#' @inheritParams SingleCellExperiment::SingleCellExperiment
#' @param design Design matrix or formula describing the experiment.
#' @param contrastList Named list of contrasts.
#' @param deResults SimpleList of differential expression results.
#' @param pathwayResults SimpleList of pathway enrichment results.
#'
#' @return A DESummarizedExperiment object.
#'
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @importFrom methods validObject
#' @export
DESummarizedExperiment <- function(..., design = NULL, contrastList = list(),
                                   deResults = SimpleList(),
                                   pathwayResults = SimpleList()) {
  sce <- SingleCellExperiment(...)
  sce <- new(
    "DESummarizedExperiment",
    sce,
    design = design,
    contrastList = contrastList,
    deResults = deResults,
    pathwayResults = pathwayResults
  )
  validObject(sce)
  sce
}


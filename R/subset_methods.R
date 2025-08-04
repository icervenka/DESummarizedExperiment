#' Subsetting methods for DESummarizedExperiment
#'
#' Methods for `[`, `[[`, and `subset` that keep custom slots in sync.
#'
#' @name subsetting-methods
#' @docType methods
NULL

#' @rdname subsetting-methods
#' @importFrom S4Vectors endoapply
#' @export
setMethod(
  "[",
  "DESummarizedExperiment",
  function(x, i, j, ..., drop = FALSE) {
    y <- callNextMethod()
    if (!missing(i)) {
      y@deResults <- S4Vectors::endoapply(x@deResults, function(z) z[i, , drop = FALSE])
      y@pathwayResults <- S4Vectors::endoapply(x@pathwayResults, function(z) z[i, , drop = FALSE])
    } else {
      y@deResults <- x@deResults
      y@pathwayResults <- x@pathwayResults
    }
    if (!missing(j) && !is.null(x@design)) {
      y@design <- x@design[j, , drop = FALSE]
    } else {
      y@design <- x@design
    }
    updateDEmetadata(y)
  }
)

#' @rdname subsetting-methods
#' @export
setMethod(
  "subset",
  "DESummarizedExperiment",
  function(x, subset, select, ...) {
    callNextMethod()
  }
)

#' @rdname subsetting-methods
#' @export
setMethod(
  "[[",
  "DESummarizedExperiment",
  function(x, i, j, ...) {
    if (!missing(i)) {
      if (i %in% names(x@deResults)) {
        return(x@deResults[[i]])
      }
      if (i %in% names(x@pathwayResults)) {
        return(x@pathwayResults[[i]])
      }
    }
    callNextMethod()
  }
)

#' @rdname subsetting-methods
#' @export
setReplaceMethod(
  "[[",
  "DESummarizedExperiment",
  function(x, i, j, ..., value) {
    if (!missing(i)) {
      if (i %in% names(x@deResults)) {
        x@deResults[[i]] <- value
        x <- updateDEmetadata(x)
        return(x)
      }
      if (i %in% names(x@pathwayResults)) {
        x@pathwayResults[[i]] <- value
        return(x)
      }
    }
    callNextMethod()
  }
)

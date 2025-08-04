setGeneric("pathwayResults", function(x) standardGeneric("pathwayResults"))

setMethod(
  "pathwayResults",
  "DESummarizedExperiment",
  function(x) x@pathwayResults
)

setGeneric("pathwayResults<-", function(x, value) standardGeneric("pathwayResults<-"))

setReplaceMethod(
  "pathwayResults",
  "DESummarizedExperiment",
  function(x, value) {
    if (!is(value, "SimpleList")) {
      stop("value must be a SimpleList")
    }
    if (length(value)) {
      if (!all(lengths(value) == nrow(x))) {
        stop("elements must have same number of rows as 'x'")
      }
      same_rows <- vapply(
        value,
        function(v) identical(rownames(v), rownames(x)),
        logical(1)
      )
      if (!all(same_rows)) {
        stop("elements must have rownames matching rownames(x)")
      }
    }
    x@pathwayResults <- value
    m <- S4Vectors::metadata(x)
    m$pathway_results <- names(value)
    S4Vectors::metadata(x) <- m
    validObject(x)
    x
  }
)

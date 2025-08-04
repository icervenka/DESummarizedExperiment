setGeneric("deResults", function(x) standardGeneric("deResults"))

setMethod(
  "deResults",
  "DESummarizedExperiment",
  function(x) x@deResults
)

setGeneric("deResults<-", function(x, value) standardGeneric("deResults<-"))

setReplaceMethod(
  "deResults",
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
    x@deResults <- value
    m <- S4Vectors::metadata(x)
    m$de_results <- names(value)
    S4Vectors::metadata(x) <- m
    validObject(x)
    x
  }
)

setGeneric("design", function(x) standardGeneric("design"))

setMethod(
  "design",
  "DESummarizedExperiment",
  function(x) x@design
)

setGeneric("design<-", function(x, value) standardGeneric("design<-"))

setReplaceMethod(
  "design",
  "DESummarizedExperiment",
  function(x, value) {
    if (!is.null(value)) {
      if (nrow(value) != ncol(x)) {
        stop("design must have one row per column in 'x'")
      }
      if (!identical(rownames(value), colnames(x))) {
        stop("rownames(value) must match colnames(x)")
      }
    }
    x@design <- value
    m <- S4Vectors::metadata(x)
    m$design <- value
    S4Vectors::metadata(x) <- m
    validObject(x)
    x
  }
)

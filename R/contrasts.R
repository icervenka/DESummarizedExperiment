setGeneric("contrasts", function(x) standardGeneric("contrasts"))

setMethod(
  "contrasts",
  "DESummarizedExperiment",
  function(x) x@contrastList
)

setGeneric("contrasts<-", function(x, value) standardGeneric("contrasts<-"))

setReplaceMethod(
  "contrasts",
  "DESummarizedExperiment",
  function(x, value) {
    if (!is.list(value)) {
      stop("value must be a list")
    }
    if (length(value)) {
      n <- ncol(x)
      same_len <- lengths(value) == n
      if (!all(same_len)) {
        stop("each contrast must have length equal to ncol(x)")
      }
      same_names <- vapply(
        value,
        function(v) is.null(names(v)) || identical(names(v), colnames(x)),
        logical(1)
      )
      if (!all(same_names)) {
        stop("names of contrasts must match colnames(x)")
      }
    }
    x@contrastList <- value
    m <- S4Vectors::metadata(x)
    m$contrasts <- names(value)
    S4Vectors::metadata(x) <- m
    validObject(x)
    x
  }
)

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
    x@pathwayResults <- value
    m <- S4Vectors::metadata(x)
    m$pathway_results <- names(value)
    S4Vectors::metadata(x) <- m
    validObject(x)
    x
  }
)

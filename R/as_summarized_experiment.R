#' Coerce to SummarizedExperiment
#'
#' @param from A DESummarizedExperiment.
#'
#' @return A SummarizedExperiment.
#' @export
#' @importFrom SummarizedExperiment SummarizedExperiment assays rowData colData
#' @importFrom S4Vectors metadata
setAs(
  "DESummarizedExperiment",
  "SummarizedExperiment",
  function(from) {
    SummarizedExperiment::SummarizedExperiment(
      assays = SummarizedExperiment::assays(from),
      rowData = SummarizedExperiment::rowData(from),
      colData = SummarizedExperiment::colData(from),
      metadata = S4Vectors::metadata(from)
    )
  }
)

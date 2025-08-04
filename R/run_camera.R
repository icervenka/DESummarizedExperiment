runCamera <- function(object, gene_sets, de_result, statistic = "logFC", ...) {
  res <- deResults(object)[[de_result]]
  stats <- res[[statistic]]
  names(stats) <- rownames(res)
  stats <- sort(stats, decreasing = TRUE)
  tab <- limma::cameraPR(stats, gene_sets, ...)
  results <- pathwayResults(object)
  results$camera <- S4Vectors::DataFrame(tab)
  pathwayResults(object) <- results
  invisible(object)
}

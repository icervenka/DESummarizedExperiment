runGSEA <- function(object, gene_sets, de_result, statistic = "logFC", ...) {
  res <- deResults(object)[[de_result]]
  stats <- res[[statistic]]
  names(stats) <- rownames(res)
  stats <- sort(stats, decreasing = TRUE)
  tab <- fgsea::fgsea(pathways = gene_sets, stats = stats, ...)
  results <- pathwayResults(object)
  results$GSEA <- S4Vectors::DataFrame(tab)
  pathwayResults(object) <- results
  invisible(object)
}

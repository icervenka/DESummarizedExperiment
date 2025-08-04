runClusterProfiler <- function(object, gene_sets, de_result, statistic = "logFC", ...) {
  res <- deResults(object)[[de_result]]
  stats <- res[[statistic]]
  names(stats) <- rownames(res)
  stats <- sort(stats, decreasing = TRUE)
  term2gene <- data.frame(
    term = rep(names(gene_sets), lengths(gene_sets)),
    gene = unlist(gene_sets),
    stringsAsFactors = FALSE
  )
  cp <- clusterProfiler::GSEA(geneList = stats, TERM2GENE = term2gene, ...)
  results <- pathwayResults(object)
  results$clusterProfiler <- S4Vectors::DataFrame(cp@result)
  pathwayResults(object) <- results
  invisible(object)
}

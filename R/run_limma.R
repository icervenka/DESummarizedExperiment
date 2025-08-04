runLimma <- function(object, design, contrast) {
  counts <- SummarizedExperiment::assay(object, "counts")
  v <- limma::voom(counts, design)
  fit <- limma::lmFit(v, design)
  fit <- limma::contrasts.fit(fit, contrast)
  fit <- limma::eBayes(fit)
  tab <- limma::topTable(fit, n = Inf, sort.by = "none")
  res <- S4Vectors::DataFrame(
    logFC = tab$logFC,
    PValue = tab$P.Value,
    FDR = tab$adj.P.Val,
    row.names = rownames(tab)
  )
  results <- deResults(object)
  results$limma <- res
  deResults(object) <- results
  invisible(object)
}

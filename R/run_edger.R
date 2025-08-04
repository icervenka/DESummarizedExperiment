runEdgeR <- function(object, design, contrast) {
  counts <- SummarizedExperiment::assay(object, "counts")
  y <- edgeR::DGEList(counts)
  y <- edgeR::calcNormFactors(y)
  fit <- edgeR::glmQLFit(y, design)
  qlf <- edgeR::glmQLFTest(fit, contrast = contrast)
  tab <- edgeR::topTags(qlf, n = Inf, sort.by = "none")$table
  res <- S4Vectors::DataFrame(
    logFC = tab$logFC,
    PValue = tab$PValue,
    FDR = tab$FDR,
    row.names = rownames(tab)
  )
  results <- deResults(object)
  results$edgeR <- res
  deResults(object) <- results
  invisible(object)
}

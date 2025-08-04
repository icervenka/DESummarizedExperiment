runDESeq <- function(object, design, contrast) {
  counts <- SummarizedExperiment::assay(object, "counts")
  col_data <- S4Vectors::DataFrame(design, row.names = colnames(counts))
  formula <- stats::as.formula(
    paste("~", paste(colnames(col_data), collapse = "+"))
  )
  dds <- DESeq2::DESeqDataSetFromMatrix(
    countData = counts,
    colData = col_data,
    design = formula
  )
  dds <- DESeq2::DESeq(dds)
  res_tab <- DESeq2::results(dds, contrast = contrast)
  res <- S4Vectors::DataFrame(
    logFC = res_tab$log2FoldChange,
    PValue = res_tab$pvalue,
    FDR = res_tab$padj,
    row.names = rownames(res_tab)
  )
  results <- deResults(object)
  results$DESeq <- res
  deResults(object) <- results
  invisible(object)
}

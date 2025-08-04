runDE <- function(object, method = c("edgeR", "limma", "DESeq"), ...) {
  method <- match.arg(method)
  switch(
    method,
    edgeR = runEdgeR(object, ...),
    limma = runLimma(object, ...),
    DESeq = runDESeq(object, ...)
  )
}

test_that("constructor populates slots", {
  mat <- matrix(1:4, nrow = 2)
  rownames(mat) <- c("a", "b")
  colnames(mat) <- c("s1", "s2")
  design <- matrix(1, nrow = 2, dimnames = list(c("s1", "s2"), "int"))
  de <- SimpleList(data.frame(stat = 1:2, row.names = c("a", "b")))
  pe <- SimpleList(data.frame(p = 1:2, row.names = c("a", "b")))
  x <- DESummarizedExperiment(
    assays = list(counts = mat),
    design = design,
    contrastList = list(c1 = "b-a"),
    deResults = de,
    pathwayResults = pe
  )
  expect_s4_class(x, "DESummarizedExperiment")
  expect_identical(x@design, design)
  expect_identical(x@contrastList, list(c1 = "b-a"))
})

test_that("validity fails for mismatched rows", {
  mat <- matrix(1:4, nrow = 2)
  rownames(mat) <- c("a", "b")
  colnames(mat) <- c("s1", "s2")
  bad_de <- SimpleList(data.frame(stat = 1:2, row.names = c("b", "a")))
  expect_error(
    DESummarizedExperiment(assays = list(counts = mat), deResults = bad_de),
    "rownames"
  )
})

#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @keywords internal
.tidyContrasts <- function(contrast) {
  if (is.null(contrast)) {
    return(
      data.frame(
        contrast = character(),
        term = character(),
        value = numeric()
      )
    )
  }
  if (is.numeric(contrast)) {
    contrast <- list(contrast)
  } else if (is.matrix(contrast)) {
    contrast <- split(contrast, col(contrast))
    contrast <- lapply(seq_along(contrast), function(i) {
      v <- contrast[[i]]
      names(v) <- rownames(contrast)
      v
    })
    names(contrast) <- colnames(contrast)
  } else if (!is.list(contrast)) {
    stop("'contrast' must be a numeric vector, matrix, or list")
  }
  names(contrast) <- names(contrast) %||% as.character(seq_along(contrast))
  res <- do.call(
    rbind,
    lapply(names(contrast), function(nm) {
      v <- contrast[[nm]]
      data.frame(
        contrast = nm,
        term = names(v),
        value = unname(v),
        stringsAsFactors = FALSE
      )
    })
  )
  rownames(res) <- NULL
  res
}

#' @keywords internal
.checkAssayCompatibility <- function(x, assays) {
  missing <- setdiff(assays, SummarizedExperiment::assayNames(x))
  if (length(missing)) {
    stop("assays not found: ", paste(missing, collapse = ", "))
  }
  mats <- lapply(assays, SummarizedExperiment::assay, x = x)
  rows <- lapply(mats, rownames)
  cols <- lapply(mats, colnames)
  if (!all(vapply(rows, identical, logical(1), rows[[1]]))) {
    stop("assays have differing rownames")
  }
  if (!all(vapply(cols, identical, logical(1), cols[[1]]))) {
    stop("assays have differing colnames")
  }
  invisible(TRUE)
}

#' @keywords internal
.writeToRowData <- function(x, value, name) {
  if (length(value) != nrow(x)) {
    stop("'value' must have length equal to nrow(x)")
  }
  rd <- SummarizedExperiment::rowData(x)
  rd[[name]] <- value
  SummarizedExperiment::rowData(x) <- rd
  x
}

#' @keywords internal
.logMessage <- function(message, log_file = NULL) {
  log_file <- log_file %||% file.path("logs", "DESummarizedExperiment.log")
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  dir.create(dirname(log_file), showWarnings = FALSE, recursive = TRUE)
  cat(
    paste0("[", timestamp, "] ", message, "\n"),
    file = log_file,
    append = TRUE
  )
  invisible(message)
}

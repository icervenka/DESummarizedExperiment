You are the automation layer for an experienced R‑package maintainer.
Your tasks: scaffold, implement, document, test, and refactor R code so that the resulting package is **readable, idiomatic, and sustainable** by humans.

The package has not yet been published and does not have any users; remove functionality outright when it's no longer needed rather than beginning a deprecation process. No need to worry about breaking changes.

# Package-level vision

DESummarizedExperiment (DE-SE) extends SingleCellExperiment, giving you everything in SummarizedExperiment plus the reduced-dimension infrastructure that SCE already implements. The class is intended to be the one-stop container for bulk RNA-seq and label-free proteomics workflows that end in differential-expression (DE) and pathway-enrichment (PE) analyses.

# Function map

## Core class & validity
- **Purpose:** Define the object and its integrity checks.
- **Key functions/generics:**
    `setClass("DESummarizedExperiment", …)`
    `DESummarizedExperiment()` – constructor
    `validDESummarizedExperiment()`
## Accessors (get / set)
- **Purpose:** Retrieve or replace parts of the object.
- **Key functions/generics:**
    - `deResults()`, `pathwayResults()`
    - `design()`, `contrasts()`
    - Standard `assays()`, `rowData()`, `colData()`, `metadata()`
    - `reducedDims()`
    - `[[` / `[[<-` wrappers
## Differential-expression (DE) wrappers
- **Purpose:** Run DE analyses and store results.
- **Key functions/generics:**
    - `runEdgeR()`
    - `runLimma()`
    - `runDESeq()`
    - `runDE(method = c("edgeR", "limma", "DESeq"))`
## Pathway-enrichment (PE) wrappers
- **Purpose:** Perform pathway or gene-set enrichment and save outputs.
- **Key functions/generics:**
    - `runGSEA()`
    - `runCamera()`
    - `runClusterProfiler()`
    - `runPathway(method = c("GSEA", "camera", "clusterProfiler"))`
## Utility / transformation
- **Purpose:** Object manipulation, import/export, and bookkeeping.
- **Key functions/generics:**
    - `exportResults()`
    - `importDE()`
    - `mergeDESummarizedExperiment()`
    - `updateDEmetadata()`
    - Overloads for `subset()`, `[`, `[[`
    - `as(se, "SummarizedExperiment")`
## Developer (internal) helpers
- **Purpose:** Re-usable internal utilities; not exported.
- **Representative helpers:
	- **`.contrastToSamples()`
	- `.tidyContrasts()`
	- `.checkAssayCompatibility()`
	- `.writeToRowData()`
	- `.logMessage()`


# Agent system overview

| Agent Name         | Role                              | Depends On                   |
| ------------------ | --------------------------------- | ---------------------------- |
| `FuncWriter`       | Writes core function code         | StructureSetup               |
| `DocumentationBot` | Adds documentation using roxygen2 | FuncWriter                   |
| `TestWriter`       | Writes tests using `testthat`     | FuncWriter                   |
| `Checker`          | Runs build and checks             | DocumentationBot, TestWriter |

# AgentName: FuncWriter

## Role
Writes R function code for a package based on a brief specification
## Input
Function name, description, arguments with types, expected output
## Output
R function with inline comments
## Trigger Conditions
Triggered by user
## Dependencies
None
## Fallbacks / Errors
If the function has complex inputs and Codex fails, flag it for manual review in logs/agent-failures.md
## Examples
Function to get a character vector of samples belonging to a group from a metadata data frame.
```r
get_samples_per_group <- function(
    metadata, group, group_colname = "SampleGroup",
    sample_colname = "SampleName") {
  # Check if 'group_colname' exists in 'metadata'
  if (!group_colname %in% colnames(metadata)) {
    stop(paste0("Group column '", group_colname, "' not found in metadata."))
  }

  # Check if 'sample_colname' exists in 'metadata' or it's rownames
  if (sample_colname == "rownames") {
    metadata$SampleName <- rownames(metadata)
    sample_colname <- "SampleName"
  }

  if (!sample_colname %in% colnames(metadata)) {
    stop(paste0("Sample column '", sample_colname, "' not found in metadata."))
  }

  # Make unique in case it's something like a colData from SCE
  metadata <- unique(
    as.data.frame(metadata[, c(sample_colname, group_colname)])
  )

  # Subset 'metadata' where the group column equals the specified 'group' value
  subset_metadata <- metadata[
    metadata[[group_colname]] == group, ,
    drop = FALSE
  ]

  # If no samples are found for the specified group, return an empty vector
  # with a warning
  if (nrow(subset_metadata) == 0) {
    warning(paste0("No samples found for group '", group, "'."))
    return(character(0))
  }

  # Extract the sample identifiers from the subset
  samples <- subset_metadata[[sample_colname]]

  # Return the vector of sample identifiers
  return(samples)
}
```

Function to get a named list of sample IDs grouped by a metadata column.
```r
get_sample_list <- function(
    metadata, sample_colname = "SampleName", group_colname = "SampleGroup") {
  # Error checking for inputs is delegated to the get_samples_per_group function

  # Ensure that the group column is a factor; if not, convert it to one
  if (!is.factor(metadata[[group_colname]])) {
    metadata[[group_colname]] <- as.factor(metadata[[group_colname]])
  }

  # Retrieve the levels of the group column (i.e., unique groups)
  group_vec <- levels(metadata[[group_colname]])

  sample_list <- lapply(group_vec, function(x) {
    get_samples_per_group(
      metadata,
      x,
      group_colname = group_colname,
      sample_colname = sample_colname
    )
  })

  sample_list <- setNames(sample_list, group_vec)

  return(sample_list)
}
```
## Notes
For details refer to the specific parts of AGENTS.md file: 
- Coding Style
- Best practices
- Formatting
- Refactoring Guidelines

# AgentName: DocumentationBot

## Role
Adds roxygen2-style documentation to R functions
## Input
R function without roxygen documentation located in R/ folder
## Output
Same R function with roxygen2 comments above it. Do not  modify existing code.
## Trigger Conditions
Whenever new functions are added to R/ without existing documentation
## Dependencies
FuncWriter
## Fallbacks / Errors
If the function has complex inputs and Codex fails, flag it for manual review in logs/agent-failures.md
## ## Examples
```r
#' Extract assay matrix with custom row names
#'
#' Retrieves the assay data from a SummarizedExperiment and sets row names from
#' a specified column in rowData. Optionally fills NAs.
#'
#' @param se A SummarizedExperiment object or a derived class.
#' @param row_data Column name in rowData to use as row names. Default is
#' "SYMBOL".
#' @param fill_na Logical. Replace NAs with the minimum value. Default is FALSE.
#'
#' @return A numeric matrix of assay data.
#'
#' @importFrom SummarizedExperiment assay rowData
#' @export
get_assay_data <- function(se, row_data = "SYMBOL", fill_na = FALSE) {
  adat <- as.matrix(assay(se))
  rownames(adat) <- as.data.frame(rowData(se))[, row_data]
  if (fill_na) {
    adat[is.na(adat)] <- min(adat, na.rm = TRUE)
  }
  adat
}
```

```r
#' Generate distinct colors similar to ggplot2 defaults
#'
#' @param i Integer. Number of distinct colors to generate. Must be > 0.
#'
#' @return Character vector of hex color codes (\code{#RRGGBB}) of length
#' \code{i}.
#'
#' @details
#' Colors are spaced in HCL color space with fixed chroma (100) and
#' luminance (65).
#' Hue values span 15 to 375 degrees for even spacing.
#'
#' @examples
##' # Generate 6 distinct colors
#' colors <- gg_color_hue(6)
#' print(colors)
#'
#' # Plotting with the generated colors
#' library(ggplot2)
#' df <- data.frame(
#'   category = factor(1:6),
#'   value = c(10, 20, 30, 40, 50, 60)
#' )
#' ggplot(df, aes(x = category, y = value, fill = category)) +
#'   geom_bar(stat = "identity") +
#'   scale_fill_manual(values = gg_color_hue(6)) +
#'   theme_minimal() +
#'   labs(
#'     title = "Bar Plot with Distinct HCL Colors", x = "Category", y = "Value"
#'   )
#'
#' @importFrom grDevices hcl
#' @export
gg_color_hue <- function(i) {
  if (!is.numeric(i) || length(i) != 1 || i <= 0 || i != as.integer(i)) {
    stop("Argument 'i' must be a positive integer.")
  }
  hues <- seq(15, 375, length = i + 1)
  hcl(h = hues, l = 65, c = 100)[1:i]
}
```
## Notes
Avoid documenting internal functions prefixed with "."

# AgentName: TestWriter

## Role
Creates a test suite with high coverage and ensures all exported functions have corresponding tests.
## Input
R/ folder source files containing function definitions that don't have corresponding tests.
## Output
* Unit tests for each public function.
* Property‑based or snapshot tests for complex outputs.
- Tests should be written in the `tests/testthat/` directory.
- Test files should be named `test-function_name.R`.
## Trigger Conditions
Whenever functions are added and documented to R/ without existing test suite.
## Dependencies
FuncWriter, DocumentationBot
## Fallbacks / Errors
- If you encounter namespacing issues, don't delete tests that otherwise should work, and instead ask me what to do.
- If the function has complex inputs and Codex fails, flag it for manual review in logs/agent-failures.md.
## Examples
```r
gg_color_hue <- function(i) {
  if (!is.numeric(i) || length(i) != 1 || i <= 0 || i != as.integer(i)) {
    stop("Argument 'i' must be a positive integer.")
  }
  hues <- seq(15, 375, length = i + 1)
  hcl(h = hues, l = 65, c = 100)[1:i]
}

test_that("gg_color_hue: generates correct number of colors", {
  expect_length(gg_color_hue(5), 5)
  expect_length(gg_color_hue(1), 1)
  expect_true(is.character(gg_color_hue(3)))
  expect_true(all(grepl("^#[0-9A-F]{6}$", gg_color_hue(3))))
  expect_snapshot(gg_color_hue(4))
})
```

```r
preserve_order_as_factor <- function(data, cols) {
  # Ensure 'data' is a data frame or tibble
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame or tibble.")
  }

   # Select only columns present in the data
  used_cols <- intersect(cols, names(data))

  non_convertible <- used_cols[!vapply(data[used_cols], function(x) {
    is.atomic(x) || is.character(x) || is.numeric(x) || is.integer(x) || is.logical(x) # nolint
  }, FUN.VALUE = logical(1))]

  if (length(non_convertible) > 0) {
    stop(
      "The following columns cannot be converted to factors: ",
      paste(non_convertible, collapse = ", ")
    )
  }

  # Proceed to convert specified columns to factors preserving order
  data <- data |>
    mutate(across(all_of(used_cols), ~ factor(.x, levels = unique(.x))))

  data
}

set_attribute <- function(obj, l, name = "parameter") {
  attr(obj, name) <- l
  obj
}

test_that("preserve_order_as_factor: works", {
  df <- tibble::tibble(
    category = c("X", "Y", "X", "Z"),
    type = c("b", "a", "c", "b"),
    type_na = c("A", NA, "B", "A"),
    id = 1:4
  )
  df_fact <- preserve_order_as_factor(df, c("type", "type_na", "id"))
  expect_s3_class(df_fact$type, "factor")
  expect_s3_class(df_fact$type_na, "factor")
  expect_s3_class(df_fact$id, "factor")
  expect_type(df_fact$category, "character")
  expect_equal(df$category, df_fact$category)
  expect_equal(levels(df_fact$type), c("b", "a", "c"))
  # NA excluded by base::factor
  expect_equal(levels(df_fact$type_na), c("A", "B"))
  expect_equal(levels(df_fact$id), c("1", "2", "3", "4"))

  # Test with non-existent column
  df_fact_non_exist <- preserve_order_as_factor(df, "non_existent_col")
  expect_equal(df, df_fact_non_exist)

  # Test error on non-data.frame
  expect_error(
    preserve_order_as_factor(1:5, "col"),
    "'data' must be a data frame or tibble."
  )
  expect_error(
    preserve_order_as_factor(matrix(1:4, nrow = 2), "col"),
    "'data' must be a data frame or tibble."
  )

  # Test error on non-convertible column
  df_list <- tibble::tibble(a = list(1, 2))
  expect_error(
    preserve_order_as_factor(df_list, "a"),
    "cannot be converted to factors"
  )
})
```
## Notes
- For general testing guidelines refer to the part: '# Testing' of AGENTS.md file.
- When you're running package tests, use devtools::load_all(); testthat::test_file("tests/testthat/path-to-file.R").

# Package Structure

```
mypkg/
├─ DESCRIPTION        # metadata (title, version, deps, authors)
├─ NAMESPACE          # auto‑generated via roxygen2
├─ R/                 # source files (one feature per file)
│   └─ utils-snake_case.R
├─ man/               # roxygen‑generated Rd files (no manual edits)
├─ tests/
│   └─ testthat/      # unit & integration tests
├─ vignettes/         # long‑form examples & docs (R Markdown)
├─ inst/              # extdata, templates, htmlwidgets etc.
├─ data/              # .rda lazy‑loaded datasets
├─ data-raw/          # scripts that create anything in data/
└─ .Rbuildignore      # ignore lists (e.g. README cache, dev scripts)
```

# Coding Style

* **Follow the tidyverse style guide**.
* One exported function per file; helpers are unexported.
* 1 ≤ function length ≤ 50 LOC, max 3 levels of nesting.
* Keep functions small and focused on a single responsibility
- Prefer flat, readable code over clever abstractions
- Create abstractions only when code shares the same semantic meaning and purpose. Don't abstract based on structural similarity alone.
* Prefer **vectorised base R** or **purrr** over explicit loops where practical.
* Use `%||%` for default values; never `if (is.null()) x <-`.

#  Best Practices

- Pure functions wherever possible
- Composition as the primary mechanism for code reuse
- Avoid heavy FP abstractions (no need for complex monads or pipe/compose patterns) unless there is a clear advantage to using them

## SOLID in R

| Principle                 | Concrete R‑package Application                                             |
| ------------------------- | -------------------------------------------------------------------------- |
| **S**ingle Responsibility | Each function tackles one concern; file names express that concern.        |
| **O**pen/Closed           | Add methods via S3/S4; do not change existing signatures for new behavior. |
| **L**iskov Substitution   | Design generics so that any valid subclass works identically.              |
| **I**nterface Segregation | Expose small, tailored exported helpers rather than one god‑function.      |
| **D**ependency Inversion  | Depend on abstractions (generics) and inject collaborators via arguments.  |
|                           |                                                                            |
#  Formatting

* **snake\_case** for objects & files, `UpperCamel` for S4 classes, `camelCase` for R6 methods.
* 80‑column soft wrap; indent 2 spaces; never use tabs.
* `if () {` and `function(x) {` — opening brace on same line.
* Place a single blank line between function definitions; none at file end.
* Alphabetise arguments in function signatures when logical.

# Refactoring Guidelines

- DRY (Don't Repeat Yourself) is about not duplicating knowledge in the system, not about eliminating all code that looks similar
- Refactoring must never break existing consumers of your code
- **Red → Green → Refactor**: write a failing test, implement, then tidy.
- Eliminate duplication via helpers in `R/utils-*.R`.
- Favour pure functions and explicit returns.
- Break variable‑length arguments into ..., then validate early.

#  Documentation & Comments

* **No inline comments**; clarity must come from code structure.
* Use **roxygen2** blocks (`#'`) for every exported object: title, description, params, return, examples.
* Mark internal helpers with `@keywords internal`.
* Keep examples runnable (`donttest` only when essential).

# Testing

* Framework: **testthat** ≥ 3e.
* Aim for ≥ 90 % statement coverage (`covr`).
* Organise tests mirroring R/ file names.
* Write:
  * Unit tests for each public function.
  * Property‑based or snapshot tests for complex outputs.
  * Edge‑case tests (NA, zero‑length, factor levels).
* When you're running package tests, use devtools::load_all(); testthat::test_file("tests/testthat/path-to-file.R"). If you encounter namespacing issues, don't delete tests that otherwise should work, and instead ask me what to do.

# Things to Avoid

* Non‑deterministic tests (random seeds, internet calls without mock).
* Hidden state (`options()`, global vars, `<<-`).
* Side‑effects in accessors (functions beginning with `get_`, `is_`).
* Long pipelines > 5 verbs without intermediate variables.
* Suppressing warnings/errors unless asserted.

# Common Anti‑Patterns

| Anti‑Pattern                                | Remedy                                              |
| ------------------------------------------- | --------------------------------------------------- |
| **God Function** (> 100 LOC, many branches) | Split into composable helpers.                      |
| **Boolean Flag Explosion** (`do_x = TRUE`)  | Use separate specific functions.                    |
| **Copy‑Paste across files**                 | Extract shared logic into one helper.               |
| **Deeply Nested Lists as API**              | Define S3/S4/R6 classes with clear accessors.       |
| **Silent Failures** (`tryCatch` swallowing) | Surface informative errors with `cli::cli_abort()`. |


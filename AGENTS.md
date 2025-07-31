# 1 · Role — Expert R Package Developer

You are the automation layer for an experienced R‑package maintainer.
Your tasks: scaffold, implement, document, test, and refactor R code so that the resulting package is **readable, idiomatic, and sustainable** by humans.

The package has not yet been published and does not have any users; remove functionality outright when it's no longer needed rather than beginning a deprecation process. No need to worry about breaking changes.

# 2 · Package Structure

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

# 3 · Coding Style

* **Follow the tidyverse style guide**.
* One exported function per file; helpers are unexported.
* 1 ≤ function length ≤ 50 LOC, max 3 levels of nesting.
* Keep functions small and focused on a single responsibility
- Prefer flat, readable code over clever abstractions
- Create abstractions only when code shares the same semantic meaning and purpose. Don't abstract based on structural similarity alone.
* Prefer **vectorised base R** or **purrr** over explicit loops where practical.
* Use `%||%` for default values; never `if (is.null()) x <-`.

# 4 · Best Practices

- Pure functions wherever possible
- Composition as the primary mechanism for code reuse
- Avoid heavy FP abstractions (no need for complex monads or pipe/compose patterns) unless there is a clear advantage to using them

### SOLID in R

| Principle                 | Concrete R‑package Application                                             |
| ------------------------- | -------------------------------------------------------------------------- |
| **S**ingle Responsibility | Each function tackles one concern; file names express that concern.        |
| **O**pen/Closed           | Add methods via S3/S4; do not change existing signatures for new behavior. |
| **L**iskov Substitution   | Design generics so that any valid subclass works identically.              |
| **I**nterface Segregation | Expose small, tailored exported helpers rather than one god‑function.      |
| **D**ependency Inversion  | Depend on abstractions (generics) and inject collaborators via arguments.  |
|                           |                                                                            |
# 5 · Formatting

* **snake\_case** for objects & files, `UpperCamel` for S4 classes, `camelCase` for R6 methods.
* 80‑column soft wrap; indent 2 spaces; never use tabs.
* `if () {` and `function(x) {` — opening brace on same line.
* Place a single blank line between function definitions; none at file end.
* Alphabetise arguments in function signatures when logical.

# 6 · Refactoring Guidelines

- DRY (Don't Repeat Yourself) is about not duplicating knowledge in the system, not about eliminating all code that looks similar
- Refactoring must never break existing consumers of your code
- **Red → Green → Refactor**: write a failing test, implement, then tidy.
- Eliminate duplication via helpers in `R/utils-*.R`.
- Favour pure functions and explicit returns.
- Break variable‑length arguments into ..., then validate early.

# 7 · Documentation & Comments

* **No inline comments**; clarity must come from code structure.
* Use **roxygen2** blocks (`#'`) for every exported object: title, description, params, return, examples.
* Mark internal helpers with `@keywords internal`.
* Keep examples runnable (`donttest` only when essential).

# 8 · Testing

* Framework: **testthat** ≥ 3e.
* Aim for ≥ 90 % statement coverage (`covr`).
* Organise tests mirroring R/ file names.
* Write:
  * Unit tests for each public function.
  * Property‑based or snapshot tests for complex outputs.
  * Edge‑case tests (NA, zero‑length, factor levels).
* When you're running package tests, use devtools::load_all(); testthat::test_file("tests/testthat/path-to-file.R"). If you encounter namespacing issues, don't delete tests that otherwise should work, and instead ask me what to do.

# 9 · Things to Avoid

* Non‑deterministic tests (random seeds, internet calls without mock).
* Hidden state (`options()`, global vars, `<<-`).
* Side‑effects in accessors (functions beginning with `get_`, `is_`).
* Long pipelines > 5 verbs without intermediate variables.
* Suppressing warnings/errors unless asserted.

# 10 · Common Anti‑Patterns

| Anti‑Pattern                                | Remedy                                              |
| ------------------------------------------- | --------------------------------------------------- |
| **God Function** (> 100 LOC, many branches) | Split into composable helpers.                      |
| **Boolean Flag Explosion** (`do_x = TRUE`)  | Use separate specific functions.                    |
| **Copy‑Paste across files**                 | Extract shared logic into one helper.               |
| **Deeply Nested Lists as API**              | Define S3/S4/R6 classes with clear accessors.       |
| **Silent Failures** (`tryCatch` swallowing) | Surface informative errors with `cli::cli_abort()`. |


